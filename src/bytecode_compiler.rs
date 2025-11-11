// bytecode_compiler.rs - Compiles Forth source code to bytecode

use crate::bytecode::{Bytecode, BytecodeAddress, Instruction, PLACEHOLDER_ADDR};
use crate::interpreter::Interpreter;
use crate::primitives::Primitive;
use std::collections::HashMap;

/// Compilation state
#[derive(Debug, Clone, Copy, PartialEq)]
enum CompileState {
    Interpret,
    Compile,
    ExpectingVariableName,
    ExpectingConstantName,
}

/// Information about a word in the dictionary
#[derive(Debug, Clone)]
pub enum WordInfo {
    Primitive {
        name: String,
    },
    UserDefined {
        name: String,
        address: BytecodeAddress,
        is_immediate: bool,
        source_tokens: Option<Vec<String>>, // For IMMEDIATE words
    },
    Variable {
        name: String,
        offset: usize,
    },
    Constant {
        name: String,
        value: i64,
    },
}

/// Control flow frame for backpatching
#[derive(Debug, Clone)]
enum ControlFrame {
    If { end_addr_placeholder: usize },
    IfElse { _else_addr_placeholder: usize, end_addr_placeholder: usize },
    Begin { start_addr: usize },
    BeginWhile { start_addr: usize, end_addr_placeholder: usize },
    Do { start_addr: usize },
    QuestionDo { question_do_setup_addr: usize },
}

/// Bytecode compiler
pub struct BytecodeCompiler {
    /// All bytecode instructions
    pub bytecode: Bytecode,

    /// Dictionary: word name -> WordInfo
    pub dictionary: HashMap<String, WordInfo>,

    /// Current compilation state
    state: CompileState,

    /// Current word being defined
    current_word_name: Option<String>,
    current_word_start: usize,
    current_word_tokens: Vec<String>,

    /// Last defined word (for IMMEDIATE)
    last_defined_word: Option<String>,

    /// Control flow stack for backpatching
    control_stack: Vec<ControlFrame>,

    /// Token stream and current position
    token_stream: Vec<String>,
    token_index: usize,

    /// Variable allocation in memory
    variable_offsets: HashMap<String, usize>,
    next_variable_offset: usize,

    /// Interpreter for immediate execution
    pub interpreter: Interpreter,
}

impl BytecodeCompiler {
    pub fn new() -> Self {
        let mut compiler = BytecodeCompiler {
            bytecode: Vec::new(),
            dictionary: HashMap::new(),
            state: CompileState::Interpret,
            current_word_name: None,
            current_word_start: 0,
            current_word_tokens: Vec::new(),
            last_defined_word: None,
            control_stack: Vec::new(),
            token_stream: Vec::new(),
            token_index: 0,
            variable_offsets: HashMap::new(),
            next_variable_offset: 0,
            interpreter: Interpreter::new(),
        };

        // Register all primitives in dictionary
        compiler.register_primitives();

        compiler
    }

    /// Register all primitives from the Primitive enum
    fn register_primitives(&mut self) {
        for (name, _prim) in Primitive::all() {
            self.dictionary.insert(
                name.to_string(),
                WordInfo::Primitive {
                    name: name.to_string(),
                },
            );
        }
    }

    /// Tokenize Forth source code
    /// Special handling: ." and S" tokens capture the entire string including quotes
    fn tokenize(source: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut chars = source.chars().peekable();
        let mut current_token = String::new();

        while let Some(ch) = chars.next() {
            match ch {
                // Whitespace - end current token
                ' ' | '\t' | '\n' | '\r' => {
                    if !current_token.is_empty() {
                        // Check if this is ." or S" - if so, capture the string
                        if current_token == ".\"" || current_token == "S\"" {
                            tokens.push(current_token.clone());
                            current_token.clear();

                            // Skip one space if present (delimiter)
                            if chars.peek() == Some(&' ') {
                                chars.next();
                            }

                            // Capture until closing quote
                            let mut string_content = String::new();
                            let mut found_close = false;
                            while let Some(ch) = chars.next() {
                                if ch == '"' {
                                    found_close = true;
                                    break;
                                }
                                string_content.push(ch);
                            }

                            if found_close {
                                // Push the string content as a token (without quotes)
                                tokens.push(string_content);
                            }
                        } else {
                            tokens.push(current_token.clone());
                            current_token.clear();
                        }
                    }
                }

                // Parenthesis comment
                '(' => {
                    // Save current token if any
                    if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                    // Skip until closing paren
                    while let Some(ch) = chars.next() {
                        if ch == ')' {
                            break;
                        }
                    }
                }

                // Backslash comment (rest of line)
                '\\' => {
                    if current_token.is_empty() || chars.peek() == Some(&' ') {
                        // It's a comment, not part of a word
                        if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                            current_token.clear();
                        }
                        // Skip rest of line
                        while let Some(ch) = chars.next() {
                            if ch == '\n' {
                                break;
                            }
                        }
                    } else {
                        // Part of a word
                        current_token.push(ch);
                    }
                }

                // Regular character
                _ => {
                    current_token.push(ch);
                }
            }
        }

        // Don't forget last token
        if !current_token.is_empty() {
            tokens.push(current_token);
        }

        tokens
    }

    /// Process Forth source code
    pub fn process_source(&mut self, source: &str) -> Result<(), String> {
        self.token_stream = Self::tokenize(source);
        self.token_index = 0;

        // Check if source contains definition keywords
        let tokens_upper: Vec<String> = self.token_stream.iter()
            .map(|t| t.to_uppercase())
            .collect();
        let has_definitions = tokens_upper.contains(&":".to_string())
            || tokens_upper.contains(&"VARIABLE".to_string())
            || tokens_upper.contains(&"CONSTANT".to_string());

        // If no definitions, execute immediately
        let execute_immediately = self.state == CompileState::Interpret && !has_definitions;
        let start_addr = if execute_immediately {
            self.bytecode.len()
        } else {
            0
        };

        while self.token_index < self.token_stream.len() {
            let token = self.token_stream[self.token_index].clone();
            self.token_index += 1;
            self.process_token(&token)?;
        }

        // If we compiled for immediate execution, add Return and execute
        if execute_immediately {
            self.emit(Instruction::Return);
            self.interpreter.execute(&self.bytecode, start_addr)?;
            // Remove the temporary bytecode
            self.bytecode.truncate(start_addr);
        }

        Ok(())
    }

    /// Emit a bytecode instruction
    fn emit(&mut self, inst: Instruction) {
        self.bytecode.push(inst);
    }

    /// Get current bytecode address
    fn here(&self) -> BytecodeAddress {
        self.bytecode.len()
    }

    /// Backpatch a jump address
    fn backpatch(&mut self, addr: usize, target: BytecodeAddress) {
        match &mut self.bytecode[addr] {
            Instruction::Jump(dest) => *dest = target,
            Instruction::JumpIfZero(dest) => *dest = target,
            Instruction::JumpIfNotZero(dest) => *dest = target,
            Instruction::LoopCheck(dest) => *dest = target,
            Instruction::PlusLoopCheck(dest) => *dest = target,
            Instruction::QuestionDoSetup(dest) => *dest = target,
            _ => panic!("Trying to backpatch non-jump instruction at {}", addr),
        }
    }

    /// Process a single token
    fn process_token(&mut self, token: &str) -> Result<(), String> {
        // Capture tokens while defining a word (for IMMEDIATE words)
        if self.state == CompileState::Compile && self.current_word_name.is_some() {
            if token != ";" && Some(token.to_uppercase()) != self.current_word_name.as_ref().map(|s| s.to_uppercase()) {
                self.current_word_tokens.push(token.to_string());
            }
        }

        // Handle special states
        match self.state {
            CompileState::ExpectingVariableName => {
                return self.define_variable(&token.to_uppercase());
            }
            CompileState::ExpectingConstantName => {
                return self.define_constant(&token.to_uppercase());
            }
            _ => {}
        }

        // Try to parse as number
        let num_result = if token.starts_with('#') {
            token[1..].parse::<i64>()
        } else if token.starts_with('$') {
            i64::from_str_radix(&token[1..], 16)
        } else if token.starts_with('%') {
            i64::from_str_radix(&token[1..], 2)
        } else {
            token.parse::<i64>()
        };

        if let Ok(num) = num_result {
            if self.state == CompileState::Compile {
                self.emit(Instruction::PushLiteral(num));
            } else {
                self.interpreter.vm.data_stack.push(num);
            }
            return Ok(());
        }

        // It's a word
        let word_upper = token.to_uppercase();

        // Special case: word name for definition
        if self.state == CompileState::Compile && self.current_word_name.is_none() {
            return self.set_definition_name(&word_upper);
        }

        // Check for special words
        match word_upper.as_str() {
            ":" => self.begin_definition(),
            ";" => self.end_definition(),
            "IF" => self.immediate_if(),
            "THEN" => self.immediate_then(),
            "ELSE" => self.immediate_else(),
            "BEGIN" => self.immediate_begin(),
            "UNTIL" => self.immediate_until(),
            "WHILE" => self.immediate_while(),
            "REPEAT" => self.immediate_repeat(),
            "DO" => self.immediate_do(),
            "?DO" => self.immediate_question_do(),
            "LOOP" => self.immediate_loop(),
            "+LOOP" => self.immediate_plus_loop(),
            "EXIT" => self.immediate_exit(),
            "RECURSE" => self.immediate_recurse(),
            "IMMEDIATE" => self.mark_immediate(),
            "VARIABLE" => self.begin_variable(),
            "CONSTANT" => self.begin_constant(),
            "CHAR" => self.immediate_char(),
            ".\"" => self.immediate_dot_quote(),
            "S\"" => self.immediate_s_quote(),

            _ => {
                // Look up word in dictionary
                if let Some(word_info) = self.dictionary.get(&word_upper).cloned() {
                    // Check if IMMEDIATE word during compilation
                    if let WordInfo::UserDefined { is_immediate: true, source_tokens: Some(ref tokens), .. } = word_info {
                        if self.state == CompileState::Compile {
                            // Re-execute tokens
                            for token in tokens {
                                self.process_token(token)?;
                            }
                            return Ok(());
                        }
                    }
                    // Process word (compile or execute based on state)
                    self.process_word(&word_info, self.state == CompileState::Compile)
                } else {
                    Err(format!("Unknown word: {}", token))
                }
            }
        }
    }

    // (Continued in next part...)

    /// Begin word definition
    fn begin_definition(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            return Err("Cannot nest definitions".to_string());
        }
        self.state = CompileState::Compile;
        self.current_word_name = None;
        self.current_word_start = self.here();
        Ok(())
    }

    /// Set the name for current definition
    fn set_definition_name(&mut self, name: &str) -> Result<(), String> {
        self.current_word_name = Some(name.to_string());
        Ok(())
    }

    /// End word definition
    fn end_definition(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("Not in definition".to_string());
        }

        let word_name = self.current_word_name.take()
            .ok_or("No word name set")?;

        // Store tokens for IMMEDIATE words
        let source_tokens = if !self.current_word_tokens.is_empty() {
            Some(self.current_word_tokens.clone())
        } else {
            None
        };

        // Emit Return instruction
        self.emit(Instruction::Return);

        // Add to dictionary
        self.dictionary.insert(
            word_name.clone(),
            WordInfo::UserDefined {
                name: word_name.clone(),
                address: self.current_word_start,
                is_immediate: false, // Will be set by mark_immediate
                source_tokens,
            },
        );

        // Reset state
        self.state = CompileState::Interpret;
        self.current_word_tokens.clear();
        self.last_defined_word = Some(word_name);

        Ok(())
    }

    /// Mark last defined word as IMMEDIATE
    fn mark_immediate(&mut self) -> Result<(), String> {
        let word_name = self.last_defined_word.as_ref()
            .ok_or("No word to mark immediate")?
            .clone();

        if let Some(word_info) = self.dictionary.get_mut(&word_name) {
            if let WordInfo::UserDefined { is_immediate, .. } = word_info {
                *is_immediate = true;
                Ok(())
            } else {
                Err(format!("Cannot mark {} as IMMEDIATE (not a user-defined word)", word_name))
            }
        } else {
            Err(format!("Word not found: {}", word_name))
        }
    }

    /// Process a word - either compile it or execute it immediately
    fn process_word(&mut self, word_info: &WordInfo, compile_mode: bool) -> Result<(), String> {
        match word_info {
            WordInfo::Primitive { name } => {
                let prim = Primitive::from_name(name)
                    .ok_or(format!("Unknown primitive: {}", name))?;
                if compile_mode {
                    self.emit(Instruction::Primitive(prim));
                } else {
                    self.interpreter.vm.execute_primitive(prim)
                        .map_err(|e| format!("Primitive error: {:?}", e))?;
                }
            }
            WordInfo::Variable { offset, .. } => {
                if compile_mode {
                    self.emit(Instruction::PushVariable(*offset));
                } else {
                    self.interpreter.vm.data_stack.push(*offset as i64);
                }
            }
            WordInfo::Constant { value, .. } => {
                if compile_mode {
                    self.emit(Instruction::PushConstant(*value));
                } else {
                    self.interpreter.vm.data_stack.push(*value);
                }
            }
            WordInfo::UserDefined { address, .. } => {
                if compile_mode {
                    self.emit(Instruction::Call(*address));
                } else {
                    self.interpreter.execute(&self.bytecode, *address)?;
                }
            }
        }
        Ok(())
    }

    // Control flow implementations...
    fn immediate_if(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("IF outside of definition".to_string());
        }

        // Emit JumpIfZero with placeholder
        self.emit(Instruction::JumpIfZero(PLACEHOLDER_ADDR));
        let placeholder_addr = self.here() - 1;

        self.control_stack.push(ControlFrame::If { end_addr_placeholder: placeholder_addr });
        Ok(())
    }

    fn immediate_then(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("THEN outside of definition".to_string());
        }

        let frame = self.control_stack.pop()
            .ok_or("THEN without IF")?;

        match frame {
            ControlFrame::If { end_addr_placeholder } => {
                self.backpatch(end_addr_placeholder, self.here());
            }
            ControlFrame::IfElse { end_addr_placeholder, .. } => {
                self.backpatch(end_addr_placeholder, self.here());
            }
            _ => return Err("THEN without matching IF".to_string()),
        }

        Ok(())
    }

    fn immediate_else(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("ELSE outside of definition".to_string());
        }

        let frame = self.control_stack.pop()
            .ok_or("ELSE without IF")?;

        match frame {
            ControlFrame::If { end_addr_placeholder: if_end } => {
                // Emit unconditional jump over ELSE clause
                self.emit(Instruction::Jump(PLACEHOLDER_ADDR));
                let else_end_placeholder = self.here() - 1;

                // Backpatch IF's jump to here (start of ELSE)
                self.backpatch(if_end, self.here());

                // Push new frame for THEN to backpatch
                self.control_stack.push(ControlFrame::IfElse {
                    _else_addr_placeholder: if_end,
                    end_addr_placeholder: else_end_placeholder,
                });
            }
            _ => return Err("ELSE without matching IF".to_string()),
        }

        Ok(())
    }

    fn immediate_begin(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("BEGIN outside of definition".to_string());
        }

        let start_addr = self.here();
        self.control_stack.push(ControlFrame::Begin { start_addr });
        Ok(())
    }

    fn immediate_until(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("UNTIL outside of definition".to_string());
        }

        let frame = self.control_stack.pop()
            .ok_or("UNTIL without BEGIN")?;

        match frame {
            ControlFrame::Begin { start_addr } => {
                // Jump back to BEGIN if TOS is zero
                self.emit(Instruction::JumpIfZero(start_addr));
            }
            _ => return Err("UNTIL without matching BEGIN".to_string()),
        }

        Ok(())
    }

    fn immediate_while(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("WHILE outside of definition".to_string());
        }

        let frame = self.control_stack.pop()
            .ok_or("WHILE without BEGIN")?;

        match frame {
            ControlFrame::Begin { start_addr } => {
                // JumpIfZero to end (will be backpatched by REPEAT)
                self.emit(Instruction::JumpIfZero(PLACEHOLDER_ADDR));
                let end_placeholder = self.here() - 1;

                self.control_stack.push(ControlFrame::BeginWhile {
                    start_addr,
                    end_addr_placeholder: end_placeholder,
                });
            }
            _ => return Err("WHILE without matching BEGIN".to_string()),
        }

        Ok(())
    }

    fn immediate_repeat(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("REPEAT outside of definition".to_string());
        }

        let frame = self.control_stack.pop()
            .ok_or("REPEAT without BEGIN/WHILE")?;

        match frame {
            ControlFrame::BeginWhile { start_addr, end_addr_placeholder } => {
                // Unconditional jump back to BEGIN
                self.emit(Instruction::Jump(start_addr));

                // Backpatch WHILE's jump to here
                self.backpatch(end_addr_placeholder, self.here());
            }
            _ => return Err("REPEAT without matching BEGIN/WHILE".to_string()),
        }

        Ok(())
    }

    fn immediate_do(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("DO outside of definition".to_string());
        }

        // Emit DO setup
        self.emit(Instruction::DoSetup);

        let start_addr = self.here();
        self.control_stack.push(ControlFrame::Do { start_addr });
        Ok(())
    }

    fn immediate_question_do(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("?DO outside of definition".to_string());
        }

        // ?DO checks if start == limit and skips loop if true
        self.emit(Instruction::QuestionDoSetup(PLACEHOLDER_ADDR));
        let question_do_setup_addr = self.here() - 1;
        let start_addr = self.here();

        self.control_stack.push(ControlFrame::Do {
            start_addr,
        });

        // Store the QuestionDoSetup address so we can backpatch it at LOOP
        self.control_stack.push(ControlFrame::QuestionDo {
            question_do_setup_addr,
        });

        Ok(())
    }

    fn immediate_loop(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("LOOP outside of definition".to_string());
        }

        // Check if this is a ?DO loop (will have QuestionDo frame on top)
        let is_question_do = matches!(
            self.control_stack.last(),
            Some(ControlFrame::QuestionDo { .. })
        );

        // Pop QuestionDo frame if present
        let question_do_setup_addr = if is_question_do {
            if let Some(ControlFrame::QuestionDo { question_do_setup_addr }) = self.control_stack.pop() {
                Some(question_do_setup_addr)
            } else {
                None
            }
        } else {
            None
        };

        // Pop the Do frame
        let frame = self.control_stack.pop()
            .ok_or("LOOP without DO")?;

        match frame {
            ControlFrame::Do { start_addr } => {
                // Increment and check - if done, jump to end; if not done, fall through
                self.emit(Instruction::LoopCheck(PLACEHOLDER_ADDR));
                let loop_check_addr = self.here() - 1;

                // Jump back to loop start (if not done)
                self.emit(Instruction::Jump(start_addr));

                let end_addr = self.here();

                // Backpatch loop check to jump here when done
                self.backpatch(loop_check_addr, end_addr);

                // Backpatch ?DO setup if this was a ?DO loop
                if let Some(addr) = question_do_setup_addr {
                    self.backpatch(addr, end_addr);
                }
            }
            _ => return Err("LOOP without matching DO".to_string()),
        }

        Ok(())
    }

    fn immediate_plus_loop(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("+LOOP outside of definition".to_string());
        }

        // Check if this is a ?DO loop (will have QuestionDo frame on top)
        let is_question_do = matches!(
            self.control_stack.last(),
            Some(ControlFrame::QuestionDo { .. })
        );

        // Pop QuestionDo frame if present
        let question_do_setup_addr = if is_question_do {
            if let Some(ControlFrame::QuestionDo { question_do_setup_addr }) = self.control_stack.pop() {
                Some(question_do_setup_addr)
            } else {
                None
            }
        } else {
            None
        };

        // Pop the Do frame
        let frame = self.control_stack.pop()
            .ok_or("+LOOP without DO")?;

        match frame {
            ControlFrame::Do { start_addr } => {
                // Emit +loop check with jump to after loop
                self.emit(Instruction::PlusLoopCheck(PLACEHOLDER_ADDR));
                let loop_check_addr = self.here() - 1;

                // Jump back to loop start
                self.emit(Instruction::Jump(start_addr));

                let end_addr = self.here();

                // Backpatch loop check to jump here (after loop)
                self.backpatch(loop_check_addr, end_addr);

                // Backpatch ?DO setup if this was a ?DO loop
                if let Some(addr) = question_do_setup_addr {
                    self.backpatch(addr, end_addr);
                }
            }
            _ => return Err("+LOOP without matching DO".to_string()),
        }

        Ok(())
    }

    fn immediate_exit(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("EXIT outside of definition".to_string());
        }

        self.emit(Instruction::Return);
        Ok(())
    }

    fn immediate_recurse(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("RECURSE outside of definition".to_string());
        }

        // Call current word being defined
        self.emit(Instruction::Call(self.current_word_start));
        Ok(())
    }

    fn begin_variable(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            return Err("VARIABLE cannot be used inside a definition".to_string());
        }

        self.state = CompileState::ExpectingVariableName;
        Ok(())
    }

    fn define_variable(&mut self, name: &str) -> Result<(), String> {
        // Allocate 8 bytes in memory
        let offset = self.next_variable_offset;
        self.next_variable_offset += 8;

        self.variable_offsets.insert(name.to_string(), offset);

        // Add to dictionary
        self.dictionary.insert(
            name.to_string(),
            WordInfo::Variable {
                name: name.to_string(),
                offset,
            },
        );

        self.state = CompileState::Interpret;
        Ok(())
    }

    fn begin_constant(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            return Err("CONSTANT cannot be used inside a definition".to_string());
        }

        self.state = CompileState::ExpectingConstantName;
        Ok(())
    }

    fn define_constant(&mut self, name: &str) -> Result<(), String> {
        // Pop value from stack
        let value = self.interpreter.vm.data_stack.pop()
            .map_err(|e| format!("CONSTANT requires a value on the stack: {:?}", e))?;

        // Add to dictionary
        self.dictionary.insert(
            name.to_string(),
            WordInfo::Constant {
                name: name.to_string(),
                value,
            },
        );

        self.state = CompileState::Interpret;
        Ok(())
    }

    /// CHAR - Get ASCII value of next character
    fn immediate_char(&mut self) -> Result<(), String> {
        if self.token_index >= self.token_stream.len() {
            return Err("CHAR: unexpected end of input".to_string());
        }

        let next_token = self.token_stream[self.token_index].clone();
        self.token_index += 1;

        let ch = next_token.chars().next()
            .ok_or("CHAR: empty token".to_string())?;
        let char_code = ch as i64;

        if self.state == CompileState::Compile {
            self.emit(Instruction::PushLiteral(char_code));
        } else {
            self.interpreter.vm.data_stack.push(char_code);
        }

        Ok(())
    }

    /// ." - Print string at compile time or runtime
    fn immediate_dot_quote(&mut self) -> Result<(), String> {
        // Get the string content from the next token (tokenizer captured it)
        if self.token_index >= self.token_stream.len() {
            return Err(".\" missing string content".to_string());
        }
        let text = self.token_stream[self.token_index].clone();
        self.token_index += 1;

        if self.state == CompileState::Compile {
            // Emit EMIT calls for each character
            let emit_prim = Primitive::from_name("EMIT")
                .ok_or("EMIT primitive not found")?;

            for ch in text.chars() {
                self.emit(Instruction::PushLiteral(ch as i64));
                self.emit(Instruction::Primitive(emit_prim));
            }
        } else {
            // Print immediately in interpret mode
            print!("{}", text);
        }

        Ok(())
    }

    /// S" - Create string literal (address and length on stack)
    fn immediate_s_quote(&mut self) -> Result<(), String> {
        // Get the string content from the next token (tokenizer captured it)
        if self.token_index >= self.token_stream.len() {
            return Err("S\" missing string content".to_string());
        }
        let text = self.token_stream[self.token_index].clone();
        self.token_index += 1;
        let text_bytes = text.as_bytes();

        if self.state == CompileState::Compile {
            // Allocate string in memory at compile time and embed the address/length
            let here = self.interpreter.vm.here;

            if here + text_bytes.len() > self.interpreter.vm.memory.len() {
                return Err("Out of memory for string".to_string());
            }

            // Store string in memory
            self.interpreter.vm.memory[here..here + text_bytes.len()]
                .copy_from_slice(text_bytes);

            // Emit instructions to push address and length
            self.emit(Instruction::PushLiteral(here as i64));
            self.emit(Instruction::PushLiteral(text_bytes.len() as i64));

            // Update HERE pointer
            self.interpreter.vm.here = here + text_bytes.len();
        } else {
            // Interpret mode - allocate and push address/length
            let here = self.interpreter.vm.here;

            if here + text_bytes.len() > self.interpreter.vm.memory.len() {
                return Err("Out of memory for string".to_string());
            }

            self.interpreter.vm.memory[here..here + text_bytes.len()]
                .copy_from_slice(text_bytes);

            self.interpreter.vm.data_stack.push(here as i64);
            self.interpreter.vm.data_stack.push(text_bytes.len() as i64);

            self.interpreter.vm.here = here + text_bytes.len();
        }

        Ok(())
    }
}

impl Default for BytecodeCompiler {
    fn default() -> Self {
        Self::new()
    }
}
