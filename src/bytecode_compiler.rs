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
    ExpectingCreatedName,
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
    Created {
        name: String,
        data_offset: usize,
    },
}

/// Control flow frame for backpatching
#[derive(Debug, Clone)]
enum ControlFrame {
    If { end_addr_placeholder: usize },
    IfElse { _else_addr_placeholder: usize, end_addr_placeholder: usize },
    Begin { start_addr: usize },
    BeginWhile { start_addr: usize, end_addr_placeholder: usize },
    Do { start_addr: usize, leave_placeholders: Vec<usize> },
    QuestionDo { question_do_setup_addr: usize },
}

/// Bytecode compiler
pub struct BytecodeCompiler {
    // === PERSISTENT STATE (never reset) ===
    /// All bytecode instructions
    pub bytecode: Bytecode,

    /// Dictionary: word name -> WordInfo
    pub dictionary: HashMap<String, WordInfo>,

    /// Variable allocation in memory
    variable_offsets: HashMap<String, usize>,
    next_variable_offset: usize,

    /// Interpreter for immediate execution
    pub interpreter: Interpreter,

    // === COMPILATION STATE (persists across lines within definitions) ===
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

    // === PER-LINE STATE (reset at start of each line) ===
    /// Current line being processed
    input: String,
    /// Position in current line
    pos: usize,
}

impl BytecodeCompiler {
    pub fn new() -> Self {
        let mut compiler = BytecodeCompiler {
            bytecode: Vec::new(),
            dictionary: HashMap::new(),
            variable_offsets: HashMap::new(),
            next_variable_offset: 0,
            interpreter: Interpreter::new(),
            state: CompileState::Interpret,
            current_word_name: None,
            current_word_start: 0,
            current_word_tokens: Vec::new(),
            last_defined_word: None,
            control_stack: Vec::new(),
            input: String::new(),
            pos: 0,
        };

        // Register all primitives in dictionary
        compiler.register_primitives();

        compiler
    }

    /// Sync the compiler's variable offset with the VM's HERE pointer
    /// Call this after loading stdlib or when HERE may have been modified
    pub fn sync_here(&mut self) {
        // If vm.here is ahead of next_variable_offset, update offset
        if self.interpreter.vm.here > self.next_variable_offset {
            self.next_variable_offset = self.interpreter.vm.here;
        } else {
            // Otherwise, update vm.here to match offset
            self.interpreter.vm.here = self.next_variable_offset;
        }
    }

    /// Sync >IN in VM memory to match self.pos
    fn sync_to_in(&mut self) {
        let to_in_bytes = (self.pos as i64).to_le_bytes();
        self.interpreter.vm.memory[crate::primitives::TO_IN_ADDR..crate::primitives::TO_IN_ADDR + 8]
            .copy_from_slice(&to_in_bytes);
    }

    /// Sync self.pos to match >IN in VM memory (for when Forth code modifies >IN)
    fn sync_pos_from_to_in(&mut self) {
        let mut to_in_bytes = [0u8; 8];
        to_in_bytes.copy_from_slice(&self.interpreter.vm.memory[crate::primitives::TO_IN_ADDR..
            crate::primitives::TO_IN_ADDR + 8]);
        self.pos = i64::from_le_bytes(to_in_bytes) as usize;
    }

    /// Parse a string literal from the current input line
    /// Used for S" and ." words
    fn parse_string_literal(&mut self) -> Result<String, String> {
        let start = self.pos;

        // Find closing quote
        while self.pos < self.input.len() {
            if self.input.as_bytes()[self.pos] == b'"' {
                let string = self.input[start..self.pos].to_string();
                self.pos += 1; // Skip closing quote
                self.sync_to_in();
                return Ok(string);
            }
            self.pos += 1;
        }

        Err("Unterminated string literal".to_string())
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

    /// Parse next token from INPUT_BUFFER starting at >IN, updating >IN
    fn next_token(&mut self) -> Result<Option<String>, String> {
        // Sync from >IN in case Forth code modified it
        self.sync_pos_from_to_in();

        // Skip whitespace and comments
        loop {
            // Skip whitespace
            while self.pos < self.input.len() && self.input.as_bytes()[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }

            // End of input
            if self.pos >= self.input.len() {
                self.sync_to_in();
                return Ok(None);
            }

            let current_byte = self.input.as_bytes()[self.pos];

            // Check for special two-character tokens BEFORE treating ( as comment
            if self.pos + 1 < self.input.len() {
                let next_byte = self.input.as_bytes()[self.pos + 1];

                // .( is a word, not a comment
                if current_byte == b'.' && next_byte == b'(' {
                    let token = ".(".to_string();
                    self.pos += 2;
                    self.sync_to_in();
                    return Ok(Some(token));
                }

                // ." is a word
                if current_byte == b'.' && next_byte == b'"' {
                    let token = ".\"".to_string();
                    self.pos += 2;
                    self.sync_to_in();
                    return Ok(Some(token));
                }

                // S" is a word
                if (current_byte == b'S' || current_byte == b's') && next_byte == b'"' {
                    let token = "S\"".to_string();
                    self.pos += 2;
                    self.sync_to_in();
                    return Ok(Some(token));
                }
            }

            // ( is a comment (now that we've ruled out .( )
            if current_byte == b'(' {
                self.pos += 1;
                // Skip until closing )
                while self.pos < self.input.len() && self.input.as_bytes()[self.pos] != b')' {
                    self.pos += 1;
                }
                if self.pos < self.input.len() {
                    self.pos += 1; // Skip )
                }
                continue; // Loop to skip more whitespace/comments
            }

            // Not whitespace or comment - ready to parse token
            break;
        }

        // Parse regular token until whitespace
        let start = self.pos;
        while self.pos < self.input.len() && !self.input.as_bytes()[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }

        let token = self.input[start..self.pos].to_uppercase();

        // Skip trailing whitespace so >IN points to the start of the next token
        while self.pos < self.input.len() && self.input.as_bytes()[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }

        self.sync_to_in();

        Ok(Some(token))
    }

    /// Process a single line of Forth source code
    /// Each line gets its own INPUT_BUFFER and >IN context
    /// NOTE: Comments should already be stripped by caller
    pub fn process_line(&mut self, line: &str) -> Result<(), String> {
        // Skip empty lines
        if line.trim().is_empty() {
            return Ok(());
        }

        // === RESET PER-LINE STATE ===
        self.input = line.to_string();
        self.pos = 0;

        // Store line in INPUT_BUFFER and reset >IN to 0
        self.interpreter.vm.set_input(line);

        // Sync pos with >IN (should be 0 after set_input)
        self.sync_pos_from_to_in();

        // === PROCESS TOKENS ===
        // Process tokens one at a time, allowing >IN manipulation between tokens
        loop {
            // Parse next token from input using >IN
            let token = match self.next_token()? {
                Some(t) => t,
                None => break, // End of input
            };

            // In interpret mode with no definitions, execute immediately
            if self.state == CompileState::Interpret {
                let start_addr = self.bytecode.len();
                self.process_token(&token)?;
                self.emit(Instruction::Return);
                self.interpreter.execute(&self.bytecode, start_addr)?;
                self.bytecode.truncate(start_addr);
            } else {
                // In compile mode, just process the token (adds to bytecode)
                self.process_token(&token)?;
            }
        }

        Ok(())
    }

    /// Strip backslash comments from a line (\ comments only)
    /// Parenthetical comments are handled by the tokenizer
    fn strip_backslash_comments(line: &str) -> &str {
        // Find \ at start or preceded by whitespace
        if let Some(pos) = line.find('\\') {
            if pos == 0 || line[..pos].ends_with(char::is_whitespace) {
                return &line[..pos];
            }
        }
        line
    }

    /// Check if a line starts a definition
    fn starts_definition(line: &str) -> bool {
        let trimmed = line.trim_start();
        trimmed.starts_with(": ") || trimmed.starts_with(":\t")
    }

    /// Process Forth source code (multi-line)
    /// Processes line-by-line, each with its own >IN context
    /// Accumulates lines when inside a definition (between : and ;)
    pub fn process_source(&mut self, source: &str) -> Result<(), String> {
        // Reset >IN at the start of processing new source (important for INCLUDE)
        let to_in_bytes = 0i64.to_le_bytes();
        self.interpreter.vm.memory[crate::primitives::TO_IN_ADDR..crate::primitives::TO_IN_ADDR + 8]
            .copy_from_slice(&to_in_bytes);

        let mut accumulated = String::new();
        let mut in_definition = false;

        for line in source.lines() {
            // Strip backslash comments only
            let line = Self::strip_backslash_comments(line);

            // Skip empty lines when not in definition
            if line.trim().is_empty() && !in_definition {
                continue;
            }

            // Check if line contains : or ;
            let has_colon = Self::starts_definition(line);
            let has_semicolon = line.contains(';');

            if in_definition {
                // Accumulate this line
                accumulated.push(' ');
                accumulated.push_str(line);

                // Check if definition ends
                if has_semicolon {
                    in_definition = false;
                    // Process the complete definition
                    self.process_line(&accumulated)?;
                    accumulated.clear();
                }
            } else if has_colon && !has_semicolon {
                // Start of multi-line definition
                in_definition = true;
                accumulated = line.to_string();
            } else {
                // Single line - process immediately
                self.process_line(line)?;
            }
        }

        // If we're still in a definition at end, that's an error
        if in_definition {
            return Err("Unterminated definition".to_string());
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
        if self.state == CompileState::Compile && self.current_word_name.is_some()
            && token != ";" && Some(token.to_uppercase()) != self.current_word_name.as_ref().map(|s| s.to_uppercase()) {
            self.current_word_tokens.push(token.to_string());
        }

        // Handle special states
        match self.state {
            CompileState::ExpectingVariableName => {
                return self.define_variable(&token.to_uppercase());
            }
            CompileState::ExpectingConstantName => {
                return self.define_constant(&token.to_uppercase());
            }
            CompileState::ExpectingCreatedName => {
                return self.define_create(&token.to_uppercase());
            }
            _ => {}
        }

        // Try to parse as number
        let num_result = if let Some(stripped) = token.strip_prefix('#') {
            // # prefix forces decimal
            stripped.parse::<i64>()
        } else if let Some(stripped) = token.strip_prefix('$') {
            // $ prefix forces hex
            i64::from_str_radix(stripped, 16)
        } else if let Some(stripped) = token.strip_prefix('%') {
            // % prefix forces binary
            i64::from_str_radix(stripped, 2)
        } else {
            // No prefix: use BASE variable
            let base_addr = crate::primitives::BASE_ADDR;
            let mut base_bytes = [0u8; 8];
            base_bytes.copy_from_slice(&self.interpreter.vm.memory[base_addr..base_addr + 8]);
            let base = i64::from_le_bytes(base_bytes) as u32;

            // Parse using current base (default 10)
            i64::from_str_radix(&token, base)
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
            "LEAVE" => self.immediate_leave(),
            "EXIT" => self.immediate_exit(),
            "RECURSE" => self.immediate_recurse(),
            "IMMEDIATE" => self.mark_immediate(),
            "VARIABLE" => self.begin_variable(),
            "CONSTANT" => self.begin_constant(),
            "CREATE" => self.begin_create(),
            "CHAR" => self.immediate_char(),
            "[CHAR]" => self.immediate_bracket_char(),
            "FIND" => self.compile_or_execute_find(),
            ".\"" => self.immediate_dot_quote(),
            ".(" => self.immediate_dot_paren(),
            "S\"" => self.immediate_s_quote(),
            "INCLUDED" => self.compile_included(),
            "EXECUTE" => self.compile_execute(),
            "'" => self.immediate_tick(),
            "[IF]" => self.conditional_if(),
            "[ELSE]" => self.conditional_else(),
            "[THEN]" => {
                // [THEN] by itself does nothing, just marks the end
                // If we get here, it means we were processing the true branch
                Ok(())
            }

            _ => {
                // Look up word in dictionary
                if let Some(word_info) = self.dictionary.get(&word_upper).cloned() {
                    // Check if IMMEDIATE word during compilation
                    if let WordInfo::UserDefined { is_immediate: true, source_tokens: Some(ref tokens), .. } = word_info
                        && self.state == CompileState::Compile {
                        // Re-execute tokens
                        for token in tokens {
                            self.process_token(token)?;
                        }
                        return Ok(());
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
            WordInfo::Created { data_offset, .. } => {
                // Push the address of the data field
                if compile_mode {
                    self.emit(Instruction::PushLiteral(*data_offset as i64));
                } else {
                    self.interpreter.vm.data_stack.push(*data_offset as i64);
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
        self.control_stack.push(ControlFrame::Do { start_addr, leave_placeholders: Vec::new() });
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
            leave_placeholders: Vec::new(),
        });

        // Store the QuestionDoSetup address so we can backpatch it at LOOP
        self.control_stack.push(ControlFrame::QuestionDo {
            question_do_setup_addr,
        });

        Ok(())
    }

    /// Helper for both LOOP and +LOOP
    fn compile_loop_end(&mut self, loop_instruction: Instruction, loop_name: &str) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err(format!("{} outside of definition", loop_name));
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
            .ok_or(format!("{} without DO", loop_name))?;

        match frame {
            ControlFrame::Do { start_addr, leave_placeholders } => {
                // Emit loop check instruction with placeholder
                self.emit(loop_instruction);
                let loop_check_addr = self.here() - 1;

                // Jump back to loop start
                self.emit(Instruction::Jump(start_addr));

                let end_addr = self.here();

                // Backpatch loop check to jump here when done
                self.backpatch(loop_check_addr, end_addr);

                // Backpatch all LEAVE instructions
                for leave_addr in leave_placeholders {
                    self.backpatch(leave_addr, end_addr);
                }

                // Backpatch ?DO setup if this was a ?DO loop
                if let Some(addr) = question_do_setup_addr {
                    self.backpatch(addr, end_addr);
                }
            }
            _ => return Err(format!("{} without matching DO", loop_name)),
        }

        Ok(())
    }

    fn immediate_loop(&mut self) -> Result<(), String> {
        self.compile_loop_end(Instruction::LoopCheck(PLACEHOLDER_ADDR), "LOOP")
    }

    fn immediate_plus_loop(&mut self) -> Result<(), String> {
        self.compile_loop_end(Instruction::PlusLoopCheck(PLACEHOLDER_ADDR), "+LOOP")
    }

    fn immediate_leave(&mut self) -> Result<(), String> {
        if self.state != CompileState::Compile {
            return Err("LEAVE outside of definition".to_string());
        }

        // Find the innermost DO loop on the control stack
        let stack_depth = self.control_stack.len();
        let mut do_index = None;

        // Search from top of stack backwards for a Do frame
        for i in (0..stack_depth).rev() {
            if matches!(self.control_stack[i], ControlFrame::Do { .. }) {
                do_index = Some(i);
                break;
            }
        }

        let do_idx = do_index.ok_or("LEAVE without DO")?;

        // Emit jump with placeholder
        self.emit(Instruction::Jump(PLACEHOLDER_ADDR));
        let leave_addr = self.here() - 1;

        // Add to leave list
        if let ControlFrame::Do { ref mut leave_placeholders, .. } = self.control_stack[do_idx] {
            leave_placeholders.push(leave_addr);
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

        // Sync VM's here with compiler's variable offset
        // This ensures HERE primitive returns correct value
        self.interpreter.vm.here = self.next_variable_offset;

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

    fn begin_create(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            return Err("CREATE cannot be used inside a definition".to_string());
        }

        self.state = CompileState::ExpectingCreatedName;
        Ok(())
    }

    fn define_create(&mut self, name: &str) -> Result<(), String> {
        // CREATE allocates space at the current variable offset
        // The created word will push this address when executed
        // Note: Unlike VARIABLE, CREATE doesn't allocate space itself
        // Space is allocated with ALLOT after CREATE
        let data_offset = self.next_variable_offset;

        // Sync VM's here with compiler's variable offset
        // This ensures HERE primitive returns correct value
        self.interpreter.vm.here = self.next_variable_offset;

        // Add to dictionary
        self.dictionary.insert(
            name.to_string(),
            WordInfo::Created {
                name: name.to_string(),
                data_offset,
            },
        );

        self.state = CompileState::Interpret;
        Ok(())
    }

    /// CHAR - Get ASCII value of next character
    fn immediate_char(&mut self) -> Result<(), String> {
        // Get next token from input
        let next_token = self.next_token()?
            .ok_or("CHAR: unexpected end of input".to_string())?;

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

    /// [CHAR] - Compile-time CHAR (immediate word)
    /// Parse next word and compile its first character as a literal
    fn immediate_bracket_char(&mut self) -> Result<(), String> {
        // [CHAR] can only be used in compilation mode
        if self.state != CompileState::Compile {
            return Err("[CHAR] can only be used inside a definition".to_string());
        }

        // Get next token from input
        let next_token = self.next_token()?
            .ok_or("[CHAR]: unexpected end of input".to_string())?;

        let ch = next_token.chars().next()
            .ok_or("[CHAR]: empty token".to_string())?;
        let char_code = ch as i64;

        // Always compile the character literal
        self.emit(Instruction::PushLiteral(char_code));

        Ok(())
    }

    /// FIND - Find a word in the dictionary (compile or execute)
    /// ( c-addr -- c-addr 0 | xt 1 | xt -1 )
    fn compile_or_execute_find(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            // Compile mode: snapshot dictionary and emit FindWord instruction
            let mut dict_snapshot = std::collections::HashMap::new();
            for (name, word_info) in &self.dictionary {
                if let WordInfo::UserDefined { address, is_immediate, .. } = word_info {
                    dict_snapshot.insert(name.clone(), (*address, *is_immediate));
                }
            }
            self.emit(Instruction::FindWord(dict_snapshot));
            Ok(())
        } else {
            // Interpret mode: execute directly
            self.execute_find()
        }
    }

    /// Execute FIND directly (interpret mode only)
    fn execute_find(&mut self) -> Result<(), String> {
        // Pop c-addr from stack
        let addr = self.interpreter.vm.data_stack
            .pop()
            .map_err(|_| "FIND: stack underflow")?;

        // Read counted string from memory
        let addr_usize = addr as usize;
        if addr_usize >= self.interpreter.vm.memory.len() {
            return Err(format!("FIND: c-addr out of bounds: {}", addr_usize));
        }

        let len = self.interpreter.vm.memory[addr_usize] as usize;
        if addr_usize + 1 + len > self.interpreter.vm.memory.len() {
            return Err("FIND: string extends beyond memory".to_string());
        }

        let name_bytes = &self.interpreter.vm.memory[addr_usize + 1..addr_usize + 1 + len];
        let name = String::from_utf8_lossy(name_bytes).to_uppercase();

        // Search dictionary
        if let Some(word_info) = self.dictionary.get(&name) {
            match word_info {
                WordInfo::UserDefined { address, is_immediate, .. } => {
                    // Found user-defined word: push xt and immediacy flag
                    self.interpreter.vm.data_stack.push(*address as i64);
                    self.interpreter.vm.data_stack.push(if *is_immediate { 1 } else { -1 });
                }
                _ => {
                    // Primitives, variables, constants don't have execution tokens
                    // Return not found
                    self.interpreter.vm.data_stack.push(addr);
                    self.interpreter.vm.data_stack.push(0);
                }
            }
        } else {
            // Not found: push c-addr and 0
            self.interpreter.vm.data_stack.push(addr);
            self.interpreter.vm.data_stack.push(0);
        }

        Ok(())
    }

    /// .( - Print message immediately (like ." but always immediate)
    fn immediate_dot_paren(&mut self) -> Result<(), String> {
        // Parse text from current input until closing )
        // Note: next_token already consumed the ".(" token and updated pos
        self.sync_pos_from_to_in();

        let start = self.pos;
        while self.pos < self.input.len() && self.input.as_bytes()[self.pos] != b')' {
            self.pos += 1;
        }

        if self.pos >= self.input.len() {
            return Err(".( missing closing )".to_string());
        }

        let text = self.input[start..self.pos].to_string();
        self.pos += 1; // Skip closing )
        self.sync_to_in();

        // .( always prints immediately, regardless of state
        print!("{}", text);
        Ok(())
    }

    /// ." - Print string at compile time or runtime
    fn immediate_dot_quote(&mut self) -> Result<(), String> {
        // Parse string literal from current input
        // Note: next_token already consumed the .\" token and updated pos
        // Now we need to skip any whitespace and parse until closing "
        self.sync_pos_from_to_in();

        // Skip one space if present (standard delimiter after .")
        if self.pos < self.input.len() && self.input.as_bytes()[self.pos] == b' ' {
            self.pos += 1;
        }

        let text = self.parse_string_literal()
            .map_err(|_| ".\" missing closing quote".to_string())?;

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

    /// ' (tick) - Get execution token of next word
    fn immediate_tick(&mut self) -> Result<(), String> {
        // Get the next word name from input
        let word_name = self.next_token()?
            .ok_or("' (tick): unexpected end of input".to_string())?
            .to_uppercase();

        // Look up the word
        let word_info = self.dictionary.get(&word_name)
            .ok_or(format!("' (tick): unknown word: {}", word_name))?
            .clone();

        // Get the execution token (bytecode address)
        let xt = match word_info {
            WordInfo::UserDefined { address, .. } => address as i64,
            _ => {
                return Err(format!("' (tick): word {} is not user-defined", word_name));
            }
        };

        // Push the XT onto the stack
        if self.state == CompileState::Compile {
            self.emit(Instruction::PushLiteral(xt));
        } else {
            self.interpreter.vm.data_stack.push(xt);
        }

        Ok(())
    }

    /// INCLUDED - Load a file from address and length on stack
    fn compile_included(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            return Err("INCLUDED cannot be used inside a definition".to_string());
        }

        // Pop length and address from stack
        let len = self.interpreter.vm.data_stack.pop()
            .map_err(|e| format!("INCLUDED: need length on stack: {:?}", e))? as usize;
        let addr = self.interpreter.vm.data_stack.pop()
            .map_err(|e| format!("INCLUDED: need address on stack: {:?}", e))? as usize;

        // Read filename from memory
        if addr + len > self.interpreter.vm.memory.len() {
            return Err("INCLUDED: address out of bounds".to_string());
        }

        let filename_bytes = &self.interpreter.vm.memory[addr..addr + len];
        let filename = std::str::from_utf8(filename_bytes)
            .map_err(|e| format!("INCLUDED: invalid UTF-8: {}", e))?;

        // Load the file
        let contents = std::fs::read_to_string(filename)
            .map_err(|e| format!("INCLUDED: failed to read {}: {}", filename, e))?;

        // Process it
        self.process_source(&contents)?;

        Ok(())
    }

    /// EXECUTE - Execute an execution token from the stack
    fn compile_execute(&mut self) -> Result<(), String> {
        if self.state == CompileState::Compile {
            // Emit ExecuteXT instruction
            self.emit(Instruction::ExecuteXT);
        } else {
            // In interpret mode, execute immediately
            // This is tricky - we need to call bytecode from within the interpreter
            // For now, just emit and execute
            let start = self.bytecode.len();
            self.emit(Instruction::ExecuteXT);
            self.emit(Instruction::Return);
            self.interpreter.execute(&self.bytecode, start)?;
            self.bytecode.truncate(start); // Remove temporary code
        }
        Ok(())
    }

    /// [IF] - Conditional compilation (interpret mode only)
    fn conditional_if(&mut self) -> Result<(), String> {
        // Pop flag from stack
        let flag = self.interpreter.vm.data_stack.pop()
            .map_err(|e| format!("[IF] requires a flag on the stack: {:?}", e))?;

        // If flag is false (0), skip until matching [ELSE] or [THEN]
        if flag == 0 {
            self.skip_until_else_or_then()?;
        }
        // If flag is true, continue processing normally
        Ok(())
    }

    /// [ELSE] - Skip to [THEN] (we're in the true branch, so skip the else part)
    fn conditional_else(&mut self) -> Result<(), String> {
        // If we encounter [ELSE] during normal processing, it means the [IF] was true
        // So we skip the else branch
        self.skip_until_then()
    }

    /// Skip tokens until matching [ELSE] or [THEN], handling nested [IF]s
    fn skip_until_else_or_then(&mut self) -> Result<(), String> {
        let mut depth = 1; // Track nesting level

        loop {
            let token = match self.next_token()? {
                Some(t) => t.to_uppercase(),
                None => return Err("[IF] without matching [THEN]".to_string()),
            };

            match token.as_str() {
                "[IF]" => depth += 1,
                "[ELSE]" => {
                    if depth == 1 {
                        // Found matching [ELSE], stop skipping
                        return Ok(());
                    }
                }
                "[THEN]" => {
                    depth -= 1;
                    if depth == 0 {
                        // Found matching [THEN], stop skipping
                        return Ok(());
                    }
                }
                _ => {
                    // Skip this token
                }
            }
        }
    }

    /// Skip tokens until matching [THEN], handling nested [IF]s
    fn skip_until_then(&mut self) -> Result<(), String> {
        let mut depth = 1; // Track nesting level

        loop {
            let token = match self.next_token()? {
                Some(t) => t.to_uppercase(),
                None => return Err("[ELSE] without matching [THEN]".to_string()),
            };

            match token.as_str() {
                "[IF]" => depth += 1,
                "[THEN]" => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                _ => {
                    // Skip this token
                }
            }
        }
    }

    /// S" - Create string literal (address and length on stack)
    fn immediate_s_quote(&mut self) -> Result<(), String> {
        // Parse string literal from current input
        // Note: next_token already consumed the S" token and updated pos
        // Now we need to skip any whitespace and parse until closing "
        self.sync_pos_from_to_in();

        // Skip one space if present (standard delimiter after S")
        if self.pos < self.input.len() && self.input.as_bytes()[self.pos] == b' ' {
            self.pos += 1;
        }

        let text = self.parse_string_literal()
            .map_err(|_| "S\" missing closing quote".to_string())?;
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
