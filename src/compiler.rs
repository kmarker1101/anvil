// compiler.rs - Forth Compiler (AST → Executable Code)

use crate::parser::{Program, Definition, Expression};
use crate::primitives::{VM, Primitive};
use std::collections::HashMap;

// ============================================================================
// COMPILED CODE - Internal representation
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Push a literal number onto the stack
    Literal(i64),

    /// Execute a primitive operation
    Primitive(Primitive),

    /// Call a user-defined word
    Call(String),

    /// Conditional branch: jump if top of stack is 0
    BranchIfZero(isize),

    /// Unconditional branch
    Branch(isize),

    /// Return from current word
    Return,

    /// DO loop setup: ( limit start -- ) pushes both to loop stack
    DoSetup,

    /// ?DO loop setup: ( limit start -- ) pushes both to loop stack, or skips if equal
    QDoSetup(isize),

    /// LOOP increment: increment index by 1, branch back if index < limit
    Loop(isize),

    /// +LOOP increment: ( n -- ) increment index by n, branch back appropriately
    PlusLoop(isize),

    /// Clean up loop stack after loop completes
    LoopEnd,

    /// Allocate string in memory and push (addr len) onto stack
    AllocString(String),

    /// Push variable address onto stack
    VariableAddr(String),

    /// Exit the program
    Bye,
}

#[derive(Debug, Clone)]
pub struct CompiledWord {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

// ============================================================================
// COMPILER
// ============================================================================

pub struct Compiler {
    /// Dictionary: word name → compiled code
    dictionary: HashMap<String, CompiledWord>,

    /// Current instruction sequence being built
    current_instructions: Vec<Instruction>,

    /// Stack of branch addresses for backpatching
    branch_stack: Vec<usize>,

    /// Temporary storage for immediate expression instructions
    immediate_instructions: Vec<Instruction>,

    /// Name of word currently being compiled (for RECURSE)
    current_word_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompileError {
    UndefinedWord(String),
    InvalidControlFlow(String),
    DuplicateDefinition(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompileError::UndefinedWord(w) => write!(f, "Undefined word: {}", w),
            CompileError::InvalidControlFlow(msg) => write!(f, "Invalid control flow: {}", msg),
            CompileError::DuplicateDefinition(w) => write!(f, "Duplicate definition: {}", w),
        }
    }
}

impl std::error::Error for CompileError {}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Compiler {
            dictionary: HashMap::new(),
            current_instructions: Vec::new(),
            branch_stack: Vec::new(),
            immediate_instructions: Vec::new(),
            current_word_name: None,
        };

        // Install primitive words in dictionary
        compiler.install_primitives();

        compiler
    }

    fn install_primitives(&mut self) {
        let primitives = vec![
            // Memory
            ("@", Primitive::Fetch),
            ("!", Primitive::Store),
            ("C@", Primitive::CFetch),
            ("C!", Primitive::CStore),

            // Stack
            ("DUP", Primitive::Dup),
            ("DROP", Primitive::Drop),
            ("SWAP", Primitive::Swap),
            ("OVER", Primitive::Over),
            ("ROT", Primitive::Rot),
            
            // Return stack
            (">R", Primitive::ToR),
            ("R>", Primitive::FromR),
            ("R@", Primitive::RFetch),
            
            // Arithmetic
            ("+", Primitive::Add),
            ("-", Primitive::Sub),
            ("*", Primitive::Mul),
            ("/", Primitive::Div),
            ("MOD", Primitive::Mod),
            
            // Comparison
            ("=", Primitive::Equals),
            ("<", Primitive::Less),
            (">", Primitive::Greater),
            
            // Logical
            ("AND", Primitive::And),
            ("OR", Primitive::Or),
            ("XOR", Primitive::Xor),
            ("INVERT", Primitive::Invert),
            
            // I/O
            ("EMIT", Primitive::Emit),
            ("KEY", Primitive::Key),
            (".", Primitive::Dot),
            ("CR", Primitive::Cr),
            ("TYPE", Primitive::Type),

            // Loop counter
            ("I", Primitive::I),

            // Stack inspection
            ("DEPTH", Primitive::Depth),
        ];

        for (name, prim) in primitives {
            self.dictionary.insert(
                name.to_string(),
                CompiledWord {
                    name: name.to_string(),
                    instructions: vec![Instruction::Primitive(prim)],
                },
            );
        }
    }

    /// Compile a complete program
    pub fn compile_program(&mut self, program: Program) -> Result<(), CompileError> {
        for definition in program.definitions {
            self.compile_definition(definition)?;
        }
        Ok(())
    }

    /// Compile a single definition
    fn compile_definition(&mut self, definition: Definition) -> Result<(), CompileError> {
        match definition {
            Definition::Variable { name } => {
                // Check for duplicate
                if self.dictionary.contains_key(&name) {
                    return Err(CompileError::DuplicateDefinition(name));
                }

                // Variables push their address when called
                // We'll use a special instruction that allocates and returns address
                let instructions = vec![Instruction::VariableAddr(name.clone())];

                let compiled = CompiledWord {
                    name: name.clone(),
                    instructions,
                };
                self.dictionary.insert(name, compiled);

                Ok(())
            }

            Definition::Word { name, body } => {
                // Check for duplicate
                if self.dictionary.contains_key(&name) {
                    return Err(CompileError::DuplicateDefinition(name));
                }

                // Compile the word body
                self.current_instructions.clear();
                self.branch_stack.clear();
                self.current_word_name = Some(name.clone());

                for expr in body {
                    self.compile_expression(expr)?;
                }

                // Add return at the end
                self.current_instructions.push(Instruction::Return);

                // Store in dictionary
                let compiled = CompiledWord {
                    name: name.clone(),
                    instructions: self.current_instructions.clone(),
                };
                self.dictionary.insert(name, compiled);

                // Clear current word name
                self.current_word_name = None;

                Ok(())
            }

            Definition::Expression(expr) => {
                // For REPL: compile standalone expression into immediate_instructions
                self.current_instructions.clear();  // Start fresh
                self.compile_expression(expr)?;
                // Move current instructions to immediate storage
                self.immediate_instructions.append(&mut self.current_instructions);
                Ok(())
            }
        }
    }

    /// Compile a single expression
    fn compile_expression(&mut self, expr: Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Number(n) => {
                self.current_instructions.push(Instruction::Literal(n));
            }

            Expression::String(s) => {
                // S" string literal - allocate in memory and push (addr len)
                self.current_instructions.push(Instruction::AllocString(s));
            }

            Expression::DotQuote(s) => {
                // ." string literal - allocate, then TYPE to print
                self.current_instructions.push(Instruction::AllocString(s));
                self.current_instructions.push(Instruction::Primitive(Primitive::Type));
            }

            Expression::Recurse => {
                // Emit a recursive call to the word currently being defined
                if let Some(word_name) = &self.current_word_name {
                    self.current_instructions.push(Instruction::Call(word_name.clone()));
                } else {
                    return Err(CompileError::UndefinedWord("RECURSE outside word definition".to_string()));
                }
            }

            Expression::Exit => {
                // EXIT causes early return from the current word
                self.current_instructions.push(Instruction::Return);
            }

            Expression::Bye => {
                // BYE exits the program
                self.current_instructions.push(Instruction::Bye);
            }

            Expression::WordCall(word) => {
                // Check if word exists in dictionary
                if self.dictionary.contains_key(&word) {
                    // Check if it's a primitive (single instruction)
                    let compiled = self.dictionary.get(&word).unwrap();
                    if compiled.instructions.len() == 1
                        && let Instruction::Primitive(prim) = compiled.instructions[0] {
                        // Inline primitive
                        self.current_instructions.push(Instruction::Primitive(prim));
                        return Ok(());
                    }
                    
                    // Otherwise, emit a call
                    self.current_instructions.push(Instruction::Call(word));
                } else {
                    return Err(CompileError::UndefinedWord(word));
                }
            }

            Expression::If { condition, then_branch } => {
                // Condition is already evaluated and on the stack
                // Just verify it's empty as expected
                assert!(condition.is_empty(), "IF condition should be empty (evaluated before IF)");

                // Emit conditional branch: if top of stack is 0, skip the THEN branch
                self.current_instructions.push(Instruction::BranchIfZero(0));
                let branch_addr = self.current_instructions.len() - 1;

                // Compile THEN branch
                for expr in then_branch {
                    self.compile_expression(expr)?;
                }

                // Backpatch the branch to jump here (after THEN branch)
                let after_then = self.current_instructions.len();
                let offset = (after_then as isize) - (branch_addr as isize) - 1;
                self.current_instructions[branch_addr] =
                    Instruction::BranchIfZero(offset);
            }

            Expression::IfElse { condition, then_branch, else_branch } => {
                // Condition is already evaluated and on the stack
                assert!(condition.is_empty(), "IF condition should be empty (evaluated before IF)");

                // Branch to else if condition is false (0)
                self.current_instructions.push(Instruction::BranchIfZero(0));
                let if_branch_addr = self.current_instructions.len() - 1;

                // Compile THEN branch (executed when condition is true)
                for expr in then_branch {
                    self.compile_expression(expr)?;
                }

                // Jump over else branch
                self.current_instructions.push(Instruction::Branch(0));
                let else_jump_addr = self.current_instructions.len() - 1;

                // Backpatch the if branch to here (start of else)
                let else_start = self.current_instructions.len();
                let if_offset = (else_start as isize) - (if_branch_addr as isize) - 1;
                self.current_instructions[if_branch_addr] =
                    Instruction::BranchIfZero(if_offset);

                // Compile else branch
                for expr in else_branch {
                    self.compile_expression(expr)?;
                }

                // Backpatch the else jump to here (after else)
                let after_else = self.current_instructions.len();
                let else_offset = (after_else as isize) - (else_jump_addr as isize) - 1;
                self.current_instructions[else_jump_addr] =
                    Instruction::Branch(else_offset);
            }

            Expression::BeginUntil { body } => {
                // Mark loop start
                let loop_start = self.current_instructions.len();

                // Compile loop body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // Branch back to start if top of stack is 0
                let offset = (loop_start as isize) - (self.current_instructions.len() as isize) - 1;
                self.current_instructions.push(Instruction::BranchIfZero(offset));
            }

            Expression::BeginWhileRepeat { condition, body } => {
                // Mark loop start
                let loop_start = self.current_instructions.len();

                // Compile condition
                for expr in condition {
                    self.compile_expression(expr)?;
                }

                // Branch to end if condition is false
                self.current_instructions.push(Instruction::BranchIfZero(0));
                let exit_branch = self.current_instructions.len() - 1;

                // Compile body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // Branch back to start
                let offset = (loop_start as isize) - (self.current_instructions.len() as isize) - 1;
                self.current_instructions.push(Instruction::Branch(offset));

                // Backpatch exit branch
                let after_loop = self.current_instructions.len();
                let exit_offset = (after_loop as isize) - (exit_branch as isize) - 1;
                self.current_instructions[exit_branch] = 
                    Instruction::BranchIfZero(exit_offset);
            }

            Expression::DoLoop { body } => {
                // DO ( limit start -- )
                // Setup loop: push limit and start to loop stack
                self.current_instructions.push(Instruction::DoSetup);

                let loop_start = self.current_instructions.len();

                // Compile loop body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // LOOP: increment and branch back if not done
                let offset = loop_start as isize - (self.current_instructions.len() + 1) as isize;
                self.current_instructions.push(Instruction::Loop(offset));

                // Clean up loop stack
                self.current_instructions.push(Instruction::LoopEnd);
            }

            Expression::DoPlusLoop { body } => {
                // DO ( limit start -- )
                self.current_instructions.push(Instruction::DoSetup);

                let loop_start = self.current_instructions.len();

                // Compile loop body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // +LOOP: increment by n and branch back if not done
                let offset = loop_start as isize - (self.current_instructions.len() + 1) as isize;
                self.current_instructions.push(Instruction::PlusLoop(offset));

                // Clean up loop stack
                self.current_instructions.push(Instruction::LoopEnd);
            }

            Expression::QDoLoop { body } => {
                // ?DO ( limit start -- )
                // QDoSetup will skip to after LoopEnd if start == limit
                self.current_instructions.push(Instruction::QDoSetup(0));
                let qdo_addr = self.current_instructions.len() - 1;

                let loop_start = self.current_instructions.len();

                // Compile loop body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // LOOP: increment and branch back if not done
                let offset = loop_start as isize - (self.current_instructions.len() + 1) as isize;
                self.current_instructions.push(Instruction::Loop(offset));

                // Clean up loop stack
                self.current_instructions.push(Instruction::LoopEnd);

                // Backpatch QDoSetup to skip to after LoopEnd
                let skip_offset = self.current_instructions.len() as isize - qdo_addr as isize - 1;
                self.current_instructions[qdo_addr] = Instruction::QDoSetup(skip_offset);
            }

            Expression::QDoPlusLoop { body } => {
                // ?DO ( limit start -- )
                self.current_instructions.push(Instruction::QDoSetup(0));
                let qdo_addr = self.current_instructions.len() - 1;

                let loop_start = self.current_instructions.len();

                // Compile loop body
                for expr in body {
                    self.compile_expression(expr)?;
                }

                // +LOOP: increment by n and branch back if not done
                let offset = loop_start as isize - (self.current_instructions.len() + 1) as isize;
                self.current_instructions.push(Instruction::PlusLoop(offset));

                // Clean up loop stack
                self.current_instructions.push(Instruction::LoopEnd);

                // Backpatch QDoSetup to skip to after LoopEnd
                let skip_offset = self.current_instructions.len() as isize - qdo_addr as isize - 1;
                self.current_instructions[qdo_addr] = Instruction::QDoSetup(skip_offset);
            }
        }

        Ok(())
    }

    /// Get a compiled word from the dictionary
    pub fn get_word(&self, name: &str) -> Option<&CompiledWord> {
        self.dictionary.get(name)
    }

    /// Get all defined words
    pub fn words(&self) -> Vec<String> {
        let mut words: Vec<String> = self.dictionary.keys().cloned().collect();
        words.sort(); // Sort to ensure consistent ordering
        words
    }

    /// Get the immediate instructions (for REPL expressions)
    pub fn take_immediate_instructions(&mut self) -> Vec<Instruction> {
        std::mem::take(&mut self.immediate_instructions)
    }

    /// Check if there are immediate instructions to execute
    pub fn has_immediate_instructions(&self) -> bool {
        !self.immediate_instructions.is_empty()
    }
}

// ============================================================================
// EXECUTOR - Runs compiled code
// ============================================================================

pub struct Executor {
    vm: VM,
    compiler: Compiler,
    variables: HashMap<String, i64>, // variable name -> memory address
    llvm_compiler: crate::llvm_jit::LLVMCompiler<'static>,
    should_exit: bool,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    pub fn new() -> Self {
        // Create a leaked context for 'static lifetime
        let context = Box::leak(Box::new(inkwell::context::Context::create()));
        let llvm_compiler = crate::llvm_jit::LLVMCompiler::new_jit(context, "forth_jit")
            .expect("Failed to create LLVM JIT compiler");

        Executor {
            vm: VM::new(),
            compiler: Compiler::new(),
            variables: HashMap::new(),
            llvm_compiler,
            should_exit: false,
        }
    }

    /// Create a new executor with the standard library loaded
    pub fn with_stdlib() -> Result<Self, String> {
        let mut executor = Self::new();

        // Load stdlib.fth from the embedded file
        const STDLIB: &str = include_str!("stdlib.fth");

        // Convert to uppercase for case-insensitive word matching
        let stdlib_upper = STDLIB.to_uppercase();

        // Parse and compile the standard library
        let mut lexer = crate::lexer::Lexer::new(&stdlib_upper);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().map_err(|e| e.to_string())?;

        executor.compiler.compile_program(program)
            .map_err(|e| e.to_string())?;

        Ok(executor)
    }

    /// Compile and execute a program
    pub fn execute_program(&mut self, program: Program) -> Result<(), String> {
        // Compile to Forth bytecode
        self.compiler.compile_program(program)
            .map_err(|e| e.to_string())?;

        // Compile all words to LLVM
        self.compile_all_words_to_llvm()?;

        // Execute any immediate instructions (from standalone expressions)
        if self.compiler.has_immediate_instructions() {
            let instructions = self.compiler.take_immediate_instructions();
            self.execute_instructions(&instructions)?;
        }

        Ok(())
    }

    /// Compile all Forth words to LLVM
    fn compile_all_words_to_llvm(&mut self) -> Result<(), String> {
        // Get list of all word names
        let word_names = self.compiler.words();

        for name in word_names {
            // Get the word
            if let Some(word) = self.compiler.get_word(&name) {
                let instructions = word.instructions.clone();
                // Compile to LLVM (this will skip if already compiled)
                self.llvm_compiler.compile_word(&name, &instructions)?;
            }
        }

        Ok(())
    }

    /// Execute a specific word
    pub fn execute_word(&mut self, name: &str) -> Result<(), String> {
        // Make sure the word exists in the compiler and get its instructions
        let instructions = {
            let word = self.compiler.get_word(name)
                .ok_or_else(|| format!("Word not found: {}", name))?;
            word.instructions.clone()
        };

        // Compile all words to LLVM (including dependencies)
        self.compile_all_words_to_llvm()?;

        // Ensure the target word is compiled
        self.llvm_compiler.compile_word(name, &instructions)?;

        // Get the JIT-compiled function
        let func = self.llvm_compiler.get_function(name)?;

        // Prepare stacks and memory as raw arrays
        let mut data_stack = vec![0i64; 256];
        let mut return_stack = vec![0i64; 256];
        let mut loop_stack = vec![0i64; 256];
        let mut memory = vec![0u8; 65536];

        // Copy VM state to arrays
        for (i, &val) in self.vm.data_stack.iter().enumerate() {
            data_stack[i] = val;
        }
        let mut data_len = self.vm.data_stack.depth();

        for (i, &val) in self.vm.return_stack.iter().enumerate() {
            return_stack[i] = val;
        }
        let mut return_len = self.vm.return_stack.depth();

        for (i, &val) in self.vm.loop_stack.iter().enumerate() {
            loop_stack[i] = val;
        }
        let mut loop_len = self.vm.loop_stack.depth();

        // Copy memory
        memory.copy_from_slice(&self.vm.memory);
        let mut here = self.vm.here;

        // Execute the JIT-compiled function
        unsafe {
            func(
                data_stack.as_mut_ptr(),
                &mut data_len,
                return_stack.as_mut_ptr(),
                &mut return_len,
                loop_stack.as_mut_ptr(),
                &mut loop_len,
                memory.as_mut_ptr(),
                &mut here,
                &mut self.should_exit,
            );
        }

        // Copy results back to VM
        self.vm.data_stack = crate::primitives::Stack::new();
        for i in 0..data_len {
            self.vm.data_stack.push(data_stack[i]);
        }

        self.vm.return_stack = crate::primitives::ReturnStack::new();
        for i in 0..return_len {
            self.vm.return_stack.push(return_stack[i]);
        }

        self.vm.loop_stack = crate::primitives::Stack::new();
        for i in 0..loop_len {
            self.vm.loop_stack.push(loop_stack[i]);
        }

        self.vm.memory.copy_from_slice(&memory);
        self.vm.here = here;

        Ok(())
    }

    /// Execute a sequence of instructions using LLVM JIT
    fn execute_instructions(&mut self, instructions: &[Instruction]) -> Result<(), String> {
        // First, ensure all words referenced in the instructions are compiled
        self.compile_all_words_to_llvm()?;

        // Use unique names for each immediate execution
        // The ExecutionEngine is created lazily on first use and sees all functions
        static EXEC_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let exec_id = EXEC_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let exec_name = format!("_exec_{}", exec_id);

        // Compile to LLVM
        self.llvm_compiler.compile_word(&exec_name, instructions)?;

        // Get the compiled function (creates ExecutionEngine on first call)
        let func = self.llvm_compiler.get_function(&exec_name)?;

        // Prepare stacks and memory as raw arrays
        let mut data_stack = vec![0i64; 256];
        let mut return_stack = vec![0i64; 256];
        let mut loop_stack = vec![0i64; 256];
        let mut memory = vec![0u8; 65536];

        // Copy VM state to arrays
        for (i, &val) in self.vm.data_stack.iter().enumerate() {
            data_stack[i] = val;
        }
        let mut data_len = self.vm.data_stack.depth();

        for (i, &val) in self.vm.return_stack.iter().enumerate() {
            return_stack[i] = val;
        }
        let mut return_len = self.vm.return_stack.depth();

        for (i, &val) in self.vm.loop_stack.iter().enumerate() {
            loop_stack[i] = val;
        }
        let mut loop_len = self.vm.loop_stack.depth();

        // Copy memory
        memory.copy_from_slice(&self.vm.memory);
        let mut here = self.vm.here;

        // Execute the JIT-compiled function
        unsafe {
            func(
                data_stack.as_mut_ptr(),
                &mut data_len,
                return_stack.as_mut_ptr(),
                &mut return_len,
                loop_stack.as_mut_ptr(),
                &mut loop_len,
                memory.as_mut_ptr(),
                &mut here,
                &mut self.should_exit,
            );
        }

        // Copy results back to VM
        self.vm.data_stack = crate::primitives::Stack::new();
        for i in 0..data_len {
            self.vm.data_stack.push(data_stack[i]);
        }

        self.vm.return_stack = crate::primitives::ReturnStack::new();
        for i in 0..return_len {
            self.vm.return_stack.push(return_stack[i]);
        }

        self.vm.loop_stack = crate::primitives::Stack::new();
        for i in 0..loop_len {
            self.vm.loop_stack.push(loop_stack[i]);
        }

        self.vm.memory.copy_from_slice(&memory);
        self.vm.here = here;

        Ok(())
    }

    /// OLD INTERPRETER VERSION - keeping for reference, will be removed
    #[allow(dead_code)]
    fn execute_instructions_interpreter(&mut self, instructions: &[Instruction]) -> Result<(), String> {
        let mut pc = 0; // Program counter

        while pc < instructions.len() {
            match &instructions[pc] {
                Instruction::Literal(n) => {
                    self.vm.data_stack.push(*n);
                    pc += 1;
                }

                Instruction::Primitive(prim) => {
                    self.vm.execute_primitive(*prim)
                        .map_err(|e| e.to_string())?;
                    pc += 1;
                }

                Instruction::Call(word) => {
                    let called_word = self.compiler.get_word(word)
                        .ok_or_else(|| format!("Word not found: {}", word))?
                        .clone();
                    self.execute_instructions(&called_word.instructions)?;
                    pc += 1;
                }

                Instruction::BranchIfZero(offset) => {
                    let flag = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;
                    if flag == 0 {
                        pc = ((pc as isize) + offset + 1) as usize;
                    } else {
                        pc += 1;
                    }
                }

                Instruction::Branch(offset) => {
                    pc = ((pc as isize) + offset + 1) as usize;
                }

                Instruction::Return => {
                    break;
                }

                Instruction::DoSetup => {
                    // DO ( limit start -- )
                    // Pop start and limit from data stack, push to loop stack
                    let start = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;
                    let limit = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;

                    // Push limit first, then index (so index is on top)
                    self.vm.loop_stack.push(limit);
                    self.vm.loop_stack.push(start);
                    pc += 1;
                }

                Instruction::QDoSetup(skip_offset) => {
                    // ?DO ( limit start -- )
                    // Pop start and limit, skip loop if they're equal
                    let start = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;
                    let limit = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;

                    if start == limit {
                        // Skip the loop entirely
                        pc = ((pc as isize) + skip_offset + 1) as usize;
                    } else {
                        // Push limit first, then index (so index is on top)
                        self.vm.loop_stack.push(limit);
                        self.vm.loop_stack.push(start);
                        pc += 1;
                    }
                }

                Instruction::Loop(offset) => {
                    // LOOP: increment index by 1, branch back if index < limit
                    let index = self.vm.loop_stack.pop()
                        .map_err(|e| e.to_string())?;
                    let limit = self.vm.loop_stack.get(self.vm.loop_stack.depth() - 1)
                        .ok_or("Loop stack underflow")?;

                    let new_index = index + 1;

                    if new_index < limit {
                        // Continue looping
                        self.vm.loop_stack.push(new_index);
                        pc = ((pc as isize) + offset + 1) as usize;
                    } else {
                        // Exit loop - don't push index back
                        pc += 1;
                    }
                }

                Instruction::PlusLoop(offset) => {
                    // +LOOP ( n -- ): increment index by n, branch back appropriately
                    let increment = self.vm.data_stack.pop()
                        .map_err(|e| e.to_string())?;
                    let index = self.vm.loop_stack.pop()
                        .map_err(|e| e.to_string())?;
                    let limit = self.vm.loop_stack.get(self.vm.loop_stack.depth() - 1)
                        .ok_or("Loop stack underflow")?;

                    let new_index = index + increment;

                    // Check for loop termination
                    // For positive increment: continue if new_index < limit
                    // For negative increment: continue if new_index > limit
                    let should_continue = if increment >= 0 {
                        new_index < limit
                    } else {
                        new_index > limit
                    };

                    if should_continue {
                        // Continue looping
                        self.vm.loop_stack.push(new_index);
                        pc = ((pc as isize) + offset + 1) as usize;
                    } else {
                        // Exit loop - don't push index back
                        pc += 1;
                    }
                }

                Instruction::LoopEnd => {
                    // Clean up: pop limit from loop stack
                    self.vm.loop_stack.pop()
                        .map_err(|e| e.to_string())?;
                    pc += 1;
                }

                Instruction::AllocString(s) => {
                    // Allocate string in memory and push (addr len)
                    let (addr, len) = self.vm.alloc_string(s)
                        .map_err(|e| e.to_string())?;
                    self.vm.data_stack.push(addr);
                    self.vm.data_stack.push(len);
                    pc += 1;
                }

                Instruction::VariableAddr(name) => {
                    // Get or allocate address for variable
                    let addr = if let Some(&existing_addr) = self.variables.get(name) {
                        existing_addr
                    } else {
                        // Allocate 8 bytes for the variable (i64)
                        let addr = self.vm.here as i64;
                        self.vm.here += 8;
                        self.variables.insert(name.clone(), addr);
                        addr
                    };
                    self.vm.data_stack.push(addr);
                    pc += 1;
                }

                Instruction::Bye => {
                    // Exit the program
                    std::process::exit(0);
                }
            }
        }

        Ok(())
    }

    /// Get reference to VM for stack inspection
    pub fn vm(&self) -> &VM {
        &self.vm
    }

    /// Get mutable reference to VM
    pub fn vm_mut(&mut self) -> &mut VM {
        &mut self.vm
    }

    /// Check if BYE was executed
    pub fn should_exit(&self) -> bool {
        self.should_exit
    }

    /// Get reference to compiler
    pub fn compiler(&self) -> &Compiler {
        &self.compiler
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn compile_and_run(input: &str) -> Result<Executor, String> {
        let mut lexer = Lexer::new(input);
        let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
        
        let mut parser = Parser::new(tokens);
        let program = parser.parse().map_err(|e| e.to_string())?;
        
        let mut executor = Executor::new();
        executor.execute_program(program)?;
        
        Ok(executor)
    }

    #[test]
    fn test_compile_simple_word() {
        let mut executor = compile_and_run(": SQUARE DUP * ;").unwrap();

        // Push 5 and call SQUARE
        executor.vm_mut().data_stack.push(5);
        executor.execute_word("SQUARE").unwrap();

        // Should get 25
        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 25);
    }

    #[test]
    fn test_compile_with_literals() {
        let mut executor = compile_and_run(": DOUBLE 2 * ;").unwrap();

        executor.vm_mut().data_stack.push(21);
        executor.execute_word("DOUBLE").unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_multiple_words() {
        let input = ": SQUARE DUP * ; : CUBE DUP SQUARE * ;";
        let mut executor = compile_and_run(input).unwrap();

        executor.vm_mut().data_stack.push(3);
        executor.execute_word("CUBE").unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 27);
    }

    #[test]
    fn test_arithmetic() {
        let input = ": ADD3 1 + 2 + ;";
        let mut executor = compile_and_run(input).unwrap();

        executor.vm_mut().data_stack.push(10);
        executor.execute_word("ADD3").unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 13);
    }

    #[test]
    fn test_stack_operations() {
        let input = ": SWAP-ADD SWAP + ;";
        let mut executor = compile_and_run(input).unwrap();

        executor.vm_mut().data_stack.push(5);
        executor.vm_mut().data_stack.push(3);
        executor.execute_word("SWAP-ADD").unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 8);
    }

    #[test]
    fn test_undefined_word() {
        let result = compile_and_run(": TEST UNDEFINED ;");
        assert!(result.is_err());
    }

    #[test]
    fn test_word_order() {
        // SQUARE must be defined before CUBE uses it
        let input = ": SQUARE DUP * ; : CUBE DUP SQUARE * ;";
        let executor = compile_and_run(input);
        assert!(executor.is_ok());
    }

    #[test]
    fn test_nested_calls() {
        let input = r#"
            : DOUBLE 2 * ;
            : QUAD DOUBLE DOUBLE ;
        "#;
        let mut executor = compile_and_run(input).unwrap();

        executor.vm_mut().data_stack.push(5);
        executor.execute_word("QUAD").unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 20);
    }

    #[test]
    fn test_immediate_expression() {
        // Test immediate expression execution (REPL mode)
        let mut executor = compile_and_run("5 5 +").unwrap();

        // Should have executed and left result on stack
        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 10);
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_multiple_immediate_expressions() {
        // First expression
        let mut executor = compile_and_run("10 20 +").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 30);

        // Second expression (create new executor due to LLVM ExecutionEngine limitations)
        // TODO: Switch to LLVM ORC JIT to support multiple immediate expressions per executor
        let mut executor2 = compile_and_run("3 DUP *").unwrap();
        assert_eq!(executor2.vm_mut().data_stack.pop().unwrap(), 9);
    }

    #[test]
    fn test_immediate_with_definitions() {
        // Test that definitions don't execute immediately but expressions do
        let input = ": SQUARE DUP * ; 5";
        let mut executor = compile_and_run(input).unwrap();

        // Should have 5 on stack from immediate expression
        assert_eq!(executor.vm().data_stack.depth(), 1);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 5);

        // SQUARE should be defined
        executor.vm_mut().data_stack.push(7);
        executor.execute_word("SQUARE").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 49);
    }

    #[test]
    fn test_immediate_stack_operations() {
        let mut executor = compile_and_run("1 2 3 SWAP DROP").unwrap();

        // Stack should have: 1 3
        assert_eq!(executor.vm().data_stack.depth(), 2);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 3);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);
    }

    #[test]
    fn test_dot_primitive() {
        // Test . (dot) prints and removes from stack
        let mut executor = compile_and_run("42 99 .").unwrap();

        // 99 should have been printed and removed
        assert_eq!(executor.vm().data_stack.depth(), 1);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_dot_in_word_definition() {
        let input = ": PRINT-SQUARE DUP * . ;";
        let mut executor = compile_and_run(input).unwrap();

        executor.vm_mut().data_stack.push(7);
        executor.execute_word("PRINT-SQUARE").unwrap();

        // Should have printed 49, stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_multiple_dots() {
        let executor = compile_and_run("1 2 3 . . .").unwrap();

        // All values should have been printed
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_stdlib_loads() {
        // Test that stdlib loads without errors
        let executor = Executor::with_stdlib();
        if let Err(e) = &executor {
            eprintln!("Stdlib load error: {}", e);
        }
        assert!(executor.is_ok());

        let executor = executor.unwrap();

        // Check that some stdlib words are defined
        assert!(executor.compiler().get_word("2DUP").is_some());
        assert!(executor.compiler().get_word("2DROP").is_some());
        assert!(executor.compiler().get_word("NEGATE").is_some());
        assert!(executor.compiler().get_word("1+").is_some());
        assert!(executor.compiler().get_word("1-").is_some());
    }

    #[test]
    fn test_stdlib_2dup() {
        let mut executor = Executor::with_stdlib().unwrap();

        // Test 2DUP: ( a b -- a b a b )
        executor.vm_mut().data_stack.push(10);
        executor.vm_mut().data_stack.push(20);
        executor.execute_word("2DUP").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 4);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 20);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 20);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
    }

    #[test]
    fn test_stdlib_negate() {
        let mut executor = Executor::with_stdlib().unwrap();

        // Test NEGATE: ( n -- -n )
        executor.vm_mut().data_stack.push(42);
        executor.execute_word("NEGATE").unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -42);
    }

    #[test]
    fn test_stdlib_abs() {
        let mut executor = Executor::with_stdlib().unwrap();

        // First check that 0< is defined
        assert!(executor.compiler().get_word("0<").is_some(), "0< word should be defined");

        // Test 0< directly
        executor.vm_mut().data_stack.push(-42);
        executor.execute_word("0<").unwrap();
        let result = executor.vm_mut().data_stack.pop().unwrap();
        eprintln!("0< with -42 returned: {}", result);
        assert_eq!(result, -1, "0< should return -1 (true) for negative numbers");

        // Check what ABS compiles to
        let abs_word = executor.compiler().get_word("ABS").unwrap();
        eprintln!("ABS instructions: {:?}", abs_word.instructions);

        // Test ABS with negative number
        executor.vm_mut().data_stack.push(-42);
        executor.execute_word("ABS").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);

        // Test ABS with positive number
        executor.vm_mut().data_stack.push(42);
        executor.execute_word("ABS").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_stdlib_min_max() {
        let mut executor = Executor::with_stdlib().unwrap();

        // Test MIN
        executor.vm_mut().data_stack.push(10);
        executor.vm_mut().data_stack.push(20);
        executor.execute_word("MIN").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);

        // Test MAX
        executor.vm_mut().data_stack.push(10);
        executor.vm_mut().data_stack.push(20);
        executor.execute_word("MAX").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 20);
    }

    #[test]
    fn test_do_loop_basic() {
        let mut executor = Executor::new();

        // : TEST 10 0 DO I . LOOP ;
        // Should print: 0 1 2 3 4 5 6 7 8 9
        let source = ": TEST 10 0 DO I LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Execute TEST
        executor.execute_word("TEST").unwrap();

        // Loop should push I values 0-9 onto stack
        assert_eq!(executor.vm().data_stack.depth(), 10);

        // Check values (in reverse order since they were pushed)
        for i in (0..10).rev() {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), i);
        }
    }

    #[test]
    fn test_do_loop_with_computation() {
        let mut executor = Executor::new();

        // : SUM5 0  5 0 DO I + LOOP ;
        // Computes 0 + 1 + 2 + 3 + 4 = 10
        let source = ": SUM5 0  5 0 DO I + LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("SUM5").unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
    }

    #[test]
    fn test_qdo_loop_skip() {
        let mut executor = Executor::new();

        // : TEST 5 5 ?DO I LOOP ;
        // Should skip loop since start == limit
        let source = ": TEST 5 5 ?DO I LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        // Stack should be empty since loop was skipped
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_qdo_loop_execute() {
        let mut executor = Executor::new();

        // : TEST 5 0 ?DO I LOOP ;
        // Should execute since start != limit
        let source = ": TEST 5 0 ?DO I LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        // Loop should push I values 0-4 onto stack
        assert_eq!(executor.vm().data_stack.depth(), 5);

        for i in (0..5).rev() {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), i);
        }
    }

    #[test]
    fn test_plusloop_basic() {
        let mut executor = Executor::new();

        // : TEST 10 0 DO I  2 +LOOP ;
        // Should give: 0 2 4 6 8
        let source = ": TEST 10 0 DO I  2 +LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 5);

        let expected = [0, 2, 4, 6, 8];
        for val in expected.iter().rev() {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), *val);
        }
    }

    #[test]
    fn test_plusloop_negative() {
        let mut executor = Executor::new();

        // : TEST 0 10 DO I  -2 +LOOP ;
        // Should count down: 10 8 6 4 2
        let source = ": TEST 0 10 DO I  -2 +LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 5);

        let expected = [10, 8, 6, 4, 2];
        for val in expected.iter().rev() {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), *val);
        }
    }

    #[test]
    fn test_nested_loops() {
        let mut executor = Executor::new();

        // : TEST 3 0 DO  2 0 DO I LOOP  LOOP ;
        // Outer loop 3 times, inner loop 2 times each
        // Should push: 0 1 (first outer) 0 1 (second outer) 0 1 (third outer)
        let source = ": TEST 3 0 DO  2 0 DO I LOOP  LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 6);

        // Should be 0 1 0 1 0 1 on stack (last pushed on top)
        let expected = [0, 1, 0, 1, 0, 1];
        for val in expected.iter().rev() {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), *val);
        }
    }

    #[test]
    fn test_qdo_plusloop() {
        let mut executor = Executor::new();

        // : TEST 10 10 ?DO I  2 +LOOP ;
        // Should skip since start == limit
        let source = ": TEST 10 10 ?DO I  2 +LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_recurse_factorial() {
        let mut executor = Executor::new();

        // : FACT ( n -- n! )
        //   DUP 1 > IF DUP 1 - RECURSE * THEN ;
        // Factorial: if n > 1, compute (n * (n-1)!)
        let source = ": FACT DUP 1 > IF DUP 1 - RECURSE * THEN ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test 5! = 120
        executor.vm_mut().data_stack.push(5);
        executor.execute_word("FACT").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 120);

        // Test 0! = 0 (our simple version)
        executor.vm_mut().data_stack.push(0);
        executor.execute_word("FACT").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);

        // Test 1! = 1
        executor.vm_mut().data_stack.push(1);
        executor.execute_word("FACT").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);
    }

    #[test]
    fn test_recurse_fibonacci() {
        let mut executor = Executor::new();

        // : FIB ( n -- fib(n) )
        //   DUP 2 < IF DROP 1 ELSE
        //     DUP 1 - RECURSE
        //     SWAP 2 - RECURSE +
        //   THEN ;
        let source = ": FIB DUP 2 < IF DROP 1 ELSE DUP 1 - RECURSE SWAP 2 - RECURSE + THEN ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test fib(0) = 1
        executor.vm_mut().data_stack.push(0);
        executor.execute_word("FIB").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);

        // Test fib(1) = 1
        executor.vm_mut().data_stack.push(1);
        executor.execute_word("FIB").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);

        // Test fib(6) = 13
        executor.vm_mut().data_stack.push(6);
        executor.execute_word("FIB").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 13);
    }

    #[test]
    fn test_recurse_countdown() {
        let mut executor = Executor::new();

        // : COUNTDOWN DUP 0 > IF DUP 1 - RECURSE THEN ;
        // Leaves numbers from n down to 0 on stack
        let source = ": COUNTDOWN DUP 0 > IF DUP 1 - RECURSE THEN ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test countdown from 5 - should push 5 4 3 2 1 0 onto stack
        executor.vm_mut().data_stack.push(5);
        executor.execute_word("COUNTDOWN").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 6);

        // Values are pushed in order: 5, then 4, 3, 2, 1, 0
        // So popping gives us: 0, 1, 2, 3, 4, 5
        for i in 0..=5 {
            assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), i);
        }
    }

    #[test]
    fn test_exit_basic() {
        let mut executor = Executor::new();

        // : TEST 1 2 EXIT 3 4 ;
        // Should only push 1 and 2, then exit before 3 and 4
        let source = ": TEST 1 2 EXIT 3 4 ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        assert_eq!(executor.vm().data_stack.depth(), 2);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 2);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);
    }

    #[test]
    fn test_exit_conditional() {
        let mut executor = Executor::new();

        // : CLAMP ( n -- n' ) DUP 10 > IF DROP 10 EXIT THEN ;
        // If > 10, clamp to 10 and exit early
        let source = ": CLAMP DUP 10 > IF DROP 10 EXIT THEN ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test with value > 10
        executor.vm_mut().data_stack.push(42);
        executor.execute_word("CLAMP").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);

        // Test with value <= 10
        executor.vm_mut().data_stack.push(5);
        executor.execute_word("CLAMP").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 5);
    }

    #[test]
    fn test_exit_in_loop() {
        let mut executor = Executor::new();

        // : FIND5 ( -- ) 10 0 DO I DUP 5 = IF EXIT THEN DROP LOOP ;
        // Exits when i=5, leaving 5 on stack
        let source = ": FIND5 10 0 DO I DUP 5 = IF EXIT THEN DROP LOOP ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("FIND5").unwrap();

        // Should have found 5 and exited, leaving 5 on stack
        assert_eq!(executor.vm().data_stack.depth(), 1);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 5);
    }

    #[test]
    fn test_exit_multiple() {
        let mut executor = Executor::new();

        // : SIGN ( n -- -1|0|1 )
        //   DUP 0 < IF DROP -1 EXIT THEN
        //   DUP 0 > IF DROP 1 EXIT THEN
        //   ;
        // Returns -1 for negative, 1 for positive, 0 for zero
        let source = ": SIGN DUP 0 < IF DROP -1 EXIT THEN DUP 0 > IF DROP 1 EXIT THEN ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test negative
        executor.vm_mut().data_stack.push(-42);
        executor.execute_word("SIGN").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -1);

        // Test positive
        executor.vm_mut().data_stack.push(42);
        executor.execute_word("SIGN").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);

        // Test zero
        executor.vm_mut().data_stack.push(0);
        executor.execute_word("SIGN").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);
    }

    #[test]
    fn test_cr() {
        let mut executor = Executor::new();

        // CR should just execute without errors
        let source = "CR";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_string_literal() {
        let mut executor = Executor::new();

        // S" Hello" should push address and length
        let source = r#"S" Hello""#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Should have addr and len on stack
        assert_eq!(executor.vm().data_stack.depth(), 2);
        let len = executor.vm_mut().data_stack.pop().unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(len, 5); // "Hello" is 5 characters
        assert!(addr >= 0x4000); // Should be allocated at or after 0x4000
    }

    #[test]
    fn test_type_primitive() {
        let mut executor = Executor::new();

        // Manually allocate a string and test TYPE
        let (addr, len) = executor.vm_mut().alloc_string("Test").unwrap();
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().data_stack.push(len);

        executor.execute_word("TYPE").unwrap();

        // Stack should be empty after TYPE
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_string_with_type() {
        let mut executor = Executor::new();

        // S" Hello" TYPE should print "Hello"
        let source = r#"S" Hello" TYPE"#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_multiple_strings() {
        let mut executor = Executor::new();

        // Multiple S" should allocate separately
        let source = r#"S" First" S" Second""#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Should have 4 values on stack: addr1 len1 addr2 len2
        assert_eq!(executor.vm().data_stack.depth(), 4);

        let len2 = executor.vm_mut().data_stack.pop().unwrap();
        let addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let len1 = executor.vm_mut().data_stack.pop().unwrap();
        let addr1 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(len1, 5); // "First"
        assert_eq!(len2, 6); // "Second"
        assert!(addr2 > addr1); // Second string should be after first
    }

    #[test]
    fn test_string_in_word() {
        let mut executor = Executor::new();

        // Define word that uses S" and TYPE
        let source = r#": GREET S" Hello, World!" TYPE CR ;"#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Execute the word
        executor.execute_word("GREET").unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_cr_in_word() {
        let mut executor = Executor::new();

        // : TEST 42 . CR ;
        let source = ": TEST 42 . CR ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        executor.execute_word("TEST").unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_variable_basic() {
        let mut executor = Executor::new();

        // Create a variable and store/fetch a value
        let source = "VARIABLE X";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // X should push its address onto the stack
        executor.execute_word("X").unwrap();
        assert_eq!(executor.vm().data_stack.depth(), 1);
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        assert!(addr > 0); // Address should be valid

        // Store 42 at the variable's address
        executor.vm_mut().data_stack.push(42);
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Store).unwrap();

        // Fetch the value back
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Fetch).unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_variable_multiple() {
        let mut executor = Executor::new();

        // Create multiple variables
        let source = "VARIABLE X VARIABLE Y VARIABLE Z";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Get addresses of all three variables
        executor.execute_word("X").unwrap();
        let addr_x = executor.vm_mut().data_stack.pop().unwrap();

        executor.execute_word("Y").unwrap();
        let addr_y = executor.vm_mut().data_stack.pop().unwrap();

        executor.execute_word("Z").unwrap();
        let addr_z = executor.vm_mut().data_stack.pop().unwrap();

        // Addresses should be different
        assert_ne!(addr_x, addr_y);
        assert_ne!(addr_y, addr_z);
        assert_ne!(addr_x, addr_z);

        // Each variable should be 8 bytes apart (size of i64)
        assert_eq!(addr_y - addr_x, 8);
        assert_eq!(addr_z - addr_y, 8);
    }

    #[test]
    fn test_variable_in_word() {
        let mut executor = Executor::new();

        // Create variable and define word to use it
        let source = r#"
            VARIABLE COUNTER
            : INC ( -- ) COUNTER @ 1 + COUNTER ! ;
            : GET ( -- n ) COUNTER @ ;
        "#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Initialize counter to 0
        executor.execute_word("COUNTER").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(0);
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Store).unwrap();

        // Get initial value
        executor.execute_word("GET").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);

        // Increment three times
        executor.execute_word("INC").unwrap();
        executor.execute_word("INC").unwrap();
        executor.execute_word("INC").unwrap();

        // Check value is now 3
        executor.execute_word("GET").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 3);
    }

    #[test]
    fn test_variable_persistence() {
        let mut executor = Executor::new();

        // Create variable
        let source = "VARIABLE DATA";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Store value
        executor.execute_word("DATA").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(12345);
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Store).unwrap();

        // Execute DATA again - should get same address
        executor.execute_word("DATA").unwrap();
        let addr2 = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(addr, addr2);

        // Value should still be there
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Fetch).unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 12345);
    }

    #[test]
    fn test_variable_with_loop() {
        let mut executor = Executor::new();

        // Use variable as accumulator in loop
        let source = r#"
            VARIABLE SUM
            : SUMTO ( n -- )
                0 SUM !
                0 DO I SUM @ + SUM ! LOOP
            ;
        "#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Sum 0 to 9 (should be 45)
        executor.vm_mut().data_stack.push(10);
        executor.execute_word("SUMTO").unwrap();

        // Fetch the sum
        executor.execute_word("SUM").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(addr);
        executor.vm_mut().execute_primitive(crate::primitives::Primitive::Fetch).unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 45);
    }

    #[test]
    fn test_depth_basic() {
        let mut executor = Executor::new();

        // Empty stack
        let source = "DEPTH";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);
    }

    #[test]
    fn test_depth_with_values() {
        let mut executor = Executor::new();

        // Push 3 values, then check depth
        let source = "1 2 3 DEPTH";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Should have pushed depth (3) on top
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 3);
        // Original values still there
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 3);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 2);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 1);
    }

    #[test]
    fn test_depth_in_word() {
        let mut executor = Executor::new();

        // Define word that uses DEPTH
        let source = ": HOWMANY ( ... -- ... n ) DEPTH ;";
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Test with different stack depths
        executor.execute_word("HOWMANY").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);

        executor.vm_mut().data_stack.push(1);
        executor.vm_mut().data_stack.push(2);
        executor.execute_word("HOWMANY").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 2);
    }

    #[test]
    fn test_dotquote_basic() {
        let mut executor = Executor::new();

        // Test basic ." printing
        let source = r#"." Hello""#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Stack should be empty (." prints and consumes)
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_dotquote_in_word() {
        let mut executor = Executor::new();

        // Define word that uses ."
        let source = r#": GREET ." Hello, World!" ;"#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Execute the word
        executor.execute_word("GREET").unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_dotquote_multiple() {
        let mut executor = Executor::new();

        // Multiple ." statements
        let source = r#"." First" ." Second" ." Third""#;
        let mut lexer = crate::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Stack should be empty
        assert_eq!(executor.vm().data_stack.depth(), 0);
    }

    #[test]
    fn test_fetch_store_primitives() {
        // Due to LLVM ExecutionEngine limitations, combine both operations into one expression
        // TODO: Switch to LLVM ORC JIT to support multiple immediate expressions per executor
        let mut executor = compile_and_run("42 100 ! 100 @").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }
}
