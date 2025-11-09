// compiler.rs - Forth Compiler (AST → Executable Code)

use crate::parser::{Definition, Expression, Program};
use crate::primitives::{Primitive, VM};
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
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompileError::UndefinedWord(w) => write!(f, "Undefined word: {}", w),
            CompileError::InvalidControlFlow(msg) => write!(f, "Invalid control flow: {}", msg),
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
        // Use the centralized primitive list from Primitive::all()
        for &(name, prim) in Primitive::all() {
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

            Definition::Constant { name, value } => {
                // Constants are compile-time values stored in the definition
                // When called, they just push their literal value
                let instructions = vec![Instruction::Literal(value)];

                let compiled = CompiledWord {
                    name: name.clone(),
                    instructions,
                };
                self.dictionary.insert(name, compiled);

                Ok(())
            }

            Definition::Word { name, body } => {
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
                self.current_instructions.clear(); // Start fresh
                self.compile_expression(expr)?;
                // Move current instructions to immediate storage
                self.immediate_instructions
                    .append(&mut self.current_instructions);
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
                self.current_instructions
                    .push(Instruction::Primitive(Primitive::Type));
            }

            Expression::Recurse => {
                // Emit a recursive call to the word currently being defined
                if let Some(word_name) = &self.current_word_name {
                    self.current_instructions
                        .push(Instruction::Call(word_name.clone()));
                } else {
                    return Err(CompileError::UndefinedWord(
                        "RECURSE outside word definition".to_string(),
                    ));
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
                        && let Instruction::Primitive(prim) = compiled.instructions[0]
                    {
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

            Expression::If {
                condition,
                then_branch,
            } => {
                // Condition is already evaluated and on the stack
                // Just verify it's empty as expected
                assert!(
                    condition.is_empty(),
                    "IF condition should be empty (evaluated before IF)"
                );

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
                self.current_instructions[branch_addr] = Instruction::BranchIfZero(offset);
            }

            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                // Condition is already evaluated and on the stack
                assert!(
                    condition.is_empty(),
                    "IF condition should be empty (evaluated before IF)"
                );

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
                self.current_instructions[if_branch_addr] = Instruction::BranchIfZero(if_offset);

                // Compile else branch
                for expr in else_branch {
                    self.compile_expression(expr)?;
                }

                // Backpatch the else jump to here (after else)
                let after_else = self.current_instructions.len();
                let else_offset = (after_else as isize) - (else_jump_addr as isize) - 1;
                self.current_instructions[else_jump_addr] = Instruction::Branch(else_offset);
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
                self.current_instructions
                    .push(Instruction::BranchIfZero(offset));
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
                self.current_instructions[exit_branch] = Instruction::BranchIfZero(exit_offset);
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
                self.current_instructions
                    .push(Instruction::PlusLoop(offset));

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
                self.current_instructions
                    .push(Instruction::PlusLoop(offset));

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

    /// Get all compiled words (for AOT compilation)
    pub fn compiled_words(&self) -> &std::collections::HashMap<String, CompiledWord> {
        &self.dictionary
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
    llvm_compiler: crate::llvm_jit::LLVMCompiler<'static>,
    should_exit: bool,
    constants: std::collections::HashMap<String, i64>,
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
            llvm_compiler,
            should_exit: false,
            constants: std::collections::HashMap::new(),
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

        // Use execute_program to properly handle expressions and constants
        executor.execute_program(program)?;

        Ok(executor)
    }

    /// Compile and execute a program
    pub fn execute_program(&mut self, program: Program) -> Result<(), String> {
        // We need to execute expressions BEFORE constant definitions
        // because constants consume values from the stack
        let mut modified_program = Program { definitions: Vec::new() };
        let mut pending_expressions = Vec::new();
        let mut expressions_for_immediate = Vec::new();

        for definition in program.definitions {
            match definition {
                crate::parser::Definition::Expression(expr) => {
                    // Accumulate expressions - we'll decide what to do with them
                    // when we see what comes next
                    pending_expressions.push(expr);
                }
                crate::parser::Definition::Constant { name, value: _ } => {
                    // Execute any pending expressions first (they provide the value)
                    for expr in pending_expressions.drain(..) {
                        self.compiler.current_instructions.clear();
                        self.compiler.compile_expression(expr).map_err(|e| e.to_string())?;
                        let instructions = self.compiler.current_instructions.clone();
                        self.execute_instructions(&instructions)?;
                    }

                    // Pop the value from the stack for the constant
                    let stack_value = self.vm.data_stack.pop().map_err(|e| format!("CONSTANT {}: {}", name, e))?;
                    self.constants.insert(name.clone(), stack_value);
                    // Add the constant with its value to the program
                    modified_program.definitions.push(crate::parser::Definition::Constant {
                        name,
                        value: stack_value
                    });
                }
                other => {
                    // For other definitions (Word, Variable), save pending expressions
                    // to be compiled later (after all definitions are available)
                    expressions_for_immediate.extend(pending_expressions.drain(..));
                    modified_program.definitions.push(other);
                }
            }
        }

        // Add any remaining pending expressions
        expressions_for_immediate.extend(pending_expressions);

        // Check for redefinitions and print warning
        for definition in &modified_program.definitions {
            let name = match definition {
                crate::parser::Definition::Word { name, .. } => Some(name),
                crate::parser::Definition::Variable { name} => Some(name),
                crate::parser::Definition::Constant { name, .. } => Some(name),
                _ => None,
            };

            if let Some(name) = name
                && self.compiler.get_word(name).is_some()
            {
                // This is a redefinition - warn the user
                print!("Warning: redefining {} ", name.to_uppercase());
            }
        }

        // Compile to Forth bytecode
        self.compiler
            .compile_program(modified_program)
            .map_err(|e| e.to_string())?;

        // Now compile the expressions that should be executed immediately
        // (after all word definitions are available)
        for expr in expressions_for_immediate {
            self.compiler.current_instructions.clear();
            self.compiler.compile_expression(expr).map_err(|e| e.to_string())?;
            self.compiler.immediate_instructions.append(&mut self.compiler.current_instructions);
        }

        // Compile all words to LLVM (will automatically recompile redefined words)
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
            let word = self
                .compiler
                .get_word(name)
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
        for &value in data_stack.iter().take(data_len) {
            self.vm.data_stack.push(value);
        }

        self.vm.return_stack = crate::primitives::ReturnStack::new();
        for &value in return_stack.iter().take(return_len) {
            self.vm.return_stack.push(value);
        }

        self.vm.loop_stack = crate::primitives::Stack::new();
        for &value in loop_stack.iter().take(loop_len) {
            self.vm.loop_stack.push(value);
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
        static EXEC_COUNTER: std::sync::atomic::AtomicUsize =
            std::sync::atomic::AtomicUsize::new(0);
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
        for &value in data_stack.iter().take(data_len) {
            self.vm.data_stack.push(value);
        }

        self.vm.return_stack = crate::primitives::ReturnStack::new();
        for &value in return_stack.iter().take(return_len) {
            self.vm.return_stack.push(value);
        }

        self.vm.loop_stack = crate::primitives::Stack::new();
        for &value in loop_stack.iter().take(loop_len) {
            self.vm.loop_stack.push(value);
        }

        self.vm.memory.copy_from_slice(&memory);
        self.vm.here = here;

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
