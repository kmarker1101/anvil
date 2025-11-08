// jit.rs - Cranelift JIT/AOT compiler for Forth

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_native;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;
use std::io::{self, Write, Read};
use target_lexicon::Triple;

use crate::compiler::{Instruction, CompiledWord};
use std::collections::HashMap as StdHashMap;

// ============================================================================
// I/O Helper Functions (called from JIT-compiled code)
// ============================================================================

/// EMIT helper: Print a single character
#[unsafe(no_mangle)]
pub extern "C" fn forth_emit(c: i64) {
    let byte = c as u8;
    let _ = io::stdout().write_all(&[byte]);
    let _ = io::stdout().flush();
}

/// KEY helper: Read a single character
#[unsafe(no_mangle)]
pub extern "C" fn forth_key() -> i64 {
    let mut buffer = [0u8; 1];
    match io::stdin().read_exact(&mut buffer) {
        Ok(()) => buffer[0] as i64,
        Err(_) => 0, // Return 0 on error
    }
}

/// DOT helper: Print a number followed by a space
#[unsafe(no_mangle)]
pub extern "C" fn forth_dot(n: i64) {
    print!("{} ", n);
    let _ = io::stdout().flush();
}

/// CR helper: Print a newline
#[unsafe(no_mangle)]
pub extern "C" fn forth_cr() {
    println!();
}

/// TYPE helper: Print a string from memory
#[unsafe(no_mangle)]
pub extern "C" fn forth_type(memory_ptr: *const u8, addr: i64, len: i64) {
    if addr < 0 || len < 0 {
        return;
    }
    unsafe {
        let start = memory_ptr.offset(addr as isize);
        let slice = std::slice::from_raw_parts(start, len as usize);
        let _ = io::stdout().write_all(slice);
        let _ = io::stdout().flush();
    }
}

/// Global flag set by BYE instruction
static BYE_FLAG: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Helper function for BYE - sets a flag to signal exit
/// IMPORTANT: This function must be as simple as possible because it's called from JIT code
#[unsafe(no_mangle)]
pub extern "C" fn forth_bye() {
    // Just set the atomic flag - nothing else
    // Using Relaxed ordering for maximum compatibility
    BYE_FLAG.store(true, std::sync::atomic::Ordering::Relaxed);
}

/// Check if BYE was called
pub fn should_exit_after_bye() -> bool {
    BYE_FLAG.load(std::sync::atomic::Ordering::SeqCst)
}

/// Set the BYE flag (called when BYE is detected in loaded files)
pub fn set_bye_flag() {
    BYE_FLAG.store(true, std::sync::atomic::Ordering::SeqCst);
}

/// Reset the BYE flag (for testing)
pub fn reset_bye_flag() {
    BYE_FLAG.store(false, std::sync::atomic::Ordering::SeqCst);
}

/// JIT compilation mode
pub enum CompilationMode {
    JIT(JITModule),
    AOT(ObjectModule),
}

/// Forth JIT compiler
pub struct ForthJIT {
    /// Cranelift module (either JIT or AOT)
    mode: CompilationMode,
    /// Builder context
    builder_context: FunctionBuilderContext,
    /// Compiled functions
    functions: HashMap<String, FuncId>,
    /// I/O helper function IDs
    io_funcs: IoFunctions,
    /// Whether definitions have been finalized
    finalized: bool,
}

/// Function IDs for I/O helpers
#[derive(Default)]
struct IoFunctions {
    emit: Option<FuncId>,
    key: Option<FuncId>,
    dot: Option<FuncId>,
    cr: Option<FuncId>,
    type_func: Option<FuncId>,
    bye: Option<FuncId>,
}

impl ForthJIT {
    /// Create a new JIT compiler
    pub fn new_jit() -> Result<Self, String> {
        let mut flag_builder = settings::builder();
        // Disable PIC mode to avoid PLT requirement on aarch64
        flag_builder.set("is_pic", "false")
            .map_err(|e| format!("Failed to set is_pic: {}", e))?;
        // Enable colocated libcalls to keep functions closer together
        flag_builder.set("use_colocated_libcalls", "true")
            .map_err(|e| format!("Failed to set use_colocated_libcalls: {}", e))?;
        let flags = settings::Flags::new(flag_builder);

        let isa_builder = cranelift_native::builder()
            .map_err(|e| format!("Failed to create ISA builder: {}", e))?;
        let isa = isa_builder.finish(flags)
            .map_err(|e| format!("Failed to create ISA: {}", e))?;

        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        // Register I/O helper functions with the JIT
        builder.symbol("forth_emit", forth_emit as *const u8);
        builder.symbol("forth_key", forth_key as *const u8);
        builder.symbol("forth_dot", forth_dot as *const u8);
        builder.symbol("forth_cr", forth_cr as *const u8);
        builder.symbol("forth_type", forth_type as *const u8);
        builder.symbol("forth_bye", forth_bye as *const u8);

        let module = JITModule::new(builder);

        Ok(ForthJIT {
            mode: CompilationMode::JIT(module),
            builder_context: FunctionBuilderContext::new(),
            functions: HashMap::new(),
            io_funcs: IoFunctions::default(),
            finalized: false,
        })
    }

    /// Create a new AOT compiler
    pub fn new_aot(triple: Triple) -> Result<Self, String> {
        let settings_builder = settings::builder();
        let flags = settings::Flags::new(settings_builder);

        let builder = ObjectBuilder::new(
            isa::lookup(triple.clone())
                .map_err(|e| format!("Failed to lookup ISA: {}", e))?
                .finish(flags)
                .map_err(|e| format!("Failed to create ISA: {}", e))?,
            "forth_program",
            cranelift_module::default_libcall_names(),
        )
        .map_err(|e| format!("Failed to create object builder: {}", e))?;

        let module = ObjectModule::new(builder);

        Ok(ForthJIT {
            mode: CompilationMode::AOT(module),
            builder_context: FunctionBuilderContext::new(),
            functions: HashMap::new(),
            io_funcs: IoFunctions::default(),
            finalized: false,
        })
    }

    /// Declare an I/O helper function in the module
    fn declare_io_func(&mut self, name: &str, params: &[AbiParam], returns: &[AbiParam]) -> Result<FuncId, String> {
        let module = match &mut self.mode {
            CompilationMode::JIT(m) => m as &mut dyn Module,
            CompilationMode::AOT(m) => m as &mut dyn Module,
        };

        let mut sig = module.make_signature();
        for param in params {
            sig.params.push(*param);
        }
        for ret in returns {
            sig.returns.push(*ret);
        }

        module
            .declare_function(name, Linkage::Import, &sig)
            .map_err(|e| format!("Failed to declare function {}: {}", name, e))
    }

    /// Declare all potentially needed I/O functions upfront
    fn ensure_io_funcs_declared(&mut self) -> Result<(), String> {
        if self.io_funcs.emit.is_none() {
            let func_id = self.declare_io_func("forth_emit", &[AbiParam::new(types::I64)], &[])?;
            self.io_funcs.emit = Some(func_id);
        }
        if self.io_funcs.key.is_none() {
            let func_id = self.declare_io_func("forth_key", &[], &[AbiParam::new(types::I64)])?;
            self.io_funcs.key = Some(func_id);
        }
        if self.io_funcs.dot.is_none() {
            let func_id = self.declare_io_func("forth_dot", &[AbiParam::new(types::I64)], &[])?;
            self.io_funcs.dot = Some(func_id);
        }
        if self.io_funcs.cr.is_none() {
            let func_id = self.declare_io_func("forth_cr", &[], &[])?;
            self.io_funcs.cr = Some(func_id);
        }
        if self.io_funcs.type_func.is_none() {
            let module = match &mut self.mode {
                CompilationMode::JIT(m) => m as &mut dyn Module,
                CompilationMode::AOT(m) => m as &mut dyn Module,
            };
            let ptr_type = module.target_config().pointer_type();
            let func_id = self.declare_io_func(
                "forth_type",
                &[AbiParam::new(ptr_type), AbiParam::new(types::I64), AbiParam::new(types::I64)],
                &[]
            )?;
            self.io_funcs.type_func = Some(func_id);
        }
        if self.io_funcs.bye.is_none() {
            let func_id = self.declare_io_func("forth_bye", &[], &[])?;
            self.io_funcs.bye = Some(func_id);
        }
        Ok(())
    }

    /// Recursively compile all words that are called by the given instructions
    fn compile_dependencies(&mut self, instructions: &[Instruction], dictionary: &StdHashMap<String, CompiledWord>) -> Result<(), String> {
        for instruction in instructions {
            if let Instruction::Call(word_name) = instruction {
                // Check if we've already compiled this word
                if !self.functions.contains_key(word_name) {
                    if let Some(compiled_word) = dictionary.get(word_name) {
                        // Recursively compile it
                        self.compile_word(word_name, &compiled_word.instructions, Some(dictionary))?;
                    } else {
                        return Err(format!("Undefined word: {}", word_name));
                    }
                }
            }
        }
        Ok(())
    }

    /// Compile a Forth word to native code
    ///
    /// The compiled function signature will be:
    /// fn(data_stack_ptr: *mut i64, data_stack_len: *mut usize,
    ///    return_stack_ptr: *mut i64, return_stack_len: *mut usize,
    ///    memory_ptr: *mut u8) -> ()
    ///
    /// This allows the compiled code to directly manipulate VM state.
    ///
    /// If `dictionary` is provided, the compiler can resolve and compile calls to user-defined words.
    pub fn compile_word(&mut self, name: &str, instructions: &[Instruction], dictionary: Option<&StdHashMap<String, CompiledWord>>) -> Result<(), String> {
        // Declare I/O functions upfront to avoid borrowing issues
        self.ensure_io_funcs_declared()?;

        // First, declare the function and register it to enable recursion
        let (func_id, sig, ptr_type) = {
            // Get the module (works for both JIT and AOT)
            let module = match &mut self.mode {
                CompilationMode::JIT(m) => m as &mut dyn Module,
                CompilationMode::AOT(m) => m as &mut dyn Module,
            };

            // Create function signature that takes VM state pointers
            // Parameters: data_stack_ptr, data_stack_len_ptr, return_stack_ptr, return_stack_len_ptr,
            //             loop_stack_ptr, loop_stack_len_ptr, memory_ptr, here_ptr
            let mut sig = module.make_signature();
            let ptr_type = module.target_config().pointer_type();
            sig.params.push(AbiParam::new(ptr_type)); // data_stack_ptr
            sig.params.push(AbiParam::new(ptr_type)); // data_stack_len_ptr
            sig.params.push(AbiParam::new(ptr_type)); // return_stack_ptr
            sig.params.push(AbiParam::new(ptr_type)); // return_stack_len_ptr
            sig.params.push(AbiParam::new(ptr_type)); // loop_stack_ptr
            sig.params.push(AbiParam::new(ptr_type)); // loop_stack_len_ptr
            sig.params.push(AbiParam::new(ptr_type)); // memory_ptr
            sig.params.push(AbiParam::new(ptr_type)); // here_ptr (points to usize)

            // Declare the function
            let func_id = module
                .declare_function(name, Linkage::Export, &sig)
                .map_err(|e| format!("Failed to declare function: {}", e))?;

            (func_id, sig, ptr_type)
        };

        // Register the function ID BEFORE compiling dependencies to enable recursion
        self.functions.insert(name.to_string(), func_id);

        // Pre-compile all called words (including recursive calls) now that we're registered
        if let Some(dict) = dictionary {
            self.compile_dependencies(instructions, dict)?;
        }

        // Now get module again for building the function body
        let module = match &mut self.mode {
            CompilationMode::JIT(m) => m as &mut dyn Module,
            CompilationMode::AOT(m) => m as &mut dyn Module,
        };

        // Create the function context
        let mut ctx = module.make_context();
        ctx.func.signature = sig;

        // Build the function body
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Get function parameters
            let params = builder.block_params(entry_block).to_vec();
            let data_stack_ptr = params[0];
            let data_stack_len_ptr = params[1];
            let return_stack_ptr = params[2];
            let return_stack_len_ptr = params[3];
            let loop_stack_ptr = params[4];
            let loop_stack_len_ptr = params[5];
            let memory_ptr = params[6];
            let here_ptr = params[7];

            // First pass: Create a block for each instruction to enable branching
            // IMPORTANT: Create separate blocks for ALL instructions, not using entry_block
            // This is because entry_block cannot be a jump target (Cranelift restriction)
            let mut blocks = Vec::new();
            for _ in 0..instructions.len() {
                blocks.push(builder.create_block());
            }

            // Handle empty word case (no instructions)
            if instructions.is_empty() {
                // Just return immediately
                builder.ins().return_(&[]);
            } else {
                // Jump from entry_block to the first instruction's block
                builder.ins().jump(blocks[0], &[]);

                // Track if we've already added a terminator (return/branch) for the LAST instruction
                let mut last_instr_has_terminator = false;
                // Track which blocks have been filled (switched to and had code added)
                let mut filled_blocks = vec![false; blocks.len()];

            // Compile each instruction
            for (idx, instruction) in instructions.iter().enumerate() {
                // Switch to this instruction's block
                builder.switch_to_block(blocks[idx]);
                filled_blocks[idx] = true;

                // Reset terminator flag for this instruction
                let mut instr_has_terminator = false;

                match instruction {
                    Instruction::Literal(value) => {
                        // Push literal onto stack
                        // stack[len] = value; len++
                        let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                        let offset = builder.ins().imul_imm(len, 8); // i64 is 8 bytes
                        let elem_ptr = builder.ins().iadd(data_stack_ptr, offset);
                        let val = builder.ins().iconst(types::I64, *value);
                        builder.ins().store(MemFlags::new(), val, elem_ptr, 0);
                        let new_len = builder.ins().iadd_imm(len, 1);
                        builder.ins().store(MemFlags::new(), new_len, data_stack_len_ptr, 0);
                    }
                    Instruction::Primitive(prim) => {
                        use crate::primitives::Primitive;
                        match prim {
                            // Arithmetic operations (binary: pop two, push one)
                            Primitive::Add | Primitive::Sub | Primitive::Mul | Primitive::Div | Primitive::Mod |
                            Primitive::And | Primitive::Or | Primitive::Xor |
                            Primitive::Equals | Primitive::Less | Primitive::Greater => {
                                // Pop two values, operate, push result
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                // Pop b: len--, b = stack[len]
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_b = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_b = builder.ins().iadd(data_stack_ptr, offset_b);
                                let b = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_b, 0);

                                // Pop a: len--, a = stack[len]
                                let len_minus_2 = builder.ins().iadd_imm(len_minus_1, -1);
                                let offset_a = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_a = builder.ins().iadd(data_stack_ptr, offset_a);
                                let a = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_a, 0);

                                // Perform operation
                                let result = match prim {
                                    Primitive::Add => builder.ins().iadd(a, b),
                                    Primitive::Sub => builder.ins().isub(a, b),
                                    Primitive::Mul => builder.ins().imul(a, b),
                                    Primitive::Div => builder.ins().sdiv(a, b),
                                    Primitive::Mod => builder.ins().srem(a, b),
                                    Primitive::And => builder.ins().band(a, b),
                                    Primitive::Or => builder.ins().bor(a, b),
                                    Primitive::Xor => builder.ins().bxor(a, b),
                                    Primitive::Equals => {
                                        let cmp = builder.ins().icmp(IntCC::Equal, a, b);
                                        // Convert bool to Forth convention: -1 for true, 0 for false
                                        // select(cmp, -1, 0)
                                        let minus_one = builder.ins().iconst(types::I64, -1);
                                        let zero = builder.ins().iconst(types::I64, 0);
                                        builder.ins().select(cmp, minus_one, zero)
                                    }
                                    Primitive::Less => {
                                        let cmp = builder.ins().icmp(IntCC::SignedLessThan, a, b);
                                        let minus_one = builder.ins().iconst(types::I64, -1);
                                        let zero = builder.ins().iconst(types::I64, 0);
                                        builder.ins().select(cmp, minus_one, zero)
                                    }
                                    Primitive::Greater => {
                                        let cmp = builder.ins().icmp(IntCC::SignedGreaterThan, a, b);
                                        let minus_one = builder.ins().iconst(types::I64, -1);
                                        let zero = builder.ins().iconst(types::I64, 0);
                                        builder.ins().select(cmp, minus_one, zero)
                                    }
                                    _ => unreachable!(),
                                };

                                // Push: stack[len++] = result
                                builder.ins().store(MemFlags::new(), result, elem_ptr_a, 0);
                                let new_len = builder.ins().iadd_imm(len_minus_2, 1);
                                builder.ins().store(MemFlags::new(), new_len, data_stack_len_ptr, 0);
                            }

                            // Stack manipulation
                            Primitive::Dup => {
                                // Duplicate top of stack: stack[len] = stack[len-1]; len++
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_top = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_top = builder.ins().iadd(data_stack_ptr, offset_top);
                                let value = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_top, 0);

                                let offset_new = builder.ins().imul_imm(len, 8);
                                let elem_ptr_new = builder.ins().iadd(data_stack_ptr, offset_new);
                                builder.ins().store(MemFlags::new(), value, elem_ptr_new, 0);

                                let new_len = builder.ins().iadd_imm(len, 1);
                                builder.ins().store(MemFlags::new(), new_len, data_stack_len_ptr, 0);
                            }

                            Primitive::Drop => {
                                // Remove top of stack: len--
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let new_len = builder.ins().iadd_imm(len, -1);
                                builder.ins().store(MemFlags::new(), new_len, data_stack_len_ptr, 0);
                            }

                            Primitive::Swap => {
                                // Swap top two: tmp = stack[len-1]; stack[len-1] = stack[len-2]; stack[len-2] = tmp
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_top = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_top = builder.ins().iadd(data_stack_ptr, offset_top);
                                let top = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_top, 0);

                                let len_minus_2 = builder.ins().iadd_imm(len, -2);
                                let offset_second = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_second = builder.ins().iadd(data_stack_ptr, offset_second);
                                let second = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_second, 0);

                                builder.ins().store(MemFlags::new(), second, elem_ptr_top, 0);
                                builder.ins().store(MemFlags::new(), top, elem_ptr_second, 0);
                            }

                            Primitive::Over => {
                                // Copy second to top: stack[len] = stack[len-2]; len++
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                let len_minus_2 = builder.ins().iadd_imm(len, -2);
                                let offset_second = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_second = builder.ins().iadd(data_stack_ptr, offset_second);
                                let second = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_second, 0);

                                let offset_new = builder.ins().imul_imm(len, 8);
                                let elem_ptr_new = builder.ins().iadd(data_stack_ptr, offset_new);
                                builder.ins().store(MemFlags::new(), second, elem_ptr_new, 0);

                                let new_len = builder.ins().iadd_imm(len, 1);
                                builder.ins().store(MemFlags::new(), new_len, data_stack_len_ptr, 0);
                            }

                            Primitive::Rot => {
                                // Rotate third to top: (a b c -- b c a)
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_c = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_c = builder.ins().iadd(data_stack_ptr, offset_c);
                                let c = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_c, 0);

                                let len_minus_2 = builder.ins().iadd_imm(len, -2);
                                let offset_b = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_b = builder.ins().iadd(data_stack_ptr, offset_b);
                                let b = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_b, 0);

                                let len_minus_3 = builder.ins().iadd_imm(len, -3);
                                let offset_a = builder.ins().imul_imm(len_minus_3, 8);
                                let elem_ptr_a = builder.ins().iadd(data_stack_ptr, offset_a);
                                let a = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_a, 0);

                                builder.ins().store(MemFlags::new(), b, elem_ptr_a, 0);
                                builder.ins().store(MemFlags::new(), c, elem_ptr_b, 0);
                                builder.ins().store(MemFlags::new(), a, elem_ptr_c, 0);
                            }

                            Primitive::Invert => {
                                // Bitwise NOT: stack[len-1] = ~stack[len-1]
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr = builder.ins().iadd(data_stack_ptr, offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);
                                let result = builder.ins().bnot(value);
                                builder.ins().store(MemFlags::new(), result, elem_ptr, 0);
                            }

                            // Memory operations
                            Primitive::Fetch => {
                                // @ ( addr -- n ) - Fetch 64-bit value from memory
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr = builder.ins().iadd(data_stack_ptr, offset);

                                // Get address from stack
                                let addr = builder.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);

                                // Calculate memory location: memory_ptr + addr
                                let mem_loc = builder.ins().iadd(memory_ptr, addr);

                                // Load 64-bit value from memory (little-endian)
                                let value = builder.ins().load(types::I64, MemFlags::new(), mem_loc, 0);

                                // Store back to stack (replacing address with value)
                                builder.ins().store(MemFlags::new(), value, elem_ptr, 0);
                            }

                            Primitive::Store => {
                                // ! ( n addr -- ) - Store 64-bit value to memory
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                // Pop addr
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_addr = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_addr = builder.ins().iadd(data_stack_ptr, offset_addr);
                                let addr = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_addr, 0);

                                // Pop value
                                let len_minus_2 = builder.ins().iadd_imm(len_minus_1, -1);
                                let offset_val = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_val = builder.ins().iadd(data_stack_ptr, offset_val);
                                let value = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_val, 0);

                                // Calculate memory location and store
                                let mem_loc = builder.ins().iadd(memory_ptr, addr);
                                builder.ins().store(MemFlags::new(), value, mem_loc, 0);

                                // Update stack length
                                builder.ins().store(MemFlags::new(), len_minus_2, data_stack_len_ptr, 0);
                            }

                            Primitive::CFetch => {
                                // C@ ( addr -- c ) - Fetch 8-bit value from memory
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr = builder.ins().iadd(data_stack_ptr, offset);

                                // Get address from stack
                                let addr = builder.ins().load(types::I64, MemFlags::new(), elem_ptr, 0);

                                // Calculate memory location
                                let mem_loc = builder.ins().iadd(memory_ptr, addr);

                                // Load 8-bit value and zero-extend to 64-bit
                                let byte = builder.ins().load(types::I8, MemFlags::new(), mem_loc, 0);
                                let value = builder.ins().uextend(types::I64, byte);

                                // Store back to stack
                                builder.ins().store(MemFlags::new(), value, elem_ptr, 0);
                            }

                            Primitive::CStore => {
                                // C! ( c addr -- ) - Store 8-bit value to memory
                                let len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                // Pop addr
                                let len_minus_1 = builder.ins().iadd_imm(len, -1);
                                let offset_addr = builder.ins().imul_imm(len_minus_1, 8);
                                let elem_ptr_addr = builder.ins().iadd(data_stack_ptr, offset_addr);
                                let addr = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_addr, 0);

                                // Pop value
                                let len_minus_2 = builder.ins().iadd_imm(len_minus_1, -1);
                                let offset_val = builder.ins().imul_imm(len_minus_2, 8);
                                let elem_ptr_val = builder.ins().iadd(data_stack_ptr, offset_val);
                                let value_i64 = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_val, 0);

                                // Truncate to 8-bit
                                let value_i8 = builder.ins().ireduce(types::I8, value_i64);

                                // Calculate memory location and store
                                let mem_loc = builder.ins().iadd(memory_ptr, addr);
                                builder.ins().store(MemFlags::new(), value_i8, mem_loc, 0);

                                // Update stack length
                                builder.ins().store(MemFlags::new(), len_minus_2, data_stack_len_ptr, 0);
                            }

                            // Return stack operations
                            Primitive::ToR => {
                                // >R ( n -- ) (R: -- n) - Move from data stack to return stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let return_len = builder.ins().load(types::I64, MemFlags::new(), return_stack_len_ptr, 0);

                                // Pop from data stack
                                let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                                let data_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), data_elem_ptr, 0);

                                // Push to return stack
                                let return_offset = builder.ins().imul_imm(return_len, 8);
                                let return_elem_ptr = builder.ins().iadd(return_stack_ptr, return_offset);
                                builder.ins().store(MemFlags::new(), value, return_elem_ptr, 0);

                                // Update lengths
                                builder.ins().store(MemFlags::new(), data_len_minus_1, data_stack_len_ptr, 0);
                                let return_len_plus_1 = builder.ins().iadd_imm(return_len, 1);
                                builder.ins().store(MemFlags::new(), return_len_plus_1, return_stack_len_ptr, 0);
                            }

                            Primitive::FromR => {
                                // R> ( -- n ) (R: n -- ) - Move from return stack to data stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let return_len = builder.ins().load(types::I64, MemFlags::new(), return_stack_len_ptr, 0);

                                // Pop from return stack
                                let return_len_minus_1 = builder.ins().iadd_imm(return_len, -1);
                                let return_offset = builder.ins().imul_imm(return_len_minus_1, 8);
                                let return_elem_ptr = builder.ins().iadd(return_stack_ptr, return_offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), return_elem_ptr, 0);

                                // Push to data stack
                                let data_offset = builder.ins().imul_imm(data_len, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                builder.ins().store(MemFlags::new(), value, data_elem_ptr, 0);

                                // Update lengths
                                builder.ins().store(MemFlags::new(), return_len_minus_1, return_stack_len_ptr, 0);
                                let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                                builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                            }

                            Primitive::RFetch => {
                                // R@ ( -- n ) (R: n -- n) - Copy top of return stack to data stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let return_len = builder.ins().load(types::I64, MemFlags::new(), return_stack_len_ptr, 0);

                                // Peek from return stack (don't pop)
                                let return_len_minus_1 = builder.ins().iadd_imm(return_len, -1);
                                let return_offset = builder.ins().imul_imm(return_len_minus_1, 8);
                                let return_elem_ptr = builder.ins().iadd(return_stack_ptr, return_offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), return_elem_ptr, 0);

                                // Push to data stack
                                let data_offset = builder.ins().imul_imm(data_len, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                builder.ins().store(MemFlags::new(), value, data_elem_ptr, 0);

                                // Update data stack length only
                                let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                                builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                            }

                            // Stack inspection
                            Primitive::Depth => {
                                // DEPTH ( -- n ) - Push the data stack depth
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                // Push depth onto stack
                                let data_offset = builder.ins().imul_imm(data_len, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                builder.ins().store(MemFlags::new(), data_len, data_elem_ptr, 0);

                                // Update length
                                let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                                builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                            }

                            Primitive::I => {
                                // I ( -- n ) - Push the current loop index onto the data stack
                                // The loop index is at loop_stack[loop_len - 1]
                                let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);

                                // Read the top of the loop stack (current index)
                                let loop_len_minus_1 = builder.ins().iadd_imm(loop_len, -1);
                                let loop_offset = builder.ins().imul_imm(loop_len_minus_1, 8);
                                let loop_elem_ptr = builder.ins().iadd(loop_stack_ptr, loop_offset);
                                let index = builder.ins().load(types::I64, MemFlags::new(), loop_elem_ptr, 0);

                                // Push index onto data stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let data_offset = builder.ins().imul_imm(data_len, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                builder.ins().store(MemFlags::new(), index, data_elem_ptr, 0);

                                // Update data stack length
                                let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                                builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                            }

                            // I/O operations
                            Primitive::Emit => {
                                // EMIT ( c -- ) - Output a character
                                let emit_func = self.io_funcs.emit.unwrap();
                                let func_ref = module.declare_func_in_func(emit_func, &mut builder.func);

                                // Pop value from stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                                let data_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), data_elem_ptr, 0);

                                // Call forth_emit(value)
                                builder.ins().call(func_ref, &[value]);

                                // Update stack length
                                builder.ins().store(MemFlags::new(), data_len_minus_1, data_stack_len_ptr, 0);
                            }

                            Primitive::Key => {
                                // KEY ( -- c ) - Read a character
                                let key_func = self.io_funcs.key.unwrap();
                                let func_ref = module.declare_func_in_func(key_func, &mut builder.func);

                                // Call forth_key() and get result
                                let inst = builder.ins().call(func_ref, &[]);
                                let result = builder.inst_results(inst)[0];

                                // Push result onto stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let data_offset = builder.ins().imul_imm(data_len, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                builder.ins().store(MemFlags::new(), result, data_elem_ptr, 0);

                                // Update stack length
                                let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                                builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                            }

                            Primitive::Dot => {
                                // . ( n -- ) - Print a number
                                let dot_func = self.io_funcs.dot.unwrap();
                                let func_ref = module.declare_func_in_func(dot_func, &mut builder.func);

                                // Pop value from stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                                let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                                let data_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                                let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                                let value = builder.ins().load(types::I64, MemFlags::new(), data_elem_ptr, 0);

                                // Call forth_dot(value)
                                builder.ins().call(func_ref, &[value]);

                                // Update stack length
                                builder.ins().store(MemFlags::new(), data_len_minus_1, data_stack_len_ptr, 0);
                            }

                            Primitive::Cr => {
                                // CR ( -- ) - Print newline
                                let cr_func = self.io_funcs.cr.unwrap();
                                let func_ref = module.declare_func_in_func(cr_func, &mut builder.func);

                                // Call forth_cr()
                                builder.ins().call(func_ref, &[]);
                            }

                            Primitive::Type => {
                                // TYPE ( addr len -- ) - Print string from memory
                                let type_func = self.io_funcs.type_func.unwrap();
                                let func_ref = module.declare_func_in_func(type_func, &mut builder.func);

                                // Pop len and addr from stack
                                let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                                let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                                let offset_len = builder.ins().imul_imm(data_len_minus_1, 8);
                                let elem_ptr_len = builder.ins().iadd(data_stack_ptr, offset_len);
                                let len = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_len, 0);

                                let data_len_minus_2 = builder.ins().iadd_imm(data_len_minus_1, -1);
                                let offset_addr = builder.ins().imul_imm(data_len_minus_2, 8);
                                let elem_ptr_addr = builder.ins().iadd(data_stack_ptr, offset_addr);
                                let addr = builder.ins().load(types::I64, MemFlags::new(), elem_ptr_addr, 0);

                                // Call forth_type(memory_ptr, addr, len)
                                builder.ins().call(func_ref, &[memory_ptr, addr, len]);

                                // Update stack length
                                builder.ins().store(MemFlags::new(), data_len_minus_2, data_stack_len_ptr, 0);
                            }
                        }
                    }
                    Instruction::Return => {
                        // Return from function
                        builder.ins().return_(&[]);
                        instr_has_terminator = true;
                        // Don't break - there may be more code after this return
                        // (e.g., the ELSE branch of an IF...THEN, or code after THEN)
                        // We'll continue to the next instruction/block
                    }

                    Instruction::Branch(offset) => {
                        // Unconditional branch: jump to target instruction
                        let target_idx = ((idx as isize) + offset + 1) as usize;
                        if target_idx < blocks.len() {
                            filled_blocks[target_idx] = true; // Mark target as used
                            builder.ins().jump(blocks[target_idx], &[]);
                        } else {
                            // Branch past end - treat as return
                            builder.ins().return_(&[]);
                        }
                        instr_has_terminator = true; // Don't add fallthrough jump
                    }

                    Instruction::BranchIfZero(offset) => {
                        // Conditional branch: jump if TOS is zero
                        // Pop value from stack
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                        let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                        let data_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                        let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                        let value = builder.ins().load(types::I64, MemFlags::new(), data_elem_ptr, 0);

                        // Update stack length
                        builder.ins().store(MemFlags::new(), data_len_minus_1, data_stack_len_ptr, 0);

                        // Compare value with zero
                        let zero = builder.ins().iconst(types::I64, 0);
                        let is_zero = builder.ins().icmp(IntCC::Equal, value, zero);

                        // Calculate target
                        let target_idx = ((idx as isize) + offset + 1) as usize;
                        let next_idx = idx + 1;

                        if target_idx < blocks.len() && next_idx < blocks.len() {
                            // Don't mark as filled - they'll be marked when switched to
                            builder.ins().brif(is_zero, blocks[target_idx], &[], blocks[next_idx], &[]);
                        } else if target_idx < blocks.len() {
                            // Target exists but no fallthrough - branch or return
                            // Don't mark as filled - it will be marked when switched to
                            builder.ins().brif(is_zero, blocks[target_idx], &[], blocks[target_idx], &[]);
                        } else if next_idx < blocks.len() {
                            // Target past end (treat as return), but next exists
                            // Don't mark as filled - it will be marked when switched to
                            // If zero, return; otherwise jump to next
                            let return_block = builder.create_block();
                            builder.ins().brif(is_zero, return_block, &[], blocks[next_idx], &[]);
                            builder.switch_to_block(return_block);
                            builder.seal_block(return_block);
                            builder.ins().return_(&[]);
                        } else {
                            // Both targets invalid - just return
                            builder.ins().return_(&[]);
                        }
                        instr_has_terminator = true; // Don't add fallthrough jump (brif handles both paths)
                    }

                    Instruction::DoSetup => {
                        // DO ( limit start -- )
                        // Pop start and limit from data stack, push to loop stack
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                        // Pop start (TOS)
                        let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                        let start_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                        let start_ptr = builder.ins().iadd(data_stack_ptr, start_offset);
                        let start = builder.ins().load(types::I64, MemFlags::new(), start_ptr, 0);

                        // Pop limit (second)
                        let data_len_minus_2 = builder.ins().iadd_imm(data_len, -2);
                        let limit_offset = builder.ins().imul_imm(data_len_minus_2, 8);
                        let limit_ptr = builder.ins().iadd(data_stack_ptr, limit_offset);
                        let limit = builder.ins().load(types::I64, MemFlags::new(), limit_ptr, 0);

                        // Update data stack length
                        builder.ins().store(MemFlags::new(), data_len_minus_2, data_stack_len_ptr, 0);

                        // Push limit to loop stack
                        let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);
                        let loop_offset = builder.ins().imul_imm(loop_len, 8);
                        let loop_elem_ptr = builder.ins().iadd(loop_stack_ptr, loop_offset);
                        builder.ins().store(MemFlags::new(), limit, loop_elem_ptr, 0);

                        // Push start to loop stack
                        let loop_len_plus_1 = builder.ins().iadd_imm(loop_len, 1);
                        let loop_offset2 = builder.ins().imul_imm(loop_len_plus_1, 8);
                        let loop_elem_ptr2 = builder.ins().iadd(loop_stack_ptr, loop_offset2);
                        builder.ins().store(MemFlags::new(), start, loop_elem_ptr2, 0);

                        // Update loop stack length
                        let loop_len_plus_2 = builder.ins().iadd_imm(loop_len, 2);
                        builder.ins().store(MemFlags::new(), loop_len_plus_2, loop_stack_len_ptr, 0);
                    }

                    Instruction::QDoSetup(skip_offset) => {
                        // ?DO ( limit start -- )
                        // Pop values and skip loop if start == limit
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);

                        // Pop start (TOS)
                        let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                        let start_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                        let start_ptr = builder.ins().iadd(data_stack_ptr, start_offset);
                        let start = builder.ins().load(types::I64, MemFlags::new(), start_ptr, 0);

                        // Pop limit (second)
                        let data_len_minus_2 = builder.ins().iadd_imm(data_len, -2);
                        let limit_offset = builder.ins().imul_imm(data_len_minus_2, 8);
                        let limit_ptr = builder.ins().iadd(data_stack_ptr, limit_offset);
                        let limit = builder.ins().load(types::I64, MemFlags::new(), limit_ptr, 0);

                        // Update data stack length
                        builder.ins().store(MemFlags::new(), data_len_minus_2, data_stack_len_ptr, 0);

                        // Check if start == limit
                        let is_equal = builder.ins().icmp(IntCC::Equal, start, limit);

                        let skip_target_idx = ((idx as isize) + skip_offset + 1) as usize;
                        let next_idx = idx + 1;

                        // If equal, skip to after loop; otherwise push to loop stack and continue
                        if skip_target_idx < blocks.len() && next_idx < blocks.len() {
                            // Create a block for pushing to loop stack
                            let push_block = builder.create_block();

                            // If equal, skip; else push
                            builder.ins().brif(is_equal, blocks[skip_target_idx], &[], push_block, &[]);

                            // Push block: add limit and start to loop stack
                            builder.switch_to_block(push_block);
                            builder.seal_block(push_block);

                            let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);
                            let loop_offset = builder.ins().imul_imm(loop_len, 8);
                            let loop_elem_ptr = builder.ins().iadd(loop_stack_ptr, loop_offset);
                            builder.ins().store(MemFlags::new(), limit, loop_elem_ptr, 0);

                            let loop_len_plus_1 = builder.ins().iadd_imm(loop_len, 1);
                            let loop_offset2 = builder.ins().imul_imm(loop_len_plus_1, 8);
                            let loop_elem_ptr2 = builder.ins().iadd(loop_stack_ptr, loop_offset2);
                            builder.ins().store(MemFlags::new(), start, loop_elem_ptr2, 0);

                            let loop_len_plus_2 = builder.ins().iadd_imm(loop_len, 2);
                            builder.ins().store(MemFlags::new(), loop_len_plus_2, loop_stack_len_ptr, 0);

                            builder.ins().jump(blocks[next_idx], &[]);
                        }
                        continue; // Don't add fallthrough
                    }

                    Instruction::Loop(offset) => {
                        // Increment index by 1, branch back if index < limit
                        let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);

                        // Get index (top of loop stack)
                        let loop_len_minus_1 = builder.ins().iadd_imm(loop_len, -1);
                        let index_offset = builder.ins().imul_imm(loop_len_minus_1, 8);
                        let index_ptr = builder.ins().iadd(loop_stack_ptr, index_offset);
                        let index = builder.ins().load(types::I64, MemFlags::new(), index_ptr, 0);

                        // Get limit (second from top)
                        let loop_len_minus_2 = builder.ins().iadd_imm(loop_len, -2);
                        let limit_offset = builder.ins().imul_imm(loop_len_minus_2, 8);
                        let limit_ptr = builder.ins().iadd(loop_stack_ptr, limit_offset);
                        let limit = builder.ins().load(types::I64, MemFlags::new(), limit_ptr, 0);

                        // Increment index
                        let new_index = builder.ins().iadd_imm(index, 1);
                        builder.ins().store(MemFlags::new(), new_index, index_ptr, 0);

                        // Compare new_index < limit
                        let continue_loop = builder.ins().icmp(IntCC::SignedLessThan, new_index, limit);

                        let loop_target_idx = ((idx as isize) + offset + 1) as usize;
                        let next_idx = idx + 1;

                        if loop_target_idx < blocks.len() && next_idx < blocks.len() {
                            builder.ins().brif(continue_loop, blocks[loop_target_idx], &[], blocks[next_idx], &[]);
                        }
                        continue; // Don't add fallthrough
                    }

                    Instruction::PlusLoop(offset) => {
                        // Pop increment from data stack, add to index, branch appropriately
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                        let data_len_minus_1 = builder.ins().iadd_imm(data_len, -1);
                        let inc_offset = builder.ins().imul_imm(data_len_minus_1, 8);
                        let inc_ptr = builder.ins().iadd(data_stack_ptr, inc_offset);
                        let increment = builder.ins().load(types::I64, MemFlags::new(), inc_ptr, 0);
                        builder.ins().store(MemFlags::new(), data_len_minus_1, data_stack_len_ptr, 0);

                        let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);

                        // Get index
                        let loop_len_minus_1 = builder.ins().iadd_imm(loop_len, -1);
                        let index_offset = builder.ins().imul_imm(loop_len_minus_1, 8);
                        let index_ptr = builder.ins().iadd(loop_stack_ptr, index_offset);
                        let index = builder.ins().load(types::I64, MemFlags::new(), index_ptr, 0);

                        // Get limit
                        let loop_len_minus_2 = builder.ins().iadd_imm(loop_len, -2);
                        let limit_offset = builder.ins().imul_imm(loop_len_minus_2, 8);
                        let limit_ptr = builder.ins().iadd(loop_stack_ptr, limit_offset);
                        let limit = builder.ins().load(types::I64, MemFlags::new(), limit_ptr, 0);

                        // Add increment to index
                        let new_index = builder.ins().iadd(index, increment);
                        builder.ins().store(MemFlags::new(), new_index, index_ptr, 0);

                        // Check if we should continue: behavior depends on sign of increment
                        // For positive increment: continue if new_index < limit
                        // For negative increment: continue if new_index > limit (strictly greater, not >=)
                        let zero = builder.ins().iconst(types::I64, 0);
                        let inc_positive = builder.ins().icmp(IntCC::SignedGreaterThan, increment, zero);
                        let below_limit = builder.ins().icmp(IntCC::SignedLessThan, new_index, limit);
                        let above_limit = builder.ins().icmp(IntCC::SignedGreaterThan, new_index, limit);

                        // continue_loop = (inc_positive && below_limit) || (!inc_positive && above_limit)
                        let continue_loop = builder.ins().select(inc_positive, below_limit, above_limit);

                        let loop_target_idx = ((idx as isize) + offset + 1) as usize;
                        let next_idx = idx + 1;

                        if loop_target_idx < blocks.len() && next_idx < blocks.len() {
                            builder.ins().brif(continue_loop, blocks[loop_target_idx], &[], blocks[next_idx], &[]);
                        }
                        continue; // Don't add fallthrough
                    }

                    Instruction::LoopEnd => {
                        // Pop limit and index from loop stack
                        let loop_len = builder.ins().load(types::I64, MemFlags::new(), loop_stack_len_ptr, 0);
                        let loop_len_minus_2 = builder.ins().iadd_imm(loop_len, -2);
                        builder.ins().store(MemFlags::new(), loop_len_minus_2, loop_stack_len_ptr, 0);
                    }

                    Instruction::Call(word_name) => {
                        // Call a user-defined word (should already be compiled by compile_dependencies)
                        let callee_id = *self.functions.get(word_name)
                            .ok_or_else(|| format!("Undefined word: {}", word_name))?;
                        let callee_ref = module.declare_func_in_func(callee_id, &mut builder.func);

                        // Call it with all the stack pointers
                        builder.ins().call(callee_ref, &[
                            data_stack_ptr, data_stack_len_ptr,
                            return_stack_ptr, return_stack_len_ptr,
                            loop_stack_ptr, loop_stack_len_ptr,
                            memory_ptr, here_ptr
                        ]);
                    }

                    Instruction::AllocString(s) => {
                        // Allocate string in memory and push (addr, len)
                        let len_val = s.len() as i64;

                        // Load current HERE value
                        let here = builder.ins().load(ptr_type, MemFlags::new(), here_ptr, 0);
                        // Convert pointer to i64 (on 64-bit systems they're the same size)
                        let here_i64 = if ptr_type == types::I64 {
                            here
                        } else {
                            builder.ins().uextend(types::I64, here)
                        };

                        // Copy string bytes to memory[here..here+len]
                        for (i, &byte) in s.as_bytes().iter().enumerate() {
                            let offset = builder.ins().iconst(types::I64, i as i64);
                            let dest = builder.ins().iadd(here_i64, offset);
                            let dest_ptr = builder.ins().iadd(memory_ptr, dest);
                            let byte_val = builder.ins().iconst(types::I8, byte as i64);
                            builder.ins().store(MemFlags::new(), byte_val, dest_ptr, 0);
                        }

                        // Update HERE: here += len
                        let len_ptr = builder.ins().iconst(ptr_type, s.len() as i64);
                        let new_here = builder.ins().iadd(here, len_ptr);
                        builder.ins().store(MemFlags::new(), new_here, here_ptr, 0);

                        // Push addr onto data stack
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                        let data_offset = builder.ins().imul_imm(data_len, 8);
                        let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                        builder.ins().store(MemFlags::new(), here_i64, data_elem_ptr, 0);

                        // Push len onto data stack
                        let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                        let data_offset2 = builder.ins().imul_imm(data_len_plus_1, 8);
                        let data_elem_ptr2 = builder.ins().iadd(data_stack_ptr, data_offset2);
                        let len_i64 = builder.ins().iconst(types::I64, len_val);
                        builder.ins().store(MemFlags::new(), len_i64, data_elem_ptr2, 0);

                        // Update data stack length
                        let data_len_plus_2 = builder.ins().iadd_imm(data_len, 2);
                        builder.ins().store(MemFlags::new(), data_len_plus_2, data_stack_len_ptr, 0);
                    }

                    Instruction::VariableAddr(_name) => {
                        // For now, just allocate 8 bytes at current HERE and push the address
                        // In a full implementation, we'd need a variables map passed as a parameter

                        // Load current HERE value
                        let here = builder.ins().load(ptr_type, MemFlags::new(), here_ptr, 0);
                        // Convert pointer to i64 (on 64-bit systems they're the same size)
                        let here_i64 = if ptr_type == types::I64 {
                            here
                        } else {
                            builder.ins().uextend(types::I64, here)
                        };

                        // Update HERE: here += 8 (for i64)
                        let eight = builder.ins().iconst(ptr_type, 8);
                        let new_here = builder.ins().iadd(here, eight);
                        builder.ins().store(MemFlags::new(), new_here, here_ptr, 0);

                        // Push address onto data stack
                        let data_len = builder.ins().load(types::I64, MemFlags::new(), data_stack_len_ptr, 0);
                        let data_offset = builder.ins().imul_imm(data_len, 8);
                        let data_elem_ptr = builder.ins().iadd(data_stack_ptr, data_offset);
                        builder.ins().store(MemFlags::new(), here_i64, data_elem_ptr, 0);

                        // Update data stack length
                        let data_len_plus_1 = builder.ins().iadd_imm(data_len, 1);
                        builder.ins().store(MemFlags::new(), data_len_plus_1, data_stack_len_ptr, 0);
                    }

                    Instruction::Bye => {
                        // BYE just returns - the main.rs REPL checks if the loaded file ends with BYE
                        // and exits accordingly. We don't call forth_bye from JIT code to avoid
                        // potential issues with symbol resolution.
                        builder.ins().return_(&[]);
                        instr_has_terminator = true;
                        last_instr_has_terminator = true;
                        // Note: code after BYE is unreachable, but we'll still compile it
                        // for CFG validity
                    }
                }

                // Add fallthrough to next instruction (unless we've already added a terminator)
                if !instr_has_terminator && idx + 1 < blocks.len() {
                    // Note: don't mark as filled here - it will be marked when we switch to it
                    builder.ins().jump(blocks[idx + 1], &[]);
                }

                // Track if this was the last instruction
                last_instr_has_terminator = instr_has_terminator;
            }

            // Handle all blocks - fill any that don't have a terminator and seal them
            // Note: Blocks that were referenced by branches but never switched to need to be filled
            for (i, block) in blocks.iter().enumerate() {
                if !filled_blocks[i] {
                    // Fill block with a return (unreachable but needed for branch target)
                    builder.switch_to_block(*block);
                    builder.ins().return_(&[]);
                }
                builder.seal_block(*block);
            }

                // If we didn't hit an explicit return at the end, add one
                // We only add this if we haven't already added a terminator (like Bye or final Return)
                if !last_instr_has_terminator {
                    builder.ins().return_(&[]);
                }
            } // end of non-empty word handling

            builder.finalize();
        }

        // Define the function
        match module.define_function(func_id, &mut ctx) {
            Ok(_) => {},
            Err(e) => {
                // Print the IR for debugging
                eprintln!("Error defining function {}:", name);
                eprintln!("Cranelift IR:\n{}", ctx.func.display());
                eprintln!("Error details: {:?}", e);
                return Err(format!("Failed to define function: {}", e));
            }
        }

        // Clear the context to free memory
        module.clear_context(&mut ctx);

        // Function ID was already stored before compiling dependencies (to enable recursion)

        Ok(())
    }

    /// Check if a function has already been compiled
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Finalize all JIT definitions (can be called multiple times)
    pub fn finalize_all(&mut self) -> Result<(), String> {
        match &mut self.mode {
            CompilationMode::JIT(module) => {
                // Cranelift allows calling finalize_definitions multiple times
                // to finalize newly-added functions
                let debug = std::env::var("FORTH_DEBUG").is_ok();
                if debug {
                    eprintln!("finalize_all: {} functions compiled", self.functions.len());
                }
                module.finalize_definitions()
                    .map_err(|e| format!("Failed to finalize: {}", e))?;
                self.finalized = true;
                Ok(())
            }
            CompilationMode::AOT(_) => Ok(()),
        }
    }

    /// Get function pointer for an already-compiled and finalized function
    pub fn finalize_jit(&mut self, name: &str) -> Result<*const u8, String> {
        // Auto-finalize if not yet done (for backwards compatibility with tests)
        if !self.finalized {
            self.finalize_all()?;
        }

        match &mut self.mode {
            CompilationMode::JIT(module) => {
                let func_id = self.functions.get(name)
                    .ok_or_else(|| format!("Function {} not found", name))?;

                let code = module.get_finalized_function(*func_id);
                Ok(code)
            }
            CompilationMode::AOT(_) => {
                Err("Cannot get function pointer in AOT mode".to_string())
            }
        }
    }

    /// Finalize AOT compilation and get object file
    pub fn finalize_aot(self) -> Result<Vec<u8>, String> {
        match self.mode {
            CompilationMode::AOT(module) => {
                let product = module.finish();
                Ok(product.emit()
                    .map_err(|e| format!("Failed to emit object: {}", e))?)
            }
            CompilationMode::JIT(_) => {
                Err("Cannot get object file in JIT mode".to_string())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::Stack;

    #[test]
    fn test_create_jit() {
        let jit = ForthJIT::new_jit();
        assert!(jit.is_ok());
    }

    #[test]
    fn test_create_aot() {
        let triple = Triple::host();
        let aot = ForthJIT::new_aot(triple);
        assert!(aot.is_ok());
    }

    #[test]
    fn test_compile_empty_word() {
        let mut jit = ForthJIT::new_jit().unwrap();
        let result = jit.compile_word("test", &[], None);
        assert!(result.is_ok());
    }

    #[test]
    fn test_compile_and_execute_literal() {
        let mut jit = ForthJIT::new_jit().unwrap();

        // Compile a word that pushes 42
        let instructions = vec![
            Instruction::Literal(42),
        ];
        jit.compile_word("test_literal", &instructions, None).unwrap();

        // Finalize and get function pointer
        let func_ptr = jit.finalize_jit("test_literal").unwrap();

        // Create a data stack
        let mut data_stack = Stack::new();
        let mut data_len = data_stack.depth();
        let data_ptr = data_stack.as_mut_ptr();

        // Create dummy stacks for return stack
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let return_ptr = return_stack.as_mut_ptr();

        // Create dummy memory
        let mut memory = vec![0u8; 65536];
        let memory_ptr = memory.as_mut_ptr();

        // Create loop stack
        let mut loop_stack = Stack::new();
        let mut loop_len = loop_stack.depth();
        let loop_ptr = loop_stack.as_mut_ptr();
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_ptr, &mut data_len as *mut usize, return_ptr, &mut return_len as *mut usize, loop_ptr, &mut loop_len as *mut usize, memory_ptr, &mut here as *mut usize);
        }

        // Check that 42 was pushed onto the stack
        assert_eq!(data_len, 1);
        unsafe {
            assert_eq!(*data_ptr, 42);
        }
    }

    #[test]
    fn test_compile_and_execute_add() {
        use crate::primitives::Primitive;

        let mut jit = ForthJIT::new_jit().unwrap();

        // Compile: 3 4 +
        let instructions = vec![
            Instruction::Literal(3),
            Instruction::Literal(4),
            Instruction::Primitive(Primitive::Add),
        ];
        jit.compile_word("test_add", &instructions, None).unwrap();

        // Finalize and get function pointer
        let func_ptr = jit.finalize_jit("test_add").unwrap();

        // Create a data stack
        let mut data_stack = Stack::new();
        let mut data_len = data_stack.depth();
        let data_ptr = data_stack.as_mut_ptr();

        // Create dummy stacks for return stack
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let return_ptr = return_stack.as_mut_ptr();

        // Create dummy memory
        let mut memory = vec![0u8; 65536];
        let memory_ptr = memory.as_mut_ptr();

        // Create loop stack
        let mut loop_stack = Stack::new();
        let mut loop_len = loop_stack.depth();
        let loop_ptr = loop_stack.as_mut_ptr();
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_ptr, &mut data_len as *mut usize, return_ptr, &mut return_len as *mut usize, loop_ptr, &mut loop_len as *mut usize, memory_ptr, &mut here as *mut usize);
        }

        // Check that 7 (3 + 4) is on the stack
        assert_eq!(data_len, 1);
        unsafe {
            assert_eq!(*data_ptr, 7);
        }
    }

    #[test]
    fn test_compile_arithmetic() {
        use crate::primitives::Primitive;

        // Test: 10 3 - (should be 7)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("sub", &[
            Instruction::Literal(10),
            Instruction::Literal(3),
            Instruction::Primitive(Primitive::Sub),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("sub").unwrap();
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 7);
        }

        // Test: 4 5 * (should be 20)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("mul", &[
            Instruction::Literal(4),
            Instruction::Literal(5),
            Instruction::Primitive(Primitive::Mul),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("mul").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 20);
        }

        // Test: 20 3 / (should be 6)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("div", &[
            Instruction::Literal(20),
            Instruction::Literal(3),
            Instruction::Primitive(Primitive::Div),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("div").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 6);
        }

        // Test: 20 3 MOD (should be 2)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("mod", &[
            Instruction::Literal(20),
            Instruction::Literal(3),
            Instruction::Primitive(Primitive::Mod),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("mod").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 2);
        }
    }

    #[test]
    fn test_compile_stack_ops() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test DUP: 5 DUP -> (5 5)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("dup", &[
            Instruction::Literal(5),
            Instruction::Primitive(Primitive::Dup),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("dup").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 2);
            assert_eq!(*data_stack.as_mut_ptr(), 5);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 5);
        }

        // Test SWAP: 3 4 SWAP -> (4 3)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("swap", &[
            Instruction::Literal(3),
            Instruction::Literal(4),
            Instruction::Primitive(Primitive::Swap),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("swap").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 2);
            assert_eq!(*data_stack.as_mut_ptr(), 4);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 3);
        }

        // Test OVER: 3 4 OVER -> (3 4 3)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("over", &[
            Instruction::Literal(3),
            Instruction::Literal(4),
            Instruction::Primitive(Primitive::Over),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("over").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 3);
            assert_eq!(*data_stack.as_mut_ptr(), 3);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 4);
            assert_eq!(*data_stack.as_mut_ptr().add(2), 3);
        }

        // Test ROT: 1 2 3 ROT -> (2 3 1)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("rot", &[
            Instruction::Literal(1),
            Instruction::Literal(2),
            Instruction::Literal(3),
            Instruction::Primitive(Primitive::Rot),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("rot").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 3);
            assert_eq!(*data_stack.as_mut_ptr(), 2);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 3);
            assert_eq!(*data_stack.as_mut_ptr().add(2), 1);
        }

        // Test DROP: 3 4 DROP -> (3)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("drop", &[
            Instruction::Literal(3),
            Instruction::Literal(4),
            Instruction::Primitive(Primitive::Drop),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("drop").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 3);
        }
    }

    #[test]
    fn test_compile_comparison() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test =: 5 5 = -> (-1)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("eq", &[
            Instruction::Literal(5),
            Instruction::Literal(5),
            Instruction::Primitive(Primitive::Equals),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("eq").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), -1); // Forth true
        }

        // Test <: 3 5 < -> (-1)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("lt", &[
            Instruction::Literal(3),
            Instruction::Literal(5),
            Instruction::Primitive(Primitive::Less),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("lt").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), -1); // Forth true
        }

        // Test >: 5 3 > -> (-1)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("gt", &[
            Instruction::Literal(5),
            Instruction::Literal(3),
            Instruction::Primitive(Primitive::Greater),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("gt").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), -1); // Forth true
        }
    }

    #[test]
    fn test_compile_logical() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test AND: 0xFF 0x0F AND -> (0x0F)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("and", &[
            Instruction::Literal(0xFF),
            Instruction::Literal(0x0F),
            Instruction::Primitive(Primitive::And),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("and").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 0x0F);
        }

        // Test OR: 0xF0 0x0F OR -> (0xFF)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("or", &[
            Instruction::Literal(0xF0),
            Instruction::Literal(0x0F),
            Instruction::Primitive(Primitive::Or),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("or").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 0xFF);
        }

        // Test XOR: 0xFF 0x0F XOR -> (0xF0)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("xor", &[
            Instruction::Literal(0xFF),
            Instruction::Literal(0x0F),
            Instruction::Primitive(Primitive::Xor),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("xor").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 0xF0);
        }

        // Test INVERT: 0x0F INVERT -> (~0x0F)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("invert", &[
            Instruction::Literal(0x0F),
            Instruction::Primitive(Primitive::Invert),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("invert").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), !0x0F);
        }
    }

    #[test]
    fn test_compile_memory_ops() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test @ and !: Store 42 at address 100, then fetch it
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("store_fetch", &[
            Instruction::Literal(42),
            Instruction::Literal(100),
            Instruction::Primitive(Primitive::Store),
            Instruction::Literal(100),
            Instruction::Primitive(Primitive::Fetch),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("store_fetch").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 42);
        }

        // Test C@ and C!: Store byte 65 ('A') at address 50, then fetch it
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("cstore_cfetch", &[
            Instruction::Literal(65),
            Instruction::Literal(50),
            Instruction::Primitive(Primitive::CStore),
            Instruction::Literal(50),
            Instruction::Primitive(Primitive::CFetch),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("cstore_cfetch").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 65);
        }
    }

    #[test]
    fn test_compile_return_stack_ops() {
        use crate::primitives::Primitive;
        use crate::primitives::ReturnStack;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test >R and R>: Move 42 to return stack and back
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("to_r_from_r", &[
            Instruction::Literal(42),
            Instruction::Primitive(Primitive::ToR),
            Instruction::Primitive(Primitive::FromR),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("to_r_from_r").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = ReturnStack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(return_len, 0); // Should be back to 0
            assert_eq!(*data_stack.as_mut_ptr(), 42);
        }

        // Test R@: Copy from return stack without popping
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("r_fetch", &[
            Instruction::Literal(99),
            Instruction::Primitive(Primitive::ToR),
            Instruction::Primitive(Primitive::RFetch),
            Instruction::Primitive(Primitive::RFetch),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("r_fetch").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = ReturnStack::new();
        let mut return_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 2); // Two copies on data stack
            assert_eq!(return_len, 1); // Still on return stack
            assert_eq!(*data_stack.as_mut_ptr(), 99);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 99);
        }
    }

    #[test]
    fn test_compile_depth() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test DEPTH: Push 3 items, check depth
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("depth_test", &[
            Instruction::Literal(10),
            Instruction::Literal(20),
            Instruction::Literal(30),
            Instruction::Primitive(Primitive::Depth),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("depth_test").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 4); // 3 items + depth
            assert_eq!(*data_stack.as_mut_ptr().add(3), 3); // Depth should be 3
        }

        // Test DEPTH with empty stack
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("depth_empty", &[
            Instruction::Primitive(Primitive::Depth),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("depth_empty").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 0); // Depth should be 0
        }
    }

    #[test]
    fn test_compile_i() {
        use crate::primitives::{Stack, Primitive};

        // Test I: Access loop index
        // Set up: loop_stack has values [5, 10] (limit=5, index=10)
        // I should push 10 onto data stack
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("i_test", &[
            Instruction::Primitive(Primitive::I),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("i_test").unwrap();

        // Create stacks
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let mut loop_stack = Stack::new();
        loop_stack.push(5);   // limit
        loop_stack.push(10);  // index
        let mut loop_len = loop_stack.depth();
        let mut memory = vec![0u8; 65536];
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 10); // Should get the loop index
        }
    }

    #[test]
    fn test_compile_io_emit() {
        use crate::primitives::Primitive;

        // Test EMIT: Output character 'A' (65)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("emit_test", &[
            Instruction::Literal(65), // 'A'
            Instruction::Primitive(Primitive::Emit),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("emit_test").unwrap();
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;

        println!("\nTesting EMIT (should print 'A'):");
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 0); // Stack should be empty after emit
        }
    }

    #[test]
    fn test_compile_io_dot() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test DOT: Print number 42
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("dot_test", &[
            Instruction::Literal(42),
            Instruction::Primitive(Primitive::Dot),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("dot_test").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;

        println!("\nTesting DOT (should print '42 '):");
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 0); // Stack should be empty after dot
        }
    }

    #[test]
    fn test_compile_io_cr() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test CR: Print newline
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("cr_test", &[
            Instruction::Primitive(Primitive::Cr),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("cr_test").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;

        println!("\nTesting CR (should print newline):");
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 0); // Stack should remain empty
        }
    }

    #[test]
    fn test_compile_io_type() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test TYPE: Print string from memory
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("type_test", &[
            Instruction::Literal(0),   // addr
            Instruction::Literal(5),   // len
            Instruction::Primitive(Primitive::Type),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("type_test").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;

        // Write "Hello" to memory at address 0
        memory[0..5].copy_from_slice(b"Hello");

        println!("\nTesting TYPE (should print 'Hello'):");
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 0); // Stack should be empty after type
        }
    }

    #[test]
    fn test_compile_io_combined() {
        use crate::primitives::Primitive;
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

        // Test combined I/O: Print "Answer: 42\n"
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("combined_io", &[
            // Print "Answer: " using TYPE
            Instruction::Literal(0),
            Instruction::Literal(8),
            Instruction::Primitive(Primitive::Type),
            // Print 42 using DOT
            Instruction::Literal(42),
            Instruction::Primitive(Primitive::Dot),
            // Print newline
            Instruction::Primitive(Primitive::Cr),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("combined_io").unwrap();
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut memory = vec![0u8; 65536];
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;

        // Write "Answer: " to memory
        memory[0..8].copy_from_slice(b"Answer: ");

        println!("\nTesting combined I/O (should print 'Answer: 42\\n'):");
        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 0);
        }
    }

    #[test]
    fn test_compile_branch() {
        use crate::primitives::Stack;

        // Test unconditional branch (skip over an instruction)
        // Code: 10 BRANCH(1) 20 30
        // Expected stack: [10, 30]  (20 is skipped)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("branch_test", &[
            Instruction::Literal(10),
            Instruction::Branch(1),      // Skip next instruction
            Instruction::Literal(20),     // This should be skipped
            Instruction::Literal(30),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("branch_test").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 2);
            assert_eq!(*data_stack.as_mut_ptr(), 10);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 30);
        }
    }

    #[test]
    fn test_compile_branch_if_zero() {
        use crate::primitives::Stack;

        // Test conditional branch when condition is zero (branch taken)
        // Code: 0 BRANCH_IF_ZERO(1) 20 30
        // Expected stack: [30]  (20 is skipped because TOS was 0)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("branchz_true", &[
            Instruction::Literal(0),          // Push 0
            Instruction::BranchIfZero(1),     // Branch if zero (skip next)
            Instruction::Literal(20),          // Skipped
            Instruction::Literal(30),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("branchz_true").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 30);
        }

        // Test conditional branch when condition is non-zero (branch NOT taken)
        // Code: 1 BRANCH_IF_ZERO(1) 20 30
        // Expected stack: [20, 30]  (20 is NOT skipped because TOS was 1)
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("branchz_false", &[
            Instruction::Literal(1),          // Push 1 (non-zero)
            Instruction::BranchIfZero(1),     // Don't branch (continue)
            Instruction::Literal(20),          // NOT skipped
            Instruction::Literal(30),
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("branchz_false").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 2);
            assert_eq!(*data_stack.as_mut_ptr(), 20);
            assert_eq!(*data_stack.as_mut_ptr().add(1), 30);
        }
    }

    #[test]
    fn test_compile_do_loop() {
        use crate::primitives::{Stack, Primitive};

        // Test DO/LOOP: Sum numbers from 0 to 4
        // Code: 0 5 0 DO I + LOOP
        // Expected: 0+0+1+2+3+4 = 10
        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("sum_loop", &[
            Instruction::Literal(0),          // Accumulator
            Instruction::Literal(5),          // Limit
            Instruction::Literal(0),          // Start
            Instruction::DoSetup,             // Setup loop (limit=5, index=0)
            // Loop body starts here (index 4)
            Instruction::Primitive(Primitive::I),  // Push current index
            Instruction::Primitive(Primitive::Add), // Add to accumulator
            Instruction::Loop(-3),            // Loop back to index 4
            Instruction::LoopEnd,
        ], None).unwrap();
        let func_ptr = jit.finalize_jit("sum_loop").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 10); // Sum of 0+1+2+3+4
            assert_eq!(loop_len, 0); // Loop stack should be cleaned up
        }
    }

    #[test]
    fn test_compile_call() {
        use crate::primitives::{Stack, Primitive};
        use crate::compiler::CompiledWord;

        // Test calling a user-defined word
        // Define DOUBLE: DUP +
        // Then test: 5 DOUBLE (should give 10)
        let mut dictionary = StdHashMap::new();
        dictionary.insert("DOUBLE".to_string(), CompiledWord {
            name: "DOUBLE".to_string(),
            instructions: vec![
                Instruction::Primitive(Primitive::Dup),
                Instruction::Primitive(Primitive::Add),
            ],
        });

        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("test_double", &[
            Instruction::Literal(5),
            Instruction::Call("DOUBLE".to_string()),
        ], Some(&dictionary)).unwrap();
        let func_ptr = jit.finalize_jit("test_double").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 10); // 5 DOUBLE = 5 DUP + = 10
        }
    }

    #[test]
    fn test_compile_nested_calls() {
        use crate::primitives::{Stack, Primitive};
        use crate::compiler::CompiledWord;

        // Test nested calls: DOUBLE calls itself recursively (via another word)
        // Define DOUBLE: DUP +
        // Define QUADRUPLE: DOUBLE DOUBLE
        // Test: 3 QUADRUPLE (should give 12)
        let mut dictionary = StdHashMap::new();
        dictionary.insert("DOUBLE".to_string(), CompiledWord {
            name: "DOUBLE".to_string(),
            instructions: vec![
                Instruction::Primitive(Primitive::Dup),
                Instruction::Primitive(Primitive::Add),
            ],
        });
        dictionary.insert("QUADRUPLE".to_string(), CompiledWord {
            name: "QUADRUPLE".to_string(),
            instructions: vec![
                Instruction::Call("DOUBLE".to_string()),
                Instruction::Call("DOUBLE".to_string()),
            ],
        });

        let mut jit = ForthJIT::new_jit().unwrap();
        jit.compile_word("test_quad", &[
            Instruction::Literal(3),
            Instruction::Call("QUADRUPLE".to_string()),
        ], Some(&dictionary)).unwrap();
        let func_ptr = jit.finalize_jit("test_quad").unwrap();

        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = 0usize;
        let mut loop_stack = Stack::new();
        let mut loop_len = 0usize;
        let mut here = 0usize;
        let mut memory = vec![0u8; 65536];

        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 12); // 3 * 2 * 2 = 12
        }
    }

    #[test]
    fn test_compile_alloc_string() {
        use crate::compiler::Instruction;

        let mut jit = ForthJIT::new_jit().unwrap();

        // Create a word that allocates a string "Hi"  (shorter string for simpler test)
        let instructions = vec![
            Instruction::AllocString("Hi".to_string()),
        ];

        jit.compile_word("alloc_string_test", &instructions, None).unwrap();
        let func_ptr = jit.finalize_jit("alloc_string_test").unwrap();

        // Set up test environment
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let mut loop_stack = Stack::new();
        let mut loop_len = loop_stack.depth();
        let mut memory = vec![0u8; 65536];
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);

            // Should push addr and len onto stack
            assert_eq!(data_len, 2, "Expected 2 values on stack");
            let addr = *data_stack.as_mut_ptr();
            let len = *data_stack.as_mut_ptr().offset(1);

            println!("addr={}, len={}, here={}", addr, len, here);

            assert_eq!(len, 2); // "Hi" is 2 bytes
            assert_eq!(addr, 0); // Should be allocated at address 0

            // Check that "Hi" was written to memory
            println!("memory[0]={}, memory[1]={}", memory[0], memory[1]);
            assert_eq!(memory[0], b'H');
            assert_eq!(memory[1], b'i');

            // Check that HERE was updated
            assert_eq!(here, 2);
        }
    }

    #[test]
    fn test_compile_variable_addr() {
        use crate::compiler::Instruction;

        let mut jit = ForthJIT::new_jit().unwrap();

        // Create a word that allocates a variable
        let instructions = vec![
            Instruction::VariableAddr("my_var".to_string()),
        ];

        jit.compile_word("variable_addr_test", &instructions, None).unwrap();
        let func_ptr = jit.finalize_jit("variable_addr_test").unwrap();

        // Set up test environment
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let mut loop_stack = Stack::new();
        let mut loop_len = loop_stack.depth();
        let mut memory = vec![0u8; 65536];
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);

            // Should push address onto stack
            assert_eq!(data_len, 1);
            let addr = *data_stack.as_mut_ptr();

            assert_eq!(addr, 0); // Should be allocated at address 0

            // Check that HERE was updated (8 bytes for i64)
            assert_eq!(here, 8);
        }
    }

    #[test]
    #[ignore] // BYE now actually exits the process, so this test would terminate the test runner
    fn test_compile_bye() {
        use crate::compiler::Instruction;

        let mut jit = ForthJIT::new_jit().unwrap();

        // Create a word that pushes 42, then BYE, then pushes 99 (which should not execute)
        let instructions = vec![
            Instruction::Literal(42),
            Instruction::Bye,
            Instruction::Literal(99), // Should never execute
        ];

        jit.compile_word("bye_test", &instructions, None).unwrap();
        let func_ptr = jit.finalize_jit("bye_test").unwrap();

        // Set up test environment
        let mut data_stack = Stack::new();
        let mut data_len = 0usize;
        let mut return_stack = Stack::new();
        let mut return_len = return_stack.depth();
        let mut loop_stack = Stack::new();
        let mut loop_len = loop_stack.depth();
        let mut memory = vec![0u8; 65536];
        let mut here = 0usize;

        // Cast and call the function
        type JitFunc = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);
        let func: JitFunc = unsafe { std::mem::transmute(func_ptr) };

        unsafe {
            func(data_stack.as_mut_ptr(), &mut data_len, return_stack.as_mut_ptr(), &mut return_len, loop_stack.as_mut_ptr(), &mut loop_len, memory.as_mut_ptr(), &mut here);

            // Should only have 42 on stack, not 99
            assert_eq!(data_len, 1);
            assert_eq!(*data_stack.as_mut_ptr(), 42);
        }
    }
}
