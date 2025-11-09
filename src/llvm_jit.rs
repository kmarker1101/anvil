// llvm_jit.rs - LLVM-based JIT and AOT compiler for Forth
//
// This module provides unified compilation using LLVM for both:
// - JIT: Execute code immediately in memory
// - AOT: Generate object files for linking
//
// All primitives are implemented as extern "C" functions and called from generated code.

#![allow(clippy::not_unsafe_ptr_arg_deref)]

use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::targets::{Target, RelocMode, CodeModel, FileType, InitializationConfig};
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use std::collections::HashMap;

use crate::compiler::Instruction;
use crate::primitives::Primitive;

/// Macro to map Primitive enum variants to their extern "C" function names
/// This generates the symbol registration and primitive declaration mappings
macro_rules! llvm_primitive_mappings {
    (
        $(
            $variant:ident => $func_name:ident
        ),* $(,)?
    ) => {
        // Generate the register_forth_symbols function
        #[inline(never)]
        fn register_forth_symbols() -> usize {
            let symbols = [
                $(
                    $func_name as usize,
                )*
            ];
            symbols[0]
        }

        // Generate helper to get (name, addr) pair for a primitive
        fn get_primitive_mapping(primitive: Primitive) -> (&'static str, usize) {
            match primitive {
                $(
                    Primitive::$variant => (stringify!($func_name), $func_name as usize),
                )*
            }
        }
    };
}

// Define all LLVM primitive mappings
llvm_primitive_mappings! {
    Fetch => forth_fetch,
    Store => forth_store,
    CFetch => forth_c_fetch,
    CStore => forth_c_store,
    Here => forth_here,
    CharPlus => forth_char_plus,
    Chars => forth_chars,
    Base => forth_base,
    ToIn => forth_to_in,
    Source => forth_source,
    Word => forth_word,
    Parse => forth_parse,
    Compare => forth_compare,
    ToNumber => forth_to_number,
    SToD => forth_s_to_d,
    Dup => forth_dup,
    Drop => forth_drop,
    Swap => forth_swap,
    Over => forth_over,
    Rot => forth_rot,
    ToR => forth_to_r,
    FromR => forth_from_r,
    RFetch => forth_r_fetch,
    Add => forth_add,
    Sub => forth_sub,
    Mul => forth_mul,
    Div => forth_div,
    Mod => forth_mod,
    Equals => forth_equals,
    Less => forth_less,
    Greater => forth_greater,
    And => forth_and,
    Or => forth_or,
    Xor => forth_xor,
    Invert => forth_invert,
    Emit => forth_emit,
    Key => forth_key,
    Dot => forth_dot,
    Cr => forth_cr,
    Type => forth_type,
    I => forth_i,
    Depth => forth_depth,
}

/// Compiled function signature:
/// fn(data_stack: *mut i64, data_len: *mut usize,
///    return_stack: *mut i64, return_len: *mut usize,
///    loop_stack: *mut i64, loop_len: *mut usize,
///    memory: *mut u8, here: *mut usize,
///    exit_flag: *mut bool)
type ForthFunction = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize, *mut bool);

pub struct LLVMCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,

    // Function registry
    functions: HashMap<String, FunctionValue<'ctx>>,

    // Primitive function declarations
    primitive_funcs: HashMap<Primitive, FunctionValue<'ctx>>,

    // Temporary execution engines for immediate code (kept alive to preserve machine code)
    temp_engines: Vec<ExecutionEngine<'ctx>>,

    // Variable allocation tracking
    variable_offsets: HashMap<String, i64>,
    next_variable_offset: i64,
}

impl<'ctx> LLVMCompiler<'ctx> {
    /// Create a new LLVM compiler in JIT mode
    pub fn new_jit(context: &'ctx Context, name: &str) -> Result<Self, String> {
        let module = context.create_module(name);
        let builder = context.create_builder();

        // Initialize LLVM targets for JIT
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to initialize native target: {}", e))?;

        // Don't create ExecutionEngine yet - delay until first execution
        // This allows all functions to be compiled first

        Ok(LLVMCompiler {
            context,
            module,
            builder,
            execution_engine: None,  // Created lazily
            functions: HashMap::new(),
            primitive_funcs: HashMap::new(),
            temp_engines: Vec::new(),
            variable_offsets: HashMap::new(),
            next_variable_offset: 0,
        })
    }

    /// Create a new LLVM compiler in AOT mode
    pub fn new_aot(context: &'ctx Context, name: &str, target_triple: Option<String>) -> Result<Self, String> {
        let module = context.create_module(name);
        let builder = context.create_builder();

        // Initialize native target with ASM printer and parser for object file generation
        Target::initialize_native(&InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: false,
            info: true,
            machine_code: true,
        })
            .map_err(|e| format!("Failed to initialize native target: {}", e))?;

        // Set target triple (use native if not specified)
        let triple = target_triple.unwrap_or_else(|| {
            inkwell::targets::TargetMachine::get_default_triple().to_string()
        });
        module.set_triple(&inkwell::targets::TargetTriple::create(&triple));

        Ok(LLVMCompiler {
            context,
            module,
            builder,
            execution_engine: None,
            functions: HashMap::new(),
            primitive_funcs: HashMap::new(),
            temp_engines: Vec::new(),
            variable_offsets: HashMap::new(),
            next_variable_offset: 0,
        })
    }

    /// Declare a primitive function as extern "C"
    fn declare_primitive(&mut self, primitive: Primitive) -> FunctionValue<'ctx> {
        if let Some(&func) = self.primitive_funcs.get(&primitive) {
            return func;
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // All primitives have the same signature as Forth functions
        let fn_type = self.context.void_type().fn_type(
            &[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                ptr_type.into(), // return_stack
                ptr_type.into(), // return_len
                ptr_type.into(), // loop_stack
                ptr_type.into(), // loop_len
                ptr_type.into(), // memory
                ptr_type.into(), // here
                ptr_type.into(), // exit_flag
            ],
            false,
        );

        // Use the centralized mapping from the macro
        let (func_name, func_addr) = get_primitive_mapping(primitive);

        let func = self.module.add_function(func_name, fn_type, None);

        // Register the function address with the execution engine
        if let Some(ee) = &self.execution_engine {
            ee.add_global_mapping(&func, func_addr);
        }

        self.primitive_funcs.insert(primitive, func);
        func
    }

    /// Remove a function from the compiler
    pub fn remove_function(&mut self, name: &str) {
        self.functions.remove(name);
        // Also remove from module if it exists
        if let Some(func) = self.module.get_function(name) {
            unsafe {
                func.delete();
            }
        }
    }

    /// Compile a Forth word to LLVM IR
    pub fn compile_word(&mut self, name: &str, instructions: &[Instruction]) -> Result<(), String> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Create function signature
        let fn_type = self.context.void_type().fn_type(
            &[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                ptr_type.into(), // return_stack
                ptr_type.into(), // return_len
                ptr_type.into(), // loop_stack
                ptr_type.into(), // loop_len
                ptr_type.into(), // memory
                ptr_type.into(), // here
                ptr_type.into(), // exit_flag
            ],
            false,
        );

        // Check if function already exists - if so, clear it and recompile
        let function = if let Some(existing) = self.module.get_function(name) {
            // Clear existing basic blocks for redefinition
            unsafe {
                while let Some(bb) = existing.get_first_basic_block() {
                    let _ = bb.delete();
                }
            }
            // Remove from our tracking
            self.functions.remove(name);
            existing
        } else {
            // Create new function
            self.module.add_function(name, fn_type, None)
        };

        let entry_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_block);

        // Get function parameters
        let data_stack_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let data_len_ptr = function.get_nth_param(1).unwrap().into_pointer_value();
        let return_stack_ptr = function.get_nth_param(2).unwrap().into_pointer_value();
        let return_len_ptr = function.get_nth_param(3).unwrap().into_pointer_value();
        let loop_stack_ptr = function.get_nth_param(4).unwrap().into_pointer_value();
        let loop_len_ptr = function.get_nth_param(5).unwrap().into_pointer_value();
        let memory_ptr = function.get_nth_param(6).unwrap().into_pointer_value();
        let here_ptr = function.get_nth_param(7).unwrap().into_pointer_value();
        let exit_flag_ptr = function.get_nth_param(8).unwrap().into_pointer_value();

        // Create basic blocks for branch targets
        let mut blocks = Vec::new();
        for i in 0..instructions.len() {
            let block = self.context.append_basic_block(function, &format!("bb{}", i));
            blocks.push(block);
        }

        // Branch from entry to first instruction
        if !blocks.is_empty() {
            self.builder.build_unconditional_branch(blocks[0])
                .map_err(|e| format!("Failed to build entry branch: {}", e))?;
        }

        // Compile each instruction
        for (idx, instruction) in instructions.iter().enumerate() {
            self.builder.position_at_end(blocks[idx]);
            match instruction {
                Instruction::Literal(value) => {
                    // Push literal onto data stack inline (no function call)
                    let i64_type = self.context.i64_type();
                    let usize_type = match &self.execution_engine {
                        Some(ee) => self.context.ptr_sized_int_type(ee.get_target_data(), None),
                        None => self.context.i64_type(), // Fallback for AOT mode
                    };

                    // Load current stack length
                    let len = self.builder.build_load(usize_type, data_len_ptr, "len")
                        .map_err(|e| format!("Failed to load length: {}", e))?
                        .into_int_value();

                    // Calculate address: data_stack + len
                    let stack_addr = unsafe {
                        self.builder.build_gep(i64_type, data_stack_ptr, &[len], "stack_addr")
                            .map_err(|e| format!("Failed to build GEP: {}", e))?
                    };

                    // Store value at that address
                    let value_const = i64_type.const_int(*value as u64, true);
                    self.builder.build_store(stack_addr, value_const)
                        .map_err(|e| format!("Failed to store value: {}", e))?;

                    // Increment stack length
                    let one = usize_type.const_int(1, false);
                    let new_len = self.builder.build_int_add(len, one, "new_len")
                        .map_err(|e| format!("Failed to add: {}", e))?;
                    self.builder.build_store(data_len_ptr, new_len)
                        .map_err(|e| format!("Failed to store new length: {}", e))?;
                }

                Instruction::Primitive(prim) => {
                    // Call primitive function
                    let prim_func = self.declare_primitive(*prim);
                    self.builder.build_call(
                        prim_func,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                            exit_flag_ptr.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to build call: {}", e))?;
                }

                Instruction::Call(word_name) => {
                    // Call another Forth word - get or declare function
                    let callee = if let Some(func) = self.functions.get(word_name) {
                        *func
                    } else if let Some(func) = self.module.get_function(word_name) {
                        // Function exists in module but not in our map (recursive call)
                        func
                    } else {
                        // Create forward declaration
                        let ptr_type = self.context.ptr_type(AddressSpace::default());
                        let fn_type = self.context.void_type().fn_type(
                            &[
                                ptr_type.into(), ptr_type.into(), ptr_type.into(), ptr_type.into(),
                                ptr_type.into(), ptr_type.into(), ptr_type.into(), ptr_type.into(),
                                ptr_type.into(), // exit_flag
                            ],
                            false,
                        );
                        self.module.add_function(word_name, fn_type, None)
                    };

                    self.builder.build_call(
                        callee,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                            exit_flag_ptr.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to build call: {}", e))?;
                }

                Instruction::Return => {
                    // Early return from current word
                    self.builder.build_return(None)
                        .map_err(|e| format!("Failed to build return: {}", e))?;
                    // Don't process more instructions in this block
                    continue;
                }

                Instruction::BranchIfZero(offset) => {
                    // Call helper to check and pop value, then branch
                    let helper_fn = self.declare_control_flow_helper("forth_branch_if_zero");
                    let result = self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "branch_cond",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;

                    let cond = result.try_as_basic_value().left().unwrap().into_int_value();
                    let zero = self.context.i64_type().const_int(0, false);
                    let is_zero = self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond,
                        zero,
                        "is_zero"
                    ).map_err(|e| format!("Failed to compare: {}", e))?;

                    // Offset is relative to next instruction (idx + 1)
                    let target_idx = ((idx + 1) as isize + offset) as usize;
                    let next_idx = idx + 1;
                    if target_idx >= blocks.len() || next_idx >= blocks.len() {
                        return Err("Branch target out of bounds".to_string());
                    }

                    self.builder.build_conditional_branch(is_zero, blocks[target_idx], blocks[next_idx])
                        .map_err(|e| format!("Failed to build branch: {}", e))?;
                    continue;
                }

                Instruction::Branch(offset) => {
                    // Offset is relative to next instruction (idx + 1)
                    let target_idx = ((idx + 1) as isize + offset) as usize;
                    if target_idx >= blocks.len() {
                        return Err("Branch target out of bounds".to_string());
                    }
                    self.builder.build_unconditional_branch(blocks[target_idx])
                        .map_err(|e| format!("Failed to build branch: {}", e))?;
                    continue;
                }

                Instruction::DoSetup => {
                    let helper_fn = self.declare_control_flow_helper("forth_do_setup");
                    self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;
                }

                Instruction::QDoSetup(skip_offset) => {
                    let helper_fn = self.declare_control_flow_helper("forth_qdo_setup");
                    let result = self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "should_skip",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;

                    let cond = result.try_as_basic_value().left().unwrap().into_int_value();
                    let zero = self.context.i64_type().const_int(0, false);
                    let should_skip = self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond,
                        zero,
                        "should_skip"
                    ).map_err(|e| format!("Failed to compare: {}", e))?;

                    // Offset is relative to next instruction (idx + 1)
                    let skip_idx = ((idx + 1) as isize + skip_offset) as usize;
                    let next_idx = idx + 1;
                    if skip_idx >= blocks.len() || next_idx >= blocks.len() {
                        return Err("QDoSetup target out of bounds".to_string());
                    }

                    self.builder.build_conditional_branch(should_skip, blocks[skip_idx], blocks[next_idx])
                        .map_err(|e| format!("Failed to build branch: {}", e))?;
                    continue;
                }

                Instruction::Loop(offset) => {
                    let helper_fn = self.declare_control_flow_helper("forth_loop_check");
                    let result = self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "loop_done",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;

                    let cond = result.try_as_basic_value().left().unwrap().into_int_value();
                    let zero = self.context.i64_type().const_int(0, false);
                    let is_done = self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond,
                        zero,
                        "is_done"
                    ).map_err(|e| format!("Failed to compare: {}", e))?;

                    // Offset is relative to next instruction (idx + 1)
                    let loop_start_idx = ((idx + 1) as isize + offset) as usize;
                    let next_idx = idx + 1;
                    if loop_start_idx >= blocks.len() || next_idx >= blocks.len() {
                        return Err("Loop target out of bounds".to_string());
                    }

                    self.builder.build_conditional_branch(is_done, blocks[next_idx], blocks[loop_start_idx])
                        .map_err(|e| format!("Failed to build branch: {}", e))?;
                    continue;
                }

                Instruction::PlusLoop(offset) => {
                    let helper_fn = self.declare_control_flow_helper("forth_plusloop_check");
                    let result = self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "loop_done",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;

                    let cond = result.try_as_basic_value().left().unwrap().into_int_value();
                    let zero = self.context.i64_type().const_int(0, false);
                    let is_done = self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond,
                        zero,
                        "is_done"
                    ).map_err(|e| format!("Failed to compare: {}", e))?;

                    // Offset is relative to next instruction (idx + 1)
                    let loop_start_idx = ((idx + 1) as isize + offset) as usize;
                    let next_idx = idx + 1;
                    if loop_start_idx >= blocks.len() || next_idx >= blocks.len() {
                        return Err("PlusLoop target out of bounds".to_string());
                    }

                    self.builder.build_conditional_branch(is_done, blocks[next_idx], blocks[loop_start_idx])
                        .map_err(|e| format!("Failed to build branch: {}", e))?;
                    continue;
                }

                Instruction::LoopEnd => {
                    let helper_fn = self.declare_control_flow_helper("forth_loop_end");
                    self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            return_stack_ptr.into(),
                            return_len_ptr.into(),
                            loop_stack_ptr.into(),
                            loop_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;
                }

                Instruction::AllocString(s) => {
                    // Allocate string in memory and push (addr len) onto stack
                    let helper_fn = self.declare_string_helper("forth_alloc_string");

                    // Create a global string constant
                    let string_global = self.builder.build_global_string_ptr(s, "str")
                        .map_err(|e| format!("Failed to create string: {}", e))?;
                    let len = self.context.i64_type().const_int(s.len() as u64, false);

                    self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            memory_ptr.into(),
                            here_ptr.into(),
                            string_global.as_pointer_value().into(),
                            len.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;
                }

                Instruction::VariableAddr(var_name) => {
                    // Push variable address onto stack
                    // Variables are allocated sequentially, 8 bytes apart
                    let helper_fn = self.declare_var_helper("forth_variable_addr");

                    // Get or allocate offset for this variable
                    let var_offset = if let Some(&offset) = self.variable_offsets.get(var_name) {
                        offset
                    } else {
                        let offset = self.next_variable_offset;
                        self.variable_offsets.insert(var_name.clone(), offset);
                        self.next_variable_offset += 8; // Each variable is 8 bytes (i64)
                        offset
                    };

                    let offset_val = self.context.i64_type().const_int(var_offset as u64, false);

                    self.builder.build_call(
                        helper_fn,
                        &[
                            data_stack_ptr.into(),
                            data_len_ptr.into(),
                            offset_val.into(),
                        ],
                        "",
                    ).map_err(|e| format!("Failed to call helper: {}", e))?;
                }

                Instruction::Bye => {
                    // Set exit flag to true
                    let bool_type = self.context.bool_type();
                    let true_val = bool_type.const_int(1, false);
                    self.builder.build_store(exit_flag_ptr, true_val)
                        .map_err(|e| format!("Failed to store exit flag: {}", e))?;

                    // Return from function
                    self.builder.build_return(None)
                        .map_err(|e| format!("Failed to build return: {}", e))?;
                    continue;
                }
            }

            // Add fall-through branch to next block (unless we already branched)
            if idx + 1 < blocks.len() {
                self.builder.build_unconditional_branch(blocks[idx + 1])
                    .map_err(|e| format!("Failed to build fall-through branch: {}", e))?;
            }
        }

        // Add return in last block if needed
        if let Some(last_block) = blocks.last() {
            self.builder.position_at_end(*last_block);
            if self.builder.get_insert_block().and_then(|b| b.get_terminator()).is_none() {
                self.builder.build_return(None)
                    .map_err(|e| format!("Failed to build return: {}", e))?;
            }
        } else {
            // No instructions - return from entry
            self.builder.position_at_end(entry_block);
            self.builder.build_return(None)
                .map_err(|e| format!("Failed to build return: {}", e))?;
        }

        // Verify function
        if !function.verify(true) {
            return Err(format!("Function verification failed for {}", name));
        }

        self.functions.insert(name.to_string(), function);
        Ok(())
    }

    /// Declare a control flow helper function
    fn declare_control_flow_helper(&mut self, name: &str) -> FunctionValue<'ctx> {
        // Check if already declared
        if let Some(func) = self.module.get_function(name) {
            return func;
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        // Control flow helpers return i64 (0 or 1 for conditionals)
        let fn_type = i64_type.fn_type(
            &[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                ptr_type.into(), // return_stack
                ptr_type.into(), // return_len
                ptr_type.into(), // loop_stack
                ptr_type.into(), // loop_len
                ptr_type.into(), // memory
                ptr_type.into(), // here
            ],
            false,
        );

        let func = self.module.add_function(name, fn_type, None);

        // Register address with execution engine
        if let Some(ee) = &self.execution_engine {
            let func_addr = match name {
                "forth_branch_if_zero" => forth_branch_if_zero as usize,
                "forth_do_setup" => forth_do_setup as usize,
                "forth_qdo_setup" => forth_qdo_setup as usize,
                "forth_loop_check" => forth_loop_check as usize,
                "forth_plusloop_check" => forth_plusloop_check as usize,
                "forth_loop_end" => forth_loop_end as usize,
                _ => panic!("Unknown control flow helper: {}", name),
            };
            ee.add_global_mapping(&func, func_addr);
        }

        func
    }

    /// Declare a string helper function
    fn declare_string_helper(&mut self, name: &str) -> FunctionValue<'ctx> {
        // Check if already declared
        if let Some(func) = self.module.get_function(name) {
            return func;
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        // String helper: takes data_stack, data_len, memory, here, string_ptr, len
        let fn_type = self.context.void_type().fn_type(
            &[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                ptr_type.into(), // memory
                ptr_type.into(), // here
                ptr_type.into(), // string_ptr
                i64_type.into(), // len
            ],
            false,
        );

        let func = self.module.add_function(name, fn_type, None);

        // Register address with execution engine
        if let Some(ee) = &self.execution_engine {
            let func_addr = match name {
                "forth_alloc_string" => forth_alloc_string as usize,
                _ => panic!("Unknown string helper: {}", name),
            };
            ee.add_global_mapping(&func, func_addr);
        }

        func
    }

    /// Declare a variable helper function
    fn declare_var_helper(&mut self, name: &str) -> FunctionValue<'ctx> {
        // Check if already declared
        if let Some(func) = self.module.get_function(name) {
            return func;
        }

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();

        // Variable helper: takes data_stack, data_len, offset
        let fn_type = self.context.void_type().fn_type(
            &[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                i64_type.into(), // offset
            ],
            false,
        );

        let func = self.module.add_function(name, fn_type, None);

        // Register address with execution engine
        if let Some(ee) = &self.execution_engine {
            let func_addr = match name {
                "forth_variable_addr" => forth_variable_addr as usize,
                _ => panic!("Unknown variable helper: {}", name),
            };
            ee.add_global_mapping(&func, func_addr);
        }

        func
    }

    /// Compile an immediate expression (allows recompilation)
    pub fn compile_word_immediate(&mut self, name: &str, instructions: &[Instruction]) -> Result<(), String> {
        // For immediate code, always recompile by removing old version first
        self.functions.remove(name);
        if let Some(func) = self.module.get_function(name) {
            unsafe {
                while let Some(bb) = func.get_first_basic_block() {
                    let _ = bb.delete();
                }
            }
        }

        // Now compile normally
        self.compile_word(name, instructions)
    }

    /// Get a function pointer for immediate code (creates ExecutionEngine if needed)
    pub fn get_immediate_function(&mut self, name: &str) -> Result<ForthFunction, String> {
        // Create the ExecutionEngine lazily on first use
        // This ensures all functions are compiled before the EE is created
        if self.execution_engine.is_none() {
            let ee = self.module
                .create_jit_execution_engine(OptimizationLevel::None)
                .map_err(|e| format!("Failed to create execution engine: {}", e))?;
            self.execution_engine = Some(ee);
        }

        // Now use the regular get_function
        self.get_function(name)
    }

    /// Get a JIT-compiled function pointer
    /// Creates a NEW ExecutionEngine each time to support dynamic function addition
    /// MCJIT will resolve forth_* primitives from the current process
    pub fn get_function(&mut self, name: &str) -> Result<ForthFunction, String> {
        // Ensure forth_* symbols are included in binary
        let _ = register_forth_symbols();

        // Clone the module to create a new ExecutionEngine
        // (LLVM doesn't allow multiple EEs for the same module)
        let module_clone = self.module.clone();

        // Create a NEW ExecutionEngine for the cloned module
        // This allows MCJIT to see all functions compiled so far
        let ee = module_clone
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .map_err(|e| format!("Failed to create execution engine: {}", e))?;

        unsafe {
            // Get the function
            let jit_fn: JitFunction<ForthFunction> = ee
                .get_function(name)
                .map_err(|e| format!("Failed to get function {}: {}", name, e))?;

            // Get the raw function pointer
            let func_ptr = jit_fn.as_raw() as *const ();
            let forth_func: ForthFunction = std::mem::transmute(func_ptr);

            // Keep the ExecutionEngine alive to preserve the JIT-compiled code
            self.temp_engines.push(ee);

            Ok(forth_func)
        }
    }

    /// Generate object file for AOT compilation
    pub fn emit_object_file(&self, path: &str) -> Result<(), String> {
        // Use the native target triple
        let native_triple = inkwell::targets::TargetMachine::get_default_triple();

        // Get the target from the native triple
        let target = Target::from_name("aarch64")
            .ok_or_else(|| "Failed to find aarch64 target".to_string())?;

        let target_machine = target
            .create_target_machine(
                &native_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| "Failed to create target machine".to_string())?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
            .map_err(|e| format!("Failed to write object file: {}", e.to_string()))?;

        Ok(())
    }

    /// Print LLVM IR for debugging
    pub fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }
}

// ============================================================================
// EXTERN C PRIMITIVE FUNCTIONS
// ============================================================================

// These functions are called from JIT-compiled code and implement all Forth primitives.
// They operate directly on stack pointers for performance.

/// Helper functions to read/write stack length safely
unsafe fn stack_push(stack_ptr: *mut i64, len_ptr: *mut usize, value: i64) {
    unsafe {
        let len = *len_ptr;
        *stack_ptr.add(len) = value;
        *len_ptr = len + 1;
    }
}

unsafe fn stack_pop(stack_ptr: *mut i64, len_ptr: *mut usize) -> Option<i64> {
    unsafe {
        let len = *len_ptr;
        if len == 0 {
            return None;
        }
        *len_ptr = len - 1;
        Some(*stack_ptr.add(len - 1))
    }
}

unsafe fn stack_peek(stack_ptr: *mut i64, len_ptr: *mut usize, offset: usize) -> Option<i64> {
    unsafe {
        let len = *len_ptr;
        if offset >= len {
            return None;
        }
        Some(*stack_ptr.add(len - 1 - offset))
    }
}

// Stack operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_dup(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(top) = stack_peek(data_stack, data_len, 0) {
            stack_push(data_stack, data_len, top);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_drop(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        stack_pop(data_stack, data_len);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_swap(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(a), Some(b)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a);
            stack_push(data_stack, data_len, b);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_over(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(second) = stack_peek(data_stack, data_len, 1) {
            stack_push(data_stack, data_len, second);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_rot(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(c), Some(b), Some(a)) = (
            stack_pop(data_stack, data_len),
            stack_pop(data_stack, data_len),
            stack_pop(data_stack, data_len),
        ) {
            stack_push(data_stack, data_len, b);
            stack_push(data_stack, data_len, c);
            stack_push(data_stack, data_len, a);
        }
    }
}

// Arithmetic operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_add(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a.wrapping_add(b));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_sub(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a.wrapping_sub(b));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_mul(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a.wrapping_mul(b));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_div(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len))
            && b != 0
        {
            stack_push(data_stack, data_len, a / b);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_mod(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len))
            && b != 0
        {
            stack_push(data_stack, data_len, a % b);
        }
    }
}

// Comparison operations (return -1 for true, 0 for false)
#[unsafe(no_mangle)]
pub extern "C" fn forth_equals(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, if a == b { -1 } else { 0 });
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_less(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, if a < b { -1 } else { 0 });
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_greater(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, if a > b { -1 } else { 0 });
        }
    }
}

// Logical operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_and(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a & b);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_or(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a | b);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_xor(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            stack_push(data_stack, data_len, a ^ b);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_invert(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(a) = stack_pop(data_stack, data_len) {
            stack_push(data_stack, data_len, !a);
        }
    }
}

// Return stack operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_to_r(
    data_stack: *mut i64, data_len: *mut usize,
    return_stack: *mut i64, return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(value) = stack_pop(data_stack, data_len) {
            stack_push(return_stack, return_len, value);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_from_r(
    data_stack: *mut i64, data_len: *mut usize,
    return_stack: *mut i64, return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(value) = stack_pop(return_stack, return_len) {
            stack_push(data_stack, data_len, value);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_r_fetch(
    data_stack: *mut i64, data_len: *mut usize,
    return_stack: *mut i64, return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(value) = stack_peek(return_stack, return_len, 0) {
            stack_push(data_stack, data_len, value);
        }
    }
}

// I/O operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_emit(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(value) = stack_pop(data_stack, data_len) {
            print!("{}", (value as u8) as char);
            use std::io::Write;
            let _ = std::io::stdout().flush();
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_dot(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(value) = stack_pop(data_stack, data_len) {
            print!("{} ", value);
            use std::io::Write;
            let _ = std::io::stdout().flush();
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_cr(
    _data_stack: *mut i64, _data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    println!();
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_key(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        use std::io::Read;
        let mut buffer = [0u8; 1];
        if std::io::stdin().read_exact(&mut buffer).is_ok() {
            stack_push(data_stack, data_len, buffer[0] as i64);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_type(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(len), Some(addr)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            let addr = addr as usize;
            let len = len as usize;
            if addr + len <= 65536 {
                for i in 0..len {
                    print!("{}", *memory.add(addr + i) as char);
                }
                use std::io::Write;
                let _ = std::io::stdout().flush();
            }
        }
    }
}

// Memory operations
#[unsafe(no_mangle)]
pub extern "C" fn forth_fetch(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(addr) = stack_pop(data_stack, data_len) {
            let addr = addr as usize;
            if addr + 8 <= 65536 {
                let value = i64::from_le_bytes([
                    *memory.add(addr),
                    *memory.add(addr + 1),
                    *memory.add(addr + 2),
                    *memory.add(addr + 3),
                    *memory.add(addr + 4),
                    *memory.add(addr + 5),
                    *memory.add(addr + 6),
                    *memory.add(addr + 7),
                ]);
                stack_push(data_stack, data_len, value);
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_store(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(addr), Some(value)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            let addr = addr as usize;
            if addr + 8 <= 65536 {
                let bytes = value.to_le_bytes();
                for (i, &byte) in bytes.iter().enumerate() {
                    *memory.add(addr + i) = byte;
                }
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_c_fetch(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(addr) = stack_pop(data_stack, data_len) {
            let addr = addr as usize;
            if addr < 65536 {
                stack_push(data_stack, data_len, *memory.add(addr) as i64);
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_c_store(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let (Some(addr), Some(value)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            let addr = addr as usize;
            if addr < 65536 {
                *memory.add(addr) = value as u8;
            }
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_here(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, here: *mut usize,
) {
    unsafe {
        stack_push(data_stack, data_len, *here as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_char_plus(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if let Some(addr) = stack_pop(data_stack, data_len) {
            stack_push(data_stack, data_len, addr.wrapping_add(1));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_chars(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // CHARS is a no-op in this implementation (1 char = 1 address unit)
        // But we still pop and push for consistency
        if let Some(n) = stack_pop(data_stack, data_len) {
            stack_push(data_stack, data_len, n);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_base(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // BASE ( -- a-addr )
        // Push the address of the BASE variable
        stack_push(data_stack, data_len, crate::primitives::BASE_ADDR as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_to_in(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // >IN ( -- a-addr )
        // Push the address of the >IN variable
        stack_push(data_stack, data_len, crate::primitives::TO_IN_ADDR as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_source(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // SOURCE ( -- c-addr u )
        // Push the address of the input buffer and its current length
        stack_push(data_stack, data_len, crate::primitives::INPUT_BUFFER_ADDR as i64);

        // Get input_length from memory - we need to read it from a known location
        // For now, we'll read the length that was stored in memory during set_input
        // The input length is stored after the input buffer
        let input_len_addr = crate::primitives::INPUT_BUFFER_ADDR + crate::primitives::INPUT_BUFFER_SIZE;
        let mut len_bytes = [0u8; 8];
        for i in 0..8 {
            len_bytes[i] = *memory.add(input_len_addr + i);
        }
        let input_length = i64::from_le_bytes(len_bytes);
        stack_push(data_stack, data_len, input_length);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_word(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // WORD ( char -- c-addr )
        let delimiter = stack_pop(data_stack, data_len).unwrap_or(32) as u8;

        // Read >IN
        let mut to_in_bytes = [0u8; 8];
        for i in 0..8 {
            to_in_bytes[i] = *memory.add(crate::primitives::TO_IN_ADDR + i);
        }
        let mut pos = i64::from_le_bytes(to_in_bytes) as usize;

        // Read input_length
        let input_len_addr = crate::primitives::INPUT_BUFFER_ADDR + crate::primitives::INPUT_BUFFER_SIZE;
        let mut len_bytes = [0u8; 8];
        for i in 0..8 {
            len_bytes[i] = *memory.add(input_len_addr + i);
        }
        let input_length = i64::from_le_bytes(len_bytes) as usize;

        // Skip leading delimiters
        while pos < input_length && *memory.add(crate::primitives::INPUT_BUFFER_ADDR + pos) == delimiter {
            pos += 1;
        }

        // Collect characters until delimiter
        let start_pos = pos;
        while pos < input_length && *memory.add(crate::primitives::INPUT_BUFFER_ADDR + pos) != delimiter {
            pos += 1;
        }
        let word_len = pos - start_pos;

        // Store as counted string at WORD_BUFFER_ADDR
        *memory.add(crate::primitives::WORD_BUFFER_ADDR) = word_len.min(255) as u8;
        if word_len > 0 {
            for i in 0..word_len {
                *memory.add(crate::primitives::WORD_BUFFER_ADDR + 1 + i) =
                    *memory.add(crate::primitives::INPUT_BUFFER_ADDR + start_pos + i);
            }
        }

        // Update >IN
        let to_in_bytes = (pos as i64).to_le_bytes();
        for i in 0..8 {
            *memory.add(crate::primitives::TO_IN_ADDR + i) = to_in_bytes[i];
        }

        // Push address of counted string
        stack_push(data_stack, data_len, crate::primitives::WORD_BUFFER_ADDR as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_parse(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // PARSE ( char -- c-addr u )
        let delimiter = stack_pop(data_stack, data_len).unwrap_or(32) as u8;

        // Read >IN
        let mut to_in_bytes = [0u8; 8];
        for i in 0..8 {
            to_in_bytes[i] = *memory.add(crate::primitives::TO_IN_ADDR + i);
        }
        let pos = i64::from_le_bytes(to_in_bytes) as usize;

        // Read input_length
        let input_len_addr = crate::primitives::INPUT_BUFFER_ADDR + crate::primitives::INPUT_BUFFER_SIZE;
        let mut len_bytes = [0u8; 8];
        for i in 0..8 {
            len_bytes[i] = *memory.add(input_len_addr + i);
        }
        let input_length = i64::from_le_bytes(len_bytes) as usize;

        // Start parsing from current position (no skipping)
        let start_pos = pos;
        let mut end_pos = pos;

        // Find delimiter or end of input
        while end_pos < input_length && *memory.add(crate::primitives::INPUT_BUFFER_ADDR + end_pos) != delimiter {
            end_pos += 1;
        }

        let parsed_len = end_pos - start_pos;

        // Update >IN to position after delimiter (or end of input)
        let new_to_in = if end_pos < input_length {
            end_pos + 1  // Skip the delimiter
        } else {
            end_pos  // At end of input
        };
        let to_in_bytes = (new_to_in as i64).to_le_bytes();
        for i in 0..8 {
            *memory.add(crate::primitives::TO_IN_ADDR + i) = to_in_bytes[i];
        }

        // Push address and length
        stack_push(data_stack, data_len, (crate::primitives::INPUT_BUFFER_ADDR + start_pos) as i64);
        stack_push(data_stack, data_len, parsed_len as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_compare(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
        let u2 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;
        let c_addr2 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;
        let u1 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;
        let c_addr1 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;

        // Compare up to the length of the shorter string
        let min_len = u1.min(u2);

        for i in 0..min_len {
            let byte1 = *memory.add(c_addr1 + i);
            let byte2 = *memory.add(c_addr2 + i);

            if byte1 < byte2 {
                stack_push(data_stack, data_len, -1);
                return;
            } else if byte1 > byte2 {
                stack_push(data_stack, data_len, 1);
                return;
            }
        }

        // Strings are equal up to min_len, compare lengths
        if u1 == u2 {
            stack_push(data_stack, data_len, 0);
        } else if u1 < u2 {
            stack_push(data_stack, data_len, -1);
        } else {
            stack_push(data_stack, data_len, 1);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_to_number(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let u1 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;
        let c_addr1 = stack_pop(data_stack, data_len).unwrap_or(0) as usize;
        let ud1 = stack_pop(data_stack, data_len).unwrap_or(0) as u64;

        // Get BASE from memory
        let mut base_bytes = [0u8; 8];
        for i in 0..8 {
            base_bytes[i] = *memory.add(crate::primitives::BASE_ADDR + i);
        }
        let base = i64::from_le_bytes(base_bytes) as u32;

        if base < 2 || base > 36 {
            // Invalid base, return unchanged
            stack_push(data_stack, data_len, ud1 as i64);
            stack_push(data_stack, data_len, c_addr1 as i64);
            stack_push(data_stack, data_len, u1 as i64);
            return;
        }

        let mut result = ud1;
        let mut pos = 0;

        // Process characters
        while pos < u1 {
            let ch = *memory.add(c_addr1 + pos) as char;

            let digit_value = if ch.is_ascii_digit() {
                (ch as u32) - ('0' as u32)
            } else if ch.is_ascii_uppercase() {
                (ch as u32) - ('A' as u32) + 10
            } else if ch.is_ascii_lowercase() {
                (ch as u32) - ('a' as u32) + 10
            } else {
                break;
            };

            if digit_value >= base {
                break;
            }

            result = result.wrapping_mul(base as u64).wrapping_add(digit_value as u64);
            pos += 1;
        }

        // Push results
        stack_push(data_stack, data_len, result as i64);
        stack_push(data_stack, data_len, (c_addr1 + pos) as i64);
        stack_push(data_stack, data_len, (u1 - pos) as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_s_to_d(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        let n = stack_pop(data_stack, data_len).unwrap_or(0);
        let high = if n < 0 { -1 } else { 0 };
        stack_push(data_stack, data_len, n);     // low cell
        stack_push(data_stack, data_len, high);  // high cell
    }
}

// Additional primitives
#[unsafe(no_mangle)]
pub extern "C" fn forth_depth(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        stack_push(data_stack, data_len, *data_len as i64);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn forth_i(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // Get the current loop index (top of loop stack - 2)
        if *loop_len >= 2 {
            let index = *loop_stack.add(*loop_len - 2);
            stack_push(data_stack, data_len, index);
        }
    }
}

// Control flow helper functions

/// Pop from data stack and return 1 if zero, 0 otherwise
#[unsafe(no_mangle)]
pub extern "C" fn forth_branch_if_zero(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, _loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) -> i64 {
    unsafe {
        if let Some(val) = stack_pop(data_stack, data_len) {
            if val == 0 { 1 } else { 0 }
        } else {
            0
        }
    }
}

/// DO loop setup: pop limit and start from data stack, push to loop stack
/// Stack: ( limit start -- ) Loop stack: ( -- start limit )
#[unsafe(no_mangle)]
pub extern "C" fn forth_do_setup(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        // Pop start (TOS), then limit
        if let (Some(start), Some(limit)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            // Push in order: start first (at loop_len-2), then limit (at loop_len-1)
            stack_push(loop_stack, loop_len, start);
            stack_push(loop_stack, loop_len, limit);
        }
    }
}

/// ?DO loop setup: like DO but returns 1 if should skip loop, 0 otherwise
#[unsafe(no_mangle)]
pub extern "C" fn forth_qdo_setup(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) -> i64 {
    unsafe {
        if let (Some(start), Some(limit)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            if start == limit {
                1  // Skip loop
            } else {
                stack_push(loop_stack, loop_len, start);
                stack_push(loop_stack, loop_len, limit);
                0  // Don't skip
            }
        } else {
            0
        }
    }
}

/// LOOP: increment counter and return 1 if done, 0 if should continue
#[unsafe(no_mangle)]
pub extern "C" fn forth_loop_check(
    _data_stack: *mut i64, _data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) -> i64 {
    unsafe {
        if *loop_len >= 2 {
            let counter_ptr = loop_stack.add(*loop_len - 2);
            let limit = *loop_stack.add(*loop_len - 1);
            *counter_ptr += 1;
            if *counter_ptr == limit { 1 } else { 0 }
        } else {
            1  // Error, exit loop
        }
    }
}

/// +LOOP: add increment from TOS to counter and return 1 if done, 0 if should continue
#[unsafe(no_mangle)]
pub extern "C" fn forth_plusloop_check(
    data_stack: *mut i64, data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) -> i64 {
    unsafe {
        if let Some(increment) = stack_pop(data_stack, data_len) {
            if *loop_len >= 2 {
                let counter_ptr = loop_stack.add(*loop_len - 2);
                let limit = *loop_stack.add(*loop_len - 1);
                let old_counter = *counter_ptr;
                *counter_ptr += increment;

                // Check if crossed the boundary
                if increment >= 0 {
                    // Positive increment: exit when counter >= limit (crossed from below)
                    if *counter_ptr >= limit && old_counter < limit { 1 } else { 0 }
                } else {
                    // Negative increment: exit when counter <= limit (crossed from above)
                    if *counter_ptr <= limit && old_counter > limit { 1 } else { 0 }
                }
            } else {
                1  // Error, exit loop
            }
        } else {
            1  // Error, exit loop
        }
    }
}

/// Loop end: pop limit and counter from loop stack
#[unsafe(no_mangle)]
pub extern "C" fn forth_loop_end(
    _data_stack: *mut i64, _data_len: *mut usize,
    _return_stack: *mut i64, _return_len: *mut usize,
    _loop_stack: *mut i64, loop_len: *mut usize,
    _memory: *mut u8, _here: *mut usize,
) {
    unsafe {
        if *loop_len >= 2 {
            *loop_len -= 2;
        }
    }
}

/// Allocate string in memory and push (addr len) onto stack
#[unsafe(no_mangle)]
pub extern "C" fn forth_alloc_string(
    data_stack: *mut i64, data_len: *mut usize,
    memory: *mut u8, here: *mut usize,
    string_ptr: *const u8, len: i64,
) {
    unsafe {
        let len_usize = len as usize;
        let here_val = *here;

        // Copy string to memory
        if here_val + len_usize <= 65536 {
            std::ptr::copy_nonoverlapping(string_ptr, memory.add(here_val), len_usize);

            // Push address and length onto stack
            stack_push(data_stack, data_len, here_val as i64);
            stack_push(data_stack, data_len, len);

            // Update HERE pointer
            *here = here_val + len_usize;
        }
    }
}

/// Push variable address onto stack
#[unsafe(no_mangle)]
pub extern "C" fn forth_variable_addr(
    data_stack: *mut i64, data_len: *mut usize,
    offset: i64,
) {
    unsafe {
        // Variables start at offset 20000 in memory
        let var_addr = 20000 + offset;
        stack_push(data_stack, data_len, var_addr);
    }
}
