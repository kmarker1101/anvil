// llvm_jit.rs - LLVM-based JIT and AOT compiler for Forth
//
// This module provides unified compilation using LLVM for both:
// - JIT: Execute code immediately in memory
// - AOT: Generate object files for linking
//
// All primitives are implemented as extern "C" functions and called from generated code.

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

/// Compiled function signature:
/// fn(data_stack: *mut i64, data_len: *mut usize,
///    return_stack: *mut i64, return_len: *mut usize,
///    loop_stack: *mut i64, loop_len: *mut usize,
///    memory: *mut u8, here: *mut usize)
type ForthFunction = unsafe extern "C" fn(*mut i64, *mut usize, *mut i64, *mut usize, *mut i64, *mut usize, *mut u8, *mut usize);

pub struct LLVMCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,

    // Function registry
    functions: HashMap<String, FunctionValue<'ctx>>,

    // Primitive function declarations
    primitive_funcs: HashMap<Primitive, FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCompiler<'ctx> {
    /// Create a new LLVM compiler in JIT mode
    pub fn new_jit(context: &'ctx Context, name: &str) -> Result<Self, String> {
        let module = context.create_module(name);
        let builder = context.create_builder();

        // Initialize LLVM targets for JIT
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to initialize native target: {}", e))?;

        // Create execution engine with symbol resolver
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None) // Use None for easier debugging
            .map_err(|e| format!("Failed to create execution engine: {}", e.to_string()))?;

        Ok(LLVMCompiler {
            context,
            module,
            builder,
            execution_engine: Some(execution_engine),
            functions: HashMap::new(),
            primitive_funcs: HashMap::new(),
        })
    }

    /// Create a new LLVM compiler in AOT mode
    pub fn new_aot(context: &'ctx Context, name: &str, target_triple: Option<String>) -> Result<Self, String> {
        let module = context.create_module(name);
        let builder = context.create_builder();

        // Initialize all targets for AOT
        Target::initialize_all(&InitializationConfig::default());

        // Set target triple
        if let Some(triple) = target_triple {
            module.set_triple(&inkwell::targets::TargetTriple::create(&triple));
        }

        Ok(LLVMCompiler {
            context,
            module,
            builder,
            execution_engine: None,
            functions: HashMap::new(),
            primitive_funcs: HashMap::new(),
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
            ],
            false,
        );

        // Map primitive names to extern C function names and addresses
        let (func_name, func_addr): (&str, usize) = match primitive {
            Primitive::Fetch => ("forth_fetch", forth_fetch as usize),
            Primitive::Store => ("forth_store", forth_store as usize),
            Primitive::CFetch => ("forth_c_fetch", forth_c_fetch as usize),
            Primitive::CStore => ("forth_c_store", forth_c_store as usize),
            Primitive::Dup => ("forth_dup", forth_dup as usize),
            Primitive::Drop => ("forth_drop", forth_drop as usize),
            Primitive::Swap => ("forth_swap", forth_swap as usize),
            Primitive::Over => ("forth_over", forth_over as usize),
            Primitive::Rot => ("forth_rot", forth_rot as usize),
            Primitive::ToR => ("forth_to_r", forth_to_r as usize),
            Primitive::FromR => ("forth_from_r", forth_from_r as usize),
            Primitive::RFetch => ("forth_r_fetch", forth_r_fetch as usize),
            Primitive::Add => ("forth_add", forth_add as usize),
            Primitive::Sub => ("forth_sub", forth_sub as usize),
            Primitive::Mul => ("forth_mul", forth_mul as usize),
            Primitive::Div => ("forth_div", forth_div as usize),
            Primitive::Mod => ("forth_mod", forth_mod as usize),
            Primitive::Equals => ("forth_equals", forth_equals as usize),
            Primitive::Less => ("forth_less", forth_less as usize),
            Primitive::Greater => ("forth_greater", forth_greater as usize),
            Primitive::And => ("forth_and", forth_and as usize),
            Primitive::Or => ("forth_or", forth_or as usize),
            Primitive::Xor => ("forth_xor", forth_xor as usize),
            Primitive::Invert => ("forth_invert", forth_invert as usize),
            Primitive::Emit => ("forth_emit", forth_emit as usize),
            Primitive::Key => ("forth_key", 0), // TODO: implement forth_key
            Primitive::Dot => ("forth_dot", forth_dot as usize),
            Primitive::Cr => ("forth_cr", forth_cr as usize),
            Primitive::Type => ("forth_type", 0), // TODO: implement forth_type
            Primitive::I => ("forth_i", forth_i as usize),
            Primitive::Depth => ("forth_depth", forth_depth as usize),
        };

        let func = self.module.add_function(func_name, fn_type, None);

        // Register the function address with the execution engine
        if let Some(ee) = &self.execution_engine {
            ee.add_global_mapping(&func, func_addr);
        }

        self.primitive_funcs.insert(primitive, func);
        func
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
            ],
            false,
        );

        let function = self.module.add_function(name, fn_type, None);
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

        // Compile each instruction
        for instruction in instructions {
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
                        ],
                        "",
                    ).map_err(|e| format!("Failed to build call: {}", e))?;
                }

                Instruction::Call(word_name) => {
                    // Call another Forth word
                    let callee = self.functions.get(word_name)
                        .ok_or_else(|| format!("Undefined word: {}", word_name))?;

                    self.builder.build_call(
                        *callee,
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
                    ).map_err(|e| format!("Failed to build call: {}", e))?;
                }

                Instruction::Return => {
                    // Early return from current word
                    self.builder.build_return(None)
                        .map_err(|e| format!("Failed to build return: {}", e))?;
                    // Don't add any more instructions after this
                    return Ok(());
                }

                _ => {
                    return Err(format!("Instruction not yet implemented: {:?}", instruction));
                }
            }
        }

        // Return at end of function (only reached if no explicit Return instruction)
        self.builder.build_return(None)
            .map_err(|e| format!("Failed to build return: {}", e))?;

        // Verify function
        if !function.verify(true) {
            return Err(format!("Function verification failed for {}", name));
        }

        self.functions.insert(name.to_string(), function);
        Ok(())
    }

    /// Get a JIT-compiled function pointer
    pub fn get_function(&self, name: &str) -> Result<ForthFunction, String> {
        let ee = self.execution_engine.as_ref()
            .ok_or_else(|| "No execution engine (AOT mode?)".to_string())?;

        unsafe {
            let func: JitFunction<ForthFunction> = ee
                .get_function(name)
                .map_err(|e| format!("Failed to get function: {}", e))?;

            // Get the raw function pointer and transmute it
            let func_ptr = func.as_raw() as *const ();
            let forth_func: ForthFunction = std::mem::transmute(func_ptr);
            Ok(forth_func)
        }
    }

    /// Generate object file for AOT compilation
    pub fn emit_object_file(&self, path: &str) -> Result<(), String> {
        let target_triple = self.module.get_triple();
        let target = Target::from_triple(&target_triple)
            .map_err(|e| format!("Failed to get target: {}", e.to_string()))?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
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
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            if b != 0 {
                stack_push(data_stack, data_len, a / b);
            }
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
        if let (Some(b), Some(a)) = (stack_pop(data_stack, data_len), stack_pop(data_stack, data_len)) {
            if b != 0 {
                stack_push(data_stack, data_len, a % b);
            }
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
                for i in 0..8 {
                    *memory.add(addr + i) = bytes[i];
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
        // Get the current loop index (top of loop stack)
        if let Some(index) = stack_peek(loop_stack, loop_len, 0) {
            stack_push(data_stack, data_len, index);
        }
    }
}
