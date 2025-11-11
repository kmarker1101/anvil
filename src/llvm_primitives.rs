// llvm_primitives.rs - LLVM wrapper primitives for Forth code generation
//
// These primitives allow Forth code to directly generate LLVM IR,
// enabling a self-hosted Forth compiler.

use crate::llvm_jit::LLVMCompiler;
use inkwell::AddressSpace;
use inkwell::values::BasicMetadataValueEnum;
use std::collections::HashMap;

// Helper to pop string from data stack
unsafe fn pop_string(
    data_stack: *mut i64,
    data_len: *mut usize,
    memory: *mut u8,
) -> Result<String, &'static str> {
    if *data_len < 2 {
        return Err("Stack underflow");
    }

    let len = *data_stack.offset((*data_len - 1) as isize) as usize;
    let addr = *data_stack.offset((*data_len - 2) as isize) as usize;
    *data_len -= 2;

    let slice = std::slice::from_raw_parts(memory.add(addr), len);
    String::from_utf8(slice.to_vec()).map_err(|_| "Invalid UTF-8")
}

// Helper to push value to data stack
unsafe fn stack_push(data_stack: *mut i64, data_len: *mut usize, value: i64) {
    *data_stack.offset(*data_len as isize) = value;
    *data_len += 1;
}

// Helper to pop value from data stack
unsafe fn stack_pop(data_stack: *mut i64, data_len: *mut usize) -> Option<i64> {
    if *data_len == 0 {
        return None;
    }
    *data_len -= 1;
    Some(*data_stack.offset(*data_len as isize))
}

/// LLVM-BEGIN-FUNCTION ( name-addr name-len -- )
/// Create a new LLVM function with the given name
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_begin_function(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        // Get the global LLVM compiler pointer
        let llvm_compiler_ptr = crate::llvm_jit::get_global_llvm_compiler();
        if llvm_compiler_ptr.is_null() {
            eprintln!("ERROR: No LLVM compiler available");
            return;
        }

        // Pop function name from stack
        let name = match unsafe { pop_string(data_stack, data_len, memory) } {
            Ok(n) => n,
            Err(e) => {
                eprintln!("ERROR in llvm-begin-function: {}", e);
                return;
            }
        };

        // Cast to LLVMCompiler and create function
        let llvm_compiler = &mut *(llvm_compiler_ptr as *mut crate::llvm_jit::LLVMCompiler);

        // Create LLVM function with standard Forth signature
        use inkwell::AddressSpace;
        let i64_type = llvm_compiler.context.i64_type();
        let ptr_type = llvm_compiler.context.i8_type().ptr_type(AddressSpace::default());
        let bool_type = llvm_compiler.context.bool_type();

        let fn_type = llvm_compiler.context.void_type().fn_type(&[
            ptr_type.into(), // data_stack
            ptr_type.into(), // data_len
            ptr_type.into(), // return_stack
            ptr_type.into(), // return_len
            ptr_type.into(), // loop_stack
            ptr_type.into(), // loop_len
            ptr_type.into(), // memory
            ptr_type.into(), // here
            ptr_type.into(), // exit_flag
        ], false);

        let function = llvm_compiler.module.add_function(&name, fn_type, None);

        // Create entry block
        let entry = llvm_compiler.context.append_basic_block(function, "entry");
        llvm_compiler.builder.position_at_end(entry);

        // Store as current function
        llvm_compiler.current_function = Some(function);
        llvm_compiler.user_words.insert(name.clone(), function);

        eprintln!("DEBUG llvm-begin-function: created function '{}', compiler_ptr={:p}", name, llvm_compiler_ptr);
    }
}

/// LLVM-END-FUNCTION ( -- )
/// Finish the current LLVM function definition
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_end_function(
    _data_stack: *mut i64,
    _data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let llvm_compiler_ptr = crate::llvm_jit::get_global_llvm_compiler();
        if llvm_compiler_ptr.is_null() {
            return;
        }

        let llvm_compiler = &mut *(llvm_compiler_ptr as *mut crate::llvm_jit::LLVMCompiler);

        // Build return instruction
        llvm_compiler.builder.build_return(None).unwrap();

        eprintln!("DEBUG llvm-end-function: finished function, clearing current_function");

        // Clear current function
        llvm_compiler.current_function = None;
    }
}

/// LLVM-EMIT-CALL ( word-addr word-len -- )
/// Emit a call to another word
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_emit_call(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let llvm_compiler_ptr = crate::llvm_jit::get_global_llvm_compiler();
        if llvm_compiler_ptr.is_null() {
            return;
        }

        let word = match unsafe { pop_string(data_stack, data_len, memory) } {
            Ok(w) => w,
            Err(e) => {
                eprintln!("ERROR in llvm-emit-call: {}", e);
                return;
            }
        };

        eprintln!("DEBUG llvm-emit-call: word='{}'", word);

        let llvm_compiler = &mut *(llvm_compiler_ptr as *mut crate::llvm_jit::LLVMCompiler);

        // Look up or declare the function
        // First check if it already exists in user_words or module
        let callee = if let Some(&user_func) = llvm_compiler.user_words.get(&word) {
            user_func
        } else if let Some(func) = llvm_compiler.module.get_function(&word) {
            func
        } else {
            // Function doesn't exist, declare it as an extern function
            // All Forth primitives have the same signature
            use inkwell::AddressSpace;
            let ptr_type = llvm_compiler.context.i8_type().ptr_type(AddressSpace::default());

            let fn_type = llvm_compiler.context.void_type().fn_type(&[
                ptr_type.into(), // data_stack
                ptr_type.into(), // data_len
                ptr_type.into(), // return_stack
                ptr_type.into(), // return_len
                ptr_type.into(), // loop_stack
                ptr_type.into(), // loop_len
                ptr_type.into(), // memory
                ptr_type.into(), // here
                ptr_type.into(), // exit_flag (optional, may be unused)
            ], false);

            llvm_compiler.module.add_function(&word, fn_type, None)
        };

        // Get function parameters to pass through
        let current_fn = llvm_compiler.current_function.expect("No current function");
        let params: Vec<_> = (0..9).map(|i| current_fn.get_nth_param(i).unwrap().into()).collect();

        // Build call with all parameters
        llvm_compiler.builder.build_call(callee, &params, "").unwrap();
    }
}

/// LLVM-EMIT-LITERAL ( n -- )
/// Emit code to push a literal value
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_emit_literal(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let llvm_compiler_ptr = crate::llvm_jit::get_global_llvm_compiler();
        if llvm_compiler_ptr.is_null() {
            return;
        }

        let value = match unsafe { stack_pop(data_stack, data_len) } {
            Some(v) => v,
            None => {
                eprintln!("ERROR: Stack underflow in llvm-emit-literal");
                return;
            }
        };

        eprintln!("DEBUG llvm-emit-literal: value={}, compiler_ptr={:p}", value, llvm_compiler_ptr);

        let llvm_compiler = &mut *(llvm_compiler_ptr as *mut crate::llvm_jit::LLVMCompiler);

        // Check if we have a current function and builder position
        eprintln!("DEBUG llvm-emit-literal: current_function={:?}", llvm_compiler.current_function.is_some());
        eprintln!("DEBUG llvm-emit-literal: builder insertion block={:?}", llvm_compiler.builder.get_insert_block().is_some());

        // Get or declare the forth_literal primitive function
        use inkwell::AddressSpace;
        let i64_type = llvm_compiler.context.i64_type();
        let ptr_type = llvm_compiler.context.i8_type().ptr_type(AddressSpace::default());

        let literal_func = llvm_compiler.module.get_function("forth_literal").unwrap_or_else(|| {
            // Declare: void forth_literal(i64* data_stack, usize* data_len, i64 value)
            let fn_type = llvm_compiler.context.void_type().fn_type(&[
                ptr_type.into(),
                ptr_type.into(),
                i64_type.into(),
            ], false);
            llvm_compiler.module.add_function("forth_literal", fn_type, None)
        });

        // Get function parameters (the stacks we're passing through)
        let current_fn = llvm_compiler.current_function.expect("No current function");
        eprintln!("DEBUG llvm-emit-literal: current_fn name={:?}", current_fn.get_name().to_str());
        let params: Vec<_> = (0..9).map(|i| current_fn.get_nth_param(i).unwrap().into()).collect();

        // Build call to forth_literal with value
        let value_const = llvm_compiler.context.i64_type().const_int(value as u64, false);
        let args = vec![
            params[0], // data_stack
            params[1], // data_len
            value_const.into(), // value to push
        ];

        let call_inst = llvm_compiler.builder.build_call(literal_func, &args, "").unwrap();
        eprintln!("DEBUG llvm-emit-literal: built call instruction, result={:?}", call_inst);
    }
}

/// LLVM-CREATE-LABEL ( id -- block-ptr )
/// Create a new basic block label for branching
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_create_label(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let compiler_ptr = crate::llvm_jit::get_global_compiler();
        if compiler_ptr.is_null() {
            return;
        }

        let label_id = match stack_pop(data_stack, data_len) {
            Some(id) => id,
            None => {
                eprintln!("ERROR: Stack underflow in llvm-create-label");
                return;
            }
        };

        eprintln!("TODO: llvm-create-label {}", label_id);
        stack_push(data_stack, data_len, 0); // Push dummy block pointer
    }
}

/// LLVM-EMIT-BRANCH-IF-ZERO ( block-ptr -- )
/// Emit conditional branch: jump to block if TOS is zero
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_emit_branch_if_zero(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let compiler_ptr = crate::llvm_jit::get_global_compiler();
        if compiler_ptr.is_null() {
            return;
        }

        let block_ptr = match stack_pop(data_stack, data_len) {
            Some(ptr) => ptr,
            None => {
                eprintln!("ERROR: Stack underflow in llvm-emit-branch-if-zero");
                return;
            }
        };

        eprintln!("TODO: llvm-emit-branch-if-zero {:p}", block_ptr as *const ());
    }
}

/// LLVM-POSITION-AT-LABEL ( block-ptr -- )
/// Position builder at the given label
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_position_at_label(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let compiler_ptr = crate::llvm_jit::get_global_compiler();
        if compiler_ptr.is_null() {
            return;
        }

        let block_ptr = match stack_pop(data_stack, data_len) {
            Some(ptr) => ptr,
            None => {
                eprintln!("ERROR: Stack underflow in llvm-position-at-label");
                return;
            }
        };

        eprintln!("TODO: llvm-position-at-label {:p}", block_ptr as *const ());
    }
}

/// LLVM-REGISTER-WORD ( name-addr name-len -- )
/// Register a generated LLVM function with the Compiler so it can be called
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_register_word(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        // Debug: show stack state
        eprintln!("forth_llvm_register_word: data_len={}", *data_len);
        if *data_len >= 2 {
            eprintln!("  stack[top-1]={} stack[top]={}",
                *data_stack.offset((*data_len - 2) as isize),
                *data_stack.offset((*data_len - 1) as isize));
        }

        let compiler_ptr = crate::llvm_jit::get_global_compiler();
        if compiler_ptr.is_null() {
            eprintln!("ERROR: Compiler not initialized");
            return;
        }

        // Pop function name from stack
        let name = match pop_string(data_stack, data_len, memory) {
            Ok(n) => n,
            Err(e) => {
                eprintln!("ERROR in llvm-register-word: {}", e);
                return;
            }
        };

        let compiler = &mut *(compiler_ptr as *mut crate::compiler::Compiler);

        // Add an empty instruction list - the function is already compiled in LLVM
        // The Compiler just needs to know the name exists
        eprintln!("Registering word in Compiler dictionary: {} at address {:p}", name, compiler_ptr);
        compiler.add_word_compiled(&name);
        eprintln!("Word registered successfully at address {:p}", compiler_ptr);
    }
}

/// LLVM-SHOW-MODULE ( -- )
/// Print the LLVM module IR to stdout
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_show_module(
    _data_stack: *mut i64,
    _data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    eprintln!("DEBUG forth_llvm_show_module called");
    unsafe {
        let llvm_compiler_ptr = crate::llvm_jit::get_global_llvm_compiler();
        if llvm_compiler_ptr.is_null() {
            eprintln!("ERROR: LLVM compiler not initialized");
            return;
        }

        let llvm_compiler = &*(llvm_compiler_ptr as *const crate::llvm_jit::LLVMCompiler);
        println!("{}", llvm_compiler.module.print_to_string().to_string());
    }
}

/// LLVM-EMIT-BRANCH ( block-ptr -- )
/// Emit unconditional branch to block
#[unsafe(no_mangle)]
pub extern "C" fn forth_llvm_emit_branch(
    data_stack: *mut i64,
    data_len: *mut usize,
    _return_stack: *mut i64,
    _return_len: *mut usize,
    _loop_stack: *mut i64,
    _loop_len: *mut usize,
    _memory: *mut u8,
    _here: *mut usize,
) {
    unsafe {
        let compiler_ptr = crate::llvm_jit::get_global_compiler();
        if compiler_ptr.is_null() {
            return;
        }

        let block_ptr = match stack_pop(data_stack, data_len) {
            Some(ptr) => ptr,
            None => {
                eprintln!("ERROR: Stack underflow in llvm-emit-branch");
                return;
            }
        };

        eprintln!("TODO: llvm-emit-branch {:p}", block_ptr as *const ());
    }
}
