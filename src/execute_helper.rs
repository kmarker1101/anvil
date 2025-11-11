// execute_helper.rs - Helper for EXECUTE primitive to call back into Executor

use crate::compiler::Executor;

/// EXECUTE helper - called from LLVM JIT code to execute a word
/// This uses the global executor pointer to call execute_word
#[unsafe(no_mangle)]
pub extern "C" fn forth_execute(
    data_stack: *mut i64,
    data_len: *mut usize,
    return_stack: *mut i64,
    return_len: *mut usize,
    loop_stack: *mut i64,
    loop_len: *mut usize,
    memory: *mut u8,
    here: *mut usize,
    _exit_flag: *mut bool,
) {
    unsafe {
        let executor_ptr = crate::llvm_jit::get_global_executor();
        if executor_ptr.is_null() {
            eprintln!("ERROR: No executor available for EXECUTE");
            return;
        }

        let executor = &mut *(executor_ptr as *mut Executor);

        // Debug: Show compiler address
        let executor_compiler_ptr = executor.compiler_ptr();
        eprintln!("forth_execute: executor.compiler at {:p}", executor_compiler_ptr);

        let global_compiler_ptr = crate::llvm_jit::get_global_compiler();
        eprintln!("forth_execute: global compiler at {:p}", global_compiler_ptr);

        // Get the word name from global storage
        let word_name = match crate::llvm_jit::get_last_found_word() {
            Some(name) => name,
            None => {
                eprintln!("ERROR: No word to execute");
                return;
            }
        };

        eprintln!("forth_execute: Executing word '{}'", word_name);

        // Copy stack state from raw arrays to VM
        executor.vm_mut().data_stack.clear();
        for i in 0..*data_len {
            executor.vm_mut().data_stack.push(*data_stack.add(i));
        }

        executor.vm_mut().return_stack.clear();
        for i in 0..*return_len {
            executor.vm_mut().return_stack.push(*return_stack.add(i));
        }

        executor.vm_mut().loop_stack.clear();
        for i in 0..*loop_len {
            executor.vm_mut().loop_stack.push(*loop_stack.add(i));
        }

        // Execute the word
        if let Err(e) = executor.execute_word(&word_name) {
            eprintln!("ERROR executing {}: {}", word_name, e);
            return;
        }

        // Copy results back to raw arrays
        *data_len = executor.vm().data_stack.depth();
        for (i, val) in executor.vm().data_stack.iter().enumerate() {
            *data_stack.add(i) = *val;
        }

        *return_len = executor.vm().return_stack.depth();
        for (i, val) in executor.vm().return_stack.iter().enumerate() {
            *return_stack.add(i) = *val;
        }

        *loop_len = executor.vm().loop_stack.depth();
        for (i, val) in executor.vm().loop_stack.iter().enumerate() {
            *loop_stack.add(i) = *val;
        }
    }
}
