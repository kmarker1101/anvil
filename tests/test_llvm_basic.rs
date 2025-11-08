// Test basic LLVM JIT compilation

use forth::llvm_jit::LLVMCompiler;
use forth::compiler::Instruction;
use forth::primitives::Primitive;
use inkwell::context::Context;

#[test]
fn test_llvm_literal_and_add() {
    eprintln!("Creating context...");
    let context = Context::create();

    eprintln!("Creating compiler...");
    let mut compiler = LLVMCompiler::new_jit(&context, "test").unwrap();

    // Compile: 2 3 +
    let instructions = vec![
        Instruction::Literal(2),
        Instruction::Literal(3),
        Instruction::Primitive(Primitive::Add),
    ];

    eprintln!("Compiling word...");
    compiler.compile_word("test_add", &instructions).unwrap();

    // Print IR for debugging
    eprintln!("Printing IR...");
    compiler.print_ir();

    eprintln!("Getting function...");
    let func = compiler.get_function("test_add").unwrap();

    // Set up stacks as raw arrays
    let mut data_stack = vec![0i64; 256];
    let mut data_len = 0usize;
    let mut return_stack = vec![0i64; 256];
    let mut return_len = 0usize;
    let mut loop_stack = vec![0i64; 256];
    let mut loop_len = 0usize;
    let mut memory = vec![0u8; 65536];
    let mut here = 0usize;

    // Execute
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
        );
    }

    // Check result
    eprintln!("Result: data_len={}, data_stack[0]={}", data_len, data_stack[0]);
    assert_eq!(data_len, 1);
    assert_eq!(data_stack[0], 5);
}

#[test]
fn test_llvm_dup_multiply() {
    let context = Context::create();
    let mut compiler = LLVMCompiler::new_jit(&context, "test").unwrap();

    // Compile: 7 DUP *  (7 squared = 49)
    let instructions = vec![
        Instruction::Literal(7),
        Instruction::Primitive(Primitive::Dup),
        Instruction::Primitive(Primitive::Mul),
    ];

    compiler.compile_word("test_square", &instructions).unwrap();
    let func = compiler.get_function("test_square").unwrap();

    // Set up stacks
    let mut data_stack = vec![0i64; 256];
    let mut data_len = 0usize;
    let mut return_stack = vec![0i64; 256];
    let mut return_len = 0usize;
    let mut loop_stack = vec![0i64; 256];
    let mut loop_len = 0usize;
    let mut memory = vec![0u8; 65536];
    let mut here = 0usize;

    // Execute
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
        );
    }

    // Check result
    assert_eq!(data_len, 1);
    assert_eq!(data_stack[0], 49);
}

#[test]
fn test_llvm_stack_manipulation() {
    let context = Context::create();
    let mut compiler = LLVMCompiler::new_jit(&context, "test").unwrap();

    // Compile: 10 20 30 SWAP DROP  (should leave 10 30)
    let instructions = vec![
        Instruction::Literal(10),
        Instruction::Literal(20),
        Instruction::Literal(30),
        Instruction::Primitive(Primitive::Swap),
        Instruction::Primitive(Primitive::Drop),
    ];

    compiler.compile_word("test_stack", &instructions).unwrap();
    let func = compiler.get_function("test_stack").unwrap();

    // Set up stacks
    let mut data_stack = vec![0i64; 256];
    let mut data_len = 0usize;
    let mut return_stack = vec![0i64; 256];
    let mut return_len = 0usize;
    let mut loop_stack = vec![0i64; 256];
    let mut loop_len = 0usize;
    let mut memory = vec![0u8; 65536];
    let mut here = 0usize;

    // Execute
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
        );
    }

    // Check result - should have [10, 30] on stack
    assert_eq!(data_len, 2);
    assert_eq!(data_stack[0], 10);
    assert_eq!(data_stack[1], 30);
}
