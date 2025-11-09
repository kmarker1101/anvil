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
            &mut false,
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
            &mut false,
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
            &mut false,
        );
    }

    // Check result - should have [10, 30] on stack
    assert_eq!(data_len, 2);
    assert_eq!(data_stack[0], 10);
    assert_eq!(data_stack[1], 30);
}

#[test]
fn test_llvm_do_loop() {
    use forth::compiler::Instruction;
    use forth::primitives::Primitive;

    let context = Context::create();
    let mut compiler = LLVMCompiler::new_jit(&context, "test").unwrap();

    // Compile: 10 0 DO I LOOP  (pushes 0-9 onto stack)
    // Stack notation: ( limit start -- ) means limit under start
    let instructions = vec![
        Instruction::Literal(10),   // limit (pushed first, will be under start)
        Instruction::Literal(0),    // start (TOS)
        Instruction::DoSetup,
        // Loop body starts here (index 3)
        Instruction::Primitive(Primitive::I),  // Push loop index
        Instruction::Loop(-2),      // Branch back to index 3 if not done
        Instruction::LoopEnd,
    ];

    compiler.compile_word("test_loop", &instructions).unwrap();
    let func = compiler.get_function("test_loop").unwrap();

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
            &mut false,
        );
    }

    // Should have pushed 0-9 onto stack
    assert_eq!(data_len, 10);
    for (i, &value) in data_stack.iter().enumerate().take(10) {
        assert_eq!(value, i as i64);
    }
}

#[test]
fn test_llvm_begin_until() {
    use forth::compiler::Instruction;
    use forth::primitives::Primitive;

    let context = Context::create();
    let mut compiler = LLVMCompiler::new_jit(&context, "test").unwrap();

    // Compile: 5 BEGIN DUP . 1 - DUP 0 = UNTIL DROP
    // This counts down from 5 to 1, leaving values on stack
    let instructions = vec![
        Instruction::Literal(5),    // Start with 5
        // BEGIN (loop start is index 1)
        Instruction::Primitive(Primitive::Dup),   // Duplicate counter
        Instruction::Literal(1),    // Push 1
        Instruction::Primitive(Primitive::Sub),   // Decrement
        Instruction::Primitive(Primitive::Dup),   // Dup for test
        Instruction::Literal(0),    // Push 0
        Instruction::Primitive(Primitive::Equals), // Check if zero
        // UNTIL: branch back to index 1 if TOS is 0 (not done)
        Instruction::BranchIfZero(-7),  // Relative to next instruction
        Instruction::Primitive(Primitive::Drop),  // Clean up
    ];

    compiler.compile_word("test_until", &instructions).unwrap();
    let func = compiler.get_function("test_until").unwrap();

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
            &mut false,
        );
    }

    // Should have: 5, 4, 3, 2, 1 on stack (each duped value before decrement)
    assert_eq!(data_len, 5);
    assert_eq!(data_stack[0], 5);
    assert_eq!(data_stack[1], 4);
    assert_eq!(data_stack[2], 3);
    assert_eq!(data_stack[3], 2);
    assert_eq!(data_stack[4], 1);
}
