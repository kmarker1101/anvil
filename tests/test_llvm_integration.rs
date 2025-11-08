// Integration test - parse Forth code and execute via LLVM

use forth::llvm_jit::LLVMCompiler;
use forth::lexer::Lexer;
use forth::parser::Parser;
use forth::compiler::Compiler;
use inkwell::context::Context;

#[test]
fn test_parse_and_execute_word() {
    // Parse ": SQUARE DUP * ;"
    let input = ": SQUARE DUP * ;";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap();

    // Compile the definition to get instructions
    let mut compiler = Compiler::new();
    compiler.compile_program(program).unwrap();

    // Get the compiled word
    let word = compiler.get_word("SQUARE").unwrap();

    // Compile with LLVM
    let context = Context::create();
    let mut llvm_compiler = LLVMCompiler::new_jit(&context, "test").unwrap();
    llvm_compiler.compile_word("SQUARE", &word.instructions).unwrap();
    let func = llvm_compiler.get_function("SQUARE").unwrap();

    // Execute with 7 on stack (7 squared = 49)
    let mut data_stack = vec![0i64; 256];
    data_stack[0] = 7;
    let mut data_len = 1usize;  // Start with 7 already on stack
    let mut return_stack = vec![0i64; 256];
    let mut return_len = 0usize;
    let mut loop_stack = vec![0i64; 256];
    let mut loop_len = 0usize;
    let mut memory = vec![0u8; 65536];
    let mut here = 0usize;

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

    // Check: 7 * 7 = 49
    assert_eq!(data_len, 1);
    assert_eq!(data_stack[0], 49);
}
