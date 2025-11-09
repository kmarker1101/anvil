use forth::compiler::Executor;
use forth::lexer::Lexer;
use forth::parser::Parser;

fn compile_and_run(input: &str) -> Result<Executor, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    let mut executor = Executor::new();
    executor.vm_mut().set_input(input);
    executor.execute_program(program)?;

    Ok(executor)
}

fn compile_and_run_with_stdlib(input: &str) -> Result<Executor, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    let mut executor = Executor::with_stdlib()?;
    executor.vm_mut().set_input(input);
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

        // Second expression (using same executor - this is the key REPL requirement!)
        let mut lexer = Lexer::new("3 DUP *");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 9);
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
        assert!(
            executor.compiler().get_word("0<").is_some(),
            "0< word should be defined"
        );

        // Test 0< directly
        executor.vm_mut().data_stack.push(-42);
        executor.execute_word("0<").unwrap();
        let result = executor.vm_mut().data_stack.pop().unwrap();
        eprintln!("0< with -42 returned: {}", result);
        assert_eq!(
            result, -1,
            "0< should return -1 (true) for negative numbers"
        );

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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Store)
            .unwrap();

        // Fetch the value back
        executor.vm_mut().data_stack.push(addr);
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Fetch)
            .unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_variable_multiple() {
        let mut executor = Executor::new();

        // Create multiple variables
        let source = "VARIABLE X VARIABLE Y VARIABLE Z";
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Initialize counter to 0
        executor.execute_word("COUNTER").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(0);
        executor.vm_mut().data_stack.push(addr);
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Store)
            .unwrap();

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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Store value
        executor.execute_word("DATA").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(12345);
        executor.vm_mut().data_stack.push(addr);
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Store)
            .unwrap();

        // Execute DATA again - should get same address
        executor.execute_word("DATA").unwrap();
        let addr2 = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(addr, addr2);

        // Value should still be there
        executor.vm_mut().data_stack.push(addr);
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Fetch)
            .unwrap();
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Sum 0 to 9 (should be 45)
        executor.vm_mut().data_stack.push(10);
        executor.execute_word("SUMTO").unwrap();

        // Fetch the sum
        executor.execute_word("SUM").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        executor.vm_mut().data_stack.push(addr);
        executor
            .vm_mut()
            .execute_primitive(forth::primitives::Primitive::Fetch)
            .unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 45);
    }

    #[test]
    fn test_depth_basic() {
        let mut executor = Executor::new();

        // Empty stack
        let source = "DEPTH";
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);
    }

    #[test]
    fn test_depth_with_values() {
        let mut executor = Executor::new();

        // Push 3 values, then check depth
        let source = "1 2 3 DEPTH";
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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
        let mut lexer = forth::lexer::Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = forth::parser::Parser::new(tokens);
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

    #[test]
    fn test_constant_basic() {
        let mut executor = compile_and_run("42 CONSTANT THE-ANSWER").unwrap();

        // Now call THE-ANSWER - it should push 42
        executor.execute_word("THE-ANSWER").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_constant_in_word() {
        let mut executor = compile_and_run("100 CONSTANT VALUE : TEST VALUE DUP + ;").unwrap();

        // Call TEST - should push VALUE twice and add them
        executor.execute_word("TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 200);
    }

    #[test]
    fn test_constant_multiple() {
        let mut executor = compile_and_run("10 CONSTANT TEN 20 CONSTANT TWENTY 30 CONSTANT THIRTY").unwrap();

        executor.execute_word("TEN").unwrap();
        executor.execute_word("TWENTY").unwrap();
        executor.execute_word("THIRTY").unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 30);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 20);
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
    }

    #[test]
    fn test_constant_negative() {
        let mut executor = compile_and_run("-42 CONSTANT NEG").unwrap();

        executor.execute_word("NEG").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -42);
    }

    #[test]
    fn test_constant_zero() {
        let mut executor = compile_and_run("0 CONSTANT ZERO").unwrap();

        executor.execute_word("ZERO").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 0);
    }

    // ========================================================================
    // BASE TESTS
    // ========================================================================

    #[test]
    fn test_base_address() {
        let mut executor = compile_and_run("BASE").unwrap();

        executor.execute_word("BASE").unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(addr, 256); // BASE_ADDR is 0x100
    }

    #[test]
    fn test_base_initialized_to_10() {
        let mut executor = compile_and_run("BASE @").unwrap();

        executor.execute_word("BASE").unwrap();
        executor.execute_word("@").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
    }

    #[test]
    fn test_decimal_prefix() {
        let mut executor = compile_and_run(": TEST #42 ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_hex_prefix() {
        let mut executor = compile_and_run(": TEST $FF ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 255);
    }

    #[test]
    fn test_binary_prefix() {
        let mut executor = compile_and_run(": TEST %1010 ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 10);
    }

    #[test]
    fn test_negative_decimal_prefix() {
        let mut executor = compile_and_run(": TEST #-42 ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -42);
    }

    #[test]
    fn test_negative_hex_prefix() {
        let mut executor = compile_and_run(": TEST $-FF ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -255);
    }

    #[test]
    fn test_negative_binary_prefix() {
        let mut executor = compile_and_run(": TEST %-101 ; TEST").unwrap();
        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), -5);
    }

    #[test]
    fn test_hex_word() {
        let mut executor = compile_and_run_with_stdlib(": TEST DECIMAL BASE @ HEX BASE @ ; TEST").unwrap();

        let hex_base = executor.vm_mut().data_stack.pop().unwrap();
        let decimal_base = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(decimal_base, 10);
        assert_eq!(hex_base, 16);
    }

    #[test]
    fn test_decimal_word() {
        let mut executor = compile_and_run_with_stdlib(": TEST HEX BASE @ DECIMAL BASE @ ; TEST").unwrap();

        let decimal_base = executor.vm_mut().data_stack.pop().unwrap();
        let hex_base = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(hex_base, 16);
        assert_eq!(decimal_base, 10);
    }

    #[test]
    fn test_base_can_be_modified() {
        let mut executor = compile_and_run(": TEST 42 BASE ! BASE @ ; TEST").unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 42);
    }

    #[test]
    fn test_mixed_prefix_arithmetic() {
        let mut executor = compile_and_run(": TEST #10 $10 + ; TEST").unwrap();

        assert_eq!(executor.vm_mut().data_stack.pop().unwrap(), 26); // 10 + 16
    }

    // ============================================================================
    // WORD Tests
    // ============================================================================

    #[test]
    fn test_word_parse_space_delimited() {
        // Set input buffer to "HELLO WORLD", then parse with space delimiter
        let input_text = "HELLO WORLD";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // Call WORD with space delimiter (32)
        let mut lexer = Lexer::new("#32 WORD");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // WORD should return address of counted string
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        // First byte is length, then characters
        let len = executor.vm().memory[addr] as usize;
        assert_eq!(len, 5, "WORD should parse 'HELLO'");

        // The parsed word should be "HELLO"
        let word_bytes = &executor.vm().memory[addr + 1..addr + 1 + len];
        let word = String::from_utf8_lossy(word_bytes);
        assert_eq!(word, "HELLO");
    }

    #[test]
    fn test_word_to_in_variable() {
        let mut executor = compile_and_run(">IN @").unwrap();

        // >IN should initially be 0
        let to_in = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(to_in, 0);
    }

    #[test]
    fn test_word_advances_to_in() {
        let mut executor = compile_and_run("#32 WORD >IN @").unwrap();

        // After parsing first word, >IN should be advanced
        let to_in = executor.vm_mut().data_stack.pop().unwrap();
        assert!(to_in > 0, ">IN should advance after WORD");
    }

    #[test]
    fn test_source_returns_input_buffer() {
        use forth::primitives::INPUT_BUFFER_ADDR;

        let input = "HELLO WORLD";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Call SOURCE primitive
        let mut lexer = Lexer::new("SOURCE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // SOURCE returns (addr len)
        let len = executor.vm_mut().data_stack.pop().unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(addr, INPUT_BUFFER_ADDR as i64, "SOURCE should return INPUT_BUFFER_ADDR");
        assert_eq!(len, input.len() as i64, "SOURCE should return correct length");
    }

    #[test]
    fn test_word_skip_leading_delimiters() {
        // Multiple spaces before the word
        let mut executor = compile_and_run("   #32 WORD").unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;

        // Should still parse "#32"
        assert!(len > 0);
    }

    #[test]
    fn test_word_with_custom_delimiter() {
        // Parse until comma
        let input = "HELLO,WORLD";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Push comma ASCII (44) as delimiter and call WORD
        let mut lexer = Lexer::new("#44 WORD");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;

        assert_eq!(len, 5, "Should parse 'HELLO' before comma");
        let word_bytes = &executor.vm().memory[addr + 1..addr + 1 + len];
        let word = String::from_utf8_lossy(word_bytes);
        assert_eq!(word, "HELLO");
    }

    #[test]
    fn test_word_returns_empty_at_end() {
        use forth::primitives::Primitive;

        let input = "TEST";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Parse first word to consume "TEST" - push 32 and call WORD
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();

        // Check that we parsed "TEST"
        let addr1 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len1 = executor.vm().memory[addr1] as usize;
        assert_eq!(len1, 4, "First WORD should parse 'TEST'");

        // Now >IN should be at end of input
        // Try to parse another word (should be empty since we're at end)
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;

        // When at end of input, WORD should return empty counted string
        assert_eq!(len, 0, "WORD should return empty string at end of input");
    }

    #[test]
    fn test_word_buffer_address() {
        use forth::primitives::WORD_BUFFER_ADDR;

        let mut executor = compile_and_run("#32 WORD").unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        assert_eq!(addr, WORD_BUFFER_ADDR, "WORD should return WORD_BUFFER_ADDR");
    }

    #[test]
    fn test_source_address() {
        use forth::primitives::INPUT_BUFFER_ADDR;

        let mut executor = compile_and_run("SOURCE DROP").unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        assert_eq!(addr, INPUT_BUFFER_ADDR, "SOURCE should return INPUT_BUFFER_ADDR");
    }

    // ============================================================================
    // SOURCE Tests
    // ============================================================================

    #[test]
    fn test_source_pushes_two_values() {
        let input = "TEST INPUT";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        let mut lexer = Lexer::new("SOURCE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // Should have 2 values on stack
        let len = executor.vm_mut().data_stack.pop().unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap();

        assert!(addr > 0);
        assert_eq!(len, input.len() as i64);
    }

    #[test]
    fn test_source_length_changes_with_input() {
        use forth::primitives::Primitive;

        let mut executor = Executor::new();

        // First input
        executor.vm_mut().set_input("SHORT");
        executor.vm_mut().execute_primitive(Primitive::Source).unwrap();
        let len1 = executor.vm_mut().data_stack.pop().unwrap();
        let _addr1 = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(len1, 5);

        // Second input
        executor.vm_mut().set_input("MUCH LONGER INPUT");
        executor.vm_mut().execute_primitive(Primitive::Source).unwrap();
        let len2 = executor.vm_mut().data_stack.pop().unwrap();
        let _addr2 = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(len2, 17);
    }

    #[test]
    fn test_source_memory_contents() {
        use forth::primitives::INPUT_BUFFER_ADDR;

        let input = "HELLO";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Verify the input is actually in memory
        let memory_slice = &executor.vm().memory[INPUT_BUFFER_ADDR..INPUT_BUFFER_ADDR + 5];
        assert_eq!(memory_slice, b"HELLO");
    }

    // ============================================================================
    // >IN Tests
    // ============================================================================

    #[test]
    fn test_to_in_is_address() {
        use forth::primitives::TO_IN_ADDR;

        let mut executor = compile_and_run(">IN").unwrap();

        let addr = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(addr, TO_IN_ADDR as i64, ">IN should return TO_IN_ADDR");
    }

    #[test]
    fn test_to_in_fetch_initial_value() {
        let mut executor = compile_and_run(">IN @").unwrap();

        let value = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(value, 0, ">IN should initially be 0");
    }

    #[test]
    fn test_to_in_can_be_modified() {
        let mut executor = compile_and_run(": TEST #42 >IN ! >IN @ ; TEST").unwrap();

        let value = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(value, 42, ">IN should be modifiable via store");
    }

    #[test]
    fn test_to_in_updated_by_word() {
        use forth::primitives::Primitive;

        let input = "HELLO WORLD";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Get initial >IN
        executor.vm_mut().execute_primitive(Primitive::ToIn).unwrap();
        let to_in_addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        let mut initial_bytes = [0u8; 8];
        initial_bytes.copy_from_slice(&executor.vm().memory[to_in_addr..to_in_addr + 8]);
        let initial = i64::from_le_bytes(initial_bytes);
        assert_eq!(initial, 0);

        // Parse a word
        executor.vm_mut().data_stack.push(32); // space delimiter
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let _addr = executor.vm_mut().data_stack.pop().unwrap();

        // Check >IN was updated
        let mut updated_bytes = [0u8; 8];
        updated_bytes.copy_from_slice(&executor.vm().memory[to_in_addr..to_in_addr + 8]);
        let updated = i64::from_le_bytes(updated_bytes);
        assert!(updated > 0, ">IN should advance after WORD");
        assert_eq!(updated, 5, ">IN should be at position 5 after parsing 'HELLO'");
    }

    #[test]
    fn test_to_in_multiple_words() {
        use forth::primitives::Primitive;

        let input = "ONE TWO THREE";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Parse first word
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr1 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len1 = executor.vm().memory[addr1] as usize;
        let word1_bytes = &executor.vm().memory[addr1 + 1..addr1 + 1 + len1];
        assert_eq!(word1_bytes, b"ONE");

        // Parse second word
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr2 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len2 = executor.vm().memory[addr2] as usize;
        let word2_bytes = &executor.vm().memory[addr2 + 1..addr2 + 1 + len2];
        assert_eq!(word2_bytes, b"TWO");

        // Parse third word
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr3 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len3 = executor.vm().memory[addr3] as usize;
        let word3_bytes = &executor.vm().memory[addr3 + 1..addr3 + 1 + len3];
        assert_eq!(word3_bytes, b"THREE");
    }

    // ============================================================================
    // Integration Tests
    // ============================================================================

    #[test]
    fn test_source_and_to_in_together() {
        use forth::primitives::Primitive;

        let input = "TESTING";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Get SOURCE
        executor.vm_mut().execute_primitive(Primitive::Source).unwrap();
        let source_len = executor.vm_mut().data_stack.pop().unwrap();
        let source_addr = executor.vm_mut().data_stack.pop().unwrap();

        // Get >IN
        executor.vm_mut().execute_primitive(Primitive::ToIn).unwrap();
        executor.vm_mut().execute_primitive(Primitive::Fetch).unwrap();
        let to_in_value = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(to_in_value, 0, ">IN should be 0 initially");
        assert_eq!(source_len, 7, "SOURCE length should match input");
        assert!(source_addr > 0, "SOURCE address should be valid");
    }

    #[test]
    fn test_word_with_different_delimiters() {
        use forth::primitives::Primitive;

        // Test that WORD works with different delimiters
        // Note: WORD only skips the delimiter it's given, not others
        let input = "  AAA   BBB  ";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input);

        // Parse with space delimiter - should skip leading spaces and get "AAA"
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;
        let word_bytes = &executor.vm().memory[addr + 1..addr + 1 + len];
        assert_eq!(word_bytes, b"AAA");

        // Parse with space delimiter again - should skip spaces and get "BBB"
        executor.vm_mut().data_stack.push(32);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;
        let word_bytes = &executor.vm().memory[addr + 1..addr + 1 + len];
        assert_eq!(word_bytes, b"BBB");

        // Test with comma delimiter
        let input2 = ",,,HELLO,,,WORLD,";
        executor.vm_mut().set_input(input2);

        // Skip leading commas and get "HELLO"
        executor.vm_mut().data_stack.push(44);
        executor.vm_mut().execute_primitive(Primitive::Word).unwrap();
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let len = executor.vm().memory[addr] as usize;
        let word_bytes = &executor.vm().memory[addr + 1..addr + 1 + len];
        assert_eq!(word_bytes, b"HELLO");
    }

    // ============================================================================
    // COMPARE Tests
    // ============================================================================

    #[test]
    fn test_compare_equal_strings() {
        let mut executor = Executor::new();

        // Write "HELLO" at address 1000
        let addr1 = 1000;
        let s1 = b"HELLO";
        for (i, &byte) in s1.iter().enumerate() {
            executor.vm_mut().memory[addr1 + i] = byte;
        }

        // Write "HELLO" at address 2000
        let addr2 = 2000;
        for (i, &byte) in s1.iter().enumerate() {
            executor.vm_mut().memory[addr2 + i] = byte;
        }

        // COMPARE should return 0
        let mut lexer = Lexer::new("1000 5 2000 5 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 0, "Equal strings should return 0");
    }

    #[test]
    fn test_compare_first_less() {
        let mut executor = Executor::new();

        // "abcde" < "abdde"
        let addr1 = 1000;
        executor.vm_mut().memory[addr1..addr1 + 5].copy_from_slice(b"abcde");

        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 5].copy_from_slice(b"abdde");

        let mut lexer = Lexer::new("1000 5 2000 5 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, -1, "\"abcde\" < \"abdde\" should return -1");
    }

    #[test]
    fn test_compare_first_greater() {
        let mut executor = Executor::new();

        // "abcde" > "abbde"
        let addr1 = 1000;
        executor.vm_mut().memory[addr1..addr1 + 5].copy_from_slice(b"abcde");

        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 5].copy_from_slice(b"abbde");

        let mut lexer = Lexer::new("1000 5 2000 5 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 1, "\"abcde\" > \"abbde\" should return 1");
    }

    #[test]
    fn test_compare_shorter_first() {
        let mut executor = Executor::new();

        // "abc" vs "abcde" (first is shorter prefix)
        let addr1 = 1000;
        executor.vm_mut().memory[addr1..addr1 + 3].copy_from_slice(b"abc");

        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 5].copy_from_slice(b"abcde");

        let mut lexer = Lexer::new("1000 3 2000 5 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, -1, "Shorter string should return -1");
    }

    #[test]
    fn test_compare_longer_first() {
        let mut executor = Executor::new();

        // "abcde" vs "abc" (first is longer)
        let addr1 = 1000;
        executor.vm_mut().memory[addr1..addr1 + 5].copy_from_slice(b"abcde");

        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 3].copy_from_slice(b"abc");

        let mut lexer = Lexer::new("1000 5 2000 3 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 1, "Longer string should return 1");
    }

    #[test]
    fn test_compare_empty_strings() {
        let mut executor = Executor::new();

        // Compare two empty strings
        let mut lexer = Lexer::new("1000 0 2000 0 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 0, "Two empty strings should be equal");
    }

    #[test]
    fn test_compare_empty_vs_nonempty() {
        let mut executor = Executor::new();

        // "" vs "abc"
        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 3].copy_from_slice(b"abc");

        let mut lexer = Lexer::new("1000 0 2000 3 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, -1, "Empty string < non-empty should return -1");
    }

    #[test]
    fn test_compare_case_sensitive() {
        let mut executor = Executor::new();

        // "abc" vs "aBc" (case matters: 'b' > 'B')
        let addr1 = 1000;
        executor.vm_mut().memory[addr1..addr1 + 3].copy_from_slice(b"abc");

        let addr2 = 2000;
        executor.vm_mut().memory[addr2..addr2 + 3].copy_from_slice(b"aBc");

        let mut lexer = Lexer::new("1000 3 2000 3 COMPARE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let result = executor.vm_mut().data_stack.pop().unwrap();
        assert_eq!(result, 1, "'b' (98) > 'B' (66), should return 1");
    }

    #[test]
    fn test_to_number_decimal() {
        let mut executor = Executor::new();

        // Set BASE to 10
        let base_bytes = 10i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "123" at address 1000
        executor.vm_mut().memory[1000] = b'1';
        executor.vm_mut().memory[1001] = b'2';
        executor.vm_mut().memory[1002] = b'3';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        // Start with 0, address 1000, length 3
        let input = "0 1000 3 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 123, "Should parse '123' to 123");
        assert_eq!(c_addr2, 1003, "Should advance address by 3");
        assert_eq!(u2, 0, "Should consume all 3 characters");
    }

    #[test]
    fn test_to_number_hex_uppercase() {
        let mut executor = Executor::new();

        // Set BASE to 16
        let base_bytes = 16i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "1A2F" at address 1000
        executor.vm_mut().memory[1000] = b'1';
        executor.vm_mut().memory[1001] = b'A';
        executor.vm_mut().memory[1002] = b'2';
        executor.vm_mut().memory[1003] = b'F';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let input = "0 1000 4 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 0x1A2F, "Should parse '1A2F' to 6703");
        assert_eq!(c_addr2, 1004, "Should advance address by 4");
        assert_eq!(u2, 0, "Should consume all 4 characters");
    }

    #[test]
    fn test_to_number_hex_lowercase() {
        let mut executor = Executor::new();

        // Set BASE to 16
        let base_bytes = 16i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "abc" at address 1000
        executor.vm_mut().memory[1000] = b'a';
        executor.vm_mut().memory[1001] = b'b';
        executor.vm_mut().memory[1002] = b'c';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let input = "0 1000 3 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 0xabc, "Should parse 'abc' to 2748");
        assert_eq!(c_addr2, 1003, "Should advance address by 3");
        assert_eq!(u2, 0, "Should consume all 3 characters");
    }

    #[test]
    fn test_to_number_binary() {
        let mut executor = Executor::new();

        // Set BASE to 2
        let base_bytes = 2i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "1101" at address 1000
        executor.vm_mut().memory[1000] = b'1';
        executor.vm_mut().memory[1001] = b'1';
        executor.vm_mut().memory[1002] = b'0';
        executor.vm_mut().memory[1003] = b'1';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let input = "0 1000 4 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 0b1101, "Should parse '1101' to 13");
        assert_eq!(c_addr2, 1004, "Should advance address by 4");
        assert_eq!(u2, 0, "Should consume all 4 characters");
    }

    #[test]
    fn test_to_number_partial_parse() {
        let mut executor = Executor::new();

        // Set BASE to 10
        let base_bytes = 10i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "123XYZ" at address 1000
        executor.vm_mut().memory[1000] = b'1';
        executor.vm_mut().memory[1001] = b'2';
        executor.vm_mut().memory[1002] = b'3';
        executor.vm_mut().memory[1003] = b'X';
        executor.vm_mut().memory[1004] = b'Y';
        executor.vm_mut().memory[1005] = b'Z';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let input = "0 1000 6 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 123, "Should parse '123' and stop at 'X'");
        assert_eq!(c_addr2, 1003, "Should point to 'X'");
        assert_eq!(u2, 3, "Should leave 3 characters unparsed");
    }

    #[test]
    fn test_to_number_empty_string() {
        let mut executor = Executor::new();

        // Set BASE to 10
        let base_bytes = 10i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        // Empty string: length 0
        let input = "42 1000 0 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(ud2, 42, "Should keep initial value");
        assert_eq!(c_addr2, 1000, "Should keep address unchanged");
        assert_eq!(u2, 0, "Should keep length at 0");
    }

    #[test]
    fn test_to_number_with_accumulator() {
        let mut executor = Executor::new();

        // Set BASE to 10
        let base_bytes = 10i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "456" at address 1000
        executor.vm_mut().memory[1000] = b'4';
        executor.vm_mut().memory[1001] = b'5';
        executor.vm_mut().memory[1002] = b'6';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        // Start with accumulator value 123
        let input = "123 1000 3 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        // 123 * 10 + 4 = 1234
        // 1234 * 10 + 5 = 12345
        // 12345 * 10 + 6 = 123456
        assert_eq!(ud2, 123456, "Should accumulate starting from 123");
        assert_eq!(c_addr2, 1003, "Should advance address by 3");
        assert_eq!(u2, 0, "Should consume all 3 characters");
    }

    #[test]
    fn test_to_number_invalid_digit_for_base() {
        let mut executor = Executor::new();

        // Set BASE to 8 (octal)
        let base_bytes = 8i64.to_le_bytes();
        executor.vm_mut().memory[0x100..0x108].copy_from_slice(&base_bytes);

        // Write "1238" at address 1000 (8 is invalid for octal)
        executor.vm_mut().memory[1000] = b'1';
        executor.vm_mut().memory[1001] = b'2';
        executor.vm_mut().memory[1002] = b'3';
        executor.vm_mut().memory[1003] = b'8';

        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        let input = "0 1000 4 >NUMBER";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let u2 = executor.vm_mut().data_stack.pop().unwrap();
        let c_addr2 = executor.vm_mut().data_stack.pop().unwrap();
        let ud2 = executor.vm_mut().data_stack.pop().unwrap();

        // Should parse "123" in octal = 1*64 + 2*8 + 3 = 83
        assert_eq!(ud2, 83, "Should parse '123' in octal and stop at '8'");
        assert_eq!(c_addr2, 1003, "Should point to '8'");
        assert_eq!(u2, 1, "Should leave 1 character unparsed");
    }

    // ============================================================================
    // S>D Tests
    // ============================================================================

    #[test]
    fn test_s_to_d_zero() {
        let mut executor = Executor::new();

        // S>D ( n -- d )
        // 0 S>D -> 0 0
        let input = "0 S>D";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, 0, "Low cell should be 0");
        assert_eq!(high, 0, "High cell should be 0");
    }

    #[test]
    fn test_s_to_d_positive_one() {
        let mut executor = Executor::new();

        // 1 S>D -> 1 0
        let input = "1 S>D";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, 1, "Low cell should be 1");
        assert_eq!(high, 0, "High cell should be 0");
    }

    #[test]
    fn test_s_to_d_positive_two() {
        let mut executor = Executor::new();

        // 2 S>D -> 2 0
        let input = "2 S>D";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, 2, "Low cell should be 2");
        assert_eq!(high, 0, "High cell should be 0");
    }

    #[test]
    fn test_s_to_d_negative_one() {
        let mut executor = Executor::new();

        // -1 S>D -> -1 -1
        let input = "-1 S>D";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, -1, "Low cell should be -1");
        assert_eq!(high, -1, "High cell should be -1 for sign extension");
    }

    #[test]
    fn test_s_to_d_negative_two() {
        let mut executor = Executor::new();

        // -2 S>D -> -2 -1
        let input = "-2 S>D";
        let tokens = Lexer::new(input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, -2, "Low cell should be -2");
        assert_eq!(high, -1, "High cell should be -1 for sign extension");
    }

    #[test]
    fn test_s_to_d_min_int() {
        let mut executor = Executor::new();

        // MIN-INT S>D -> MIN-INT -1
        let min_int = i64::MIN;
        let input = format!("{} S>D", min_int);
        let tokens = Lexer::new(&input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, min_int, "Low cell should be MIN-INT");
        assert_eq!(high, -1, "High cell should be -1 for sign extension");
    }

    #[test]
    fn test_s_to_d_max_int() {
        let mut executor = Executor::new();

        // MAX-INT S>D -> MAX-INT 0
        let max_int = i64::MAX;
        let input = format!("{} S>D", max_int);
        let tokens = Lexer::new(&input).tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let high = executor.vm_mut().data_stack.pop().unwrap();
        let low = executor.vm_mut().data_stack.pop().unwrap();

        assert_eq!(low, max_int, "Low cell should be MAX-INT");
        assert_eq!(high, 0, "High cell should be 0");
    }

    // ============================================================================
    // PARSE Tests
    // ============================================================================

    #[test]
    fn test_parse_basic() {
        // Test basic PARSE with space delimiter
        let input_text = "HELLO WORLD";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // Call PARSE with space delimiter (32)
        let mut lexer = Lexer::new("#32 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // PARSE should return length and address
        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        assert_eq!(len, 5, "PARSE should return length 5");

        // Check the parsed string
        let parsed_bytes = &executor.vm().memory[addr..addr + len];
        let parsed = String::from_utf8_lossy(parsed_bytes);
        assert_eq!(parsed, "HELLO");
    }

    #[test]
    fn test_parse_no_skip_leading() {
        // PARSE does NOT skip leading delimiters (unlike WORD)
        let input_text = "  HELLO";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // Call PARSE with space delimiter
        let mut lexer = Lexer::new("#32 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        // PARSE should return empty string (stopped at first space)
        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        assert_eq!(len, 0, "PARSE should return empty string for leading delimiter");
    }

    #[test]
    fn test_parse_empty_input() {
        // PARSE with empty input should return zero length
        let input_text = "";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        let mut lexer = Lexer::new("#32 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        assert_eq!(len, 0, "PARSE should return 0 for empty input");
    }

    #[test]
    fn test_parse_to_end_of_line() {
        // PARSE until newline (char 10)
        let input_text = "Hello World\nNext Line";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // PARSE with newline delimiter (10)
        let mut lexer = Lexer::new("#10 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        assert_eq!(len, 11, "PARSE should parse until newline");

        let parsed_bytes = &executor.vm().memory[addr..addr + len];
        let parsed = String::from_utf8_lossy(parsed_bytes);
        assert_eq!(parsed, "Hello World");
    }

    #[test]
    fn test_parse_advances_to_in() {
        // Test that PARSE advances >IN correctly
        let input_text = "ABC DEF";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // First PARSE
        let mut lexer = Lexer::new("#32 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len1 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr1 = executor.vm_mut().data_stack.pop().unwrap() as usize;

        // Check first parse
        let parsed1 = String::from_utf8_lossy(&executor.vm().memory[addr1..addr1 + len1]);
        assert_eq!(parsed1, "ABC");

        // Second PARSE
        let mut lexer2 = Lexer::new("#32 PARSE");
        let tokens2 = lexer2.tokenize().unwrap();
        let mut parser2 = Parser::new(tokens2);
        let program2 = parser2.parse().unwrap();
        executor.execute_program(program2).unwrap();

        let len2 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr2 = executor.vm_mut().data_stack.pop().unwrap() as usize;

        // Check second parse
        let parsed2 = String::from_utf8_lossy(&executor.vm().memory[addr2..addr2 + len2]);
        assert_eq!(parsed2, "DEF");
    }

    #[test]
    fn test_parse_no_delimiter_found() {
        // PARSE to end if delimiter not found
        let input_text = "HELLO";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // Parse with comma delimiter (not present)
        let mut lexer = Lexer::new("#44 PARSE");  // 44 = comma
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        assert_eq!(len, 5, "PARSE should parse entire input if delimiter not found");

        let parsed_bytes = &executor.vm().memory[addr..addr + len];
        let parsed = String::from_utf8_lossy(parsed_bytes);
        assert_eq!(parsed, "HELLO");
    }

    #[test]
    fn test_parse_with_tab_delimiter() {
        // Test PARSE with tab character (9)
        let input_text = "FIRST\tSECOND";
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // Parse with tab delimiter
        let mut lexer = Lexer::new("#9 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr = executor.vm_mut().data_stack.pop().unwrap() as usize;

        assert_eq!(len, 5, "PARSE should stop at tab");

        let parsed_bytes = &executor.vm().memory[addr..addr + len];
        let parsed = String::from_utf8_lossy(parsed_bytes);
        assert_eq!(parsed, "FIRST");
    }

    #[test]
    fn test_parse_consecutive_delimiters() {
        // Test multiple consecutive delimiters
        let input_text = "A  B";  // Two spaces
        let mut executor = Executor::new();
        executor.vm_mut().set_input(input_text);

        // First PARSE
        let mut lexer = Lexer::new("#32 PARSE");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        executor.execute_program(program).unwrap();

        let len1 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr1 = executor.vm_mut().data_stack.pop().unwrap() as usize;

        let parsed1 = String::from_utf8_lossy(&executor.vm().memory[addr1..addr1 + len1]);
        assert_eq!(parsed1, "A");

        // Second PARSE - should get empty string (one space)
        let mut lexer2 = Lexer::new("#32 PARSE");
        let tokens2 = lexer2.tokenize().unwrap();
        let mut parser2 = Parser::new(tokens2);
        let program2 = parser2.parse().unwrap();
        executor.execute_program(program2).unwrap();

        let len2 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        assert_eq!(len2, 0, "PARSE should return empty for leading delimiter");

        // Third PARSE - should get "B"
        let mut lexer3 = Lexer::new("#32 PARSE");
        let tokens3 = lexer3.tokenize().unwrap();
        let mut parser3 = Parser::new(tokens3);
        let program3 = parser3.parse().unwrap();
        executor.execute_program(program3).unwrap();

        let len3 = executor.vm_mut().data_stack.pop().unwrap() as usize;
        let addr3 = executor.vm_mut().data_stack.pop().unwrap() as usize;

        let parsed3 = String::from_utf8_lossy(&executor.vm().memory[addr3..addr3 + len3]);
        assert_eq!(parsed3, "B");
    }
