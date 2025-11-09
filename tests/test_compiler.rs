use forth::compiler::Executor;
use forth::lexer::Lexer;
use forth::parser::Parser;

fn compile_and_run(input: &str) -> Result<Executor, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    let mut executor = Executor::new();
    executor.execute_program(program)?;

    Ok(executor)
}

fn compile_and_run_with_stdlib(input: &str) -> Result<Executor, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(|e| e.to_string())?;

    let mut executor = Executor::with_stdlib()?;
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
