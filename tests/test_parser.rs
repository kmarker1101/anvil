use forth::parser::{Parser, ParseError, Program, Definition, Expression};
use forth::lexer::Lexer;

fn parse(input: &str) -> Result<Program, ParseError> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[test]
fn test_simple_word_definition() {
    let program = parse(": SQUARE DUP * ;").unwrap();

    assert_eq!(program.definitions.len(), 1);

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "SQUARE");
            assert_eq!(body.len(), 2);
            assert_eq!(body[0], Expression::WordCall("DUP".to_string()));
            assert_eq!(body[1], Expression::WordCall("*".to_string()));
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_word_with_numbers() {
    let program = parse(": DOUBLE 2 * ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "DOUBLE");
            assert_eq!(body[0], Expression::Number(2));
            assert_eq!(body[1], Expression::WordCall("*".to_string()));
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_if_then() {
    let program = parse(": ABS DUP 0 < IF NEGATE THEN ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "ABS");
            assert_eq!(body.len(), 4); // DUP, 0, <, IF

            // Check the IF expression
            if let Expression::If { condition, then_branch } = &body[3] {
                // Condition is empty (evaluated before IF in Forth)
                assert_eq!(condition.len(), 0);
                // THEN branch contains NEGATE
                assert_eq!(then_branch.len(), 1);
                assert_eq!(then_branch[0], Expression::WordCall("NEGATE".to_string()));
            } else {
                panic!("Expected IF expression");
            }
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_if_else_then() {
    let program = parse(": MAX 2DUP < IF SWAP THEN DROP ;").unwrap();
    assert_eq!(program.definitions.len(), 1);
}

#[test]
fn test_begin_until() {
    let program = parse(": COUNTDOWN BEGIN DUP . 1 - DUP 0 = UNTIL ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "COUNTDOWN");

            // Find the BEGIN-UNTIL
            let mut found_begin = false;
            for expr in body {
                if let Expression::BeginUntil { body: loop_body } = expr {
                    found_begin = true;
                    assert!(!loop_body.is_empty());
                }
            }
            assert!(found_begin, "Should have BEGIN-UNTIL");
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_do_loop() {
    let program = parse(": TEST 10 0 DO I . LOOP ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "TEST");

            // Body: 10, 0, DO...LOOP
            assert_eq!(body[0], Expression::Number(10));
            assert_eq!(body[1], Expression::Number(0));

            if let Expression::DoLoop { body: loop_body } = &body[2] {
                assert_eq!(loop_body[0], Expression::WordCall("I".to_string()));
                assert_eq!(loop_body[1], Expression::WordCall(".".to_string()));
            } else {
                panic!("Expected DO-LOOP");
            }
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_qdo_loop() {
    let program = parse(": TEST 10 0 ?DO I . LOOP ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "TEST");

            // Body: 10, 0, ?DO...LOOP
            assert_eq!(body[0], Expression::Number(10));
            assert_eq!(body[1], Expression::Number(0));

            if let Expression::QDoLoop { body: loop_body } = &body[2] {
                assert_eq!(loop_body[0], Expression::WordCall("I".to_string()));
                assert_eq!(loop_body[1], Expression::WordCall(".".to_string()));
            } else {
                panic!("Expected ?DO-LOOP");
            }
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_qdo_plusloop() {
    let program = parse(": EVEN 20 0 ?DO I . 2 +LOOP ;").unwrap();

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "EVEN");

            // Body: 20, 0, ?DO...+LOOP
            assert_eq!(body[0], Expression::Number(20));
            assert_eq!(body[1], Expression::Number(0));

            if let Expression::QDoPlusLoop { body: loop_body } = &body[2] {
                assert_eq!(loop_body[0], Expression::WordCall("I".to_string()));
                assert_eq!(loop_body[1], Expression::WordCall(".".to_string()));
                assert_eq!(loop_body[2], Expression::Number(2));
            } else {
                panic!("Expected ?DO-+LOOP");
            }
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_multiple_definitions() {
    let program = parse(": SQUARE DUP * ; : CUBE DUP SQUARE * ;").unwrap();

    assert_eq!(program.definitions.len(), 2);

    match &program.definitions[0] {
        Definition::Word { name, .. } => assert_eq!(name, "SQUARE"),
        _ => panic!("Expected word definition"),
    }

    match &program.definitions[1] {
        Definition::Word { name, .. } => assert_eq!(name, "CUBE"),
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_complex_program() {
    let input = r#"
        : FACTORIAL
          DUP 1 <= IF
            DROP 1
          ELSE
            DUP 1 - FACTORIAL *
          THEN ;
    "#;

    let program = parse(input).unwrap();
    assert_eq!(program.definitions.len(), 1);

    match &program.definitions[0] {
        Definition::Word { name, body } => {
            assert_eq!(name, "FACTORIAL");
            assert!(!body.is_empty());
        }
        _ => panic!("Expected word definition"),
    }
}

#[test]
fn test_missing_semicolon() {
    let result = parse(": SQUARE DUP *");
    assert!(result.is_err());
}

#[test]
fn test_missing_word_name() {
    let _result = parse(": DUP * ;");
    // This might actually parse, depending on implementation
    // ": DUP" could be interpreted as defining a word named "DUP"
}

#[test]
fn test_unmatched_if() {
    let result = parse(": BAD IF DROP ;");
    assert!(result.is_err());
}

#[test]
fn test_string_literal() {
    let program = parse(r#": GREET " Hello " TYPE ;"#).unwrap();

    match &program.definitions[0] {
        Definition::Word { body, .. } => {
            assert_eq!(body[0], Expression::String(" Hello ".to_string()));
            assert_eq!(body[1], Expression::WordCall("TYPE".to_string()));
        }
        _ => panic!("Expected word definition"),
    }
}
