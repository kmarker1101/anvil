use forth::lexer::{Lexer, LexerError, Token};

#[test]
fn test_simple_tokens() {
    let mut lexer = Lexer::new(": ;");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![Token::Colon, Token::Semicolon]);
}

#[test]
fn test_numbers() {
    let mut lexer = Lexer::new("42 -17 0");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(42),
        Token::Number(-17),
        Token::Number(0),
    ]);
}

#[test]
fn test_hex_numbers() {
    let mut lexer = Lexer::new("0xFF 0x10 0xDEADBEEF");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(255),
        Token::Number(16),
        Token::Number(0xDEADBEEF),
    ]);
}

#[test]
fn test_words() {
    let mut lexer = Lexer::new("DUP SWAP OVER");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Word("DUP".to_string()),
        Token::Word("SWAP".to_string()),
        Token::Word("OVER".to_string()),
    ]);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("IF THEN ELSE BEGIN UNTIL");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::If,
        Token::Then,
        Token::Else,
        Token::Begin,
        Token::Until,
    ]);
}

#[test]
fn test_case_insensitive() {
    let mut lexer = Lexer::new("if IF iF If");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::If,
        Token::If,
        Token::If,
        Token::If,
    ]);
}

#[test]
fn test_string() {
    let mut lexer = Lexer::new(r#"" hello world ""#);
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::String(" hello world ".to_string()),
    ]);
}

#[test]
fn test_colon_definition() {
    let mut lexer = Lexer::new(": SQUARE DUP * ;");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Colon,
        Token::Word("SQUARE".to_string()),
        Token::Word("DUP".to_string()),
        Token::Word("*".to_string()),
        Token::Semicolon,
    ]);
}

#[test]
fn test_if_then() {
    let mut lexer = Lexer::new(": ABS DUP 0 < IF NEGATE THEN ;");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Colon,
        Token::Word("ABS".to_string()),
        Token::Word("DUP".to_string()),
        Token::Number(0),
        Token::Word("<".to_string()),
        Token::If,
        Token::Word("NEGATE".to_string()),
        Token::Then,
        Token::Semicolon,
    ]);
}

#[test]
fn test_paren_comment() {
    let mut lexer = Lexer::new("42 ( this is a comment ) 99");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(42),
        Token::Number(99),
    ]);
}

#[test]
fn test_nested_paren_comment() {
    let mut lexer = Lexer::new("1 ( outer ( inner ) outer ) 2");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(1),
        Token::Number(2),
    ]);
}

#[test]
fn test_line_comment() {
    let mut lexer = Lexer::new("42 \\ this is a comment\n99");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(42),
        Token::Number(99),
    ]);
}

#[test]
fn test_complex_program() {
    let input = r#"
        : FACTORIAL ( n -- n! )
          DUP 1 <= IF
            DROP 1
          ELSE
            DUP 1 - FACTORIAL *
          THEN ;
    "#;
    
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    
    assert_eq!(tokens[0], Token::Colon);
    assert_eq!(tokens[1], Token::Word("FACTORIAL".to_string()));
    assert_eq!(tokens[2], Token::Word("DUP".to_string()));
    assert!(tokens.contains(&Token::If));
    assert!(tokens.contains(&Token::Else));
    assert!(tokens.contains(&Token::Then));
    assert_eq!(tokens.last(), Some(&Token::Semicolon));
}

#[test]
fn test_begin_until() {
    let mut lexer = Lexer::new("BEGIN DUP 0> WHILE DUP . 1- REPEAT");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Begin,
        Token::Word("DUP".to_string()),
        Token::Word("0>".to_string()),  // Now parsed as a word, not number + symbol
        Token::While,
        Token::Word("DUP".to_string()),
        Token::Word(".".to_string()),
        Token::Word("1-".to_string()),  // Now parsed as a word, not number + symbol
        Token::Repeat,
    ]);
}

#[test]
fn test_do_loop() {
    let mut lexer = Lexer::new("10 0 DO I . LOOP");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(10),
        Token::Number(0),
        Token::Do,
        Token::Word("I".to_string()),
        Token::Word(".".to_string()),
        Token::Loop,
    ]);
}

#[test]
fn test_qdo_loop() {
    let mut lexer = Lexer::new("10 0 ?DO I . LOOP");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(10),
        Token::Number(0),
        Token::QDo,
        Token::Word("I".to_string()),
        Token::Word(".".to_string()),
        Token::Loop,
    ]);
}

#[test]
fn test_qdo_plusloop() {
    let mut lexer = Lexer::new("20 0 ?DO I . 2 +LOOP");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(20),
        Token::Number(0),
        Token::QDo,
        Token::Word("I".to_string()),
        Token::Word(".".to_string()),
        Token::Number(2),
        Token::PlusLoop,
    ]);
}

#[test]
fn test_whitespace_handling() {
    let mut lexer = Lexer::new("  :  SQUARE   DUP  *  ;  ");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Colon,
        Token::Word("SQUARE".to_string()),
        Token::Word("DUP".to_string()),
        Token::Word("*".to_string()),
        Token::Semicolon,
    ]);
}

#[test]
fn test_empty_input() {
    let mut lexer = Lexer::new("");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_whitespace_only() {
    let mut lexer = Lexer::new("   \n\t  ");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_unterminated_string() {
    let mut lexer = Lexer::new(r#"" hello"#);
    let result = lexer.tokenize();
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), LexerError::UnterminatedString);
}

#[test]
fn test_unterminated_comment() {
    let mut lexer = Lexer::new("( this comment never ends");
    let result = lexer.tokenize();
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), LexerError::UnterminatedComment);
}

#[test]
fn test_word_names_with_digits() {
    // Test words that start with digits followed by symbols
    let mut lexer = Lexer::new("1+ 1- 2* 2DUP");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Word("1+".to_string()),
        Token::Word("1-".to_string()),
        Token::Word("2*".to_string()),
        Token::Word("2DUP".to_string()),
    ]);
}

#[test]
fn test_pure_numbers_still_work() {
    // Make sure regular numbers still parse correctly
    let mut lexer = Lexer::new("1 2 42 -17");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Number(1),
        Token::Number(2),
        Token::Number(42),
        Token::Number(-17),
    ]);
}

#[test]
fn test_mixed_numbers_and_digit_words() {
    let mut lexer = Lexer::new(": INCR 1 + ; 42 1+");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Colon,
        Token::Word("INCR".to_string()),
        Token::Number(1),
        Token::Word("+".to_string()),
        Token::Semicolon,
        Token::Number(42),
        Token::Word("1+".to_string()),
    ]);
}

#[test]
fn test_zero_comparison_words() {
    // Test that ": 0< 0 < ;" tokenizes correctly (0< as word name, 0 and < as separate tokens in body)
    let mut lexer = Lexer::new(": 0< 0 < ;");
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens, vec![
        Token::Colon,
        Token::Word("0<".to_string()),  // Word name can be 0<
        Token::Number(0),                // But in the body, 0 and < are separate
        Token::Word("<".to_string()),
        Token::Semicolon,
    ]);
}
