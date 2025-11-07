// lexer.rs - Forth Lexer/Tokenizer

use std::fmt;

// ============================================================================
// TOKEN TYPES
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Special symbols
    Colon,              // :
    Semicolon,          // ;
    
    // Control flow
    If,                 // IF
    Then,               // THEN
    Else,               // ELSE
    Begin,              // BEGIN
    Until,              // UNTIL
    While,              // WHILE
    Repeat,             // REPEAT
    Do,                 // DO
    QDo,                // ?DO
    Loop,               // LOOP
    PlusLoop,           // +LOOP
    Recurse,            // RECURSE
    Exit,               // EXIT
    
    // Literals
    Number(i64),        // 42, -17, 0xFF
    String(String),     // " hello world"
    
    // Words (identifiers)
    Word(String),       // DUP, SWAP, MYWORD, etc.
    
    // Comments are skipped, not tokenized
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::If => write!(f, "IF"),
            Token::Then => write!(f, "THEN"),
            Token::Else => write!(f, "ELSE"),
            Token::Begin => write!(f, "BEGIN"),
            Token::Until => write!(f, "UNTIL"),
            Token::While => write!(f, "WHILE"),
            Token::Repeat => write!(f, "REPEAT"),
            Token::Do => write!(f, "DO"),
            Token::QDo => write!(f, "?DO"),
            Token::Loop => write!(f, "LOOP"),
            Token::PlusLoop => write!(f, "+LOOP"),
            Token::Recurse => write!(f, "RECURSE"),
            Token::Exit => write!(f, "EXIT"),
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Word(w) => write!(f, "{}", w),
        }
    }
}

// ============================================================================
// LEXER
// ============================================================================

pub struct Lexer {
    input: Vec<char>,
    position: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            
            if self.is_at_end() {
                break;
            }

            // Skip comments
            if self.peek() == Some('(') {
                self.skip_paren_comment()?;
                continue;
            }
            
            if self.peek() == Some('\\') {
                self.skip_line_comment();
                continue;
            }

            // Parse token
            let token = self.next_token()?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexerError> {
        let ch = self.peek().ok_or(LexerError::UnexpectedEof)?;

        match ch {
            ':' => {
                self.advance();
                Ok(Token::Colon)
            }
            ';' => {
                self.advance();
                Ok(Token::Semicolon)
            }
            '"' => self.parse_string(),
            '-' | '0'..='9' => self.parse_number(),
            _ => self.parse_word(),
        }
    }

    fn parse_word(&mut self) -> Result<Token, LexerError> {
        let start = self.position;

        // Check for S" specifically (string literal)
        if self.peek() == Some('S') {
            let saved_pos = self.position;
            self.advance();
            if self.peek() == Some('"') {
                // This is S" - parse the string
                self.advance(); // consume the "
                return self.parse_string();
            }
            // Not S", restore position and continue
            self.position = saved_pos;
        }

        // Read until whitespace or special character
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() || ch == ':' || ch == ';' || ch == '(' || ch == '"' {
                break;
            }
            self.advance();
        }

        let word: String = self.input[start..self.position].iter().collect();

        if word.is_empty() {
            return Err(LexerError::EmptyWord);
        }

        // Check for keywords (case-insensitive in Forth)
        let upper_word = word.to_uppercase();
        let token = match upper_word.as_str() {
            "IF" => Token::If,
            "THEN" => Token::Then,
            "ELSE" => Token::Else,
            "BEGIN" => Token::Begin,
            "UNTIL" => Token::Until,
            "WHILE" => Token::While,
            "REPEAT" => Token::Repeat,
            "DO" => Token::Do,
            "?DO" => Token::QDo,
            "LOOP" => Token::Loop,
            "+LOOP" => Token::PlusLoop,
            "RECURSE" => Token::Recurse,
            "EXIT" => Token::Exit,
            _ => Token::Word(word),
        };

        Ok(token)
    }

    fn parse_number(&mut self) -> Result<Token, LexerError> {
        let start = self.position;
        
        // Handle negative sign
        if self.peek() == Some('-') {
            self.advance();
        }

        // Check for hex prefix (0x or 0X)
        if self.peek() == Some('0') {
            self.advance();
            if let Some('x') | Some('X') = self.peek() {
                self.advance();
                return self.parse_hex_number();
            }
            // Back up if not hex
            self.position -= 1;
        }

        // Parse decimal digits
        let mut has_digits = false;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                has_digits = true;
                self.advance();
            } else if ch.is_whitespace() || ch == ':' || ch == ';' || ch == '(' {
                break;
            } else {
                // Found a non-digit character (like +, -, *, alphabetic, etc.)
                // This means it's likely a word like "1+", "1-", "2DUP", etc.
                // Back up and parse as word instead
                self.position = start;
                return self.parse_word();
            }
        }

        if !has_digits {
            // No digits found (e.g., just "-" or "0x" without hex digits)
            // Back up and parse as word instead
            self.position = start;
            return self.parse_word();
        }

        let num_str: String = self.input[start..self.position].iter().collect();
        let value = num_str.parse::<i64>()
            .map_err(|_| LexerError::InvalidNumber)?;

        Ok(Token::Number(value))
    }

    fn parse_hex_number(&mut self) -> Result<Token, LexerError> {
        let hex_start = self.position;
        
        while let Some(ch) = self.peek() {
            if ch.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }

        if self.position == hex_start {
            return Err(LexerError::InvalidNumber);
        }

        let hex_str: String = self.input[hex_start..self.position].iter().collect();
        let value = i64::from_str_radix(&hex_str, 16)
            .map_err(|_| LexerError::InvalidNumber)?;

        Ok(Token::Number(value))
    }

    fn parse_string(&mut self) -> Result<Token, LexerError> {
        self.advance(); // consume opening "
        
        let start = self.position;
        
        while let Some(ch) = self.peek() {
            if ch == '"' {
                let s: String = self.input[start..self.position].iter().collect();
                self.advance(); // consume closing "
                return Ok(Token::String(s));
            }
            self.advance();
        }

        Err(LexerError::UnterminatedString)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_paren_comment(&mut self) -> Result<(), LexerError> {
        self.advance(); // consume (
        
        let mut depth = 1;
        while depth > 0 {
            match self.peek() {
                Some('(') => {
                    depth += 1;
                    self.advance();
                }
                Some(')') => {
                    depth -= 1;
                    self.advance();
                }
                Some(_) => {
                    self.advance();
                }
                None => return Err(LexerError::UnterminatedComment),
            }
        }

        Ok(())
    }

    fn skip_line_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn peek(&self) -> Option<char> {
        if self.position < self.input.len() {
            Some(self.input[self.position])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if self.position < self.input.len() {
            self.position += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
}

// ============================================================================
// ERROR TYPES
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedEof,
    InvalidNumber,
    UnterminatedString,
    UnterminatedComment,
    EmptyWord,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexerError::UnexpectedEof => write!(f, "Unexpected end of input"),
            LexerError::InvalidNumber => write!(f, "Invalid number format"),
            LexerError::UnterminatedString => write!(f, "Unterminated string literal"),
            LexerError::UnterminatedComment => write!(f, "Unterminated comment"),
            LexerError::EmptyWord => write!(f, "Empty word"),
        }
    }
}

impl std::error::Error for LexerError {}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

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
}
