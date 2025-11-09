// lexer.rs - Forth Lexer/Tokenizer

use std::fmt;

// ============================================================================
// TOKEN TYPES
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Special symbols
    Colon,     // :
    Semicolon, // ;
    Variable,  // VARIABLE

    // Control flow
    If,       // IF
    Then,     // THEN
    Else,     // ELSE
    Begin,    // BEGIN
    Until,    // UNTIL
    While,    // WHILE
    Repeat,   // REPEAT
    Do,       // DO
    QDo,      // ?DO
    Loop,     // LOOP
    PlusLoop, // +LOOP
    Recurse,  // RECURSE
    Exit,     // EXIT
    Bye,      // BYE

    // Literals
    Number(i64),      // 42, -17, 0xFF
    String(String),   // S" hello world"
    DotQuote(String), // ." hello world"

    // Words (identifiers)
    Word(String), // DUP, SWAP, MYWORD, etc.

                  // Comments are skipped, not tokenized
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::Variable => write!(f, "VARIABLE"),
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
            Token::Bye => write!(f, "BYE"),
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::DotQuote(s) => write!(f, ".\"{}\"", s),
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

        // Check for ." specifically (dot-quote)
        if self.peek() == Some('.') {
            let saved_pos = self.position;
            self.advance();
            if self.peek() == Some('"') {
                // This is ." - parse the string as DotQuote
                self.advance(); // consume the "
                return self.parse_dot_quote();
            }
            // Not .", restore position and continue
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
            "VARIABLE" => Token::Variable,
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
            "BYE" => Token::Bye,
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
        let value = num_str
            .parse::<i64>()
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
        let value = i64::from_str_radix(&hex_str, 16).map_err(|_| LexerError::InvalidNumber)?;

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

    fn parse_dot_quote(&mut self) -> Result<Token, LexerError> {
        // Opening " is already consumed by parse_word
        let start = self.position;

        while let Some(ch) = self.peek() {
            if ch == '"' {
                let s: String = self.input[start..self.position].iter().collect();
                self.advance(); // consume closing "
                return Ok(Token::DotQuote(s));
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
