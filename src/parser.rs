// parser.rs - Forth Parser (Tokens → AST)

use crate::lexer::Token;

// ============================================================================
// AST (Abstract Syntax Tree) - Represents Forth program structure
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    /// : SQUARE DUP * ;
    Word { name: String, body: Vec<Expression> },

    /// VARIABLE name
    Variable { name: String },

    /// CONSTANT name
    Constant { name: String, value: i64 },

    /// Just a top-level expression (for REPL mode)
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal number: 42
    Number(i64),

    /// String literal: S" hello "
    String(String),

    /// Print string literal: ." hello "
    DotQuote(String),

    /// Word call: DUP, SWAP, MYWORD, +, *, etc.
    WordCall(String),

    /// IF condition THEN
    If {
        condition: Vec<Expression>,
        then_branch: Vec<Expression>,
    },

    /// IF condition THEN else-branch ELSE
    IfElse {
        condition: Vec<Expression>,
        then_branch: Vec<Expression>,
        else_branch: Vec<Expression>,
    },

    /// BEGIN body UNTIL
    BeginUntil { body: Vec<Expression> },

    /// BEGIN condition WHILE body REPEAT
    BeginWhileRepeat {
        condition: Vec<Expression>,
        body: Vec<Expression>,
    },

    /// DO body LOOP
    DoLoop { body: Vec<Expression> },

    /// DO body +LOOP
    DoPlusLoop { body: Vec<Expression> },

    /// ?DO body LOOP
    QDoLoop { body: Vec<Expression> },

    /// ?DO body +LOOP
    QDoPlusLoop { body: Vec<Expression> },

    /// RECURSE - recursive call to current word being defined
    Recurse,

    /// EXIT - early return from current word
    Exit,

    /// BYE - exit the program
    Bye,
}

// ============================================================================
// PARSER
// ============================================================================

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    UnexpectedToken { expected: String, found: Token },
    MissingWordName,
    UnmatchedIf,
    UnmatchedBegin,
    UnmatchedDo,
    InvalidControlFlow(String),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedEof => write!(f, "Unexpected end of input"),
            ParseError::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found {}", expected, found)
            }
            ParseError::MissingWordName => write!(f, "Missing word name after ':'"),
            ParseError::UnmatchedIf => write!(f, "IF without matching THEN"),
            ParseError::UnmatchedBegin => write!(f, "BEGIN without matching UNTIL/REPEAT"),
            ParseError::UnmatchedDo => write!(f, "DO without matching LOOP"),
            ParseError::InvalidControlFlow(msg) => write!(f, "Invalid control flow: {}", msg),
        }
    }
}

impl std::error::Error for ParseError {}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    /// Parse entire program
    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut definitions = Vec::new();

        while !self.is_at_end() {
            let def = self.parse_definition()?;
            definitions.push(def);
        }

        Ok(Program { definitions })
    }

    /// Parse a single definition or expression
    fn parse_definition(&mut self) -> Result<Definition, ParseError> {
        if self.check(&Token::Colon) {
            self.parse_word_definition()
        } else if self.check(&Token::Variable) {
            self.parse_variable_definition()
        } else if self.check(&Token::Constant) {
            self.parse_constant_definition()
        } else {
            // Top-level expression (for REPL)
            let expr = self.parse_expression()?;
            Ok(Definition::Expression(expr))
        }
    }

    /// Parse variable definition: VARIABLE NAME
    fn parse_variable_definition(&mut self) -> Result<Definition, ParseError> {
        self.advance(); // consume VARIABLE

        // Get variable name
        let name = match self.peek() {
            Some(Token::Word(w)) => {
                let n = w.clone();
                self.advance();
                n
            }
            Some(token) => {
                return Err(ParseError::UnexpectedToken {
                    expected: "variable name".to_string(),
                    found: token.clone(),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok(Definition::Variable { name })
    }

    /// Parse constant definition: n CONSTANT NAME
    fn parse_constant_definition(&mut self) -> Result<Definition, ParseError> {
        self.advance(); // consume CONSTANT

        // Get constant name
        let name = match self.peek() {
            Some(Token::Word(w)) => {
                let n = w.clone();
                self.advance();
                n
            }
            Some(token) => {
                return Err(ParseError::UnexpectedToken {
                    expected: "constant name".to_string(),
                    found: token.clone(),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Note: In Forth, CONSTANT expects the value on the stack at runtime
        // We'll handle this in the compiler by popping the value
        Ok(Definition::Constant { name, value: 0 })
    }

    /// Parse word definition: : NAME body ;
    fn parse_word_definition(&mut self) -> Result<Definition, ParseError> {
        self.consume_colon()?;

        // Get word name
        let name = match self.peek() {
            Some(Token::Word(w)) => {
                let n = w.clone();
                self.advance();
                n
            }
            Some(token) => {
                return Err(ParseError::UnexpectedToken {
                    expected: "word name".to_string(),
                    found: token.clone(),
                });
            }
            None => return Err(ParseError::MissingWordName),
        };

        // Parse body until ;
        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(&Token::Semicolon) {
            let expr = self.parse_expression()?;
            body.push(expr);
        }

        self.consume_semicolon()?;

        Ok(Definition::Word { name, body })
    }

    /// Parse a single expression
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        match self.peek() {
            Some(Token::Number(n)) => {
                let num = *n;
                self.advance();
                Ok(Expression::Number(num))
            }

            Some(Token::String(s)) => {
                let string = s.clone();
                self.advance();
                Ok(Expression::String(string))
            }

            Some(Token::DotQuote(s)) => {
                let string = s.clone();
                self.advance();
                Ok(Expression::DotQuote(string))
            }

            Some(Token::If) => self.parse_if(),

            Some(Token::Begin) => self.parse_begin(),

            Some(Token::Do) => self.parse_do(),
            Some(Token::QDo) => self.parse_qdo(),

            Some(Token::Recurse) => {
                self.advance();
                Ok(Expression::Recurse)
            }

            Some(Token::Exit) => {
                self.advance();
                Ok(Expression::Exit)
            }

            Some(Token::Bye) => {
                self.advance();
                Ok(Expression::Bye)
            }

            Some(Token::Word(w)) => {
                let word = w.clone();
                self.advance();
                Ok(Expression::WordCall(word))
            }

            Some(token) => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: token.clone(),
            }),

            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Parse IF-THEN or IF-ELSE-THEN
    fn parse_if(&mut self) -> Result<Expression, ParseError> {
        self.advance(); // consume IF

        // In Forth, IF is postfix: the condition is already evaluated and on the stack
        // So we parse: condition-code IF then-code THEN
        // or: condition-code IF then-code ELSE else-code THEN

        // Parse THEN branch (everything between IF and ELSE/THEN)
        let mut then_branch = Vec::new();
        while !self.is_at_end() && !self.check(&Token::Then) && !self.check(&Token::Else) {
            let expr = self.parse_expression()?;
            then_branch.push(expr);
        }

        // Check for ELSE
        if self.check(&Token::Else) {
            self.advance(); // consume ELSE

            // Parse else branch
            let mut else_branch = Vec::new();
            while !self.is_at_end() && !self.check(&Token::Then) {
                let expr = self.parse_expression()?;
                else_branch.push(expr);
            }

            if !self.check(&Token::Then) {
                return Err(ParseError::UnmatchedIf);
            }
            self.advance(); // consume THEN

            Ok(Expression::IfElse {
                condition: Vec::new(), // Condition is evaluated before IF
                then_branch,
                else_branch,
            })
        } else {
            // Simple IF-THEN (no ELSE)
            if !self.check(&Token::Then) {
                return Err(ParseError::UnmatchedIf);
            }
            self.advance(); // consume THEN

            Ok(Expression::If {
                condition: Vec::new(), // Condition is evaluated before IF
                then_branch,
            })
        }
    }

    /// Parse BEGIN-UNTIL or BEGIN-WHILE-REPEAT
    fn parse_begin(&mut self) -> Result<Expression, ParseError> {
        self.advance(); // consume BEGIN

        let mut body = Vec::new();

        // Parse until we hit UNTIL, WHILE, or REPEAT
        while !self.is_at_end() {
            if self.check(&Token::Until) {
                self.advance(); // consume UNTIL
                return Ok(Expression::BeginUntil { body });
            }

            if self.check(&Token::While) {
                self.advance(); // consume WHILE

                // body so far was the condition
                let condition = body;
                let mut while_body = Vec::new();

                // Parse body until REPEAT
                while !self.is_at_end() && !self.check(&Token::Repeat) {
                    let expr = self.parse_expression()?;
                    while_body.push(expr);
                }

                if !self.check(&Token::Repeat) {
                    return Err(ParseError::UnmatchedBegin);
                }
                self.advance(); // consume REPEAT

                return Ok(Expression::BeginWhileRepeat {
                    condition,
                    body: while_body,
                });
            }

            let expr = self.parse_expression()?;
            body.push(expr);
        }

        Err(ParseError::UnmatchedBegin)
    }

    /// Parse DO-LOOP or DO-+LOOP
    fn parse_do(&mut self) -> Result<Expression, ParseError> {
        self.advance(); // consume DO

        let mut body = Vec::new();

        while !self.is_at_end() && !self.check(&Token::Loop) && !self.check(&Token::PlusLoop) {
            let expr = self.parse_expression()?;
            body.push(expr);
        }

        if self.check(&Token::Loop) {
            self.advance();
            Ok(Expression::DoLoop { body })
        } else if self.check(&Token::PlusLoop) {
            self.advance();
            Ok(Expression::DoPlusLoop { body })
        } else {
            Err(ParseError::UnmatchedDo)
        }
    }

    /// Parse ?DO-LOOP or ?DO-+LOOP (same as DO but skips if start=end)
    fn parse_qdo(&mut self) -> Result<Expression, ParseError> {
        self.advance(); // consume ?DO

        let mut body = Vec::new();

        while !self.is_at_end() && !self.check(&Token::Loop) && !self.check(&Token::PlusLoop) {
            let expr = self.parse_expression()?;
            body.push(expr);
        }

        if self.check(&Token::Loop) {
            self.advance();
            Ok(Expression::QDoLoop { body })
        } else if self.check(&Token::PlusLoop) {
            self.advance();
            Ok(Expression::QDoPlusLoop { body })
        } else {
            Err(ParseError::UnmatchedDo)
        }
    }

    // ========================================================================
    // HELPER METHODS
    // ========================================================================

    fn peek(&self) -> Option<&Token> {
        if self.position < self.tokens.len() {
            Some(&self.tokens[self.position])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() {
            self.position += 1;
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    fn check(&self, token_type: &Token) -> bool {
        if let Some(token) = self.peek() {
            std::mem::discriminant(token) == std::mem::discriminant(token_type)
        } else {
            false
        }
    }

    fn consume_colon(&mut self) -> Result<(), ParseError> {
        if self.check(&Token::Colon) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: ":".to_string(),
                found: self
                    .peek()
                    .cloned()
                    .unwrap_or(Token::Word("EOF".to_string())),
            })
        }
    }

    fn consume_semicolon(&mut self) -> Result<(), ParseError> {
        if self.check(&Token::Semicolon) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: ";".to_string(),
                found: self
                    .peek()
                    .cloned()
                    .unwrap_or(Token::Word("EOF".to_string())),
            })
        }
    }
}
