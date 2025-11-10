# Forth Lexer Testing

## Overview

The Forth-based lexer (`src/lexer.fth`) has comprehensive tests that verify it produces correct token streams.

## Test Files

### Production Files
- `src/lexer.fth` - The Forth lexer implementation
- `tests/lexer-forth-tests.fth` - Comprehensive test suite with 20 test cases

### Test Infrastructure
- `tests/test_forth_lexer_integration.rs` - Rust integration test that runs the Forth lexer tests

## Running Tests

### Run as Integration Test (Recommended)
```bash
cargo test test_forth_lexer
```

This runs the Forth lexer test suite as a Rust integration test and verifies:
- All 20 tests execute
- No errors occur
- "ALL TESTS COMPLETE" message appears

### Run Manually
```bash
cargo run --bin anvil -- tests/lexer-forth-tests.fth
```

This runs the tests and displays all token output for manual verification.

## Test Coverage

The test suite covers all cases from the Rust lexer tests (`tests/test_lexer.rs`):

1. **Simple tokens** - Colon, semicolon
2. **Numbers** - Positive, negative, zero
3. **Words** - Simple words, keywords
4. **Case handling** - Case variations
5. **Definitions** - Colon definitions, IF/THEN
6. **Comments** - Paren comments, line comments
7. **Control flow** - BEGIN/WHILE/REPEAT, DO/LOOP, ?DO/+LOOP
8. **Whitespace** - Extra whitespace, empty input
9. **Complex programs** - Multi-line definitions
10. **Edge cases** - Zero comparison words, mixed numbers and symbols

## Known Differences from Rust Lexer

The Forth lexer has some intentional differences from the Rust lexer:

### Number-First Parsing
The Forth lexer tries to parse numbers before words, so:
- `1+` → `NUMBER(1) WORD(+)` (Forth) vs `WORD(1+)` (Rust)
- `0>` → `NUMBER(0) WORD(>)` (Forth) vs `WORD(0>)` (Rust)

This is acceptable because the parser will handle these correctly.

### No Keyword Recognition
The Forth lexer treats all keywords as words:
- `IF` → `WORD(IF)` (not `Token::If`)

The parser is responsible for recognizing keywords, keeping the lexer simple.

### Token Buffer Location
The Forth lexer uses address 30000 for the token buffer (not 20000) to avoid memory conflicts with variable storage.

## Test Failure Detection

The integration test will fail if:
- Any Forth error occurs (undefined word, stack underflow, etc.)
- The test file doesn't complete ("ALL TESTS COMPLETE" not found)
- Any test doesn't run

## Adding New Tests

To add a new test case to `tests/lexer-forth-tests.fth`:

```forth
.\" Test N: Description\" CR
S\" input code here\" TEST-LEX
.\" Expected: TOKEN1 TOKEN2 ... EOF\" CR CR
```

The integration test will automatically pick up the new test.

## Implementation Notes

### Token Structure
Each token is 24 bytes:
- Bytes 0-7: Token type (TOK-NUMBER, TOK-WORD, etc.)
- Bytes 8-15: Data (number value or string address)
- Bytes 16-23: Length (for strings, 0 otherwise)

### Token Types
- `TOK-EOF` (0): End of input
- `TOK-COLON` (1): `:` character
- `TOK-SEMICOLON` (2): `;` character
- `TOK-NUMBER` (3): Numeric literal
- `TOK-WORD` (4): Identifier/keyword
- `TOK-STRING` (5): String literal

### Memory Layout
- Token buffer: 30000-30000+(24*n) where n is token count
- Input buffer: Temporary location (managed by caller)
- String data: Inline in source (tokens reference original addresses)
