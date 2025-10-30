# Parser and AST Documentation

## Overview

Anvil uses a hand-written two-stage parser:
1. **Tokenization** - Source text → Tokens
2. **AST Building** - Tokens → Abstract Syntax Tree

No external parser libraries are used. Forth's simplicity (whitespace-delimited tokens, no operator precedence) makes this straightforward.

## Stage 1: Tokenization

**Location**: `src/parser.h`, `src/parser.cpp`

### Token Types

```cpp
enum class TokenType {
    WORD,      // Dictionary word (DUP, SWAP, user-defined words)
    NUMBER,    // 64-bit integer literal
    STRING,    // String literal (for S" and .")
    COMMENT,   // Line (\) or block ((..)) comments
    END        // End of input
};

struct Token {
    TokenType type;
    std::string value;  // Word name, number string, or string content
    int64_t number_value; // Parsed number (for NUMBER tokens)
};
```

### Number Format Support

```forth
42          \ Decimal
-123        \ Negative decimal
0xFF        \ Hexadecimal (C-style)
$FF         \ Hexadecimal (Forth-style with $)
0b1010      \ Binary (C-style)
%1111       \ Binary (Forth-style with %)
```

All numbers are parsed to `int64_t` (64-bit signed integers).

### Comment Handling

```forth
\ This is a line comment to end of line

( This is a block comment )
( Nested ( comments ) are supported )
```

Comments are filtered during tokenization and do not appear in the token stream.

### Parser API

```cpp
class Parser {
public:
    // Tokenize entire source string
    std::vector<Token> tokenize(const std::string& source);
};
```

### Example

```cpp
Parser parser;
auto tokens = parser.tokenize("42 DUP + \\ add to itself");

// Result:
// [
//   Token{NUMBER, "42", 42},
//   Token{WORD, "DUP", 0},
//   Token{WORD, "+", 0}
// ]
// (comment is filtered out)
```

## Stage 2: AST Building

**Location**: `src/ast.h`, `src/ast.cpp`

### AST Node Types

```cpp
enum class ASTNodeType {
    LITERAL,              // Number literal: 42
    WORD_CALL,            // Dictionary word: DUP, +, custom words
    SEQUENCE,             // Sequential execution: A B C
    DEFINITION,           // Word definition: : NAME ... ;
    IF_THEN,              // Conditional: IF ... THEN
    IF_ELSE_THEN,         // Conditional with else: IF ... ELSE ... THEN
    BEGIN_UNTIL,          // Loop: BEGIN ... UNTIL
    BEGIN_WHILE_REPEAT,   // Loop: BEGIN ... WHILE ... REPEAT
    DO_LOOP,              // Counted loop: DO ... LOOP or DO ... +LOOP
    STRING_LITERAL        // String literal: S" hello"
};
```

### AST Node Structure

Each node type has specific data:

```cpp
struct LiteralNode : ASTNode {
    int64_t value;  // The literal value
};

struct WordCallNode : ASTNode {
    std::string word_name;  // Name of word to call
};

struct DefinitionNode : ASTNode {
    std::string name;             // Word being defined
    std::unique_ptr<ASTNode> body; // Body of definition
};

struct IfElseThenNode : ASTNode {
    std::unique_ptr<ASTNode> then_branch;
    std::unique_ptr<ASTNode> else_branch;  // nullptr if no ELSE
};

struct DoLoopNode : ASTNode {
    std::unique_ptr<ASTNode> body;
    bool is_plus_loop;  // true for +LOOP, false for LOOP
};

// ... etc
```

### AST Builder API

```cpp
class ASTBuilder {
public:
    // Parse source string into AST
    std::unique_ptr<ASTNode> parse(const std::string& source);
};
```

### Parsing Strategy

Recursive descent parser with methods for each construct:

```cpp
class ASTBuilder {
private:
    std::unique_ptr<ASTNode> parse_expression();
    std::unique_ptr<ASTNode> parse_definition();
    std::unique_ptr<ASTNode> parse_if();
    std::unique_ptr<ASTNode> parse_begin();
    std::unique_ptr<ASTNode> parse_do_loop();
    std::unique_ptr<ASTNode> parse_sequence();
};
```

### Example AST Construction

**Source:**
```forth
: SQUARE DUP * ;
```

**AST:**
```
DefinitionNode("SQUARE")
  └─ SequenceNode
      ├─ WordCallNode("DUP")
      └─ WordCallNode("*")
```

**Source:**
```forth
: ABS DUP 0 < IF -1 * THEN ;
```

**AST:**
```
DefinitionNode("ABS")
  └─ SequenceNode
      ├─ WordCallNode("DUP")
      ├─ LiteralNode(0)
      ├─ WordCallNode("<")
      └─ IfThenNode
          └─ then_branch: SequenceNode
              ├─ LiteralNode(-1)
              └─ WordCallNode("*")
```

## Case Insensitivity

### Keywords
Control structure keywords are case-insensitive:
```forth
IF    \ recognized
if    \ recognized
If    \ recognized
```

### Word Names
Word names preserve their original case but are looked up case-insensitively in the dictionary:
```forth
: square dup * ;
5 SQUARE .   \ Works - "SQUARE" finds "square"
5 Square .   \ Works - "Square" finds "square"
```

## Error Handling

Parser provides detailed error messages:

```forth
: BROKEN DUP + IF ;  \ Missing THEN
```

Error:
```
Parse error: IF without matching THEN at token 5
```

## Parser State Management

The parser maintains minimal state:
- Current token position
- Token stream
- Nesting depth for comments and control structures

Each call to `parse()` is stateless - no state persists between calls.

## Performance Characteristics

- **Tokenization**: O(n) where n is source length
- **AST Building**: O(t) where t is number of tokens
- **Memory**: Single-pass, minimal allocation
- **Speed**: ~1-2ms for typical definitions on modern hardware

## Integration with Compiler

The AST is passed directly to the compiler:

```cpp
// User types: : DOUBLE DUP + ;

// 1. Parse
ASTBuilder builder;
auto ast = builder.parse(": DOUBLE DUP + ;");

// 2. Compile to IR
Compiler compiler(llvm_ctx, module);
llvm::Function* func = compiler.compile(ast.get());

// 3. Add to dictionary
global_dictionary.add("DOUBLE", new Word(func));
```

## See Also

- [Compiler Documentation](compiler.md) - How AST is converted to LLVM IR
- [Architecture](architecture.md) - Overall system design
