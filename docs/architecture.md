# Anvil Architecture Overview

## High-Level Architecture

Anvil is a Forth compiler with an LLVM backend, supporting three execution modes from a single intermediate representation:

```
Forth Source → Parser → AST → LLVM IR → [Interpreter | JIT | AOT]
                                            ↓         ↓      ↓
                                        Execute   Execute  Binary
```

## Core Design Principles

### 1. Mode-Agnostic Semantics
All three execution modes work from the same LLVM IR and share identical semantics. This prevents behavior divergence between modes.

### 2. Inline-First Strategy
All primitives emit LLVM IR directly rather than generating function calls. This enables:
- Cross-primitive optimization
- Elimination of calling overhead
- Better dataflow analysis

### 3. Global Dictionary
Single `std::unordered_map<std::string, Word*>` for all word definitions:
- Fast O(1) lookup
- Traditional Forth semantics
- Simple word redefinition

### 4. Two-Stack Model
- **Data Stack**: Parameters, working values, function arguments/results
- **Return Stack**: Temporary storage, loop counters, local state
- **LLVM Call Stack**: Actual function returns (managed automatically)

### 5. 64-Bit Only
Every value is 64 bits (`int64_t`). Stack cells, literals, and all operations use 64-bit values.

## Component Architecture

### Frontend (Parsing)
- **Parser** ([parser.md](parser.md)): Tokenizes Forth source
- **AST Builder** ([parser.md](parser.md)): Builds abstract syntax tree
- **Dictionary**: Word storage and lookup

### Middle (Compilation)
- **Compiler** ([compiler.md](compiler.md)): AST → LLVM IR generation
- **Primitives** ([primitives.md](primitives.md)): IR emission functions for each primitive

### Backend (Execution)
- **Interpreter** ([execution-modes.md](execution-modes.md)): Walks IR directly
- **JIT** ([execution-modes.md](execution-modes.md)): Compiles to memory on demand
- **AOT** ([aot.md](aot.md)): Compiles to standalone executables

### Runtime
- Stack implementation (data + return)
- I/O operations
- Memory management

## Data Flow

### Word Definition (`:  SQUARE DUP * ;`)

```
1. Parser tokenizes: [WORD(":"), WORD("SQUARE"), WORD("DUP"), WORD("*"), WORD(";")]
2. AST Builder creates: DefinitionNode("SQUARE", [WordCall("DUP"), WordCall("*")])
3. Compiler generates LLVM IR for the function
4. Dictionary stores: "SQUARE" → Word { ir_function, native_code=null }
5. On first call (JIT mode): compile IR → native_code, then execute
```

### Word Execution Flow

```
1. User types: SQUARE
2. Dictionary lookup: find Word object
3. Check compilation mode:
   - JIT: Compile on first call if needed, then execute native code
   - Interpreter: Walk IR/AST directly
   - AOT: Already compiled, execute native code
```

## Memory Layout

### Execution Context
```cpp
struct ExecutionContext {
    int64_t data_stack[1024];    // Data stack
    int64_t return_stack[1024];  // Return stack
    size_t dsp;                  // Data stack pointer
    size_t rsp;                  // Return stack pointer
};
```

### Word Structure
```cpp
struct Word {
    std::string name;
    llvm::Function* ir_function;  // LLVM IR
    void* native_code;            // JIT-compiled code (null until compiled)
    WordFlags flags;              // Metadata
    bool is_primitive;            // True for C++-implemented primitives
};
```

## File Organization

```
src/
├── main.cpp              # REPL and CLI
├── parser.h/cpp          # Tokenization
├── ast.h/cpp             # AST building
├── compiler.h/cpp        # IR generation
├── execution_engine.h/cpp # JIT/Interpreter
├── aot_compiler.h        # AOT compilation
├── primitives.h          # Primitive implementations
├── primitives_registry.h/cpp # Primitive registration
├── dictionary.h/cpp      # Word storage
└── runtime.c             # Minimal AOT runtime

tests/
├── test_primitives.cpp   # Primitive tests
├── test_parser.cpp       # Parser tests
├── test_ast.cpp          # AST tests
├── test_compiler.cpp     # Compiler/JIT tests
└── test_interpreter.cpp  # Interpreter tests
```

## Key Interfaces

### Parser Interface
```cpp
class Parser {
public:
    std::vector<Token> tokenize(const std::string& source);
};

class ASTBuilder {
public:
    std::unique_ptr<ASTNode> parse(const std::string& source);
};
```

### Compiler Interface
```cpp
class Compiler {
public:
    llvm::Function* compile(ASTNode* ast);
};
```

### Execution Engine Interface
```cpp
class AnvilExecutionEngine {
public:
    static AnvilExecutionEngine* create(
        std::unique_ptr<llvm::Module> module,
        ExecutionMode mode,
        std::string* error);

    void execute(llvm::Function* func, ExecutionContext* ctx);
};
```

### Primitive Registration
```cpp
void initialize_primitives() {
    global_primitives.register_primitive("+", emit_add);
    global_primitives.register_primitive("DUP", emit_dup);
    // ... etc
}
```

## Compilation Modes

See [execution-modes.md](execution-modes.md) for details:

- **Lazy JIT** (default): Compile on first execution
- **Interpreter** (`--no-jit`): Always interpret
- **Eager JIT** (`--jit`): Compile immediately on definition
- **AOT** (`--compile`): Compile to standalone executable

## See Also

- [Parser Documentation](parser.md) - Tokenization and AST building
- [Compiler Documentation](compiler.md) - IR generation details
- [Primitives Documentation](primitives.md) - Primitive implementations
- [Execution Modes](execution-modes.md) - JIT vs Interpreter vs AOT
- [AOT Compilation](aot.md) - Ahead-of-time compilation details
