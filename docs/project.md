# Anvil: A Forth Compiler with LLVM Backend

## Project Overview

Anvil is a Forth implementation written in C++ that compiles directly to LLVM IR, supporting three execution modes: interpretation, JIT compilation, and ahead-of-time (AOT) compilation. The goal is to create a self-hosting compiler with excellent performance through LLVM's optimization capabilities.

## Core Design Principles

### 1. Mode-Agnostic Semantics

All three execution modes (interpreted, JIT, AOT) must work from the same intermediate representation and share identical semantics. This prevents the bugs encountered in the previous Rust implementation where behavior diverged between modes.

**Architecture:**
```
Forth source → Parse → LLVM IR → [Interpreter | JIT | AOT]
                                       ↓        ↓      ↓
                                   Execute  Execute  Object file
```

### 2. Inline-First Strategy

All primitives are inlined from the start. Rather than generating function calls, each primitive emits LLVM IR directly into the current basic block. This enables:

- Cross-primitive optimization by LLVM
- Elimination of calling overhead
- Better dataflow analysis
- Simpler JIT integration

### 3. Minimal Primitive Set

**A word is primitive if and only if it cannot be defined using existing primitives.**

This keeps the C++/LLVM core small and maintainable, with most functionality implemented in Forth itself.

### 4. Smart Compilation Strategy

Words use **lazy JIT compilation by default**: interpreted on first execution, then compiled for subsequent calls. Compilation mode can be controlled via command-line flags or runtime words.

### 5. Global Dictionary

Anvil uses a **single global dictionary** for word definitions, following traditional Forth behavior. This provides:

- Simple implementation and fast lookup
- Natural interactive REPL experience
- Traditional Forth semantics
- Easy word redefinition

The dictionary is encapsulated behind a clean API to allow future enhancement with modules/vocabularies without breaking existing code.

### 6. Two-Stack Architecture

Anvil uses the **traditional Forth two-stack model**:

- **Data Stack:** Parameters, working values, function arguments/results
- **Return Stack:** Temporary storage, loop counters, local state

LLVM's native call stack handles actual function returns, while the Forth return stack serves as programmer-accessible scratch space. This preserves traditional Forth semantics while leveraging LLVM's efficient calling conventions.

### 7. 64-Bit Architecture (Mandatory)

Anvil is a **pure 64-bit system**. Every value is 64 bits:

**Stack Cells:**
- Data stack: `int64_t data_stack[1024]`
- Return stack: `int64_t return_stack[1024]`
- Stack pointers: `size_t dsp, rsp` (64-bit on 64-bit platforms)

**LLVM IR Generation:**
- Always use `builder.getInt64Ty()` for stack operations
- Always use `builder.getInt64(n)` for immediate values
- All arithmetic, bitwise, and comparison operations on 64-bit values
- Comparisons return i1, then `CreateSExt` to i64 (-1 for true, 0 for false)

**Dictionary:**
- Word flags: `uint64_t flags` (not uint32_t)
- Execution tokens: Native pointers (64-bit on 64-bit systems)

**Memory Operations:**
- `@` and `!`: Load/store 64-bit values
- `C@` and `C!`: Load/store 8-bit bytes (with zero-extend/truncate to/from 64-bit)
- Addresses are full 64-bit pointers

**Never Use:**
- `int32_t`, `uint32_t`, `int16_t`, `uint16_t` for stack values
- `builder.getInt32Ty()` or other non-64-bit integer types for stack operations

**Why 64-Bit:**
- Consistent cell size across all operations
- Pointers fit in stack cells on 64-bit systems
- No accidental truncation of values
- Clean interoperability with native 64-bit code
- Future-proof for large address spaces

## Implementation Language: C++

### Why C++ Over C

- LLVM is written in C++ with a first-class C++ API
- RAII for managing LLVM object lifetimes (contexts, modules, builders)
- Smart pointers for memory safety
- STL containers for dictionaries and data structures
- Better type safety when building IR

### C++ Style Guidelines

Use modern C++ (C++17/C++20) with restraint - think "C with better tools":

- Use RAII and smart pointers (`std::unique_ptr` primarily)
- Use STL containers where appropriate
- Keep code mostly procedural
- Avoid heavy template metaprogramming
- Avoid deep inheritance hierarchies
- Use references over pointers where ownership is clear

## Modular Architecture

### Layer Separation

**Frontend (Parsing & Dictionary)**
- **Parser**: Forth source → Tokens (whitespace-delimited)
- **AST Builder**: Tokens → Abstract Syntax Tree
- **Dictionary**: Word definitions, lookups, case-insensitive resolution
- Knows nothing about execution strategies

**Middle (IR Generation)**
- **IR Builder**: AST → LLVM IR
- Each primitive has an IR emission function
- Traverses AST and generates corresponding IR
- Knows nothing about optimization or execution backends

**Backend (Execution Strategies)**
- **Interpreter**: Walks IR/AST directly
- **JIT**: Compiles functions to memory on first execution
- **AOT**: Compiles to object files
- All three consume the same IR

**Runtime Services (Shared)**
- I/O operations
- Memory management
- Stack implementation (data + return stacks)
- Shared across all execution modes

### Dictionary Architecture

**Global Dictionary Design:**

```cpp
class Anvil {
private:
    std::unordered_map<std::string, Word*> dictionary_;
    
public:
    Word* find(const std::string& name);
    void define(const std::string& name, Word* word);
    bool exists(const std::string& name);
    void remove(const std::string& name);
};
```

**Characteristics:**
- Single global namespace (traditional Forth)
- Fast O(1) lookup
- Words immediately available after definition
- Simple redefinition semantics
- Encapsulated behind API for future enhancement

**Future Enhancement Path:**

The API design allows for later addition of:
- Vocabularies/wordlists (ANS Forth `WORDLIST`, `SET-CURRENT`)
- Module system
- Search order management
- Isolated contexts for testing

These can be added by changing internal implementation without breaking existing code.

### Parser Architecture

**Hand-Written Parser (No External Libraries)**

Forth's simplicity makes a hand-written parser ideal:
- Whitespace-delimited tokens
- No operator precedence
- No complex grammar rules
- ~250 lines total implementation

**Two-Stage Parsing:**

```
Source Code → Parser → Tokens → AST Builder → AST Tree → IR Generator
```

**Stage 1: Tokenization (parser.h)**

The `Parser` class tokenizes Forth source into five token types:

```cpp
enum class TokenType {
    WORD,      // Dictionary word (DUP, SWAP, custom words)
    NUMBER,    // 64-bit integer literal
    STRING,    // String literal for S" and ."
    COMMENT,   // Line (\) or block ((..)) comments
    END        // End of input
};
```

**Number Format Support:**
- Decimal: `42`, `-123`
- Hexadecimal: `0xFF`, `$FF` (Forth style with `$`)
- Binary: `0b1010`, `%1111` (Forth style with `%`)
- All parsed to `int64_t` (64-bit)

**Comment Handling:**
- Line comments: `\ comment to end of line`
- Block comments: `( nested ( comments ) supported )`
- Automatically filtered during tokenization

**Usage:**
```cpp
Parser parser;
auto tokens = parser.tokenize("42 DUP +");
// Returns: [NUMBER(42), WORD("DUP"), WORD("+")]
```

**Stage 2: AST Building (ast.h)**

The `ASTBuilder` class converts flat token streams into hierarchical AST:

```cpp
enum class ASTNodeType {
    LITERAL,              // Number literal
    WORD_CALL,            // Call to dictionary word
    SEQUENCE,             // Sequential execution
    DEFINITION,           // : NAME ... ;
    IF_THEN,              // IF ... THEN
    IF_ELSE_THEN,         // IF ... ELSE ... THEN
    BEGIN_UNTIL,          // BEGIN ... UNTIL
    BEGIN_WHILE_REPEAT,   // BEGIN ... WHILE ... REPEAT
    DO_LOOP,              // DO ... LOOP (and +LOOP)
    STRING_LITERAL,       // String literal
    VARIABLE,             // VARIABLE name
    CONSTANT              // CONSTANT name value
};
```

**AST Structure:**
- Recursive tree using `std::unique_ptr<ASTNode>`
- Each node type has specific data (e.g., `LiteralNode` has `int64_t value`)
- Control structures have child nodes (e.g., `IfElseThenNode` has `then_branch` and `else_branch`)

**Example AST Construction:**

Source: `: SQUARE DUP * ;`

AST:
```
DefinitionNode("SQUARE")
  └─ SequenceNode
      ├─ WordCallNode("DUP")
      └─ WordCallNode("*")
```

**Recursive Descent Parsing:**
- `parse_expression()` - Single expression (literal, word, or control structure)
- `parse_definition()` - `: NAME ... ;` word definitions
- `parse_if()` - IF...THEN...ELSE control flow
- `parse_begin()` - BEGIN...UNTIL/WHILE...REPEAT loops
- `parse_do_loop()` - DO...LOOP/+LOOP loops

**Case-Insensitive Keywords:**
- `IF`, `if`, `If` all recognized as keywords
- Word names preserve original case for error messages
- Dictionary handles case-insensitivity at lookup time

**Usage:**
```cpp
ASTBuilder builder;
auto ast = builder.parse(": SQUARE DUP * ;");
// Returns AST tree ready for IR generation
```

**Why Hand-Written?**
- Forth is uniquely simple (whitespace-delimited tokens)
- Parser libraries (Flex/Bison, ANTLR, Boost.Spirit) are overkill
- Direct implementation is faster, simpler, and easier to maintain
- Zero external dependencies
- Can easily add Forth-specific features

### Stack Architecture

**Two-Stack Model:**

```cpp
struct ExecutionContext {
    int64_t data_stack[DATA_STACK_SIZE];
    int64_t return_stack[RETURN_STACK_SIZE];
    size_t dsp;   // Data stack pointer
    size_t rsp;   // Return stack pointer
};
```

**Stack Roles:**

**Data Stack:**
- Primary working storage
- Function parameters and results
- All arithmetic and logical operations
- Standard Forth stack operations (`DUP`, `SWAP`, etc.)

**Return Stack:**
- Temporary value storage (`>R`, `R>`, `R@`)
- Loop counters (`DO...LOOP` implementation)
- Local variable storage
- Preserving values across word calls

**LLVM Call Stack (implicit):**
- Actual function return addresses
- Native calling conventions
- Automatic management by LLVM

**Why This Works:**

When compiling word calls, LLVM generates proper `call` and `ret` instructions using its native stack. The Forth return stack is orthogonal - it's just additional storage that Forth programmers can manipulate. This gives us:

- Traditional Forth semantics for return stack operations
- Efficient native calling conventions via LLVM
- Best of both worlds

**LLVM IR Generation:**

Each compiled word receives stack pointers as parameters:

```cpp
void emit_word(IRBuilder<>& builder, 
               Value* data_stack, 
               Value* return_stack,
               Value* dsp,
               Value* rsp) {
    // Generate IR that manipulates stacks
}
```

**Example Primitive Implementations:**

```cpp
// Data stack: DUP
void emit_dup(IRBuilder<>& builder, Value* dstack, Value* dsp) {
    Value* top = load_from_stack(builder, dstack, dsp);
    increment_stack_pointer(builder, dsp);
    store_to_stack(builder, dstack, dsp, top);
}

// Return stack: >R (to-r)
void emit_to_r(IRBuilder<>& builder, 
               Value* dstack, Value* rstack,
               Value* dsp, Value* rsp) {
    Value* value = load_from_stack(builder, dstack, dsp);
    decrement_stack_pointer(builder, dsp);
    increment_stack_pointer(builder, rsp);
    store_to_stack(builder, rstack, rsp, value);
}
```

**Stack Safety:**

- Overflow/underflow checking in debug builds
- Optional bounds checking in primitives
- Stack depth tracking for optimization

### Variables and Constants

Anvil provides `VARIABLE` and `CONSTANT` as compile-time primitives that create named storage and values.

**VARIABLE Implementation:**

`VARIABLE` allocates storage at compile time in the data space and creates a word that pushes the variable's address:

```forth
VARIABLE X      \ Allocates 8 bytes, creates word X
42 X !          \ Store 42 at address X
X @             \ Fetch value from X (returns 42)
```

**Implementation details:**
- Each variable allocates exactly 8 bytes (one 64-bit cell)
- Address is computed at compile time using `compile_time_here_` tracking
- The created word generates LLVM IR to compute: `data_space + offset`
- Variables maintain fixed addresses throughout execution
- Multiple variables get sequential addresses in data space

**CONSTANT Implementation:**

`CONSTANT` creates a word that pushes a literal value directly:

```forth
CONSTANT PI 314     \ Creates word PI that pushes 314
PI                  \ Stack: 314
PI PI *             \ Stack: 98596 (314 * 314)
```

**Implementation details:**
- Value is embedded directly in LLVM IR as a literal
- No data space allocation (value lives in code)
- Extremely efficient - just pushes immediate value
- Syntax: `CONSTANT name value` (non-standard but compile-friendly)
- Standard Forth uses `value CONSTANT name` but requires runtime evaluation

**Usage Examples:**

```forth
\ Counter with variable
VARIABLE counter
0 counter !
counter @ 1 + counter !   \ Increment
counter @ .               \ Print: 1

\ Multiple variables
VARIABLE X
VARIABLE Y
10 X !
20 Y !
X @ Y @ + .              \ Print: 30

\ Constants in calculations
CONSTANT WIDTH 80
CONSTANT HEIGHT 24
WIDTH HEIGHT * .         \ Print: 1920 (total cells)

\ Mix variables and constants
VARIABLE temperature
CONSTANT FREEZING 32
70 temperature !
temperature @ FREEZING - .  \ Print: 38 (degrees above freezing)
```

**Memory Layout:**

```
Data Space:
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  Variable 1 │  Variable 2 │  Variable 3 │   (unused)  │
│   (8 bytes) │   (8 bytes) │   (8 bytes) │             │
└─────────────┴─────────────┴─────────────┴─────────────┘
      ↑              ↑              ↑
   offset=0      offset=8      offset=16

Constants: No memory allocation (values in IR)
```

**Why These Are Primitives:**

VARIABLE and CONSTANT require compile-time behavior:
- Need to allocate storage during compilation, not execution
- Must create new dictionary entries
- Generate LLVM IR for the created words
- Cannot be implemented using existing primitives

In standard Forth, these would use `CREATE` and `DOES>`, but we haven't implemented those meta-programming features yet. These primitive implementations provide the core functionality needed for data storage and named constants.

### Testing Strategy

Every feature must be tested in all three modes:
```cpp
test_feature_interpreted();
test_feature_jit();
test_feature_aot();
```

Stack operations must be tested for:
- Correct depth management
- Overflow/underflow conditions
- Data/return stack isolation
- Interaction with LLVM calling conventions

This catches semantic divergence immediately.

## Execution Model: Smart Compilation

### Compilation Modes

**JIT (Default):**
```bash
anvil
```
- Words compile on first execution (lazy JIT)
- Responsive REPL (definitions return instantly)
- Hot paths automatically optimized

**Interpretation Only:**
```bash
anvil --no-jit
```
- All words always interpreted
- Useful for debugging or resource-constrained environments
- No compilation overhead

**AOT Compilation:**
```bash
anvil --compile myprogram.fth -o myprogram
```
- Compile entire program to standalone executable
- No runtime compilation
- Maximum startup performance
- Cross-platform: macOS (tested), Linux, Windows (portable code)

### Word Definition (Same Syntax Everywhere)

```forth
: SQUARE DUP * ;
```

The `:` syntax is identical in all modes. The compilation strategy is controlled globally via command-line flags.

### Word Structure

```cpp
struct Word {
    std::string name;
    llvm::Function* ir_function;    // LLVM IR
    void* native_code;              // nullptr until JIT compiled
    WordFlags flags;
    bool is_primitive;

    void execute(ExecutionContext* ctx) {
        if (mode == Interpreter) {
            interpret_ir(ir_function, ctx);
        } else {  // JIT mode
            if (!native_code) {
                // Lazy JIT: compile on first call
                native_code = jit_compile(ir_function);
            }
            call_native(native_code, ctx);
        }
    }
};
```

### Compilation Flow

**When a word is defined (Lazy JIT mode):**
```forth
: 2DUP OVER OVER ;
```

1. Parse definition into word references
2. Look up each word in global dictionary
3. Store word reference list in new Word object
4. Add to global dictionary
5. Set `native_code = nullptr`
6. Return to REPL (instant)

**When the word is first called:**
1. Look up word in global dictionary
2. Check if `native_code` exists
3. If null: compile definition to LLVM IR → native code
4. Store native code pointer
5. Execute native code with execution context (both stacks)

**Subsequent calls:**
- Look up word in global dictionary
- Execute native code directly (fast path)

**With --no-jit:**
- `native_code` remains nullptr forever
- Word always interprets IR

### Benefits of This Approach

✅ **Consistent syntax:** Same `:` everywhere, no cognitive overhead
✅ **Smart defaults:** Lazy JIT optimizes common case
✅ **User control:** Flags for different use cases (--no-jit, --compile)
✅ **Simple mental model:** Mode is a global setting, not per-word decision
✅ **REPL responsive:** Definitions return instantly
✅ **No wasted work:** Only compile words that are actually used (lazy JIT)
✅ **Traditional Forth:** Global dictionary and two-stack model match expected behavior

## Primitive Word Set

Currently implemented: **45 primitives**

**Data Stack Operations:**
- `DUP`, `DROP`, `SWAP`, `OVER`, `ROT`

**Arithmetic:**
- `+`, `-`, `*`, `/MOD`

**Bitwise/Logical:**
- `AND`, `OR`, `XOR`, `INVERT`

**Comparison:**
- `<`, `=` (others can be derived)

**Memory Access:**
- `@`, `!` - 64-bit load/store
- `C@`, `C!` - 8-bit load/store
- `HERE` - Get current data space pointer
- `ALLOT` - Allocate space in data space
- `,` - Store value and advance HERE

**Variables and Constants:**
- `VARIABLE` - Create a variable (allocates 8 bytes, word pushes address)
- `CONSTANT` - Create a constant (word pushes literal value)

**Return Stack Operations:**
- `>R` - Move top of data stack to return stack
- `R>` - Move top of return stack to data stack
- `R@` - Copy top of return stack to data stack
- `2>R`, `2R>`, `2R@` - Double-cell versions for DO loop support

**Loop Support:**
- `I` - Current loop index
- `J` - Outer loop index (for nested loops)

**I/O Operations:**
- `.` - Print top of stack as number
- `EMIT` - Output single character (ASCII code from stack)
- `CR` - Print newline
- `TYPE` - Print string

**Terminal I/O:**
- `KEY` - Read single character (blocking)
- `KEY?` - Check if character available (non-blocking, returns flag)
- `RAW-MODE` - Switch terminal to raw mode (no echo, no buffering)
- `COOKED-MODE` - Restore normal terminal mode
- `EMIT-ESC` - Output ESC character (ASCII 27) for ANSI escape sequences

**Input Buffer Management:**
- `TIB` - Returns address of Terminal Input Buffer
- `>IN` - Returns address of current parse position variable
- `#TIB` - Returns address of buffer length variable
- `SOURCE` - Returns TIB address and length (equivalent to `TIB #TIB @`)
- `ACCEPT` - Read a line of input with backspace support ( c-addr +n1 -- +n2 )
- `PARSE` - Parse string from input until delimiter ( char "ccc<char>" -- c-addr u )

**String Manipulation (ANS Forth Core):**
- `CMOVE` - Copy memory low-to-high ( c-addr1 c-addr2 u -- )
- `CMOVE>` - Copy memory high-to-low for overlapping regions ( c-addr1 c-addr2 u -- )
- `FILL` - Fill memory with byte value ( c-addr u char -- )
- `COMPARE` - Compare two strings lexicographically ( c-addr1 u1 c-addr2 u2 -- n )
  - Returns: n < 0 if str1 < str2, n = 0 if equal, n > 0 if str1 > str2

All primitives emit LLVM IR directly (no function calls for stack operations).
Primitives work identically in all three execution modes (Interpreter, JIT, AOT).

### Primitive Implementation Pattern

Each primitive is implemented as a function that emits LLVM IR:

```cpp
void emit_dup(IRBuilder<>& builder, 
              Value* data_stack, 
              Value* dsp) {
    // Load top of stack
    Value* top = /* load from data_stack[dsp] */;
    // Increment stack pointer  
    /* dsp++ */
    // Store value again
    /* data_stack[dsp] = top */
}

void emit_to_r(IRBuilder<>& builder,
               Value* data_stack,
               Value* return_stack, 
               Value* dsp,
               Value* rsp) {
    // Pop from data stack
    Value* val = /* data_stack[dsp] */;
    /* dsp-- */
    // Push to return stack
    /* rsp++ */
    /* return_stack[rsp] = val */
}
```

Dictionary entries hold function pointers to these IR-emitting functions.

## Bootstrapping Strategy

### Phase 1: Core Primitives (C++)

Implement the ~25-30 primitives in C++ with LLVM IR emission. Get these working and tested in all three execution modes.

**Dictionary initialization:**
```cpp
void initialize_primitives(Anvil& anvil) {
    // Data stack ops
    anvil.define("DUP", new PrimitiveWord(emit_dup));
    anvil.define("DROP", new PrimitiveWord(emit_drop));
    anvil.define("SWAP", new PrimitiveWord(emit_swap));
    
    // Return stack ops
    anvil.define(">R", new PrimitiveWord(emit_to_r));
    anvil.define("R>", new PrimitiveWord(emit_from_r));
    anvil.define("R@", new PrimitiveWord(emit_r_fetch));
    
    // Arithmetic
    anvil.define("+", new PrimitiveWord(emit_add));
    anvil.define("-", new PrimitiveWord(emit_sub));
    
    // ... etc
}
```

### Phase 2: Standard Library (Forth)

Once primitives work, define everything else in Forth. These definitions are added to the global dictionary just like primitives:

1. **Additional stack words:** `2DUP`, `NIP`, `TUCK`, `2SWAP`, `-ROT`, etc.
2. **Control structures:** `IF...THEN`, `BEGIN...UNTIL`, `DO...LOOP`, `WHILE...REPEAT`
3. **Compiler words:** `:`, `;`, `IMMEDIATE`, `[`, `]`, `LITERAL`
4. **Dictionary management:** `CREATE`, `DOES>` (future work)
5. **String/parsing:** `WORD`, `PARSE`, `S"`, `CHAR`
6. **Extended arithmetic:** `2*`, `2/`, `<=`, `>=`, `<>`, `ABS`, `MIN`, `MAX`
7. **Return stack utilities:** Using `>R` and `R>` for temporary storage patterns

**Example standard library definitions:**
```forth
: 2DUP OVER OVER ;
: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: 2DROP DROP DROP ;
: 2SWAP >R -ROT R> -ROT ;
: WITHIN OVER - >R - R> U< ;  \ Uses return stack
```

All of these are stored in the same global dictionary alongside primitives.

### Phase 3: Self-Hosting

The Forth compiler itself becomes Forth code built on the primitives. When a user defines a new word, the compiler:

1. Parses the definition
2. Walks through each word in the definition
3. Looks up each word in the global dictionary
4. Emits LLVM IR by calling the appropriate emit functions
5. Manages both data and return stack in generated code
6. Optimizes the resulting IR
7. Stores the new word in the global dictionary
8. Makes it available for execution (compiled according to current mode)

## Performance Expectations

### LLVM vs Hand-Written Assembly

For most code, LLVM will outperform hand-written assembly due to:
- Sophisticated optimization passes (hundreds of them)
- Architecture-specific optimizations
- Register allocation across large scopes
- Vectorization and loop optimizations

Hand-written assembly might only win for:
- Extremely simple primitives where instruction count is minimal
- Inner interpreter loops in threaded code implementations
- Architecture-specific tricks LLVM doesn't know

### The Real Win

Performance comes from compilation strategy (inlined native code) rather than micro-optimizing individual instructions. Starting with LLVM gives:

- Faster development velocity
- Portability across architectures
- Professional-grade optimization
- Ability to drop to inline assembly for specific hotspots later if needed

### Stack Performance

**Two stacks don't significantly impact performance because:**
- Both are typically in L1 cache
- Stack pointer increments are cheap (single instruction)
- LLVM can often optimize stack operations away entirely through dataflow analysis
- Return stack operations are less frequent than data stack operations
- Inlining allows LLVM to see through stack manipulations

### Compilation Mode Performance Characteristics

**JIT (default):**
- **Interactive use:** Near-instant word definitions, responsive REPL
- **First execution:** Slight delay while compiling (typically milliseconds)
- **Steady state:** All hot paths are native code, maximum performance
- **Memory:** Compiled code accumulates but is typically small

**Interpretation only (--no-jit):**
- **All execution:** Slower, but consistent performance
- **Memory:** Minimal, no compiled code
- **Best for:** Debugging, understanding control flow, stack visualization

**AOT compilation (--compile):**
- **Compilation:** Once, ahead of time
- **Startup:** Instant, everything pre-compiled
- **Execution:** Maximum performance
- **Binary size:** ~33KB for typical programs
- **Cross-platform:** macOS (tested), Linux, Windows
- **Best for:** Production deployments, standalone executables, distribution

## Future Directions

- **QUIT interpreter loop** (Issue #9 - Complete):
  - ✅ `FIND` primitive for runtime dictionary lookup with JIT support
  - ✅ ' (tick) and EXECUTE with proper JIT address resolution
  - ✅ STATE-VAR for interpreter state management
  - ✅ NUMBER primitive for parsing numeric literals (decimal/hex)
  - ✅ INTERPRET-WORD for core interpretation logic
  - 🔲 IMMEDIATE flag support for [ and ] words (future work)
  - 🔲 Full QUIT REPL loop (future work - needs IMMEDIATE support)
  - Core interpreter infrastructure complete and tested
- Profile-guided optimization: track execution counts, recompile hot words with higher optimization
- Background compilation: compile in separate thread while interpreting
- Code garbage collection: reclaim unused compiled words
- LLVM optimization levels: `-O0` for fast compilation vs `-O3` for maximum optimization
- Self-hosting: compiler written in Anvil itself
- Tiered compilation: interpret → basic JIT → optimized JIT
- Vocabularies/wordlists: ANS Forth compatibility for namespace management
- Module system: organized code with explicit imports/exports
- Stack effect checking: static analysis of stack operations
- Potential transformation to native code generation without LLVM dependency

## Why "Anvil"?

The name reflects the project's nature:
- Forging (compiling) with LLVM
- Solid, grounded systems programming
- Where raw materials (Forth source) are shaped into refined tools (native code)
- Evocative of low-level, foundational work

## Getting Started

### Prerequisites

- C++17 or C++20 compatible compiler
- LLVM 21 development libraries
- CMake 4.1.2+

### Building

```bash
mkdir build
cd build
cmake ..
make
```

### Running

```bash
# Interactive REPL (default: JIT)
./anvil

# Interpretation only
./anvil --no-jit

# AOT compilation
./anvil --compile myprogram.fth           # Output: a.out
./anvil --compile myprogram.fth -o myprog # Output: myprog
```

### Testing

```bash
make test
```

**Test Coverage:**
- 198 total tests, all passing (100%)
- All primitive tests run in both JIT and Interpreter modes automatically
- Stdlib words tested in both modes
- Ensures semantic consistency across execution modes

**Test Organization:**
- `tests/test_primitives.cpp` - All primitives tested in JIT + Interpreter modes
- `tests/test_input_buffer.cpp` - Input buffer primitives (TIB, >IN, #TIB, SOURCE, PARSE)
- `tests/test_compiler.cpp` - Compiler and high-level features (JIT mode)
- `tests/test_interpreter.cpp` - Interpreter-specific tests
- `tests/test_stdlib.cpp` - Standard library words (both modes)
- `tests/test_variables.cpp` - VARIABLE and CONSTANT tests
- `tests/test_emit.cpp` - EMIT primitive tests
- `tests/test_terminal.cpp` - Terminal I/O primitives (KEY, KEY?, RAW-MODE, COOKED-MODE)
- `tests/test_parser.cpp` - Tokenization and parsing
- `tests/test_ast.cpp` - AST building
- `tests/test_dictionary.cpp` - Dictionary operations

**Running specific tests:**
```bash
./anvil_tests "[primitives]"           # All primitive tests
./anvil_tests "[primitives][jit]"      # JIT mode only
./anvil_tests "[primitives][interpreter]" # Interpreter mode only
./anvil_tests "[stdlib]"               # Standard library tests
./anvil_tests "[input]"                # Input buffer management tests
./anvil_tests "[terminal]"             # Terminal I/O tests
```

## Language Features

### Standard Library

Anvil automatically loads a standard library (`stdlib.fth`) at startup, providing common Forth words built on top of the primitives:

**Stack manipulation:**
- `NIP ( a b -- b )` - Drop second item
- `TUCK ( a b -- b a b )` - Copy top item under second
- `2DROP ( a b -- )` - Drop top two items
- `2DUP ( a b -- a b a b )` - Duplicate top two items
- `0= ( n -- flag )` - Test if zero

**Arithmetic:**
- `NEGATE ( n -- -n )` - Negate number
- `ABS ( n -- +n )` - Absolute value

**Memory operations:**
- `OVER ( a b -- a b a )` - Copy second item to top
- `CELLS ( n -- n*8 )` - Convert cell count to byte count
- `CELL+ ( addr -- addr+8 )` - Add one cell to address
- `+! ( n addr -- )` - Add to memory location

**Output:**
- `BL ( -- 32 )` - Push space character code (constant)
- `SPACE ( -- )` - Output a single space
- `SPACES ( n -- )` - Output n spaces

**String manipulation:**
- `COUNT ( c-addr -- c-addr+1 u )` - Convert counted string to address and length
- `MOVE ( addr1 addr2 u -- )` - Copy u bytes (handles overlapping regions)
- `S= ( c-addr1 u1 c-addr2 u2 -- flag )` - Compare two strings for equality
- `BLANK ( c-addr u -- )` - Fill memory region with spaces
- `ERASE ( addr u -- )` - Fill memory region with zeros
- `PAD ( -- c-addr )` - Return address of temporary buffer (HERE + 256)

**Input buffer:**
- `REFILL ( -- flag )` - Read new line into TIB, reset >IN, return true
- `WORD ( char -- c-addr )` - Parse word delimited by char, return counted string at HERE

**Interpreter state:**
- `STATE-VAR ( -- addr )` - Push address of STATE variable (0 = interpreting, -1 = compiling)
- `FIND ( c-addr u -- xt flag )` - Search dictionary for word, return XT and flag (-1=found, 0=not found)
- `EXECUTE ( xt -- )` - Execute the execution token on the stack
- `' name ( -- xt )` - Get execution token of word (uses FIND at runtime for proper JIT address resolution)
- `NUMBER ( c-addr u -- n flag )` - Parse string as number, return value and flag (1=valid, 0=invalid)
- `INTERPRET-WORD ( c-addr u -- )` - Interpret a word: execute if in dictionary, otherwise parse as number

**Interpreter infrastructure (Issue #9 - Complete):** Anvil now has a complete interpreter infrastructure with FIND, EXECUTE, NUMBER, and INTERPRET-WORD. The FIND primitive enables runtime dictionary lookup with proper JIT address resolution. The ' (tick) word calls FIND at runtime, ensuring user-defined words get correct JIT-compiled addresses. NUMBER parses strings as decimal or hex numbers. INTERPRET-WORD provides the core interpretation logic: lookup word with FIND, execute if found, otherwise try NUMBER. This provides everything needed for custom interpreters and meta-compilation. Note: Full QUIT REPL loop and [ ] require IMMEDIATE flag support (future work).

The standard library is compiled to LLVM IR and added to the global dictionary, making these words available immediately in the REPL and in all execution modes (JIT, interpreter, and AOT).

**Example:**
```forth
5 3 NIP .         \ Output: 3
5 10 TUCK . . .   \ Output: 10 5 10
65 EMIT           \ Output: A
BL EMIT           \ Output: " " (space)
SPACE             \ Output: " " (space)
5 SPACES 72 EMIT  \ Output: "     H"
```

### Comments

Anvil supports two types of comments:

**Line comments:**
```forth
\ This is a comment to end of line
5 3 + .  \ Inline comment after code
```

**Block comments:**
```forth
( This is a block comment )

( Block comments can span
  multiple lines )

( Nested ( block ) comments are supported )
```

**Stack effect notation:**
```forth
: add ( n1 n2 -- sum )
    + ;

: calculate ( x y -- result )
    SWAP double SWAP +
    ;
```

Comments are handled during tokenization and completely stripped before parsing, so they have zero runtime overhead.

### File Inclusion

The `INCLUDE` directive allows loading Forth source files from within the REPL or other files:

**Basic usage:**
```forth
INCLUDE /path/to/file.fth
INCLUDE mylib.fth
```

**Features:**
- Case-insensitive (`INCLUDE` or `include`)
- Supports absolute and relative paths
- Handles multi-line definitions correctly
- Nested includes (files can include other files)
- Expands includes recursively before compilation

**Example library file** (`math.fth`):
```forth
\ Mathematical utilities
: square ( n -- n² )
    DUP * ;

: cube ( n -- n³ )
    DUP square * ;
```

**Using it:**
```forth
INCLUDE math.fth
5 square .  \ Output: 25
3 cube .    \ Output: 27
```

Files loaded via `INCLUDE` are compiled to LLVM IR in all execution modes, ensuring consistent behavior between JIT, interpreter, and AOT compilation.

### Error Reporting

Anvil provides clear, Forth-style error messages:

**Undefined words:**
```forth
foo
\ Error: foo ?
```

**Missing space after colon:**
```forth
:kevin DUP * ;
\ Error: :kevin ?
```

**File not found:**
```forth
INCLUDE /nonexistent.fth
\ Error: Could not open file: /nonexistent.fth
```

All errors use the simple Forth convention of printing the problematic word followed by `?`, making it easy to identify issues at a glance.

## Testing Framework

### Dual-Mode Testing

Anvil uses Catch2 for testing and automatically runs tests in both JIT and Interpreter modes to ensure semantic consistency. This is critical for maintaining identical behavior across execution modes.

**Test Helper Functions:**

```cpp
// Run test in specific mode
ExecutionContext execute_test_mode(
    const std::string& test_name,
    ExecutionMode mode,
    std::function<void(IRBuilder<>&, Value* data_stack, Value* dsp)> emit_func);

// Macro to run test in both modes
TEST_BOTH_MODES("Test name", "[tags]", {
    // Test code here
    // 'mode' variable is available (ExecutionMode::JIT or Interpreter)
    auto ctx = execute_test_mode("test_name", mode, [](auto& builder, auto* ds, auto* dsp) {
        // Generate test IR
    });
    REQUIRE(ctx.dsp == expected_value);
});
```

**Example:**
```cpp
TEST_BOTH_MODES("Addition primitive", "[primitives][add]", {
    auto ctx = execute_test_mode("test_add", mode, [](auto& builder, auto* ds, auto* dsp) {
        push_values(builder, ds, dsp, {2, 3});
        emit_add(builder, ds, dsp);
    });

    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 5);  // 2 + 3 = 5
})
```

This single test definition automatically creates two test cases:
1. "Addition primitive [JIT]" - runs in JIT mode
2. "Addition primitive [Interpreter]" - runs in Interpreter mode

### Testing Forth Code

For testing Forth words (including stdlib), use the `execute_with_stdlib` helper:

```cpp
TEST_CASE("Standard Library - NIP", "[stdlib][nip]") {
    SECTION("NIP removes second item") {
        auto ctx = execute_with_stdlib("5 3 NIP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }

    SECTION("NIP in interpreter mode") {
        auto ctx = execute_with_stdlib("7 8 NIP", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }
}
```

This approach:
- ✅ Tests primitives and user-defined words the same way
- ✅ Automatically verifies both execution modes
- ✅ Catches semantic divergence between modes early
- ✅ Integrates with CI/CD pipelines
- ✅ Provides clear test output with Catch2
