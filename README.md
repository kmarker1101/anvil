# Anvil Forth

A modern Forth implementation with LLVM backend, supporting three execution modes from a single IR.

## Features

- **Three Execution Modes**: JIT compilation, interpretation, and AOT compilation
- **LLVM Backend**: Optimized native code generation
- **Mode-Agnostic Semantics**: Identical behavior across all execution modes
- **Inline Primitives**: Direct LLVM IR emission for maximum optimization
- **Standard Library**: Automatically loaded Forth definitions
- **File Inclusion**: Load and compile external Forth files
- **Comprehensive Testing**: 148 tests running in both JIT and Interpreter modes (100% passing)

## Quick Start

### Build

```bash
mkdir build && cd build
cmake ..
make
```

### Run

```bash
# Interactive REPL (JIT mode)
./anvil

# Interpreter mode
./anvil --no-jit

# Compile to executable
./anvil --compile program.fth -o program
./program
```

### Test

```bash
make test                           # Run all tests
./anvil_tests "[primitives]"        # Test primitives
./anvil_tests "[stdlib]"            # Test standard library
```

## Example Session

```forth
\ Define a word
: square ( n -- n² ) DUP * ;

\ Use it
5 square .          \ Output: 25

\ Load external library
INCLUDE mylib.fth

\ Standard library words available
5 10 NIP .          \ Output: 10
1 2 3 2DROP .       \ Output: 1
```

## Project Structure

```
anvil/
├── src/                # C++ source code
│   ├── main.cpp        # REPL and CLI
│   ├── compiler.h      # LLVM IR compiler
│   ├── parser.h        # Tokenizer
│   ├── ast.h           # AST builder
│   ├── primitives.h    # Primitive word definitions
│   └── dictionary.h    # Word dictionary
├── forth/              # Forth source files
│   └── stdlib.fth      # Standard library
├── tests/              # Catch2 test suite
└── docs/               # Documentation
    └── project.md      # Comprehensive design doc
```

## Documentation

- **[CLAUDE.md](CLAUDE.md)** - Quick reference for Claude Code
- **[docs/project.md](docs/project.md)** - Complete architecture and design
- **[docs/architecture.md](docs/architecture.md)** - System components
- **[docs/parser.md](docs/parser.md)** - Tokenization details
- **[docs/aot.md](docs/aot.md)** - AOT compilation

## Language Support

**Primitives:**
- Stack: `DUP`, `DROP`, `SWAP`, `OVER`, `ROT`
- Arithmetic: `+`, `-`, `*`, `/MOD`
- Comparison: `<`, `=`
- Bitwise: `AND`, `OR`, `XOR`, `INVERT`
- Return stack: `>R`, `R>`, `R@`, `2>R`, `2R>`, `2R@`
- Memory: `@`, `!`, `C@`, `C!`
- I/O: `.`, `EMIT`, `CR`
- Control: `IF...THEN...ELSE`, `BEGIN...UNTIL`, `BEGIN...WHILE...REPEAT`, `DO...LOOP`

**Standard Library:**
- `NIP` - Remove second stack item
- `TUCK` - Copy top under second
- `2DROP` - Drop two items

**Comments:**
- Line: `\ comment to end of line`
- Block: `( comment )` with nesting support

**File Inclusion:**
- `INCLUDE filename.fth`

## Testing

All primitives are tested in both JIT and Interpreter modes to ensure semantic consistency. The test framework uses Catch2 with custom helpers:

```cpp
TEST_BOTH_MODES("Test name", "[tags]", {
    auto ctx = execute_test_mode("test", mode, [](auto& b, auto* ds, auto* dsp) {
        // Emit test IR
    });
    REQUIRE(ctx.dsp == expected);
})
```

**Current test count:** 148 tests, all passing (100%)

## Requirements

- C++17/C++20 compiler
- LLVM 21 development libraries
- CMake 3.13+

## License

See LICENSE file for details.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.
