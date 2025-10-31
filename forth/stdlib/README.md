# Anvil Forth Standard Library

The standard library is split into multiple files to ensure proper dependency ordering and maintainability.

## File Organization

Files are loaded in numerical order. Each file only depends on files with lower numbers.

### 00-stack.fth
Basic stack manipulation words with no dependencies:
- `2DROP`, `2DUP`, `NIP`, `OVER`, `TUCK`

### 10-arithmetic.fth
Arithmetic operations (depends on stack words for NIP):
- `1-`, `NEGATE`, `/`, `ABS`

### 20-comparison.fth
Comparison operations (no stdlib dependencies):
- `0=`, `>`

### 25-stack-advanced.fth
Advanced stack manipulation (depends on comparison and arithmetic):
- `PICK` (uses `0=` and `1-`)

### 30-memory.fth
Memory operations (no stdlib dependencies):
- `+!`, `CELL+`, `CELLS`

### 40-strings.fth
String operations (no stdlib dependencies):
- `COUNT`, `ERASE`, `MOVE`, `S=`

### 50-io.fth
Input/output operations (depends on arithmetic for `1-`):
- `BL` (constant), `SPACE`, `SPACES`, `BLANK`, `PAD`

### 60-parsing.fth
Parsing operations (depends on comparison for `0=`):
- `REFILL`, `WORD`

## Build Process

During the build, CMake concatenates these files in order into `build/stdlib.fth`.
The build system automatically tracks changes to any source file and regenerates
the combined file as needed.

## Adding New Words

When adding new words:
1. Identify dependencies on other stdlib words
2. Place the word in the appropriate numbered file based on dependencies
3. If no suitable file exists, create a new one with an appropriate number
4. Update CMakeLists.txt to include the new file in the correct position
5. Document dependencies in the file header

## Dependency Rules

- Words must be defined before they are used
- Each file can only use words from primitives or earlier-numbered files
- Avoid circular dependencies by careful file organization
