# AOT (Ahead-of-Time) Compilation

## Overview

AOT compilation transforms Forth source code into standalone native executables. The process involves:

1. Parse Forth source → AST
2. Compile AST → LLVM IR
3. Emit LLVM IR → native object file
4. Link object file with minimal runtime → executable

## Usage

```bash
# Basic compilation
./anvil --compile program.fth

# Specify output name
./anvil --compile program.fth -o myprogram

# The result is a standalone executable
./myprogram
```

## Cross-Platform Support

The AOT compiler is designed to work on multiple platforms:

### Supported Platforms
- ✅ **macOS** (x86-64, ARM64/M-series) - Tested
- ✅ **Linux** (x86-64, ARM64) - Portable code, needs testing
- ✅ **Windows** (x86-64, ARM64) - Portable code, needs testing

### Platform Detection

The compiler uses LLVM's target detection:
```cpp
llvm::Triple target_triple(llvm::sys::getDefaultTargetTriple());
```

This automatically detects:
- Architecture (x86-64, ARM64, etc.)
- Operating system (macOS, Linux, Windows)
- ABI details

## Implementation Details

### File Locations

**AOT Compiler Core**: `src/aot_compiler.h`
- `initialize_aot()` - Initialize LLVM for AOT mode
- `compile_to_object()` - Generate native object file

**Linking Logic**: `src/main.cpp`
- `compile_file()` - Orchestrates full compilation pipeline
- Platform-specific helpers for file operations

**Runtime**: `src/runtime.c`
- Minimal runtime (~35 lines)
- Provides `main()` that calls `__anvil_main()`

### Compilation Pipeline

```
Forth Source (.fth)
    ↓
[Parser] → AST
    ↓
[Compiler] → LLVM IR Module
    ↓
[compile_to_object] → Object File (.o)
    ↓
[Platform Linker] + Runtime → Executable
```

### Platform-Specific Behavior

#### Executable Path Detection
- **macOS**: `_NSGetExecutablePath()`
- **Windows**: `GetModuleFileNameA()`
- **Linux**: Read `/proc/self/exe`

#### Temporary Directories
- **Unix/Linux/macOS**: `/tmp/anvil_build_XXXXXX` via `mkdtemp()`
- **Windows**: `%TEMP%\anvil_XXXXX` via `GetTempPath()`

#### File Operations
- **Unix/Linux/macOS**: `cp`, `rm -rf`, `chmod +x`
- **Windows**: `CopyFileA()`, `rmdir /S /Q`, no chmod needed

#### Compiler Detection
Tries in order:
- **Unix/Linux/macOS**: `cc`, `gcc`, `clang`
- **Windows**: `cl.exe` (MSVC), `gcc.exe`, `clang.exe`

#### Runtime Library Name
- **Unix/Linux/macOS**: `libanvil_runtime.a`
- **Windows**: `anvil_runtime.lib`

### Object File Generation

**Location**: `src/aot_compiler.h:compile_to_object()`

```cpp
bool compile_to_object(llvm::Module* module,
                       const std::string& output_path,
                       std::string* error_msg) {
    // 1. Set target triple
    llvm::Triple target_triple(llvm::sys::getDefaultTargetTriple());
    module->setTargetTriple(target_triple);

    // 2. Create target machine
    llvm::TargetMachine* target_machine = target->createTargetMachine(
        target_triple,
        "generic",           // CPU
        "",                  // Features
        opt,
        llvm::Reloc::PIC_,
        llvm::CodeModel::Small,
        llvm::CodeGenOptLevel::Aggressive
    );

    // 3. Emit object file
    llvm::legacy::PassManager pass;
    target_machine->addPassesToEmitFile(
        pass, dest, nullptr,
        llvm::CodeGenFileType::ObjectFile
    );
    pass.run(*module);
}
```

### Linking Process

**Location**: `src/main.cpp:compile_file()`

```cpp
bool compile_file(const std::string& input_file,
                  const std::string& output_file) {
    // 1. Parse and compile to LLVM IR
    // 2. Emit object file to temp directory
    // 3. Find runtime library next to anvil executable
    // 4. Link: compiler temp.o runtime.a -o temp_exe
    // 5. Copy temp_exe to user's output location
    // 6. Make executable (Unix only)
    // 7. Cleanup temp directory
}
```

### Runtime Library

**Location**: `src/runtime.c`

Minimal runtime that:
1. Initializes execution context (stacks)
2. Calls `__anvil_main()` (the compiled Forth program)
3. Returns exit code

```c
typedef struct {
    int64_t data_stack[1024];
    int64_t return_stack[1024];
    size_t dsp;
    size_t rsp;
} ExecutionContext;

extern void __anvil_main(ExecutionContext* ctx);

int main(int argc, char** argv) {
    ExecutionContext ctx;
    ctx.dsp = 0;
    ctx.rsp = 0;
    __anvil_main(&ctx);
    return 0;
}
```

### Entry Point

The Forth program's main function is renamed to `__anvil_main`:

```cpp
// In compile_file():
llvm::Function* func = compiler.compile(ast.get());
func->setName("__anvil_main");  // Runtime expects this name
```

## Binary Size

Compiled executables are very small:

```bash
$ ./anvil --compile hello.fth -o hello
$ ls -lh hello
-rwxr-xr-x  1 user  wheel  33K  Oct 30 14:20 hello
```

- **33KB** for typical programs
- **Well under 50KB requirement**
- No runtime dependencies beyond libc
- Statically linked runtime

## Optimization Levels

Currently uses `llvm::CodeGenOptLevel::Aggressive` for:
- Maximum speed
- Minimal size
- All LLVM optimizations enabled

Future: could expose `-O0` through `-O3` flags.

## Limitations and Future Work

### Current Limitations

1. **No dynamic loading**: Compiled programs can't load new Forth code at runtime
2. **No REPL in AOT**: Interactive mode only available with interpreter/JIT
3. **Static dictionary**: All words must be defined at compile time
4. **No dynamic code generation**: Can't use `EVALUATE` or compile strings at runtime

### Future Enhancements

1. **Optimization flags**:
   ```bash
   ./anvil --compile -O0 debug.fth    # Fast compile, debug info
   ./anvil --compile -O3 prod.fth     # Maximum optimization
   ```

2. **Static linking options**:
   ```bash
   ./anvil --compile --static prog.fth  # Fully static binary
   ```

3. **Cross-compilation**:
   ```bash
   ./anvil --compile --target=aarch64-linux prog.fth
   ```

4. **LTO (Link-Time Optimization)**:
   ```bash
   ./anvil --compile --lto prog.fth   # Enable LTO
   ```

5. **Strip symbols**:
   ```bash
   ./anvil --compile --strip prog.fth  # Smaller binary
   ```

## Debugging AOT Compiled Programs

### Emit LLVM IR

```bash
# View the LLVM IR before compilation
./anvil --emit-llvm program.fth -o program.ll
```

### Use Standard Debuggers

AOT-compiled programs work with standard debuggers:
```bash
# macOS
lldb ./program

# Linux
gdb ./program
```

## Comparison with JIT

| Feature | AOT | JIT |
|---------|-----|-----|
| Startup time | Instant | Fast (lazy compile) |
| Runtime performance | Maximum | Maximum |
| Binary size | 33KB | N/A (in-memory) |
| Compilation time | Once (ahead) | On first call |
| Interactive REPL | No | Yes |
| Dynamic code | No | Yes |
| Portability | Binary per platform | Source is portable |

## See Also

- [Execution Modes](execution-modes.md) - Comparison of Interpreter/JIT/AOT
- [Architecture](architecture.md) - Overall system design
- [Compiler Documentation](compiler.md) - LLVM IR generation
