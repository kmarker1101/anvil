# IMMEDIATE Word Implementation Status

## Overview

IMMEDIATE words are Forth words that execute during compilation rather than being compiled into the word being defined. This is essential for implementing control flow words like IF, THEN, ELSE, BEGIN, UNTIL, etc. in Forth itself.

## Architecture

### Current Architecture
```
Lexer (Forth) → Parser (Rust) → Compiler (Rust) → Executor (Rust)
```

### Target Architecture for IMMEDIATE
```
Lexer (Forth) → Meta-Compiler (Forth) → LLVM/Bytecode → Executor (Rust)
                      ↓
                Executes IMMEDIATE words
                Compiles non-IMMEDIATE words
```

## What's Been Implemented

### 1. Compiler Infrastructure (✅ Complete)
- **File**: `src/compiler.rs`
- Added `immediate_words: HashSet<String>` to track immediate words
- Added `last_defined_word: Option<String>` to track most recently defined word
- Added `compilation_state: bool` for STATE variable
- Methods:
  - `mark_immediate()` - Mark last defined word as immediate
  - `is_immediate(word)` - Check if word is immediate
  - `get_state()` - Get compilation state
  - `get_last_defined_word()` - Get last defined word name

### 2. Primitives (✅ Complete)
- **File**: `src/primitives.rs`
- `IMMEDIATE` - Marks most recent definition as immediate (placeholder in VM)
- `STATE` - Returns address of compilation state variable
- `FIND-WORD` - Look up word in dictionary (placeholder)
- `EXECUTE` - Execute an execution token (placeholder)
- `IMMEDIATE?` - Check if word is immediate (placeholder)

### 3. LLVM Bindings (✅ Complete)
- **File**: `src/llvm_jit.rs`
- Added extern "C" functions for all new primitives
- Mapped primitives in `llvm_primitive_mappings!` macro

### 4. Meta-Compiler Framework (✅ Partial)
- **File**: `src/meta-compiler.fth`
- Token access functions (PEEK-TOKEN-TYPE, GET-TOKEN-DATA, etc.)
- Bytecode emission (EMIT-OP, EMIT-CELL, EMIT-LITERAL, etc.)
- Branch stack for control flow compilation
- Basic META-COMPILE-DEFINITION structure
- Placeholders for IMMEDIATE?, FIND-WORD, EXECUTE

### 5. Standard Library Extensions (✅ Documented)
- **File**: `src/stdlib.fth`
- Documented (but commented out) immediate control flow words:
  - `[` and `]` - Switch between interpret/compile modes
  - `IF`, `THEN`, `ELSE` - Conditional compilation
  - `BEGIN`, `UNTIL` - Loop compilation
  - `LITERAL` - Compile-time literal insertion

## What Needs To Be Implemented

### Critical Missing Pieces

#### 1. Compiler-VM Bridge (⚠️ HIGH PRIORITY)
**Problem**: The meta-compiler primitives (FIND-WORD, EXECUTE, IMMEDIATE?) need access to the Compiler instance, but primitives only have access to the VM.

**Solution Options**:
a) **Pass Compiler pointer to VM** - Add a `compiler: *mut Compiler` field to VM
b) **Callback mechanism** - VM holds function pointers that call back into Compiler
c) **Shared state** - Use a global/thread-local to share Compiler state with VM

**Recommended**: Option (a) - Pass Compiler pointer to VM

```rust
pub struct VM {
    // ... existing fields
    compiler: Option<*mut Compiler>,  // Set during compilation
}

impl VM {
    fn op_find_word(&mut self) -> Result<(), ForthError> {
        let len = self.data_stack.pop()? as usize;
        let addr = self.data_stack.pop()? as usize;

        if let Some(compiler_ptr) = self.compiler {
            unsafe {
                let compiler = &*compiler_ptr;
                let slice = std::slice::from_raw_parts(addr as *const u8, len);
                let word = std::str::from_utf8(slice).unwrap();

                // Look up word in compiler dictionary
                if let Some(compiled_word) = compiler.dictionary.get(word) {
                    self.data_stack.push(/* xt */ as i64);
                    self.data_stack.push(-1); // found
                } else {
                    self.data_stack.push(0);
                    self.data_stack.push(0); // not found
                }
            }
        }
        Ok(())
    }
}
```

#### 2. Execution Token (XT) System (⚠️ HIGH PRIORITY)
**Problem**: Need a way to represent and execute words dynamically.

**Solution**: Execution tokens should be addresses/indices into the dictionary.

```rust
// In Compiler
pub fn get_word_xt(&self, name: &str) -> Option<usize> {
    // Return an index or address that can be used to execute the word
}

pub fn execute_xt(&mut self, vm: &mut VM, xt: usize) -> Result<(), ForthError> {
    // Execute the word at the given xt
    // This may need to call into the Executor
}
```

#### 3. EXECUTE Primitive Implementation (⚠️ HIGH PRIORITY)
**Problem**: EXECUTE needs to actually run compiled Forth code.

**Solution**: Call into Executor with the word's instructions.

```rust
fn op_execute(&mut self) -> Result<(), ForthError> {
    let xt = self.data_stack.pop()? as usize;

    if let Some(compiler_ptr) = self.compiler {
        unsafe {
            let compiler = &*compiler_ptr;
            // Get the word's instructions and execute them
            // This is complex - may need Executor integration
        }
    }
    Ok(())
}
```

#### 4. Integration with Existing Parser (⚠️ MEDIUM PRIORITY)
**Problem**: Currently the Rust parser builds an AST. Need to integrate meta-compiler or replace parser.

**Options**:
a) **Hybrid approach**: Use Rust parser for base language, meta-compiler for user definitions
b) **Full replacement**: Replace Rust parser with meta-compiler
c) **Bootstrap**: Use Rust compiler to compile meta-compiler, then switch

**Recommended**: Option (a) initially, move to (b) over time

#### 5. STATE Variable Implementation (⚠️ MEDIUM PRIORITY)
**Problem**: STATE currently returns address 0, but need proper compilation state tracking.

**Solution**: Reserve memory location for STATE, update during compilation.

```rust
const STATE_ADDR: usize = 0;

// In Compiler::compile_definition
vm.memory[STATE_ADDR..STATE_ADDR+8].copy_from_slice(&1i64.to_le_bytes()); // Compiling

// After definition
vm.memory[STATE_ADDR..STATE_ADDR+8].copy_from_slice(&0i64.to_le_bytes()); // Interpreting
```

#### 6. Bytecode Emission from Forth (⚠️ LOW PRIORITY)
**Problem**: Meta-compiler needs to emit actual bytecode that Executor can run.

**Solution**: Meta-compiler should emit same Instruction enum as Rust compiler.
- May need to expose bytecode format to Forth
- Or compile to intermediate format that gets translated

## Testing Strategy

### Phase 1: Basic Infrastructure
1. Test IMMEDIATE marking works: `: FOO ; IMMEDIATE` marks FOO as immediate
2. Test STATE variable: `STATE @` returns correct value
3. Test FIND-WORD: Can look up words in dictionary

### Phase 2: Simple IMMEDIATE Word
1. Define a simple immediate word: `: [  0 STATE ! ; IMMEDIATE`
2. Verify it executes during compilation, not at runtime

### Phase 3: Control Flow
1. Implement IF/THEN in Forth
2. Test: `: ABS DUP 0< IF NEGATE THEN ;`
3. Verify compiled code works correctly

### Phase 4: Full Bootstrap
1. Compile entire meta-compiler in Forth
2. Use meta-compiler to compile new definitions
3. Verify LLVM compilation still works

## Next Steps (Priority Order)

1. **Implement Compiler-VM Bridge** - Add compiler pointer to VM
2. **Implement FIND-WORD properly** - Look up words in Compiler dictionary
3. **Implement basic XT system** - Use word names or indices as XTs
4. **Implement EXECUTE** - Execute words by XT
5. **Test basic IMMEDIATE word** - e.g., `[` and `]`
6. **Implement IMMEDIATE? properly** - Check Compiler's immediate_words set
7. **Integrate meta-compiler into compilation pipeline**
8. **Uncomment and test control flow words in stdlib**
9. **Test complex IMMEDIATE words** - IF/THEN/ELSE
10. **Full bootstrap** - Compile everything through meta-compiler

## Files Modified

- ✅ `src/compiler.rs` - Added IMMEDIATE tracking
- ✅ `src/primitives.rs` - Added IMMEDIATE, STATE, FIND-WORD, EXECUTE, IMMEDIATE? primitives
- ✅ `src/llvm_jit.rs` - Added LLVM bindings for new primitives
- ✅ `src/meta-compiler.fth` - Created meta-compiler framework
- ✅ `src/stdlib.fth` - Documented IMMEDIATE control flow words
- ✅ `IMMEDIATE_IMPLEMENTATION.md` - This document

## Notes

- The current implementation provides the **infrastructure** for IMMEDIATE words but doesn't fully connect the pieces
- The main missing piece is the **Compiler-VM bridge** - VM primitives need access to Compiler state
- Once the bridge is in place, FIND-WORD, EXECUTE, and IMMEDIATE? can be properly implemented
- The meta-compiler framework is in place and ready to use once the runtime support is complete
- This is a significant architectural change that enables true Forth-style extensibility
