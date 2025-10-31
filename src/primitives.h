#ifndef ANVIL_PRIMITIVES_H
#define ANVIL_PRIMITIVES_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

namespace anvil {

// ============================================================================
// Helper functions for common stack operations
// ============================================================================

// Load value at given depth from stack
// depth 0 = top of stack (dsp-1), depth 1 = second element (dsp-2), etc.
inline llvm::Value *load_stack_at_depth(llvm::IRBuilder<> &builder,
                                        llvm::Value *stack_ptr,
                                        llvm::Value *dsp_ptr, int depth) {
  // Load current dsp
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");

  // Calculate index: dsp - (depth + 1)
  llvm::Value *idx = builder.CreateSub(dsp, builder.getInt64(depth + 1), "idx");

  // GEP and load
  llvm::Value *elem_ptr =
      builder.CreateGEP(builder.getInt64Ty(), stack_ptr, idx, "elem_ptr");
  return builder.CreateLoad(builder.getInt64Ty(), elem_ptr, "value");
}

// Store value at given depth on stack
// depth 0 = top of stack (dsp-1), depth 1 = second element (dsp-2), etc.
inline void store_stack_at_depth(llvm::IRBuilder<> &builder,
                                 llvm::Value *stack_ptr, llvm::Value *dsp_ptr,
                                 int depth, llvm::Value *value) {
  // Load current dsp
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");

  // Calculate index: dsp - (depth + 1)
  llvm::Value *idx = builder.CreateSub(dsp, builder.getInt64(depth + 1), "idx");

  // GEP and store
  llvm::Value *elem_ptr =
      builder.CreateGEP(builder.getInt64Ty(), stack_ptr, idx, "elem_ptr");
  builder.CreateStore(value, elem_ptr);
}

// Adjust stack pointer by delta (positive = push, negative = pop)
inline void adjust_dsp(llvm::IRBuilder<> &builder, llvm::Value *dsp_ptr,
                       int delta) {
  // Load current dsp
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");

  // Add delta
  llvm::Value *new_dsp =
      builder.CreateAdd(dsp, builder.getInt64(delta), "new_dsp");

  // Store back
  builder.CreateStore(new_dsp, dsp_ptr);
}

// ============================================================================
// Primitive word implementations
// ============================================================================

// Emit LLVM IR for the + primitive
// Stack effect: ( n1 n2 -- n1+n2 )
// Pops two values, adds them, pushes result
inline void emit_add(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Add them
  llvm::Value *result = builder.CreateAdd(n1, n2, "sum");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the - primitive
// Stack effect: ( n1 n2 -- n1-n2 )
// Pops two values, subtracts them, pushes result
inline void emit_sub(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Subtract them
  llvm::Value *result = builder.CreateSub(n1, n2, "difference");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the * primitive
// Stack effect: ( n1 n2 -- n1*n2 )
// Pops two values, multiplies them, pushes result
inline void emit_mul(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Multiply them
  llvm::Value *result = builder.CreateMul(n1, n2, "product");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the /MOD primitive
// Stack effect: ( n1 n2 -- remainder quotient )
// Pops two values, divides them, pushes remainder then quotient
inline void emit_divmod(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top (divisor)
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second (dividend)

  // Compute quotient and remainder (signed)
  llvm::Value *quotient = builder.CreateSDiv(n1, n2, "quotient");
  llvm::Value *remainder = builder.CreateSRem(n1, n2, "remainder");

  // Stack grows by 0 (2 consumed, 2 produced)
  // No adjustment needed to dsp

  // Store remainder at depth 1 (where n1 was)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, remainder);

  // Store quotient at depth 0 (where n2 was, now top)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, quotient);
}

// Emit LLVM IR for the DUP primitive
// Stack effect: ( n -- n n )
// Duplicates the top stack value
inline void emit_dup(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top value
  llvm::Value *top = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Push one value (net +1)
  adjust_dsp(builder, dsp_ptr, 1);

  // Store the duplicated value at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, top);
}

// Emit LLVM IR for the SWAP primitive
// Stack effect: ( n1 n2 -- n2 n1 )
// Swaps the top two stack values
inline void emit_swap(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Store them swapped (no stack pointer change)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, n1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, n2);
}

// Emit LLVM IR for the DROP primitive
// Stack effect: ( n -- )
// Removes the top stack value
inline void emit_drop(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  // Just decrement stack pointer (net -1)
  adjust_dsp(builder, dsp_ptr, -1);
}

// Emit LLVM IR for the OVER primitive
// Stack effect: ( n1 n2 -- n1 n2 n1 )
// Copies the second stack value to the top
inline void emit_over(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  // Load second value (depth 1)
  llvm::Value *n1 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Push one value (net +1)
  adjust_dsp(builder, dsp_ptr, 1);

  // Store the copied value at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, n1);
}

// Emit LLVM IR for the ROT primitive
// Stack effect: ( n1 n2 n3 -- n2 n3 n1 )
// Rotates the top three stack values
inline void emit_rot(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top three values
  llvm::Value *n3 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2); // Third

  // Rotate: n1 n2 n3 -> n2 n3 n1
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, n1); // n1 to top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, n3); // n3 to second
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2, n2); // n2 to third
}

// Emit LLVM IR for the AND primitive
// Stack effect: ( n1 n2 -- n1&n2 )
// Bitwise AND of top two values
inline void emit_and(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Bitwise AND
  llvm::Value *result = builder.CreateAnd(n1, n2, "and");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the OR primitive
// Stack effect: ( n1 n2 -- n1|n2 )
// Bitwise OR of top two values
inline void emit_or(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                    llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Bitwise OR
  llvm::Value *result = builder.CreateOr(n1, n2, "or");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the XOR primitive
// Stack effect: ( n1 n2 -- n1^n2 )
// Bitwise XOR of top two values
inline void emit_xor(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Bitwise XOR
  llvm::Value *result = builder.CreateXor(n1, n2, "xor");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the INVERT primitive
// Stack effect: ( n -- ~n )
// Bitwise NOT (one's complement) of top value
inline void emit_invert(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load top value
  llvm::Value *n = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Bitwise NOT
  llvm::Value *result = builder.CreateNot(n, "invert");

  // Store result at top (no stack pointer change)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the < primitive
// Stack effect: ( n1 n2 -- flag )
// Compares n1 < n2, returns -1 (true) or 0 (false)
inline void emit_lt(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                    llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Compare: n1 < n2 (signed comparison)
  llvm::Value *cmp = builder.CreateICmpSLT(n1, n2, "lt_cmp");

  // Convert i1 to int64: true -> -1, false -> 0 (Forth convention)
  llvm::Value *result = builder.CreateSExt(cmp, builder.getInt64Ty(), "lt_result");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the = primitive (equals comparison)
// Stack effect: ( n1 n2 -- flag )
// Compares two values: true if n1 == n2, false otherwise
inline void emit_eq(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                    llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *n2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *n1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Compare: n1 == n2
  llvm::Value *cmp = builder.CreateICmpEQ(n1, n2, "eq_cmp");

  // Convert i1 to int64: true -> -1, false -> 0 (Forth convention)
  llvm::Value *result = builder.CreateSExt(cmp, builder.getInt64Ty(), "eq_result");

  // Pop one value (2 consumed, 1 produced = net -1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store result at new top
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result);
}

// Emit LLVM IR for the @ primitive (fetch)
// Stack effect: ( addr -- value )
// Reads a 64-bit value from memory address
inline void emit_fetch(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load address from top of stack
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Convert int64 address to pointer (opaque pointer)
  llvm::Value *ptr = builder.CreateIntToPtr(addr,
                                            llvm::PointerType::get(builder.getContext(), 0),
                                            "fetch_ptr");

  // Load value from memory
  llvm::Value *value = builder.CreateLoad(builder.getInt64Ty(), ptr, "fetched_value");

  // Store value at top (replacing address, no stack pointer change)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, value);
}

// Emit LLVM IR for the ! primitive (store)
// Stack effect: ( value addr -- )
// Writes a 64-bit value to memory address
inline void emit_store(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load addr (top) and value (second)
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *value = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Convert int64 address to pointer (opaque pointer)
  llvm::Value *ptr = builder.CreateIntToPtr(addr,
                                            llvm::PointerType::get(builder.getContext(), 0),
                                            "store_ptr");

  // Store value to memory
  builder.CreateStore(value, ptr);

  // Pop both values (2 consumed, 0 produced = net -2)
  adjust_dsp(builder, dsp_ptr, -2);
}

// Emit LLVM IR for the C@ primitive (char fetch)
// Stack effect: ( addr -- byte )
// Reads an 8-bit byte from memory address
inline void emit_cfetch(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load address from top of stack
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Convert int64 address to pointer (opaque pointer)
  llvm::Value *ptr = builder.CreateIntToPtr(addr,
                                            llvm::PointerType::get(builder.getContext(), 0),
                                            "cfetch_ptr");

  // Load byte from memory
  llvm::Value *byte = builder.CreateLoad(builder.getInt8Ty(), ptr, "fetched_byte");

  // Zero-extend byte to int64
  llvm::Value *value = builder.CreateZExt(byte, builder.getInt64Ty(), "byte_extended");

  // Store value at top (replacing address, no stack pointer change)
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, value);
}

// Emit LLVM IR for the C! primitive (char store)
// Stack effect: ( byte addr -- )
// Writes an 8-bit byte to memory address
inline void emit_cstore(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr, llvm::Value *dsp_ptr) {
  // Load addr (top) and value (second)
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *value = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Convert int64 address to pointer (opaque pointer)
  llvm::Value *ptr = builder.CreateIntToPtr(addr,
                                            llvm::PointerType::get(builder.getContext(), 0),
                                            "cstore_ptr");

  // Truncate value to byte
  llvm::Value *byte = builder.CreateTrunc(value, builder.getInt8Ty(), "value_byte");

  // Store byte to memory
  builder.CreateStore(byte, ptr);

  // Pop both values (2 consumed, 0 produced = net -2)
  adjust_dsp(builder, dsp_ptr, -2);
}

// ============================================================================
// Return stack operations
// ============================================================================

// Emit LLVM IR for the >R primitive (to-r)
// Stack effect: ( n -- ) (R: -- n)
// Moves value from data stack to return stack
inline void emit_to_r(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                      llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                      llvm::Value *rsp_ptr) {
  // Load value from top of data stack
  llvm::Value *value = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Pop from data stack
  adjust_dsp(builder, dsp_ptr, -1);

  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Push to return stack: return_stack[rsp] = value, rsp++
  llvm::Value *elem_ptr = builder.CreateGEP(builder.getInt64Ty(),
                                             return_stack_ptr, rsp, "rstack_elem_ptr");
  builder.CreateStore(value, elem_ptr);

  // Increment return stack pointer
  llvm::Value *new_rsp = builder.CreateAdd(rsp, builder.getInt64(1), "new_rsp");
  builder.CreateStore(new_rsp, rsp_ptr);
}

// Emit LLVM IR for the R> primitive (r-from)
// Stack effect: ( -- n ) (R: n -- )
// Moves value from return stack to data stack
inline void emit_from_r(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr,
                        llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                        llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Decrement return stack pointer (pop)
  llvm::Value *new_rsp = builder.CreateSub(rsp, builder.getInt64(1), "new_rsp");
  builder.CreateStore(new_rsp, rsp_ptr);

  // Load value from return stack: return_stack[new_rsp]
  llvm::Value *elem_ptr = builder.CreateGEP(builder.getInt64Ty(),
                                             return_stack_ptr, new_rsp, "rstack_elem_ptr");
  llvm::Value *value = builder.CreateLoad(builder.getInt64Ty(), elem_ptr, "rstack_value");

  // Push to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, value);
}

// Emit LLVM IR for the R@ primitive (r-fetch)
// Stack effect: ( -- n ) (R: n -- n)
// Copies top of return stack to data stack without removing it
inline void emit_r_fetch(llvm::IRBuilder<> &builder,
                         llvm::Value *data_stack_ptr,
                         llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                         llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Load value from top of return stack: return_stack[rsp-1]
  llvm::Value *idx = builder.CreateSub(rsp, builder.getInt64(1), "r_top_idx");
  llvm::Value *elem_ptr = builder.CreateGEP(builder.getInt64Ty(),
                                             return_stack_ptr, idx, "rstack_elem_ptr");
  llvm::Value *value = builder.CreateLoad(builder.getInt64Ty(), elem_ptr, "rstack_value");

  // Push to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, value);
}

// Emit LLVM IR for the 2>R primitive (two-to-r)
// Stack effect: ( n1 n2 -- ) (R: -- n1 n2)
// Moves two values from data stack to return stack
inline void emit_two_to_r(llvm::IRBuilder<> &builder,
                          llvm::Value *data_stack_ptr,
                          llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                          llvm::Value *rsp_ptr) {
  // Load two values from data stack (n2 is on top, n1 is second)
  llvm::Value *n2 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *n1 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Pop both from data stack
  adjust_dsp(builder, dsp_ptr, -2);

  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Push n1 first, then n2 to return stack
  llvm::Value *elem_ptr1 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, rsp, "rstack_elem_ptr1");
  builder.CreateStore(n1, elem_ptr1);

  llvm::Value *rsp_plus_1 = builder.CreateAdd(rsp, builder.getInt64(1), "rsp_plus_1");
  llvm::Value *elem_ptr2 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, rsp_plus_1, "rstack_elem_ptr2");
  builder.CreateStore(n2, elem_ptr2);

  // Increment return stack pointer by 2
  llvm::Value *new_rsp = builder.CreateAdd(rsp, builder.getInt64(2), "new_rsp");
  builder.CreateStore(new_rsp, rsp_ptr);
}

// Emit LLVM IR for the 2R> primitive (two-r-from)
// Stack effect: ( -- n1 n2 ) (R: n1 n2 -- )
// Moves two values from return stack to data stack
inline void emit_two_from_r(llvm::IRBuilder<> &builder,
                            llvm::Value *data_stack_ptr,
                            llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                            llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Decrement return stack pointer by 2
  llvm::Value *new_rsp = builder.CreateSub(rsp, builder.getInt64(2), "new_rsp");
  builder.CreateStore(new_rsp, rsp_ptr);

  // Load two values from return stack (n1 is at [new_rsp], n2 is at [new_rsp+1])
  llvm::Value *elem_ptr1 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, new_rsp, "rstack_elem_ptr1");
  llvm::Value *n1 = builder.CreateLoad(builder.getInt64Ty(), elem_ptr1, "n1");

  llvm::Value *idx2 = builder.CreateAdd(new_rsp, builder.getInt64(1), "idx2");
  llvm::Value *elem_ptr2 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, idx2, "rstack_elem_ptr2");
  llvm::Value *n2 = builder.CreateLoad(builder.getInt64Ty(), elem_ptr2, "n2");

  // Push both to data stack (n1 first, then n2, so n2 ends up on top)
  adjust_dsp(builder, dsp_ptr, 2);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, n1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, n2);
}

// Emit LLVM IR for the 2R@ primitive (two-r-fetch)
// Stack effect: ( -- n1 n2 ) (R: n1 n2 -- n1 n2)
// Copies top two values from return stack to data stack without removing them
inline void emit_two_r_fetch(llvm::IRBuilder<> &builder,
                              llvm::Value *data_stack_ptr,
                              llvm::Value *return_stack_ptr, llvm::Value *dsp_ptr,
                              llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Load two values from top of return stack
  // n1 is at [rsp-2], n2 is at [rsp-1]
  llvm::Value *idx1 = builder.CreateSub(rsp, builder.getInt64(2), "idx1");
  llvm::Value *elem_ptr1 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, idx1, "rstack_elem_ptr1");
  llvm::Value *n1 = builder.CreateLoad(builder.getInt64Ty(), elem_ptr1, "n1");

  llvm::Value *idx2 = builder.CreateSub(rsp, builder.getInt64(1), "idx2");
  llvm::Value *elem_ptr2 = builder.CreateGEP(builder.getInt64Ty(),
                                              return_stack_ptr, idx2, "rstack_elem_ptr2");
  llvm::Value *n2 = builder.CreateLoad(builder.getInt64Ty(), elem_ptr2, "n2");

  // Push both to data stack (n1 first, then n2)
  adjust_dsp(builder, dsp_ptr, 2);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, n1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, n2);
}

// ============================================================================
// Control flow primitives
// ============================================================================

// Emit LLVM IR for the BRANCH primitive (unconditional branch)
// Stack effect: ( -- )
// Branches unconditionally to the target basic block
inline void emit_branch(llvm::IRBuilder<> &builder,
                        llvm::BasicBlock *target_block) {
  builder.CreateBr(target_block);
}

// Emit LLVM IR for the 0BRANCH primitive (conditional branch if zero)
// Stack effect: ( flag -- )
// Pops a value from the stack; branches to target if zero, falls through otherwise
inline void emit_0branch(llvm::IRBuilder<> &builder,
                         llvm::Value *data_stack_ptr,
                         llvm::Value *dsp_ptr,
                         llvm::BasicBlock *target_block,
                         llvm::BasicBlock *fallthrough_block) {
  // Load value from top of stack
  llvm::Value *flag = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Pop the value
  adjust_dsp(builder, dsp_ptr, -1);

  // Compare flag to zero
  llvm::Value *is_zero = builder.CreateICmpEQ(flag, builder.getInt64(0), "is_zero");

  // Branch: if zero, go to target; otherwise, fall through
  builder.CreateCondBr(is_zero, target_block, fallthrough_block);
}

// Emit LLVM IR for the EXIT primitive (return from word)
// Stack effect: ( -- )
// Returns from the current function (word)
inline void emit_exit(llvm::IRBuilder<> &builder) {
  builder.CreateRetVoid();
}

// Emit LLVM IR for the LIT primitive (literal)
// Stack effect: ( -- n )
// Pushes a literal value onto the data stack
inline void emit_lit(llvm::IRBuilder<> &builder,
                     llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr,
                     int64_t literal_value) {
  // Increase stack pointer first
  adjust_dsp(builder, dsp_ptr, 1);

  // Store the literal value at the new top of stack
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0,
                       builder.getInt64(literal_value));
}

// Emit LLVM IR for the EXECUTE primitive
// Stack effect: ( xt -- )
// Executes the execution token (function pointer) on top of the stack
// The xt is expected to be a function pointer with signature:
//   void (*)(ExecutionContext*)
inline void emit_execute(llvm::IRBuilder<> &builder,
                         llvm::Value *data_stack_ptr,
                         llvm::Value *dsp_ptr,
                         llvm::Value *ctx_ptr) {
  // Load the execution token (function pointer as int64) from top of stack
  llvm::Value *xt_int = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Pop the execution token
  adjust_dsp(builder, dsp_ptr, -1);

  // Convert int64 to function pointer
  // Function signature: void (*)(ExecutionContext*)
  llvm::Type *ctx_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::FunctionType *func_type = llvm::FunctionType::get(
      builder.getVoidTy(),
      {ctx_ptr_type},
      false
  );
  llvm::Type *func_ptr_type = llvm::PointerType::get(builder.getContext(), 0);

  // Convert the int64 to function pointer
  llvm::Value *func_ptr = builder.CreateIntToPtr(xt_int, func_ptr_type, "func_ptr");

  // Call the function, passing the ExecutionContext pointer
  builder.CreateCall(func_type, func_ptr, {ctx_ptr});
}

// ============================================================================
// String/IO primitives
// ============================================================================

// Emit LLVM IR for the TYPE primitive
// Stack effect: ( addr len -- )
// Prints a string to stdout
inline void emit_type(llvm::IRBuilder<> &builder,
                      llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  // Load length and address from stack
  llvm::Value *len = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Pop both values
  adjust_dsp(builder, dsp_ptr, -2);

  // Convert int64 address to pointer
  llvm::Value *str_ptr = builder.CreateIntToPtr(
      addr,
      llvm::PointerType::get(builder.getContext(), 0),
      "str_ptr"
  );

  // Declare write function: ssize_t write(int fd, const void *buf, size_t count)
  llvm::FunctionType *write_type = llvm::FunctionType::get(
      builder.getInt64Ty(),
      {builder.getInt32Ty(), llvm::PointerType::get(builder.getContext(), 0), builder.getInt64Ty()},
      false
  );

  llvm::FunctionCallee write_func = builder.GetInsertBlock()->getModule()->getOrInsertFunction(
      "write",
      write_type
  );

  // Call write(1, str_ptr, len) - 1 is stdout
  builder.CreateCall(write_func, {builder.getInt32(1), str_ptr, len});
}

// ============================================================================
// Loop control primitives
// ============================================================================

// Emit LLVM IR for the I primitive (loop index)
// Stack effect: ( -- n )
// Pushes the current loop index onto the data stack
// The loop index is stored on the return stack at rsp-1 (limit at rsp-2, index at rsp-1)
inline void emit_i(llvm::IRBuilder<> &builder,
                   llvm::Value *data_stack_ptr,
                   llvm::Value *return_stack_ptr,
                   llvm::Value *dsp_ptr,
                   llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Load loop index from return stack at [rsp-1]
  // Return stack layout: [..., limit, index]
  llvm::Value *idx = builder.CreateSub(rsp, builder.getInt64(1), "idx");
  llvm::Value *elem_ptr = builder.CreateGEP(builder.getInt64Ty(),
                                             return_stack_ptr, idx, "rstack_elem_ptr");
  llvm::Value *loop_index = builder.CreateLoad(builder.getInt64Ty(), elem_ptr, "loop_index");

  // Push to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, loop_index);
}

// Emit LLVM IR for the . primitive (dot - print and pop)
// Stack effect: ( n -- )
// Prints the top of stack as a number and removes it
inline void emit_dot(llvm::IRBuilder<> &builder,
                     llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();
  llvm::Function *current_function = builder.GetInsertBlock()->getParent();

  // Get printf function
  llvm::FunctionType *printf_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {llvm::PointerType::get(builder.getContext(), 0)},
      true); // variadic
  llvm::FunctionCallee printf_func = module->getOrInsertFunction("printf", printf_type);

  // Check for stack underflow
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
  llvm::Value *is_empty = builder.CreateICmpEQ(dsp, builder.getInt64(0), "is_empty");

  llvm::BasicBlock *underflow_block = llvm::BasicBlock::Create(
      builder.getContext(), "underflow", current_function);
  llvm::BasicBlock *normal_block = llvm::BasicBlock::Create(
      builder.getContext(), "normal", current_function);

  builder.CreateCondBr(is_empty, underflow_block, normal_block);

  // Underflow block - print error and return
  builder.SetInsertPoint(underflow_block);

  // Get or create underflow error string (shared)
  llvm::GlobalVariable *error_global = module->getGlobalVariable(".underflow_msg", true);
  if (!error_global) {
    llvm::Constant *error_str = llvm::ConstantDataArray::getString(
        builder.getContext(), "Stack underflow\n", false);
    error_global = new llvm::GlobalVariable(
        *module, error_str->getType(), true,
        llvm::GlobalValue::PrivateLinkage, error_str, ".underflow_msg");
  }
  llvm::Value *error_ptr = builder.CreateBitCast(error_global,
      llvm::PointerType::get(builder.getContext(), 0), "error_ptr");
  builder.CreateCall(printf_func, {error_ptr});

  // Just return without exiting - let REPL continue
  builder.CreateRetVoid();

  // Normal block - pop and print
  builder.SetInsertPoint(normal_block);
  llvm::Value *value = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  adjust_dsp(builder, dsp_ptr, -1);

  // Get or create format string "%lld " (shared across all . calls)
  llvm::GlobalVariable *format_global = module->getGlobalVariable(".dot_fmt", true);
  if (!format_global) {
    llvm::Constant *format_str = llvm::ConstantDataArray::getString(
        builder.getContext(), "%lld ", false);
    format_global = new llvm::GlobalVariable(
        *module, format_str->getType(), true,
        llvm::GlobalValue::PrivateLinkage, format_str, ".dot_fmt");
  }
  llvm::Value *format_ptr = builder.CreateBitCast(format_global,
      llvm::PointerType::get(builder.getContext(), 0), "format_ptr");

  // Call printf
  builder.CreateCall(printf_func, {format_ptr, value});

  // Flush stdout to ensure output appears immediately
  llvm::FunctionType *fflush_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {llvm::PointerType::get(builder.getContext(), 0)},
      false);
  llvm::FunctionCallee fflush_func = module->getOrInsertFunction("fflush", fflush_type);
  builder.CreateCall(fflush_func, {llvm::ConstantPointerNull::get(
      llvm::PointerType::get(builder.getContext(), 0))});

  // Continue in normal_block - next primitive will add instructions here
}

// Emit LLVM IR for the EMIT primitive
// Stack effect: ( c -- )
// Pops a character code and outputs the character
inline void emit_emit(llvm::IRBuilder<> &builder,
                      llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Get putchar function (more efficient than printf for single char)
  llvm::FunctionType *putchar_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {builder.getInt32Ty()},
      false);
  llvm::FunctionCallee putchar_func = module->getOrInsertFunction("putchar", putchar_type);

  // Pop character code from stack
  llvm::Value *char_code = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  adjust_dsp(builder, dsp_ptr, -1);

  // Truncate to 32-bit int for putchar (it takes int, not char)
  llvm::Value *char_code_i32 = builder.CreateTrunc(char_code, builder.getInt32Ty(), "char_i32");

  // Call putchar
  builder.CreateCall(putchar_func, {char_code_i32});

  // Flush stdout to ensure output appears immediately
  llvm::FunctionType *fflush_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {llvm::PointerType::get(builder.getContext(), 0)},
      false);
  llvm::FunctionCallee fflush_func = module->getOrInsertFunction("fflush", fflush_type);
  builder.CreateCall(fflush_func, {llvm::ConstantPointerNull::get(
      llvm::PointerType::get(builder.getContext(), 0))});
}

// Emit LLVM IR for the CR primitive (carriage return)
// Stack effect: ( -- )
// Prints a newline character
inline void emit_cr(llvm::IRBuilder<> &builder) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Get putchar function
  llvm::FunctionType *putchar_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {builder.getInt32Ty()},
      false);
  llvm::FunctionCallee putchar_func = module->getOrInsertFunction("putchar", putchar_type);

  // Call putchar('\n') - newline is ASCII 10
  builder.CreateCall(putchar_func, {builder.getInt32(10)});

  // Flush stdout to ensure output appears immediately
  llvm::FunctionType *fflush_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {llvm::PointerType::get(builder.getContext(), 0)},
      false);
  llvm::FunctionCallee fflush_func = module->getOrInsertFunction("fflush", fflush_type);
  builder.CreateCall(fflush_func, {llvm::ConstantPointerNull::get(
      llvm::PointerType::get(builder.getContext(), 0))});
}

// Emit LLVM IR for the J primitive (outer loop index)
// Stack effect: ( -- n )
// Pushes the outer loop index onto the data stack (for nested loops)
// The outer loop index is at rsp-3 (outer_limit, outer_index, inner_limit, inner_index)
inline void emit_j(llvm::IRBuilder<> &builder,
                   llvm::Value *data_stack_ptr,
                   llvm::Value *return_stack_ptr,
                   llvm::Value *dsp_ptr,
                   llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Load outer loop index from return stack at [rsp-3]
  // Return stack layout: [..., outer_limit, outer_index, inner_limit, inner_index]
  llvm::Value *idx = builder.CreateSub(rsp, builder.getInt64(3), "idx");
  llvm::Value *elem_ptr = builder.CreateGEP(builder.getInt64Ty(),
                                             return_stack_ptr, idx, "rstack_elem_ptr");
  llvm::Value *outer_loop_index = builder.CreateLoad(builder.getInt64Ty(), elem_ptr, "outer_loop_index");

  // Push to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, outer_loop_index);
}

// Emit LLVM IR for the HERE primitive
// Stack effect: ( -- addr )
// Pushes the address of the next free byte in data space
inline void emit_here(llvm::IRBuilder<> &builder,
                      llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr,
                      llvm::Value *data_space_ptr,
                      llvm::Value *here_ptr) {
  // Load current HERE value
  llvm::Value *here = builder.CreateLoad(builder.getInt64Ty(), here_ptr, "here");

  // Calculate absolute address: data_space_ptr + here
  llvm::Type *i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
  llvm::Value *addr_ptr = builder.CreateGEP(builder.getInt8Ty(), data_space_i8, here, "addr_ptr");
  llvm::Value *addr = builder.CreatePtrToInt(addr_ptr, builder.getInt64Ty(), "addr");

  // Push address to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr);
}

// Emit LLVM IR for the ALLOT primitive
// Stack effect: ( n -- )
// Allocates n bytes in data space
inline void emit_allot(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr,
                       llvm::Value *dsp_ptr,
                       llvm::Value *here_ptr) {
  // Pop n from stack
  llvm::Value *n = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  adjust_dsp(builder, dsp_ptr, -1);

  // Load current HERE
  llvm::Value *here = builder.CreateLoad(builder.getInt64Ty(), here_ptr, "here");

  // Add n to HERE
  llvm::Value *new_here = builder.CreateAdd(here, n, "new_here");

  // Store new HERE
  builder.CreateStore(new_here, here_ptr);
}

// Emit LLVM IR for the , (comma) primitive
// Stack effect: ( n -- )
// Stores n at HERE and advances HERE by one cell (8 bytes)
inline void emit_comma(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr,
                       llvm::Value *dsp_ptr,
                       llvm::Value *data_space_ptr,
                       llvm::Value *here_ptr) {
  // Pop value from stack
  llvm::Value *value = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  adjust_dsp(builder, dsp_ptr, -1);

  // Load current HERE
  llvm::Value *here = builder.CreateLoad(builder.getInt64Ty(), here_ptr, "here");

  // Calculate address: data_space_ptr + here
  llvm::Type *i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
  llvm::Value *addr_ptr = builder.CreateGEP(builder.getInt8Ty(), data_space_i8, here, "addr_ptr");

  // Cast to i64* for storing
  llvm::Type *i64_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *addr_i64 = builder.CreateBitCast(addr_ptr, i64_ptr_type, "addr_i64");

  // Store value at address
  builder.CreateStore(value, addr_i64);

  // Advance HERE by 8 bytes (one cell)
  llvm::Value *new_here = builder.CreateAdd(here, builder.getInt64(8), "new_here");
  builder.CreateStore(new_here, here_ptr);
}

// ============================================================================
// Terminal I/O primitives
// ============================================================================

// Emit LLVM IR for the KEY primitive
// Stack effect: ( -- char )
// Reads a single character from stdin (blocking)
inline void emit_key(llvm::IRBuilder<> &builder,
                     llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Get getchar function
  llvm::FunctionType *getchar_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {},
      false);
  llvm::FunctionCallee getchar_func = module->getOrInsertFunction("getchar", getchar_type);

  // Call getchar to read character
  llvm::Value *char_val = builder.CreateCall(getchar_func);

  // Convert to 64-bit for stack
  llvm::Value *char_val_i64 = builder.CreateSExt(char_val, builder.getInt64Ty(), "char_i64");

  // Push onto data stack
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
  llvm::Value *stack_top = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp, "stack_top");
  builder.CreateStore(char_val_i64, stack_top);
  llvm::Value *new_dsp = builder.CreateAdd(dsp, builder.getInt64(1), "new_dsp");
  builder.CreateStore(new_dsp, dsp_ptr);
}

// Emit LLVM IR for the KEY? primitive
// Stack effect: ( -- flag )
// Returns true if a character is available to read
inline void emit_key_question(llvm::IRBuilder<> &builder,
                               llvm::Value *data_stack_ptr,
                               llvm::Value *dsp_ptr) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Declare anvil_key_available() from runtime
  llvm::FunctionType *key_available_type = llvm::FunctionType::get(
      builder.getInt32Ty(),
      {},
      false);
  llvm::FunctionCallee key_available_func =
      module->getOrInsertFunction("anvil_key_available", key_available_type);

  // Call anvil_key_available
  llvm::Value *available = builder.CreateCall(key_available_func);

  // Convert to boolean: 0 = false, -1 = true (Forth convention)
  llvm::Value *is_zero = builder.CreateICmpEQ(available, builder.getInt32(0), "is_zero");
  llvm::Value *flag = builder.CreateSelect(
      is_zero,
      builder.getInt64(0),   // False
      builder.getInt64(-1),  // True
      "flag");

  // Push onto data stack
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
  llvm::Value *stack_top = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp, "stack_top");
  builder.CreateStore(flag, stack_top);
  llvm::Value *new_dsp = builder.CreateAdd(dsp, builder.getInt64(1), "new_dsp");
  builder.CreateStore(new_dsp, dsp_ptr);
}

// Emit LLVM IR for the RAW-MODE primitive
// Stack effect: ( -- )
// Switches terminal to raw mode (no echo, no buffering)
inline void emit_raw_mode(llvm::IRBuilder<> &builder) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Declare anvil_set_raw_mode() from runtime
  llvm::FunctionType *raw_mode_type = llvm::FunctionType::get(
      builder.getVoidTy(),
      {},
      false);
  llvm::FunctionCallee raw_mode_func =
      module->getOrInsertFunction("anvil_set_raw_mode", raw_mode_type);

  // Call anvil_set_raw_mode
  builder.CreateCall(raw_mode_func);
}

// Emit LLVM IR for the COOKED-MODE primitive
// Stack effect: ( -- )
// Restores normal terminal mode
inline void emit_cooked_mode(llvm::IRBuilder<> &builder) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Declare anvil_set_cooked_mode() from runtime
  llvm::FunctionType *cooked_mode_type = llvm::FunctionType::get(
      builder.getVoidTy(),
      {},
      false);
  llvm::FunctionCallee cooked_mode_func =
      module->getOrInsertFunction("anvil_set_cooked_mode", cooked_mode_type);

  // Call anvil_set_cooked_mode
  builder.CreateCall(cooked_mode_func);
}

// Emit LLVM IR for the EMIT-ESC primitive
// Stack effect: ( -- )
// Outputs ESC character (ASCII 27) for building ANSI escape sequences
inline void emit_emit_esc(llvm::IRBuilder<> &builder,
                          llvm::Value *data_stack_ptr,
                          llvm::Value *dsp_ptr) {
  // Simply push 27 (ESC) and call EMIT
  // This is equivalent to: 27 EMIT

  // Push 27 onto stack
  llvm::Value *dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
  llvm::Value *stack_top = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp, "stack_top");
  builder.CreateStore(builder.getInt64(27), stack_top);
  llvm::Value *new_dsp = builder.CreateAdd(dsp, builder.getInt64(1), "new_dsp");
  builder.CreateStore(new_dsp, dsp_ptr);

  // Call emit_emit to output it
  emit_emit(builder, data_stack_ptr, dsp_ptr);
}

} // namespace anvil

#endif // ANVIL_PRIMITIVES_H
