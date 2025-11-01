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

// Emit LLVM IR for the U< primitive (unsigned less-than comparison)
// Stack effect: ( u1 u2 -- flag )
// Compares two values as unsigned: true if u1 < u2, false otherwise
inline void emit_ult(llvm::IRBuilder<> &builder, llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr) {
  // Load top two values
  llvm::Value *u2 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0); // Top
  llvm::Value *u1 =
      load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1); // Second

  // Compare: u1 < u2 (unsigned comparison)
  llvm::Value *cmp = builder.CreateICmpULT(u1, u2, "ult_cmp");

  // Convert i1 to int64: true -> -1, false -> 0 (Forth convention)
  llvm::Value *result = builder.CreateSExt(cmp, builder.getInt64Ty(), "ult_result");

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

// Emit LLVM IR for UNLOOP
// Stack effect: ( -- ) (R: n1 n2 -- )
// Discards loop control parameters from return stack
inline void emit_unloop(llvm::IRBuilder<> &builder,
                        llvm::Value *rsp_ptr) {
  // Load current return stack pointer
  llvm::Value *rsp = builder.CreateLoad(builder.getInt64Ty(), rsp_ptr, "rsp");

  // Decrement return stack pointer by 2 (discard index and limit)
  llvm::Value *new_rsp = builder.CreateSub(rsp, builder.getInt64(2), "new_rsp");
  builder.CreateStore(new_rsp, rsp_ptr);
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
                         llvm::Value *dsp_ptr) {
  // Load the execution token (function pointer as int64) from top of stack
  llvm::Value *xt_int = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Pop the execution token
  adjust_dsp(builder, dsp_ptr, -1);

  // Get the context pointer from the current function's first argument
  llvm::Function *current_func = builder.GetInsertBlock()->getParent();
  llvm::Value *ctx_ptr = current_func->getArg(0);

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

// Emit LLVM IR for the FIND primitive
// Stack effect: ( c-addr u -- xt flag )
// Searches dictionary for word, returns XT and flag (-1 if found, 0 if not)
inline void emit_find(llvm::IRBuilder<> &builder,
                      llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  // Load length and address from stack
  llvm::Value *len = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Pop both values
  adjust_dsp(builder, dsp_ptr, -2);

  // Convert int64 address to pointer
  llvm::Type *char_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *str_ptr = builder.CreateIntToPtr(addr, char_ptr_type, "str_ptr");

  // Declare the runtime helper function
  llvm::Module *module = builder.GetInsertBlock()->getParent()->getParent();
  llvm::FunctionType *find_func_type = llvm::FunctionType::get(
      builder.getInt64Ty(),
      {char_ptr_type, builder.getInt64Ty()},
      false
  );
  llvm::FunctionCallee find_func = module->getOrInsertFunction(
      "anvil_find_word", find_func_type
  );

  // Call the runtime helper
  llvm::Value *xt = builder.CreateCall(find_func, {str_ptr, len}, "xt");

  // Check if found (xt != 0)
  llvm::Value *found = builder.CreateICmpNE(xt, builder.getInt64(0), "found");
  llvm::Value *flag = builder.CreateSelect(
      found,
      builder.getInt64(-1),  // Found: return -1
      builder.getInt64(0),   // Not found: return 0
      "flag"
  );

  // Push XT onto stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, xt);

  // Push flag onto stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, flag);
}

// Emit LLVM IR for the NUMBER primitive
// Stack effect: ( c-addr u -- n flag )
// Parses string as number, returns number and flag (1 if valid, 0 if invalid)
inline void emit_number(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr,
                        llvm::Value *dsp_ptr) {
  // Load length and address from stack
  llvm::Value *len = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Pop both values
  adjust_dsp(builder, dsp_ptr, -2);

  // Convert int64 address to pointer
  llvm::Type *char_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *str_ptr = builder.CreateIntToPtr(addr, char_ptr_type, "str_ptr");

  // Allocate stack space for output parameters
  llvm::Value *number_out = builder.CreateAlloca(builder.getInt64Ty(), nullptr, "number_out");
  llvm::Value *flag_out = builder.CreateAlloca(builder.getInt64Ty(), nullptr, "flag_out");

  // Declare the runtime helper function
  llvm::Module *module = builder.GetInsertBlock()->getParent()->getParent();
  llvm::Type *int64_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::FunctionType *parse_func_type = llvm::FunctionType::get(
      builder.getVoidTy(),
      {char_ptr_type, builder.getInt64Ty(), int64_ptr_type, int64_ptr_type},
      false
  );
  llvm::FunctionCallee parse_func = module->getOrInsertFunction(
      "anvil_parse_number", parse_func_type
  );

  // Call the runtime helper
  builder.CreateCall(parse_func, {str_ptr, len, number_out, flag_out});

  // Load the results
  llvm::Value *number = builder.CreateLoad(builder.getInt64Ty(), number_out, "number");
  llvm::Value *flag = builder.CreateLoad(builder.getInt64Ty(), flag_out, "flag");

  // Push number onto stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, number);

  // Push flag onto stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, flag);
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

// ============================================================================
// Input Buffer Management Primitives
// ============================================================================

// Emit LLVM IR for the TIB primitive
// Stack effect: ( -- addr )
// Returns address of Terminal Input Buffer
inline void emit_tib(llvm::IRBuilder<> &builder,
                     llvm::Value *data_stack_ptr,
                     llvm::Value *dsp_ptr,
                     llvm::Value *tib_ptr) {
  // Convert TIB pointer to integer address
  llvm::Value *addr = builder.CreatePtrToInt(tib_ptr, builder.getInt64Ty(), "tib_addr");

  // Push address to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr);
}

// Emit LLVM IR for the >IN primitive
// Stack effect: ( -- addr )
// Returns address of parse position variable
inline void emit_to_in(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr,
                       llvm::Value *dsp_ptr,
                       llvm::Value *to_in_ptr) {
  // Convert >IN pointer to integer address
  llvm::Value *addr = builder.CreatePtrToInt(to_in_ptr, builder.getInt64Ty(), "to_in_addr");

  // Push address to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr);
}

// Emit LLVM IR for the #TIB primitive
// Stack effect: ( -- addr )
// Returns address of buffer length variable
inline void emit_num_tib(llvm::IRBuilder<> &builder,
                         llvm::Value *data_stack_ptr,
                         llvm::Value *dsp_ptr,
                         llvm::Value *num_tib_ptr) {
  // Convert #TIB pointer to integer address
  llvm::Value *addr = builder.CreatePtrToInt(num_tib_ptr, builder.getInt64Ty(), "num_tib_addr");

  // Push address to data stack
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr);
}

// Emit LLVM IR for the SOURCE primitive
// Stack effect: ( -- c-addr u )
// Returns address and length of current input buffer
inline void emit_source(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr,
                        llvm::Value *dsp_ptr,
                        llvm::Value *tib_ptr,
                        llvm::Value *num_tib_ptr) {
  // Get TIB address
  llvm::Value *tib_addr = builder.CreatePtrToInt(tib_ptr, builder.getInt64Ty(), "tib_addr");

  // Load #TIB value
  llvm::Value *num_tib = builder.CreateLoad(builder.getInt64Ty(), num_tib_ptr, "num_tib");

  // Push both to stack (address first, then length)
  adjust_dsp(builder, dsp_ptr, 2);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1, tib_addr);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, num_tib);
}

// Emit LLVM IR for the ACCEPT primitive
// Stack effect: ( c-addr +n1 -- +n2 )
// Read a line of input into buffer, return actual count
inline void emit_accept(llvm::IRBuilder<> &builder,
                        llvm::Value *data_stack_ptr,
                        llvm::Value *dsp_ptr) {
  llvm::Module *module = builder.GetInsertBlock()->getModule();

  // Declare anvil_accept(char* buf, size_t maxlen) -> size_t
  llvm::Type *i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::FunctionType *accept_type = llvm::FunctionType::get(
      builder.getInt64Ty(),
      {i8_ptr_type, builder.getInt64Ty()},
      false);
  llvm::FunctionCallee accept_func = module->getOrInsertFunction("anvil_accept", accept_type);

  // Pop max length and address from stack
  llvm::Value *maxlen = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  llvm::Value *addr_int = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);

  // Convert address to pointer
  llvm::Value *buf_ptr = builder.CreateIntToPtr(addr_int, i8_ptr_type, "buf_ptr");

  // Call anvil_accept
  llvm::Value *actual_len = builder.CreateCall(accept_func, {buf_ptr, maxlen});

  // Pop one value (consumed 2, returning 1)
  adjust_dsp(builder, dsp_ptr, -1);

  // Store actual length at top of stack
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, actual_len);
}

// Emit LLVM IR for the PARSE primitive
// Stack effect: ( char "ccc<char>" -- c-addr u )
// Parse string from input buffer until delimiter or end
inline void emit_parse(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr,
                       llvm::Value *dsp_ptr,
                       llvm::Value *tib_ptr,
                       llvm::Value *to_in_ptr,
                       llvm::Value *num_tib_ptr) {
  // Pop delimiter character
  llvm::Value *delim = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Load >IN and #TIB
  llvm::Value *to_in = builder.CreateLoad(builder.getInt64Ty(), to_in_ptr, "to_in");
  llvm::Value *num_tib = builder.CreateLoad(builder.getInt64Ty(), num_tib_ptr, "num_tib");

  // Get TIB base address as integer
  llvm::Value *tib_addr = builder.CreatePtrToInt(tib_ptr, builder.getInt64Ty(), "tib_addr");

  // Calculate start address: TIB + >IN
  llvm::Value *start_addr = builder.CreateAdd(tib_addr, to_in, "start_addr");

  // Create loop to find delimiter or end
  llvm::BasicBlock *loop_test = llvm::BasicBlock::Create(
      builder.getContext(), "parse_loop_test", builder.GetInsertBlock()->getParent());
  llvm::BasicBlock *loop_body = llvm::BasicBlock::Create(
      builder.getContext(), "parse_loop_body", builder.GetInsertBlock()->getParent());
  llvm::BasicBlock *loop_exit = llvm::BasicBlock::Create(
      builder.getContext(), "parse_loop_exit", builder.GetInsertBlock()->getParent());

  // Initialize position counter (starts at >IN)
  llvm::Type *i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
  llvm::Value *tib_i8 = builder.CreateIntToPtr(tib_addr, i8_ptr_type, "tib_i8");

  builder.CreateBr(loop_test);

  // Loop test: check if position < #TIB
  builder.SetInsertPoint(loop_test);
  llvm::PHINode *pos_phi = builder.CreatePHI(builder.getInt64Ty(), 2, "pos");
  pos_phi->addIncoming(to_in, loop_test->getSinglePredecessor());
  llvm::Value *at_end = builder.CreateICmpUGE(pos_phi, num_tib, "at_end");
  llvm::BasicBlock *check_delim = llvm::BasicBlock::Create(
      builder.getContext(), "check_delim", builder.GetInsertBlock()->getParent());
  builder.CreateCondBr(at_end, loop_exit, check_delim);

  // Check if current character is delimiter
  builder.SetInsertPoint(check_delim);
  llvm::Value *char_ptr = builder.CreateGEP(builder.getInt8Ty(), tib_i8, pos_phi, "char_ptr");
  llvm::Value *ch = builder.CreateLoad(builder.getInt8Ty(), char_ptr, "ch");
  llvm::Value *ch_i64 = builder.CreateZExt(ch, builder.getInt64Ty(), "ch_i64");
  llvm::Value *is_delim = builder.CreateICmpEQ(ch_i64, delim, "is_delim");
  builder.CreateCondBr(is_delim, loop_exit, loop_body);

  // Loop body: increment position
  builder.SetInsertPoint(loop_body);
  llvm::Value *next_pos = builder.CreateAdd(pos_phi, builder.getInt64(1), "next_pos");
  pos_phi->addIncoming(next_pos, loop_body);
  builder.CreateBr(loop_test);

  // Loop exit: calculate length and update >IN
  builder.SetInsertPoint(loop_exit);
  llvm::PHINode *end_pos = builder.CreatePHI(builder.getInt64Ty(), 2, "end_pos");
  end_pos->addIncoming(pos_phi, loop_test);
  end_pos->addIncoming(pos_phi, check_delim);

  llvm::Value *length = builder.CreateSub(end_pos, to_in, "length");

  // Update >IN to position after parsed string
  // If we stopped at delimiter, skip it
  llvm::Value *stopped_at_delim = builder.CreateICmpULT(end_pos, num_tib, "stopped_at_delim");
  llvm::Value *new_to_in = builder.CreateSelect(
      stopped_at_delim,
      builder.CreateAdd(end_pos, builder.getInt64(1), "skip_delim"),
      end_pos,
      "new_to_in");
  builder.CreateStore(new_to_in, to_in_ptr);

  // Replace delimiter on stack with address, then push length
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, start_addr);
  adjust_dsp(builder, dsp_ptr, 1);
  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, length);
}

// CMOVE ( c-addr1 c-addr2 u -- )
// Copy u bytes from c-addr1 to c-addr2, low to high addresses
inline void emit_cmove(llvm::IRBuilder<> &builder,
                       llvm::Value *data_stack_ptr,
                       llvm::Value *dsp_ptr) {
  using namespace llvm;

  // Pop count (u), dest address (c-addr2), source address (c-addr1)
  Value *count = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  Value *dest_addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);
  Value *src_addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2);
  adjust_dsp(builder, dsp_ptr, -3);

  // Convert addresses to pointers
  Value *src_ptr = builder.CreateIntToPtr(src_addr, PointerType::get(builder.getContext(), 0), "src_ptr");
  Value *dest_ptr = builder.CreateIntToPtr(dest_addr, PointerType::get(builder.getContext(), 0), "dest_ptr");

  // Use LLVM's memcpy intrinsic - inline, no function call
  builder.CreateMemCpy(dest_ptr, MaybeAlign(1), src_ptr, MaybeAlign(1), count);
}

// CMOVE> ( c-addr1 c-addr2 u -- )
// Copy u bytes from c-addr1 to c-addr2, high to low addresses (for overlapping)
inline void emit_cmove_backward(llvm::IRBuilder<> &builder,
                                 llvm::Value *data_stack_ptr,
                                 llvm::Value *dsp_ptr) {
  using namespace llvm;

  // Pop count (u), dest address (c-addr2), source address (c-addr1)
  Value *count = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  Value *dest_addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);
  Value *src_addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2);
  adjust_dsp(builder, dsp_ptr, -3);

  // Convert addresses to pointers
  Value *src_ptr = builder.CreateIntToPtr(src_addr, PointerType::get(builder.getContext(), 0), "src_ptr");
  Value *dest_ptr = builder.CreateIntToPtr(dest_addr, PointerType::get(builder.getContext(), 0), "dest_ptr");

  // Use LLVM's memmove intrinsic - handles overlapping regions, inline
  builder.CreateMemMove(dest_ptr, MaybeAlign(1), src_ptr, MaybeAlign(1), count);
}

// FILL ( c-addr u char -- )
// Fill u bytes starting at c-addr with character char
inline void emit_fill(llvm::IRBuilder<> &builder,
                      llvm::Value *data_stack_ptr,
                      llvm::Value *dsp_ptr) {
  using namespace llvm;

  // Pop char, count (u), address (c-addr)
  Value *fill_char = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  Value *count = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);
  Value *addr = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2);
  adjust_dsp(builder, dsp_ptr, -3);

  // Convert to byte and pointer
  Value *byte_val = builder.CreateTrunc(fill_char, builder.getInt8Ty(), "byte_val");
  Value *ptr = builder.CreateIntToPtr(addr, PointerType::get(builder.getContext(), 0), "ptr");

  // Use LLVM's memset intrinsic - inline
  builder.CreateMemSet(ptr, byte_val, count, MaybeAlign(1));
}

// COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
// Compare two strings lexicographically
// Return: n<0 if string1 < string2, n=0 if equal, n>0 if string1 > string2
inline void emit_compare(llvm::IRBuilder<> &builder,
                         llvm::Value *data_stack_ptr,
                         llvm::Value *dsp_ptr) {
  using namespace llvm;

  // Pop u2, c-addr2, u1, c-addr1
  Value *len2 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);
  Value *addr2 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 1);
  Value *len1 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 2);
  Value *addr1 = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 3);
  adjust_dsp(builder, dsp_ptr, -3);  // Keep one slot for result

  // Convert addresses to pointers
  Value *ptr1 = builder.CreateIntToPtr(addr1, PointerType::get(builder.getContext(), 0), "ptr1");
  Value *ptr2 = builder.CreateIntToPtr(addr2, PointerType::get(builder.getContext(), 0), "ptr2");

  // Get minimum length
  Value *min_len = builder.CreateSelect(
      builder.CreateICmpULT(len1, len2), len1, len2, "min_len");

  // Create blocks
  llvm::Function *func = builder.GetInsertBlock()->getParent();
  BasicBlock *entry_block = builder.GetInsertBlock();
  BasicBlock *loop_header = BasicBlock::Create(builder.getContext(), "cmp_loop", func);
  BasicBlock *loop_body = BasicBlock::Create(builder.getContext(), "cmp_body", func);
  BasicBlock *bytes_differ = BasicBlock::Create(builder.getContext(), "cmp_differ", func);
  BasicBlock *compare_lengths = BasicBlock::Create(builder.getContext(), "cmp_lengths", func);
  BasicBlock *done = BasicBlock::Create(builder.getContext(), "cmp_done", func);

  // Jump to loop header
  builder.CreateBr(loop_header);

  // Loop header - use PHI for index
  builder.SetInsertPoint(loop_header);
  PHINode *index_phi = builder.CreatePHI(builder.getInt64Ty(), 2, "index");
  index_phi->addIncoming(builder.getInt64(0), entry_block);  // Initial value from entry

  // Check if we've compared all common bytes
  Value *continue_cmp = builder.CreateICmpULT(index_phi, min_len, "continue");
  builder.CreateCondBr(continue_cmp, loop_body, compare_lengths);

  // Loop body - compare bytes
  builder.SetInsertPoint(loop_body);
  Value *byte1_ptr = builder.CreateGEP(builder.getInt8Ty(), ptr1, index_phi, "b1_ptr");
  Value *byte2_ptr = builder.CreateGEP(builder.getInt8Ty(), ptr2, index_phi, "b2_ptr");
  Value *byte1 = builder.CreateLoad(builder.getInt8Ty(), byte1_ptr, "b1");
  Value *byte2 = builder.CreateLoad(builder.getInt8Ty(), byte2_ptr, "b2");

  Value *bytes_equal = builder.CreateICmpEQ(byte1, byte2, "eq");
  Value *next_index = builder.CreateAdd(index_phi, builder.getInt64(1), "next_idx");
  index_phi->addIncoming(next_index, loop_body);  // Update PHI for next iteration

  builder.CreateCondBr(bytes_equal, loop_header, bytes_differ);

  // Bytes differ - return difference
  builder.SetInsertPoint(bytes_differ);
  Value *b1_ext = builder.CreateZExt(byte1, builder.getInt64Ty(), "b1_ext");
  Value *b2_ext = builder.CreateZExt(byte2, builder.getInt64Ty(), "b2_ext");
  Value *diff = builder.CreateSub(b1_ext, b2_ext, "diff");
  builder.CreateBr(done);

  // All bytes equal - compare lengths
  builder.SetInsertPoint(compare_lengths);
  Value *len_diff = builder.CreateSub(len1, len2, "len_diff");
  builder.CreateBr(done);

  // Done - merge results
  builder.SetInsertPoint(done);
  PHINode *result_phi = builder.CreatePHI(builder.getInt64Ty(), 2, "result");
  result_phi->addIncoming(diff, bytes_differ);
  result_phi->addIncoming(len_diff, compare_lengths);

  store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, result_phi);
}

// ============================================================================
// Interpreter state primitives
// ============================================================================

// Emit LLVM IR for the [ primitive (left-bracket)
// Stack effect: ( -- )
// Switches to interpret state by setting STATE to 0
// This is an IMMEDIATE word
inline void emit_left_bracket(llvm::IRBuilder<> &builder,
                               llvm::Value *data_stack_ptr,
                               llvm::Value *data_space_ptr,
                               llvm::Value *dsp_ptr) {
  // Get the address of STATE-VAR (first variable in data space at offset 0)
  llvm::Value *state_addr = builder.CreatePtrToInt(
      data_space_ptr, builder.getInt64Ty(), "state_addr"
  );

  // Convert address back to pointer
  llvm::Value *state_ptr = builder.CreateIntToPtr(
      state_addr,
      llvm::PointerType::get(builder.getContext(), 0),
      "state_ptr"
  );

  // Store 0 (interpret mode)
  builder.CreateStore(builder.getInt64(0), state_ptr);
}

// Emit LLVM IR for the ] primitive (right-bracket)
// Stack effect: ( -- )
// Switches to compile state by setting STATE to -1
inline void emit_right_bracket(llvm::IRBuilder<> &builder,
                                llvm::Value *data_stack_ptr,
                                llvm::Value *data_space_ptr,
                                llvm::Value *dsp_ptr) {
  // Get the address of STATE-VAR (first variable in data space at offset 0)
  llvm::Value *state_addr = builder.CreatePtrToInt(
      data_space_ptr, builder.getInt64Ty(), "state_addr"
  );

  // Convert address back to pointer
  llvm::Value *state_ptr = builder.CreateIntToPtr(
      state_addr,
      llvm::PointerType::get(builder.getContext(), 0),
      "state_ptr"
  );

  // Store -1 (compile mode)
  builder.CreateStore(builder.getInt64(-1), state_ptr);
}

// ============================================================================
// Stack reset and error recovery primitives
// ============================================================================

// Emit LLVM IR for the DSP! primitive (set data stack pointer)
// Stack effect: ( n -- )
// Sets the data stack pointer to n
inline void emit_dsp_store(llvm::IRBuilder<> &builder,
                           llvm::Value *data_stack_ptr,
                           llvm::Value *dsp_ptr) {
  // Load the new DSP value from stack
  llvm::Value *new_dsp = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Store it to the DSP pointer
  builder.CreateStore(new_dsp, dsp_ptr);
}

// Emit LLVM IR for the RSP! primitive (set return stack pointer)
// Stack effect: ( n -- )
// Sets the return stack pointer to n
inline void emit_rsp_store(llvm::IRBuilder<> &builder,
                           llvm::Value *data_stack_ptr,
                           llvm::Value *dsp_ptr,
                           llvm::Value *rsp_ptr) {
  // Load the new RSP value from data stack
  llvm::Value *new_rsp = load_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0);

  // Pop from data stack
  adjust_dsp(builder, dsp_ptr, -1);

  // Store it to the RSP pointer
  builder.CreateStore(new_rsp, rsp_ptr);
}

// Emit LLVM IR for the ABORT primitive
// Stack effect: ( -- )
// Clears both stacks by resetting pointers to 0
inline void emit_abort(llvm::IRBuilder<> &builder,
                       llvm::Value *dsp_ptr,
                       llvm::Value *rsp_ptr) {
  // Reset data stack pointer to 0
  builder.CreateStore(builder.getInt64(0), dsp_ptr);

  // Reset return stack pointer to 0
  builder.CreateStore(builder.getInt64(0), rsp_ptr);
}

} // namespace anvil

#endif // ANVIL_PRIMITIVES_H
