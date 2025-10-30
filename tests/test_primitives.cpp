#include <catch2/catch_test_macros.hpp>
#include "test_helpers.h"
#include "primitives.h"

using namespace anvil;
using namespace anvil::test;

TEST_CASE("Addition primitive", "[primitives][add]") {
    auto ctx = execute_test("test_add", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 2 and 3 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {2, 3});

        // Execute add primitive
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 2 + 3 = 5
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 5);
}

TEST_CASE("Subtraction primitive", "[primitives][sub]") {
    auto ctx = execute_test("test_sub", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 5 and 3 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 3});

        // Execute sub primitive
        emit_sub(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 5 - 3 = 2
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 2);
}

TEST_CASE("Multiple additions", "[primitives][add]") {
    auto ctx = execute_test("test_multiple_add", [](llvm::IRBuilder<>& builder,
                                                    llvm::Value* data_stack_ptr,
                                                    llvm::Value* dsp_ptr) {
        // Push 10, 20, 30 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {10, 20, 30});

        // Execute: 10 + 20 = 30 (stack: 30, 30)
        emit_add(builder, data_stack_ptr, dsp_ptr);

        // Execute: 30 + 30 = 60 (stack: 60)
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: (10 + 20) + 30 = 60
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 60);
}

TEST_CASE("Subtraction with negative result", "[primitives][sub]") {
    auto ctx = execute_test("test_sub_negative", [](llvm::IRBuilder<>& builder,
                                                    llvm::Value* data_stack_ptr,
                                                    llvm::Value* dsp_ptr) {
        // Push 3 and 5 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {3, 5});

        // Execute sub primitive
        emit_sub(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 3 - 5 = -2
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -2);
}

TEST_CASE("Mixed operations", "[primitives][add][sub]") {
    auto ctx = execute_test("test_mixed", [](llvm::IRBuilder<>& builder,
                                            llvm::Value* data_stack_ptr,
                                            llvm::Value* dsp_ptr) {
        // Push 100, 30, 20 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {100, 30, 20});

        // Execute: 30 - 20 = 10 (stack: 100, 10)
        emit_sub(builder, data_stack_ptr, dsp_ptr);

        // Execute: 100 + 10 = 110 (stack: 110)
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: (30 - 20) = 10, then 100 + 10 = 110
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 110);
}

TEST_CASE("Multiplication primitive", "[primitives][mul]") {
    auto ctx = execute_test("test_mul", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 6 and 7 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {6, 7});

        // Execute mul primitive
        emit_mul(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 6 * 7 = 42
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);
}

TEST_CASE("/MOD primitive - exact division", "[primitives][divmod]") {
    auto ctx = execute_test("test_divmod", [](llvm::IRBuilder<>& builder,
                                              llvm::Value* data_stack_ptr,
                                              llvm::Value* dsp_ptr) {
        // Push 20 and 4 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {20, 4});

        // Execute divmod primitive
        emit_divmod(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 20 /MOD 4 = remainder:0 quotient:5
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 0);  // remainder
    REQUIRE(ctx.data_stack[1] == 5);  // quotient
}

TEST_CASE("/MOD primitive - with remainder", "[primitives][divmod]") {
    auto ctx = execute_test("test_divmod_remainder", [](llvm::IRBuilder<>& builder,
                                                        llvm::Value* data_stack_ptr,
                                                        llvm::Value* dsp_ptr) {
        // Push 7 and 2 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {7, 2});

        // Execute divmod primitive
        emit_divmod(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 7 /MOD 2 = remainder:1 quotient:3
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 1);  // remainder
    REQUIRE(ctx.data_stack[1] == 3);  // quotient
}

TEST_CASE("/MOD primitive - negative dividend", "[primitives][divmod]") {
    auto ctx = execute_test("test_divmod_negative", [](llvm::IRBuilder<>& builder,
                                                       llvm::Value* data_stack_ptr,
                                                       llvm::Value* dsp_ptr) {
        // Push -20 and 4 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {-20, 4});

        // Execute divmod primitive
        emit_divmod(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: -20 /MOD 4 = remainder:0 quotient:-5
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 0);   // remainder
    REQUIRE(ctx.data_stack[1] == -5);  // quotient
}

TEST_CASE("Complex expression", "[primitives][add][sub][mul][divmod]") {
    auto ctx = execute_test("test_complex", [](llvm::IRBuilder<>& builder,
                                              llvm::Value* data_stack_ptr,
                                              llvm::Value* dsp_ptr) {
        // Push 2, 3, 4 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {2, 3, 4});

        // Execute: 3 * 4 = 12 (stack: 2, 12)
        emit_mul(builder, data_stack_ptr, dsp_ptr);

        // Execute: 2 + 12 = 14 (stack: 14)
        emit_add(builder, data_stack_ptr, dsp_ptr);

        // Manually push 7 onto the stack (stack: 14, 7)
        llvm::Value* dsp = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* idx = builder.CreateAdd(dsp, builder.getInt64(0), "idx");
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(builder.getInt64(7), elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp, builder.getInt64(1)), dsp_ptr);

        // Execute: 14 /MOD 7 = remainder:0 quotient:2 (stack: 0, 2)
        emit_divmod(builder, data_stack_ptr, dsp_ptr);

        // SWAP to get quotient on top, then DROP remainder (stack: 2)
        emit_swap(builder, data_stack_ptr, dsp_ptr);
        emit_drop(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: (2 + 3 * 4) / 7 = 14 / 7 = 2
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 2);
}

TEST_CASE("DUP primitive", "[primitives][stack][dup]") {
    auto ctx = execute_test("test_dup", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 42 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {42});

        // Execute dup primitive
        emit_dup(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 42 -> 42 42
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 42);
    REQUIRE(ctx.data_stack[1] == 42);
}

TEST_CASE("SWAP primitive", "[primitives][stack][swap]") {
    auto ctx = execute_test("test_swap", [](llvm::IRBuilder<>& builder,
                                            llvm::Value* data_stack_ptr,
                                            llvm::Value* dsp_ptr) {
        // Push 10 and 20 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {10, 20});

        // Execute swap primitive
        emit_swap(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 10 20 -> 20 10
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 20);
    REQUIRE(ctx.data_stack[1] == 10);
}

TEST_CASE("DROP primitive", "[primitives][stack][drop]") {
    auto ctx = execute_test("test_drop", [](llvm::IRBuilder<>& builder,
                                            llvm::Value* data_stack_ptr,
                                            llvm::Value* dsp_ptr) {
        // Push 10, 20, 30 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {10, 20, 30});

        // Execute drop primitive
        emit_drop(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 10 20 30 -> 10 20
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 10);
    REQUIRE(ctx.data_stack[1] == 20);
}

TEST_CASE("Stack operations combined", "[primitives][stack]") {
    auto ctx = execute_test("test_stack_combo", [](llvm::IRBuilder<>& builder,
                                                   llvm::Value* data_stack_ptr,
                                                   llvm::Value* dsp_ptr) {
        // Push 5 and 10
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 10});

        // DUP: 5 10 -> 5 10 10
        emit_dup(builder, data_stack_ptr, dsp_ptr);

        // SWAP: 5 10 10 -> 5 10 10 (swap top two)
        emit_swap(builder, data_stack_ptr, dsp_ptr);

        // DROP: 5 10 10 -> 5 10
        emit_drop(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should have 5 10 on stack
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 5);
    REQUIRE(ctx.data_stack[1] == 10);
}

TEST_CASE("DUP then multiply", "[primitives][stack][dup][mul]") {
    auto ctx = execute_test("test_dup_mul", [](llvm::IRBuilder<>& builder,
                                               llvm::Value* data_stack_ptr,
                                               llvm::Value* dsp_ptr) {
        // Push 7
        push_values(builder, data_stack_ptr, dsp_ptr, {7});

        // DUP: 7 -> 7 7
        emit_dup(builder, data_stack_ptr, dsp_ptr);

        // MUL: 7 7 -> 49
        emit_mul(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 7 squared = 49
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 49);
}

TEST_CASE("OVER primitive", "[primitives][stack][over]") {
    auto ctx = execute_test("test_over", [](llvm::IRBuilder<>& builder,
                                            llvm::Value* data_stack_ptr,
                                            llvm::Value* dsp_ptr) {
        // Push 100 and 200 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {100, 200});

        // Execute over primitive
        emit_over(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 100 200 -> 100 200 100
    REQUIRE(ctx.dsp == 3);
    REQUIRE(ctx.data_stack[0] == 100);
    REQUIRE(ctx.data_stack[1] == 200);
    REQUIRE(ctx.data_stack[2] == 100);
}

TEST_CASE("ROT primitive", "[primitives][stack][rot]") {
    auto ctx = execute_test("test_rot", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 1, 2, 3 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {1, 2, 3});

        // Execute rot primitive
        emit_rot(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 1 2 3 -> 2 3 1
    REQUIRE(ctx.dsp == 3);
    REQUIRE(ctx.data_stack[0] == 2);
    REQUIRE(ctx.data_stack[1] == 3);
    REQUIRE(ctx.data_stack[2] == 1);
}

TEST_CASE("OVER then add", "[primitives][stack][over][add]") {
    auto ctx = execute_test("test_over_add", [](llvm::IRBuilder<>& builder,
                                                llvm::Value* data_stack_ptr,
                                                llvm::Value* dsp_ptr) {
        // Push 5 and 10 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 10});

        // OVER: 5 10 -> 5 10 5
        emit_over(builder, data_stack_ptr, dsp_ptr);

        // ADD: 5 10 5 -> 5 15
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 5, (10 + 5) = 5, 15
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 5);
    REQUIRE(ctx.data_stack[1] == 15);
}

TEST_CASE("ROT to reorder for subtraction", "[primitives][stack][rot][sub]") {
    auto ctx = execute_test("test_rot_sub", [](llvm::IRBuilder<>& builder,
                                               llvm::Value* data_stack_ptr,
                                               llvm::Value* dsp_ptr) {
        // Push 100, 30, 20 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {100, 30, 20});

        // ROT: 100 30 20 -> 30 20 100
        emit_rot(builder, data_stack_ptr, dsp_ptr);

        // DROP: 30 20 100 -> 30 20
        emit_drop(builder, data_stack_ptr, dsp_ptr);

        // SUB: 30 20 -> 10
        emit_sub(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 30 - 20 = 10
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 10);
}

TEST_CASE("AND primitive", "[primitives][bitwise][and]") {
    auto ctx = execute_test("test_and", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 0b1100 (12) and 0b1010 (10) onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {12, 10});

        // Execute and primitive
        emit_and(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 12 & 10 = 0b1000 = 8
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 8);
}

TEST_CASE("OR primitive", "[primitives][bitwise][or]") {
    auto ctx = execute_test("test_or", [](llvm::IRBuilder<>& builder,
                                          llvm::Value* data_stack_ptr,
                                          llvm::Value* dsp_ptr) {
        // Push 0b1100 (12) and 0b1010 (10) onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {12, 10});

        // Execute or primitive
        emit_or(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 12 | 10 = 0b1110 = 14
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 14);
}

TEST_CASE("XOR primitive", "[primitives][bitwise][xor]") {
    auto ctx = execute_test("test_xor", [](llvm::IRBuilder<>& builder,
                                           llvm::Value* data_stack_ptr,
                                           llvm::Value* dsp_ptr) {
        // Push 0b1100 (12) and 0b1010 (10) onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {12, 10});

        // Execute xor primitive
        emit_xor(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 12 ^ 10 = 0b0110 = 6
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 6);
}

TEST_CASE("INVERT primitive", "[primitives][bitwise][invert]") {
    auto ctx = execute_test("test_invert", [](llvm::IRBuilder<>& builder,
                                              llvm::Value* data_stack_ptr,
                                              llvm::Value* dsp_ptr) {
        // Push 0 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {0});

        // Execute invert primitive
        emit_invert(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: ~0 = -1 (all bits set)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -1);
}

TEST_CASE("INVERT with specific value", "[primitives][bitwise][invert]") {
    auto ctx = execute_test("test_invert_value", [](llvm::IRBuilder<>& builder,
                                                    llvm::Value* data_stack_ptr,
                                                    llvm::Value* dsp_ptr) {
        // Push 0b00001111 (15) onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {15});

        // Execute invert primitive
        emit_invert(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: ~15 = -16 (in two's complement)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -16);
}

TEST_CASE("Bitwise combination", "[primitives][bitwise]") {
    auto ctx = execute_test("test_bitwise_combo", [](llvm::IRBuilder<>& builder,
                                                     llvm::Value* data_stack_ptr,
                                                     llvm::Value* dsp_ptr) {
        // Push 255 and 15 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {255, 15});

        // AND: 255 & 15 = 15 (stack: 15)
        emit_and(builder, data_stack_ptr, dsp_ptr);

        // INVERT: ~15 = -16 (stack: -16)
        emit_invert(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: ~(255 & 15) = ~15 = -16
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -16);
}

TEST_CASE("< primitive - true case", "[primitives][comparison][lt]") {
    auto ctx = execute_test("test_lt_true", [](llvm::IRBuilder<>& builder,
                                               llvm::Value* data_stack_ptr,
                                               llvm::Value* dsp_ptr) {
        // Push 5 and 10 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 10});

        // Execute < primitive
        emit_lt(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 5 < 10 = true (-1)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -1);
}

TEST_CASE("< primitive - false case", "[primitives][comparison][lt]") {
    auto ctx = execute_test("test_lt_false", [](llvm::IRBuilder<>& builder,
                                                llvm::Value* data_stack_ptr,
                                                llvm::Value* dsp_ptr) {
        // Push 10 and 5 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {10, 5});

        // Execute < primitive
        emit_lt(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 10 < 5 = false (0)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 0);
}

TEST_CASE("< primitive - equal values", "[primitives][comparison][lt]") {
    auto ctx = execute_test("test_lt_equal", [](llvm::IRBuilder<>& builder,
                                                llvm::Value* data_stack_ptr,
                                                llvm::Value* dsp_ptr) {
        // Push 7 and 7 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {7, 7});

        // Execute < primitive
        emit_lt(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 7 < 7 = false (0)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 0);
}

TEST_CASE("< primitive - negative numbers", "[primitives][comparison][lt]") {
    auto ctx = execute_test("test_lt_negative", [](llvm::IRBuilder<>& builder,
                                                   llvm::Value* data_stack_ptr,
                                                   llvm::Value* dsp_ptr) {
        // Push -10 and -5 onto the stack
        push_values(builder, data_stack_ptr, dsp_ptr, {-10, -5});

        // Execute < primitive
        emit_lt(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: -10 < -5 = true (-1)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -1);
}

TEST_CASE("@ and ! primitives", "[primitives][memory][fetch][store]") {
    auto ctx = execute_test("test_fetch_store", [](llvm::IRBuilder<>& builder,
                                                    llvm::Value* data_stack_ptr,
                                                    llvm::Value* dsp_ptr) {
        // Allocate a local variable in the generated function
        llvm::Value* var = builder.CreateAlloca(builder.getInt64Ty(), nullptr, "test_var");

        // Get address as int64
        llvm::Value* addr = builder.CreatePtrToInt(var, builder.getInt64Ty(), "addr");

        // Push value 42 and address onto stack
        push_values(builder, data_stack_ptr, dsp_ptr, {42});

        // Manually push address
        llvm::Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* idx = dsp_val;
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Execute ! to store 42 at address
        emit_store(builder, data_stack_ptr, dsp_ptr);

        // Push address again
        dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        idx = dsp_val;
        elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Execute @ to fetch value from address
        emit_fetch(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should have 42 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);
}

TEST_CASE("C@ and C! primitives", "[primitives][memory][cfetch][cstore]") {
    auto ctx = execute_test("test_cfetch_cstore", [](llvm::IRBuilder<>& builder,
                                                      llvm::Value* data_stack_ptr,
                                                      llvm::Value* dsp_ptr) {
        // Allocate a byte variable in the generated function
        llvm::Value* var = builder.CreateAlloca(builder.getInt8Ty(), nullptr, "test_byte");

        // Get address as int64
        llvm::Value* addr = builder.CreatePtrToInt(var, builder.getInt64Ty(), "addr");

        // Push value 255 (0xFF) and address onto stack
        push_values(builder, data_stack_ptr, dsp_ptr, {255});

        // Manually push address
        llvm::Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* idx = dsp_val;
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Execute C! to store byte 255 at address
        emit_cstore(builder, data_stack_ptr, dsp_ptr);

        // Push address again
        dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        idx = dsp_val;
        elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Execute C@ to fetch byte from address
        emit_cfetch(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should have 255 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 255);
}

TEST_CASE("C! truncates to byte", "[primitives][memory][cstore]") {
    auto ctx = execute_test("test_cstore_truncate", [](llvm::IRBuilder<>& builder,
                                                        llvm::Value* data_stack_ptr,
                                                        llvm::Value* dsp_ptr) {
        // Allocate a byte variable
        llvm::Value* var = builder.CreateAlloca(builder.getInt8Ty(), nullptr, "test_byte");
        llvm::Value* addr = builder.CreatePtrToInt(var, builder.getInt64Ty(), "addr");

        // Push value 0x1234 (only low byte 0x34 = 52 should be stored)
        push_values(builder, data_stack_ptr, dsp_ptr, {0x1234});

        // Push address
        llvm::Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Store byte
        emit_cstore(builder, data_stack_ptr, dsp_ptr);

        // Push address and fetch
        dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(addr, elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        emit_cfetch(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should have 0x34 = 52 on stack (low byte of 0x1234)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 0x34);
}

TEST_CASE(">R and R> primitives", "[primitives][rstack][to-r][from-r]") {
    auto ctx = execute_test_rstack("test_to_r_from_r",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 42 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {42});

        // Move to return stack
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Move back to data stack
        emit_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
    });

    // Check result: 42 should be back on data stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);
    REQUIRE(ctx.rsp == 0);  // Return stack should be empty
}

TEST_CASE("R@ primitive", "[primitives][rstack][r-fetch]") {
    auto ctx = execute_test_rstack("test_r_fetch",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 100 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {100});

        // Move to return stack
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Copy from return stack (without removing)
        emit_r_fetch(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
    });

    // Check result: 100 on data stack, also on return stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 100);
    REQUIRE(ctx.rsp == 1);  // Return stack should still have value
    REQUIRE(ctx.return_stack[0] == 100);
}

TEST_CASE("Return stack for temporary storage", "[primitives][rstack]") {
    auto ctx = execute_test_rstack("test_rstack_temp",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 5, 10, 15 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 10, 15});

        // Save 15 to return stack: >R
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Now stack is: 5, 10
        // Add them: 5 + 10 = 15
        emit_add(builder, data_stack_ptr, dsp_ptr);

        // Retrieve saved value: R>
        emit_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Now stack is: 15, 15
        // Multiply: 15 * 15 = 225
        emit_mul(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: (5 + 10) * 15 = 225
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 225);
    REQUIRE(ctx.rsp == 0);
}

TEST_CASE("Multiple values on return stack", "[primitives][rstack]") {
    auto ctx = execute_test_rstack("test_multiple_rstack",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 1, 2, 3 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {1, 2, 3});

        // Move all to return stack
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 3
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 2
        emit_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 1

        // Move all back (LIFO order)
        emit_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 1
        emit_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 2
        emit_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);  // 3
    });

    // Check result: LIFO reversal: 1 2 3 -> 1 2 3 (back in order)
    REQUIRE(ctx.dsp == 3);
    REQUIRE(ctx.data_stack[0] == 1);
    REQUIRE(ctx.data_stack[1] == 2);
    REQUIRE(ctx.data_stack[2] == 3);
    REQUIRE(ctx.rsp == 0);
}

TEST_CASE("2>R and 2R> primitives", "[primitives][rstack][two-to-r][two-from-r]") {
    auto ctx = execute_test_rstack("test_two_to_r_two_from_r",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 10 and 20 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {10, 20});

        // Move both to return stack: 2>R
        emit_two_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Manually push 30 onto data stack (dsp is now 0 after 2>R)
        llvm::Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(builder.getInt64(30), elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Retrieve both from return stack: 2R>
        emit_two_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
    });

    // Check result: should be 30, 10, 20 on data stack (30 at bottom, 20 on top)
    REQUIRE(ctx.dsp == 3);
    REQUIRE(ctx.data_stack[0] == 30);
    REQUIRE(ctx.data_stack[1] == 10);
    REQUIRE(ctx.data_stack[2] == 20);
    REQUIRE(ctx.rsp == 0);
}

TEST_CASE("2R@ primitive", "[primitives][rstack][two-r-fetch]") {
    auto ctx = execute_test_rstack("test_two_r_fetch",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Push 100 and 200 onto data stack
        push_values(builder, data_stack_ptr, dsp_ptr, {100, 200});

        // Move both to return stack: 2>R
        emit_two_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Copy (non-destructive) both from return stack: 2R@
        emit_two_r_fetch(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Copy again to verify return stack unchanged
        emit_two_r_fetch(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
    });

    // Check result: should be 100, 200, 100, 200 on data stack
    REQUIRE(ctx.dsp == 4);
    REQUIRE(ctx.data_stack[0] == 100);
    REQUIRE(ctx.data_stack[1] == 200);
    REQUIRE(ctx.data_stack[2] == 100);
    REQUIRE(ctx.data_stack[3] == 200);
    // Return stack should still have 100, 200
    REQUIRE(ctx.rsp == 2);
    REQUIRE(ctx.return_stack[0] == 100);
    REQUIRE(ctx.return_stack[1] == 200);
}

TEST_CASE("2>R/2R> for DO loop indices", "[primitives][rstack][practical]") {
    auto ctx = execute_test_rstack("test_two_r_loop_simulation",
                                    [](llvm::IRBuilder<>& builder,
                                       llvm::Value* data_stack_ptr,
                                       llvm::Value* return_stack_ptr,
                                       llvm::Value* dsp_ptr,
                                       llvm::Value* rsp_ptr) {
        // Simulate setting up a DO loop: start=5, limit=15
        push_values(builder, data_stack_ptr, dsp_ptr, {5, 15});

        // Save loop indices to return stack: 2>R
        emit_two_to_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

        // Do some work - push a value
        // Manually push 100 onto data stack
        llvm::Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(builder.getInt64(100), elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Copy indices to compute something: 2R@
        emit_two_r_fetch(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
        // Stack now: 100, 5, 15

        // Add the indices: 5 + 15 = 20
        emit_add(builder, data_stack_ptr, dsp_ptr);
        // Stack now: 100, 20

        // Add to our saved value: 100 + 20 = 120
        emit_add(builder, data_stack_ptr, dsp_ptr);
        // Stack now: 120

        // Restore original indices: 2R>
        emit_two_from_r(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);
        // Stack now: 120, 5, 15

        // Verify we can still use them: multiply start * 2
        emit_drop(builder, data_stack_ptr, dsp_ptr);  // Drop limit
        // Stack now: 120, 5

        emit_dup(builder, data_stack_ptr, dsp_ptr);   // Duplicate start
        // Stack now: 120, 5, 5

        emit_add(builder, data_stack_ptr, dsp_ptr);   // 5 + 5 = 10
        // Stack now: 120, 10

        emit_add(builder, data_stack_ptr, dsp_ptr);   // 120 + 10 = 130
        // Stack now: 130
    });

    // Check result: should be 130
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 130);
    REQUIRE(ctx.rsp == 0);
}

TEST_CASE("BRANCH primitive", "[primitives][control][branch]") {
    auto ctx = execute_test("test_branch",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* block1 = BasicBlock::Create(builder.getContext(), "block1", func);
        BasicBlock* block2 = BasicBlock::Create(builder.getContext(), "block2", func);
        BasicBlock* block3 = BasicBlock::Create(builder.getContext(), "block3", func);

        // Entry: push 10, then branch to block2 (skipping block1)
        push_values(builder, data_stack_ptr, dsp_ptr, {10});
        emit_branch(builder, block2);

        // Block1: this should be skipped - push 99
        builder.SetInsertPoint(block1);
        push_values(builder, data_stack_ptr, dsp_ptr, {99});
        emit_branch(builder, block3);

        // Block2: push 20, then go to block3
        builder.SetInsertPoint(block2);
        Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(builder.getInt64(20), elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);
        emit_branch(builder, block3);

        // Block3: add the two values
        builder.SetInsertPoint(block3);
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should be 30 (10 + 20), not involving 99
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 30);
}

TEST_CASE("0BRANCH primitive - takes branch when zero", "[primitives][control][0branch]") {
    auto ctx = execute_test("test_0branch_zero",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* true_block = BasicBlock::Create(builder.getContext(), "true_block", func);
        BasicBlock* false_block = BasicBlock::Create(builder.getContext(), "false_block", func);
        BasicBlock* merge_block = BasicBlock::Create(builder.getContext(), "merge", func);

        // Push 0 (false flag) and test it
        push_values(builder, data_stack_ptr, dsp_ptr, {0});
        emit_0branch(builder, data_stack_ptr, dsp_ptr, true_block, false_block);

        // True block: push 100 (taken when flag is zero)
        builder.SetInsertPoint(true_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {100});
        emit_branch(builder, merge_block);

        // False block: push 200 (taken when flag is non-zero)
        builder.SetInsertPoint(false_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {200});
        emit_branch(builder, merge_block);

        // Merge block: nothing to do
        builder.SetInsertPoint(merge_block);
    });

    // Check result: should be 100 (zero flag means take the branch)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 100);
}

TEST_CASE("0BRANCH primitive - falls through when non-zero", "[primitives][control][0branch]") {
    auto ctx = execute_test("test_0branch_nonzero",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* true_block = BasicBlock::Create(builder.getContext(), "true_block", func);
        BasicBlock* false_block = BasicBlock::Create(builder.getContext(), "false_block", func);
        BasicBlock* merge_block = BasicBlock::Create(builder.getContext(), "merge", func);

        // Push -1 (true flag) and test it
        push_values(builder, data_stack_ptr, dsp_ptr, {-1});
        emit_0branch(builder, data_stack_ptr, dsp_ptr, true_block, false_block);

        // True block: push 100 (taken when flag is zero)
        builder.SetInsertPoint(true_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {100});
        emit_branch(builder, merge_block);

        // False block: push 200 (taken when flag is non-zero)
        builder.SetInsertPoint(false_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {200});
        emit_branch(builder, merge_block);

        // Merge block: nothing to do
        builder.SetInsertPoint(merge_block);
    });

    // Check result: should be 200 (non-zero flag means fall through)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 200);
}

TEST_CASE("EXIT primitive", "[primitives][control][exit]") {
    auto ctx = execute_test("test_exit",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* exit_block = BasicBlock::Create(builder.getContext(), "exit_block", func);
        BasicBlock* unreachable_block = BasicBlock::Create(builder.getContext(), "unreachable", func);

        // Push 42
        push_values(builder, data_stack_ptr, dsp_ptr, {42});

        // Branch to exit block
        emit_branch(builder, exit_block);

        // Exit block: return early
        builder.SetInsertPoint(exit_block);
        emit_exit(builder);

        // Unreachable block: this should never execute
        builder.SetInsertPoint(unreachable_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {999});
    });

    // Check result: should be 42 (EXIT prevented 999 from being pushed)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);
}

TEST_CASE("Conditional early exit", "[primitives][control][practical]") {
    auto ctx = execute_test("test_conditional_exit",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* check_block = BasicBlock::Create(builder.getContext(), "check", func);
        BasicBlock* exit_block = BasicBlock::Create(builder.getContext(), "exit", func);
        BasicBlock* continue_block = BasicBlock::Create(builder.getContext(), "continue", func);

        // Push initial value 5
        push_values(builder, data_stack_ptr, dsp_ptr, {5});

        // Duplicate and check if < 10
        emit_dup(builder, data_stack_ptr, dsp_ptr);
        // Manually push 10
        Value* dsp_val = builder.CreateLoad(builder.getInt64Ty(), dsp_ptr, "dsp");
        Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, dsp_val, "elem_ptr");
        builder.CreateStore(builder.getInt64(10), elem_ptr);
        builder.CreateStore(builder.CreateAdd(dsp_val, builder.getInt64(1)), dsp_ptr);

        // Compare: 5 < 10 = -1 (true)
        emit_lt(builder, data_stack_ptr, dsp_ptr);

        // Branch based on result
        emit_branch(builder, check_block);

        // Check block: if true (-1), exit early
        builder.SetInsertPoint(check_block);
        emit_0branch(builder, data_stack_ptr, dsp_ptr, continue_block, exit_block);

        // Exit block: return with just 5
        builder.SetInsertPoint(exit_block);
        emit_exit(builder);

        // Continue block: would multiply by 2
        builder.SetInsertPoint(continue_block);
        emit_dup(builder, data_stack_ptr, dsp_ptr);
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: should be 5 (exited early because 5 < 10)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 5);
}

TEST_CASE("Simple IF-THEN simulation", "[primitives][control][practical]") {
    auto ctx = execute_test("test_if_then",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        using namespace llvm;
        Function* func = builder.GetInsertBlock()->getParent();

        // Create basic blocks
        BasicBlock* then_block = BasicBlock::Create(builder.getContext(), "then", func);
        BasicBlock* else_block = BasicBlock::Create(builder.getContext(), "else", func);
        BasicBlock* merge_block = BasicBlock::Create(builder.getContext(), "merge", func);

        // Test: IF 7 3 > THEN 100 ELSE 200 THEN
        push_values(builder, data_stack_ptr, dsp_ptr, {7, 3});

        // Compare: 7 < 3 = 0 (false, so 7 > 3 is true)
        emit_lt(builder, data_stack_ptr, dsp_ptr);

        // Invert the result (we want > but we have <)
        // Actually, let's just use the flag directly
        // 7 < 3 = false (0), so 0BRANCH will take the branch to THEN
        emit_0branch(builder, data_stack_ptr, dsp_ptr, then_block, else_block);

        // THEN block: push 100
        builder.SetInsertPoint(then_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {100});
        emit_branch(builder, merge_block);

        // ELSE block: push 200
        builder.SetInsertPoint(else_block);
        push_values(builder, data_stack_ptr, dsp_ptr, {200});
        emit_branch(builder, merge_block);

        // Merge
        builder.SetInsertPoint(merge_block);
    });

    // Check result: 7 < 3 = false (0), so 0BRANCH takes branch, pushing 100
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 100);
}

TEST_CASE("LIT primitive", "[primitives][literal][lit]") {
    auto ctx = execute_test("test_lit",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        // Push literal 42
        emit_lit(builder, data_stack_ptr, dsp_ptr, 42);

        // Push literal 100
        emit_lit(builder, data_stack_ptr, dsp_ptr, 100);

        // Add them
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: 42 + 100 = 142
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 142);
}

TEST_CASE("LIT with negative values", "[primitives][literal][lit]") {
    auto ctx = execute_test("test_lit_negative",
                            [](llvm::IRBuilder<>& builder,
                               llvm::Value* data_stack_ptr,
                               llvm::Value* dsp_ptr) {
        // Push literal -50
        emit_lit(builder, data_stack_ptr, dsp_ptr, -50);

        // Push literal 30
        emit_lit(builder, data_stack_ptr, dsp_ptr, 30);

        // Add them: -50 + 30 = -20
        emit_add(builder, data_stack_ptr, dsp_ptr);
    });

    // Check result: -20
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == -20);
}

TEST_CASE("EXECUTE primitive", "[primitives][execute]") {
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
    LLVMContext context;
    auto module = std::make_unique<Module>("test_execute", context);
    IRBuilder<> builder(context);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    // Create a helper function that we'll execute
    // This function adds 10 to the top of the stack
    Type* ctx_ptr_type = PointerType::get(context, 0);
    FunctionType* helper_func_type = FunctionType::get(
        builder.getVoidTy(),
        {ctx_ptr_type},
        false
    );

    Function* helper_func = Function::Create(
        helper_func_type,
        Function::ExternalLinkage,
        "helper_add_10",
        module.get()
    );

    // Build the helper function
    BasicBlock* helper_entry = BasicBlock::Create(context, "entry", helper_func);
    builder.SetInsertPoint(helper_entry);

    Value* helper_ctx_ptr = helper_func->getArg(0);
    Value* helper_data_stack_ptr = builder.CreateStructGEP(ctx_type, helper_ctx_ptr, 0, "data_stack_ptr");
    Value* helper_dsp_ptr = builder.CreateStructGEP(ctx_type, helper_ctx_ptr, 2, "dsp_ptr");

    // Push literal 10
    emit_lit(builder, helper_data_stack_ptr, helper_dsp_ptr, 10);

    // Add it to whatever is on the stack
    emit_add(builder, helper_data_stack_ptr, helper_dsp_ptr);

    builder.CreateRetVoid();

    // Now create the main test function
    FunctionType* main_func_type = FunctionType::get(
        builder.getVoidTy(),
        {ctx_ptr_type},
        false
    );

    Function* main_func = Function::Create(
        main_func_type,
        Function::ExternalLinkage,
        "test_execute_main",
        module.get()
    );

    BasicBlock* main_entry = BasicBlock::Create(context, "entry", main_func);
    builder.SetInsertPoint(main_entry);

    Value* main_ctx_ptr = main_func->getArg(0);
    Value* main_data_stack_ptr = builder.CreateStructGEP(ctx_type, main_ctx_ptr, 0, "data_stack_ptr");
    Value* main_dsp_ptr = builder.CreateStructGEP(ctx_type, main_ctx_ptr, 2, "dsp_ptr");

    // Push initial value 32
    emit_lit(builder, main_data_stack_ptr, main_dsp_ptr, 32);

    // Get function address as int64 and push it onto stack
    Value* func_ptr = builder.CreatePtrToInt(helper_func, builder.getInt64Ty(), "func_as_int");
    adjust_dsp(builder, main_dsp_ptr, 1);
    store_stack_at_depth(builder, main_data_stack_ptr, main_dsp_ptr, 0, func_ptr);

    // Execute the function using EXECUTE primitive
    emit_execute(builder, main_data_stack_ptr, main_dsp_ptr, main_ctx_ptr);

    builder.CreateRetVoid();

    // Verify both functions
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*helper_func, &error_stream)) {
        throw std::runtime_error("Helper function verification failed: " + error_str);
    }
    if (verifyFunction(*main_func, &error_stream)) {
        throw std::runtime_error("Main function verification failed: " + error_str);
    }

    // Create JIT execution engine
    std::string jit_error;
    EngineBuilder engine_builder(std::move(module));
    engine_builder.setErrorStr(&jit_error);
    engine_builder.setEngineKind(EngineKind::JIT);
    ExecutionEngine* engine = engine_builder.create();

    if (!engine) {
        throw std::runtime_error("Failed to create execution engine: " + jit_error);
    }

    // Create execution context
    ExecutionContext ctx;

    // Get function pointer and execute
    void (*test_func)(ExecutionContext*) =
        (void (*)(ExecutionContext*))engine->getFunctionAddress("test_execute_main");

    if (!test_func) {
        delete engine;
        throw std::runtime_error("Failed to get function address");
    }

    // Execute the function
    test_func(&ctx);

    delete engine;

    // Check result: 32 + 10 = 42
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);
}

TEST_CASE("EXECUTE with multiple calls", "[primitives][execute]") {
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
    LLVMContext context;
    auto module = std::make_unique<Module>("test_execute_multiple", context);
    IRBuilder<> builder(context);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    Type* ctx_ptr_type = PointerType::get(context, 0);

    // Create two helper functions
    // Function 1: Duplicate top of stack
    FunctionType* helper_func_type = FunctionType::get(
        builder.getVoidTy(),
        {ctx_ptr_type},
        false
    );

    Function* dup_func = Function::Create(
        helper_func_type,
        Function::ExternalLinkage,
        "helper_dup",
        module.get()
    );

    BasicBlock* dup_entry = BasicBlock::Create(context, "entry", dup_func);
    builder.SetInsertPoint(dup_entry);

    Value* dup_ctx_ptr = dup_func->getArg(0);
    Value* dup_data_stack_ptr = builder.CreateStructGEP(ctx_type, dup_ctx_ptr, 0, "data_stack_ptr");
    Value* dup_dsp_ptr = builder.CreateStructGEP(ctx_type, dup_ctx_ptr, 2, "dsp_ptr");

    emit_dup(builder, dup_data_stack_ptr, dup_dsp_ptr);
    builder.CreateRetVoid();

    // Function 2: Add top two values
    Function* add_func = Function::Create(
        helper_func_type,
        Function::ExternalLinkage,
        "helper_add",
        module.get()
    );

    BasicBlock* add_entry = BasicBlock::Create(context, "entry", add_func);
    builder.SetInsertPoint(add_entry);

    Value* add_ctx_ptr = add_func->getArg(0);
    Value* add_data_stack_ptr = builder.CreateStructGEP(ctx_type, add_ctx_ptr, 0, "data_stack_ptr");
    Value* add_dsp_ptr = builder.CreateStructGEP(ctx_type, add_ctx_ptr, 2, "dsp_ptr");

    emit_add(builder, add_data_stack_ptr, add_dsp_ptr);
    builder.CreateRetVoid();

    // Create main test function
    FunctionType* main_func_type = FunctionType::get(
        builder.getVoidTy(),
        {ctx_ptr_type},
        false
    );

    Function* main_func = Function::Create(
        main_func_type,
        Function::ExternalLinkage,
        "test_execute_multiple_main",
        module.get()
    );

    BasicBlock* main_entry = BasicBlock::Create(context, "entry", main_func);
    builder.SetInsertPoint(main_entry);

    Value* main_ctx_ptr = main_func->getArg(0);
    Value* main_data_stack_ptr = builder.CreateStructGEP(ctx_type, main_ctx_ptr, 0, "data_stack_ptr");
    Value* main_dsp_ptr = builder.CreateStructGEP(ctx_type, main_ctx_ptr, 2, "dsp_ptr");

    // Push initial value 5
    emit_lit(builder, main_data_stack_ptr, main_dsp_ptr, 5);

    // Push dup_func address and execute it (stack: 5 5)
    Value* dup_func_ptr = builder.CreatePtrToInt(dup_func, builder.getInt64Ty(), "dup_func_as_int");
    adjust_dsp(builder, main_dsp_ptr, 1);
    store_stack_at_depth(builder, main_data_stack_ptr, main_dsp_ptr, 0, dup_func_ptr);
    emit_execute(builder, main_data_stack_ptr, main_dsp_ptr, main_ctx_ptr);

    // Push add_func address and execute it (stack: 10)
    Value* add_func_ptr = builder.CreatePtrToInt(add_func, builder.getInt64Ty(), "add_func_as_int");
    adjust_dsp(builder, main_dsp_ptr, 1);
    store_stack_at_depth(builder, main_data_stack_ptr, main_dsp_ptr, 0, add_func_ptr);
    emit_execute(builder, main_data_stack_ptr, main_dsp_ptr, main_ctx_ptr);

    builder.CreateRetVoid();

    // Verify all functions
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*dup_func, &error_stream)) {
        throw std::runtime_error("Dup function verification failed: " + error_str);
    }
    if (verifyFunction(*add_func, &error_stream)) {
        throw std::runtime_error("Add function verification failed: " + error_str);
    }
    if (verifyFunction(*main_func, &error_stream)) {
        throw std::runtime_error("Main function verification failed: " + error_str);
    }

    // Create JIT execution engine
    std::string jit_error;
    EngineBuilder engine_builder(std::move(module));
    engine_builder.setErrorStr(&jit_error);
    engine_builder.setEngineKind(EngineKind::JIT);
    ExecutionEngine* engine = engine_builder.create();

    if (!engine) {
        throw std::runtime_error("Failed to create execution engine: " + jit_error);
    }

    // Create execution context
    ExecutionContext ctx;

    // Get function pointer and execute
    void (*test_func)(ExecutionContext*) =
        (void (*)(ExecutionContext*))engine->getFunctionAddress("test_execute_multiple_main");

    if (!test_func) {
        delete engine;
        throw std::runtime_error("Failed to get function address");
    }

    // Execute the function
    test_func(&ctx);

    delete engine;

    // Check result: 5 DUP + = 10
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 10);
}
