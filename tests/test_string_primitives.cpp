#include <catch2/catch_test_macros.hpp>
#include "test_helpers.h"
#include "primitives.h"
#include "execution_context_layout.h"

using namespace anvil;
using namespace anvil::test;

// Helper to set up memory buffer with test data
inline void setup_buffer(llvm::IRBuilder<>& builder,
                         llvm::Value* data_space_ptr,
                         const std::vector<uint8_t>& data,
                         uint64_t offset) {
    // data_space_ptr already points to the array - just use GEP directly
    for (size_t i = 0; i < data.size(); ++i) {
        llvm::Value* idx = builder.getInt64(offset + i);
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, idx);
        builder.CreateStore(builder.getInt8(data[i]), elem_ptr);
    }
}

TEST_BOTH_MODES("CMOVE copies non-overlapping regions", "[primitives][string][cmove]", {
    auto ctx = execute_test_mode("test_cmove", mode, [](llvm::IRBuilder<>& builder,
                                                   llvm::Value* data_stack_ptr,
                                                   llvm::Value* dsp_ptr) {
        // Get context pointer to access data_space
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // Setup source buffer at offset 0 with "HELLO" - inline the setup
        llvm::Value* idx0 = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, builder.getInt64(0));
        builder.CreateStore(builder.getInt8('H'), idx0);
        llvm::Value* idx1 = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, builder.getInt64(1));
        builder.CreateStore(builder.getInt8('E'), idx1);
        llvm::Value* idx2 = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, builder.getInt64(2));
        builder.CreateStore(builder.getInt8('L'), idx2);
        llvm::Value* idx3 = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, builder.getInt64(3));
        builder.CreateStore(builder.getInt8('L'), idx3);
        llvm::Value* idx4 = builder.CreateGEP(builder.getInt8Ty(), data_space_ptr, builder.getInt64(4));
        builder.CreateStore(builder.getInt8('O'), idx4);

        // Get addresses and push onto stack
        llvm::Value* src_addr = builder.CreatePtrToInt(data_space_ptr, builder.getInt64Ty(), "src_addr");
        llvm::Value* dest_addr = builder.CreateAdd(src_addr, builder.getInt64(100), "dest_addr");

        // Push to stack - adjust BEFORE storing!
        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, src_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, dest_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(5));

        // Execute CMOVE
        emit_cmove(builder, data_stack_ptr, dsp_ptr);
    });

    // Check that source was set up
    REQUIRE(ctx.data_space[0] == 'H');
    REQUIRE(ctx.data_space[1] == 'E');
    REQUIRE(ctx.data_space[2] == 'L');
    REQUIRE(ctx.data_space[3] == 'L');
    REQUIRE(ctx.data_space[4] == 'O');
    // Check that data was copied
    REQUIRE(ctx.data_space[100] == 'H');
    REQUIRE(ctx.data_space[101] == 'E');
    REQUIRE(ctx.data_space[102] == 'L');
    REQUIRE(ctx.data_space[103] == 'L');
    REQUIRE(ctx.data_space[104] == 'O');
})

TEST_BOTH_MODES("CMOVE> copies overlapping regions backward", "[primitives][string][cmove-backward]", {
    auto ctx = execute_test_mode("test_cmove_backward", mode, [](llvm::IRBuilder<>& builder,
                                                            llvm::Value* data_stack_ptr,
                                                            llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // Setup source buffer with "WORLD"
        std::vector<uint8_t> source = {'W', 'O', 'R', 'L', 'D'};
        setup_buffer(builder, data_space_ptr, source, 0);

        // Overlapping copy: copy to offset 2 (overlap test)
        llvm::Value* src_addr = builder.CreatePtrToInt(data_space_ptr, builder.getInt64Ty(), "src_addr");
        llvm::Value* dest_addr = builder.CreateAdd(src_addr, builder.getInt64(2), "dest_addr");

        // Push src, dest, count
        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, src_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, dest_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(5));

        // Execute CMOVE>
        emit_cmove_backward(builder, data_stack_ptr, dsp_ptr);
    });

    // Check overlapping copy worked correctly (backward copy preserves data)
    REQUIRE(ctx.dsp == 0);
    REQUIRE(ctx.data_space[2] == 'W');
    REQUIRE(ctx.data_space[3] == 'O');
    REQUIRE(ctx.data_space[4] == 'R');
    REQUIRE(ctx.data_space[5] == 'L');
    REQUIRE(ctx.data_space[6] == 'D');
})

TEST_BOTH_MODES("FILL fills buffer with character", "[primitives][string][fill]", {
    auto ctx = execute_test_mode("test_fill", mode, [](llvm::IRBuilder<>& builder,
                                                 llvm::Value* data_stack_ptr,
                                                   llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        llvm::Value* data_space_addr = builder.CreatePtrToInt(data_space_ptr, builder.getInt64Ty(), "data_space_addr");

        // Push addr, count, char onto stack
        // FILL ( c-addr u char -- )
        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, data_space_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(10));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64('X'));

        // Execute FILL
        emit_fill(builder, data_stack_ptr, dsp_ptr);
    });

    // Check that buffer was filled with 'X'
    REQUIRE(ctx.dsp == 0);
    for (int i = 0; i < 10; i++) {
        REQUIRE(ctx.data_space[i] == 'X');
    }
})

TEST_BOTH_MODES("FILL with zero (like ERASE)", "[primitives][string][fill]", {
    auto ctx = execute_test_mode("test_fill_zero", mode, [](llvm::IRBuilder<>& builder,
                                                       llvm::Value* data_stack_ptr,
                                                       llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // First fill with garbage
        std::vector<uint8_t> garbage = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF};
        setup_buffer(builder, data_space_ptr, garbage, 0);

        llvm::Value* data_space_addr = builder.CreatePtrToInt(data_space_ptr, builder.getInt64Ty(), "data_space_addr");

        // Now fill with zeros
        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, data_space_addr);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(5));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(0));

        emit_fill(builder, data_stack_ptr, dsp_ptr);
    });

    // Check that buffer was zeroed
    REQUIRE(ctx.dsp == 0);
    for (int i = 0; i < 5; i++) {
        REQUIRE(ctx.data_space[i] == 0);
    }
})

TEST_BOTH_MODES("COMPARE equal strings", "[primitives][string][compare]", {
    auto ctx = execute_test_mode("test_compare_equal", mode, [](llvm::IRBuilder<>& builder,
                                                           llvm::Value* data_stack_ptr,
                                                           llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // Setup two identical strings
        std::vector<uint8_t> str1 = {'T', 'E', 'S', 'T'};
        std::vector<uint8_t> str2 = {'T', 'E', 'S', 'T'};
        setup_buffer(builder, data_space_ptr, str1, 0);
        setup_buffer(builder, data_space_ptr, str2, 50);

        llvm::Type* i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
        llvm::Value* data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
        llvm::Value* addr1 = builder.CreatePtrToInt(data_space_i8, builder.getInt64Ty(), "addr1");
        llvm::Value* addr2 = builder.CreateAdd(addr1, builder.getInt64(50), "addr2");

        // Push c-addr1, u1, c-addr2, u2
        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr1);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(4));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr2);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(4));

        emit_compare(builder, data_stack_ptr, dsp_ptr);
    });

    // Result should be 0 (equal)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 0);
})

TEST_BOTH_MODES("COMPARE first string less than second", "[primitives][string][compare]", {
    auto ctx = execute_test_mode("test_compare_less", mode, [](llvm::IRBuilder<>& builder,
                                                          llvm::Value* data_stack_ptr,
                                                          llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // "ABC" < "XYZ"
        std::vector<uint8_t> str1 = {'A', 'B', 'C'};
        std::vector<uint8_t> str2 = {'X', 'Y', 'Z'};
        setup_buffer(builder, data_space_ptr, str1, 0);
        setup_buffer(builder, data_space_ptr, str2, 50);

        llvm::Type* i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
        llvm::Value* data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
        llvm::Value* addr1 = builder.CreatePtrToInt(data_space_i8, builder.getInt64Ty(), "addr1");
        llvm::Value* addr2 = builder.CreateAdd(addr1, builder.getInt64(50), "addr2");

        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr1);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(3));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr2);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(3));

        emit_compare(builder, data_stack_ptr, dsp_ptr);
    });

    // Result should be negative ('A' - 'X')
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] < 0);
})

TEST_BOTH_MODES("COMPARE first string greater than second", "[primitives][string][compare]", {
    auto ctx = execute_test_mode("test_compare_greater", mode, [](llvm::IRBuilder<>& builder,
                                                             llvm::Value* data_stack_ptr,
                                                             llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // "XYZ" > "ABC"
        std::vector<uint8_t> str1 = {'X', 'Y', 'Z'};
        std::vector<uint8_t> str2 = {'A', 'B', 'C'};
        setup_buffer(builder, data_space_ptr, str1, 0);
        setup_buffer(builder, data_space_ptr, str2, 50);

        llvm::Type* i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
        llvm::Value* data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
        llvm::Value* addr1 = builder.CreatePtrToInt(data_space_i8, builder.getInt64Ty(), "addr1");
        llvm::Value* addr2 = builder.CreateAdd(addr1, builder.getInt64(50), "addr2");

        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr1);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(3));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr2);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(3));

        emit_compare(builder, data_stack_ptr, dsp_ptr);
    });

    // Result should be positive ('X' - 'A')
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] > 0);
})

TEST_BOTH_MODES("COMPARE different lengths", "[primitives][string][compare]", {
    auto ctx = execute_test_mode("test_compare_diff_len", mode, [](llvm::IRBuilder<>& builder,
                                                              llvm::Value* data_stack_ptr,
                                                              llvm::Value* dsp_ptr) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        llvm::Value* ctx_ptr = func->getArg(0);

        llvm::StructType* ctx_type = create_execution_context_type(builder, builder.getContext());
        llvm::Value* data_space_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");

        // "TEST" (4) vs "TESTING" (7) - first 4 bytes equal
        std::vector<uint8_t> str1 = {'T', 'E', 'S', 'T'};
        std::vector<uint8_t> str2 = {'T', 'E', 'S', 'T', 'I', 'N', 'G'};
        setup_buffer(builder, data_space_ptr, str1, 0);
        setup_buffer(builder, data_space_ptr, str2, 50);

        llvm::Type* i8_ptr_type = llvm::PointerType::get(builder.getContext(), 0);
        llvm::Value* data_space_i8 = builder.CreateBitCast(data_space_ptr, i8_ptr_type, "data_space_i8");
        llvm::Value* addr1 = builder.CreatePtrToInt(data_space_i8, builder.getInt64Ty(), "addr1");
        llvm::Value* addr2 = builder.CreateAdd(addr1, builder.getInt64(50), "addr2");

        push_values(builder, data_stack_ptr, dsp_ptr, {});
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr1);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(4));
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, addr2);
        adjust_dsp(builder, dsp_ptr, 1);
        store_stack_at_depth(builder, data_stack_ptr, dsp_ptr, 0, builder.getInt64(7));

        emit_compare(builder, data_stack_ptr, dsp_ptr);
    });

    // Result should be negative (shorter string < longer string when prefixes match)
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] < 0);  // 4 - 7 = -3
})
