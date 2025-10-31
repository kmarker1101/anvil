#include <catch2/catch_test_macros.hpp>
#include "test_helpers.h"
#include "primitives.h"
#include "stack.h"

using namespace anvil;
using namespace anvil::test;

// Helper to create test with full ExecutionContext access
inline ExecutionContext execute_test_input_mode(
    const std::string& test_name,
    ExecutionMode mode,
    std::function<void(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*)> emit_func)
{
    using namespace llvm;

    initialize_llvm_once();

    LLVMContext context;
    auto module = std::make_unique<Module>(test_name, context);
    IRBuilder<> builder(context);

    // Create function signature: void test_func(ExecutionContext*)
    Type* ctx_ptr_type = PointerType::get(context, 0);
    FunctionType* func_type = FunctionType::get(
        builder.getVoidTy(),
        {ctx_ptr_type},
        false
    );

    Function* func = Function::Create(
        func_type,
        Function::ExternalLinkage,
        test_name,
        module.get()
    );

    BasicBlock* entry = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    Value* ctx_ptr = func->getArg(0);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    ArrayType* data_space_array_type = ArrayType::get(builder.getInt8Ty(), DATA_SPACE_SIZE);
    ArrayType* tib_array_type = ArrayType::get(builder.getInt8Ty(), TIB_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,       // 0: data_stack
        stack_array_type,       // 1: return_stack
        data_space_array_type,  // 2: data_space
        tib_array_type,         // 3: tib
        builder.getInt64Ty(),   // 4: dsp
        builder.getInt64Ty(),   // 5: rsp
        builder.getInt64Ty(),   // 6: here
        builder.getInt64Ty(),   // 7: to_in
        builder.getInt64Ty()    // 8: num_tib
    }, "ExecutionContext");

    // Get pointers to all fields
    Value* data_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 0, "data_stack_ptr");
    Value* tib_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 3, "tib_ptr");
    Value* dsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 4, "dsp_ptr");
    Value* to_in_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 7, "to_in_ptr");
    Value* num_tib_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 8, "num_tib_ptr");

    // Call the user-provided emission function
    emit_func(builder, data_stack_ptr, tib_ptr, dsp_ptr, to_in_ptr, num_tib_ptr, ctx_ptr);

    builder.CreateRetVoid();

    // Verify the function
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*func, &error_stream)) {
        throw std::runtime_error("Function verification failed: " + error_str);
    }

    // Create execution engine
    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), mode, &engine_error);

    if (!engine) {
        throw std::runtime_error("Failed to create execution engine: " + engine_error);
    }

    ExecutionContext ctx;

    // Execute
    engine->execute(func, &ctx);

    delete engine;
    return ctx;
}

TEST_BOTH_MODES("TIB primitive returns buffer address", "[primitives][input][tib]", {
    auto ctx = execute_test_input_mode("test_tib", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value* tib_ptr,
           llvm::Value* dsp_ptr, llvm::Value*, llvm::Value*, llvm::Value*) {

        emit_tib(builder, data_stack_ptr, dsp_ptr, tib_ptr);
    });

    // TIB should push the address of the TIB buffer
    REQUIRE(ctx.dsp == 1);
    // Address should be the address of ctx.tib
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.tib[0]));
})

TEST_BOTH_MODES(">IN primitive returns parse position address", "[primitives][input][to-in]", {
    auto ctx = execute_test_input_mode("test_to_in", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value*,
           llvm::Value* dsp_ptr, llvm::Value* to_in_ptr, llvm::Value*, llvm::Value*) {

        emit_to_in(builder, data_stack_ptr, dsp_ptr, to_in_ptr);
    });

    // >IN should push the address of the to_in variable
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.to_in));
})

TEST_BOTH_MODES("#TIB primitive returns buffer length address", "[primitives][input][num-tib]", {
    auto ctx = execute_test_input_mode("test_num_tib", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value*,
           llvm::Value* dsp_ptr, llvm::Value*, llvm::Value* num_tib_ptr, llvm::Value*) {

        emit_num_tib(builder, data_stack_ptr, dsp_ptr, num_tib_ptr);
    });

    // #TIB should push the address of the num_tib variable
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.num_tib));
})

TEST_BOTH_MODES("SOURCE primitive returns TIB address and length", "[primitives][input][source]", {
    auto ctx = execute_test_input_mode("test_source", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value* tib_ptr,
           llvm::Value* dsp_ptr, llvm::Value*, llvm::Value* num_tib_ptr, llvm::Value* ctx_ptr) {

        // Set num_tib to 42
        builder.CreateStore(builder.getInt64(42), num_tib_ptr);

        emit_source(builder, data_stack_ptr, dsp_ptr, tib_ptr, num_tib_ptr);
    });

    // SOURCE should push TIB address and #TIB value
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.tib[0]));
    REQUIRE(ctx.data_stack[1] == 42);
})

TEST_BOTH_MODES("PARSE primitive with simple string", "[primitives][input][parse]", {
    auto ctx = execute_test_input_mode("test_parse_simple", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value* tib_ptr,
           llvm::Value* dsp_ptr, llvm::Value* to_in_ptr, llvm::Value* num_tib_ptr, llvm::Value*) {

        // Setup TIB with "HELLO WORLD"
        const char* test_str = "HELLO WORLD";
        for (int i = 0; test_str[i]; i++) {
            llvm::Value* idx = builder.getInt64(i);
            llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt8Ty(), tib_ptr, idx);
            builder.CreateStore(builder.getInt8(test_str[i]), elem_ptr);
        }

        // Set num_tib = 11, to_in = 0
        builder.CreateStore(builder.getInt64(11), num_tib_ptr);
        builder.CreateStore(builder.getInt64(0), to_in_ptr);

        // Push space delimiter
        llvm::Value* idx = builder.getInt64(0);
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx);
        builder.CreateStore(builder.getInt64(32), elem_ptr);  // space
        builder.CreateStore(builder.getInt64(1), dsp_ptr);

        emit_parse(builder, data_stack_ptr, dsp_ptr, tib_ptr, to_in_ptr, num_tib_ptr);
    });

    // PARSE should return address of "HELLO" and length 5
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.tib[0]));
    REQUIRE(ctx.data_stack[1] == 5);  // "HELLO"
    REQUIRE(ctx.to_in == 6);  // After space delimiter
})

TEST_BOTH_MODES("PARSE primitive continues from >IN", "[primitives][input][parse]", {
    auto ctx = execute_test_input_mode("test_parse_continue", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value* tib_ptr,
           llvm::Value* dsp_ptr, llvm::Value* to_in_ptr, llvm::Value* num_tib_ptr, llvm::Value*) {

        // Setup TIB with "HELLO WORLD"
        const char* test_str = "HELLO WORLD";
        for (int i = 0; test_str[i]; i++) {
            llvm::Value* idx = builder.getInt64(i);
            llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt8Ty(), tib_ptr, idx);
            builder.CreateStore(builder.getInt8(test_str[i]), elem_ptr);
        }

        // Set num_tib = 11, to_in = 6 (start at "WORLD")
        builder.CreateStore(builder.getInt64(11), num_tib_ptr);
        builder.CreateStore(builder.getInt64(6), to_in_ptr);

        // Push space delimiter
        llvm::Value* idx = builder.getInt64(0);
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx);
        builder.CreateStore(builder.getInt64(32), elem_ptr);  // space
        builder.CreateStore(builder.getInt64(1), dsp_ptr);

        emit_parse(builder, data_stack_ptr, dsp_ptr, tib_ptr, to_in_ptr, num_tib_ptr);
    });

    // PARSE should return address of "WORLD" and length 5
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.tib[6]));
    REQUIRE(ctx.data_stack[1] == 5);  // "WORLD"
    REQUIRE(ctx.to_in == 11);  // End of buffer
})

TEST_BOTH_MODES("PARSE primitive at end of buffer", "[primitives][input][parse]", {
    auto ctx = execute_test_input_mode("test_parse_end", mode,
        [](llvm::IRBuilder<>& builder, llvm::Value* data_stack_ptr, llvm::Value* tib_ptr,
           llvm::Value* dsp_ptr, llvm::Value* to_in_ptr, llvm::Value* num_tib_ptr, llvm::Value*) {

        // Setup TIB with "TEST"
        const char* test_str = "TEST";
        for (int i = 0; test_str[i]; i++) {
            llvm::Value* idx = builder.getInt64(i);
            llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt8Ty(), tib_ptr, idx);
            builder.CreateStore(builder.getInt8(test_str[i]), elem_ptr);
        }

        // Set num_tib = 4, to_in = 0
        builder.CreateStore(builder.getInt64(4), num_tib_ptr);
        builder.CreateStore(builder.getInt64(0), to_in_ptr);

        // Push space delimiter
        llvm::Value* idx = builder.getInt64(0);
        llvm::Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx);
        builder.CreateStore(builder.getInt64(32), elem_ptr);  // space
        builder.CreateStore(builder.getInt64(1), dsp_ptr);

        emit_parse(builder, data_stack_ptr, dsp_ptr, tib_ptr, to_in_ptr, num_tib_ptr);
    });

    // PARSE should return "TEST" (delimiter not found, parsed to end)
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == reinterpret_cast<int64_t>(&ctx.tib[0]));
    REQUIRE(ctx.data_stack[1] == 4);  // "TEST"
    REQUIRE(ctx.to_in == 4);  // At end
})
