#ifndef ANVIL_TEST_HELPERS_H
#define ANVIL_TEST_HELPERS_H

#include "stack.h"
#include "primitives.h"
#include "execution_engine.h"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>

#include <functional>
#include <memory>
#include <string>

namespace anvil {
namespace test {

// Initialize LLVM once for all tests
inline void initialize_llvm_once() {
    static bool initialized = false;
    if (!initialized) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        initialized = true;
    }
}

// Helper to create and execute a test function
// The emit_func callback should generate IR using the provided builder and pointers
inline ExecutionContext execute_test(
    const std::string& test_name,
    std::function<void(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*)> emit_func)
{
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
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

    // Create entry basic block
    BasicBlock* entry = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    // Get the ExecutionContext* parameter
    Value* ctx_ptr = func->getArg(0);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    // Get pointers to data_stack and dsp
    Value* data_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 0, "data_stack_ptr");
    Value* dsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 2, "dsp_ptr");

    // Call the user-provided emission function
    emit_func(builder, data_stack_ptr, dsp_ptr);

    // Return
    builder.CreateRetVoid();

    // Verify the function
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*func, &error_stream)) {
        throw std::runtime_error("Function verification failed: " + error_str);
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
        (void (*)(ExecutionContext*))engine->getFunctionAddress(test_name);

    if (!test_func) {
        delete engine;
        throw std::runtime_error("Failed to get function address");
    }

    // Execute the function
    test_func(&ctx);

    delete engine;
    return ctx;
}

// Helper to create and execute a test function with return stack access
// The emit_func callback receives data_stack, return_stack, dsp, and rsp pointers
inline ExecutionContext execute_test_rstack(
    const std::string& test_name,
    std::function<void(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*)> emit_func)
{
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
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

    // Create entry basic block
    BasicBlock* entry = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    // Get the ExecutionContext* parameter
    Value* ctx_ptr = func->getArg(0);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    // Get pointers to data_stack, return_stack, dsp, and rsp
    Value* data_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 0, "data_stack_ptr");
    Value* return_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 1, "return_stack_ptr");
    Value* dsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 2, "dsp_ptr");
    Value* rsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 3, "rsp_ptr");

    // Call the user-provided emission function
    emit_func(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

    // Return
    builder.CreateRetVoid();

    // Verify the function
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*func, &error_stream)) {
        throw std::runtime_error("Function verification failed: " + error_str);
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
        (void (*)(ExecutionContext*))engine->getFunctionAddress(test_name);

    if (!test_func) {
        delete engine;
        throw std::runtime_error("Failed to get function address");
    }

    // Execute the function
    test_func(&ctx);

    delete engine;
    return ctx;
}

// Helper to push values onto the stack before executing primitives
inline void push_values(llvm::IRBuilder<>& builder,
                       llvm::Value* data_stack_ptr,
                       llvm::Value* dsp_ptr,
                       const std::vector<int64_t>& values)
{
    using namespace llvm;

    // Push each value
    for (size_t i = 0; i < values.size(); ++i) {
        Value* idx = builder.getInt64(i);
        Value* elem_ptr = builder.CreateGEP(builder.getInt64Ty(), data_stack_ptr, idx, "elem_ptr");
        builder.CreateStore(builder.getInt64(values[i]), elem_ptr);
    }

    // Set dsp to number of values pushed
    builder.CreateStore(builder.getInt64(values.size()), dsp_ptr);
}

// Helper to execute test in a specific mode (JIT or Interpreter)
inline ExecutionContext execute_test_mode(
    const std::string& test_name,
    ExecutionMode mode,
    std::function<void(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*)> emit_func)
{
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
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

    // Create entry basic block
    BasicBlock* entry = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    // Get the ExecutionContext* parameter
    Value* ctx_ptr = func->getArg(0);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    // Get pointers to data_stack and dsp
    Value* data_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 0, "data_stack_ptr");
    Value* dsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 2, "dsp_ptr");

    // Call the user-provided emission function
    emit_func(builder, data_stack_ptr, dsp_ptr);

    // Return
    builder.CreateRetVoid();

    // Verify the function
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*func, &error_stream)) {
        throw std::runtime_error("Function verification failed: " + error_str);
    }

    // Create execution engine based on mode
    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), mode, &engine_error);

    if (!engine) {
        throw std::runtime_error("Failed to create execution engine: " + engine_error);
    }

    // Create execution context
    ExecutionContext ctx;

    // Execute
    engine->execute(func, &ctx);

    delete engine;
    return ctx;
}

// Helper to execute test with return stack in a specific mode
inline ExecutionContext execute_test_rstack_mode(
    const std::string& test_name,
    ExecutionMode mode,
    std::function<void(llvm::IRBuilder<>&, llvm::Value*, llvm::Value*, llvm::Value*, llvm::Value*)> emit_func)
{
    using namespace llvm;

    initialize_llvm_once();

    // Create LLVM context and module
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

    // Create entry basic block
    BasicBlock* entry = BasicBlock::Create(context, "entry", func);
    builder.SetInsertPoint(entry);

    // Get the ExecutionContext* parameter
    Value* ctx_ptr = func->getArg(0);

    // Define ExecutionContext struct type
    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    StructType* ctx_type = StructType::create(context, {
        stack_array_type,      // data_stack
        stack_array_type,      // return_stack
        builder.getInt64Ty(),  // dsp
        builder.getInt64Ty()   // rsp
    }, "ExecutionContext");

    // Get pointers to all stack components
    Value* data_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 0, "data_stack_ptr");
    Value* return_stack_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 1, "return_stack_ptr");
    Value* dsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 2, "dsp_ptr");
    Value* rsp_ptr = builder.CreateStructGEP(ctx_type, ctx_ptr, 3, "rsp_ptr");

    // Call the user-provided emission function
    emit_func(builder, data_stack_ptr, return_stack_ptr, dsp_ptr, rsp_ptr);

    // Return
    builder.CreateRetVoid();

    // Verify the function
    std::string error_str;
    raw_string_ostream error_stream(error_str);
    if (verifyFunction(*func, &error_stream)) {
        throw std::runtime_error("Function verification failed: " + error_str);
    }

    // Create execution engine based on mode
    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), mode, &engine_error);

    if (!engine) {
        throw std::runtime_error("Failed to create execution engine: " + engine_error);
    }

    // Create execution context
    ExecutionContext ctx;

    // Execute
    engine->execute(func, &ctx);

    delete engine;
    return ctx;
}

} // namespace test
} // namespace anvil

// Macro to run a test in both JIT and Interpreter modes
#define TEST_BOTH_MODES(test_name, tags, test_body) \
    TEST_CASE(test_name " [JIT]", tags " [jit]") { \
        SECTION("JIT mode") { \
            ExecutionMode mode = ExecutionMode::JIT; \
            test_body \
        } \
    } \
    TEST_CASE(test_name " [Interpreter]", tags " [interpreter]") { \
        SECTION("Interpreter mode") { \
            ExecutionMode mode = ExecutionMode::Interpreter; \
            test_body \
        } \
    }

#endif // ANVIL_TEST_HELPERS_H
