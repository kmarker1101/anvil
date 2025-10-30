#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"

using namespace anvil;

TEST_CASE("VARIABLE creates a variable", "[variable]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_variable", context);
    Compiler compiler(context, *module);

    // Test: VARIABLE X  X
    // Should create variable X and push its address
    ASTBuilder builder;
    auto ast = builder.parse("VARIABLE X  X");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have address of X on stack
    REQUIRE(ctx.dsp == 1);
    // The address should point into data_space
    uint64_t addr = ctx.data_stack[0];
    uint64_t data_space_start = reinterpret_cast<uint64_t>(&ctx.data_space[0]);
    uint64_t data_space_end = reinterpret_cast<uint64_t>(&ctx.data_space[DATA_SPACE_SIZE]);
    REQUIRE(addr >= data_space_start);
    REQUIRE(addr < data_space_end);

    delete engine;
}

TEST_CASE("VARIABLE stores and retrieves values", "[variable]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_variable_store", context);
    Compiler compiler(context, *module);

    // Test: VARIABLE X  42 X !  X @
    ASTBuilder builder;
    auto ast = builder.parse("VARIABLE X  42 X !  X @");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have value 42 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 42);

    delete engine;
}

TEST_CASE("Multiple VARIABLE declarations", "[variable]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_multiple_vars", context);
    Compiler compiler(context, *module);

    // Test: VARIABLE X  VARIABLE Y  10 X !  20 Y !  X @ Y @
    ASTBuilder builder;
    auto ast = builder.parse("VARIABLE X  VARIABLE Y  10 X !  20 Y !  X @ Y @");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have X's value (10) and Y's value (20) on stack
    REQUIRE(ctx.dsp == 2);
    REQUIRE(ctx.data_stack[0] == 10);
    REQUIRE(ctx.data_stack[1] == 20);

    delete engine;
}

TEST_CASE("CONSTANT creates a constant", "[constant]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_constant", context);
    Compiler compiler(context, *module);

    // Test: CONSTANT PI 314  PI
    ASTBuilder builder;
    auto ast = builder.parse("CONSTANT PI 314  PI");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have value 314 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 314);

    delete engine;
}

TEST_CASE("CONSTANT used in calculations", "[constant]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_constant_calc", context);
    Compiler compiler(context, *module);

    // Test: CONSTANT TWO 2  CONSTANT THREE 3  TWO THREE *
    ASTBuilder builder;
    auto ast = builder.parse("CONSTANT TWO 2  CONSTANT THREE 3  TWO THREE *");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have 2 * 3 = 6 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 6);

    delete engine;
}

TEST_CASE("VARIABLE and CONSTANT together", "[variable][constant]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_var_const", context);
    Compiler compiler(context, *module);

    // Test: VARIABLE X  CONSTANT TEN 10  TEN X !  X @
    ASTBuilder builder;
    auto ast = builder.parse("VARIABLE X  CONSTANT TEN 10  TEN X !  X @");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should have value 10 on stack
    REQUIRE(ctx.dsp == 1);
    REQUIRE(ctx.data_stack[0] == 10);

    delete engine;
}
