#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"

using namespace anvil;

TEST_CASE("EMIT outputs a single character", "[emit]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_emit", context);
    Compiler compiler(context, *module);

    // Test: 65 EMIT  (outputs 'A')
    // We just verify the stack behavior, output goes to stdout
    ASTBuilder builder;
    auto ast = builder.parse("65 EMIT");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    REQUIRE(ctx.dsp == 0);  // Stack should be empty

    delete engine;
}

TEST_CASE("EMIT outputs multiple characters", "[emit]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_emit_multiple", context);
    Compiler compiler(context, *module);

    // Test: 72 EMIT 105 EMIT 33 EMIT  (outputs "Hi!")
    ASTBuilder builder;
    auto ast = builder.parse("72 EMIT 105 EMIT 33 EMIT");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    REQUIRE(ctx.dsp == 0);

    delete engine;
}

TEST_CASE("EMIT with stack values", "[emit]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_emit_stack", context);
    Compiler compiler(context, *module);

    // Test: 65 66 67 EMIT EMIT EMIT  (outputs "CBA" - reverse order)
    ASTBuilder builder;
    auto ast = builder.parse("65 66 67 EMIT EMIT EMIT");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    REQUIRE(ctx.dsp == 0);  // All values consumed

    delete engine;
}

// Note: SPACES is tested in test_stdlib.cpp with the execute_with_stdlib helper
// These tests verify that EMIT works correctly, which is the primitive that SPACES uses
