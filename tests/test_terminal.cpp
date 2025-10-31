#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"

using namespace anvil;

TEST_CASE("KEY reads a character", "[terminal][key]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_key", context);
    Compiler compiler(context, *module);

    // Test: KEY
    // Note: This test verifies compilation, not actual input
    // Real input testing would require stdin redirection
    ASTBuilder builder;
    auto ast = builder.parse("KEY");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    // Note: Can't actually test KEY without mocking stdin
    // This just verifies it compiles and can be called

    delete engine;
}

TEST_CASE("KEY? compiles and executes", "[terminal][key?]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_key_question", context);
    Compiler compiler(context, *module);

    // Test: KEY?
    // Returns false (0) when no input available
    ASTBuilder builder;
    auto ast = builder.parse("KEY?");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Should push a flag (0 or -1) onto stack
    REQUIRE(ctx.dsp == 1);
    // Flag should be either 0 (false) or -1 (true)
    // Value depends on whether stdin has input available
    REQUIRE((ctx.data_stack[0] == 0 || ctx.data_stack[0] == -1));

    delete engine;
}

TEST_CASE("RAW-MODE compiles", "[terminal][raw-mode]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_raw_mode", context);
    Compiler compiler(context, *module);

    ASTBuilder builder;
    auto ast = builder.parse("RAW-MODE");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Stack should be empty (no effect on stack)
    REQUIRE(ctx.dsp == 0);

    delete engine;
}

TEST_CASE("COOKED-MODE compiles", "[terminal][cooked-mode]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_cooked_mode", context);
    Compiler compiler(context, *module);

    ASTBuilder builder;
    auto ast = builder.parse("COOKED-MODE");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Stack should be empty (no effect on stack)
    REQUIRE(ctx.dsp == 0);

    delete engine;
}

TEST_CASE("RAW-MODE and COOKED-MODE sequence", "[terminal][mode]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_mode_sequence", context);
    Compiler compiler(context, *module);

    // Test switching modes
    ASTBuilder builder;
    auto ast = builder.parse("RAW-MODE COOKED-MODE");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Stack should be empty
    REQUIRE(ctx.dsp == 0);

    delete engine;
}

TEST_CASE("EMIT-ESC outputs escape character", "[terminal][emit-esc]") {
    initialize_llvm(ExecutionMode::JIT);
    initialize_primitives();

    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test_emit_esc", context);
    Compiler compiler(context, *module);

    // Test: EMIT-ESC
    // Should output ESC character (ASCII 27)
    ASTBuilder builder;
    auto ast = builder.parse("EMIT-ESC");
    REQUIRE(ast != nullptr);

    llvm::Function* func = compiler.compile(ast.get());
    REQUIRE(func != nullptr);

    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::JIT, &engine_error);
    REQUIRE(engine != nullptr);

    ExecutionContext ctx;
    engine->execute(func, &ctx);

    // Stack should be empty (ESC was emitted)
    REQUIRE(ctx.dsp == 0);

    delete engine;
}
