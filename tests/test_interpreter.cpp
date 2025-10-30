#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"

using namespace anvil;

// Helper to compile and execute Forth code in interpreter mode
ExecutionContext execute_forth_interpreted(const std::string& source) {
    initialize_llvm(ExecutionMode::Interpreter);
    initialize_primitives();

    // Parse to AST
    ASTBuilder builder;
    auto ast = builder.parse(source);

    // Create LLVM module
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test", context);

    // Compile
    Compiler compiler(context, *module);
    llvm::Function* func = compiler.compile(ast.get());

    REQUIRE(func != nullptr);

    // Verify function
    std::string error_str;
    llvm::raw_string_ostream error_stream(error_str);
    if (llvm::verifyFunction(*func, &error_stream)) {
        FAIL("Function verification failed: " + error_str);
    }

    // Create execution engine in interpreter mode
    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), ExecutionMode::Interpreter, &engine_error);

    if (!engine) {
        FAIL("Failed to create execution engine: " + engine_error);
    }

    // Execute
    ExecutionContext ctx;
    engine->execute(func, &ctx);

    delete engine;

    return ctx;
}

TEST_CASE("Interpreter executes literals", "[interpreter][literal]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Single literal") {
        auto ctx = execute_forth_interpreted("42");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Multiple literals") {
        auto ctx = execute_forth_interpreted("10 20 30");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
    }
}

TEST_CASE("Interpreter executes arithmetic primitives", "[interpreter][primitives][arithmetic]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Addition") {
        auto ctx = execute_forth_interpreted("5 3 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("Subtraction") {
        auto ctx = execute_forth_interpreted("10 3 -");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("Multiplication") {
        auto ctx = execute_forth_interpreted("6 7 *");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }
}

TEST_CASE("Interpreter executes stack primitives", "[interpreter][primitives][stack]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("DUP") {
        auto ctx = execute_forth_interpreted("42 DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);
    }

    SECTION("SWAP") {
        auto ctx = execute_forth_interpreted("10 20 SWAP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 20);
        REQUIRE(ctx.data_stack[1] == 10);
    }
}

TEST_CASE("Interpreter executes control flow", "[interpreter][control][if]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("IF...THEN true case") {
        auto ctx = execute_forth_interpreted("-1 IF 42 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("IF...ELSE...THEN false branch") {
        auto ctx = execute_forth_interpreted("0 IF 100 ELSE 200 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 200);
    }
}

TEST_CASE("Interpreter executes loops", "[interpreter][loops]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple DO...LOOP") {
        // Sum from 0 to 4: 0+1+2+3+4 = 10
        auto ctx = execute_forth_interpreted("0  5 0 DO I + LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }
}

TEST_CASE("Interpreter executes user-defined words", "[interpreter][definitions]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple user-defined word") {
        auto ctx = execute_forth_interpreted(": SQUARE DUP * ; 5 SQUARE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);
    }
}

TEST_CASE("Interpreter executes CR primitive", "[interpreter][cr][io]") {
    // Clear dictionary before tests
    global_dictionary.clear();

    SECTION("CR alone") {
        auto ctx = execute_forth_interpreted("CR");
        // CR doesn't affect the stack
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("CR with values on stack") {
        auto ctx = execute_forth_interpreted("42 CR");
        // CR doesn't consume or produce values
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Multiple CRs") {
        auto ctx = execute_forth_interpreted("1 2 3 CR CR CR");
        // Stack should be unchanged
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("CR between values") {
        auto ctx = execute_forth_interpreted("10 CR 20 CR 30");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
    }

    SECTION("CR with arithmetic") {
        auto ctx = execute_forth_interpreted("5 3 + CR 2 *");
        // 5 + 3 = 8, CR, 8 * 2 = 16
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 16);
    }
}

// Clean up dictionary after tests
TEST_CASE("Interpreter cleanup", "[.cleanup]") {
    global_dictionary.clear();
}
