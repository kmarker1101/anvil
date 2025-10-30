#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"
#include <fstream>
#include <sstream>

using namespace anvil;

// Helper to load and compile stdlib, then execute test code
ExecutionContext execute_with_stdlib(const std::string& test_code,
                                     ExecutionMode mode = ExecutionMode::JIT) {
    initialize_llvm(mode);
    initialize_primitives();

    // Load stdlib
    std::ifstream stdlib_file("stdlib.fth");
    if (!stdlib_file.is_open()) {
        FAIL("Could not open stdlib.fth - run tests from build directory");
    }

    std::stringstream buffer;
    buffer << stdlib_file.rdbuf();
    std::string stdlib_source = buffer.str();
    stdlib_file.close();

    // Create LLVM context and module
    llvm::LLVMContext context;
    auto module = std::make_unique<llvm::Module>("test", context);
    Compiler compiler(context, *module);

    // Compile stdlib first
    ASTBuilder stdlib_builder;
    auto stdlib_ast = stdlib_builder.parse(stdlib_source);
    REQUIRE(stdlib_ast != nullptr);
    compiler.compile(stdlib_ast.get());

    // Now compile and execute test code
    ASTBuilder test_builder;
    auto test_ast = test_builder.parse(test_code);
    REQUIRE(test_ast != nullptr);

    llvm::Function* func = compiler.compile(test_ast.get());
    REQUIRE(func != nullptr);

    // Verify function
    std::string error_str;
    llvm::raw_string_ostream error_stream(error_str);
    if (llvm::verifyFunction(*func, &error_stream)) {
        FAIL("Function verification failed: " + error_str);
    }

    // Create execution engine
    std::string engine_error;
    AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
        std::move(module), mode, &engine_error);

    if (!engine) {
        FAIL("Failed to create execution engine: " + engine_error);
    }

    // Execute
    ExecutionContext ctx;
    engine->execute(func, &ctx);

    delete engine;
    return ctx;
}

TEST_CASE("Standard Library - NIP", "[stdlib][nip]") {
    SECTION("NIP removes second item") {
        auto ctx = execute_with_stdlib("5 3 NIP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }

    SECTION("NIP in JIT mode") {
        auto ctx = execute_with_stdlib("10 20 NIP", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 20);
    }

    SECTION("NIP in interpreter mode") {
        auto ctx = execute_with_stdlib("7 8 NIP", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }
}

TEST_CASE("Standard Library - TUCK", "[stdlib][tuck]") {
    SECTION("TUCK copies top under second") {
        auto ctx = execute_with_stdlib("5 10 TUCK");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 5);
        REQUIRE(ctx.data_stack[2] == 10);
    }

    SECTION("TUCK in JIT mode") {
        auto ctx = execute_with_stdlib("3 7 TUCK", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 7);
        REQUIRE(ctx.data_stack[1] == 3);
        REQUIRE(ctx.data_stack[2] == 7);
    }

    SECTION("TUCK in interpreter mode") {
        auto ctx = execute_with_stdlib("1 2 TUCK", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 2);
        REQUIRE(ctx.data_stack[1] == 1);
        REQUIRE(ctx.data_stack[2] == 2);
    }
}

TEST_CASE("Standard Library - 2DROP", "[stdlib][2drop]") {
    SECTION("2DROP removes top two items") {
        auto ctx = execute_with_stdlib("1 2 3 2DROP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("2DROP with exact two items") {
        auto ctx = execute_with_stdlib("5 10 2DROP");
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("2DROP in JIT mode") {
        auto ctx = execute_with_stdlib("100 200 300 2DROP", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("2DROP in interpreter mode") {
        auto ctx = execute_with_stdlib("7 8 9 2DROP", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }
}

TEST_CASE("Standard Library - NEGATE", "[stdlib][negate]") {
    SECTION("NEGATE positive number") {
        auto ctx = execute_with_stdlib("5 NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);
    }

    SECTION("NEGATE negative number") {
        auto ctx = execute_with_stdlib("-7 NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("NEGATE zero") {
        auto ctx = execute_with_stdlib("0 NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("NEGATE in JIT mode") {
        auto ctx = execute_with_stdlib("42 NEGATE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -42);
    }

    SECTION("NEGATE in interpreter mode") {
        auto ctx = execute_with_stdlib("100 NEGATE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -100);
    }

    SECTION("Double NEGATE") {
        auto ctx = execute_with_stdlib("15 NEGATE NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);
    }

    SECTION("NEGATE with arithmetic") {
        auto ctx = execute_with_stdlib("5 3 - NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -2);
    }

    SECTION("NEGATE multiple values") {
        auto ctx = execute_with_stdlib("10 NEGATE 20 NEGATE");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -10);
        REQUIRE(ctx.data_stack[1] == -20);
    }
}

TEST_CASE("Standard Library - ABS", "[stdlib][abs]") {
    SECTION("ABS of positive number") {
        auto ctx = execute_with_stdlib("42 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("ABS of negative number") {
        auto ctx = execute_with_stdlib("-17 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 17);
    }

    SECTION("ABS of zero") {
        auto ctx = execute_with_stdlib("0 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("ABS of -1") {
        auto ctx = execute_with_stdlib("-1 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("ABS of large negative") {
        auto ctx = execute_with_stdlib("-9999 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 9999);
    }

    SECTION("ABS in JIT mode") {
        auto ctx = execute_with_stdlib("-256 ABS", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 256);
    }

    SECTION("ABS in interpreter mode") {
        auto ctx = execute_with_stdlib("-512 ABS", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 512);
    }

    SECTION("ABS multiple values") {
        auto ctx = execute_with_stdlib("-5 ABS 10 ABS -3 ABS");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 10);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("ABS with arithmetic") {
        auto ctx = execute_with_stdlib("5 10 - ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("ABS of subtraction result") {
        auto ctx = execute_with_stdlib("3 8 - ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("Double ABS (idempotent)") {
        auto ctx = execute_with_stdlib("-25 ABS ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);
    }
}

TEST_CASE("Standard Library - OVER", "[stdlib][over]") {
    SECTION("OVER copies second to top") {
        auto ctx = execute_with_stdlib("10 20 OVER");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 10);
    }

    SECTION("OVER with negative numbers") {
        auto ctx = execute_with_stdlib("-5 7 OVER");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == -5);
        REQUIRE(ctx.data_stack[1] == 7);
        REQUIRE(ctx.data_stack[2] == -5);
    }

    SECTION("OVER with zero") {
        auto ctx = execute_with_stdlib("0 42 OVER");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 0);
        REQUIRE(ctx.data_stack[1] == 42);
        REQUIRE(ctx.data_stack[2] == 0);
    }

    SECTION("OVER in JIT mode") {
        auto ctx = execute_with_stdlib("3 9 OVER", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 3);
        REQUIRE(ctx.data_stack[1] == 9);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("OVER in interpreter mode") {
        auto ctx = execute_with_stdlib("15 25 OVER", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 15);
        REQUIRE(ctx.data_stack[1] == 25);
        REQUIRE(ctx.data_stack[2] == 15);
    }

    SECTION("OVER then add") {
        auto ctx = execute_with_stdlib("5 10 OVER +");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 15);
    }

    SECTION("Multiple OVER operations") {
        auto ctx = execute_with_stdlib("1 2 OVER OVER");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 1);
        REQUIRE(ctx.data_stack[3] == 2);
    }

    SECTION("OVER with three items") {
        auto ctx = execute_with_stdlib("1 2 3 OVER");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
        REQUIRE(ctx.data_stack[3] == 2);
    }

    SECTION("OVER then multiply") {
        auto ctx = execute_with_stdlib("4 5 OVER *");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 4);
        REQUIRE(ctx.data_stack[1] == 20);
    }

    SECTION("OVER in complex expression") {
        auto ctx = execute_with_stdlib("3 7 OVER + SWAP DROP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }
}

TEST_CASE("Standard Library - Combined operations", "[stdlib][combined]") {
    SECTION("NIP and TUCK together") {
        auto ctx = execute_with_stdlib("1 2 3 NIP TUCK");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 3);
        REQUIRE(ctx.data_stack[1] == 1);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("All stdlib words") {
        auto ctx = execute_with_stdlib("10 20 30 2DROP 5 TUCK NIP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 5);
    }

    SECTION("NEGATE with stack operations") {
        auto ctx = execute_with_stdlib("10 DUP NEGATE");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == -10);
    }

    SECTION("NEGATE in expression") {
        auto ctx = execute_with_stdlib("5 NEGATE 3 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -2);
    }

    SECTION("ABS and NEGATE together") {
        auto ctx = execute_with_stdlib("-10 ABS NEGATE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -10);
    }

    SECTION("ABS with stack operations") {
        auto ctx = execute_with_stdlib("-7 DUP ABS SWAP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 7);
        REQUIRE(ctx.data_stack[1] == -7);
    }

    SECTION("Computing distance with ABS") {
        auto ctx = execute_with_stdlib("3 8 - ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("OVER with NIP") {
        auto ctx = execute_with_stdlib("1 2 3 OVER NIP");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 2);
    }

    SECTION("OVER with TUCK") {
        auto ctx = execute_with_stdlib("5 10 OVER TUCK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 5);
        REQUIRE(ctx.data_stack[2] == 10);
        REQUIRE(ctx.data_stack[3] == 5);
    }
}
