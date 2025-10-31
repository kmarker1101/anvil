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

TEST_CASE("Standard Library - / (division)", "[stdlib][divide]") {
    SECTION("Exact division") {
        auto ctx = execute_with_stdlib("20 4 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("Division with remainder (truncates)") {
        auto ctx = execute_with_stdlib("7 2 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);  // Quotient only, remainder discarded
    }

    SECTION("Division resulting in 1") {
        auto ctx = execute_with_stdlib("9 9 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("Division with negative dividend") {
        auto ctx = execute_with_stdlib("-20 4 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);
    }

    SECTION("Division with negative divisor") {
        auto ctx = execute_with_stdlib("20 -4 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);
    }

    SECTION("Division with both negative") {
        auto ctx = execute_with_stdlib("-20 -4 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("Division by 1") {
        auto ctx = execute_with_stdlib("42 1 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Zero divided by number") {
        auto ctx = execute_with_stdlib("0 5 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Large division") {
        auto ctx = execute_with_stdlib("1000 10 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("Division in JIT mode") {
        auto ctx = execute_with_stdlib("100 5 /", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 20);
    }

    SECTION("Division in Interpreter mode") {
        auto ctx = execute_with_stdlib("100 5 /", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 20);
    }

    SECTION("Multiple divisions") {
        auto ctx = execute_with_stdlib("100 5 / 2 /");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);  // (100/5)/2 = 20/2 = 10
    }

    SECTION("Division in expression") {
        auto ctx = execute_with_stdlib("50 10 / 3 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);  // (50/10)+3 = 5+3 = 8
    }

    SECTION("Verify / only returns quotient (not remainder)") {
        auto ctx = execute_with_stdlib("12 9 /");
        REQUIRE(ctx.dsp == 1);  // Only one value on stack
        REQUIRE(ctx.data_stack[0] == 1);  // Quotient (remainder 3 is discarded)
    }

    SECTION("Compare / vs /MOD behavior") {
        auto ctx = execute_with_stdlib("17 5 / 17 5 /MOD");
        REQUIRE(ctx.dsp == 3);  // / leaves 1 value, /MOD leaves 2 values
        REQUIRE(ctx.data_stack[0] == 3);  // / result (quotient)
        REQUIRE(ctx.data_stack[1] == 2);  // /MOD remainder
        REQUIRE(ctx.data_stack[2] == 3);  // /MOD quotient
    }
}

TEST_CASE("Standard Library - MOD (modulo)", "[stdlib][mod]") {
    SECTION("Simple modulo") {
        auto ctx = execute_with_stdlib("12 5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 2);  // 12 mod 5 = 2
    }

    SECTION("Modulo with no remainder") {
        auto ctx = execute_with_stdlib("20 5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // 20 mod 5 = 0
    }

    SECTION("Modulo smaller than divisor") {
        auto ctx = execute_with_stdlib("3 5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);  // 3 mod 5 = 3
    }

    SECTION("Modulo by 2 (even test)") {
        auto ctx = execute_with_stdlib("10 2 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // 10 is even
    }

    SECTION("Modulo by 2 (odd test)") {
        auto ctx = execute_with_stdlib("11 2 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);  // 11 is odd
    }

    SECTION("Modulo with negative dividend") {
        auto ctx = execute_with_stdlib("-12 5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -2);  // -12 mod 5 = -2
    }

    SECTION("Modulo with negative divisor") {
        auto ctx = execute_with_stdlib("12 -5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 2);  // 12 mod -5 = 2
    }

    SECTION("Modulo with both negative") {
        auto ctx = execute_with_stdlib("-12 -5 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -2);  // -12 mod -5 = -2
    }

    SECTION("Modulo by 10 (last digit)") {
        auto ctx = execute_with_stdlib("12345 10 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);  // Last digit
    }

    SECTION("Modulo by 100") {
        auto ctx = execute_with_stdlib("12345 100 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 45);  // Last two digits
    }

    SECTION("Modulo large numbers") {
        auto ctx = execute_with_stdlib("1000 7 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 6);  // 1000 mod 7 = 6
    }

    SECTION("Multiple modulo operations") {
        auto ctx = execute_with_stdlib("17 5 MOD 23 7 MOD");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);  // 17 mod 5 = 2
        REQUIRE(ctx.data_stack[1] == 2);  // 23 mod 7 = 2
    }

    SECTION("MOD in JIT mode") {
        auto ctx = execute_with_stdlib("17 5 MOD", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 2);
    }

    SECTION("MOD in Interpreter mode") {
        auto ctx = execute_with_stdlib("17 5 MOD", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 2);
    }

    SECTION("Verify MOD only returns remainder") {
        auto ctx = execute_with_stdlib("12 5 MOD");
        REQUIRE(ctx.dsp == 1);  // Only one value on stack
        REQUIRE(ctx.data_stack[0] == 2);  // Remainder (quotient 2 is discarded)
    }

    SECTION("Compare MOD vs /MOD behavior") {
        auto ctx = execute_with_stdlib("17 5 MOD 17 5 /MOD");
        REQUIRE(ctx.dsp == 3);  // MOD leaves 1 value, /MOD leaves 2 values
        REQUIRE(ctx.data_stack[0] == 2);  // MOD result (remainder)
        REQUIRE(ctx.data_stack[1] == 2);  // /MOD remainder
        REQUIRE(ctx.data_stack[2] == 3);  // /MOD quotient
    }

    SECTION("MOD and / together reconstruct number") {
        // n = (n/d)*d + (n mod d)
        auto ctx = execute_with_stdlib("17 5 DUP >R / 5 * 17 5 MOD + R> DROP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 17);  // Reconstructed original
    }

    SECTION("MOD for range checking") {
        // Common use: wrap around in range [0, n)
        auto ctx = execute_with_stdlib("15 10 MOD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);  // 15 wrapped to [0,10) = 5
    }
}

TEST_CASE("Standard Library - 1+ (increment)", "[stdlib][1+]") {
    SECTION("Increment positive number") {
        auto ctx = execute_with_stdlib("5 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 6);
    }

    SECTION("Increment from zero") {
        auto ctx = execute_with_stdlib("0 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("Increment negative number to zero") {
        auto ctx = execute_with_stdlib("-1 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Increment negative number") {
        auto ctx = execute_with_stdlib("-5 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -4);
    }

    SECTION("Increment large number") {
        auto ctx = execute_with_stdlib("999 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1000);
    }

    SECTION("Multiple increments") {
        auto ctx = execute_with_stdlib("5 1+ 1+ 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);  // 5+1+1+1 = 8
    }

    SECTION("Increment in expression") {
        auto ctx = execute_with_stdlib("20 1+ 5 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 26);  // (20+1)+5 = 21+5 = 26
    }

    SECTION("Increment with DUP") {
        auto ctx = execute_with_stdlib("8 DUP 1+");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 8);
        REQUIRE(ctx.data_stack[1] == 9);  // Original 8, then 8+1=9
    }

    SECTION("Increment in JIT mode") {
        auto ctx = execute_with_stdlib("42 1+", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 43);
    }

    SECTION("Increment in Interpreter mode") {
        auto ctx = execute_with_stdlib("42 1+", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 43);
    }

    SECTION("Increment used in counter pattern") {
        // Simulating count-up: 1 2 3
        auto ctx = execute_with_stdlib("0 1+ DUP 1+ DUP 1+");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("Increment with addition") {
        auto ctx = execute_with_stdlib("10 1+ 5 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 16);  // (10+1)+5 = 11+5 = 16
    }

    SECTION("Verify 1+ is equivalent to 1 +") {
        auto ctx = execute_with_stdlib("15 1+ 15 1 +");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 16);  // 15 1+
        REQUIRE(ctx.data_stack[1] == 16);  // 15 1 +
    }

    SECTION("1+ and 1- are inverses") {
        auto ctx = execute_with_stdlib("42 1+ 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);  // Back to original
    }

    SECTION("1- and 1+ are inverses") {
        auto ctx = execute_with_stdlib("42 1- 1+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);  // Back to original
    }
}

TEST_CASE("Standard Library - 1- (decrement)", "[stdlib][1-]") {
    SECTION("Decrement positive number") {
        auto ctx = execute_with_stdlib("5 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 4);
    }

    SECTION("Decrement to zero") {
        auto ctx = execute_with_stdlib("1 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Decrement zero to negative") {
        auto ctx = execute_with_stdlib("0 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);
    }

    SECTION("Decrement negative number") {
        auto ctx = execute_with_stdlib("-5 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -6);
    }

    SECTION("Decrement large number") {
        auto ctx = execute_with_stdlib("1000 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 999);
    }

    SECTION("Multiple decrements") {
        auto ctx = execute_with_stdlib("10 1- 1- 1-");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);  // 10-1-1-1 = 7
    }

    SECTION("Decrement in expression") {
        auto ctx = execute_with_stdlib("20 1- 5 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);  // (20-1)+5 = 19+5 = 24
    }

    SECTION("Decrement with DUP") {
        auto ctx = execute_with_stdlib("8 DUP 1-");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 8);
        REQUIRE(ctx.data_stack[1] == 7);  // Original 8, then 8-1=7
    }

    SECTION("Decrement in JIT mode") {
        auto ctx = execute_with_stdlib("42 1-", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 41);
    }

    SECTION("Decrement in Interpreter mode") {
        auto ctx = execute_with_stdlib("42 1-", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 41);
    }

    SECTION("Decrement used in loop-like pattern") {
        // Simulating countdown: 3 2 1
        auto ctx = execute_with_stdlib("3 DUP 1- DUP 1-");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 3);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 1);
    }

    SECTION("Decrement with subtraction") {
        auto ctx = execute_with_stdlib("10 1- 5 -");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 4);  // (10-1)-5 = 9-5 = 4
    }

    SECTION("Verify 1- is equivalent to 1 -") {
        auto ctx = execute_with_stdlib("15 1- 15 1 -");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 14);  // 15 1-
        REQUIRE(ctx.data_stack[1] == 14);  // 15 1 -
    }
}

TEST_CASE("Standard Library - ?DUP", "[stdlib][qdup]") {
    SECTION("?DUP with zero leaves single zero") {
        auto ctx = execute_with_stdlib("0 ?DUP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Single zero, not duplicated
    }

    SECTION("?DUP with positive number duplicates it") {
        auto ctx = execute_with_stdlib("42 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);  // Duplicated
    }

    SECTION("?DUP with negative number duplicates it") {
        auto ctx = execute_with_stdlib("-17 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -17);
        REQUIRE(ctx.data_stack[1] == -17);  // Duplicated
    }

    SECTION("?DUP with 1 duplicates") {
        auto ctx = execute_with_stdlib("1 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 1);
    }

    SECTION("?DUP with -1 duplicates") {
        auto ctx = execute_with_stdlib("-1 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -1);
        REQUIRE(ctx.data_stack[1] == -1);
    }

    SECTION("Multiple ?DUP on zero") {
        auto ctx = execute_with_stdlib("0 ?DUP ?DUP ?DUP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Still just one zero
    }

    SECTION("Multiple ?DUP on non-zero") {
        auto ctx = execute_with_stdlib("5 ?DUP ?DUP");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 5);
        REQUIRE(ctx.data_stack[2] == 5);  // Tripled (5 -> 5,5 -> 5,5,5)
    }

    SECTION("?DUP in conditional pattern (non-zero)") {
        // Common pattern: value ?DUP IF ... operate on duplicate ... THEN
        // But IF consumes the duplicate, so need value below
        auto ctx = execute_with_stdlib("10 20 OVER ?DUP IF DROP + THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 30);  // 10 + 20
    }

    SECTION("?DUP in conditional pattern (zero)") {
        // With zero, ?DUP doesn't duplicate, leaves one zero
        // The typical pattern is: n ?DUP IF ... ELSE ... THEN
        // Since IF consumes the value, we just verify ?DUP behavior
        auto ctx = execute_with_stdlib("0 ?DUP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Just one zero
    }

    SECTION("?DUP with expression result (non-zero)") {
        auto ctx = execute_with_stdlib("3 4 * ?DUP");  // 12 ?DUP
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 12);
        REQUIRE(ctx.data_stack[1] == 12);
    }

    SECTION("?DUP with expression result (zero)") {
        auto ctx = execute_with_stdlib("5 5 - ?DUP");  // 0 ?DUP
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("?DUP with large positive number") {
        auto ctx = execute_with_stdlib("999999 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 999999);
        REQUIRE(ctx.data_stack[1] == 999999);
    }

    SECTION("?DUP with large negative number") {
        auto ctx = execute_with_stdlib("-999999 ?DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -999999);
        REQUIRE(ctx.data_stack[1] == -999999);
    }

    SECTION("?DUP on stack with multiple items (zero on top)") {
        auto ctx = execute_with_stdlib("1 2 3 0 ?DUP");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
        REQUIRE(ctx.data_stack[3] == 0);  // Not duplicated
    }

    SECTION("?DUP on stack with multiple items (non-zero on top)") {
        auto ctx = execute_with_stdlib("1 2 3 99 ?DUP");
        REQUIRE(ctx.dsp == 5);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
        REQUIRE(ctx.data_stack[3] == 99);
        REQUIRE(ctx.data_stack[4] == 99);  // Duplicated
    }

    SECTION("JIT mode - ?DUP with zero") {
        auto ctx = execute_with_stdlib(": TEST-ZERO 0 ?DUP ; TEST-ZERO", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("JIT mode - ?DUP with non-zero") {
        auto ctx = execute_with_stdlib(": TEST-NONZERO 42 ?DUP ; TEST-NONZERO", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);
    }

    SECTION("Interpreter mode - ?DUP with zero") {
        auto ctx = execute_with_stdlib(": TEST-ZERO 0 ?DUP ; TEST-ZERO", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Interpreter mode - ?DUP with non-zero") {
        auto ctx = execute_with_stdlib(": TEST-NONZERO 42 ?DUP ; TEST-NONZERO", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);
    }

    SECTION("?DUP vs DUP behavior comparison (non-zero)") {
        auto ctx = execute_with_stdlib("5 ?DUP 5 DUP");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 5);  // First 5
        REQUIRE(ctx.data_stack[1] == 5);  // ?DUP duplicate
        REQUIRE(ctx.data_stack[2] == 5);  // Second 5
        REQUIRE(ctx.data_stack[3] == 5);  // DUP duplicate
        // Both behave the same for non-zero
    }

    SECTION("?DUP vs DUP behavior comparison (zero)") {
        auto ctx = execute_with_stdlib("0 ?DUP 0 DUP");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 0);  // From 0 ?DUP (not duplicated)
        REQUIRE(ctx.data_stack[1] == 0);  // Second 0
        REQUIRE(ctx.data_stack[2] == 0);  // From 0 DUP (duplicated)
        // ?DUP leaves 1 zero, DUP leaves 2 zeros
    }

    SECTION("?DUP useful for error checking pattern") {
        // Pattern: get-value ?DUP IF process ELSE handle-zero THEN
        // Simulated: push value, ?DUP, use IF to check
        auto ctx = execute_with_stdlib("0 ?DUP 0= IF 999 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 999);  // Error case handled
    }

    SECTION("?DUP with calculation preserves result (non-zero)") {
        auto ctx = execute_with_stdlib("7 3 - ?DUP");  // 4 ?DUP
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 4);
        REQUIRE(ctx.data_stack[1] == 4);
    }

    SECTION("?DUP with calculation preserves result (zero)") {
        auto ctx = execute_with_stdlib("7 7 - ?DUP");  // 0 ?DUP
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }
}

TEST_CASE("Standard Library - 2*", "[stdlib][2star]") {
    SECTION("2* doubles positive number") {
        auto ctx = execute_with_stdlib("5 2*");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("2* doubles negative number") {
        auto ctx = execute_with_stdlib("-7 2*");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -14);
    }

    SECTION("2* with zero") {
        auto ctx = execute_with_stdlib("0 2*");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("2* with one") {
        auto ctx = execute_with_stdlib("1 2*");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 2);
    }

    SECTION("2* with large number") {
        auto ctx = execute_with_stdlib("50000 2*");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100000);
    }

    SECTION("Multiple 2* operations") {
        auto ctx = execute_with_stdlib("3 2* 2* 2*");  // 3 * 2 * 2 * 2 = 24
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);
    }

    SECTION("2* is equivalent to 2 *") {
        auto ctx = execute_with_stdlib("15 2* 15 2 *");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 30);  // 15 2*
        REQUIRE(ctx.data_stack[1] == 30);  // 15 2 *
    }

    SECTION("2* with expression") {
        auto ctx = execute_with_stdlib("3 4 + 2*");  // (3+4)*2 = 14
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 14);
    }

    SECTION("JIT mode - 2*") {
        auto ctx = execute_with_stdlib(": DOUBLE 2* ; 8 DOUBLE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 16);
    }

    SECTION("Interpreter mode - 2*") {
        auto ctx = execute_with_stdlib(": DOUBLE 2* ; 8 DOUBLE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 16);
    }
}

TEST_CASE("Standard Library - 2/", "[stdlib][2slash]") {
    SECTION("2/ halves positive even number") {
        auto ctx = execute_with_stdlib("10 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("2/ halves positive odd number") {
        auto ctx = execute_with_stdlib("11 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);  // Truncates toward zero
    }

    SECTION("2/ halves negative even number") {
        auto ctx = execute_with_stdlib("-10 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);
    }

    SECTION("2/ halves negative odd number") {
        auto ctx = execute_with_stdlib("-11 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);  // Truncates toward zero
    }

    SECTION("2/ with zero") {
        auto ctx = execute_with_stdlib("0 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("2/ with two") {
        auto ctx = execute_with_stdlib("2 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("2/ with large number") {
        auto ctx = execute_with_stdlib("100000 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 50000);
    }

    SECTION("Multiple 2/ operations") {
        auto ctx = execute_with_stdlib("64 2/ 2/ 2/");  // 64 / 2 / 2 / 2 = 8
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("2/ is equivalent to 2 /") {
        auto ctx = execute_with_stdlib("30 2/ 30 2 /");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 15);  // 30 2/
        REQUIRE(ctx.data_stack[1] == 15);  // 30 2 /
    }

    SECTION("2* and 2/ are inverses (even number)") {
        auto ctx = execute_with_stdlib("42 2* 2/");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);  // Back to original
    }

    SECTION("2/ then 2* loses precision for odd") {
        auto ctx = execute_with_stdlib("11 2/ 2*");  // 11/2=5, 5*2=10 (not 11)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);  // Lost the 1
    }

    SECTION("JIT mode - 2/") {
        auto ctx = execute_with_stdlib(": HALVE 2/ ; 20 HALVE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("Interpreter mode - 2/") {
        auto ctx = execute_with_stdlib(": HALVE 2/ ; 20 HALVE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }
}

TEST_CASE("Standard Library - */", "[stdlib][starslash]") {
    SECTION("*/ basic operation") {
        auto ctx = execute_with_stdlib("3 4 2 */");  // (3*4)/2 = 12/2 = 6
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 6);
    }

    SECTION("*/ for scaling up") {
        auto ctx = execute_with_stdlib("10 3 1 */");  // (10*3)/1 = 30
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 30);
    }

    SECTION("*/ for scaling down") {
        auto ctx = execute_with_stdlib("100 1 10 */");  // (100*1)/10 = 10
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("*/ with negative numbers") {
        auto ctx = execute_with_stdlib("-12 5 3 */");  // (-12*5)/3 = -60/3 = -20
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -20);
    }

    SECTION("*/ percentage calculation") {
        auto ctx = execute_with_stdlib("200 75 100 */");  // 75% of 200 = 150
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 150);
    }

    SECTION("*/ avoids intermediate overflow") {
        // With large numbers, */ can help avoid overflow in intermediate result
        auto ctx = execute_with_stdlib("1000 500 100 */");  // (1000*500)/100 = 5000
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5000);
    }

    SECTION("*/ with zero numerator") {
        auto ctx = execute_with_stdlib("0 5 3 */");  // (0*5)/3 = 0
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("*/ with zero multiplier") {
        auto ctx = execute_with_stdlib("10 0 5 */");  // (10*0)/5 = 0
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("*/ division by zero protection") {
        auto ctx = execute_with_stdlib("10 5 0 */");  // (10*5)/0 = division by zero
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Protected, returns 0
    }

    SECTION("*/ with ones (identity)") {
        auto ctx = execute_with_stdlib("42 1 1 */");  // (42*1)/1 = 42
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("*/ scaling ratio") {
        auto ctx = execute_with_stdlib("50 3 2 */");  // (50*3)/2 = 150/2 = 75
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 75);
    }

    SECTION("*/ with truncation") {
        auto ctx = execute_with_stdlib("7 5 3 */");  // (7*5)/3 = 35/3 = 11
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 11);  // Truncates remainder
    }

    SECTION("Multiple */ operations") {
        auto ctx = execute_with_stdlib("10 2 1 */ 3 2 */");  // ((10*2)/1 * 3)/2 = (20*3)/2 = 30
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 30);
    }

    SECTION("*/ compared to separate * and /") {
        auto ctx = execute_with_stdlib("8 3 2 */ 8 3 * 2 /");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 12);  // 8 3 2 */
        REQUIRE(ctx.data_stack[1] == 12);  // 8 3 * 2 /
        // Results are the same
    }

    SECTION("JIT mode - */") {
        auto ctx = execute_with_stdlib(": SCALE */ ; 10 5 2 SCALE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);  // (10*5)/2
    }

    SECTION("Interpreter mode - */") {
        auto ctx = execute_with_stdlib(": SCALE */ ; 10 5 2 SCALE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);  // (10*5)/2
    }

    SECTION("*/ for fraction multiplication") {
        // Compute 2/3 of 15: (15 * 2) / 3 = 10
        auto ctx = execute_with_stdlib("15 2 3 */");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("*/ preserves precision better") {
        // 100 * 3 / 4 vs 100 3 4 */ - both should give same result
        auto ctx = execute_with_stdlib("100 3 4 */");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 75);
    }
}

TEST_CASE("Standard Library - */MOD", "[stdlib][starslashmod]") {
    SECTION("*/MOD basic operation") {
        auto ctx = execute_with_stdlib("17 3 2 */MOD");  // (17*3)/2 = 51/2 = 25 remainder 1
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 1);   // remainder
        REQUIRE(ctx.data_stack[1] == 25);  // quotient
    }

    SECTION("*/MOD with no remainder") {
        auto ctx = execute_with_stdlib("10 4 2 */MOD");  // (10*4)/2 = 40/2 = 20 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);   // remainder
        REQUIRE(ctx.data_stack[1] == 20);  // quotient
    }

    SECTION("*/MOD scaling with remainder") {
        auto ctx = execute_with_stdlib("7 5 3 */MOD");  // (7*5)/3 = 35/3 = 11 remainder 2
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);   // remainder
        REQUIRE(ctx.data_stack[1] == 11);  // quotient
    }

    SECTION("*/MOD with negative dividend") {
        auto ctx = execute_with_stdlib("-17 3 2 */MOD");  // (-17*3)/2 = -51/2 = -25 remainder -1
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -1);   // remainder
        REQUIRE(ctx.data_stack[1] == -25);  // quotient
    }

    SECTION("*/MOD with zero numerator") {
        auto ctx = execute_with_stdlib("0 5 3 */MOD");  // (0*5)/3 = 0/3 = 0 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);  // remainder
        REQUIRE(ctx.data_stack[1] == 0);  // quotient
    }

    SECTION("*/MOD with zero multiplier") {
        auto ctx = execute_with_stdlib("10 0 5 */MOD");  // (10*0)/5 = 0/5 = 0 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);  // remainder
        REQUIRE(ctx.data_stack[1] == 0);  // quotient
    }

    SECTION("*/MOD division by zero") {
        auto ctx = execute_with_stdlib("10 5 0 */MOD");  // (10*5)/0 = division by zero
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 50);  // /MOD returns product as remainder
        REQUIRE(ctx.data_stack[1] == 0);   // quotient is 0
    }

    SECTION("*/MOD percentage with remainder") {
        auto ctx = execute_with_stdlib("100 33 100 */MOD");  // 33% of 100 = 33 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);   // remainder
        REQUIRE(ctx.data_stack[1] == 33);  // quotient (33%)
    }

    SECTION("*/MOD time conversion") {
        // Convert 125 seconds to minutes and seconds
        auto ctx = execute_with_stdlib("125 1 60 */MOD");  // (125*1)/60 = 2 remainder 5
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 5);  // 5 seconds
        REQUIRE(ctx.data_stack[1] == 2);  // 2 minutes
    }

    SECTION("*/MOD fraction multiplication with remainder") {
        // 2/3 of 16 = (16*2)/3 = 32/3 = 10 remainder 2
        auto ctx = execute_with_stdlib("16 2 3 */MOD");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);   // remainder
        REQUIRE(ctx.data_stack[1] == 10);  // quotient
    }

    SECTION("*/MOD reconstruct original") {
        // quotient * divisor + remainder = original product
        auto ctx = execute_with_stdlib("17 3 2 */MOD 2 * +");  // 25*2 + 1 = 51 (which is 17*3)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 51);  // 17*3
    }

    SECTION("*/MOD vs */ and MOD separately") {
        auto ctx = execute_with_stdlib("7 5 3 */MOD 7 5 3 */ 7 5 * 3 MOD");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 2);   // */MOD remainder
        REQUIRE(ctx.data_stack[1] == 11);  // */MOD quotient
        REQUIRE(ctx.data_stack[2] == 11);  // */ result (quotient only)
        REQUIRE(ctx.data_stack[3] == 2);   // MOD result (remainder only)
    }

    SECTION("*/MOD large numbers") {
        auto ctx = execute_with_stdlib("1000 500 100 */MOD");  // (1000*500)/100 = 5000 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);     // remainder
        REQUIRE(ctx.data_stack[1] == 5000);  // quotient
    }

    SECTION("*/MOD scaling ratio") {
        auto ctx = execute_with_stdlib("50 7 4 */MOD");  // (50*7)/4 = 350/4 = 87 remainder 2
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);   // remainder
        REQUIRE(ctx.data_stack[1] == 87);  // quotient
    }

    SECTION("*/MOD with ones") {
        auto ctx = execute_with_stdlib("42 1 1 */MOD");  // (42*1)/1 = 42 remainder 0
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);   // remainder
        REQUIRE(ctx.data_stack[1] == 42);  // quotient
    }

    SECTION("JIT mode - */MOD") {
        auto ctx = execute_with_stdlib(": SCALE-MOD */MOD ; 17 3 2 SCALE-MOD", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 1);   // remainder
        REQUIRE(ctx.data_stack[1] == 25);  // quotient
    }

    SECTION("Interpreter mode - */MOD") {
        auto ctx = execute_with_stdlib(": SCALE-MOD */MOD ; 17 3 2 SCALE-MOD", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 1);   // remainder
        REQUIRE(ctx.data_stack[1] == 25);  // quotient
    }

    SECTION("*/MOD fixed-point arithmetic") {
        // Scale 123 by 456/1000 (0.456)
        auto ctx = execute_with_stdlib("123 456 1000 */MOD");  // (123*456)/1000 = 56088/1000 = 56 remainder 88
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 88);  // fractional part (0.088)
        REQUIRE(ctx.data_stack[1] == 56);  // integer part
    }

    SECTION("*/MOD preserves both results") {
        // Unlike */ which only gives quotient, */MOD gives both
        auto ctx = execute_with_stdlib("11 7 5 */MOD");  // (11*7)/5 = 77/5 = 15 remainder 2
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);   // remainder (lost in */)
        REQUIRE(ctx.data_stack[1] == 15);  // quotient (same as */)
    }

    SECTION("*/MOD with small numbers") {
        auto ctx = execute_with_stdlib("2 3 4 */MOD");  // (2*3)/4 = 6/4 = 1 remainder 2
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);  // remainder
        REQUIRE(ctx.data_stack[1] == 1);  // quotient
    }
}

TEST_CASE("Standard Library - MIN", "[stdlib][min]") {
    SECTION("MIN of two positive numbers (first smaller)") {
        auto ctx = execute_with_stdlib("5 10 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MIN of two positive numbers (second smaller)") {
        auto ctx = execute_with_stdlib("10 5 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MIN with equal numbers") {
        auto ctx = execute_with_stdlib("7 7 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("MIN with negative numbers") {
        auto ctx = execute_with_stdlib("-10 -5 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -10);  // -10 is less than -5
    }

    SECTION("MIN with positive and negative") {
        auto ctx = execute_with_stdlib("5 -3 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -3);
    }

    SECTION("MIN with negative and positive") {
        auto ctx = execute_with_stdlib("-3 5 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -3);
    }

    SECTION("MIN with zero (zero smaller)") {
        auto ctx = execute_with_stdlib("0 10 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("MIN with zero (zero larger)") {
        auto ctx = execute_with_stdlib("-5 0 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);
    }

    SECTION("MIN with both zero") {
        auto ctx = execute_with_stdlib("0 0 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("MIN with large numbers") {
        auto ctx = execute_with_stdlib("100000 99999 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 99999);
    }

    SECTION("Multiple MIN operations") {
        auto ctx = execute_with_stdlib("15 8 MIN 3 MIN");  // MIN(MIN(15,8),3) = MIN(8,3) = 3
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }

    SECTION("MIN of expression results") {
        auto ctx = execute_with_stdlib("3 4 + 2 5 * MIN");  // MIN(7, 10) = 7
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("MIN is commutative") {
        auto ctx = execute_with_stdlib("10 5 MIN 5 10 MIN");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 5);  // Both give same result
    }

    SECTION("MIN clamping (lower bound)") {
        // Ensure value is at least 10
        auto ctx = execute_with_stdlib("5 10 MAX");  // Use MAX for lower bound, but test MIN exists
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("JIT mode - MIN") {
        auto ctx = execute_with_stdlib(": MINIMUM MIN ; 12 7 MINIMUM", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("Interpreter mode - MIN") {
        auto ctx = execute_with_stdlib(": MINIMUM MIN ; 12 7 MINIMUM", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("MIN in range checking") {
        // Clamp value to maximum
        auto ctx = execute_with_stdlib("150 100 MIN");  // Cap at 100
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("MIN with one") {
        auto ctx = execute_with_stdlib("5 1 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("MIN with negative one") {
        auto ctx = execute_with_stdlib("0 -1 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);
    }
}

TEST_CASE("Standard Library - MAX", "[stdlib][max]") {
    SECTION("MAX of two positive numbers (first larger)") {
        auto ctx = execute_with_stdlib("10 5 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("MAX of two positive numbers (second larger)") {
        auto ctx = execute_with_stdlib("5 10 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("MAX with equal numbers") {
        auto ctx = execute_with_stdlib("7 7 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("MAX with negative numbers") {
        auto ctx = execute_with_stdlib("-10 -5 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -5);  // -5 is greater than -10
    }

    SECTION("MAX with positive and negative") {
        auto ctx = execute_with_stdlib("5 -3 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MAX with negative and positive") {
        auto ctx = execute_with_stdlib("-3 5 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MAX with zero (zero larger)") {
        auto ctx = execute_with_stdlib("0 -10 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("MAX with zero (zero smaller)") {
        auto ctx = execute_with_stdlib("5 0 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MAX with both zero") {
        auto ctx = execute_with_stdlib("0 0 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("MAX with large numbers") {
        auto ctx = execute_with_stdlib("100000 99999 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100000);
    }

    SECTION("Multiple MAX operations") {
        auto ctx = execute_with_stdlib("5 8 MAX 12 MAX");  // MAX(MAX(5,8),12) = MAX(8,12) = 12
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 12);
    }

    SECTION("MAX of expression results") {
        auto ctx = execute_with_stdlib("3 4 + 2 5 * MAX");  // MAX(7, 10) = 10
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("MAX is commutative") {
        auto ctx = execute_with_stdlib("10 5 MAX 5 10 MAX");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 10);  // Both give same result
    }

    SECTION("MAX clamping (lower bound)") {
        // Ensure value is at least 10
        auto ctx = execute_with_stdlib("5 10 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("JIT mode - MAX") {
        auto ctx = execute_with_stdlib(": MAXIMUM MAX ; 12 7 MAXIMUM", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 12);
    }

    SECTION("Interpreter mode - MAX") {
        auto ctx = execute_with_stdlib(": MAXIMUM MAX ; 12 7 MAXIMUM", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 12);
    }

    SECTION("MAX in range checking") {
        // Ensure at least minimum value
        auto ctx = execute_with_stdlib("5 10 MAX");  // At least 10
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("MAX with one") {
        auto ctx = execute_with_stdlib("5 1 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("MAX with negative one") {
        auto ctx = execute_with_stdlib("0 -1 MAX");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }
}

TEST_CASE("Standard Library - MIN and MAX together", "[stdlib][min][max]") {
    SECTION("MIN and MAX combined (clamping)") {
        // Clamp value between 10 and 100
        auto ctx = execute_with_stdlib("5 10 MAX 100 MIN");  // 5 -> 10 -> 10
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("Clamp value in range (below minimum)") {
        auto ctx = execute_with_stdlib("5 10 MAX 100 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);  // Clamped to minimum
    }

    SECTION("Clamp value in range (above maximum)") {
        auto ctx = execute_with_stdlib("150 10 MAX 100 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);  // Clamped to maximum
    }

    SECTION("Clamp value in range (within bounds)") {
        auto ctx = execute_with_stdlib("50 10 MAX 100 MIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 50);  // Unchanged, within range
    }

    SECTION("MIN and MAX are inverses for same values") {
        auto ctx = execute_with_stdlib("7 3 MIN 7 3 MAX");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 3);  // MIN
        REQUIRE(ctx.data_stack[1] == 7);  // MAX
    }

    SECTION("MIN of MAX") {
        auto ctx = execute_with_stdlib("5 10 MAX 8 MIN");  // MAX(5,10)=10, MIN(10,8)=8
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("MAX of MIN") {
        auto ctx = execute_with_stdlib("5 10 MIN 8 MAX");  // MIN(5,10)=5, MAX(5,8)=8
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }
}

TEST_CASE("Standard Library - WITHIN", "[stdlib][within]") {
    SECTION("WITHIN - value inside range") {
        auto ctx = execute_with_stdlib("5 3 10 WITHIN");  // Is 5 within [3,10)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True (flag is -1)
    }

    SECTION("WITHIN - value at lower bound (inclusive)") {
        auto ctx = execute_with_stdlib("3 3 10 WITHIN");  // Is 3 within [3,10)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True (lower bound is inclusive)
    }

    SECTION("WITHIN - value at upper bound (exclusive)") {
        auto ctx = execute_with_stdlib("10 3 10 WITHIN");  // Is 10 within [3,10)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False (upper bound is exclusive)
    }

    SECTION("WITHIN - value below range") {
        auto ctx = execute_with_stdlib("2 3 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        // (2-3) = -1 (large unsigned), which is NOT < (10-3) = 7 unsigned
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - value above range") {
        auto ctx = execute_with_stdlib("11 3 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - negative range") {
        auto ctx = execute_with_stdlib("-5 -10 0 WITHIN");  // Is -5 within [-10,0)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - negative value below negative range") {
        auto ctx = execute_with_stdlib("-15 -10 0 WITHIN");
        REQUIRE(ctx.dsp == 1);
        // (-15) - (-10) = -5 (large unsigned), NOT < (0 - (-10)) = 10 unsigned
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - zero in range") {
        auto ctx = execute_with_stdlib("0 -5 5 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - zero at lower bound") {
        auto ctx = execute_with_stdlib("0 0 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True (inclusive)
    }

    SECTION("WITHIN - zero at upper bound") {
        auto ctx = execute_with_stdlib("0 -10 0 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False (exclusive)
    }

    SECTION("WITHIN - single value range") {
        auto ctx = execute_with_stdlib("5 5 6 WITHIN");  // [5,6)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True (5 is in range)
    }

    SECTION("WITHIN - empty range") {
        auto ctx = execute_with_stdlib("5 5 5 WITHIN");  // [5,5) is empty
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False (no values in empty range)
    }

    SECTION("WITHIN - character range (ASCII)") {
        // Check if 'M' (77) is within ['A','Z'+1) = [65,91)
        auto ctx = execute_with_stdlib("77 65 91 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - digit range") {
        // Check if '5' (53) is within ['0','9'+1) = [48,58)
        auto ctx = execute_with_stdlib("53 48 58 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - not a digit") {
        // Check if 'A' (65) is within ['0','9'+1) = [48,58)
        auto ctx = execute_with_stdlib("65 48 58 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - large positive range") {
        auto ctx = execute_with_stdlib("50000 10000 100000 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - percentage range (0-100)") {
        auto ctx = execute_with_stdlib("50 0 101 WITHIN");  // 50% in [0,101)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - percentage out of range") {
        auto ctx = execute_with_stdlib("150 0 101 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - with expression") {
        auto ctx = execute_with_stdlib("3 4 + 5 10 WITHIN");  // Is 7 within [5,10)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - range boundaries as expressions") {
        auto ctx = execute_with_stdlib("50 10 5 * 20 5 * WITHIN");  // 50 within [50,100)?
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True (at lower bound)
    }

    SECTION("WITHIN - used in conditional") {
        auto ctx = execute_with_stdlib("7 5 10 WITHIN IF 999 ELSE 0 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 999);  // Branch taken
    }

    SECTION("WITHIN - used in conditional (false case)") {
        auto ctx = execute_with_stdlib("3 5 10 WITHIN IF 999 ELSE 0 THEN");
        REQUIRE(ctx.dsp == 1);
        // 3 is below 5, (3-5)=-2 as unsigned is NOT < (10-5)=5
        REQUIRE(ctx.data_stack[0] == 0);  // Else branch taken
    }

    SECTION("JIT mode - WITHIN") {
        auto ctx = execute_with_stdlib(": IN-RANGE WITHIN ; 7 5 10 IN-RANGE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("Interpreter mode - WITHIN") {
        auto ctx = execute_with_stdlib(": IN-RANGE WITHIN ; 7 5 10 IN-RANGE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True
    }

    SECTION("WITHIN - array index validation") {
        // Check if index 5 is valid for array size 10 [0,10)
        auto ctx = execute_with_stdlib("5 0 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // Valid index
    }

    SECTION("WITHIN - array index out of bounds") {
        auto ctx = execute_with_stdlib("10 0 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Out of bounds
    }

    SECTION("WITHIN - temperature range check") {
        // Check if 72°F is comfortable [68,78)
        auto ctx = execute_with_stdlib("72 68 78 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // Comfortable
    }

    SECTION("WITHIN - wrapping behavior understanding") {
        // WITHIN uses unsigned comparison logic internally
        // Testing edge case understanding
        auto ctx = execute_with_stdlib("5 0 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);
    }

    SECTION("WITHIN - one past upper bound") {
        auto ctx = execute_with_stdlib("11 5 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Just past range
    }

    SECTION("WITHIN - one before lower bound") {
        auto ctx = execute_with_stdlib("4 5 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        // (4-5)=-1 as unsigned is NOT < (10-5)=5
        REQUIRE(ctx.data_stack[0] == 0);  // False
    }

    SECTION("WITHIN - combined with MIN/MAX") {
        // Clamp then check: clamp 3 to [5,10], then check if result is in [5,10)
        auto ctx = execute_with_stdlib("3 5 MAX 10 MIN 5 10 WITHIN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // 5 (clamped) is in [5,10)
    }
}

TEST_CASE("Standard Library - ROT", "[stdlib][rot]") {
    SECTION("ROT basic operation") {
        auto ctx = execute_with_stdlib("1 2 3 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 2);
        REQUIRE(ctx.data_stack[1] == 3);
        REQUIRE(ctx.data_stack[2] == 1);
    }

    SECTION("ROT with different values") {
        auto ctx = execute_with_stdlib("10 20 30 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 20);
        REQUIRE(ctx.data_stack[1] == 30);
        REQUIRE(ctx.data_stack[2] == 10);
    }

    SECTION("ROT with negative numbers") {
        auto ctx = execute_with_stdlib("-5 -10 -15 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == -10);
        REQUIRE(ctx.data_stack[1] == -15);
        REQUIRE(ctx.data_stack[2] == -5);
    }

    SECTION("ROT with mixed positive and negative") {
        auto ctx = execute_with_stdlib("5 -10 15 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == -10);
        REQUIRE(ctx.data_stack[1] == 15);
        REQUIRE(ctx.data_stack[2] == 5);
    }

    SECTION("ROT with zeros") {
        auto ctx = execute_with_stdlib("0 1 2 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 0);
    }

    SECTION("Multiple ROT operations") {
        auto ctx = execute_with_stdlib("1 2 3 ROT ROT");
        REQUIRE(ctx.dsp == 3);
        // First ROT: 1 2 3 -> 2 3 1
        // Second ROT: 2 3 1 -> 3 1 2
        REQUIRE(ctx.data_stack[0] == 3);
        REQUIRE(ctx.data_stack[1] == 1);
        REQUIRE(ctx.data_stack[2] == 2);
    }

    SECTION("Three ROT operations (full cycle)") {
        auto ctx = execute_with_stdlib("1 2 3 ROT ROT ROT");
        REQUIRE(ctx.dsp == 3);
        // After 3 ROTs, should be back to original order
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("ROT with more items on stack") {
        auto ctx = execute_with_stdlib("100 200 300 400 500 ROT");
        REQUIRE(ctx.dsp == 5);
        REQUIRE(ctx.data_stack[0] == 100);
        REQUIRE(ctx.data_stack[1] == 200);
        REQUIRE(ctx.data_stack[2] == 400);  // Third from top
        REQUIRE(ctx.data_stack[3] == 500);  // Second from top
        REQUIRE(ctx.data_stack[4] == 300);  // Was top, now at top
    }

    SECTION("ROT with expressions") {
        auto ctx = execute_with_stdlib("1 2 + 3 4 + 5 6 + ROT");  // 3 7 11 ROT
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 7);
        REQUIRE(ctx.data_stack[1] == 11);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("ROT in user-defined word") {
        auto ctx = execute_with_stdlib(": THIRD ROT ROT ; 10 20 30 THIRD");
        REQUIRE(ctx.dsp == 3);
        // THIRD extracts third item: ROT ROT brings third to top
        // 10 20 30 -> (ROT) 20 30 10 -> (ROT) 30 10 20
        REQUIRE(ctx.data_stack[0] == 30);
        REQUIRE(ctx.data_stack[1] == 10);
        REQUIRE(ctx.data_stack[2] == 20);
    }

    SECTION("ROT combined with SWAP") {
        auto ctx = execute_with_stdlib("1 2 3 ROT SWAP");
        REQUIRE(ctx.dsp == 3);
        // ROT: 1 2 3 -> 2 3 1
        // SWAP: 2 3 1 -> 2 1 3
        REQUIRE(ctx.data_stack[0] == 2);
        REQUIRE(ctx.data_stack[1] == 1);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("ROT combined with DROP") {
        auto ctx = execute_with_stdlib("1 2 3 ROT DROP");
        REQUIRE(ctx.dsp == 2);
        // ROT: 1 2 3 -> 2 3 1
        // DROP: 2 3 1 -> 2 3
        REQUIRE(ctx.data_stack[0] == 2);
        REQUIRE(ctx.data_stack[1] == 3);
    }

    SECTION("ROT combined with DUP") {
        auto ctx = execute_with_stdlib("1 2 3 ROT DUP");
        REQUIRE(ctx.dsp == 4);
        // ROT: 1 2 3 -> 2 3 1
        // DUP: 2 3 1 -> 2 3 1 1
        REQUIRE(ctx.data_stack[0] == 2);
        REQUIRE(ctx.data_stack[1] == 3);
        REQUIRE(ctx.data_stack[2] == 1);
        REQUIRE(ctx.data_stack[3] == 1);
    }

    SECTION("ROT for reordering calculation") {
        // Calculate: (a * c) - b where stack is a b c
        auto ctx = execute_with_stdlib("5 3 10 ROT * SWAP -");
        // 5 3 10 -> (ROT) 3 10 5 -> (*) 3 50 -> (SWAP) 50 3 -> (-) 47
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 47);
    }

    SECTION("JIT mode - ROT") {
        auto ctx = execute_with_stdlib(": TEST-ROT ROT ; 7 8 9 TEST-ROT", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 8);
        REQUIRE(ctx.data_stack[1] == 9);
        REQUIRE(ctx.data_stack[2] == 7);
    }

    SECTION("Interpreter mode - ROT") {
        auto ctx = execute_with_stdlib(": TEST-ROT ROT ; 7 8 9 TEST-ROT", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 8);
        REQUIRE(ctx.data_stack[1] == 9);
        REQUIRE(ctx.data_stack[2] == 7);
    }

    SECTION("ROT vs manual equivalent") {
        auto ctx = execute_with_stdlib("1 2 3 ROT 10 20 30 >R SWAP R> SWAP");
        REQUIRE(ctx.dsp == 6);
        // Both should produce same result
        REQUIRE(ctx.data_stack[0] == 2);   // From ROT
        REQUIRE(ctx.data_stack[1] == 3);
        REQUIRE(ctx.data_stack[2] == 1);
        REQUIRE(ctx.data_stack[3] == 20);  // From manual version
        REQUIRE(ctx.data_stack[4] == 30);
        REQUIRE(ctx.data_stack[5] == 10);
    }

    SECTION("ROT with large numbers") {
        auto ctx = execute_with_stdlib("100000 200000 300000 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 200000);
        REQUIRE(ctx.data_stack[1] == 300000);
        REQUIRE(ctx.data_stack[2] == 100000);
    }

    SECTION("ROT inverse property") {
        // ROT ROT ROT should return to original order
        auto ctx = execute_with_stdlib("42 84 126 DUP DUP DUP ROT ROT ROT");
        REQUIRE(ctx.dsp == 6);
        // First three: 42 84 126
        // Second three (after DUP DUP DUP): 126 126 126
        // After ROT ROT ROT on top three: back to 126 126 126
        REQUIRE(ctx.data_stack[3] == 126);
        REQUIRE(ctx.data_stack[4] == 126);
        REQUIRE(ctx.data_stack[5] == 126);
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

TEST_CASE("Standard Library - PICK", "[stdlib][pick]") {
    SECTION("PICK 0 (equivalent to DUP)") {
        auto ctx = execute_with_stdlib("10 20 30 0 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
        REQUIRE(ctx.data_stack[3] == 30);  // Copied top
    }

    SECTION("PICK 1 (equivalent to OVER)") {
        auto ctx = execute_with_stdlib("10 20 30 1 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
        REQUIRE(ctx.data_stack[3] == 20);  // Copied second
    }

    SECTION("PICK 2 (third from top)") {
        auto ctx = execute_with_stdlib("10 20 30 2 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
        REQUIRE(ctx.data_stack[3] == 10);  // Copied third
    }

    SECTION("PICK 3 (fourth from top)") {
        auto ctx = execute_with_stdlib("5 10 20 30 3 PICK");
        REQUIRE(ctx.dsp == 5);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 10);
        REQUIRE(ctx.data_stack[2] == 20);
        REQUIRE(ctx.data_stack[3] == 30);
        REQUIRE(ctx.data_stack[4] == 5);  // Copied fourth
    }

    SECTION("PICK 4 (fifth from top)") {
        auto ctx = execute_with_stdlib("1 5 10 20 30 4 PICK");
        REQUIRE(ctx.dsp == 6);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 5);
        REQUIRE(ctx.data_stack[2] == 10);
        REQUIRE(ctx.data_stack[3] == 20);
        REQUIRE(ctx.data_stack[4] == 30);
        REQUIRE(ctx.data_stack[5] == 1);  // Copied fifth
    }

    SECTION("PICK with negative numbers") {
        auto ctx = execute_with_stdlib("-10 -20 -30 1 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == -10);
        REQUIRE(ctx.data_stack[1] == -20);
        REQUIRE(ctx.data_stack[2] == -30);
        REQUIRE(ctx.data_stack[3] == -20);
    }

    SECTION("PICK with zeros") {
        auto ctx = execute_with_stdlib("0 0 0 2 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 0);
        REQUIRE(ctx.data_stack[1] == 0);
        REQUIRE(ctx.data_stack[2] == 0);
        REQUIRE(ctx.data_stack[3] == 0);
    }

    SECTION("PICK in expression") {
        auto ctx = execute_with_stdlib("5 10 15 1 PICK +");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 10);
        REQUIRE(ctx.data_stack[2] == 25);  // 15 + 10
    }

    SECTION("Multiple PICK operations") {
        auto ctx = execute_with_stdlib("10 20 30 2 PICK 1 PICK");
        REQUIRE(ctx.dsp == 5);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
        REQUIRE(ctx.data_stack[3] == 10);  // 2 PICK copied 10
        REQUIRE(ctx.data_stack[4] == 30);  // 1 PICK copied 30
    }

    SECTION("PICK 0 multiple times") {
        auto ctx = execute_with_stdlib("42 0 PICK 0 PICK 0 PICK");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);
        REQUIRE(ctx.data_stack[2] == 42);
        REQUIRE(ctx.data_stack[3] == 42);
    }

    SECTION("PICK in JIT mode") {
        auto ctx = execute_with_stdlib("100 200 300 1 PICK", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 100);
        REQUIRE(ctx.data_stack[1] == 200);
        REQUIRE(ctx.data_stack[2] == 300);
        REQUIRE(ctx.data_stack[3] == 200);
    }

    SECTION("PICK in Interpreter mode") {
        auto ctx = execute_with_stdlib("100 200 300 1 PICK", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 100);
        REQUIRE(ctx.data_stack[1] == 200);
        REQUIRE(ctx.data_stack[2] == 300);
        REQUIRE(ctx.data_stack[3] == 200);
    }

    SECTION("PICK deep stack") {
        auto ctx = execute_with_stdlib("1 2 3 4 5 6 7 8 9 10 5 PICK");
        REQUIRE(ctx.dsp == 11);
        REQUIRE(ctx.data_stack[4] == 5);
        REQUIRE(ctx.data_stack[10] == 5);  // Copied 5th from top
    }

    SECTION("PICK used in word definition") {
        auto ctx = execute_with_stdlib(": THIRD 2 PICK ; 10 20 30 THIRD");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
        REQUIRE(ctx.data_stack[3] == 10);
    }

    SECTION("Verify PICK is recursive (uses RECURSE)") {
        // Deep pick to ensure recursion works
        auto ctx = execute_with_stdlib("1 2 3 4 5 6 7 8 9 10 9 PICK");
        REQUIRE(ctx.dsp == 11);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[10] == 1);  // 9 PICK gets the bottom
    }

    SECTION("PICK vs manual equivalent for index 0") {
        auto ctx = execute_with_stdlib("42 0 PICK 42 DUP");
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);  // 0 PICK result
        REQUIRE(ctx.data_stack[2] == 42);
        REQUIRE(ctx.data_stack[3] == 42);  // DUP result
    }

    SECTION("PICK vs manual equivalent for index 1") {
        auto ctx = execute_with_stdlib("10 20 1 PICK 10 20 OVER");
        REQUIRE(ctx.dsp == 6);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 10);  // 1 PICK result
        REQUIRE(ctx.data_stack[3] == 10);
        REQUIRE(ctx.data_stack[4] == 20);
        REQUIRE(ctx.data_stack[5] == 10);  // OVER result
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

TEST_CASE("Standard Library - CELLS", "[stdlib][cells]") {
    SECTION("CELLS converts 0 cells") {
        auto ctx = execute_with_stdlib("0 CELLS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("CELLS converts 1 cell") {
        auto ctx = execute_with_stdlib("1 CELLS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("CELLS converts 5 cells") {
        auto ctx = execute_with_stdlib("5 CELLS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 40);
    }

    SECTION("CELLS converts 10 cells") {
        auto ctx = execute_with_stdlib("10 CELLS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 80);
    }

    SECTION("CELLS in JIT mode") {
        auto ctx = execute_with_stdlib("3 CELLS", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);
    }

    SECTION("CELLS in interpreter mode") {
        auto ctx = execute_with_stdlib("7 CELLS", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 56);
    }
}

TEST_CASE("Standard Library - CELL+", "[stdlib][cell+]") {
    SECTION("CELL+ adds 8 to address") {
        auto ctx = execute_with_stdlib("100 CELL+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 108);
    }

    SECTION("CELL+ with zero") {
        auto ctx = execute_with_stdlib("0 CELL+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("Multiple CELL+ operations") {
        auto ctx = execute_with_stdlib("0 CELL+ CELL+ CELL+");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);
    }

    SECTION("CELL+ in JIT mode") {
        auto ctx = execute_with_stdlib("200 CELL+", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 208);
    }

    SECTION("CELL+ in interpreter mode") {
        auto ctx = execute_with_stdlib("1000 CELL+", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1008);
    }
}

TEST_CASE("Standard Library - +!", "[stdlib][plus-store]") {
    SECTION("+! adds to memory location using ! and @") {
        // First test: use ! to store, then +! to add
        auto ctx = execute_with_stdlib("HERE DUP 42 SWAP ! DUP 10 SWAP +! @");
        // Stack: ( 52 )
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 52);
    }

    SECTION("+! with comma and fetch") {
        auto ctx = execute_with_stdlib("42 HERE DUP >R ! R> DUP 10 SWAP +! @");
        // Store 42 at HERE, then add 10 to it, then fetch
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 52);
    }
}

TEST_CASE("Standard Library - Memory helpers combined", "[stdlib][memory]") {
    SECTION("Using HERE, comma, and @ together") {
        auto ctx = execute_with_stdlib("HERE 99 , HERE 8 - @");
        REQUIRE(ctx.dsp == 2);
        int64_t* data_as_cells = reinterpret_cast<int64_t*>(ctx.data_space);
        REQUIRE(data_as_cells[0] == 99);
        REQUIRE(ctx.data_stack[1] == 99);  // Value read back
    }

    SECTION("Allocate space with CELLS and ALLOT") {
        auto ctx = execute_with_stdlib("3 CELLS ALLOT HERE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.here == 24);  // 3 cells = 24 bytes
    }

    SECTION("Navigate cells with CELL+") {
        // Use HERE and comma to store values, then read them back
        auto ctx = execute_with_stdlib("HERE 10 , 20 , DROP HERE 16 - @");
        // Stores 10 and 20, then reads back the first value
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("Store and retrieve using manual variable pattern") {
        // Pattern: get address, store value, retrieve it
        auto ctx = execute_with_stdlib("HERE DUP 42 SWAP ! @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }
}

TEST_CASE("Standard Library - BL", "[stdlib][bl]") {
    SECTION("BL pushes space character code") {
        auto ctx = execute_with_stdlib("BL");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 32);  // ASCII space
    }

    SECTION("BL in JIT mode") {
        auto ctx = execute_with_stdlib("BL", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 32);
    }

    SECTION("BL in interpreter mode") {
        auto ctx = execute_with_stdlib("BL", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 32);
    }
}

TEST_CASE("Standard Library - SPACE", "[stdlib][space]") {
    SECTION("SPACE outputs a space") {
        auto ctx = execute_with_stdlib("SPACE");
        REQUIRE(ctx.dsp == 0);  // Stack should be empty after SPACE
    }

    SECTION("Multiple SPACE calls") {
        auto ctx = execute_with_stdlib("SPACE SPACE SPACE");
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("SPACE in JIT mode") {
        auto ctx = execute_with_stdlib("SPACE", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("SPACE in interpreter mode") {
        auto ctx = execute_with_stdlib("SPACE", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 0);
    }
}

TEST_CASE("Standard Library - SPACES", "[stdlib][spaces]") {
    SECTION("SPACES with positive count") {
        // SPACES consumes the count and outputs spaces
        auto ctx = execute_with_stdlib("5 SPACES");
        REQUIRE(ctx.dsp == 0);  // Stack should be empty after SPACES
    }

    SECTION("SPACES with zero") {
        auto ctx = execute_with_stdlib("0 SPACES");
        REQUIRE(ctx.dsp == 0);  // Stack should be empty
    }

    SECTION("SPACES in JIT mode") {
        auto ctx = execute_with_stdlib("3 SPACES", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("SPACES in interpreter mode") {
        auto ctx = execute_with_stdlib("10 SPACES", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("Multiple SPACES calls") {
        auto ctx = execute_with_stdlib("2 SPACES 3 SPACES");
        REQUIRE(ctx.dsp == 0);
    }
}

TEST_CASE("Standard Library - COUNT", "[stdlib][count][input]") {
    SECTION("COUNT extracts length and address from counted string") {
        // Create a counted string at HERE: length byte + characters
        // We'll manually set up a counted string for testing
        auto ctx = execute_with_stdlib(
            "HERE 5 OVER C! "    // Store length 5 at HERE
            "72 OVER 1 + C! "    // 'H' at HERE+1
            "69 OVER 2 + C! "    // 'E' at HERE+2
            "76 OVER 3 + C! "    // 'L' at HERE+3
            "76 OVER 4 + C! "    // 'L' at HERE+4
            "79 OVER 5 + C! "    // 'O' at HERE+5
            "COUNT"               // Call COUNT
        );
        REQUIRE(ctx.dsp == 2);
        // First element should be original address + 1
        // Second element should be length (5)
        REQUIRE(ctx.data_stack[1] == 5);
    }

    SECTION("COUNT with zero length") {
        auto ctx = execute_with_stdlib(
            "HERE 0 OVER C! "  // Store length 0
            "COUNT"
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[1] == 0);
    }
}

TEST_CASE("Standard Library - WORD basics", "[stdlib][word][input]") {
    SECTION("WORD with prepared input buffer") {
        // Manually set up TIB with "TEST"
        auto ctx = execute_with_stdlib(
            "TIB 84 OVER C! "    // 'T' at TIB[0]
            "69 OVER 1 + C! "    // 'E' at TIB[1]
            "83 OVER 2 + C! "    // 'S' at TIB[2]
            "84 OVER 3 + C! "    // 'T' at TIB[3]
            "32 OVER 4 + C! "    // space at TIB[4]
            "DROP "              // Drop TIB address
            "5 #TIB ! "          // Set buffer length
            "0 >IN ! "           // Reset parse position
            "32 WORD "           // Parse space-delimited word
            "COUNT"              // Get address and length
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[1] == 4);  // Length of "TEST"
    }
}

TEST_CASE("Standard Library - Input buffer primitives integration", "[stdlib][input]") {
    SECTION("TIB returns valid address") {
        auto ctx = execute_with_stdlib("TIB");
        REQUIRE(ctx.dsp == 1);
        // TIB should return address of the buffer
        REQUIRE(ctx.data_stack[0] != 0);
    }

    SECTION(">IN returns valid address") {
        auto ctx = execute_with_stdlib(">IN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] != 0);
    }

    SECTION("#TIB returns valid address") {
        auto ctx = execute_with_stdlib("#TIB");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] != 0);
    }

    SECTION("SOURCE returns TIB and length") {
        auto ctx = execute_with_stdlib("SOURCE");
        REQUIRE(ctx.dsp == 2);
        // First element should be TIB address
        // Second element should be #TIB value (initially 0)
        REQUIRE(ctx.data_stack[0] != 0);
        REQUIRE(ctx.data_stack[1] == 0);
    }

    SECTION("Input buffer state manipulation") {
        auto ctx = execute_with_stdlib(
            "42 #TIB ! "      // Set #TIB to 42
            "10 >IN ! "       // Set >IN to 10
            "#TIB @ >IN @"    // Read both back
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 10);
    }
}

TEST_CASE("Standard Library - String words", "[stdlib][strings]") {
    SECTION("COUNT extracts length and data from counted string") {
        auto ctx = execute_with_stdlib(
            "HERE DUP "          // Get buffer address, keep copy
            "5 OVER C! "         // Store length byte = 5
            "72 OVER 1 + C! "    // Store 'H'
            "69 OVER 2 + C! "    // Store 'E'
            "76 OVER 3 + C! "    // Store 'L'
            "76 OVER 4 + C! "    // Store 'L'
            "79 OVER 5 + C! "    // Store 'O'
            "COUNT "             // ( c-addr -- c-addr+1 u )
        );
        REQUIRE(ctx.dsp == 3);
        // Stack should have: original-addr, data-addr, length
        int64_t orig_addr = ctx.data_stack[0];
        int64_t data_addr = ctx.data_stack[1];
        int64_t length = ctx.data_stack[2];

        REQUIRE(data_addr == orig_addr + 1);  // Points past length byte
        REQUIRE(length == 5);                  // Length is 5
    }

    SECTION("MOVE copies non-overlapping memory") {
        auto ctx = execute_with_stdlib(
            "HERE DUP "          // Get two copies of HERE
            "72 OVER C! "        // Store 'H' at offset 0
            "73 OVER 1 + C! "    // Store 'I' at offset 1
            "OVER 100 + "        // Calculate dest = HERE + 100
            "2 "                 // Count = 2
            "MOVE "              // Copy 2 bytes
            "HERE 100 + C@ "     // Read first byte from dest
            "HERE 101 + C@"      // Read second byte from dest
        );
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[1] == 72);  // 'H'
        REQUIRE(ctx.data_stack[2] == 73);  // 'I'
    }

    SECTION("MOVE handles overlapping regions correctly") {
        auto ctx = execute_with_stdlib(
            "HERE "
            "65 OVER C! "        // Store 'A' at offset 0
            "66 OVER 1 + C! "    // Store 'B' at offset 1
            "67 OVER 2 + C! "    // Store 'C' at offset 2
            "DUP 1 + "           // dest = HERE + 1 (overlapping!)
            "3 "                 // Count = 3
            "MOVE "              // Should use CMOVE> automatically
            "HERE 1 + C@ "       // Read offset 1
            "HERE 2 + C@ "       // Read offset 2
            "HERE 3 + C@"        // Read offset 3
        );
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 65);  // 'A' copied to offset 1
        REQUIRE(ctx.data_stack[1] == 66);  // 'B' copied to offset 2
        REQUIRE(ctx.data_stack[2] == 67);  // 'C' copied to offset 3
    }

    SECTION("S= returns true for equal strings") {
        auto ctx = execute_with_stdlib(
            "HERE "
            "84 OVER C! "        // Store 'T'
            "69 OVER 1 + C! "    // Store 'E'
            "83 OVER 2 + C! "    // Store 'S'
            "3 "                 // len1
            "2DUP "              // Duplicate addr, len
            "S="                 // Compare identical strings
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // True in Forth
    }

    SECTION("S= returns false for different strings") {
        auto ctx = execute_with_stdlib(
            "HERE "
            "65 OVER C! "        // Store 'A'
            "66 OVER 1 + C! "    // Store 'B'
            "1 "                 // addr, len1 = 1
            "OVER 1 + 1 "        // addr2 (offset by 1), len2 = 1
            "S="                 // Compare 'A' vs 'B'
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);   // False
    }

    SECTION("BLANK fills memory with spaces") {
        auto ctx = execute_with_stdlib(
            "HERE "
            "0 OVER C! "         // Store 0
            "0 OVER 1 + C! "     // Store 0
            "0 OVER 2 + C! "     // Store 0
            "DUP 3 BLANK "       // Fill 3 bytes with spaces
            "HERE C@ "           // Read first byte
            "HERE 1 + C@ "       // Read second byte
            "HERE 2 + C@"        // Read third byte
        );
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[1] == 32);  // Space
        REQUIRE(ctx.data_stack[2] == 32);  // Space
        REQUIRE(ctx.data_stack[3] == 32);  // Space
    }

    SECTION("ERASE fills memory with zeros") {
        auto ctx = execute_with_stdlib(
            "HERE "
            "255 OVER C! "       // Store 255
            "255 OVER 1 + C! "   // Store 255
            "255 OVER 2 + C! "   // Store 255
            "DUP 3 ERASE "       // Fill 3 bytes with zeros
            "HERE C@ "           // Read first byte
            "HERE 1 + C@ "       // Read second byte
            "HERE 2 + C@"        // Read third byte
        );
        REQUIRE(ctx.dsp == 4);
        REQUIRE(ctx.data_stack[1] == 0);   // Zero
        REQUIRE(ctx.data_stack[2] == 0);   // Zero
        REQUIRE(ctx.data_stack[3] == 0);   // Zero
    }

    SECTION("PAD returns address 256 bytes after HERE") {
        auto ctx = execute_with_stdlib(
            "HERE PAD SWAP -"    // PAD - HERE should be 256
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 256);
    }
}

TEST_CASE("Standard Library - Interpreter state and execution", "[stdlib][state][execute]") {
    SECTION("FIND locates a defined word using string in memory") {
        auto ctx = execute_with_stdlib(
            ": TEST-WORD 42 ; "     // Define a word
            // Create string "TEST-WORD" in memory at HERE
            "HERE "                 // Address where we'll store the string
            "84 OVER C! "           // 'T'
            "69 OVER 1 + C! "       // 'E'
            "83 OVER 2 + C! "       // 'S'
            "84 OVER 3 + C! "       // 'T'
            "45 OVER 4 + C! "       // '-'
            "87 OVER 5 + C! "       // 'W'
            "79 OVER 6 + C! "       // 'O'
            "82 OVER 7 + C! "       // 'R'
            "68 OVER 8 + C! "       // 'D'
            "9 "                    // Length
            "FIND"                  // ( c-addr u -- xt flag )
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[1] == -1);  // flag: found
        REQUIRE(ctx.data_stack[0] != 0);   // xt: non-zero address
    }

    SECTION("FIND returns 0 flag for undefined word") {
        auto ctx = execute_with_stdlib(
            "HERE "                 // Address
            "78 OVER C! "           // 'N'
            "79 OVER 1 + C! "       // 'O'
            "83 OVER 2 + C! "       // 'S'
            "85 OVER 3 + C! "       // 'U'
            "67 OVER 4 + C! "       // 'C'
            "72 OVER 5 + C! "       // 'H'
            "87 OVER 6 + C! "       // 'W'
            "79 OVER 7 + C! "       // 'O'
            "82 OVER 8 + C! "       // 'R'
            "68 OVER 9 + C! "       // 'D'
            "10 "                   // Length
            "FIND"
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[1] == 0);   // flag: not found
        REQUIRE(ctx.data_stack[0] == 0);   // xt: 0 when not found
    }

    SECTION("' (tick) gets execution token and EXECUTE calls it") {
        auto ctx = execute_with_stdlib(
            ": TEST-WORD 42 ; "      // Define a word that pushes 42
            "' TEST-WORD EXECUTE"    // Get XT and execute it
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("' and EXECUTE work with nested words") {
        auto ctx = execute_with_stdlib(
            ": INNER 10 20 + ; "     // Word that adds 10 + 20
            "' INNER EXECUTE"        // Execute it
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 30);
    }

    SECTION("' and EXECUTE preserve stack correctly") {
        auto ctx = execute_with_stdlib(
            "100 "                   // Put value on stack
            ": PUSHVAL 99 ; "        // Word that pushes a value
            "' PUSHVAL EXECUTE"      // Execute it
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 100);  // Original value
        REQUIRE(ctx.data_stack[1] == 99);   // Value from executed word
    }

    SECTION("NUMBER parses positive decimal") {
        auto ctx = execute_with_stdlib(
            "HERE "                 // Address
            "49 OVER C! "           // '1'
            "50 OVER 1 + C! "       // '2'
            "51 OVER 2 + C! "       // '3'
            "3 "                    // Length
            "NUMBER"                // ( c-addr u -- n flag )
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 123);  // Parsed number
        REQUIRE(ctx.data_stack[1] == 1);    // flag: valid
    }

    SECTION("NUMBER parses negative number") {
        auto ctx = execute_with_stdlib(
            "HERE "                 // Address
            "45 OVER C! "           // '-'
            "52 OVER 1 + C! "       // '4'
            "50 OVER 2 + C! "       // '2'
            "3 "                    // Length
            "NUMBER"
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == -42);  // Parsed number
        REQUIRE(ctx.data_stack[1] == 1);    // flag: valid
    }

    SECTION("NUMBER returns 0 flag for invalid string") {
        auto ctx = execute_with_stdlib(
            "HERE "                 // Address
            "88 OVER C! "           // 'X'
            "89 OVER 1 + C! "       // 'Y'
            "90 OVER 2 + C! "       // 'Z'
            "3 "                    // Length
            "NUMBER"
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);    // number (0 for invalid)
        REQUIRE(ctx.data_stack[1] == 0);    // flag: invalid
    }
}

TEST_CASE("Standard Library - Error recovery primitives", "[stdlib][abort][error-recovery]") {
    SECTION("ABORT clears data stack") {
        auto ctx = execute_with_stdlib(
            "1 2 3 4 5 "  // Push 5 items
            "ABORT"       // Clear stacks
        );
        REQUIRE(ctx.dsp == 0);  // Stack should be empty
    }

    SECTION("ABORT clears return stack") {
        auto ctx = execute_with_stdlib(
            "1 >R 2 >R 3 >R "  // Push 3 items to return stack
            "ABORT"             // Clear stacks
        );
        REQUIRE(ctx.rsp == 0);  // Return stack should be empty
        REQUIRE(ctx.dsp == 0);  // Data stack should be empty too
    }

    SECTION("DSP! sets data stack pointer") {
        auto ctx = execute_with_stdlib(
            "1 2 3 4 5 "  // Push 5 items (dsp = 5)
            "0 DSP!"      // Reset DSP to 0
        );
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("RSP! sets return stack pointer") {
        auto ctx = execute_with_stdlib(
            "1 >R 2 >R 3 >R "  // Push to return stack (rsp = 3)
            "0 RSP!"           // Reset RSP to 0
        );
        REQUIRE(ctx.rsp == 0);
    }
}
