#include <catch2/catch_test_macros.hpp>
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "execution_engine.h"

using namespace anvil;

// Helper to compile and execute Forth code
// mode: JIT (default) or Interpreter
ExecutionContext execute_forth(const std::string& source,
                                ExecutionMode mode = ExecutionMode::JIT) {
    initialize_llvm(mode);
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

TEST_CASE("Compiler compiles literals", "[compiler][literal]") {
    SECTION("Single literal") {
        auto ctx = execute_forth("42");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Multiple literals") {
        auto ctx = execute_forth("10 20 30");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 30);
    }

    SECTION("Negative literal") {
        auto ctx = execute_forth("-123");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -123);
    }

    SECTION("Hex literal") {
        auto ctx = execute_forth("0xFF");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 255);
    }
}

TEST_CASE("Compiler compiles IF...THEN", "[compiler][control][if]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("IF...THEN true case") {
        auto ctx = execute_forth("-1 IF 42 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("IF...THEN false case") {
        auto ctx = execute_forth("0 IF 42 THEN");
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("IF...ELSE...THEN true branch") {
        auto ctx = execute_forth("-1 IF 100 ELSE 200 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("IF...ELSE...THEN false branch") {
        auto ctx = execute_forth("0 IF 100 ELSE 200 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 200);
    }
}

TEST_CASE("Compiler compiles arithmetic primitives", "[compiler][primitives][arithmetic]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Addition") {
        auto ctx = execute_forth("5 3 +");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 8);
    }

    SECTION("Subtraction") {
        auto ctx = execute_forth("10 3 -");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 7);
    }

    SECTION("Multiplication") {
        auto ctx = execute_forth("6 7 *");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("/MOD") {
        auto ctx = execute_forth("17 5 /MOD");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 2);  // remainder
        REQUIRE(ctx.data_stack[1] == 3);  // quotient
    }

    SECTION("Complex arithmetic expression") {
        auto ctx = execute_forth("10 5 + 3 *");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 45);
    }
}

TEST_CASE("Compiler compiles stack primitives", "[compiler][primitives][stack]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("DUP") {
        auto ctx = execute_forth("42 DUP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 42);
        REQUIRE(ctx.data_stack[1] == 42);
    }

    SECTION("DROP") {
        auto ctx = execute_forth("10 20 30 DROP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
    }

    SECTION("SWAP") {
        auto ctx = execute_forth("10 20 SWAP");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 20);
        REQUIRE(ctx.data_stack[1] == 10);
    }

    SECTION("OVER") {
        auto ctx = execute_forth("10 20 OVER");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
        REQUIRE(ctx.data_stack[2] == 10);
    }

    SECTION("ROT") {
        auto ctx = execute_forth("10 20 30 ROT");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 20);
        REQUIRE(ctx.data_stack[1] == 30);
        REQUIRE(ctx.data_stack[2] == 10);
    }

    SECTION("Combined stack operations") {
        auto ctx = execute_forth("5 DUP * ");  // Square: 5 DUP * = 25
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);
    }
}

TEST_CASE("Compiler compiles bitwise primitives", "[compiler][primitives][bitwise]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("AND") {
        auto ctx = execute_forth("0xFF 0x0F AND");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0x0F);
    }

    SECTION("OR") {
        auto ctx = execute_forth("0xF0 0x0F OR");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0xFF);
    }

    SECTION("XOR") {
        auto ctx = execute_forth("0xFF 0x0F XOR");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0xF0);
    }

    SECTION("INVERT") {
        auto ctx = execute_forth("0 INVERT");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // All bits set
    }
}

TEST_CASE("Compiler compiles comparison primitives", "[compiler][primitives][comparison]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("< true case") {
        auto ctx = execute_forth("3 5 <");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);  // Forth true
    }

    SECTION("< false case") {
        auto ctx = execute_forth("5 3 <");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Forth false
    }

    SECTION("< equal case") {
        auto ctx = execute_forth("5 5 <");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // Forth false
    }
}

TEST_CASE("Compiler compiles primitives with control flow", "[compiler][primitives][control]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("IF with comparison") {
        auto ctx = execute_forth("3 5 < IF 100 ELSE 200 THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("IF with arithmetic in branches") {
        auto ctx = execute_forth("-1 IF 10 20 + ELSE 30 40 + THEN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 30);
    }
}

TEST_CASE("Compiler compiles DO...LOOP", "[compiler][loops]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple DO...LOOP") {
        // Sum from 0 to 4: 0+1+2+3+4 = 10
        auto ctx = execute_forth("0  5 0 DO I + LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("DO...LOOP with multiplication") {
        // Multiply all numbers from 1 to 4: 1*2*3*4 = 24
        auto ctx = execute_forth("1  5 1 DO I * LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);
    }

    SECTION("DO...+LOOP with increment of 2") {
        // Sum 0, 2, 4, 6, 8 = 20
        auto ctx = execute_forth("0  10 0 DO I + 2 +LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 20);
    }

    SECTION("DO...+LOOP with increment of 3") {
        // Sum 0, 3, 6, 9 = 18
        auto ctx = execute_forth("0  12 0 DO I + 3 +LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 18);
    }

    SECTION("Nested DO...LOOP with I and J") {
        // Outer loop: 0,1,2  Inner loop: 0,1
        // Sum all I*J: (0*0 + 0*1) + (1*0 + 1*1) + (2*0 + 2*1) = 0 + 1 + 2 = 3
        auto ctx = execute_forth("0  3 0 DO  2 0 DO  J I * +  LOOP  LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }

    SECTION("DO...LOOP with LEAVE") {
        // Sum from 0 until we hit 5, then exit
        auto ctx = execute_forth("0  10 0 DO I DUP 5 = IF DROP LEAVE THEN + LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);  // 0+1+2+3+4 = 10 (stops at 5)
    }

    SECTION("Empty loop (start == limit)") {
        auto ctx = execute_forth("42  5 5 DO I + LOOP");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);  // Loop doesn't execute
    }

    SECTION("DO...LOOP in user-defined word") {
        auto ctx = execute_forth(": SUM10 0  10 0 DO I + LOOP ; SUM10");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 45);  // 0+1+2+...+9 = 45
    }
}

TEST_CASE("Compiler compiles strings", "[compiler][strings]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("S\" pushes address and length") {
        auto ctx = execute_forth("S\" Hello\" ");
        REQUIRE(ctx.dsp == 2);
        // Stack should have: [addr, len]
        REQUIRE(ctx.data_stack[0] == 5);  // length of "Hello"
        // addr is at stack[1], should be non-zero
        REQUIRE(ctx.data_stack[1] != 0);
    }

    SECTION("S\" with empty string") {
        auto ctx = execute_forth("S\" \"");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 0);  // length = 0
        REQUIRE(ctx.data_stack[1] != 0);  // addr still valid
    }

    SECTION("Multiple S\" strings") {
        auto ctx = execute_forth("S\" First\" S\" Second\"");
        REQUIRE(ctx.dsp == 4);
        // Stack: [first_len, first_addr, second_len, second_addr]
        REQUIRE(ctx.data_stack[0] == 5);  // "First" length
        REQUIRE(ctx.data_stack[2] == 6);  // "Second" length
    }
}

TEST_CASE("Compiler compiles user-defined words", "[compiler][definitions]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple user-defined word") {
        auto ctx = execute_forth(": SQUARE DUP * ; 5 SQUARE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 25);
    }

    SECTION("User word calling another user word") {
        auto ctx = execute_forth(": DOUBLE DUP + ; : QUADRUPLE DOUBLE DOUBLE ; 3 QUADRUPLE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 12);
    }

    SECTION("User word with arithmetic") {
        auto ctx = execute_forth(": ADD3 3 + ; 10 ADD3");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 13);
    }

    SECTION("User word with multiple operations") {
        auto ctx = execute_forth(": AVG + 2 /MOD SWAP DROP ; 10 20 AVG");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);
    }

    SECTION("User word with stack manipulation") {
        // ROT ROT rotates third item to top: (a b c -- c a b)
        auto ctx = execute_forth(": THIRD ROT ROT ; 10 20 30 THIRD");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 30);
        REQUIRE(ctx.data_stack[1] == 10);
        REQUIRE(ctx.data_stack[2] == 20);
    }

    SECTION("User word with control flow") {
        auto ctx = execute_forth(": ABS DUP 0 < IF -1 * THEN ; -5 ABS");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("Multiple definitions") {
        auto ctx = execute_forth(": DOUBLE DUP + ; : TRIPLE DUP 2 * + ; 5 DOUBLE 7 TRIPLE");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 21);
    }
}

// Clean up dictionary after tests
TEST_CASE("Cleanup", "[.cleanup]") {
    global_dictionary.clear();
}
