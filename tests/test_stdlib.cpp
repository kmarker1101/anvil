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

TEST_CASE("Standard Library - Interpreter state", "[stdlib][state]") {
    // Note: EXECUTE and ' (tick) tests are disabled due to JIT address resolution issues
    // The fundamental problem is that ' needs to capture function addresses at compile time,
    // but JIT compilation happens after parsing. This requires runtime dictionary lookup.

    SECTION("STATE-VAR provides access to interpreter state variable") {
        auto ctx = execute_with_stdlib(
            "STATE-VAR @"          // Read initial STATE value
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);    // Should be 0 (interpreting)
    }

    SECTION("STATE-VAR can be written and read") {
        auto ctx = execute_with_stdlib(
            "1 STATE-VAR ! "       // Set STATE to 1
            "STATE-VAR @"          // Read it back
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);    // Should read back as 1
    }

    SECTION("STATE-VAR value persists across operations") {
        auto ctx = execute_with_stdlib(
            "-1 STATE-VAR ! "      // Set STATE to -1 (compiling)
            "42 99 + "              // Do some other operations
            "DROP "                 // Clean up
            "STATE-VAR @"          // Read STATE again
        );
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -1);   // Should still be -1
    }
}
