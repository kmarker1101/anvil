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

// Helper to load stdlib, compile and execute Forth code
ExecutionContext execute_forth_with_stdlib(const std::string& test_code,
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

// Note: Arithmetic, stack, bitwise, and comparison primitive tests have been
// moved to Forth-based tests in forth/tests/ directory. These tests run in all
// three execution modes (JIT, interpreter, AOT) automatically.
// See: forth/tests/arithmetic.fth, stack.fth, bitwise.fth, comparison.fth

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
        // Stack should have: [addr, len] (addr on bottom, len on top)
        REQUIRE(ctx.data_stack[0] != 0);  // addr (non-zero)
        REQUIRE(ctx.data_stack[1] == 5);  // length of "Hello"
    }

    SECTION("S\" with empty string") {
        auto ctx = execute_forth("S\" \"");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] != 0);  // addr still valid
        REQUIRE(ctx.data_stack[1] == 0);  // length = 0
    }

    SECTION("Multiple S\" strings") {
        auto ctx = execute_forth("S\" First\" S\" Second\"");
        REQUIRE(ctx.dsp == 4);
        // Stack: [first_addr, first_len, second_addr, second_len]
        REQUIRE(ctx.data_stack[1] == 5);  // "First" length
        REQUIRE(ctx.data_stack[3] == 6);  // "Second" length
    }

    SECTION(".\" prints string immediately (stack empty)") {
        auto ctx = execute_forth(".\" Hello\"");
        // ." consumes the string and prints it, leaving stack empty
        REQUIRE(ctx.dsp == 0);
    }

    SECTION(".\" with empty string") {
        auto ctx = execute_forth(".\" \"");
        REQUIRE(ctx.dsp == 0);
    }

    SECTION(".\" followed by stack operations") {
        auto ctx = execute_forth(".\" Test\" 42");
        // ." prints and consumes, then 42 is pushed
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Multiple .\" strings") {
        auto ctx = execute_forth(".\" First\" .\" Second\"");
        // Both strings printed and consumed
        REQUIRE(ctx.dsp == 0);
    }

    SECTION(".\" in word definition") {
        auto ctx = execute_forth(": GREET .\" Hello, World!\" ; GREET");
        REQUIRE(ctx.dsp == 0);
    }

    SECTION(".\" with S\" mixed") {
        auto ctx = execute_forth("S\" stored\" .\" printed\"");
        // S" pushes addr+len, ." prints
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[1] == 6);  // "stored" length
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

TEST_CASE("Compiler compiles CR primitive", "[compiler][cr][io]") {
    SECTION("CR alone") {
        auto ctx = execute_forth("CR");
        // CR doesn't affect the stack
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("CR with values on stack") {
        auto ctx = execute_forth("42 CR");
        // CR doesn't consume or produce values
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Multiple CRs") {
        auto ctx = execute_forth("1 2 3 CR CR CR");
        // Stack should be unchanged
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
    }

    SECTION("CR in user-defined word") {
        auto ctx = execute_forth(": NEWLINE CR ; 99 NEWLINE");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 99);
    }

    SECTION("CR with arithmetic") {
        auto ctx = execute_forth("5 3 + CR 2 *");
        // 5 + 3 = 8, CR, 8 * 2 = 16
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 16);
    }
}

TEST_CASE("Compiler compiles RECURSE", "[compiler][recurse][recursion]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple factorial using RECURSE") {
        // Factorial: : FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ;
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 5 FACT");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 120);  // 5! = 120
    }

    SECTION("Factorial of 0") {
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 0 FACT");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);  // 0! = 1
    }

    SECTION("Factorial of 1") {
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 1 FACT");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);  // 1! = 1
    }

    SECTION("Factorial of 6") {
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 6 FACT");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 720);  // 6! = 720
    }

    SECTION("Simple countdown using RECURSE") {
        // Countdown that sums: : COUNTDOWN DUP 0 > IF DUP 1- RECURSE + ELSE THEN ;
        auto ctx = execute_forth_with_stdlib(": COUNTDOWN DUP 0 > IF DUP 1- RECURSE + ELSE THEN ; 5 COUNTDOWN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);  // 5+4+3+2+1 = 15
    }

    SECTION("Countdown from 10") {
        auto ctx = execute_forth_with_stdlib(": COUNTDOWN DUP 0 > IF DUP 1- RECURSE + ELSE THEN ; 10 COUNTDOWN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 55);  // 10+9+8+...+1 = 55
    }

    SECTION("Countdown from 0 (base case)") {
        auto ctx = execute_forth_with_stdlib(": COUNTDOWN DUP 0 > IF DUP 1- RECURSE + ELSE THEN ; 0 COUNTDOWN");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Fibonacci using RECURSE") {
        // Fib: : FIB DUP 2 < IF ELSE DUP 1- RECURSE SWAP 2 - RECURSE + THEN ;
        // This is inefficient but tests RECURSE correctness
        // FIB(0)=0, FIB(1)=1, FIB(n)=FIB(n-1)+FIB(n-2)
        auto ctx = execute_forth_with_stdlib(": FIB DUP 2 < IF ELSE DUP 1- RECURSE SWAP 2 - RECURSE + THEN ; 0 FIB");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);  // FIB(0) = 0
    }

    SECTION("Fibonacci of 1") {
        auto ctx = execute_forth_with_stdlib(": FIB DUP 2 < IF ELSE DUP 1- RECURSE SWAP 2 - RECURSE + THEN ; 1 FIB");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);  // FIB(1) = 1
    }

    SECTION("Fibonacci of 5") {
        auto ctx = execute_forth_with_stdlib(": FIB DUP 2 < IF ELSE DUP 1- RECURSE SWAP 2 - RECURSE + THEN ; 5 FIB");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);  // FIB(5) = 5
    }

    SECTION("Fibonacci of 7") {
        auto ctx = execute_forth_with_stdlib(": FIB DUP 2 < IF ELSE DUP 1- RECURSE SWAP 2 - RECURSE + THEN ; 7 FIB");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 13);  // FIB(7) = 13
    }

    SECTION("RECURSE in JIT mode") {
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 4 FACT", ExecutionMode::JIT);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);  // 4! = 24
    }

    SECTION("RECURSE in Interpreter mode") {
        auto ctx = execute_forth_with_stdlib(": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; 4 FACT", ExecutionMode::Interpreter);
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 24);  // 4! = 24
    }

    SECTION("Multiple recursive words") {
        // Define two recursive words and use both
        auto ctx = execute_forth_with_stdlib(
            ": SUM DUP 0 > IF DUP 1- RECURSE + ELSE THEN ; "
            ": FACT DUP 1 > IF DUP 1- RECURSE * ELSE DROP 1 THEN ; "
            "4 SUM 3 FACT"
        );
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);  // SUM(4) = 4+3+2+1 = 10
        REQUIRE(ctx.data_stack[1] == 6);   // FACT(3) = 6
    }

    SECTION("RECURSE with stack manipulation") {
        // GCD using Euclidean algorithm: : GCD OVER 0 = IF NIP ELSE DUP ROT ROT /MOD DROP RECURSE THEN ;
        // GCD(48, 18) = 6
        auto ctx = execute_forth_with_stdlib(": GCD OVER 0 = IF NIP ELSE DUP ROT ROT /MOD DROP RECURSE THEN ; 48 18 GCD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 6);
    }

    SECTION("GCD of 100 and 35") {
        auto ctx = execute_forth_with_stdlib(": GCD OVER 0 = IF NIP ELSE DUP ROT ROT /MOD DROP RECURSE THEN ; 100 35 GCD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);  // GCD(100, 35) = 5
    }

    SECTION("GCD of equal numbers") {
        auto ctx = execute_forth_with_stdlib(": GCD OVER 0 = IF NIP ELSE DUP ROT ROT /MOD DROP RECURSE THEN ; 42 42 GCD");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);  // GCD(42, 42) = 42
    }
}

TEST_CASE("Compiler compiles EXIT", "[compiler][control][exit]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("Simple EXIT in word") {
        auto ctx = execute_forth(": TEST 1 2 EXIT 3 ; TEST");
        // Should push 1 and 2, then EXIT before pushing 3
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
    }

    SECTION("EXIT in IF branch") {
        auto ctx = execute_forth_with_stdlib(": TEST DUP 5 > IF EXIT THEN 99 ; 10 TEST");
        // 10 > 5 is true, so EXIT, leaving just 10 on stack
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("EXIT not taken in ELSE branch") {
        auto ctx = execute_forth_with_stdlib(": TEST DUP 5 > IF EXIT THEN 99 ; 3 TEST");
        // 3 > 5 is false, so continue and push 99
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 3);
        REQUIRE(ctx.data_stack[1] == 99);
    }

    SECTION("EXIT with conditional logic") {
        auto ctx = execute_forth(": ABS DUP 0 < IF -1 * EXIT THEN ; -5 ABS");
        // -5 < 0, so negate and EXIT
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 5);
    }

    SECTION("EXIT in loop") {
        auto ctx = execute_forth(": TEST 0 10 0 DO I + I 5 = IF EXIT THEN LOOP ; TEST");
        // Sum 0+1+2+3+4+5, then EXIT when I=5
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);
    }

    SECTION("Multiple paths with EXIT") {
        auto ctx = execute_forth(": TEST DUP 0 = IF DROP 0 EXIT THEN DUP 1 = IF DROP 1 EXIT THEN 99 ; 0 TEST");
        // Input is 0, first IF is true, push 0 and EXIT
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Multiple paths with EXIT - second path") {
        auto ctx = execute_forth(": TEST DUP 0 = IF DROP 0 EXIT THEN DUP 1 = IF DROP 1 EXIT THEN 99 ; 1 TEST");
        // Input is 1, second IF is true, push 1 and EXIT
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1);
    }

    SECTION("Multiple paths with EXIT - fallthrough") {
        auto ctx = execute_forth(": TEST DUP 0 = IF DROP 0 EXIT THEN DUP 1 = IF DROP 1 EXIT THEN 99 ; 5 TEST");
        // Input is 5, both IFs false, push 99
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 5);
        REQUIRE(ctx.data_stack[1] == 99);
    }
}

TEST_CASE("Compiler compiles UNLOOP", "[compiler][control][unloop][loops]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("UNLOOP with EXIT in DO loop") {
        auto ctx = execute_forth(": TEST 0 10 0 DO I + I 5 = IF UNLOOP EXIT THEN LOOP ; TEST");
        // Sum 0+1+2+3+4+5, when I=5 UNLOOP and EXIT
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);
    }

    SECTION("UNLOOP allows clean EXIT from nested loop") {
        auto ctx = execute_forth(": TEST 3 0 DO 3 0 DO I J + DUP 4 = IF UNLOOP UNLOOP EXIT THEN DROP LOOP LOOP 99 ; TEST");
        // Nested loop, exit when I+J=4, return that value
        // If no early exit, pushes 99
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 4);
    }

    SECTION("UNLOOP in conditional within loop") {
        auto ctx = execute_forth_with_stdlib(": TEST 0 20 0 DO I + I 7 > IF UNLOOP EXIT THEN LOOP ; TEST");
        // Sum 0+1+2+...+8, then UNLOOP and EXIT when I>7 (I=8)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 36);  // 0+1+2+3+4+5+6+7+8 = 36
    }

    SECTION("UNLOOP with early termination") {
        auto ctx = execute_forth(": TEST 100 0 DO I DUP 3 = IF UNLOOP EXIT THEN DROP LOOP 99 ; TEST");
        // Loop until I=3, then UNLOOP EXIT, leaving 3 on stack
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }

    SECTION("UNLOOP prevents stack corruption on EXIT") {
        auto ctx = execute_forth_with_stdlib(": TEST 5 0 DO I 1+ DUP 3 = IF UNLOOP EXIT THEN DROP LOOP 99 ; TEST");
        // Loop, when I+1=3 (I=2), UNLOOP EXIT leaving 3 on stack
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 3);
    }
}

TEST_CASE("Compiler compiles ?DO...LOOP", "[compiler][control][loops][questiondo]") {
    // Clear dictionary before each test
    global_dictionary.clear();

    SECTION("?DO...LOOP executes when start < limit") {
        auto ctx = execute_forth(": TEST 0 5 0 ?DO I + LOOP ; TEST");
        // Sum 0+1+2+3+4 = 10
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 10);
    }

    SECTION("?DO...LOOP skips when start = limit") {
        auto ctx = execute_forth(": TEST 99 5 5 ?DO I + LOOP ; TEST");
        // Loop doesn't execute, 99 remains
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 99);
    }

    SECTION("?DO...LOOP skips when start > limit") {
        auto ctx = execute_forth(": TEST 42 5 10 ?DO I + LOOP ; TEST");
        // limit=5, start=10: 10 >= 5 so loop doesn't execute, 42 remains
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("?DO...LOOP with negative range") {
        auto ctx = execute_forth(": TEST 0 0 -5 ?DO I + LOOP ; TEST");
        // -5 to -1: sum = -5+-4+-3+-2+-1 = -15
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -15);
    }

    SECTION("?DO...+LOOP with increment") {
        auto ctx = execute_forth(": TEST 0 10 0 ?DO I + 2 +LOOP ; TEST");
        // 0, 2, 4, 6, 8 : sum = 20
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 20);
    }

    SECTION("?DO...+LOOP skips when start >= limit") {
        auto ctx = execute_forth(": TEST 100 5 10 ?DO I + 2 +LOOP ; TEST");
        // start > limit, doesn't execute
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("Nested ?DO loops") {
        auto ctx = execute_forth(": TEST 0 3 0 ?DO 3 0 ?DO I J + + LOOP LOOP ; TEST");
        // Outer: I=0,1,2  Inner: J=0,1,2
        // Accumulates all I+J combinations
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 18);  // (0+0)+(0+1)+(0+2) + (1+0)+(1+1)+(1+2) + (2+0)+(2+1)+(2+2)
    }

    SECTION("?DO with LEAVE") {
        auto ctx = execute_forth(": TEST 0 10 0 ?DO I + I 5 = IF LEAVE THEN LOOP ; TEST");
        // Sum until I=5, then LEAVE
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 15);  // 0+1+2+3+4+5 = 15
    }

    SECTION("?DO with UNLOOP and EXIT") {
        auto ctx = execute_forth(": TEST 0 10 0 ?DO I + I 3 = IF UNLOOP EXIT THEN LOOP 99 ; TEST");
        // Sum 0+1+2+3, then UNLOOP EXIT
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 6);  // 0+1+2+3 = 6
    }
}

// ============================================================================
// VARIABLE Tests
// ============================================================================

TEST_CASE("VARIABLE - Basic storage and retrieval", "[variable]") {
    SECTION("Store and fetch a value") {
        auto ctx = execute_forth("VARIABLE MYVAR  100 MYVAR !  MYVAR @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("Store and fetch zero") {
        auto ctx = execute_forth("VARIABLE ZEROVAR  0 ZEROVAR !  ZEROVAR @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 0);
    }

    SECTION("Store and fetch negative value") {
        auto ctx = execute_forth("VARIABLE NEGVAR  -42 NEGVAR !  NEGVAR @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == -42);
    }

    SECTION("Store and fetch large value") {
        auto ctx = execute_forth("VARIABLE BIGVAR  1000000 BIGVAR !  BIGVAR @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 1000000);
    }
}

TEST_CASE("VARIABLE - Multiple variables", "[variable]") {
    SECTION("Two variables with different values") {
        auto ctx = execute_forth("VARIABLE VAR1  VARIABLE VAR2  "
                                 "10 VAR1 !  20 VAR2 !  "
                                 "VAR1 @  VAR2 @");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
    }

    SECTION("Three variables") {
        auto ctx = execute_forth("VARIABLE A  VARIABLE B  VARIABLE C  "
                                 "100 A !  200 B !  300 C !  "
                                 "A @  B @  C @");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 100);
        REQUIRE(ctx.data_stack[1] == 200);
        REQUIRE(ctx.data_stack[2] == 300);
    }

    SECTION("Variables don't interfere with each other") {
        auto ctx = execute_forth("VARIABLE X  VARIABLE Y  "
                                 "42 X !  X @  "
                                 "99 Y !  Y @  "
                                 "X @");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 42);  // First X @
        REQUIRE(ctx.data_stack[1] == 99);  // Y @
        REQUIRE(ctx.data_stack[2] == 42);  // Second X @ (unchanged)
    }
}

TEST_CASE("VARIABLE - Update values", "[variable]") {
    SECTION("Store, fetch, update, fetch") {
        auto ctx = execute_forth("VARIABLE VAR  "
                                 "10 VAR !  VAR @  "
                                 "20 VAR !  VAR @");
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
    }

    SECTION("Multiple updates") {
        auto ctx = execute_forth("VARIABLE CNT  "
                                 "1 CNT !  CNT @  "
                                 "2 CNT !  CNT @  "
                                 "3 CNT !  CNT @");
        REQUIRE(ctx.dsp == 3);
        REQUIRE(ctx.data_stack[0] == 1);
        REQUIRE(ctx.data_stack[1] == 2);
        REQUIRE(ctx.data_stack[2] == 3);
    }
}

TEST_CASE("VARIABLE - In word definitions", "[variable]") {
    SECTION("Variable used in word definition") {
        auto ctx = execute_forth("VARIABLE COUNTER  "
                                 ": SETCOUNTER ( n -- ) COUNTER ! ;  "
                                 ": GETCOUNTER ( -- n ) COUNTER @ ;  "
                                 "42 SETCOUNTER  GETCOUNTER");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 42);
    }

    SECTION("Variable modified by word") {
        auto ctx = execute_forth("VARIABLE VAL  "
                                 ": INIT ( -- ) 100 VAL ! ;  "
                                 "INIT  VAL @");
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }
}

TEST_CASE("VARIABLE - VARIABLE address behavior", "[variable]") {
    SECTION("VARIABLE pushes its address") {
        auto ctx = execute_forth("VARIABLE TEST  TEST");
        REQUIRE(ctx.dsp == 1);
        // Address should be non-zero and point to data space
        REQUIRE(ctx.data_stack[0] != 0);
    }

    SECTION("Two VARIABLE calls push different addresses") {
        auto ctx = execute_forth("VARIABLE A  VARIABLE B  A  B");
        REQUIRE(ctx.dsp == 2);
        // Addresses should be different
        REQUIRE(ctx.data_stack[0] != ctx.data_stack[1]);
    }
}

TEST_CASE("VARIABLE - Stack consumption by ! and @", "[variable][store][fetch]") {
    SECTION("! consumes both arguments") {
        auto ctx = execute_forth("VARIABLE X  50 X !");
        // ! should consume both the value and address
        REQUIRE(ctx.dsp == 0);
    }

    SECTION("@ consumes address and leaves value") {
        auto ctx = execute_forth("VARIABLE X  100 X !  X @");
        // Should have exactly 1 value on stack (the fetched value)
        REQUIRE(ctx.dsp == 1);
        REQUIRE(ctx.data_stack[0] == 100);
    }

    SECTION("Multiple stores and fetches") {
        auto ctx = execute_forth("VARIABLE X  "
                                 "10 X !  X @  "
                                 "20 X !  X @");
        // Should have 2 values: 10 and 20
        REQUIRE(ctx.dsp == 2);
        REQUIRE(ctx.data_stack[0] == 10);
        REQUIRE(ctx.data_stack[1] == 20);
    }
}

// Clean up dictionary after tests
TEST_CASE("Cleanup", "[.cleanup]") {
    global_dictionary.clear();
}
