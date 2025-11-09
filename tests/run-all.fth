\ ============================================================================
\ RUN ALL TESTS
\ Main test runner that executes all test files
\ ============================================================================

\ Load standard library
\ INCLUDE src/stdlib.fth

\ Load testing framework
INCLUDE tests/testing.fth

\ Initialize test counters
INIT-TESTS

\ Run all test suites
INCLUDE tests/arithmetic.fth
INCLUDE tests/stack.fth
INCLUDE tests/compare.fth
INCLUDE examples/math.fth
INCLUDE tests/memory.fth
INCLUDE tests/base.fth
INCLUDE tests/chars.fth

\ Add more test files here as they are created:
\ INCLUDE forth/tests/definitions.fth

\ Print final report
REPORT

BYE
