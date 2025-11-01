\ ============================================================================
\ RUN ALL TESTS
\ Main test runner that executes all test files
\ ============================================================================

\ Load testing framework
INCLUDE forth/testing.fth

\ Initialize test counters
INIT-TESTS

\ Run all test suites
INCLUDE forth/tests/arithmetic.fth
INCLUDE forth/tests/stack.fth
INCLUDE forth/tests/comparison.fth
INCLUDE forth/tests/bitwise.fth

\ Add more test files here as they are created:
\ INCLUDE forth/tests/control-flow.fth
\ INCLUDE forth/tests/memory.fth
\ INCLUDE forth/tests/strings.fth
\ INCLUDE forth/tests/definitions.fth

\ Print final report
REPORT
