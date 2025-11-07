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

\ Add more test files here as they are created:
\ INCLUDE forth/tests/definitions.fth

\ Print final report
REPORT

BYE
