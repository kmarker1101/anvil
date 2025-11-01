\ ============================================================================
\ ARITHMETIC TESTS
\ Tests for arithmetic primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE forth/testing.fth and call INIT-TESTS first

S" arithmetic.fth" TEST-FILE

TEST
5 3 + 8 EXPECT
S" Addition" TEST-NAME

TEST
10 3 - 7 EXPECT
S" Subtraction" TEST-NAME

TEST
6 7 * 42 EXPECT
S" Multiplication" TEST-NAME

TEST
15 3 /MOD 5 EXPECT 0 EXPECT
S" Division (exact)" TEST-NAME

TEST
17 5 /MOD 3 EXPECT 2 EXPECT
S" Division (with remainder)" TEST-NAME
