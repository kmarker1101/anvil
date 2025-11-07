\ ============================================================================
\ ARITHMETIC TESTS
\ Tests for arithmetic primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE tests/testing.fth and call INIT-TESTS first

S" arithmetic.fth" TEST-FILE

TEST
5 NEGATE -5 EXPECT
S" NEGATE" TEST-NAME

TEST
-7 ABS 7 EXPECT
S" ABS (negative)" TEST-NAME

TEST
7 ABS 7 EXPECT
S" ABS (positive)" TEST-NAME

TEST
5 1+ 6 EXPECT
S" 1+" TEST-NAME

TEST
5 1- 4 EXPECT
S" 1-" TEST-NAME

TEST
3 2* 6 EXPECT
S" 2*" TEST-NAME

TEST
8 2/ 4 EXPECT
S" 2/" TEST-NAME

TEST
3 5 7 */ 2 EXPECT
S" */" TEST-NAME

TEST
4 8 MIN 4 EXPECT
S" MIN" TEST-NAME

TEST
4 8 MAX 8 EXPECT
S" MAX" TEST-NAME
