\ ============================================================================
\ ARITHMETIC TESTS
\ Tests for arithmetic primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE tests/testing.fth and call INIT-TESTS first

S" arithmetic.fth" TEST-FILE

TEST
5 NEGATE -5 EXPECT
S" NEGATE (positive)" TEST-NAME

TEST
-5 NEGATE 5 EXPECT
S" NEGATE (negative)" TEST-NAME

TEST
0 NEGATE 0 EXPECT
S" NEGATE (zero)" TEST-NAME

TEST
-7 ABS 7 EXPECT
S" ABS (negative)" TEST-NAME

TEST
7 ABS 7 EXPECT
S" ABS (positive)" TEST-NAME

TEST
0 ABS 0 EXPECT
S" ABS (zero)" TEST-NAME

TEST
5 1+ 6 EXPECT
S" 1+ (positive)" TEST-NAME

TEST
-5 1+ -4 EXPECT
S" 1+ (negative)" TEST-NAME

TEST
0 1+ 1 EXPECT
S" 1+ (zero)" TEST-NAME

TEST
5 1- 4 EXPECT
S" 1- (positive)" TEST-NAME

TEST
-5 1- -6 EXPECT
S" 1- (negative)" TEST-NAME

TEST
0 1- -1 EXPECT
S" 1- (zero)" TEST-NAME

TEST
3 2* 6 EXPECT
S" 2* (positive)" TEST-NAME

TEST
-3 2* -6 EXPECT
S" 2* (negative)" TEST-NAME

TEST
0 2* 0 EXPECT
S" 2* (zero)" TEST-NAME

TEST
8 2/ 4 EXPECT
S" 2/ (even)" TEST-NAME

TEST
9 2/ 4 EXPECT
S" 2/ (odd)" TEST-NAME

TEST
-8 2/ -4 EXPECT
S" 2/ (negative)" TEST-NAME

TEST
3 5 7 */ 2 EXPECT
S" */" TEST-NAME

TEST
4 8 MIN 4 EXPECT
S" MIN (first smaller)" TEST-NAME

TEST
8 4 MIN 4 EXPECT
S" MIN (second smaller)" TEST-NAME

TEST
5 5 MIN 5 EXPECT
S" MIN (equal)" TEST-NAME

TEST
-5 3 MIN -5 EXPECT
S" MIN (negative)" TEST-NAME

TEST
4 8 MAX 8 EXPECT
S" MAX (second larger)" TEST-NAME

TEST
8 4 MAX 8 EXPECT
S" MAX (first larger)" TEST-NAME

TEST
5 5 MAX 5 EXPECT
S" MAX (equal)" TEST-NAME

TEST
-5 3 MAX 3 EXPECT
S" MAX (negative)" TEST-NAME
