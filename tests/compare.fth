\ ============================================================================
\ COMPARISON TESTS
\ Tests for comparison primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth

S" compare.fth" TEST-FILE

TEST
5 6 - 0= 0 EXPECT
S" 0= (false)" TEST-NAME

TEST
0 0= -1 EXPECT
S" 0= (zero)" TEST-NAME

TEST
5 5 - 0= -1 EXPECT
S" 0= (true)" TEST-NAME

TEST
5 0< 0 EXPECT
S" 0< (positive)" TEST-NAME

TEST
-5 0< -1 EXPECT
S" 0< (negative)" TEST-NAME

TEST
0 0< 0 EXPECT
S" 0< (zero)" TEST-NAME

TEST
-5 0> 0 EXPECT
S" 0> (negative)" TEST-NAME

TEST
5 0> -1 EXPECT
S" 0> (positive)" TEST-NAME

TEST
0 0> 0 EXPECT
S" 0> (zero)" TEST-NAME

TEST
5 0<> -1 EXPECT
S" 0<> (positive)" TEST-NAME

TEST
-5 0<> -1 EXPECT
S" 0<> (negative)" TEST-NAME

TEST
0 0<> 0 EXPECT
S" 0<> (zero)" TEST-NAME

TEST
5 3 <> -1 EXPECT
S" <> (not equal)" TEST-NAME

TEST
5 5 <> 0 EXPECT
S" <> (equal)" TEST-NAME

TEST
-5 -3 <> -1 EXPECT
S" <> (negative not equal)" TEST-NAME

TEST
-5 5 <> -1 EXPECT
S" <> (negative positive)" TEST-NAME

TEST
5 3 >= -1 EXPECT
S" >= (greater than)" TEST-NAME

TEST
5 5 >= -1 EXPECT
S" >= (equal)" TEST-NAME

TEST
3 5 >= 0 EXPECT
S" >= (less than)" TEST-NAME

TEST
-3 -5 >= -1 EXPECT
S" >= (negative greater)" TEST-NAME

TEST
-5 3 >= 0 EXPECT
S" >= (negative vs positive)" TEST-NAME

TEST
3 5 <= -1 EXPECT
S" <= (less than)" TEST-NAME

TEST
5 5 <= -1 EXPECT
S" <= (equal)" TEST-NAME

TEST
5 3 <= 0 EXPECT
S" <= (greater than)" TEST-NAME

TEST
-5 -3 <= -1 EXPECT
S" <= (negative less)" TEST-NAME

TEST
-5 3 <= -1 EXPECT
S" <= (negative vs positive)" TEST-NAME
