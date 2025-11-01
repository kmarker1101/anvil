\ ============================================================================
\ COMPARISON TESTS
\ Tests for comparison primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE forth/testing.fth and call INIT-TESTS first

S" comparison.fth" TEST-FILE

TEST
3 5 < -1 EXPECT
S" Less than (true)" TEST-NAME

TEST
5 3 < 0 EXPECT
S" Less than (false)" TEST-NAME

TEST
5 5 = -1 EXPECT
S" Equals (true)" TEST-NAME

TEST
5 3 = 0 EXPECT
S" Equals (false)" TEST-NAME

TEST
5 0> -1 EXPECT
S" 0> (true)" TEST-NAME

TEST
0 0> 0 EXPECT
S" 0> (zero)" TEST-NAME

TEST
-5 0> 0 EXPECT
S" 0> (false)" TEST-NAME
