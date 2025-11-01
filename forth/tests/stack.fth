\ ============================================================================
\ STACK TESTS
\ Tests for stack manipulation primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE forth/testing.fth and call INIT-TESTS first

S" stack.fth" TEST-FILE

TEST
42 DUP + 84 EXPECT
S" DUP" TEST-NAME

TEST
5 7 SWAP 5 EXPECT 7 EXPECT
S" SWAP" TEST-NAME

TEST
1 2 3 DROP 2 EXPECT 1 EXPECT
S" DROP" TEST-NAME

TEST
10 20 OVER 10 EXPECT 20 EXPECT 10 EXPECT
S" OVER" TEST-NAME
