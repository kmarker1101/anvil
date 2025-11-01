\ ============================================================================
\ BITWISE TESTS
\ Tests for bitwise operation primitives
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE forth/testing.fth and call INIT-TESTS first

S" bitwise.fth" TEST-FILE

TEST
15 7 AND 7 EXPECT
S" AND" TEST-NAME

TEST
8 4 OR 12 EXPECT
S" OR" TEST-NAME

TEST
15 10 XOR 5 EXPECT
S" XOR" TEST-NAME

TEST
0 INVERT -1 EXPECT
S" INVERT" TEST-NAME
