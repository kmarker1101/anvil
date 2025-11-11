\ ============================================================================
\ CHARACTER AND OUTPUT TESTS
\ Tests for BL, SPACE, and SPACES
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE tests/testing.fth and call INIT-TESTS first

S" chars.fth" TEST-FILE

TEST
BL 32 EXPECT
S" BL constant is 32" TEST-NAME

\ TEST
\ SPACE FALSE EXPECT
\ S" SPACE doesn't crash" TEST-NAME

\ TEST
\ 0 SPACES TRUE EXPECT
\ S" SPACES 0" TEST-NAME

\ TEST
\ 5 SPACES TRUE EXPECT
\ S" SPACES 5" TEST-NAME

\ TEST
\ 10 SPACES TRUE EXPECT
\ S" SPACES 10" TEST-NAME
