\ ============================================================================
\ STRING TESTS
\ Tests for string manipulation
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth
\ If running standalone, you need to INCLUDE test/testing.fth and call INIT-TESTS first

S" memory.fth" TEST-FILE

\ ============================================================================
\ FILL TESTS
\ ============================================================================

\ Test FILL with single byte
TEST
1000 DUP 1 65 FILL C@ 65 EXPECT
S" FILL - single byte with 'A' (65)" TEST-NAME

\ Test FILL with multiple bytes
TEST
2000 DUP 5 42 FILL
DUP C@ 42 EXPECT
DUP 1 + C@ 42 EXPECT
DUP 2 + C@ 42 EXPECT
DUP 3 + C@ 42 EXPECT
4 + C@ 42 EXPECT
S" FILL - 5 bytes with 42" TEST-NAME

\ Test FILL with zero length (should do nothing)
TEST
3000 DUP 100 SWAP C!
DUP 0 65 FILL
C@ 100 EXPECT
S" FILL - zero length does nothing" TEST-NAME

\ Test FILL with different characters
TEST
4000 DUP 3 88 FILL
DUP C@ 88 EXPECT
DUP 1 + C@ 88 EXPECT
2 + C@ 88 EXPECT
S" FILL - 3 bytes with 'X' (88)" TEST-NAME

\ Test FILL with space character (32)
TEST
5000 DUP 4 32 FILL
DUP C@ 32 EXPECT
DUP 1 + C@ 32 EXPECT
DUP 2 + C@ 32 EXPECT
3 + C@ 32 EXPECT
S" FILL - 4 bytes with space (32)" TEST-NAME

\ Test FILL with zero character
TEST
6000 DUP 3 0 FILL
DUP C@ 0 EXPECT
DUP 1 + C@ 0 EXPECT
2 + C@ 0 EXPECT
S" FILL - 3 bytes with null (0)" TEST-NAME

\ Test FILL at different memory locations
TEST
7000 DUP 2 90 FILL
DUP C@ 90 EXPECT
1 + C@ 90 EXPECT
S" FILL - at address 7000" TEST-NAME

\ Test FILL with larger count
TEST
8000 DUP 10 48 FILL
DUP C@ 48 EXPECT
DUP 5 + C@ 48 EXPECT
9 + C@ 48 EXPECT
S" FILL - 10 bytes with '0' (48)" TEST-NAME
