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

\ ============================================================================
\ COUNT TESTS
\ ============================================================================

\ Test COUNT with empty string (length 0)
TEST
9000 DUP 0 SWAP C!
COUNT
0 EXPECT
9001 EXPECT
S" COUNT - empty string" TEST-NAME

\ Test COUNT with single character
TEST
9100 DUP 1 SWAP C!
DUP 1+ 65 SWAP C!
COUNT
1 EXPECT
9101 EXPECT
S" COUNT - single char" TEST-NAME

\ Test COUNT with multiple characters
TEST
9200 DUP 5 SWAP C!
COUNT
5 EXPECT
9201 EXPECT
S" COUNT - length 5" TEST-NAME

\ Test COUNT with maximum single-byte length
TEST
9300 DUP 255 SWAP C!
COUNT
255 EXPECT
9301 EXPECT
S" COUNT - length 255" TEST-NAME

\ Test COUNT returns correct address (past length byte)
TEST
9400 DUP 3 SWAP C!
DUP 1+ 72 SWAP C!
DUP 2 + 73 SWAP C!
DUP 3 + 33 SWAP C!
COUNT DROP C@
72 EXPECT
S" COUNT - correct address returned" TEST-NAME

\ ============================================================================
\ S= TESTS
\ ============================================================================

\ Helper: Set up two identical strings "HELLO" at different addresses
\ String 1 at 10000, String 2 at 11000

\ Test S= with equal strings
TEST
\ Write "HELLO" at 10000
10000 DUP 72 SWAP C!
10001 DUP 69 SWAP C!
10002 DUP 76 SWAP C!
10003 DUP 76 SWAP C!
10004 79 SWAP C!
\ Write "HELLO" at 11000
11000 DUP 72 SWAP C!
11001 DUP 69 SWAP C!
11002 DUP 76 SWAP C!
11003 DUP 76 SWAP C!
11004 79 SWAP C!
\ Compare
10000 5 11000 5 S=
-1 EXPECT
S" S= - equal strings" TEST-NAME

\ Test S= with different strings
TEST
\ "HELLO" at 10000 (already there)
\ Write "WORLD" at 11000
11000 DUP 87 SWAP C!
11001 DUP 79 SWAP C!
11002 DUP 82 SWAP C!
11003 DUP 76 SWAP C!
11004 68 SWAP C!
\ Compare
10000 5 11000 5 S=
0 EXPECT
S" S= - different strings" TEST-NAME

\ Test S= with different lengths
TEST
\ "HELLO" (5 chars) at 10000
\ "HI" (2 chars) at 11000
11000 DUP 72 SWAP C!
11001 73 SWAP C!
\ Compare
10000 5 11000 2 S=
0 EXPECT
S" S= - different lengths" TEST-NAME

\ Test S= with empty strings
TEST
10000 0 11000 0 S=
-1 EXPECT
S" S= - empty strings equal" TEST-NAME

\ Test S= with one empty string
TEST
10000 5 11000 0 S=
0 EXPECT
S" S= - non-empty vs empty" TEST-NAME

\ Test S= with empty vs non-empty
TEST
10000 0 11000 5 S=
0 EXPECT
S" S= - empty vs non-empty" TEST-NAME

\ Test S= case sensitive (lowercase != uppercase)
TEST
\ "hello" at 10000
10000 DUP 104 SWAP C!
10001 DUP 101 SWAP C!
10002 DUP 108 SWAP C!
10003 DUP 108 SWAP C!
10004 111 SWAP C!
\ "HELLO" at 11000
11000 DUP 72 SWAP C!
11001 DUP 69 SWAP C!
11002 DUP 76 SWAP C!
11003 DUP 76 SWAP C!
11004 79 SWAP C!
\ Compare
10000 5 11000 5 S=
0 EXPECT
S" S= - case sensitive" TEST-NAME

\ Test S= with single character strings (equal)
TEST
10000 65 SWAP C!
11000 65 SWAP C!
10000 1 11000 1 S=
-1 EXPECT
S" S= - single char equal" TEST-NAME

\ Test S= with single character strings (different)
TEST
10000 65 SWAP C!
11000 66 SWAP C!
10000 1 11000 1 S=
0 EXPECT
S" S= - single char different" TEST-NAME

\ Test S= with identical prefix but different length
TEST
\ "TEST" at 10000
10000 DUP 84 SWAP C!
10001 DUP 69 SWAP C!
10002 DUP 83 SWAP C!
10003 84 SWAP C!
\ "TESTING" at 11000
11000 DUP 84 SWAP C!
11001 DUP 69 SWAP C!
11002 DUP 83 SWAP C!
11003 DUP 84 SWAP C!
11004 DUP 73 SWAP C!
11005 DUP 78 SWAP C!
11006 71 SWAP C!
\ Compare
10000 4 11000 7 S=
0 EXPECT
S" S= - prefix match but different length" TEST-NAME
