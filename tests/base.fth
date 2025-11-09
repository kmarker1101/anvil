\ ============================================================================
\ BASE AND NUMERIC CONVERSION TESTS
\ Tests for BASE, DECIMAL, HEX, and number prefixes
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth

S" base.fth" TEST-FILE

\ Test BASE variable exists and is initialized to decimal
TEST
BASE @ #10 EXPECT
TEST-NAME S" BASE initialized to 10"

\ Test decimal prefix
TEST
#42 #42 EXPECT
TEST-NAME S" #42 decimal prefix"

\ Test hex prefix
TEST
$10 #16 EXPECT
TEST-NAME S" $10 hex prefix equals 16"

\ Test hex prefix FF
TEST
$FF #255 EXPECT
TEST-NAME S" $FF hex prefix equals 255"

\ Test binary prefix
TEST
%1010 #10 EXPECT
TEST-NAME S" %1010 binary prefix equals 10"

\ Test binary prefix all ones
TEST
%11111111 #255 EXPECT
TEST-NAME S" %11111111 binary prefix equals 255"

\ Test DECIMAL word sets BASE to 10
TEST
HEX BASE @ #16 EXPECT
DECIMAL BASE @ #10 EXPECT
TEST-NAME S" DECIMAL sets BASE to 10"

\ Test HEX word sets BASE to 16
TEST
DECIMAL BASE @ #10 EXPECT
HEX BASE @ #16 EXPECT
TEST-NAME S" HEX sets BASE to 16"

\ Test BASE can be read and written
TEST
#42 BASE !
BASE @ #42 EXPECT
DECIMAL  \ restore to decimal
TEST-NAME S" BASE can be written with !"

\ Test multiple number prefixes in sequence
TEST
#10 $10 + #26 EXPECT
TEST-NAME S" #10 + $10 equals 26"

\ Test binary and hex together
TEST
%1111 $F = -1 EXPECT
TEST-NAME S" %1111 equals $F"

\ Test zero in different bases
TEST
#0 $0 = -1 EXPECT
TEST-NAME S" #0 equals $0"

TEST
#0 %0 = -1 EXPECT
TEST-NAME S" #0 equals %0"

\ Test large hex number
TEST
$FFFF #65535 EXPECT
TEST-NAME S" $FFFF equals 65535"

\ Test negative numbers still work
TEST
#-42 #42 + #0 EXPECT
TEST-NAME S" -42 + 42 equals 0"

\ Test BASE address is constant
TEST
BASE BASE = -1 EXPECT
TEST-NAME S" BASE returns same address"

\ Test switching bases multiple times
TEST
DECIMAL HEX DECIMAL HEX DECIMAL
BASE @ #10 EXPECT
TEST-NAME S" Multiple base switches end in DECIMAL"
