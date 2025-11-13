\ ============================================================================
\ BASE AND NUMERIC CONVERSION TESTS
\ Tests for BASE, DECIMAL, HEX, and number prefixes
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth

S" base.fth" TEST-FILE

\ Test BASE variable exists and is initialized to decimal
TEST
BASE @ #10 EXPECT
S" BASE initialized to 10" TEST-NAME

\ Test decimal prefix
TEST
#42 #42 EXPECT
S" #42 decimal prefix" TEST-NAME

\ Test hex prefix
TEST
$10 #16 EXPECT
S" $10 hex prefix equals 16" TEST-NAME

\ Test hex prefix FF
TEST
$FF #255 EXPECT
S" $FF hex prefix equals 255" TEST-NAME

\ Test binary prefix
TEST
%1010 #10 EXPECT
S" %1010 binary prefix equals 10" TEST-NAME

\ Test binary prefix all ones
TEST
%11111111 #255 EXPECT
S" %11111111 binary prefix equals 255" TEST-NAME

\ Test DECIMAL word sets BASE to 10
TEST
HEX BASE @ #16 EXPECT
DECIMAL BASE @ #10 EXPECT
S" DECIMAL sets BASE to 10" TEST-NAME

\ Test HEX word sets BASE to 16
TEST
DECIMAL BASE @ #10 EXPECT
HEX BASE @ #16 EXPECT
S" HEX sets BASE to 16" TEST-NAME

\ Test BASE can be read and written
\ TEST
\ #42 BASE !
\ BASE @ #42 EXPECT
\ DECIMAL  \ restore to decimal
\ S" BASE can be written with !" TEST-NAME

\ Test multiple number prefixes in sequence
TEST
#10 $10 + #26 EXPECT
S" #10 + $10 equals 26" TEST-NAME

\ Test binary and hex together
TEST
%1111 $F = -1 EXPECT
S" %1111 equals $F" TEST-NAME

\ Test zero in different bases
TEST
#0 $0 = -1 EXPECT
S" #0 equals $0" TEST-NAME

TEST
#0 %0 = -1 EXPECT
S" #0 equals %0" TEST-NAME

\ Test large hex number
TEST
$FFFF #65535 EXPECT
S" $FFFF equals 65535" TEST-NAME

\ Test negative numbers still work
TEST
#-42 #42 + #0 EXPECT
S" -42 + 42 equals 0" TEST-NAME

\ Test BASE address is constant
TEST
BASE BASE = -1 EXPECT
S" BASE returns same address" TEST-NAME

\ Test switching bases multiple times
TEST
DECIMAL HEX DECIMAL HEX DECIMAL
BASE @ #10 EXPECT
S" Multiple base switches end in DECIMAL" TEST-NAME
