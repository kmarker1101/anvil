\ parser-forth-tests.fth
\ Parser tests focusing on syntax parsing capabilities

INCLUDE src/lexer.fth
INCLUDE src/parser.fth

\ ============================================================================
\ TEST HELPERS
\ ============================================================================

VARIABLE TEST-NUMBER
0 TEST-NUMBER !

: TEST-START ( -- )
  ." TEST " TEST-NUMBER @ . ." : " ;

: TEST-END ( -- )
  ." PASSED" CR
  1 TEST-NUMBER +!
  \ Clear any stack remnants from the test
  BEGIN DEPTH WHILE DROP REPEAT
  \ Extra reset to be safe
  RESET-PARSER
;

: ASSERT-EQUAL ( n1 n2 -- )
  = 0= IF
    ." ASSERTION FAILED!" CR
    ABORT
  THEN ;

\ ============================================================================
\ BASIC PARSER FUNCTIONALITY TESTS
\ ============================================================================

TEST-START ." Tokenize simple expression" CR
S" 42 DUP +" TOKENIZE
TOKEN-COUNT @ 4 ASSERT-EQUAL
TEST-END

TEST-START ." Token navigation" CR
S" 42 DUP +" TOKENIZE
0 TOKEN-POS !
PEEK-TOKEN-TYPE TOK-NUMBER ASSERT-EQUAL
1 TOKEN-POS !
PEEK-TOKEN-TYPE TOK-WORD ASSERT-EQUAL
TEST-END

TEST-START ." Primitive lookup - DUP" CR
S" DUP" LOOKUP-PRIMITIVE 15 ASSERT-EQUAL
TEST-END

TEST-START ." Primitive lookup - +" CR
S" +" LOOKUP-PRIMITIVE 23 ASSERT-EQUAL
TEST-END

TEST-START ." Primitive lookup - not found" CR
S" NOTAPRIMITIVE" LOOKUP-PRIMITIVE -1 ASSERT-EQUAL
TEST-END

TEST-START ." Word matching - number token" CR
S" 42 DUP +" TOKENIZE
0 TOKEN-POS !
S" 42" WORD-IS? 0 ASSERT-EQUAL
TEST-END

TEST-START ." Word matching - DUP matches" CR
S" 42 DUP +" TOKENIZE
1 TOKEN-POS !
S" DUP" WORD-IS? -1 ASSERT-EQUAL
TEST-END

TEST-START ." Word matching - SWAP doesn't match" CR
S" 42 DUP +" TOKENIZE
1 TOKEN-POS !
S" SWAP" WORD-IS? 0 ASSERT-EQUAL
TEST-END

\ ============================================================================
\ BYTECODE GENERATION TESTS
\ ============================================================================

TEST-START ." Generate bytecode for simple definition" CR
S" : SQUARE DUP * ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 > -1 ASSERT-EQUAL
TEST-END

TEST-START ." Generate bytecode for number literal" CR
S" : DOUBLE 2 * ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 > -1 ASSERT-EQUAL
TEST-END

TEST-START ." Dictionary entry created" CR
S" : TEST DUP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
DICT-HERE @ 65000 > -1 ASSERT-EQUAL
TEST-END

\ ============================================================================
\ CONTROL STRUCTURE BYTECODE TESTS
\ ============================================================================

TEST-START ." IF-THEN generates branch" CR
S" : TEST DUP 0 < IF DROP THEN ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." IF-ELSE-THEN generates branches" CR
S" : TEST DUP 0 < IF DROP ELSE DUP THEN ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 48 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." BEGIN-UNTIL generates loop" CR
S" : TEST BEGIN DUP 1 - DUP 0 = UNTIL ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." BEGIN-WHILE-REPEAT generates loop" CR
S" : TEST BEGIN DUP 10 < WHILE DUP 1 + REPEAT DROP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 48 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." DO-LOOP generates loop structure" CR
S" : TEST 10 0 DO I LOOP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." ?DO-LOOP generates conditional loop" CR
S" : TEST 10 0 ?DO I LOOP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." DO-+LOOP generates increment loop" CR
S" : TEST 1 21 DO I 2 +LOOP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." ?DO-+LOOP generates conditional increment loop" CR
S" : TEST 20 0 ?DO I 2 +LOOP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

\ ============================================================================
\ VARIABLE AND CONSTANT
\ ============================================================================

TEST-START ." VARIABLE generates bytecode" CR
S" VARIABLE MYVAR" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 > -1 ASSERT-EQUAL
TEST-END

TEST-START ." CONSTANT generates bytecode" CR
S" 42 CONSTANT ANSWER" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 > -1 ASSERT-EQUAL
TEST-END

TEST-START ." RECURSE generates call to current word" CR
S" : TEST DUP 0 > IF 1 - RECURSE THEN ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

TEST-START ." EXIT generates return" CR
S" : TEST DUP 0 = IF EXIT THEN DROP ;" TOKENIZE
RESET-PARSER
PARSE-PROGRAM
BYTECODE-HERE @ 45000 32 + > -1 ASSERT-EQUAL
TEST-END

\ ============================================================================
\ SUMMARY
\ ============================================================================

CR
." ============================================" CR
." ALL TESTS COMPLETE" CR
." Total tests run: " TEST-NUMBER @ . CR
." ============================================" CR

BYE
