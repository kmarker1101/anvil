\ lexer-forth-tests.fth
\ Manual verification tests for the Forth lexer
\ Based on Rust lexer tests in tests/test_lexer.rs

INCLUDE src/lexer.fth

: TEST-LEX ( addr len -- )
  TOKENIZE
  ." Tokens: " .TOKENS
;

." ============================================" CR
." FORTH LEXER TESTS" CR
." ============================================" CR CR

." Test 1: Simple tokens (colon semicolon)" CR
S" : ;" TEST-LEX
." Expected: COLON SEMICOLON EOF" CR CR

." Test 2: Multiple numbers" CR
S" 42 -17 0" TEST-LEX
." Expected: NUMBER(42) NUMBER(-17) NUMBER(0) EOF" CR CR

." Test 3: Simple words" CR
S" DUP SWAP OVER" TEST-LEX
." Expected: WORD(DUP) WORD(SWAP) WORD(OVER) EOF" CR CR

\ Note: Forth lexer doesn't distinguish keywords - they're all WORDs
\ The parser will handle keyword recognition
." Test 4: Keywords (as words)" CR
S" IF THEN ELSE BEGIN UNTIL" TEST-LEX
." Expected: WORD(IF) WORD(THEN) WORD(ELSE) WORD(BEGIN) WORD(UNTIL) EOF" CR CR

\ Forth is case-insensitive at parse time, but our lexer preserves case
." Test 5: Case variations" CR
S" if IF iF If" TEST-LEX
." Expected: WORD(if) WORD(IF) WORD(iF) WORD(If) EOF" CR CR

." Test 6: Colon definition" CR
S" : SQUARE DUP * ;" TEST-LEX
." Expected: COLON WORD(SQUARE) WORD(DUP) WORD(*) SEMICOLON EOF" CR CR

." Test 7: IF THEN construct" CR
S" : ABS DUP 0 < IF NEGATE THEN ;" TEST-LEX
." Expected: COLON WORD(ABS) WORD(DUP) NUMBER(0) WORD(<) WORD(IF) WORD(NEGATE) WORD(THEN) SEMICOLON EOF" CR CR

." Test 8: Paren comment" CR
S" 42 ( this is a comment ) 99" TEST-LEX
." Expected: NUMBER(42) NUMBER(99) EOF" CR CR

." Test 9: Line comment" CR
S" 42 \\ this is a comment" TEST-LEX
." Expected: NUMBER(42) EOF" CR CR

." Test 10: Complex program (partial)" CR
S" : FACTORIAL DUP 1 <=" TEST-LEX
." Expected: COLON WORD(FACTORIAL) WORD(DUP) NUMBER(1) WORD(<=) EOF" CR CR

." Test 11: BEGIN WHILE REPEAT" CR
S" BEGIN DUP 0> WHILE DUP . 1- REPEAT" TEST-LEX
." Expected: WORD(BEGIN) WORD(DUP) WORD(0>) WORD(WHILE) WORD(DUP) WORD(.) WORD(1-) WORD(REPEAT) EOF" CR CR

." Test 12: DO LOOP" CR
S" 10 0 DO I . LOOP" TEST-LEX
." Expected: NUMBER(10) NUMBER(0) WORD(DO) WORD(I) WORD(.) WORD(LOOP) EOF" CR CR

." Test 13: ?DO LOOP" CR
S" 10 0 ?DO I . LOOP" TEST-LEX
." Expected: NUMBER(10) NUMBER(0) WORD(?DO) WORD(I) WORD(.) WORD(LOOP) EOF" CR CR

." Test 14: ?DO +LOOP" CR
S" 20 0 ?DO I . 2 +LOOP" TEST-LEX
." Expected: NUMBER(20) NUMBER(0) WORD(?DO) WORD(I) WORD(.) NUMBER(2) WORD(+LOOP) EOF" CR CR

." Test 15: Extra whitespace" CR
S"   :  SQUARE   DUP  *  ;  " TEST-LEX
." Expected: COLON WORD(SQUARE) WORD(DUP) WORD(*) SEMICOLON EOF" CR CR

." Test 16: Empty input" CR
S" " TEST-LEX
." Expected: EOF" CR CR

." Test 17: Whitespace only" CR
S"    " TEST-LEX
." Expected: EOF" CR CR

\ Note: Forth lexer treats "1+" differently than Rust lexer
\ Rust: "1+" -> WORD("1+")
\ Forth: "1+" -> NUMBER(1) WORD(+)
\ This is because Forth lexer tries to parse numbers first

." Test 18: Numbers vs digit-words" CR
S" 1 2 42 -17" TEST-LEX
." Expected: NUMBER(1) NUMBER(2) NUMBER(42) NUMBER(-17) EOF" CR CR

." Test 19: Mixed numbers and symbols" CR
S" : INCR 1 + ; 42 TEST" TEST-LEX
." Expected: COLON WORD(INCR) NUMBER(1) WORD(+) SEMICOLON NUMBER(42) WORD(TEST) EOF" CR CR

." Test 20: Zero comparison definition" CR
S" : 0< 0 < ;" TEST-LEX
." Expected: COLON WORD(0<) NUMBER(0) WORD(<) SEMICOLON EOF" CR CR

." ============================================" CR
." ALL TESTS COMPLETE" CR
." Review output above to verify correctness" CR
." ============================================" CR

BYE
