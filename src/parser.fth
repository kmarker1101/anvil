\ parser.fth - Forth-based Parser
\ This parser converts tokens (from lexer) into bytecode instructions

\ ============================================================================
\ TOKEN TYPES (from lexer.fth)
\ ============================================================================

\ Token types - we can't use CONSTANT since Anvil doesn't support it yet
\ So we define words that push these values instead
: TOK-EOF 0 ;
: TOK-COLON 1 ;
: TOK-SEMICOLON 2 ;
: TOK-NUMBER 3 ;
: TOK-WORD 4 ;
: TOK-STRING 5 ;

\ These are recognized as TOK-WORD by the lexer, we check the string value
\ (No separate token types for keywords)
\ TOK-IF, TOK-THEN, TOK-ELSE, TOK-BEGIN, TOK-UNTIL, etc. are all TOK-WORD

\ ============================================================================
\ BYTECODE INSTRUCTION OPCODES
\ ============================================================================

\ Opcodes must match Rust Instruction enum in src/compiler.rs
\ We use word definitions instead of CONSTANT since Anvil doesn't support it
: OP-LITERAL 0 ;
: OP-PRIMITIVE 1 ;
: OP-CALL 2 ;
: OP-BRANCH-IF-ZERO 3 ;
: OP-BRANCH 4 ;
: OP-RETURN 5 ;
: OP-DO-SETUP 6 ;
: OP-QDO-SETUP 7 ;
: OP-LOOP 8 ;
: OP-PLUS-LOOP 9 ;
: OP-LOOP-END 10 ;
: OP-ALLOC-STRING 11 ;
: OP-VARIABLE-ADDR 12 ;
: OP-BYE 13 ;

\ Instruction size: [opcode: 8 bytes][data: 8 bytes] = 16 bytes
: INSTRUCTION-SIZE 16 ;

\ ============================================================================
\ MEMORY LAYOUT
\ ============================================================================

\ Token buffer (from lexer, read-only) - moved to 35000 to avoid variable conflicts
: TOKEN-BUFFER 35000 ;
: TOKEN-SIZE 24 ;

\ Bytecode buffer (parser output)
: BYTECODE-BUFFER 45000 ;
VARIABLE BYTECODE-HERE
45000 BYTECODE-HERE !

\ Branch stack (for backpatching control flow)
: BRANCH-STACK-BASE 55000 ;
VARIABLE BRANCH-SP
55000 BRANCH-SP !

\ Dictionary (word definitions)
: DICT-BASE 65000 ;
VARIABLE DICT-HERE
65000 DICT-HERE !
VARIABLE LATEST    \ Points to most recent dictionary entry
0 LATEST !

\ Parser state
VARIABLE TOKEN-POS    \ Current token index
0 TOKEN-POS !
VARIABLE CURRENT-WORD-ADDR   \ Code address of word being compiled (for RECURSE)
0 CURRENT-WORD-ADDR !

\ ============================================================================
\ TOKEN NAVIGATION
\ ============================================================================

\ Get address of token at index
: TOKEN-ADDR ( index -- addr )
  TOKEN-SIZE * TOKEN-BUFFER +
;

\ Get current token's address
: CURRENT-TOKEN ( -- addr )
  TOKEN-POS @ TOKEN-ADDR
;

\ Peek at current token type without consuming
: PEEK-TOKEN-TYPE ( -- type )
  CURRENT-TOKEN @
;

\ Get current token data
: GET-TOKEN-DATA ( -- data )
  CURRENT-TOKEN 8 + @
;

\ Get current token length
: GET-TOKEN-LENGTH ( -- len )
  CURRENT-TOKEN 16 + @
;

\ Advance to next token
: ADVANCE-TOKEN ( -- )
  1 TOKEN-POS +!
;

\ Check if current token matches expected type
: CHECK-TOKEN ( expected-type -- flag )
  PEEK-TOKEN-TYPE =
;

\ Check if current word token matches a given string
: WORD-IS? ( str-addr str-len -- flag )
  GET-TOKEN-LENGTH OVER <> IF 2DROP 0 EXIT THEN
  GET-TOKEN-DATA GET-TOKEN-LENGTH
  COMPARE 0=
;

\ Expect a specific token type, abort if not found
: EXPECT-TOKEN ( expected-type -- )
  CHECK-TOKEN 0= IF
    ." Parse error at token " TOKEN-POS @ . CR
    ." Expected token type " DUP .
    ." but got " PEEK-TOKEN-TYPE . CR
    ABORT
  THEN
;

\ ============================================================================
\ BYTECODE EMISSION
\ ============================================================================

\ Emit an opcode (8 bytes)
: EMIT-OPCODE ( opcode -- )
  BYTECODE-HERE @ !
  8 BYTECODE-HERE +!
;

\ Emit a data cell (8 bytes)
: EMIT-CELL ( n -- )
  BYTECODE-HERE @ !
  8 BYTECODE-HERE +!
;

\ Emit a complete instruction
: EMIT-INSTRUCTION ( opcode data -- )
  SWAP EMIT-OPCODE
  EMIT-CELL
;

\ Emit a literal number
: EMIT-LITERAL ( n -- )
  OP-LITERAL SWAP EMIT-INSTRUCTION
;

\ Emit a primitive call
: EMIT-PRIMITIVE ( prim-id -- )
  OP-PRIMITIVE SWAP EMIT-INSTRUCTION
;

\ Emit a word call
: EMIT-CALL ( word-addr -- )
  OP-CALL SWAP EMIT-INSTRUCTION
;

\ Emit a return
: EMIT-RETURN ( -- )
  OP-RETURN 0 EMIT-INSTRUCTION
;

\ Emit a branch-if-zero with placeholder offset
: EMIT-BRANCH-IF-ZERO ( offset -- )
  OP-BRANCH-IF-ZERO SWAP EMIT-INSTRUCTION
;

\ Emit an unconditional branch
: EMIT-BRANCH ( offset -- )
  OP-BRANCH SWAP EMIT-INSTRUCTION
;

\ ============================================================================
\ BRANCH STACK (for backpatching)
\ ============================================================================

\ Push address onto branch stack
: >BRANCH ( addr -- )
  BRANCH-SP @ !
  8 BRANCH-SP +!
;

\ Pop address from branch stack
: BRANCH> ( -- addr )
  -8 BRANCH-SP +!
  BRANCH-SP @ @
;

\ Peek at top of branch stack without popping
: BRANCH@ ( -- addr )
  BRANCH-SP @ 8 - @
;

\ ============================================================================
\ DICTIONARY MANAGEMENT
\ ============================================================================

\ Dictionary entry structure (32 bytes):
\   +0: Link to previous entry (8 bytes)
\   +8: Name address (8 bytes)
\  +16: Name length (8 bytes)
\  +24: Code address (8 bytes)

\ Create a new dictionary entry
: CREATE-WORD ( name-addr name-len code-addr -- )
  DICT-HERE @ >R        \ Save dictionary pointer
  LATEST @ R@ !         \ Store link to previous word
  8 R@ + !              \ Store name address (at +8)
  SWAP 16 R@ + !        \ Store name length (at +16)
  24 R@ + !             \ Store code address (at +24)
  R@ LATEST !           \ Update LATEST
  32 DICT-HERE +!       \ Advance dictionary pointer
;

\ Find a word in the dictionary
: FIND-WORD ( name-addr name-len -- code-addr | 0 )
  LATEST @
  BEGIN DUP WHILE
    >R                  \ Save current entry address
    2DUP                \ Copy name addr/len
    R@ 8 + @            \ Get entry's name address
    R@ 16 + @           \ Get entry's name length
    COMPARE 0= IF
      2DROP R@ 24 + @ R> DROP EXIT  \ Found it - return code address
    THEN
    R> @                \ Follow link to next entry
  REPEAT
  2DROP 0               \ Not found
;

\ ============================================================================
\ PRIMITIVE LOOKUP
\ ============================================================================

\ Primitive IDs must match the order in src/primitives.rs Primitive enum
\ Starting from 0

\ Helper to compare string with primitive name
: PRIM-MATCH? ( name-addr name-len prim-name-addr prim-name-len -- name-addr name-len flag )
  2OVER 2SWAP COMPARE 0=
;

\ Map word names to primitive IDs (matches primitives.rs order)
: LOOKUP-PRIMITIVE ( name-addr name-len -- prim-id | -1 )
  \ Memory operations (0-14)
  S" @" PRIM-MATCH? IF 2DROP 0 EXIT THEN
  S" !" PRIM-MATCH? IF 2DROP 1 EXIT THEN
  S" C@" PRIM-MATCH? IF 2DROP 2 EXIT THEN
  S" C!" PRIM-MATCH? IF 2DROP 3 EXIT THEN
  S" HERE" PRIM-MATCH? IF 2DROP 4 EXIT THEN
  S" CHAR+" PRIM-MATCH? IF 2DROP 5 EXIT THEN
  S" CHARS" PRIM-MATCH? IF 2DROP 6 EXIT THEN
  S" BASE" PRIM-MATCH? IF 2DROP 7 EXIT THEN
  S" >IN" PRIM-MATCH? IF 2DROP 8 EXIT THEN
  S" SOURCE" PRIM-MATCH? IF 2DROP 9 EXIT THEN
  S" WORD" PRIM-MATCH? IF 2DROP 10 EXIT THEN
  S" PARSE" PRIM-MATCH? IF 2DROP 11 EXIT THEN
  S" COMPARE" PRIM-MATCH? IF 2DROP 12 EXIT THEN
  S" >NUMBER" PRIM-MATCH? IF 2DROP 13 EXIT THEN
  S" S>D" PRIM-MATCH? IF 2DROP 14 EXIT THEN

  \ Stack operations (15-19)
  S" DUP" PRIM-MATCH? IF 2DROP 15 EXIT THEN
  S" DROP" PRIM-MATCH? IF 2DROP 16 EXIT THEN
  S" SWAP" PRIM-MATCH? IF 2DROP 17 EXIT THEN
  S" OVER" PRIM-MATCH? IF 2DROP 18 EXIT THEN
  S" ROT" PRIM-MATCH? IF 2DROP 19 EXIT THEN

  \ Return stack (20-22)
  S" >R" PRIM-MATCH? IF 2DROP 20 EXIT THEN
  S" R>" PRIM-MATCH? IF 2DROP 21 EXIT THEN
  S" R@" PRIM-MATCH? IF 2DROP 22 EXIT THEN

  \ Arithmetic (23-27)
  S" +" PRIM-MATCH? IF 2DROP 23 EXIT THEN
  S" -" PRIM-MATCH? IF 2DROP 24 EXIT THEN
  S" *" PRIM-MATCH? IF 2DROP 25 EXIT THEN
  S" /" PRIM-MATCH? IF 2DROP 26 EXIT THEN
  S" MOD" PRIM-MATCH? IF 2DROP 27 EXIT THEN

  \ Comparison (28-30)
  S" =" PRIM-MATCH? IF 2DROP 28 EXIT THEN
  S" <" PRIM-MATCH? IF 2DROP 29 EXIT THEN
  S" >" PRIM-MATCH? IF 2DROP 30 EXIT THEN

  \ Logical (31-34)
  S" AND" PRIM-MATCH? IF 2DROP 31 EXIT THEN
  S" OR" PRIM-MATCH? IF 2DROP 32 EXIT THEN
  S" XOR" PRIM-MATCH? IF 2DROP 33 EXIT THEN
  S" INVERT" PRIM-MATCH? IF 2DROP 34 EXIT THEN

  \ I/O (35-39)
  S" EMIT" PRIM-MATCH? IF 2DROP 35 EXIT THEN
  S" KEY" PRIM-MATCH? IF 2DROP 36 EXIT THEN
  S" ." PRIM-MATCH? IF 2DROP 37 EXIT THEN
  S" CR" PRIM-MATCH? IF 2DROP 38 EXIT THEN
  S" TYPE" PRIM-MATCH? IF 2DROP 39 EXIT THEN

  \ Other (40)
  S" DEPTH" PRIM-MATCH? IF 2DROP 40 EXIT THEN
  S" I" PRIM-MATCH? IF 2DROP 41 EXIT THEN

  \ Not a primitive
  2DROP -1
;

\ ============================================================================
\ EXPRESSION PARSING
\ ============================================================================

\ Parse a number token
: PARSE-NUMBER ( -- )
  GET-TOKEN-DATA EMIT-LITERAL
  ADVANCE-TOKEN
;

\ Parse a word call token
: PARSE-WORD-CALL ( -- )
  GET-TOKEN-DATA GET-TOKEN-LENGTH

  \ First check if it's a primitive
  2DUP LOOKUP-PRIMITIVE
  DUP -1 <> IF
    NIP NIP EMIT-PRIMITIVE
    ADVANCE-TOKEN
    EXIT
  THEN
  DROP

  \ Check if it's a user-defined word
  FIND-WORD
  DUP 0= IF
    ." Parse error: Undefined word at token " TOKEN-POS @ . CR
    ." Word: " GET-TOKEN-DATA GET-TOKEN-LENGTH TYPE CR
    ABORT
  THEN

  EMIT-CALL
  ADVANCE-TOKEN
;

\ Parse a string literal
: PARSE-STRING ( -- )
  GET-TOKEN-DATA GET-TOKEN-LENGTH
  OP-ALLOC-STRING SWAP EMIT-INSTRUCTION
  ADVANCE-TOKEN
;

\ Forward declaration for recursive parsing
: PARSE-EXPRESSION ( -- ) ;

\ Parse IF ... THEN
: PARSE-IF ( -- )
  ADVANCE-TOKEN                    \ Consume IF

  \ Parse condition (already on stack from previous expression)

  \ Emit branch-if-zero with placeholder
  OP-BRANCH-IF-ZERO EMIT-OPCODE
  BYTECODE-HERE @                  \ Save address to backpatch
  0 EMIT-CELL                      \ Placeholder offset
  >BRANCH                          \ Push on branch stack

  \ Parse THEN branch
  BEGIN
    S" THEN" WORD-IS? S" ELSE" WORD-IS? OR 0=
  WHILE
    PARSE-EXPRESSION
  REPEAT

  \ Check if ELSE
  S" ELSE" WORD-IS? IF
    ADVANCE-TOKEN                  \ Consume ELSE

    \ Emit unconditional branch to skip ELSE part
    OP-BRANCH EMIT-OPCODE
    BYTECODE-HERE @
    0 EMIT-CELL
    >BRANCH

    \ Backpatch the IF branch to here
    BRANCH>
    BYTECODE-HERE @ OVER - 8 /     \ Calculate offset
    SWAP !

    \ Parse ELSE branch
    BEGIN
      S" THEN" WORD-IS? 0=
    WHILE
      PARSE-EXPRESSION
    REPEAT

    \ Backpatch the ELSE skip branch
    BRANCH>
    BYTECODE-HERE @ OVER - 8 /
    SWAP !
  ELSE
    \ No ELSE - just backpatch IF branch
    BRANCH>
    BYTECODE-HERE @ OVER - 8 /
    SWAP !
  THEN

  ADVANCE-TOKEN                    \ Consume THEN
;

\ Parse BEGIN ... (check for UNTIL or WHILE)
: PARSE-BEGIN ( -- )
  ADVANCE-TOKEN                    \ Consume BEGIN
  BYTECODE-HERE @ >BRANCH          \ Save loop start address

  \ Parse body until we hit UNTIL or WHILE
  BEGIN
    S" UNTIL" WORD-IS? S" WHILE" WORD-IS? OR 0=
  WHILE
    PARSE-EXPRESSION
  REPEAT

  \ Check which keyword we found
  S" UNTIL" WORD-IS? IF
    \ BEGIN ... UNTIL
    ADVANCE-TOKEN                  \ Consume UNTIL

    \ Emit branch-if-zero back to loop start
    BRANCH>
    BYTECODE-HERE @ - 8 /          \ Calculate negative offset
    EMIT-BRANCH-IF-ZERO
  ELSE
    \ BEGIN ... WHILE ... REPEAT
    ADVANCE-TOKEN                  \ Consume WHILE

    \ Emit branch-if-zero with placeholder (exit loop if false)
    OP-BRANCH-IF-ZERO EMIT-OPCODE
    BYTECODE-HERE @
    0 EMIT-CELL
    >BRANCH                        \ Save exit branch address

    \ Parse body up to REPEAT
    BEGIN
      S" REPEAT" WORD-IS? 0=
    WHILE
      PARSE-EXPRESSION
    REPEAT

    ADVANCE-TOKEN                  \ Consume REPEAT

    \ Emit unconditional branch back to loop start
    BRANCH>                        \ Get exit branch address
    BRANCH>                        \ Get loop start
    BYTECODE-HERE @ - 8 /          \ Calculate negative offset
    EMIT-BRANCH

    \ Backpatch the WHILE exit branch
    BYTECODE-HERE @ OVER - 8 /
    SWAP !
  THEN
;

\ Parse DO ... LOOP or DO ... +LOOP
: PARSE-DO ( -- )
  ADVANCE-TOKEN                    \ Consume DO

  \ Emit DO-SETUP (pushes limit and start onto loop stack)
  OP-DO-SETUP 0 EMIT-INSTRUCTION

  \ Save loop start address
  BYTECODE-HERE @ >BRANCH

  \ Parse loop body
  BEGIN
    S" LOOP" WORD-IS? S" +LOOP" WORD-IS? OR 0=
  WHILE
    PARSE-EXPRESSION
  REPEAT

  \ Check which loop type
  S" LOOP" WORD-IS? IF
    \ Regular LOOP (increment by 1)
    ADVANCE-TOKEN

    \ Emit LOOP instruction with backward branch
    BRANCH>
    BYTECODE-HERE @ - 8 /
    OP-LOOP SWAP EMIT-INSTRUCTION
  ELSE
    \ +LOOP (increment by value on stack)
    ADVANCE-TOKEN

    \ Emit +LOOP instruction with backward branch
    BRANCH>
    BYTECODE-HERE @ - 8 /
    OP-PLUS-LOOP SWAP EMIT-INSTRUCTION
  THEN

  \ Emit LOOP-END (clean up loop stack)
  OP-LOOP-END 0 EMIT-INSTRUCTION
;

\ Parse ?DO ... LOOP or ?DO ... +LOOP
: PARSE-QDO ( -- )
  ADVANCE-TOKEN                    \ Consume ?DO

  \ Emit QDO-SETUP with placeholder skip address
  OP-QDO-SETUP EMIT-OPCODE
  BYTECODE-HERE @
  0 EMIT-CELL
  >BRANCH                          \ Save address to backpatch

  \ Save loop start address
  BYTECODE-HERE @ >BRANCH

  \ Parse loop body
  BEGIN
    S" LOOP" WORD-IS? S" +LOOP" WORD-IS? OR 0=
  WHILE
    PARSE-EXPRESSION
  REPEAT

  \ Check which loop type
  S" LOOP" WORD-IS? IF
    \ Regular LOOP
    ADVANCE-TOKEN

    \ Emit LOOP instruction with backward branch
    BRANCH>
    BYTECODE-HERE @ - 8 /
    OP-LOOP SWAP EMIT-INSTRUCTION
  ELSE
    \ +LOOP
    ADVANCE-TOKEN

    \ Emit +LOOP instruction with backward branch
    BRANCH>
    BYTECODE-HERE @ - 8 /
    OP-PLUS-LOOP SWAP EMIT-INSTRUCTION
  THEN

  \ Emit LOOP-END
  OP-LOOP-END 0 EMIT-INSTRUCTION

  \ Backpatch the ?DO skip address
  BRANCH>
  BYTECODE-HERE @ OVER - 8 /
  SWAP !
;

\ ============================================================================
\ EXPRESSION PARSING
\ ============================================================================

\ Parse an expression (number, word, or control structure)
: PARSE-EXPRESSION ( -- )
  \ ." [EXPR] Token " TOKEN-POS @ . ." type " PEEK-TOKEN-TYPE . CR
  PEEK-TOKEN-TYPE

  \ Number literal
  DUP TOK-NUMBER = IF
    DROP PARSE-NUMBER EXIT
  THEN

  \ String literal
  DUP TOK-STRING = IF
    DROP PARSE-STRING EXIT
  THEN

  \ Word token - could be keyword or word call
  DUP TOK-WORD = IF
    DROP

    \ Check for IF keyword
    S" IF" WORD-IS? IF PARSE-IF EXIT THEN

    \ Check for BEGIN keyword
    S" BEGIN" WORD-IS? IF PARSE-BEGIN EXIT THEN

    \ Check for DO keyword
    S" DO" WORD-IS? IF PARSE-DO EXIT THEN

    \ Check for ?DO keyword
    S" ?DO" WORD-IS? IF PARSE-QDO EXIT THEN

    \ Check for EXIT keyword (early return)
    S" EXIT" WORD-IS? IF
      ADVANCE-TOKEN
      EMIT-RETURN
      EXIT
    THEN

    \ Check for RECURSE keyword
    S" RECURSE" WORD-IS? IF
      ADVANCE-TOKEN
      CURRENT-WORD-ADDR @ DUP 0= IF
        ." RECURSE outside of word definition" CR ABORT
      THEN
      EMIT-CALL
      EXIT
    THEN

    \ Regular word call
    PARSE-WORD-CALL EXIT
  THEN

  \ Semicolon or EOF - end of expression
  DUP TOK-SEMICOLON = OVER TOK-EOF = OR IF
    DROP EXIT
  THEN

  \ Unknown token
  DROP
  ." Parse error: Unexpected token at position " TOKEN-POS @ . CR
  ." Token type: " PEEK-TOKEN-TYPE . CR
  ." Expected: number, word, string, or control structure" CR
  ABORT
;

\ ============================================================================
\ DEFINITION PARSING
\ ============================================================================

\ Parse a colon definition: : NAME ... ;
: PARSE-WORD-DEF ( -- )
  ADVANCE-TOKEN                    \ Consume :

  TOK-WORD EXPECT-TOKEN
  GET-TOKEN-DATA GET-TOKEN-LENGTH  \ Get word name
  2DUP                             \ Save for dictionary entry

  \ Save current bytecode position as word's code address
  BYTECODE-HERE @
  DUP CURRENT-WORD-ADDR !          \ Save for RECURSE

  ADVANCE-TOKEN                    \ Consume name

  \ Parse word body until ;
  BEGIN
    PEEK-TOKEN-TYPE TOK-SEMICOLON <>
  WHILE
    PARSE-EXPRESSION
  REPEAT

  \ Emit return
  EMIT-RETURN

  ADVANCE-TOKEN                    \ Consume ;

  \ Create dictionary entry
  CREATE-WORD

  \ Clear current word address
  0 CURRENT-WORD-ADDR !
;

\ Parse a variable definition: VARIABLE NAME
: PARSE-VARIABLE ( -- )
  ADVANCE-TOKEN                    \ Consume VARIABLE

  TOK-WORD EXPECT-TOKEN
  GET-TOKEN-DATA GET-TOKEN-LENGTH
  2DUP                             \ Save name for dictionary

  \ Allocate space in memory for the variable
  DICT-HERE @                      \ Variable's address
  SWAP >R >R                       \ Save name on return stack
  8 DICT-HERE +!                   \ Allocate 8 bytes
  0 OVER !                         \ Initialize to 0

  \ Generate code that pushes the variable address
  BYTECODE-HERE @                  \ Code address for this word
  OP-VARIABLE-ADDR OVER EMIT-INSTRUCTION
  EMIT-RETURN

  \ Create dictionary entry
  R> R> ROT                        \ name-addr name-len code-addr
  CREATE-WORD

  ADVANCE-TOKEN                    \ Consume name
;

\ Parse a constant definition: CONSTANT NAME ( value is on stack before calling)
: PARSE-CONSTANT ( -- )
  ADVANCE-TOKEN                    \ Consume CONSTANT

  TOK-WORD EXPECT-TOKEN
  GET-TOKEN-DATA GET-TOKEN-LENGTH
  2DUP                             \ Save name for dictionary

  \ Generate code that pushes the constant value
  BYTECODE-HERE @                  \ Code address for this word

  \ The value should be on stack, but we're parsing tokens
  \ We need to get the value from the previous expression
  \ For now, emit code to push a placeholder (0)
  \ TODO: This needs to be fixed - constants need the value
  0 EMIT-LITERAL
  EMIT-RETURN

  \ Create dictionary entry
  CREATE-WORD

  ADVANCE-TOKEN                    \ Consume name
;

\ ============================================================================
\ PROGRAM PARSING
\ ============================================================================

\ Parse a complete program (sequence of definitions)
: PARSE-PROGRAM ( -- )
  0 TOKEN-POS !                    \ Reset token position

  BEGIN
    PEEK-TOKEN-TYPE TOK-EOF <>
  WHILE
    PEEK-TOKEN-TYPE

    \ Colon definition
    DUP TOK-COLON = IF
      DROP PARSE-WORD-DEF
    ELSE
    \ Check for VARIABLE/CONSTANT keywords (they're TOK-WORD in lexer)
    DUP TOK-WORD = IF
      DROP
      S" VARIABLE" WORD-IS? IF
        PARSE-VARIABLE
      ELSE S" CONSTANT" WORD-IS? IF
        PARSE-CONSTANT
      ELSE
        \ Top-level expression
        PARSE-EXPRESSION
      THEN THEN
    ELSE
    \ Other top-level expression
      DROP PARSE-EXPRESSION
    THEN THEN
  REPEAT
;

\ ============================================================================
\ DEBUG / UTILITY
\ ============================================================================

\ Print current parser state
: .PARSER-STATE ( -- )
  ." Token position: " TOKEN-POS @ . CR
  ." Current token type: " PEEK-TOKEN-TYPE . CR
  ." Bytecode position: " BYTECODE-HERE @ . CR
  ." Dictionary position: " DICT-HERE @ . CR
;

\ Reset parser state
: RESET-PARSER ( -- )
  0 TOKEN-POS !
  45000 BYTECODE-HERE !
  55000 BRANCH-SP !
  65000 DICT-HERE !
  0 LATEST !
  0 CURRENT-WORD-ADDR !
;
