\ meta-compiler.fth
\ Forth meta-compiler that handles IMMEDIATE words
\ This layer sits between the lexer and the bytecode generator

\ ============================================================================
\ COMPILATION OUTPUT BUFFER
\ ============================================================================

VARIABLE CODE-HERE
40000 CODE-HERE !

\ ============================================================================
\ BYTECODE OPCODES
\ ============================================================================

0 CONSTANT OP-LITERAL
1 CONSTANT OP-CALL
2 CONSTANT OP-BRANCH-IF-ZERO
3 CONSTANT OP-BRANCH
4 CONSTANT OP-RETURN
5 CONSTANT OP-PRIMITIVE

\ ============================================================================
\ BYTECODE EMISSION
\ ============================================================================

: EMIT-OP ( opcode -- )
  CODE-HERE @ !
  8 CODE-HERE +!
;

: EMIT-CELL ( n -- )
  CODE-HERE @ !
  8 CODE-HERE +!
;

: EMIT-LITERAL ( n -- )
  OP-LITERAL EMIT-OP
  EMIT-CELL
;

: EMIT-CALL ( xt -- )
  OP-CALL EMIT-OP
  EMIT-CELL
;

: EMIT-RETURN ( -- )
  OP-RETURN EMIT-OP
;

\ ============================================================================
\ BRANCH STACK FOR CONTROL FLOW
\ ============================================================================

VARIABLE BRANCH-SP
50000 BRANCH-SP !

: >BRANCH ( addr -- )
  BRANCH-SP @ !
  8 BRANCH-SP +!
;

: BRANCH> ( -- addr )
  -8 BRANCH-SP +!
  BRANCH-SP @ @
;

\ ============================================================================
\ TOKEN ACCESS (uses lexer token buffer)
\ ============================================================================

\ Token position tracking
VARIABLE TOKEN-INDEX
0 TOKEN-INDEX !

: ADVANCE-TOKEN ( -- )
  1 TOKEN-INDEX +!
;

: PEEK-TOKEN-TYPE ( -- type )
  TOKEN-INDEX @ TOKEN-TYPE@
;

: GET-TOKEN-DATA ( -- data )
  TOKEN-INDEX @ TOKEN-DATA@
;

: GET-TOKEN-LENGTH ( -- len )
  TOKEN-INDEX @ TOKEN-LENGTH@
;

: GET-NEXT-TOKEN ( -- )
  ADVANCE-TOKEN
;

\ ============================================================================
\ WORD LOOKUP AND IMMEDIATE CHECK
\ ============================================================================

\ Note: These will need to call into Rust to interact with the compiler

\ Check if a word is marked as immediate
\ : IMMEDIATE? ( addr len -- flag )
\   \ TODO: Call Rust syscall to check compiler.is_immediate()
\   2DROP 0  \ Placeholder
\ ;

\ Find a word and return its execution token
\ : FIND-WORD ( addr len -- xt | 0 )
\   \ TODO: Call Rust syscall to look up word in dictionary
\   2DROP 0  \ Placeholder
\ ;

\ Execute an execution token
\ : EXECUTE ( xt -- )
\   \ TODO: Call Rust to execute the word
\   DROP  \ Placeholder
\ ;

\ ============================================================================
\ META-COMPILATION
\ ============================================================================

\ Compile a single token
: META-COMPILE-TOKEN ( -- )
  PEEK-TOKEN-TYPE

  DUP TOK-NUMBER = IF
    DROP
    GET-TOKEN-DATA EMIT-LITERAL
    ADVANCE-TOKEN
  ELSE DUP TOK-WORD = IF
    DROP
    GET-TOKEN-DATA GET-TOKEN-LENGTH  ( addr len )

    \ Check if it's immediate
    \ 2DUP IMMEDIATE? IF
    \   \ Execute it NOW (during compilation)
    \   FIND-WORD EXECUTE
    \ ELSE
    \   \ Compile a call to it
    \   FIND-WORD EMIT-CALL
    \ THEN

    \ For now, just compile all words as calls
    \ TODO: Add IMMEDIATE handling
    2DROP  \ Remove addr len for now
    ADVANCE-TOKEN
  ELSE
    ." Unknown token type: " . CR
    ABORT
  THEN THEN
;

\ Compile a word definition
: META-COMPILE-DEFINITION ( -- )
  \ Expect word name token
  GET-NEXT-TOKEN
  PEEK-TOKEN-TYPE TOK-WORD <> IF
    ." Expected word name after :" CR
    ABORT
  THEN

  \ Get the name
  GET-TOKEN-DATA GET-TOKEN-LENGTH
  ." Compiling: " 2DUP TYPE CR

  \ Reset code buffer
  40000 CODE-HERE !

  \ Compile tokens until ';'
  ADVANCE-TOKEN
  BEGIN
    PEEK-TOKEN-TYPE
    DUP TOK-SEMICOLON = IF
      DROP
      ADVANCE-TOKEN
      EMIT-RETURN
      EXIT
    THEN
    DUP TOK-EOF = IF
      ." Unexpected EOF in definition" CR
      ABORT
    THEN
    DROP

    META-COMPILE-TOKEN
  AGAIN
;

\ Main meta-compiler entry point
: META-COMPILE-PROGRAM ( -- )
  \ Assume tokens are already in TOKEN-BUFFER from lexer
  0 TOKEN-INDEX !

  BEGIN
    PEEK-TOKEN-TYPE TOK-EOF <>
  WHILE
    PEEK-TOKEN-TYPE

    DUP TOK-COLON = IF
      DROP
      META-COMPILE-DEFINITION
    ELSE
      ." Unexpected token at top level: " . CR
      ABORT
    THEN
  REPEAT
;
