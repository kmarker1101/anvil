: NIP ( a b -- b ) SWAP DROP ;
: \ SOURCE NIP >IN ! ; IMMEDIATE

\ stdlib.fth - Forth Standard Library
\ These are words built from primitives

\ ============================================================================
\ CONSTANTS
\ ============================================================================

#32 CONSTANT BL

\ ============================================================================
\ VARIABLES
\ ============================================================================

\ BASE is a primitive, initialize it here
#10 BASE !

\ ============================================================================
\ SYSTEM
\ ============================================================================

\ BYE exits - handled specially by REPL, no-op when defined here
: BYE ;

\ ============================================================================
\ I/O
\ ============================================================================

: CR ( -- ) 10 EMIT ;

\ ============================================================================
\ STACK OPERATIONS
\ ============================================================================

: 2DUP ( a b -- a b a b ) OVER OVER ;

: 2DROP ( a b -- ) DROP DROP ;

\ : NIP ( a b -- b ) SWAP DROP ;

: TUCK ( a b -- b a b ) SWAP OVER ;

: 3DROP ( x1 x2 x3 -- )
  DROP DROP DROP ;

: ?DUP ( x1 -- x1 x1 | 0 )
  DUP IF DUP THEN ;

: -ROT ( x1 x2 x3 -- x3 x1 x2 )
  ROT ROT ;

: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
  >R >R OVER OVER R> ROT ROT R> ROT ROT ;

: 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
  >R ROT ROT R> ROT ROT ;

: 3DUP ( x1 x2 x3 -- x1 x2 x3 x1 x2 x3 )
  DUP 2OVER ROT ;

: PICK ( x0 i*x u.i -- x0 i*x x0 )
  DUP 0 = if DROP DUP exit then  SWAP >R 1 - RECURSE R> SWAP ;

\ ============================================================================
\ ARITHMETIC
\ ============================================================================

: NEGATE ( n -- -n ) 0 SWAP - ;
: 1+ ( n -- n+1 ) 1 + ;
: 1- ( n -- n-1 ) 1 - ;
: 2* ( n -- n*2 ) DUP + ;
: 2/ ( n -- n/2 ) 2 / ;
: */ ( n1 n2 n3 -- n4 ) >R * R> / ;

: ABS ( n -- |n| ) 
  DUP 0 < IF 0 SWAP - THEN ;

: MIN ( a b -- min ) 
  2DUP < IF DROP ELSE SWAP DROP THEN ;

: MAX ( a b -- max ) 
  2DUP > IF DROP ELSE SWAP DROP THEN ;

\ ============================================================================
\ COMPARISON
\ ============================================================================

: 0= ( n -- flag ) 0 = ;
: 0< ( n -- flag ) 0 < ;
: 0> ( n -- flag ) 0 > ;
: 0<> ( n -- flag ) 0= 0= ;
: <> ( x1 x2 -- flag ) = 0= ;
: >= ( n1 n2 -- flag ) < 0= ;
: <= ( n1 n2 -- flag ) > 0= ;

\ ============================================================================
\ BOOLEAN
\ ============================================================================

: TRUE ( -- -1 ) -1 ;
: FALSE ( -- 0 ) 0 ;
: NOT    ( flag -- ~flag ) 0= ;

\ ============================================================================
\ MEMORY HELPERS
\ ============================================================================

: CHAR+ ( addr -- addr+1 ) 1+ ;
: +! ( n addr -- ) DUP @ ROT + SWAP ! ;
: FILL ( c-char u char -- ) ROT ROT 0 ?DO 2DUP C! CHAR+ LOOP 2DROP ;

\ Cell-based memory operations
: CELLS ( n1 -- n2 ) 8 * ;        \ Size in bytes of n1 cells (64-bit cells)
: CELL+ ( addr -- addr' ) 8 + ;   \ Add size of one cell to address
\ ALLOT is now a primitive
\ : ALLOT ( n -- ) HERE @ + HERE ! ;  \ Allocate n bytes

\ CREATE needs to be a primitive in the bytecode compiler
\ because it needs to create new dictionary entries dynamically.
\ For now, we can use a workaround for simple cases:
\ Instead of: CREATE FOO 32 CELLS ALLOT
\ You can do: VARIABLE FOO ( and manually manage the array )

\ ============================================================================
\ STRINGS
\ ============================================================================

: COUNT ( c-addr -- addr len ) DUP 1+ SWAP C@ ;
: _cmp ( x1 x2 -- -1|0|1 ) - DUP IF 0< IF -1 ELSE 1 THEN THEN ;
: COMPARE ( addr1 u1 addr2 u2 -- -1|0|1 )
  ROT 2DUP SWAP _cmp >R
  MIN ?DUP IF
    0 DO
      COUNT >R SWAP COUNT R> _cmp ?DUP IF
        NIP NIP UNLOOP R> DROP EXIT
      THEN
      SWAP
    LOOP
  THEN
  2DROP
  R>
;
: S=  ( addr1 len1 addr2 len2 -- flag ) COMPARE 0= ;

\ ============================================================================
\ NUMERIC BASE
\ ============================================================================

: DECIMAL ( -- ) #10 BASE ! ;
: HEX ( -- ) $10 BASE ! ;

\ ============================================================================
\ CHARS
\ ============================================================================

\ CHAR is implemented as an IMMEDIATE word in the compiler
: SPACE ( -- ) BL EMIT ;
: SPACES ( n -- ) 0 ?DO SPACE LOOP ;

\ ============================================================================
\ USEFUL EXAMPLES
\ ============================================================================

\ : SQUARE ( n -- n^2 ) DUP * ;
\ : CUBE ( n -- n^3 ) DUP SQUARE * ;

\ Note: FACTORIAL would need proper recursion support
\ : FACTORIAL ( n -- n! )
\   DUP 1 <= IF DROP 1 ELSE DUP 1 - FACTORIAL * THEN ;

\ UNLESS - inverted IF
: UNLESS  ( flag -- )
    0= IF
; IMMEDIATE

: ENDUNLESS
    THEN
; IMMEDIATE

\ ============================================================================
\ ENVIRONMENT QUERIES
\ ============================================================================

\ ENVIRONMENT? - Query system capabilities
\ For now, return FALSE for all queries (we don't support floating point yet)
: ENVIRONMENT? ( c-addr u -- false | i*x true )
    2DROP FALSE ;

\ .( - Print message (similar to ." but always immediate)
\ This is handled by the compiler as a special word

\ INCLUDED - Load a file given address and length
\ This needs to be handled by the compiler/REPL since we need to actually load files
\ It's the same as INCLUDE but takes addr/len instead of parsing

