: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: 0= 0 = ;
: NEGATE -1 * ;
: ABS ( n -- +n ) DUP 0 < IF NEGATE THEN ;
: OVER ( x1 x2 -- x1 x2 x1 ) >R DUP R> SWAP ;

\ Memory allocation helpers
\ These are helpers for working with variables and constants
\ Note: Full VARIABLE and CONSTANT require CREATE/DOES> which we haven't implemented yet
\ Users can define variables like: : MY-VAR HERE 0 , ; MY-VAR @
\ And constants like: : MY-CONST 42 ;

: CELLS ( n -- n*8 ) 8 * ;     \ Convert cell count to byte count
: CELL+ ( addr -- addr+8 ) 8 + ; \ Add one cell to address
: +! ( n addr -- ) DUP @ ROT + SWAP ! ; \ Add to memory location

\ Interpreter state
\ STATE variable: 0 = interpreting, -1 = compiling
VARIABLE STATE-VAR              \ STATE-VAR returns address of the variable

\ Note: [ and ] would toggle STATE, but require IMMEDIATE flag
\ which needs more infrastructure. For now, STATE-VAR is available.

\ Output helpers
CONSTANT BL 32              \ BL is the space character (ASCII 32)
: SPACE ( -- ) BL EMIT ;    \ Output a single space
: SPACES ( n -- )           \ Output n spaces
    BEGIN DUP WHILE
        SPACE 1 -
    REPEAT DROP ;

\ String helpers
: COUNT ( c-addr -- c-addr+1 u )
    DUP 1 + SWAP C@ ;    \ Get address+1 and length byte

: MOVE ( addr1 addr2 u -- )
    >R 2DUP < IF         \ If source < dest, copy backward
        R> CMOVE>        \ Use CMOVE> for overlapping regions
    ELSE
        R> CMOVE         \ Use CMOVE for normal copy
    THEN ;

: S= ( c-addr1 u1 c-addr2 u2 -- flag )
    COMPARE 0= ;         \ Compare returns 0 if equal

: BLANK ( c-addr u -- )
    BL FILL ;            \ Fill with space character

: ERASE ( addr u -- )
    0 FILL ;             \ Fill with zeros

: PAD ( -- c-addr )
    HERE 256 + ;         \ PAD is 256 bytes after HERE

\ Input buffer helpers
: REFILL ( -- flag )
    TIB 1024 ACCEPT     \ Read line into TIB (up to 1024 chars)
    #TIB !              \ Store length in #TIB
    0 >IN !             \ Reset parse position to start
    -1 ;                \ Return true (successfully refilled)

\ WORD - parse word and return counted string
\ Uses data space at HERE as temporary buffer
: WORD ( char -- c-addr )
    PARSE               \ Parse until delimiter ( char -- c-addr u )
    HERE DUP >R         \ Save HERE address ( c-addr u here -- ) ( R: here )
    OVER >R             \ Save length ( c-addr u here -- ) ( R: here len )
    1 + SWAP            \ Point past length byte for copying ( c-addr here+1 u )

    \ Copy loop: ( src-addr dst-addr count -- )
    0 DO                \ For each character
        OVER I + C@     \ Get source char at offset I
        OVER I + C!     \ Store to dest at offset I
    LOOP
    2DROP               \ Drop addresses

    R> R@  C!           \ Store length at counted string address
    R> ;                \ Return counted string address
