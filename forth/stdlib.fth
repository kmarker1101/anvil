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

\ ============================================================================
\ Interpreter Infrastructure (Issue #9)
\ ============================================================================

\ STATE variable: Interpreter state management
\ STATE-VAR is the address of the STATE variable
\ STATE = 0 means interpreting, STATE = -1 means compiling
VARIABLE STATE-VAR

\ Convenience word to access STATE value
: STATE ( -- addr ) STATE-VAR ;

\ INTERPRET-WORD: interpret or compile a single word
\ Stack effect: ( c-addr u -- )
\ Behavior depends on STATE:
\   - If interpreting (STATE=0): Execute the word or push the number
\   - If compiling (STATE=-1): Compile the word (unless IMMEDIATE) or literal
: INTERPRET-WORD ( c-addr u -- )
    2DUP FIND           \ ( c-addr u c-addr u xt flag )
    IF                  \ Found in dictionary
        NIP NIP         \ ( xt ) - drop address and length
        \ TODO: Check IMMEDIATE flag here
        \ For now, always execute (interpret mode behavior)
        EXECUTE
    ELSE                \ Not found, try as number
        DROP            \ ( c-addr u ) - drop 0 xt from FIND
        2DUP            \ ( c-addr u c-addr u ) - duplicate for NUMBER
        NUMBER          \ ( c-addr u n flag )
        IF              \ Valid number
            \ Number successfully parsed
            NIP NIP     \ ( n ) - drop saved c-addr u, keep number
            \ TODO: In compile mode, should compile as LIT
            \ For now, just leave on stack (works for interpret mode)
        ELSE
            \ Not a word and not a number - error
            DROP        \ ( c-addr u ) - drop the 0 flag from NUMBER
            \ Print error message: word_name followed by " ?"
            TYPE        \ Print the word name
            ."  ?" CR   \ Print " ?" and newline
        THEN
    THEN
;

\ INTERPRET-LINE: Process one line of input
\ Parses and interprets all words in the current input buffer
: INTERPRET-LINE ( -- )
    BEGIN
        BL WORD         \ Parse next word delimited by space
        DUP C@          \ Get length of counted string
    WHILE               \ While word is not empty
        COUNT           \ Convert counted string to ( addr len )
        INTERPRET-WORD  \ Interpret or compile it
    REPEAT
    DROP                \ Drop empty word address
;

\ QUIT: Main interpreter loop
\ Reads lines, interprets them, and prints "ok" prompts
: QUIT ( -- )
    0 STATE-VAR !       \ Start in interpret mode
    BEGIN
        REFILL          \ Read new line of input
    WHILE               \ If got input (REFILL returns true)
        INTERPRET-LINE  \ Process the line
        STATE-VAR @ 0= IF    \ If in interpret mode
            ."  ok" CR       \ Print ok prompt
        THEN
    REPEAT
;
