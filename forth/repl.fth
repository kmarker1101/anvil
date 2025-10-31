\ ============================================================================
\ REPL Infrastructure (Issue #9)
\ ============================================================================
\ This file contains the REPL (QUIT loop) and interpreter infrastructure.
\ It is loaded only in interactive/JIT modes, NOT in AOT compilation.

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
            ABORT       \ Clear stacks and recover
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
