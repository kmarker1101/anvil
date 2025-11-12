\ metacompiler.fth - Control structures defined in Forth using primitives
\ This defines IF/THEN/ELSE, BEGIN/UNTIL, etc. using compilation primitives

\ ============================================================================
\ SYSTEM VARIABLE ACCESSORS
\ ============================================================================
\ These match the addresses in primitives.rs

#288 CONSTANT STATE-ADDR      \ 0x120: Compilation state
#272 CONSTANT LATEST-ADDR     \ 0x110: Pointer to last word
#280 CONSTANT DP-ADDR         \ 0x118: Data pointer

: STATE STATE-ADDR ;
: LATEST LATEST-ADDR ;
: DP DP-ADDR ;

\ ============================================================================
\ COMPILATION MODE CONTROL
\ ============================================================================

: ]  \ Enter compile mode
    1 STATE !
;

: [  \ Exit compile mode (IMMEDIATE)
    0 STATE !
; IMMEDIATE

\ ============================================================================
\ CONTROL STRUCTURES
\ ============================================================================
\ These use the COMPILE-* primitives to build control flow

: IF ( -- addr )
    \ Compile conditional branch, leave address for backpatching
    DP @                   \ Save address for backpatching
    0 COMPILE-BRANCH0      \ Compile BRANCH0 with placeholder target
; IMMEDIATE

: THEN ( addr -- )
    \ Backpatch IF - patch the branch target address
    DP @                   \ Get current address (target)
    SWAP 1 +               \ Get address of the 8-byte target field (after opcode)
    !                      \ Store target address
; IMMEDIATE

: ELSE ( addr1 -- addr2 )
    \ Compile unconditional branch and backpatch IF
    DP @                   \ Save address for ELSE backpatch
    0 COMPILE-JUMP         \ Compile unconditional jump with placeholder
    SWAP                   \ Get IF address
    DP @ SWAP 1 + !        \ Backpatch IF to jump here
; IMMEDIATE

: BEGIN ( -- addr )
    \ Mark loop start
    DP @                   \ Save address of loop start
; IMMEDIATE

: UNTIL ( addr -- )
    \ Compile loop with conditional exit (branch back if TOS is zero)
    COMPILE-BRANCH0        \ Compile BRANCH0 back to BEGIN
; IMMEDIATE

: WHILE ( addr1 -- addr1 addr2 )
    \ Compile conditional exit from loop
    IF                     \ Compile conditional branch
    SWAP                   \ Keep BEGIN addr on top
; IMMEDIATE

: REPEAT ( addr1 addr2 -- )
    \ Compile unconditional jump to BEGIN and backpatch WHILE
    SWAP                   \ Get BEGIN address
    COMPILE-JUMP           \ Jump back to BEGIN
    DP @ SWAP 1 + !        \ Backpatch WHILE to jump here
; IMMEDIATE

\ ============================================================================
\ LITERAL COMPILATION
\ ============================================================================

: LITERAL ( n -- )
    \ Compile a literal value
    COMPILE-LIT
; IMMEDIATE

\ ============================================================================
\ OTHER COMPILATION WORDS
\ ============================================================================

: EXIT ( -- )
    \ Compile early return from word
    COMPILE-RETURN
; IMMEDIATE

: RECURSE ( -- )
    \ Compile a call to the word currently being defined
    \ LATEST points to the current word, get its XT and compile a call
    LATEST @               \ ( -- word-addr )
    16 +                   \ ( word-addr -- name-len-addr )  Skip LINK+FLAGS
    DUP C@                 \ ( name-len-addr -- name-len-addr len )
    1 + +                  \ ( name-len-addr len -- addr-after-name )
    ALIGNED                \ ( addr-after-name -- aligned-addr )
    @                      \ ( aligned-addr -- xt ) Fetch CODE-ADDR
    COMPILE-CALL           \ ( xt -- ) Compile a call to it
; IMMEDIATE

\ ============================================================================
\ LOOP STRUCTURES
\ ============================================================================

: DO ( -- addr )
    \ Compile DO loop start
    COMPILE-DO-SETUP       \ Compile DO setup bytecode
    DP @                   \ Save address for LOOP to jump back to
; IMMEDIATE

: LOOP ( addr -- )
    \ Compile LOOP - increment and check, jump back if not done
    COMPILE-LOOP-CHECK     \ Compile LOOP check with backjump address
    COMPILE-LOOP-END       \ Compile LOOP end (cleanup)
; IMMEDIATE

\ Now control structures are available!
\ IMMEDIATE is a primitive

