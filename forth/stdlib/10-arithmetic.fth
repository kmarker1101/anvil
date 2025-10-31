\ ============================================================================
\ Arithmetic Operations
\ ============================================================================
\ Depends on: 00-stack.fth (for NIP)

: / ( n1 n2 -- quotient )
    DUP 0 = IF
        DROP DROP 0  \ Division by zero returns 0
    ELSE
        /MOD NIP
    THEN ;

: 1+ ( n -- n+1 )
    1 + ;

: 1- ( n -- n-1 )
    1 - ;

: 2* ( n -- n*2 )
    2 * ;

: 2/ ( n -- n/2 )
    2 / ;

: NEGATE ( n -- -n )
    -1 * ;

: */ ( n1 n2 n3 -- n4 )
    >R * R> / ;

: */MOD ( n1 n2 n3 -- remainder quotient )
    >R * R> /MOD ;

: MOD ( n1 n2 -- remainder )
    DUP 0 = IF
        DROP DROP 0  \ Modulo by zero returns 0
    ELSE
        /MOD DROP
    THEN ;

: ABS ( n -- +n )
    DUP 0 < IF NEGATE THEN ;
