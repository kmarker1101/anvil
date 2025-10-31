\ ============================================================================
\ Input/Output Operations
\ ============================================================================
\ Depends on: 10-arithmetic.fth (for 1-)

CONSTANT BL 32

: SPACE ( -- )
    BL EMIT ;

: SPACES ( n -- )
    BEGIN DUP WHILE
        SPACE 1 -
    REPEAT DROP ;

: BLANK ( c-addr u -- )
    BL FILL ;

: PAD ( -- c-addr )
    HERE 256 + ;
