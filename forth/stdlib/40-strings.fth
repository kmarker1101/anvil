\ ============================================================================
\ String Operations
\ ============================================================================
\ Depends on: (BL constant defined in 50-io.fth, but BLANK/ERASE can wait)

: COUNT ( c-addr -- c-addr+1 u )
    DUP 1 + SWAP C@ ;

: ERASE ( addr u -- )
    0 FILL ;

: MOVE ( addr1 addr2 u -- )
    >R 2DUP < IF
        R> CMOVE>
    ELSE
        R> CMOVE
    THEN ;

: S= ( c-addr1 u1 c-addr2 u2 -- flag )
    COMPARE 0= ;
