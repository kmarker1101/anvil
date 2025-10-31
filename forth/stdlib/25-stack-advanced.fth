\ ============================================================================
\ Advanced Stack Manipulation
\ ============================================================================
\ Depends on: 20-comparison.fth (for 0=), 10-arithmetic.fth (for 1-)

: PICK ( xu ... x1 x0 u -- xu ... x1 x0 xu )
    DUP 0= IF
        DROP DUP
    ELSE
        SWAP >R 1- RECURSE R> SWAP
    THEN ;
