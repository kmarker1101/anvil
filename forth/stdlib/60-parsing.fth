\ ============================================================================
\ Parsing Operations
\ ============================================================================
\ Depends on: 20-comparison.fth (for 0=)

: REFILL ( -- flag )
    TIB 1024 ACCEPT
    DUP #TIB !
    0 >IN !
    0= 0= ;

: WORD ( char -- c-addr )
    PARSE
    HERE DUP >R
    OVER >R
    1 + SWAP

    0 DO
        OVER I + C@
        OVER I + C!
    LOOP
    2DROP

    R> R@  C!
    R> ;
