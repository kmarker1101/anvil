\ ============================================================================
\ Stack Manipulation Words
\ ============================================================================
\ Basic stack operations with no dependencies on other stdlib words

: 2DROP ( x1 x2 -- )
    DROP DROP ;

: 2DUP ( x1 x2 -- x1 x2 x1 x2 )
    OVER OVER ;

: ?DUP ( x1 -- x1 x1 | 0 )
    DUP IF DUP THEN ;

: NIP ( x1 x2 -- x2 )
    SWAP DROP ;

: OVER ( x1 x2 -- x1 x2 x1 )
    >R DUP R> SWAP ;

: ROT ( x1 x2 x3 -- x2 x3 x1 )
    >R SWAP R> SWAP ;

: -ROT ( x1 x2 x3 -- x3 x1 x2 )
    ROT ROT ;

: 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
    >R >R 2DUP R> ROT ROT R> ROT ROT ;

: 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
    >R ROT ROT R> ROT ROT ;

: TUCK ( x1 x2 -- x2 x1 x2 )
    SWAP OVER ;
