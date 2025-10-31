\ ============================================================================
\ Stack Manipulation Words
\ ============================================================================
\ Basic stack operations with no dependencies on other stdlib words

: 2DROP ( x1 x2 -- )
    DROP DROP ;

: 2DUP ( x1 x2 -- x1 x2 x1 x2 )
    OVER OVER ;

: NIP ( x1 x2 -- x2 )
    SWAP DROP ;

: OVER ( x1 x2 -- x1 x2 x1 )
    >R DUP R> SWAP ;

: TUCK ( x1 x2 -- x2 x1 x2 )
    SWAP OVER ;
