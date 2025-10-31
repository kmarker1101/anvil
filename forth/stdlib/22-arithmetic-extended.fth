\ ============================================================================
\ Extended Arithmetic Operations
\ ============================================================================
\ Depends on: 00-stack.fth (for 2DUP, DROP, NIP, OVER), 20-comparison.fth (for <, >)

: MIN ( n1 n2 -- n )
    2DUP < IF DROP ELSE NIP THEN ;

: MAX ( n1 n2 -- n )
    2DUP > IF DROP ELSE NIP THEN ;

: WITHIN ( n low high -- flag )
    OVER - >R - R> U< ;
