: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: 2DROP DROP DROP ;
: NEGATE -1 * ;
: ABS ( n -- +n ) DUP 0 < IF NEGATE THEN ;
: OVER ( x1 x2 -- x1 x2 x1 ) >R DUP R> SWAP ;
