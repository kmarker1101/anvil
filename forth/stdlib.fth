: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: 2DROP DROP DROP ;
: NEGATE -1 * ;
: ABS ( n -- +n ) DUP 0 < IF NEGATE THEN ;
: OVER ( x1 x2 -- x1 x2 x1 ) >R DUP R> SWAP ;

\ Memory allocation helpers
\ These are helpers for working with variables and constants
\ Note: Full VARIABLE and CONSTANT require CREATE/DOES> which we haven't implemented yet
\ Users can define variables like: : MY-VAR HERE 0 , ; MY-VAR @
\ And constants like: : MY-CONST 42 ;

: CELLS ( n -- n*8 ) 8 * ;     \ Convert cell count to byte count
: CELL+ ( addr -- addr+8 ) 8 + ; \ Add one cell to address
: +! ( n addr -- ) DUP @ ROT + SWAP ! ; \ Add to memory location
