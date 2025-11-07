\ ============================================================================
\ EXAMPLE TESTS
\ Tests for examples programs
\ ============================================================================

\ Note: This file is meant to be INCLUDEd by run-all.fth

S" math.fth" TEST-FILE

: gcd   ( u1 u2 -- gcd )  
    begin
      dup 0<> while
        2dup < if swap then
        2dup mod rot drop
    repeat
    drop ;

: factorial ( n -- n! )
  DUP 2 < IF DROP 1 EXIT THEN
  DUP 1- RECURSE *
  ;

: fib ( n -- fib )
  0 1 rot 0 do
    over + swap
  loop drop ;

: acker ( m n -- u )
  over 0= if nip 1+ exit then
  swap 1- swap
  dup 0= if 1+ recurse exit then
  1- over 1+ swap recurse recurse ;


TEST
12 18 GCD 6 EXPECT
S" GCD Test" TEST-NAME

TEST
5 FACTORIAL 120 EXPECT
S" Factorial Test" TEST-NAME

TEST
20 FIB 6765 EXPECT
S" FIB Test" TEST-NAME

TEST
3 3 ACKER 61 EXPECT
S" Acker Test" TEST-NAME

