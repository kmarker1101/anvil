\ Minimal bootstrap - defines : and ; using primitives only
\ This runs first, then metacompiler.fth can use : and ;

\ First word: TEST that just returns (simplest possible)
S" TEST" CREATE COMPILE-RETURN

\ Now we can test if it works!
