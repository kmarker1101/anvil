\ codegen.fth - Convert bytecode to LLVM IR
\ This is the code generator for the self-hosted Forth compiler

\ Label counter for control flow
VARIABLE LABEL-COUNTER
0 LABEL-COUNTER !

\ Get next unique label ID
: NEXT-LABEL ( -- n )
  LABEL-COUNTER @ DUP 1+ LABEL-COUNTER ! ;

\ Bytecode instruction opcodes (matching Instruction enum in compiler.rs)
0 CONSTANT OP-LITERAL
1 CONSTANT OP-PRIMITIVE
2 CONSTANT OP-CALL
3 CONSTANT OP-BRANCH-IF-ZERO
4 CONSTANT OP-BRANCH
5 CONSTANT OP-RETURN
6 CONSTANT OP-DO-SETUP
7 CONSTANT OP-QDO-SETUP
8 CONSTANT OP-LOOP
9 CONSTANT OP-PLUS-LOOP
10 CONSTANT OP-LOOP-END
11 CONSTANT OP-ALLOC-STRING
12 CONSTANT OP-VARIABLE-ADDR
13 CONSTANT OP-BYE

\ Simple bytecode processing
\ For now, just handle literals and calls
: PROCESS-INSTRUCTION ( bc-addr bc-len -- bc-addr' bc-len' )
  OVER @ ( bc-addr bc-len opcode )

  DUP OP-LITERAL = IF
    DROP
    \ Read literal value from next cell
    OVER 8 + @  ( bc-addr bc-len value )
    LLVM-EMIT-LITERAL
    16 /STRING  ( advance 16 bytes: 8 for opcode + 8 for value )
    EXIT
  THEN

  DUP OP-PRIMITIVE = IF
    DROP
    \ Primitive has: opcode(8) + prim_id(8)
    OVER 8 + @  ( bc-addr bc-len prim-id )
    ." [PRIM=" DUP . ." ]" CR

    \ Map primitive ID to LLVM function name and emit call
    \ IDs from LOOKUP-PRIMITIVE: Add=23, Sub=24, Mul=25, Div=26, Mod=27
    DUP 23 = IF ." [MATCH 23]" CR DROP S" forth_add" LLVM-EMIT-CALL 16 /STRING EXIT THEN
    DUP 24 = IF ." [MATCH 24]" CR DROP S" forth_sub" LLVM-EMIT-CALL 16 /STRING EXIT THEN
    DUP 25 = IF ." [MATCH 25]" CR DROP S" forth_mul" LLVM-EMIT-CALL 16 /STRING EXIT THEN
    DUP 26 = IF ." [MATCH 26]" CR DROP S" forth_div" LLVM-EMIT-CALL 16 /STRING EXIT THEN
    DUP 27 = IF ." [MATCH 27]" CR DROP S" forth_mod" LLVM-EMIT-CALL 16 /STRING EXIT THEN
    ." [NO MATCH]" CR
    DROP

    16 /STRING  ( advance 16 bytes )
    EXIT
  THEN

  DUP OP-CALL = IF
    DROP
    \ Call has: opcode(8) + name_len(8) + name_data(variable)
    8 + OVER @  ( bc-addr bc-len name-len )
    DUP >R      ( save name-len )
    16 + SWAP   ( name-data-addr bc-addr bc-len )
    DROP        ( name-data-addr bc-len )
    R@          ( name-data-addr bc-len name-len )
    LLVM-EMIT-CALL
    R> 16 + /STRING  ( advance: 8 opcode + 8 len + name-len )
    EXIT
  THEN

  DUP OP-RETURN = IF
    DROP
    8 /STRING
    EXIT
  THEN

  DUP OP-BRANCH-IF-ZERO = IF
    DROP
    \ Branch-if-zero has: opcode(8) + offset(8)
    8 + OVER @  ( bc-addr bc-len offset )
    \ TODO: Handle branches with labels
    \ For now, skip
    DROP
    16 /STRING
    EXIT
  THEN

  DUP OP-BRANCH = IF
    DROP
    \ Branch has: opcode(8) + offset(8)
    8 + OVER @  ( bc-addr bc-len offset )
    \ TODO: Handle branches with labels
    DROP
    16 /STRING
    EXIT
  THEN

  DUP OP-DO-SETUP = IF
    DROP
    \ DO-SETUP has: opcode(8) + data(8)
    \ TODO: Handle DO loops
    16 /STRING
    EXIT
  THEN

  DUP OP-LOOP = IF
    DROP
    \ LOOP has: opcode(8) + offset(8)
    \ TODO: Handle LOOP
    16 /STRING
    EXIT
  THEN

  DUP OP-ALLOC-STRING = IF
    DROP
    \ AllocString has: opcode(8) + len(8) + string_data(variable)
    8 + OVER @  ( bc-addr bc-len str-len )
    DUP >R
    \ TODO: Handle string allocation
    R> 16 + /STRING
    EXIT
  THEN

  DUP OP-VARIABLE-ADDR = IF
    DROP
    \ VariableAddr has: opcode(8) + name_len(8) + name_data(variable)
    8 + OVER @  ( bc-addr bc-len name-len )
    DUP >R
    \ TODO: Handle variable address
    R> 16 + /STRING
    EXIT
  THEN

  \ Unknown opcode - skip it
  ." Unknown opcode: " . CR
  8 /STRING
;

\ Track if we hit a return
VARIABLE HIT-RETURN
0 HIT-RETURN !

\ Process all bytecode instructions
: PROCESS-BYTECODE ( bc-addr bc-len -- )
  0 HIT-RETURN !
  BEGIN
    DUP 0> HIT-RETURN @ 0= AND
  WHILE
    OVER @ OP-RETURN = IF -1 HIT-RETURN ! THEN
    PROCESS-INSTRUCTION
  REPEAT
  2DROP
;

\ Generate LLVM code for a word
: CODEGEN-WORD ( name-addr name-len bytecode-addr bytecode-len -- )
  \ Stack: name-addr name-len bc-addr bc-len

  \ Duplicate name for later use
  2OVER 2OVER      ( name-addr name-len bc-addr bc-len name-addr name-len bc-addr bc-len )
  >R >R            ( save bc-addr bc-len for later, stack: name-addr name-len bc-addr bc-len name-addr name-len )
  >R >R            ( save name-addr name-len for later, stack: name-addr name-len bc-addr bc-len )

  \ Start LLVM function with name
  2SWAP            ( bc-addr bc-len name-addr name-len )
  LLVM-BEGIN-FUNCTION  ( bc-addr bc-len )

  \ Process bytecode
  PROCESS-BYTECODE

  \ DEBUG: Show module before ending function
  ." [BEFORE END-FUNCTION]" CR
  LLVM-SHOW-MODULE

  \ End LLVM function
  LLVM-END-FUNCTION

  \ DEBUG: Show module after ending function
  ." [AFTER END-FUNCTION]" CR
  LLVM-SHOW-MODULE

  \ Register in Rust Compiler dictionary
  R> R>            ( restore name-addr name-len )
  2DUP             ( duplicate for CREATE-WORD: name-addr name-len name-addr name-len )
  LLVM-REGISTER-WORD ( consumes name-addr name-len, leaves name-addr name-len )

  \ Also add to Forth dictionary so FIND-WORD can find it
  R> R>            ( stack: name-addr name-len bc-addr bc-len )
  DROP             ( stack: name-addr name-len bc-addr )
  CREATE-WORD      ( name-addr name-len bc-addr )
;

\ Test: Create a simple function
: TEST-SIMPLE-CODEGEN ( -- )
  S" TESTFUNC"
  0 0  \ Empty bytecode
  CODEGEN-WORD
  ." Simple codegen test complete" CR
;

\ Test: Create function that pushes 123
: TEST-LITERAL-CODEGEN ( -- )
  S" PUSH123"
  LLVM-BEGIN-FUNCTION
  123 LLVM-EMIT-LITERAL
  LLVM-END-FUNCTION
  ." Literal codegen test complete" CR
;

\ Test: Create function that calls DUP
: TEST-CALL-CODEGEN ( -- )
  S" CALLDUP"
  LLVM-BEGIN-FUNCTION
  S" DUP" LLVM-EMIT-CALL
  LLVM-END-FUNCTION
  ." Call codegen test complete" CR
;
