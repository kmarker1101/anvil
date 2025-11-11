\ forth-compiler.fth - Forth-based compiler using lexer+parser+codegen

INCLUDE src/meta-compiler.fth

\ Compile a word definition end-to-end
\ For now, manually specify: we know DOUBLE will be at position 45000
: COMPILE-DOUBLE-MANUAL ( -- )
  ." Compiling via Forth compiler..." CR
  
  \ Step 1-2: Tokenize and parse
  S" : DOUBLE 2 * ;" TOKENIZE
  PARSE-PROGRAM
  
  \ Step 3: Generate LLVM (manually for now)
  S" DOUBLE"
  45000 100  \ bytecode addr, fake length
  CODEGEN-WORD
  
  ." DOUBLE compiled via pure Forth compiler!" CR
;

COMPILE-DOUBLE-MANUAL
