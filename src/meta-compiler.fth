\ meta-compiler.fth - Forth meta-compiler
\ Uses lexer.fth and parser.fth to compile Forth source

INCLUDE src/lexer.fth
INCLUDE src/parser.fth
INCLUDE src/codegen.fth

\ Dictionary entry structure (from parser.fth):
\   +0: Link to previous entry (8 bytes)
\   +8: Name address (8 bytes)
\  +16: Name length (8 bytes)
\  +24: Code address (8 bytes)

\ LATEST is defined as a VARIABLE in parser.fth
\ It's somewhere in memory after DICT-HERE
\ We need to access it properly - for now, just check if it exists
\ Actually, parser.fth already defines LATEST as a VARIABLE
\ So we can just use LATEST @ to get the value

\ Get bytecode start/length for a word
: GET-WORD-BYTECODE ( dict-entry -- bc-addr bc-len )
  24 + @  ( code-addr )
  \ For now, we don't know the length easily
  \ We'd need to either:
  \ 1. Store it in dictionary
  \ 2. Scan for OP-RETURN
  \ 3. Use next word's code-addr as boundary
  \ For now, return 0 length (will need to fix)
  0
;

\ Generate LLVM for one dictionary entry
: CODEGEN-DICT-ENTRY ( dict-entry -- )
  DUP 8 + @      ( entry name-addr )
  OVER 16 + @    ( entry name-addr name-len )
  ROT            ( name-addr name-len entry )
  GET-WORD-BYTECODE ( name-addr name-len bc-addr bc-len )
  CODEGEN-WORD
;

\ Generate LLVM for all words in dictionary
: CODEGEN-ALL-WORDS ( -- )
  LATEST @ ( get latest word )
  DUP 0= IF
    DROP
    ." No words to compile" CR
    EXIT
  THEN
  BEGIN DUP WHILE
    DUP CODEGEN-DICT-ENTRY
    @ ( follow link to previous )
  REPEAT
  DROP
;

\ Compile Forth source string to LLVM
: FORTH-COMPILE ( source-addr source-len -- )
  \ Step 1: Tokenize with Forth lexer
  TOKENIZE

  \ Step 2: Parse tokens to bytecode
  PARSE-PROGRAM

  \ Step 3: Generate LLVM from bytecode for each word
  CODEGEN-ALL-WORDS

  ." Forth->LLVM compilation complete!" CR
;

\ Test compiling a simple definition
: TEST-META-COMPILE ( -- )
  S" : DOUBLE 2 * ;"
  FORTH-COMPILE
;
