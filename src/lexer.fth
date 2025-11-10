\ lexer.fth - Forth-based Lexer
\ This lexer tokenizes Forth source code into a token stream
\ that can be consumed by the parser

\ ============================================================================
\ TOKEN STRUCTURE
\ ============================================================================

\ Token structure (24 bytes per token)
\   +0: Type (1 cell = 8 bytes)
\   +8: Data (1 cell = 8 bytes) - number value OR string address
\  +16: Length (1 cell = 8 bytes) - string length OR 0 for non-strings

\ Constants for token buffer
35000 CONSTANT TOKEN-BUFFER    \ Start of token array (moved to 35000 to avoid variable conflicts)
VARIABLE TOKEN-COUNT            \ Number of tokens
24 CONSTANT TOKEN-SIZE          \ Size of each token

\ ============================================================================
\ TOKEN ACCESSORS
\ ============================================================================

: TOKEN@ ( index -- addr )
  TOKEN-SIZE * TOKEN-BUFFER +
;

: TOKEN-TYPE@ ( index -- type )
  TOKEN@ @
;

: TOKEN-DATA@ ( index -- data )
  TOKEN@ 8 + @
;

: TOKEN-LENGTH@ ( index -- len )
  TOKEN@ 16 + @
;

: TOKEN-TYPE! ( type index -- )
  TOKEN@ !
;

: TOKEN-DATA! ( data index -- )
  TOKEN@ 8 + !
;

: TOKEN-LENGTH! ( len index -- )
  TOKEN@ 16 + !
;

\ ============================================================================
\ TOKEN TYPES
\ ============================================================================

\ Basic token types (lexer emits these)
0 CONSTANT TOK-EOF
1 CONSTANT TOK-COLON
2 CONSTANT TOK-SEMICOLON
3 CONSTANT TOK-NUMBER
4 CONSTANT TOK-WORD
5 CONSTANT TOK-STRING

\ Note: We use Option B (Generic words)
\ The lexer emits TOK-WORD for all identifiers (including keywords)
\ The parser is responsible for recognizing keywords like IF, THEN, etc.

\ ============================================================================
\ INPUT BUFFER TRACKING
\ ============================================================================

VARIABLE LEX-INPUT-BUFFER     \ Address of source code
VARIABLE LEX-INPUT-LENGTH     \ Length of source code
VARIABLE LEX-INPUT-POS        \ Current position in buffer

\ ============================================================================
\ LEXER INITIALIZATION
\ ============================================================================

: LEX-INIT ( addr len -- )
  LEX-INPUT-LENGTH !
  LEX-INPUT-BUFFER !
  0 LEX-INPUT-POS !
  0 TOKEN-COUNT !
;

\ ============================================================================
\ INPUT NAVIGATION
\ ============================================================================

\ Get current character (or 0 if EOF)
: CURRENT-CHAR ( -- char | 0 )
  LEX-INPUT-POS @ LEX-INPUT-LENGTH @ < IF
    LEX-INPUT-BUFFER @ LEX-INPUT-POS @ + C@
  ELSE
    0  \ EOF
  THEN
;

\ Advance position by 1
: ADVANCE ( -- )
  1 LEX-INPUT-POS +!
;

\ Peek ahead n characters
: PEEK ( n -- char | 0 )
  LEX-INPUT-POS @ +
  DUP LEX-INPUT-LENGTH @ < IF
    LEX-INPUT-BUFFER @ + C@
  ELSE
    DROP 0
  THEN
;

\ ============================================================================
\ CHARACTER CLASSIFICATION
\ ============================================================================

: IS-WHITESPACE? ( char -- flag )
  DUP 32 = SWAP      \ space
  DUP 10 = SWAP      \ newline (LF)
  DUP 13 = SWAP      \ carriage return (CR)
  9 = OR OR OR       \ tab
;

: IS-DIGIT? ( char -- flag )
  DUP 48 >= SWAP 57 <= AND  \ '0' to '9'
;

: IS-ALPHA? ( char -- flag )
  DUP DUP 97 >= SWAP 122 <= AND  \ 'a' to 'z'
  SWAP DUP 65 >= SWAP 90 <= AND  \ 'A' to 'Z'
  OR
;

\ Characters valid in word names (includes operators and special chars)
: IS-WORD-CHAR? ( char -- flag )
  DUP IS-ALPHA? SWAP
  DUP IS-DIGIT? SWAP
  DUP 45 = SWAP   \ '-'
  DUP 95 = SWAP   \ '_'
  DUP 63 = SWAP   \ '?'
  DUP 33 = SWAP   \ '!'
  DUP 43 = SWAP   \ '+'
  DUP 42 = SWAP   \ '*'
  DUP 47 = SWAP   \ '/'
  DUP 60 = SWAP   \ '<'
  DUP 61 = SWAP   \ '='
  DUP 62 = SWAP   \ '>'
  DUP 64 = SWAP   \ '@'
  46 = OR OR OR OR OR OR OR OR OR OR OR OR OR  \ '.' - 13 ORs for 14 tests
;

\ ============================================================================
\ WHITESPACE AND COMMENT HANDLING
\ ============================================================================

\ Skip whitespace characters
: SKIP-WHITESPACE ( -- )
  BEGIN
    CURRENT-CHAR
    DUP IS-WHITESPACE?
  WHILE
    DROP ADVANCE
  REPEAT
  DROP
;

\ Skip line comment (backslash to end of line)
: SKIP-LINE-COMMENT ( -- )
  BEGIN
    CURRENT-CHAR
    DUP 10 <> SWAP 0<> AND  \ Not newline and not EOF
  WHILE
    ADVANCE
  REPEAT
;

\ Skip paren comment ( ... )
: SKIP-PAREN-COMMENT ( -- )
  ADVANCE  \ Skip opening paren
  BEGIN
    CURRENT-CHAR
    DUP 41 <> SWAP 0<> AND  \ Not ')' and not EOF
  WHILE
    ADVANCE
  REPEAT
  ADVANCE  \ Skip closing paren
;

\ ============================================================================
\ TOKEN EMISSION
\ ============================================================================

\ Emit a token to the token buffer
: EMIT-TOKEN ( type data length -- )
  TOKEN-COUNT @         ( type data length index )
  >R                    ( type data length ) ( R: index )
  R@ TOKEN-LENGTH!
  R@ TOKEN-DATA!
  R> TOKEN-TYPE!
  1 TOKEN-COUNT +!
;

\ ============================================================================
\ NUMBER PARSING
\ ============================================================================

\ Parse a number token (handles negative numbers)
: LEX-NUMBER ( -- )
  0                                     ( negative-flag=0 )

  \ Check for negative sign
  CURRENT-CHAR 45 = IF
    -1                                  ( -1 negative-flag )
    ADVANCE                             \ Skip the minus
  THEN

  LEX-INPUT-BUFFER @ LEX-INPUT-POS @ +  ( negative-flag digit-start-addr )
  0                                     ( negative-flag digit-start-addr count )

  \ Count consecutive digits
  BEGIN
    CURRENT-CHAR IS-DIGIT?
  WHILE
    1+
    ADVANCE
  REPEAT

  ( negative-flag start-addr count )
  \ Convert string to number using >NUMBER
  \ >NUMBER expects: ud1-low ud1-high c-addr1 u1 -- ud2-low ud2-high c-addr2 u2
  >R >R                        ( negative-flag ) ( R: count start-addr )
  0 0                          ( negative-flag 0 0 )
  R> R>                        ( negative-flag 0 0 start-addr count )
  >NUMBER                      ( negative-flag ud2-low ud2-high addr2 count2 )
  2DROP                        ( negative-flag ud2-low ud2-high )
  SWAP DROP                    ( negative-flag value ) \ Keep low cell, drop high cell

  \ Apply negative flag
  SWAP IF NEGATE THEN          ( final-value )

  TOK-NUMBER SWAP 0 EMIT-TOKEN
;

\ ============================================================================
\ WORD PARSING
\ ============================================================================

\ Parse a word token (identifier)
: LEX-WORD ( -- )
  LEX-INPUT-BUFFER @ LEX-INPUT-POS @ +  ( start-addr )
  0                                     ( start-addr count )

  \ Count word characters
  BEGIN
    CURRENT-CHAR IS-WORD-CHAR?
  WHILE
    1+
    ADVANCE
  REPEAT

  ( start-addr count )
  TOK-WORD -ROT EMIT-TOKEN
;

\ ============================================================================
\ STRING PARSING
\ ============================================================================

\ Parse string literal: S" or ."
: LEX-STRING ( -- )
  ADVANCE  \ Skip the quote

  LEX-INPUT-BUFFER @ LEX-INPUT-POS @ +  ( start-addr )
  0                                     ( start-addr count )

  \ Find closing quote
  BEGIN
    CURRENT-CHAR
    DUP 34 <> SWAP 0<> AND  \ Not " and not EOF
  WHILE
    1+
    ADVANCE
  REPEAT

  ( start-addr count )
  ADVANCE  \ Skip closing quote

  TOK-STRING -ROT EMIT-TOKEN
;

\ ============================================================================
\ MAIN TOKENIZER
\ ============================================================================

\ Tokenize source code
: TOKENIZE ( addr len -- )
  LEX-INIT

  BEGIN
    SKIP-WHITESPACE
    CURRENT-CHAR DUP 0<>    \ Not EOF?
  WHILE
    \ Check for special characters
    DUP 58 = IF                        \ ':' colon
      DROP ADVANCE
      TOK-COLON 0 0 EMIT-TOKEN
    ELSE DUP 59 = IF                   \ ';' semicolon
      DROP ADVANCE
      TOK-SEMICOLON 0 0 EMIT-TOKEN
    ELSE DUP 92 = IF                   \ '\' line comment (backslash)
      DROP ADVANCE
      SKIP-LINE-COMMENT
    ELSE DUP 40 = IF                   \ '(' paren comment
      DROP
      SKIP-PAREN-COMMENT
    ELSE DUP 34 = IF                   \ '"' string literal
      DROP
      LEX-STRING
    ELSE DUP 45 = IF                   \ '-' could be negative number or operator
      \ Peek ahead: if next char is digit, it's a negative number
      1 PEEK IS-DIGIT? IF
        DROP LEX-NUMBER
      ELSE
        DROP LEX-WORD
      THEN
    ELSE DUP IS-DIGIT? IF              \ Number
      DROP LEX-NUMBER
    ELSE DUP IS-WORD-CHAR? IF          \ Word/identifier
      DROP LEX-WORD
    ELSE
      \ Unknown character - skip it
      DROP ADVANCE
    THEN THEN THEN THEN THEN THEN THEN THEN
  REPEAT
  DROP

  \ Emit EOF token
  TOK-EOF 0 0 EMIT-TOKEN
;

\ ============================================================================
\ DEBUG PRINTING
\ ============================================================================

\ Print a single token (for debugging)
: .TOKEN ( index -- )
  DUP TOKEN-TYPE@
  DUP TOK-NUMBER = IF
    DROP ." NUMBER(" DUP TOKEN-DATA@ . ." ) " DROP EXIT
  THEN
  DUP TOK-COLON = IF
    DROP ." COLON " DROP EXIT
  THEN
  DUP TOK-SEMICOLON = IF
    DROP ." SEMICOLON " DROP EXIT
  THEN
  DUP TOK-WORD = IF
    DROP ." WORD("
    DUP TOKEN-DATA@ OVER TOKEN-LENGTH@ TYPE
    ." ) " DROP EXIT
  THEN
  DUP TOK-STRING = IF
    DROP ." STRING("
    DUP TOKEN-DATA@ OVER TOKEN-LENGTH@ TYPE
    ." ) " DROP EXIT
  THEN
  DUP TOK-EOF = IF
    DROP ." EOF " DROP EXIT
  THEN
  DROP ." UNKNOWN " DROP
;

\ Print all tokens
: .TOKENS ( -- )
  TOKEN-COUNT @ 0 DO
    I .TOKEN
  LOOP
  CR
;
