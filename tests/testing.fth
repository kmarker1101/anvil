\ ============================================================================
\ BASIC FORTH TESTING FRAMEWORK
\ Simple, clear, and easy to understand
\ ============================================================================

\ --- Test Counters ---
VARIABLE TESTS-RUN
VARIABLE TESTS-PASSED
VARIABLE TESTS-FAILED
VARIABLE CURRENT-TEST-FAILED

\ --- Current Test File Name ---
VARIABLE TEST-FILE-ADDR
VARIABLE TEST-FILE-LEN

\ --- Initialize ---
: INIT-TESTS ( -- )
    0 TESTS-RUN !
    0 TESTS-PASSED !
    0 TESTS-FAILED !
    0 TEST-FILE-ADDR !
    0 TEST-FILE-LEN ! ;

\ --- Set Current Test File ---
: TEST-FILE ( addr len -- )
    TEST-FILE-LEN !
    TEST-FILE-ADDR ! ;

\ --- Core Assertion ---
: EXPECT ( actual expected -- )
    = IF
        1 TESTS-PASSED +!
    ELSE
        1 TESTS-FAILED +!
        1 CURRENT-TEST-FAILED !
    THEN
    1 TESTS-RUN +! ;

\ --- Clear Stack Helper ---
: CLEAR-STACK ( ... -- )
    BEGIN DEPTH WHILE DROP REPEAT ;

\ --- Test Wrapper ---
: TEST ( -- )
    CLEAR-STACK
    0 CURRENT-TEST-FAILED ! ;

\ --- Print test name only if it failed ---
\ Usage: TEST-NAME S" test name"
: TEST-NAME ( addr len -- )
    CURRENT-TEST-FAILED @ IF
        CR ." FAILED: "
        TEST-FILE-LEN @ 0> IF
            ." [" TEST-FILE-ADDR @ TEST-FILE-LEN @ TYPE ." ] "
        THEN
        TYPE
    ELSE
        2DROP
    THEN ;

\ --- Summary ---
: REPORT ( -- )
    CR
    TESTS-FAILED @ 0= IF
        ." All " TESTS-RUN @ . ." tests passed" CR
    ELSE
        TESTS-FAILED @ . ." of " TESTS-RUN @ . ." tests failed" CR
    THEN ;

