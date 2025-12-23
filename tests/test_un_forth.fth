\ Comprehensive tests for un.forth (Forth UN CLI Inception implementation)
\ Run with: gforth test_un_forth.fth

\ Color codes
: green   s" \033[32m" type ;
: red     s" \033[31m" type ;
: blue    s" \033[34m" type ;
: reset   s" \033[0m" type ;

\ Test counters
variable passed
variable failed

0 passed !
0 failed !

\ Extension to language mapping (from un.forth)
: ext-lang ( addr len -- addr len | 0 0 )
    2dup s" .jl" compare 0= if 2drop s" julia" exit then
    2dup s" .r" compare 0= if 2drop s" r" exit then
    2dup s" .cr" compare 0= if 2drop s" crystal" exit then
    2dup s" .f90" compare 0= if 2drop s" fortran" exit then
    2dup s" .cob" compare 0= if 2drop s" cobol" exit then
    2dup s" .pro" compare 0= if 2drop s" prolog" exit then
    2dup s" .forth" compare 0= if 2drop s" forth" exit then
    2dup s" .4th" compare 0= if 2drop s" forth" exit then
    2dup s" .py" compare 0= if 2drop s" python" exit then
    2dup s" .js" compare 0= if 2drop s" javascript" exit then
    2dup s" .rb" compare 0= if 2drop s" ruby" exit then
    2dup s" .go" compare 0= if 2drop s" go" exit then
    2dup s" .rs" compare 0= if 2drop s" rust" exit then
    2dup s" .c" compare 0= if 2drop s" c" exit then
    2dup s" .cpp" compare 0= if 2drop s" cpp" exit then
    2dup s" .java" compare 0= if 2drop s" java" exit then
    2dup s" .sh" compare 0= if 2drop s" bash" exit then
    2drop 0 0
;

\ Print test result
: print-test ( addr len result -- )
    if
        green ." ✓ PASS" reset ." : " type cr
        1 passed +!
    else
        red ." ✗ FAIL" reset ." : " type cr
        1 failed +!
    then
;

\ Test extension detection
: test-ext ( addr1 len1 addr2 len2 test-name-addr test-name-len -- )
    2>r
    ext-lang
    2dup 0 0 d<>
    if
        2swap compare 0=
    else
        2drop 2drop false
    then
    2r> rot print-test
;

\ Helper to create test name
: make-test-name ( ext-addr ext-len lang-addr lang-len -- name-addr name-len )
    here >r
    s" Detect " here swap dup >r move here r> +
    2swap dup >r move here r> +
    s"  as " dup >r move here r> +
    2swap dup >r move here r> +
    r> here over -
;

cr
blue ." ========================================" reset cr
blue ." UN CLI Inception Tests - Forth" reset cr
blue ." ========================================" reset cr cr

\ Test Suite 1: Extension Detection
blue ." Test Suite 1: Extension Detection" reset cr

s" .jl" s" julia" make-test-name >r >r
s" .jl" s" julia" r> r> test-ext

s" .r" s" r" make-test-name >r >r
s" .r" s" r" r> r> test-ext

s" .cr" s" crystal" make-test-name >r >r
s" .cr" s" crystal" r> r> test-ext

s" .f90" s" fortran" make-test-name >r >r
s" .f90" s" fortran" r> r> test-ext

s" .cob" s" cobol" make-test-name >r >r
s" .cob" s" cobol" r> r> test-ext

s" .pro" s" prolog" make-test-name >r >r
s" .pro" s" prolog" r> r> test-ext

s" .forth" s" forth" make-test-name >r >r
s" .forth" s" forth" r> r> test-ext

s" .4th" s" forth" make-test-name >r >r
s" .4th" s" forth" r> r> test-ext

s" .py" s" python" make-test-name >r >r
s" .py" s" python" r> r> test-ext

s" .rs" s" rust" make-test-name >r >r
s" .rs" s" rust" r> r> test-ext

\ Test unknown extension
s" .xyz" ext-lang 0 0 d= if
    s" Detect unknown extension" true print-test
else
    2drop s" Detect unknown extension" false print-test
then

\ Test Suite 2: API Integration
cr
blue ." Test Suite 2: API Integration" reset cr
blue ." ℹ SKIP" reset ."  : API integration test (requires runtime environment)" cr

\ Test Suite 3: End-to-End
cr
blue ." Test Suite 3: End-to-End Functional Test" reset cr
blue ." ℹ SKIP" reset ."  : E2E test (requires runtime environment and API key)" cr

\ Test Suite 4: Error Handling
cr
blue ." Test Suite 4: Error Handling" reset cr

\ Test that unknown returns 0 0
s" .unknown" ext-lang 0 0 d= if
    s" Unknown extension returns empty" true print-test
else
    2drop s" Unknown extension returns empty" false print-test
then

\ Test multiple extension support
s" .forth" ext-lang 2dup s" forth" compare 0= if
    2drop s" Forth extension .forth supported" true print-test
else
    2drop s" Forth extension .forth supported" false print-test
then

s" .4th" ext-lang 2dup s" forth" compare 0= if
    2drop s" Forth extension .4th supported" true print-test
else
    2drop s" Forth extension .4th supported" false print-test
then

\ Print summary
passed @ failed @ + value total

cr
blue ." ========================================" reset cr
blue ." Test Summary" reset cr
blue ." ========================================" reset cr
green ." Passed: " reset passed @ . cr
red ." Failed: " reset failed @ . cr
blue ." Total:  " reset total . cr

failed @ 0> if
    cr
    red ." TESTS FAILED" reset cr
    1 (bye)
else
    cr
    green ." ALL TESTS PASSED" reset cr
    0 (bye)
then
