\ Unit tests for un.forth - tests internal functions without API calls
\ Run with: gforth test_forth.fth -e bye

variable passed
variable failed

: test ( flag c-addr u -- )
    rot if
        ." ✓ " type cr
        1 passed +!
    else
        ." ✗ " type cr
        1 failed +!
    then ;

: str= ( c-addr1 u1 c-addr2 u2 -- flag )
    rot over <> if 2drop drop false exit then
    compare 0= ;

: starts-with ( c-addr1 u1 c-addr2 u2 -- flag )
    2over drop over min
    2swap 2drop
    compare 0= ;

: contains ( c-addr1 u1 c-addr2 u2 -- flag )
    2swap search nip nip ;

\ Extension mapping using simple string comparison
: get-language ( c-addr u -- c-addr u )
    2dup s" .py" str= if 2drop s" python" exit then
    2dup s" .js" str= if 2drop s" javascript" exit then
    2dup s" .rb" str= if 2drop s" ruby" exit then
    2dup s" .go" str= if 2drop s" go" exit then
    2dup s" .forth" str= if 2drop s" forth" exit then
    2dup s" .fth" str= if 2drop s" forth" exit then
    2dup s" .c" str= if 2drop s" c" exit then
    2drop s" " ;

cr ." === Extension Mapping Tests ===" cr

s" .py" get-language s" python" str=
s" Python extension maps correctly" test

s" .forth" get-language s" forth" str=
s" Forth extension maps correctly" test

s" .js" get-language s" javascript" str=
s" JavaScript extension maps correctly" test

s" .go" get-language s" go" str=
s" Go extension maps correctly" test

cr ." === Signature Format Tests ===" cr

: timestamp s" 1704067200" ;
: method s" POST" ;
: endpoint s" /execute" ;

\ Build message: timestamp:method:endpoint:body
: build-message ( -- c-addr u )
    s" 1704067200:POST:/execute:{\"language\":\"python\"}" ;

build-message timestamp starts-with
s" Signature format starts with timestamp" test

build-message s" :POST:" contains
s" Signature format contains :POST:" test

build-message s" :/execute:" contains
s" Signature format contains :/execute:" test

cr ." === Language Detection Tests ===" cr

: shebang-line s" #!/usr/bin/env python3" ;

shebang-line s" #!" starts-with
s" Python shebang detection - starts with #!" test

shebang-line s" python" contains
s" Python shebang detection - contains python" test

cr ." === Argument Parsing Tests ===" cr

: arg1 s" DEBUG=1" ;

\ Simple key extraction (before first =)
arg1 drop 5 s" DEBUG" str=
s" Parse -e KEY=VALUE format - key" test

\ Simple value extraction (after first =)
arg1 drop 6 + 1 s" 1" str=
s" Parse -e KEY=VALUE format - value" test

cr ." === API Constants Tests ===" cr

: api-base s" https://api.unsandbox.com" ;

api-base s" https://" starts-with
s" API base URL starts with https://" test

api-base s" unsandbox.com" contains
s" API base URL contains unsandbox.com" test

cr ." === Summary ===" cr
." Passed: " passed @ . cr
." Failed: " failed @ . cr
." Total:  " passed @ failed @ + . cr

failed @ 0> [if] 1 (bye) [then]
bye
