\ PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
\
\ This is free public domain software for the public good of a permacomputer hosted
\ at permacomputer.com - an always-on computer by the people, for the people. One
\ which is durable, easy to repair, and distributed like tap water for machine
\ learning intelligence.
\
\ The permacomputer is community-owned infrastructure optimized around four values:
\
\   TRUTH    - First principles, math & science, open source code freely distributed
\   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
\   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
\   LOVE     - Be yourself without hurting others, cooperation through natural law
\
\ This software contributes to that vision by enabling code execution across 42+
\ programming languages through a unified interface, accessible to all. Code is
\ seeds to sprout on any abandoned technology.
\
\ Learn more: https://www.permacomputer.com
\
\ Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
\ software, either in source code form or as a compiled binary, for any purpose,
\ commercial or non-commercial, and by any means.
\
\ NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
\
\ That said, our permacomputer's digital membrane stratum continuously runs unit,
\ integration, and functional tests on all of it's own software - with our
\ permacomputer monitoring itself, repairing itself, with minimal human in the
\ loop guidance. Our agents do their best.
\
\ Copyright 2025 TimeHexOn & foxhop & russell@unturf
\ https://www.timehexon.com
\ https://www.foxhop.net
\ https://www.unturf.com/software


\ Unsandbox CLI in Forth
\ Usage: gforth un.forth <source_file>
\        gforth un.forth session [options]
\        gforth un.forth service [options]
\        gforth un.forth key [options]

\ Constants
: portal-base ( -- addr len )
    s" https://unsandbox.com"
;

\ Extension to language mapping (simple linear search)
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
    2dup s" .ts" compare 0= if 2drop s" typescript" exit then
    2dup s" .rb" compare 0= if 2drop s" ruby" exit then
    2dup s" .php" compare 0= if 2drop s" php" exit then
    2dup s" .pl" compare 0= if 2drop s" perl" exit then
    2dup s" .lua" compare 0= if 2drop s" lua" exit then
    2dup s" .sh" compare 0= if 2drop s" bash" exit then
    2dup s" .go" compare 0= if 2drop s" go" exit then
    2dup s" .rs" compare 0= if 2drop s" rust" exit then
    2dup s" .c" compare 0= if 2drop s" c" exit then
    2dup s" .cpp" compare 0= if 2drop s" cpp" exit then
    2dup s" .java" compare 0= if 2drop s" java" exit then
    2drop 0 0
;

\ Find extension in filename
: find-ext ( addr len -- addr len )
    2dup
    begin
        1- dup 0>=
    while
        2dup + c@ [char] . =
        if
            >r >r 2dup r> r> + swap over - exit
        then
    repeat
    2drop 0 0
;

\ Detect language from filename
: detect-language ( addr len -- addr len )
    find-ext ext-lang
;

\ Get API key from environment
: get-api-key ( -- addr len )
    s" UNSANDBOX_API_KEY" getenv
    dup 0= if
        s" Error: UNSANDBOX_API_KEY not set" type cr
        1 (bye)
    then
;

\ Execute a file
: execute-file ( addr len -- )
    \ Check file exists
    2dup file-status nip 0<> if
        s" Error: File not found" type cr
        2drop
        1 (bye)
    then

    \ Detect language
    2dup detect-language
    2dup 0 0 d= if
        2drop 2drop
        s" Error: Unknown language" type cr
        1 (bye)
    then

    \ Get API key
    get-api-key

    \ Build curl command (simplified - stores filename and language in temp vars)
    \ In a real implementation, would construct full command string
    s" /tmp/unsandbox_script.sh" w/o create-file throw
    >r
    s" #!/bin/bash" r@ write-line throw
    s" API_KEY='" r@ write-file throw
    get-api-key r@ write-file throw
    s" '" r@ write-line throw
    s" LANG='" r@ write-file throw
    2swap 2drop \ drop language, keep filename on stack
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" FILE='" r@ write-file throw
    r@ write-file throw
    s" '" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/execute -H 'Content-Type: application/json' -H \"Authorization: Bearer $API_KEY\" --data-binary @- -o /tmp/unsandbox_resp.json < <(jq -Rs '{language: \"'$LANG'\", code: .}' < \"$FILE\"); jq -r '.stdout // empty' /tmp/unsandbox_resp.json | sed 's/^/\\x1b[34m/' | sed 's/$/\\x1b[0m/'; jq -r '.stderr // empty' /tmp/unsandbox_resp.json | sed 's/^/\\x1b[31m/' | sed 's/$/\\x1b[0m/' >&2; rm -f /tmp/unsandbox_resp.json" r@ write-line throw
    r> close-file throw

    s" chmod +x /tmp/unsandbox_script.sh && /tmp/unsandbox_script.sh && rm -f /tmp/unsandbox_script.sh" system
    (bye)
;

\ Session list
: session-list ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/sessions -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' | jq -r '.sessions[] | \"\\(.id) \\(.shell) \\(.status) \\(.created_at)\"' 2>/dev/null || echo 'No active sessions'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Session kill
: session-kill ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X DELETE https://api.unsandbox.com/sessions/" r@ write-file throw
    2dup r@ write-file throw
    s"  -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' >/dev/null && echo -e '\\x1b[32mSession terminated: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service list
: service-list ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' | jq -r '.services[] | \"\\(.id) \\(.name) \\(.status)\"' 2>/dev/null || echo 'No services'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service info
: service-info ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services/" r@ write-file throw
    2dup r@ write-file throw
    s"  -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service logs
: service-logs ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services/" r@ write-file throw
    2dup r@ write-file throw
    s" /logs -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' | jq -r '.logs'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service sleep
: service-sleep ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/services/" r@ write-file throw
    2dup r@ write-file throw
    s" /sleep -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' >/dev/null && echo -e '\\x1b[32mService sleeping: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service wake
: service-wake ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/services/" r@ write-file throw
    2dup r@ write-file throw
    s" /wake -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' >/dev/null && echo -e '\\x1b[32mService waking: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service destroy
: service-destroy ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" curl -s -X DELETE https://api.unsandbox.com/services/" r@ write-file throw
    2dup r@ write-file throw
    s"  -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' >/dev/null && echo -e '\\x1b[32mService destroyed: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service create (requires --name, optional --ports, --domains, --type, --bootstrap)
: service-create ( -- )
    get-api-key
    \ Parse arguments (simplified - in real implementation would iterate through args)
    \ For now, just create the curl command that will be constructed by bash
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" NAME=''; PORTS=''; DOMAINS=''; TYPE=''; BOOTSTRAP=''" r@ write-line throw
    s" for ((i=3; i<$#; i+=2)); do" r@ write-line throw
    s"   case ${!i} in" r@ write-line throw
    s"     --name) NAME=${!((i+1))} ;;" r@ write-line throw
    s"     --ports) PORTS=${!((i+1))} ;;" r@ write-line throw
    s"     --domains) DOMAINS=${!((i+1))} ;;" r@ write-line throw
    s"     --type) TYPE=${!((i+1))} ;;" r@ write-line throw
    s"     --bootstrap) BOOTSTRAP=${!((i+1))} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s" done" r@ write-line throw
    s" [ -z \"$NAME\" ] && echo 'Error: --name required' && exit 1" r@ write-line throw
    s" PAYLOAD='{\"name\":\"'\"$NAME\"'\"}'" r@ write-line throw
    s" [ -n \"$PORTS\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg p \"$PORTS\" '. + {ports: ($p | split(\",\") | map(tonumber))}')" r@ write-line throw
    s" [ -n \"$DOMAINS\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg d \"$DOMAINS\" '. + {domains: ($d | split(\",\"))}')" r@ write-line throw
    s" [ -n \"$TYPE\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg t \"$TYPE\" '. + {service_type: $t}')" r@ write-line throw
    s" [ -n \"$BOOTSTRAP\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg b \"$BOOTSTRAP\" '. + {bootstrap: $b}')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/services -H 'Content-Type: application/json' -H 'Authorization: Bearer " r@ write-file throw
    get-api-key r@ write-file throw
    s" ' -d \"$PAYLOAD\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Key validate
: validate-key ( extend-flag -- )
    get-api-key
    s" /tmp/unsandbox_key_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" API_KEY='" r@ write-file throw
    get-api-key r@ write-file throw
    s" '" r@ write-line throw
    s" PORTAL_BASE='" r@ write-file throw
    portal-base r@ write-file throw
    s" '" r@ write-line throw

    \ Check if extend flag is set
    0= if
        \ Normal validation
        s" curl -s -X POST $PORTAL_BASE/keys/validate -H 'Content-Type: application/json' -H \"Authorization: Bearer $API_KEY\" -o /tmp/unsandbox_key_resp.json" r@ write-line throw
        s" STATUS=$?" r@ write-line throw
        s" if [ $STATUS -ne 0 ]; then" r@ write-line throw
        s"   echo -e '\\x1b[31mInvalid\\x1b[0m'" r@ write-line throw
        s"   exit 1" r@ write-line throw
        s" fi" r@ write-line throw
        s" EXPIRED=$(jq -r '.expired // false' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s" if [ \"$EXPIRED\" = \"true\" ]; then" r@ write-line throw
        s"   echo -e '\\x1b[31mExpired\\x1b[0m'" r@ write-line throw
        s"   echo 'Public Key: '$(jq -r '.public_key // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Tier: '$(jq -r '.tier // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Expired: '$(jq -r '.expires_at // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo -e '\\x1b[33mTo renew: Visit https://unsandbox.com/keys/extend\\x1b[0m'" r@ write-line throw
        s"   rm -f /tmp/unsandbox_key_resp.json" r@ write-line throw
        s"   exit 1" r@ write-line throw
        s" else" r@ write-line throw
        s"   echo -e '\\x1b[32mValid\\x1b[0m'" r@ write-line throw
        s"   echo 'Public Key: '$(jq -r '.public_key // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Tier: '$(jq -r '.tier // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Status: '$(jq -r '.status // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Expires: '$(jq -r '.expires_at // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Time Remaining: '$(jq -r '.time_remaining // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Rate Limit: '$(jq -r '.rate_limit // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Burst: '$(jq -r '.burst // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s"   echo 'Concurrency: '$(jq -r '.concurrency // \"N/A\"' /tmp/unsandbox_key_resp.json)" r@ write-line throw
        s" fi" r@ write-line throw
        s" rm -f /tmp/unsandbox_key_resp.json" r@ write-line throw
    else
        \ Extend mode
        s" RESP=$(curl -s -X POST $PORTAL_BASE/keys/validate -H 'Content-Type: application/json' -H \"Authorization: Bearer $API_KEY\")" r@ write-line throw
        s" PUBLIC_KEY=$(echo \"$RESP\" | jq -r '.public_key // \"N/A\"')" r@ write-line throw
        s" xdg-open \"$PORTAL_BASE/keys/extend?pk=$PUBLIC_KEY\" 2>/dev/null" r@ write-line throw
    then

    r> close-file throw
    s" chmod +x /tmp/unsandbox_key_cmd.sh && /tmp/unsandbox_key_cmd.sh && rm -f /tmp/unsandbox_key_cmd.sh" system
;

\ Handle key subcommand
: handle-key ( -- )
    argc @ 3 < if
        0 validate-key
        0 (bye)
    then

    2 arg 2dup s" --extend" compare 0= if
        2drop
        1 validate-key
        0 (bye)
    then

    2drop
    0 validate-key
    0 (bye)
;

\ Handle session subcommand
: handle-session ( -- )
    argc @ 3 < if
        s" Error: Use --list or --kill ID" type cr
        1 (bye)
    then

    2 arg 2dup s" --list" compare 0= if
        2drop session-list
        0 (bye)
    then

    2dup s" -l" compare 0= if
        2drop session-list
        0 (bye)
    then

    2dup s" --kill" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --kill requires session ID" type cr
            1 (bye)
        then
        3 arg session-kill
        0 (bye)
    then

    2drop
    s" Error: Use --list or --kill ID" type cr
    1 (bye)
;

\ Handle service subcommand
: handle-service ( -- )
    argc @ 3 < if
        s" Error: Use --name (create), --list, --info, --logs, --sleep, --wake, or --destroy" type cr
        1 (bye)
    then

    2 arg 2dup s" --list" compare 0= if
        2drop service-list
        0 (bye)
    then

    2dup s" -l" compare 0= if
        2drop service-list
        0 (bye)
    then

    2dup s" --name" compare 0= if
        2drop service-create
        0 (bye)
    then

    2dup s" --info" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --info requires service ID" type cr
            1 (bye)
        then
        3 arg service-info
        0 (bye)
    then

    2dup s" --logs" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --logs requires service ID" type cr
            1 (bye)
        then
        3 arg service-logs
        0 (bye)
    then

    2dup s" --sleep" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --sleep requires service ID" type cr
            1 (bye)
        then
        3 arg service-sleep
        0 (bye)
    then

    2dup s" --wake" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --wake requires service ID" type cr
            1 (bye)
        then
        3 arg service-wake
        0 (bye)
    then

    2dup s" --destroy" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --destroy requires service ID" type cr
            1 (bye)
        then
        3 arg service-destroy
        0 (bye)
    then

    2drop
    s" Error: Use --name (create), --list, --info, --logs, --sleep, --wake, or --destroy" type cr
    1 (bye)
;

\ Main program
: main
    \ Get command line argument count
    argc @ 2 < if
        s" Usage: gforth un.forth <source_file>" type cr
        s"        gforth un.forth session [options]" type cr
        s"        gforth un.forth service [options]" type cr
        s"        gforth un.forth key [options]" type cr
        1 (bye)
    then

    \ Get first argument (skip gforth and script name)
    1 arg

    \ Check for subcommands
    2dup s" session" compare 0= if
        2drop handle-session
        0 (bye)
    then

    2dup s" service" compare 0= if
        2drop handle-service
        0 (bye)
    then

    2dup s" key" compare 0= if
        2drop handle-key
        0 (bye)
    then

    \ Default: execute file
    execute-file
;

main
