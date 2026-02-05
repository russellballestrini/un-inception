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

3600 constant LANGUAGES_CACHE_TTL

: languages-cache-file ( -- addr len )
    s" $HOME/.unsandbox/languages.json"
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

\ Get API keys from environment (HMAC or legacy)
: get-public-key ( -- addr len )
    s" UNSANDBOX_PUBLIC_KEY" getenv
    dup 0= if
        2drop s" UNSANDBOX_API_KEY" getenv
    then
    dup 0= if
        s" Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set" type cr
        1 (bye)
    then
;

: get-secret-key ( -- addr len )
    s" UNSANDBOX_SECRET_KEY" getenv
    dup 0= if
        2drop s" UNSANDBOX_API_KEY" getenv
    then
;

\ Get API key (legacy compatibility)
: get-api-key ( -- addr len )
    get-public-key
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
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" LANG='" r@ write-file throw
    2swap 2drop \ drop language, keep filename on stack
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" FILE='" r@ write-file throw
    r@ write-file throw
    s" '" r@ write-line throw
    s" BODY=$(jq -Rs '{language: \"'$LANG'\", code: .}' < \"$FILE\")" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/execute:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/execute -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" -o /tmp/unsandbox_resp.json" r@ write-line throw
    s" RESP=$(cat /tmp/unsandbox_resp.json)" r@ write-line throw
    s" if echo \"$RESP\" | grep -q \"timestamp\" && (echo \"$RESP\" | grep -Eq \"(401|expired|invalid)\"); then" r@ write-line throw
    s"   echo -e '\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m' >&2" r@ write-line throw
    s"   echo -e '\\x1b[33mYour computer'\\''s clock may have drifted.\\x1b[0m' >&2" r@ write-line throw
    s"   echo 'Check your system time and sync with NTP if needed:' >&2" r@ write-line throw
    s"   echo '  Linux:   sudo ntpdate -s time.nist.gov' >&2" r@ write-line throw
    s"   echo '  macOS:   sudo sntp -sS time.apple.com' >&2" r@ write-line throw
    s"   echo -e '  Windows: w32tm /resync\\x1b[0m' >&2" r@ write-line throw
    s"   rm -f /tmp/unsandbox_resp.json" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    s" jq -r '.stdout // empty' /tmp/unsandbox_resp.json | sed 's/^/\\x1b[34m/' | sed 's/$/\\x1b[0m/'" r@ write-line throw
    s" jq -r '.stderr // empty' /tmp/unsandbox_resp.json | sed 's/^/\\x1b[31m/' | sed 's/$/\\x1b[0m/' >&2" r@ write-line throw
    s" rm -f /tmp/unsandbox_resp.json" r@ write-line throw
    r> close-file throw

    s" chmod +x /tmp/unsandbox_script.sh && /tmp/unsandbox_script.sh && rm -f /tmp/unsandbox_script.sh" system
    (bye)
;

\ Session list
: session-list ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/sessions:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/sessions -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq -r '.sessions[] | \"\\(.id) \\(.shell) \\(.status) \\(.created_at)\"' 2>/dev/null || echo 'No active sessions'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Session kill
: session-kill ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SESSION_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:DELETE:/sessions/$SESSION_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X DELETE https://api.unsandbox.com/sessions/$SESSION_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mSession terminated: " r@ write-file throw
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
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/services:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq -r '.services[] | \"\\(.id) \\(.name) \\(.status)\"' 2>/dev/null || echo 'No services'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service info
: service-info ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/services/$SERVICE_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services/$SERVICE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service logs
: service-logs ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/services/$SERVICE_ID/logs:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/services/$SERVICE_ID/logs -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq -r '.logs'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service sleep
: service-sleep ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/services/$SERVICE_ID/freeze:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/services/$SERVICE_ID/freeze -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mService frozen: " r@ write-file throw
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
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/services/$SERVICE_ID/unfreeze:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/services/$SERVICE_ID/unfreeze -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mService unfreezing: " r@ write-file throw
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
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:DELETE:/services/$SERVICE_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/services/$SERVICE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\")" r@ write-line throw
    s" HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" BODY=$(echo \"$RESP\" | sed '$d')" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"428\" ]; then" r@ write-line throw
    s"   CHALLENGE_ID=$(echo \"$BODY\" | grep -o '\"challenge_id\":\"[^\"]*\"' | cut -d'\"' -f4)" r@ write-line throw
    s"   echo -e '\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m' >&2" r@ write-line throw
    s"   echo -n 'Enter OTP: ' >&2" r@ write-line throw
    s"   read OTP" r@ write-line throw
    s"   if [ -z \"$OTP\" ]; then echo 'Error: Operation cancelled' >&2; exit 1; fi" r@ write-line throw
    s"   TIMESTAMP=$(date +%s)" r@ write-line throw
    s"   MESSAGE=\"$TIMESTAMP:DELETE:/services/$SERVICE_ID:\"" r@ write-line throw
    s"   SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"   RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/services/$SERVICE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H \"X-Sudo-OTP: $OTP\" -H \"X-Sudo-Challenge: $CHALLENGE_ID\")" r@ write-line throw
    s"   HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"200\" ]; then" r@ write-line throw
    s"   echo -e '\\x1b[32mService destroyed: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    s" else" r@ write-line throw
    s"   echo -e \"\\x1b[31mError: HTTP $HTTP_CODE\\x1b[0m\" >&2" r@ write-line throw
    s"   echo \"$BODY\" >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service resize
: service-resize ( service-id-addr service-id-len vcpu-addr vcpu-len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2over r@ write-file throw
    s" '" r@ write-line throw
    s" VCPU='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" if [ \"$VCPU\" -lt 1 ] || [ \"$VCPU\" -gt 8 ]; then" r@ write-line throw
    s"   echo -e '\\x1b[31mError: --vcpu must be between 1 and 8\\x1b[0m' >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    s" RAM=$((VCPU * 2))" r@ write-line throw
    s" BODY='{\"vcpu\":'$VCPU'}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:PATCH:/services/$SERVICE_ID:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X PATCH https://api.unsandbox.com/services/$SERVICE_ID -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" >/dev/null && echo -e \"\\x1b[32mService resized to $VCPU vCPU, $RAM GB RAM\\x1b[0m\"" r@ write-line throw
    r> close-file throw
    2drop 2drop \ clean up the stack
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service env status
: service-env-status ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/services/$SERVICE_ID/env:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET \"https://api.unsandbox.com/services/$SERVICE_ID/env\" -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service env set (with -e and --env-file support via shell script)
: service-env-set ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" SERVICE_ID=''; ENV_CONTENT=''; ENV_FILE=''" r@ write-line throw
    s" i=4" r@ write-line throw
    s" SERVICE_ID=$3" r@ write-line throw
    s" while [ $i -le $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     -e) ((i++)); VAL=${!i}" r@ write-line throw
    s"       if [ -n \"$ENV_CONTENT\" ]; then ENV_CONTENT=\"$ENV_CONTENT\n$VAL\"; else ENV_CONTENT=\"$VAL\"; fi ;;" r@ write-line throw
    s"     --env-file) ((i++)); ENV_FILE=${!i} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" if [ -n \"$ENV_FILE\" ] && [ -f \"$ENV_FILE\" ]; then" r@ write-line throw
    s"   while IFS= read -r line || [ -n \"$line\" ]; do" r@ write-line throw
    s"     case \"$line\" in \"#\"*|\"\") continue ;; esac" r@ write-line throw
    s"     if [ -n \"$ENV_CONTENT\" ]; then ENV_CONTENT=\"$ENV_CONTENT\n$line\"; else ENV_CONTENT=\"$line\"; fi" r@ write-line throw
    s"   done < \"$ENV_FILE\"" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ -z \"$ENV_CONTENT\" ]; then echo -e '\\x1b[31mError: No environment variables to set\\x1b[0m' >&2; exit 1; fi" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:PUT:/services/$SERVICE_ID/env:$ENV_CONTENT\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" echo -e \"$ENV_CONTENT\" | curl -s -X PUT \"https://api.unsandbox.com/services/$SERVICE_ID/env\" -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: text/plain' --data-binary @- | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service env export
: service-env-export ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/services/$SERVICE_ID/env/export:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST \"https://api.unsandbox.com/services/$SERVICE_ID/env/export\" -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq -r '.content // empty'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service env delete
: service-env-delete ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:DELETE:/services/$SERVICE_ID/env:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X DELETE \"https://api.unsandbox.com/services/$SERVICE_ID/env\" -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mVault deleted for: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service dump bootstrap
: service-dump-bootstrap ( service-id-addr service-id-len file-addr file-len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SERVICE_ID='" r@ write-file throw
    2over r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" echo 'Fetching bootstrap script from $SERVICE_ID...' >&2" r@ write-line throw
    s" BODY='{\"command\":\"cat /tmp/bootstrap.sh\"}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/services/$SERVICE_ID/execute:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -X POST https://api.unsandbox.com/services/$SERVICE_ID/execute -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\")" r@ write-line throw
    s" STDOUT=$(echo \"$RESP\" | jq -r '.stdout // empty')" r@ write-line throw
    s" if [ -n \"$STDOUT\" ]; then" r@ write-line throw
    2dup 0 0 d= if
        \ No file specified, print to stdout
        2drop
        s"   echo \"$STDOUT\"" r@ write-line throw
    else
        \ File specified, save to file
        s"   echo \"$STDOUT\" > '" r@ write-file throw
        r@ write-file throw
        s" ' && chmod 755 '" r@ write-file throw
        2dup r@ write-file throw
        s" ' && echo 'Bootstrap saved to " r@ write-file throw
        r@ write-file throw
        s" '" r@ write-line throw
    then
    s" else" r@ write-line throw
    s"   echo -e '\\x1b[31mError: Failed to fetch bootstrap\\x1b[0m' >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Service create (requires --name, optional --ports, --domains, --type, --bootstrap, -f, -e, --env-file)
: service-create ( -- )
    get-api-key
    \ Parse arguments (simplified - in real implementation would iterate through args)
    \ For now, just create the curl command that will be constructed by bash
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" NAME=''; PORTS=''; DOMAINS=''; TYPE=''; BOOTSTRAP=''; BOOTSTRAP_FILE=''; INPUT_FILES=''" r@ write-line throw
    s" ENV_CONTENT=''; ENV_FILE=''" r@ write-line throw
    s" i=3" r@ write-line throw
    s" while [ $i -lt $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     --name) ((i++)); NAME=${!i} ;;" r@ write-line throw
    s"     --ports) ((i++)); PORTS=${!i} ;;" r@ write-line throw
    s"     --domains) ((i++)); DOMAINS=${!i} ;;" r@ write-line throw
    s"     --type) ((i++)); TYPE=${!i} ;;" r@ write-line throw
    s"     --bootstrap) ((i++)); BOOTSTRAP=${!i} ;;" r@ write-line throw
    s"     --bootstrap-file) ((i++)); BOOTSTRAP_FILE=${!i} ;;" r@ write-line throw
    s"     -e) ((i++)); VAL=${!i}" r@ write-line throw
    s"       if [ -n \"$ENV_CONTENT\" ]; then ENV_CONTENT=\"$ENV_CONTENT\n$VAL\"; else ENV_CONTENT=\"$VAL\"; fi ;;" r@ write-line throw
    s"     --env-file) ((i++)); ENV_FILE=${!i} ;;" r@ write-line throw
    s"     -f) ((i++)); FILE=${!i}" r@ write-line throw
    s"       if [ -f \"$FILE\" ]; then" r@ write-line throw
    s"         BASENAME=$(basename \"$FILE\")" r@ write-line throw
    s"         CONTENT=$(base64 -w0 \"$FILE\")" r@ write-line throw
    s"         if [ -z \"$INPUT_FILES\" ]; then" r@ write-line throw
    s"           INPUT_FILES=\"{\\\"filename\\\":\\\"$BASENAME\\\",\\\"content\\\":\\\"$CONTENT\\\"}\"" r@ write-line throw
    s"         else" r@ write-line throw
    s"           INPUT_FILES=\"$INPUT_FILES,{\\\"filename\\\":\\\"$BASENAME\\\",\\\"content\\\":\\\"$CONTENT\\\"}\"" r@ write-line throw
    s"         fi" r@ write-line throw
    s"       else" r@ write-line throw
    s"         echo \"Error: File not found: $FILE\" >&2" r@ write-line throw
    s"         exit 1" r@ write-line throw
    s"       fi ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" # Parse env file if specified" r@ write-line throw
    s" if [ -n \"$ENV_FILE\" ] && [ -f \"$ENV_FILE\" ]; then" r@ write-line throw
    s"   while IFS= read -r line || [ -n \"$line\" ]; do" r@ write-line throw
    s"     case \"$line\" in \"#\"*|\"\") continue ;; esac" r@ write-line throw
    s"     if [ -n \"$ENV_CONTENT\" ]; then ENV_CONTENT=\"$ENV_CONTENT\n$line\"; else ENV_CONTENT=\"$line\"; fi" r@ write-line throw
    s"   done < \"$ENV_FILE\"" r@ write-line throw
    s" fi" r@ write-line throw
    s" [ -z \"$NAME\" ] && echo 'Error: --name required' && exit 1" r@ write-line throw
    s" PAYLOAD='{\"name\":\"'\"$NAME\"'\"}'" r@ write-line throw
    s" [ -n \"$PORTS\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg p \"$PORTS\" '. + {ports: ($p | split(\",\") | map(tonumber))}')" r@ write-line throw
    s" [ -n \"$DOMAINS\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg d \"$DOMAINS\" '. + {domains: ($d | split(\",\"))}')" r@ write-line throw
    s" [ -n \"$TYPE\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg t \"$TYPE\" '. + {service_type: $t}')" r@ write-line throw
    s" [ -n \"$BOOTSTRAP\" ] && PAYLOAD=$(echo $PAYLOAD | jq --arg b \"$BOOTSTRAP\" '. + {bootstrap: $b}')" r@ write-line throw
    s" if [ -n \"$BOOTSTRAP_FILE\" ]; then" r@ write-line throw
    s"   [ ! -f \"$BOOTSTRAP_FILE\" ] && echo -e '\\x1b[31mError: Bootstrap file not found: '$BOOTSTRAP_FILE'\\x1b[0m' >&2 && exit 1" r@ write-line throw
    s"   PAYLOAD=$(echo $PAYLOAD | jq --rawfile b \"$BOOTSTRAP_FILE\" '. + {bootstrap_content: $b}')" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ -n \"$INPUT_FILES\" ]; then" r@ write-line throw
    s"   PAYLOAD=$(echo $PAYLOAD | jq --argjson f \"[$INPUT_FILES]\" '. + {input_files: $f}')" r@ write-line throw
    s" fi" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/services:$PAYLOAD\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -X POST https://api.unsandbox.com/services -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$PAYLOAD\")" r@ write-line throw
    s" echo \"$RESP\" | jq ." r@ write-line throw
    s" # Auto-set vault if env vars were provided" r@ write-line throw
    s" if [ -n \"$ENV_CONTENT\" ]; then" r@ write-line throw
    s"   SERVICE_ID=$(echo \"$RESP\" | jq -r '.id // empty')" r@ write-line throw
    s"   if [ -n \"$SERVICE_ID\" ]; then" r@ write-line throw
    s"     echo -e '\\x1b[33mSetting vault for service...\\x1b[0m'" r@ write-line throw
    s"     TIMESTAMP=$(date +%s)" r@ write-line throw
    s"     MESSAGE=\"$TIMESTAMP:PUT:/services/$SERVICE_ID/env:$ENV_CONTENT\"" r@ write-line throw
    s"     SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"     echo -e \"$ENV_CONTENT\" | curl -s -X PUT \"https://api.unsandbox.com/services/$SERVICE_ID/env\" -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: text/plain' --data-binary @- | jq ." r@ write-line throw
    s"   fi" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Key validate
: validate-key ( extend-flag -- )
    get-api-key
    s" /tmp/unsandbox_key_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" PORTAL_BASE='" r@ write-file throw
    portal-base r@ write-file throw
    s" '" r@ write-line throw
    s" BODY='{}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/keys/validate:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw

    \ Check if extend flag is set
    0= if
        \ Normal validation
        s" curl -s -X POST $PORTAL_BASE/keys/validate -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" -o /tmp/unsandbox_key_resp.json" r@ write-line throw
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
        s" RESP=$(curl -s -X POST $PORTAL_BASE/keys/validate -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\")" r@ write-line throw
        s" FETCHED_PUBLIC_KEY=$(echo \"$RESP\" | jq -r '.public_key // \"N/A\"')" r@ write-line throw
        s" xdg-open \"$PORTAL_BASE/keys/extend?pk=$FETCHED_PUBLIC_KEY\" 2>/dev/null" r@ write-line throw
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

\ Session create with input_files support
: session-create ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" SHELL='bash'" r@ write-line throw
    s" INPUT_FILES=''" r@ write-line throw
    s" for ((i=2; i<$#; i++)); do" r@ write-line throw
    s"   case ${!i} in" r@ write-line throw
    s"     --shell|-s) ((i++)); SHELL=${!i} ;;" r@ write-line throw
    s"     -f) ((i++)); FILE=${!i}" r@ write-line throw
    s"       if [ -f \"$FILE\" ]; then" r@ write-line throw
    s"         BASENAME=$(basename \"$FILE\")" r@ write-line throw
    s"         CONTENT=$(base64 -w0 \"$FILE\")" r@ write-line throw
    s"         if [ -z \"$INPUT_FILES\" ]; then" r@ write-line throw
    s"           INPUT_FILES=\"{\\\"filename\\\":\\\"$BASENAME\\\",\\\"content\\\":\\\"$CONTENT\\\"}\"" r@ write-line throw
    s"         else" r@ write-line throw
    s"           INPUT_FILES=\"$INPUT_FILES,{\\\"filename\\\":\\\"$BASENAME\\\",\\\"content\\\":\\\"$CONTENT\\\"}\"" r@ write-line throw
    s"         fi" r@ write-line throw
    s"       else" r@ write-line throw
    s"         echo \"Error: File not found: $FILE\" >&2" r@ write-line throw
    s"         exit 1" r@ write-line throw
    s"       fi ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s" done" r@ write-line throw
    s" if [ -n \"$INPUT_FILES\" ]; then" r@ write-line throw
    s"   BODY=\"{\\\"shell\\\":\\\"$SHELL\\\",\\\"input_files\\\":[$INPUT_FILES]}\"" r@ write-line throw
    s" else" r@ write-line throw
    s"   BODY=\"{\\\"shell\\\":\\\"$SHELL\\\"}\"" r@ write-line throw
    s" fi" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/sessions:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" echo -e '\\x1b[33mCreating session...\\x1b[0m'" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/sessions -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\"" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
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

    \ Check for --shell or -f flags (create session)
    2dup s" --shell" compare 0= if
        2drop session-create
        0 (bye)
    then

    2dup s" -s" compare 0= if
        2drop session-create
        0 (bye)
    then

    2dup s" -f" compare 0= if
        2drop session-create
        0 (bye)
    then

    \ Check if argument starts with '-'
    2dup drop c@ [char] - = if
        s" Unknown option: " type type cr
        s" Usage: un.forth session [options]" type cr
        2drop
        1 (bye)
    then

    2drop
    session-create
    0 (bye)
;

\ Handle service subcommand
: handle-service ( -- )
    argc @ 3 < if
        s" Error: Use --name (create), --list, --info, --logs, --freeze, --unfreeze, or --destroy" type cr
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

    2dup s" --freeze" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --freeze requires service ID" type cr
            1 (bye)
        then
        3 arg service-sleep
        0 (bye)
    then

    2dup s" --unfreeze" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --unfreeze requires service ID" type cr
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

    2dup s" --resize" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --resize requires service ID" type cr
            1 (bye)
        then
        \ Look for --vcpu or -v in remaining args
        argc @ 5 < if
            s" Error: --resize requires --vcpu N" type cr
            1 (bye)
        then
        4 arg 2dup s" --vcpu" compare 0= if
            2drop
            argc @ 6 < if
                s" Error: --vcpu requires a value" type cr
                1 (bye)
            then
            3 arg 5 arg service-resize
            0 (bye)
        then
        2dup s" -v" compare 0= if
            2drop
            argc @ 6 < if
                s" Error: -v requires a value" type cr
                1 (bye)
            then
            3 arg 5 arg service-resize
            0 (bye)
        then
        2drop
        s" Error: --resize requires --vcpu N" type cr
        1 (bye)
    then

    2dup s" --dump-bootstrap" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --dump-bootstrap requires service ID" type cr
            1 (bye)
        then
        3 arg
        \ Check for --dump-file
        argc @ 5 >= if
            4 arg 2dup s" --dump-file" compare 0= if
                2drop
                argc @ 6 < if
                    s" Error: --dump-file requires filename" type cr
                    1 (bye)
                then
                5 arg
            else
                2drop 0 0
            then
        else
            0 0
        then
        service-dump-bootstrap
        0 (bye)
    then

    \ Handle env subcommand: service env <action> <service_id> [options]
    2dup s" env" compare 0= if
        2drop
        argc @ 4 < if
            s" Usage: un.forth service env <status|set|export|delete> <service_id> [options]" type cr
            1 (bye)
        then
        3 arg 2dup s" status" compare 0= if
            2drop
            argc @ 5 < if
                s" Error: status requires service ID" type cr
                1 (bye)
            then
            4 arg service-env-status
            0 (bye)
        then
        2dup s" set" compare 0= if
            2drop
            argc @ 5 < if
                s" Error: set requires service ID" type cr
                1 (bye)
            then
            service-env-set
            0 (bye)
        then
        2dup s" export" compare 0= if
            2drop
            argc @ 5 < if
                s" Error: export requires service ID" type cr
                1 (bye)
            then
            4 arg service-env-export
            0 (bye)
        then
        2dup s" delete" compare 0= if
            2drop
            argc @ 5 < if
                s" Error: delete requires service ID" type cr
                1 (bye)
            then
            4 arg service-env-delete
            0 (bye)
        then
        2drop
        s" Error: Unknown env action. Use status, set, export, or delete" type cr
        1 (bye)
    then

    2drop
    s" Error: Use --name (create), --list, --info, --logs, --freeze, --unfreeze, --destroy, --dump-bootstrap, or env" type cr
    1 (bye)
;

\ Languages list with caching
: languages-list ( json-flag -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" CACHE_TTL=3600" r@ write-line throw
    s" CACHE_FILE=\"$HOME/.unsandbox/languages.json\"" r@ write-line throw
    s" JSON_OUTPUT=" r@ write-file throw
    if
        s" 1" r@ write-line throw
    else
        s" 0" r@ write-line throw
    then
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    \ Check cache first
    s" if [ -f \"$CACHE_FILE\" ]; then" r@ write-line throw
    s"     CACHE_TS=$(jq -r '.timestamp // 0' \"$CACHE_FILE\" 2>/dev/null)" r@ write-line throw
    s"     CURRENT_TS=$(date +%s)" r@ write-line throw
    s"     AGE=$((CURRENT_TS - CACHE_TS))" r@ write-line throw
    s"     if [ $AGE -lt $CACHE_TTL ]; then" r@ write-line throw
    s"         if [ \"$JSON_OUTPUT\" = \"1\" ]; then" r@ write-line throw
    s"             jq -c '.languages' \"$CACHE_FILE\"" r@ write-line throw
    s"         else" r@ write-line throw
    s"             jq -r '.languages[]' \"$CACHE_FILE\"" r@ write-line throw
    s"         fi" r@ write-line throw
    s"         exit 0" r@ write-line throw
    s"     fi" r@ write-line throw
    s" fi" r@ write-line throw
    \ Fetch from API
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/languages:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -X GET https://api.unsandbox.com/languages -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\")" r@ write-line throw
    s" LANGS=$(echo \"$RESP\" | jq -c '.languages // []')" r@ write-line throw
    \ Save to cache
    s" mkdir -p \"$HOME/.unsandbox\"" r@ write-line throw
    s" echo \"{\\\"languages\\\":$LANGS,\\\"timestamp\\\":$(date +%s)}\" > \"$CACHE_FILE\"" r@ write-line throw
    \ Output
    s" if [ \"$JSON_OUTPUT\" = \"1\" ]; then" r@ write-line throw
    s"     echo \"$LANGS\"" r@ write-line throw
    s" else" r@ write-line throw
    s"     echo \"$LANGS\" | jq -r '.[]'" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Handle languages subcommand
: handle-languages ( -- )
    argc @ 3 < if
        0 languages-list
        0 (bye)
    then

    2 arg 2dup s" --json" compare 0= if
        2drop
        1 languages-list
        0 (bye)
    then

    2drop
    0 languages-list
    0 (bye)
;

\ Image list
: image-list ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/images:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/images -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image info
: image-info ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/images/$IMAGE_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/images/$IMAGE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image delete
: image-delete ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:DELETE:/images/$IMAGE_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/images/$IMAGE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\")" r@ write-line throw
    s" HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" BODY=$(echo \"$RESP\" | sed '$d')" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"428\" ]; then" r@ write-line throw
    s"   CHALLENGE_ID=$(echo \"$BODY\" | grep -o '\"challenge_id\":\"[^\"]*\"' | cut -d'\"' -f4)" r@ write-line throw
    s"   echo -e '\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m' >&2" r@ write-line throw
    s"   echo -n 'Enter OTP: ' >&2" r@ write-line throw
    s"   read OTP" r@ write-line throw
    s"   if [ -z \"$OTP\" ]; then echo 'Error: Operation cancelled' >&2; exit 1; fi" r@ write-line throw
    s"   TIMESTAMP=$(date +%s)" r@ write-line throw
    s"   MESSAGE=\"$TIMESTAMP:DELETE:/images/$IMAGE_ID:\"" r@ write-line throw
    s"   SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"   RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/images/$IMAGE_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H \"X-Sudo-OTP: $OTP\" -H \"X-Sudo-Challenge: $CHALLENGE_ID\")" r@ write-line throw
    s"   HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"200\" ]; then" r@ write-line throw
    s"   echo -e '\\x1b[32mImage deleted: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    s" else" r@ write-line throw
    s"   echo -e \"\\x1b[31mError: HTTP $HTTP_CODE\\x1b[0m\" >&2" r@ write-line throw
    s"   echo \"$BODY\" >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image lock
: image-lock ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/lock:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/lock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mImage locked: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image unlock
: image-unlock ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/unlock:{}\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -w '\\n%{http_code}' -X POST https://api.unsandbox.com/images/$IMAGE_ID/unlock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: application/json' -d '{}')" r@ write-line throw
    s" HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" BODY=$(echo \"$RESP\" | sed '$d')" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"428\" ]; then" r@ write-line throw
    s"   CHALLENGE_ID=$(echo \"$BODY\" | grep -o '\"challenge_id\":\"[^\"]*\"' | cut -d'\"' -f4)" r@ write-line throw
    s"   echo -e '\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m' >&2" r@ write-line throw
    s"   echo -n 'Enter OTP: ' >&2" r@ write-line throw
    s"   read OTP" r@ write-line throw
    s"   if [ -z \"$OTP\" ]; then echo 'Error: Operation cancelled' >&2; exit 1; fi" r@ write-line throw
    s"   TIMESTAMP=$(date +%s)" r@ write-line throw
    s"   MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/unlock:{}\"" r@ write-line throw
    s"   SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"   RESP=$(curl -s -w '\\n%{http_code}' -X POST https://api.unsandbox.com/images/$IMAGE_ID/unlock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: application/json' -H \"X-Sudo-OTP: $OTP\" -H \"X-Sudo-Challenge: $CHALLENGE_ID\" -d '{}')" r@ write-line throw
    s"   HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"200\" ]; then" r@ write-line throw
    s"   echo -e '\\x1b[32mImage unlocked: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    s" else" r@ write-line throw
    s"   echo -e \"\\x1b[31mError: HTTP $HTTP_CODE\\x1b[0m\" >&2" r@ write-line throw
    s"   echo \"$BODY\" >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image visibility
: image-visibility ( image-id-addr image-id-len mode-addr mode-len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2over r@ write-file throw
    s" '" r@ write-line throw
    s" MODE='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" BODY='{\"visibility\":\"'$MODE'\"}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/visibility:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/visibility -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" >/dev/null && echo -e \"\\x1b[32mImage visibility set to $MODE\\x1b[0m\"" r@ write-line throw
    r> close-file throw
    2drop 2drop \ clean up the stack
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image spawn
: image-spawn ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" NAME=''; PORTS=''" r@ write-line throw
    s" i=4" r@ write-line throw
    s" while [ $i -le $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     --name) ((i++)); NAME=${!i} ;;" r@ write-line throw
    s"     --ports) ((i++)); PORTS=${!i} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" BODY='{}'" r@ write-line throw
    s" [ -n \"$NAME\" ] && BODY=$(echo $BODY | jq --arg n \"$NAME\" '. + {name: $n}')" r@ write-line throw
    s" [ -n \"$PORTS\" ] && BODY=$(echo $BODY | jq --arg p \"$PORTS\" '. + {ports: ($p | split(\",\") | map(tonumber))}')" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/spawn:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/spawn -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" | jq ." r@ write-line throw
    s" echo -e '\\x1b[32mService spawned from image\\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image clone
: image-clone ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" NAME=''" r@ write-line throw
    s" i=4" r@ write-line throw
    s" while [ $i -le $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     --name) ((i++)); NAME=${!i} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" BODY='{}'" r@ write-line throw
    s" [ -n \"$NAME\" ] && BODY=$(echo $BODY | jq --arg n \"$NAME\" '. + {name: $n}')" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/clone:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/clone -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" | jq ." r@ write-line throw
    s" echo -e '\\x1b[32mImage cloned\\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image publish
: image-publish ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" SOURCE_ID=''; SOURCE_TYPE=''; NAME=''" r@ write-line throw
    s" i=3" r@ write-line throw
    s" while [ $i -le $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     --publish) ((i++)); SOURCE_ID=${!i} ;;" r@ write-line throw
    s"     --source-type) ((i++)); SOURCE_TYPE=${!i} ;;" r@ write-line throw
    s"     --name) ((i++)); NAME=${!i} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" [ -z \"$SOURCE_TYPE\" ] && echo -e '\\x1b[31mError: --source-type required (service or snapshot)\\x1b[0m' >&2 && exit 1" r@ write-line throw
    s" BODY='{\"source_type\":\"'$SOURCE_TYPE'\",\"source_id\":\"'$SOURCE_ID'\"}'" r@ write-line throw
    s" [ -n \"$NAME\" ] && BODY=$(echo $BODY | jq --arg n \"$NAME\" '. + {name: $n}')" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/publish:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/publish -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" | jq ." r@ write-line throw
    s" echo -e '\\x1b[32mImage published\\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image grant access
: image-grant-access ( image-id-addr image-id-len key-addr key-len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2over r@ write-file throw
    s" '" r@ write-line throw
    s" TRUSTED_KEY='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" BODY='{\"trusted_api_key\":\"'$TRUSTED_KEY'\"}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/grant-access:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/grant-access -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" >/dev/null && echo -e \"\\x1b[32mAccess granted to $TRUSTED_KEY\\x1b[0m\"" r@ write-line throw
    r> close-file throw
    2drop 2drop \ clean up the stack
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image revoke access
: image-revoke-access ( image-id-addr image-id-len key-addr key-len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2over r@ write-file throw
    s" '" r@ write-line throw
    s" TRUSTED_KEY='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" BODY='{\"trusted_api_key\":\"'$TRUSTED_KEY'\"}'" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/images/$IMAGE_ID/revoke-access:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/images/$IMAGE_ID/revoke-access -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" >/dev/null && echo -e \"\\x1b[32mAccess revoked from $TRUSTED_KEY\\x1b[0m\"" r@ write-line throw
    r> close-file throw
    2drop 2drop \ clean up the stack
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Image list trusted
: image-list-trusted ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" IMAGE_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/images/$IMAGE_ID/trusted:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/images/$IMAGE_ID/trusted -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot list
: snapshot-list ( -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/snapshots:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/snapshots -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot info
: snapshot-info ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:GET:/snapshots/$SNAPSHOT_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X GET https://api.unsandbox.com/snapshots/$SNAPSHOT_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" | jq ." r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot restore
: snapshot-restore ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/snapshots/$SNAPSHOT_ID/restore:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/snapshots/$SNAPSHOT_ID/restore -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mSnapshot restored: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot delete
: snapshot-delete ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:DELETE:/snapshots/$SNAPSHOT_ID:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/snapshots/$SNAPSHOT_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\")" r@ write-line throw
    s" HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" BODY=$(echo \"$RESP\" | sed '$d')" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"428\" ]; then" r@ write-line throw
    s"   CHALLENGE_ID=$(echo \"$BODY\" | grep -o '\"challenge_id\":\"[^\"]*\"' | cut -d'\"' -f4)" r@ write-line throw
    s"   echo -e '\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m' >&2" r@ write-line throw
    s"   echo -n 'Enter OTP: ' >&2" r@ write-line throw
    s"   read OTP" r@ write-line throw
    s"   if [ -z \"$OTP\" ]; then echo 'Error: Operation cancelled' >&2; exit 1; fi" r@ write-line throw
    s"   TIMESTAMP=$(date +%s)" r@ write-line throw
    s"   MESSAGE=\"$TIMESTAMP:DELETE:/snapshots/$SNAPSHOT_ID:\"" r@ write-line throw
    s"   SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"   RESP=$(curl -s -w '\\n%{http_code}' -X DELETE https://api.unsandbox.com/snapshots/$SNAPSHOT_ID -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H \"X-Sudo-OTP: $OTP\" -H \"X-Sudo-Challenge: $CHALLENGE_ID\")" r@ write-line throw
    s"   HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"200\" ]; then" r@ write-line throw
    s"   echo -e '\\x1b[32mSnapshot deleted: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    s" else" r@ write-line throw
    s"   echo -e \"\\x1b[31mError: HTTP $HTTP_CODE\\x1b[0m\" >&2" r@ write-line throw
    s"   echo \"$BODY\" >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot lock
: snapshot-lock ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/snapshots/$SNAPSHOT_ID/lock:\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/snapshots/$SNAPSHOT_ID/lock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" >/dev/null && echo -e '\\x1b[32mSnapshot locked: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot unlock
: snapshot-unlock ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/snapshots/$SNAPSHOT_ID/unlock:{}\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" RESP=$(curl -s -w '\\n%{http_code}' -X POST https://api.unsandbox.com/snapshots/$SNAPSHOT_ID/unlock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: application/json' -d '{}')" r@ write-line throw
    s" HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" BODY=$(echo \"$RESP\" | sed '$d')" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"428\" ]; then" r@ write-line throw
    s"   CHALLENGE_ID=$(echo \"$BODY\" | grep -o '\"challenge_id\":\"[^\"]*\"' | cut -d'\"' -f4)" r@ write-line throw
    s"   echo -e '\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m' >&2" r@ write-line throw
    s"   echo -n 'Enter OTP: ' >&2" r@ write-line throw
    s"   read OTP" r@ write-line throw
    s"   if [ -z \"$OTP\" ]; then echo 'Error: Operation cancelled' >&2; exit 1; fi" r@ write-line throw
    s"   TIMESTAMP=$(date +%s)" r@ write-line throw
    s"   MESSAGE=\"$TIMESTAMP:POST:/snapshots/$SNAPSHOT_ID/unlock:{}\"" r@ write-line throw
    s"   SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s"   RESP=$(curl -s -w '\\n%{http_code}' -X POST https://api.unsandbox.com/snapshots/$SNAPSHOT_ID/unlock -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -H 'Content-Type: application/json' -H \"X-Sudo-OTP: $OTP\" -H \"X-Sudo-Challenge: $CHALLENGE_ID\" -d '{}')" r@ write-line throw
    s"   HTTP_CODE=$(echo \"$RESP\" | tail -1)" r@ write-line throw
    s" fi" r@ write-line throw
    s" if [ \"$HTTP_CODE\" = \"200\" ]; then" r@ write-line throw
    s"   echo -e '\\x1b[32mSnapshot unlocked: " r@ write-file throw
    r@ write-file throw
    s" \\x1b[0m'" r@ write-line throw
    s" else" r@ write-line throw
    s"   echo -e \"\\x1b[31mError: HTTP $HTTP_CODE\\x1b[0m\" >&2" r@ write-line throw
    s"   echo \"$BODY\" >&2" r@ write-line throw
    s"   exit 1" r@ write-line throw
    s" fi" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && /tmp/unsandbox_cmd.sh && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Snapshot clone
: snapshot-clone ( addr len -- )
    get-api-key
    s" /tmp/unsandbox_cmd.sh" w/o create-file throw >r
    s" #!/bin/bash" r@ write-line throw
    s" SNAPSHOT_ID='" r@ write-file throw
    2dup r@ write-file throw
    s" '" r@ write-line throw
    s" PUBLIC_KEY='" r@ write-file throw
    get-public-key r@ write-file throw
    s" '" r@ write-line throw
    s" SECRET_KEY='" r@ write-file throw
    get-secret-key r@ write-file throw
    s" '" r@ write-line throw
    s" CLONE_TYPE='session'; NAME=''; PORTS=''; SHELL=''" r@ write-line throw
    s" i=4" r@ write-line throw
    s" while [ $i -le $# ]; do" r@ write-line throw
    s"   arg=${!i}" r@ write-line throw
    s"   case \"$arg\" in" r@ write-line throw
    s"     --type) ((i++)); CLONE_TYPE=${!i} ;;" r@ write-line throw
    s"     --name) ((i++)); NAME=${!i} ;;" r@ write-line throw
    s"     --ports) ((i++)); PORTS=${!i} ;;" r@ write-line throw
    s"     --shell) ((i++)); SHELL=${!i} ;;" r@ write-line throw
    s"   esac" r@ write-line throw
    s"   ((i++))" r@ write-line throw
    s" done" r@ write-line throw
    s" BODY='{\"clone_type\":\"'$CLONE_TYPE'\"}'" r@ write-line throw
    s" [ -n \"$NAME\" ] && BODY=$(echo $BODY | jq --arg n \"$NAME\" '. + {name: $n}')" r@ write-line throw
    s" [ -n \"$PORTS\" ] && BODY=$(echo $BODY | jq --arg p \"$PORTS\" '. + {ports: ($p | split(\",\") | map(tonumber))}')" r@ write-line throw
    s" [ -n \"$SHELL\" ] && BODY=$(echo $BODY | jq --arg s \"$SHELL\" '. + {shell: $s}')" r@ write-line throw
    s" TIMESTAMP=$(date +%s)" r@ write-line throw
    s" MESSAGE=\"$TIMESTAMP:POST:/snapshots/$SNAPSHOT_ID/clone:$BODY\"" r@ write-line throw
    s" SIGNATURE=$(echo -n \"$MESSAGE\" | openssl dgst -sha256 -hmac \"$SECRET_KEY\" -hex | sed 's/.*= //')" r@ write-line throw
    s" curl -s -X POST https://api.unsandbox.com/snapshots/$SNAPSHOT_ID/clone -H 'Content-Type: application/json' -H \"Authorization: Bearer $PUBLIC_KEY\" -H \"X-Timestamp: $TIMESTAMP\" -H \"X-Signature: $SIGNATURE\" -d \"$BODY\" | jq ." r@ write-line throw
    s" echo -e '\\x1b[32mSnapshot cloned\\x1b[0m'" r@ write-line throw
    r> close-file throw
    s" chmod +x /tmp/unsandbox_cmd.sh && bash /tmp/unsandbox_cmd.sh \"$@\" && rm -f /tmp/unsandbox_cmd.sh" system
;

\ Handle snapshot subcommand
: handle-snapshot ( -- )
    argc @ 3 < if
        snapshot-list
        0 (bye)
    then

    2 arg 2dup s" --list" compare 0= if
        2drop snapshot-list
        0 (bye)
    then

    2dup s" -l" compare 0= if
        2drop snapshot-list
        0 (bye)
    then

    2dup s" --info" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --info requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-info
        0 (bye)
    then

    2dup s" --restore" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --restore requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-restore
        0 (bye)
    then

    2dup s" --delete" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --delete requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-delete
        0 (bye)
    then

    2dup s" --lock" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --lock requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-lock
        0 (bye)
    then

    2dup s" --unlock" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --unlock requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-unlock
        0 (bye)
    then

    2dup s" --clone" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --clone requires snapshot ID" type cr
            1 (bye)
        then
        3 arg snapshot-clone
        0 (bye)
    then

    2drop
    s" Error: Use --list, --info ID, --restore ID, --delete ID, --lock ID, --unlock ID, or --clone ID" type cr
    1 (bye)
;

\ Handle image subcommand
: handle-image ( -- )
    argc @ 3 < if
        s" Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID" type cr
        1 (bye)
    then

    2 arg 2dup s" --list" compare 0= if
        2drop image-list
        0 (bye)
    then

    2dup s" -l" compare 0= if
        2drop image-list
        0 (bye)
    then

    2dup s" --info" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --info requires image ID" type cr
            1 (bye)
        then
        3 arg image-info
        0 (bye)
    then

    2dup s" --delete" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --delete requires image ID" type cr
            1 (bye)
        then
        3 arg image-delete
        0 (bye)
    then

    2dup s" --lock" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --lock requires image ID" type cr
            1 (bye)
        then
        3 arg image-lock
        0 (bye)
    then

    2dup s" --unlock" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --unlock requires image ID" type cr
            1 (bye)
        then
        3 arg image-unlock
        0 (bye)
    then

    2dup s" --publish" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --publish requires source ID" type cr
            1 (bye)
        then
        image-publish
        0 (bye)
    then

    2dup s" --visibility" compare 0= if
        2drop
        argc @ 5 < if
            s" Error: --visibility requires image ID and mode" type cr
            1 (bye)
        then
        3 arg 4 arg image-visibility
        0 (bye)
    then

    2dup s" --spawn" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --spawn requires image ID" type cr
            1 (bye)
        then
        3 arg image-spawn
        0 (bye)
    then

    2dup s" --clone" compare 0= if
        2drop
        argc @ 4 < if
            s" Error: --clone requires image ID" type cr
            1 (bye)
        then
        3 arg image-clone
        0 (bye)
    then

    2drop
    s" Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID" type cr
    1 (bye)
;

\ Main program
: main
    \ Get command line argument count
    argc @ 2 < if
        s" Usage: gforth un.forth <source_file>" type cr
        s"        gforth un.forth session [options]" type cr
        s"        gforth un.forth service [options]" type cr
        s"        gforth un.forth image [options]" type cr
        s"        gforth un.forth key [options]" type cr
        s"        gforth un.forth languages [--json]" type cr
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

    2dup s" image" compare 0= if
        2drop handle-image
        0 (bye)
    then

    2dup s" snapshot" compare 0= if
        2drop handle-snapshot
        0 (bye)
    then

    2dup s" key" compare 0= if
        2drop handle-key
        0 (bye)
    then

    2dup s" languages" compare 0= if
        2drop handle-languages
        0 (bye)
    then

    \ Default: execute file
    execute-file
;

main
