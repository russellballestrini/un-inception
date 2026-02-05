#!/usr/bin/env -S awk -f
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software

#!/usr/bin/awk -f
# un.awk - Unsandbox CLI Client (AWK Implementation)
#
# Usage: awk -f un.awk <source_file>
#
# Note: AWK has limited capabilities, so this uses system() to call curl
# Requires: UNSANDBOX_API_KEY environment variable

BEGIN {
    VERSION = "4.2.50"
    API_BASE = "https://api.unsandbox.com"
    PORTAL_BASE = "https://unsandbox.com"
    LANGUAGES_CACHE_TTL = 3600  # 1 hour cache TTL
    LANGUAGES_CACHE_FILE = ENVIRON["HOME"] "/.unsandbox/languages.json"
    LAST_ERROR = ""

    # Extension to language map
    split("py:python js:javascript ts:typescript rb:ruby php:php pl:perl lua:lua sh:bash go:go rs:rust c:c cpp:cpp java:java kt:kotlin cs:csharp fs:fsharp hs:haskell ml:ocaml clj:clojure scm:scheme lisp:commonlisp erl:erlang ex:elixir jl:julia r:r cr:crystal d:d nim:nim zig:zig v:v dart:dart groovy:groovy f90:fortran cob:cobol pro:prolog forth:forth tcl:tcl raku:raku m:objc awk:awk ps1:powershell", pairs, " ")
    for (i in pairs) {
        split(pairs[i], kv, ":")
        ext_map[kv[1]] = kv[2]
    }

    # Colors
    BLUE = "\033[34m"
    RED = "\033[31m"
    GREEN = "\033[32m"
    YELLOW = "\033[33m"
    RESET = "\033[0m"
}

# ============================================================================
# Utility Functions
# ============================================================================

function version() {
    return VERSION
}

function last_error() {
    return LAST_ERROR
}

function set_error(msg) {
    LAST_ERROR = msg
}

function detect_language(filename    , ext) {
    if (filename == "") return ""
    ext = get_extension(filename)
    if (ext in ext_map) {
        return ext_map[ext]
    }
    return ""
}

function hmac_sign(secret, message    , cmd, sig) {
    if (secret == "" || message == "") return ""
    cmd = "echo -n '" message "' | openssl dgst -sha256 -hmac '" secret "' | sed 's/^.* //'"
    cmd | getline sig
    close(cmd)
    return sig
}

function health_check(    cmd, result) {
    cmd = "curl -s -o /dev/null -w '%{http_code}' '" API_BASE "/health'"
    cmd | getline result
    close(cmd)
    return (result == "200")
}

function get_api_keys(    public_key, secret_key, cmd) {
    # Get public key
    cmd = "echo -n $UNSANDBOX_PUBLIC_KEY"
    cmd | getline public_key
    close(cmd)

    # Get secret key
    cmd = "echo -n $UNSANDBOX_SECRET_KEY"
    cmd | getline secret_key
    close(cmd)

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if (public_key == "") {
        cmd = "echo -n $UNSANDBOX_API_KEY"
        cmd | getline public_key
        close(cmd)
        secret_key = ""
    }

    if (public_key == "") {
        print RED "Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set" RESET > "/dev/stderr"
        exit 1
    }

    GLOBAL_PUBLIC_KEY = public_key
    GLOBAL_SECRET_KEY = secret_key
}

function get_extension(filename) {
    n = split(filename, parts, ".")
    if (n > 1) {
        return parts[n]
    }
    return ""
}

function escape_json(s) {
    gsub(/\\/, "\\\\", s)
    gsub(/"/, "\\\"", s)
    gsub(/\n/, "\\n", s)
    gsub(/\r/, "\\r", s)
    gsub(/\t/, "\\t", s)
    return s
}

function execute(filename    , api_key) {
    get_api_keys()
    api_key = GLOBAL_PUBLIC_KEY

    # Get extension and language
    ext = get_extension(filename)
    language = ext_map[ext]

    if (language == "") {
        print RED "Error: Unknown extension: ." ext RESET > "/dev/stderr"
        exit 1
    }

    # Read file content
    code = ""
    while ((getline line < filename) > 0) {
        if (code != "") code = code "\n"
        code = code line
    }
    close(filename)

    # Escape for JSON
    escaped_code = escape_json(code)

    # Build JSON
    json = "{\"language\":\"" language "\",\"code\":\"" escaped_code "\"}"

    # Write to temp file
    tmp = "/tmp/un_awk_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        # HMAC signature: timestamp:METHOD:path:body
        sig_input = timestamp ":POST:/execute:" json
        sig_tmp = "/tmp/un_awk_sig_" PROCINFO["pid"]
        print sig_input > sig_tmp
        close(sig_tmp)
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        system("rm -f " sig_tmp)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE "/execute' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " api_key "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    # Check for timestamp authentication errors
    if (match(response, /timestamp/) && (match(response, /401/) || match(response, /expired/) || match(response, /invalid/))) {
        print RED "Error: Request timestamp expired (must be within 5 minutes of server time)" RESET > "/dev/stderr"
        print YELLOW "Your computer's clock may have drifted." RESET > "/dev/stderr"
        print "Check your system time and sync with NTP if needed:" > "/dev/stderr"
        print "  Linux:   sudo ntpdate -s time.nist.gov" > "/dev/stderr"
        print "  macOS:   sudo sntp -sS time.apple.com" > "/dev/stderr"
        print "  Windows: w32tm /resync" > "/dev/stderr"
        exit 1
    }

    # Parse stdout from response (simple regex)
    if (match(response, /"stdout":"([^"]*)"/, arr)) {
        stdout = arr[1]
        gsub(/\\n/, "\n", stdout)
        gsub(/\\t/, "\t", stdout)
        gsub(/\\"/, "\"", stdout)
        gsub(/\\\\/, "\\", stdout)
        printf "%s%s%s", BLUE, stdout, RESET
    }

    # Parse stderr
    if (match(response, /"stderr":"([^"]*)"/, arr)) {
        stderr = arr[1]
        gsub(/\\n/, "\n", stderr)
        gsub(/\\t/, "\t", stderr)
        gsub(/\\"/, "\"", stderr)
        gsub(/\\\\/, "\\", stderr)
        printf "%s%s%s", RED, stderr, RESET > "/dev/stderr"
    }

    # Parse exit code
    if (match(response, /"exit_code":([0-9]+)/, arr)) {
        exit arr[1]
    }
}

function session_list(    timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/sessions:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/sessions' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function session_kill(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/sessions/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    system(cmd)
    print GREEN "Session terminated: " id RESET
}

# Alias for session_kill
function session_destroy(id) {
    session_kill(id)
}

function session_get(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/sessions/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function session_freeze(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/sessions/" id "/freeze"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Session frozen: " id RESET
}

function session_unfreeze(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/sessions/" id "/unfreeze"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Session unfreezing: " id RESET
}

function session_boost(id, vcpu    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json) {
    get_api_keys()
    endpoint = "/sessions/" id "/boost"
    if (vcpu == "") vcpu = 2
    json = "{\"vcpu\":" vcpu "}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '" json "'"
    system(cmd " > /dev/null")
    print GREEN "Session boosted: " id RESET
}

function session_unboost(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/sessions/" id "/unboost"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Session unboosted: " id RESET
}

function session_execute(id, command    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json, line, response) {
    get_api_keys()
    endpoint = "/sessions/" id "/execute"
    json = "{\"command\":\"" escape_json(command) "\"}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '" json "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    print response
}

function service_list(    timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/services:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/services' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function service_get(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/services/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function service_freeze(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/services/" id "/freeze"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Service frozen: " id RESET
}

function service_unfreeze(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/services/" id "/unfreeze"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Service unfreezing: " id RESET
}

function service_lock(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/services/" id "/lock"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Service locked: " id RESET
}

function service_unlock(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/services/" id "/unlock"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -w '\\n%{http_code}' -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '{}'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)

    # Extract HTTP code
    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    if (http_code == 428) {
        if (handle_sudo_challenge(response, "POST", endpoint, "{}")) {
            return
        }
        exit 1
    }

    print GREEN "Service unlocked: " id RESET
}

function service_redeploy(id, bootstrap    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json) {
    get_api_keys()
    endpoint = "/services/" id "/redeploy"
    json = "{}"
    if (bootstrap != "") {
        json = "{\"bootstrap\":\"" escape_json(bootstrap) "\"}"
    }
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '" json "'"
    system(cmd " > /dev/null")
    print GREEN "Service redeployed: " id RESET
}

function service_logs(id, lines    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/services/" id "/logs"
    if (lines != "") {
        endpoint = endpoint "?lines=" lines
    }
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function service_execute(id, command    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json, line, response) {
    get_api_keys()
    endpoint = "/services/" id "/execute"
    json = "{\"command\":\"" escape_json(command) "\"}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '" json "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    print response
}

# Handle 428 Sudo OTP challenge - prompt user for OTP and retry
function handle_sudo_challenge(response, method, endpoint, body    , otp, challenge_id, timestamp, sig_headers, signature, sig_input, sig_cmd, cmd, retry_response, line, sudo_headers) {
    # Extract challenge_id from response
    challenge_id = ""
    if (match(response, /"challenge_id":"([^"]+)"/, arr)) {
        challenge_id = arr[1]
    }

    print YELLOW "Confirmation required. Check your email for a one-time code." RESET > "/dev/stderr"
    printf "Enter OTP: " > "/dev/stderr"

    # Read OTP from stdin
    if ((getline otp < "/dev/stdin") <= 0 || otp == "") {
        print RED "Error: Operation cancelled" RESET > "/dev/stderr"
        return 0
    }
    # Strip newline/carriage return
    gsub(/[\r\n]/, "", otp)

    if (otp == "") {
        print RED "Error: Operation cancelled" RESET > "/dev/stderr"
        return 0
    }

    # Retry with sudo headers
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":" method ":" endpoint ":" (body != "" ? body : "")
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    sudo_headers = "-H 'X-Sudo-OTP: " otp "' "
    if (challenge_id != "") {
        sudo_headers = sudo_headers "-H 'X-Sudo-Challenge: " challenge_id "' "
    }

    if (method == "DELETE") {
        cmd = "curl -s -w '\\n%{http_code}' -X DELETE '" API_BASE endpoint "' " \
              "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
              sig_headers sudo_headers
    } else {
        cmd = "curl -s -w '\\n%{http_code}' -X " method " '" API_BASE endpoint "' " \
              "-H 'Content-Type: application/json' " \
              "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
              sig_headers sudo_headers \
              (body != "" ? "-d '" body "'" : "")
    }

    retry_response = ""
    while ((cmd | getline line) > 0) {
        retry_response = retry_response line "\n"
    }
    close(cmd)

    # Check if successful (last line is HTTP code)
    if (match(retry_response, /\n([0-9]+)\n?$/, arr)) {
        if (arr[1] >= 200 && arr[1] < 300) {
            print GREEN "Operation completed successfully" RESET
            return 1
        }
    }

    print RED "Error: OTP verification failed" RESET > "/dev/stderr"
    return 0
}

function service_destroy(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/services/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -w '\\n%{http_code}' -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)

    # Extract HTTP code from last line
    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    # Handle 428 Precondition Required (sudo OTP needed)
    if (http_code == 428) {
        if (handle_sudo_challenge(response, "DELETE", endpoint, "")) {
            return
        }
        exit 1
    }

    if (http_code != 200) {
        print RED "Error: HTTP " http_code RESET > "/dev/stderr"
        print response > "/dev/stderr"
        exit 1
    }

    print GREEN "Service destroyed: " id RESET
}

function service_resize(id, vcpu    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, ram) {
    get_api_keys()
    endpoint = "/services/" id
    json = "{\"vcpu\":" vcpu "}"

    # Write to temp file
    tmp = "/tmp/un_awk_resize_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":PATCH:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    cmd = "curl -s -X PATCH '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    system(cmd " > /dev/null")

    # Clean up
    system("rm -f " tmp)

    ram = vcpu * 2
    print GREEN "Service resized to " vcpu " vCPU, " ram " GB RAM" RESET
}

function set_unfreeze_on_demand(id, enabled    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, enabled_str) {
    get_api_keys()
    endpoint = "/services/" id
    enabled_str = (enabled ? "true" : "false")
    json = "{\"unfreeze_on_demand\":" enabled_str "}"

    # Write to temp file
    tmp = "/tmp/un_awk_unfreeze_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":PATCH:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    cmd = "curl -s -X PATCH '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    system(cmd " > /dev/null")

    # Clean up
    system("rm -f " tmp)

    print GREEN "Service unfreeze_on_demand set to " enabled_str RESET
}

function service_dump_bootstrap(id, dump_file    , endpoint, json_body, timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    print "Fetching bootstrap script from " id "..." > "/dev/stderr"

    endpoint = "/services/" id "/execute"
    json_body = "{\"command\":\"cat /tmp/bootstrap.sh\"}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json_body
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Build the curl command to execute on the service
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '" json_body "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Parse stdout from response
    if (match(response, /"stdout":"([^"]*)"/, arr)) {
        stdout = arr[1]
        # Unescape JSON
        gsub(/\\n/, "\n", stdout)
        gsub(/\\t/, "\t", stdout)
        gsub(/\\"/, "\"", stdout)
        gsub(/\\\\/, "\\", stdout)

        if (dump_file != "") {
            # Write to file
            print stdout > dump_file
            close(dump_file)
            system("chmod 755 " dump_file)
            print "Bootstrap saved to " dump_file
        } else {
            # Print to stdout
            printf "%s", stdout
        }
    } else {
        print RED "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" RESET > "/dev/stderr"
        exit 1
    }
}

function read_and_base64(filepath    , cmd, b64) {
    cmd = "base64 -w0 '" filepath "' 2>/dev/null || base64 '" filepath "'"
    cmd | getline b64
    close(cmd)
    return b64
}

function build_input_files_json(files_str    , n, files, i, fname, b64, json) {
    if (files_str == "") return ""
    n = split(files_str, files, ",")
    json = ",\"input_files\":["
    for (i = 1; i <= n; i++) {
        fname = files[i]
        b64 = read_and_base64(fname)
        if (i > 1) json = json ","
        # Get just the basename for filename
        cmd = "basename '" fname "'"
        cmd | getline basename
        close(cmd)
        json = json "{\"filename\":\"" escape_json(basename) "\",\"content\":\"" b64 "\"}"
    }
    json = json "]"
    return json
}

function session_create(shell, network, vcpu, input_files    , json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response, input_files_json) {
    get_api_keys()

    # Build JSON payload
    json = "{\"shell\":\"" (shell != "" ? shell : "bash") "\""

    if (network != "") {
        json = json ",\"network\":\"" escape_json(network) "\""
    }

    if (vcpu != "") {
        json = json ",\"vcpu\":" vcpu
    }

    # Add input_files if provided
    input_files_json = build_input_files_json(input_files)
    if (input_files_json != "") {
        json = json input_files_json
    }

    json = json "}"

    # Write to temp file
    tmp = "/tmp/un_awk_sess_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:/sessions:" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE "/sessions' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    print YELLOW "Session created (WebSocket required)" RESET
    print response
}

function service_create(name, ports, domains, service_type, bootstrap, bootstrap_file, input_files, unfreeze_on_demand    , json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, boot_content, line, input_files_json, response) {
    get_api_keys()

    # Build JSON payload
    json = "{\"name\":\"" escape_json(name) "\""

    if (ports != "") {
        json = json ",\"ports\":[" ports "]"
    }

    if (domains != "") {
        # Split domains by comma and build array
        split(domains, domain_arr, ",")
        json = json ",\"domains\":["
        for (i in domain_arr) {
            if (i > 1) json = json ","
            json = json "\"" escape_json(domain_arr[i]) "\""
        }
        json = json "]"
    }

    if (service_type != "") {
        json = json ",\"service_type\":\"" escape_json(service_type) "\""
    }

    if (bootstrap != "") {
        json = json ",\"bootstrap\":\"" escape_json(bootstrap) "\""
    }

    if (bootstrap_file != "") {
        # Read file content
        boot_content = ""
        while ((getline line < bootstrap_file) > 0) {
            if (boot_content != "") boot_content = boot_content "\n"
            boot_content = boot_content line
        }
        close(bootstrap_file)

        if (boot_content == "") {
            print RED "Error: Bootstrap file not found or empty: " bootstrap_file RESET > "/dev/stderr"
            exit 1
        }

        json = json ",\"bootstrap_content\":\"" escape_json(boot_content) "\""
    }

    # Add input_files if provided
    input_files_json = build_input_files_json(input_files)
    if (input_files_json != "") {
        json = json input_files_json
    }

    # Add unfreeze_on_demand if provided
    if (unfreeze_on_demand != "") {
        json = json ",\"unfreeze_on_demand\":" (unfreeze_on_demand ? "true" : "false")
    }

    json = json "}"

    # Write to temp file
    tmp = "/tmp/un_awk_svc_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:/services:" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE "/services' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    # Extract service ID for auto-vault
    LAST_SERVICE_ID = ""
    if (match(response, /"id":"([^"]+)"/, arr)) {
        LAST_SERVICE_ID = arr[1]
    }

    # Print response
    print response
}

function validate_key(do_extend    , timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:/keys/validate:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl to validate key
    cmd = "curl -s -X POST '" PORTAL_BASE "/keys/validate' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Parse expired status (simple regex check)
    if (match(response, /"expired":true/)) {
        print RED "Expired" RESET

        # Extract public_key if present
        if (match(response, /"public_key":"([^"]+)"/, arr)) {
            public_key = arr[1]
            print "Public Key: " public_key
        }

        # Extract tier
        if (match(response, /"tier":"([^"]+)"/, arr)) {
            print "Tier: " arr[1]
        }

        # Extract expires_at
        if (match(response, /"expires_at":"([^"]+)"/, arr)) {
            print "Expired: " arr[1]
        }

        print YELLOW "To renew: Visit https://unsandbox.com/keys/extend" RESET

        if (do_extend && public_key) {
            url = PORTAL_BASE "/keys/extend?pk=" public_key
            print ""
            print BLUE "Opening browser to: " url RESET
            system("xdg-open '" url "' 2>/dev/null || open '" url "' 2>/dev/null &")
        }
        exit 1
    }

    # Valid key
    print GREEN "Valid" RESET

    # Extract and display fields
    if (match(response, /"public_key":"([^"]+)"/, arr)) {
        public_key = arr[1]
        print "Public Key: " public_key
    }
    if (match(response, /"tier":"([^"]+)"/, arr)) {
        print "Tier: " arr[1]
    }
    if (match(response, /"status":"([^"]+)"/, arr)) {
        print "Status: " arr[1]
    }
    if (match(response, /"expires_at":"([^"]+)"/, arr)) {
        print "Expires: " arr[1]
    }
    if (match(response, /"time_remaining":"([^"]+)"/, arr)) {
        print "Time Remaining: " arr[1]
    }
    if (match(response, /"rate_limit":"?([^",}]+)"?/, arr)) {
        print "Rate Limit: " arr[1]
    }
    if (match(response, /"burst":"?([^",}]+)"?/, arr)) {
        print "Burst: " arr[1]
    }
    if (match(response, /"concurrency":"?([^",}]+)"?/, arr)) {
        print "Concurrency: " arr[1]
    }

    if (do_extend && public_key) {
        url = PORTAL_BASE "/keys/extend?pk=" public_key
        print ""
        print BLUE "Opening browser to: " url RESET
        system("xdg-open '" url "' 2>/dev/null || open '" url "' 2>/dev/null &")
    }
}

# Alias for validate_key for API parity
function validate_keys() {
    validate_key(0)
}

function cmd_key(do_extend) {
    validate_key(do_extend)
}

function read_languages_cache(    cmd, line, cache_content, cache_timestamp, current_time) {
    # Check if cache file exists and is valid
    cmd = "cat '" LANGUAGES_CACHE_FILE "' 2>/dev/null"
    cache_content = ""
    while ((cmd | getline line) > 0) {
        cache_content = cache_content line
    }
    close(cmd)

    if (cache_content == "") return ""

    # Extract timestamp from cache
    if (match(cache_content, /"timestamp":([0-9]+)/, arr)) {
        cache_timestamp = arr[1]
        current_time = systime()
        # Check if cache is still valid (within TTL)
        if ((current_time - cache_timestamp) < LANGUAGES_CACHE_TTL) {
            return cache_content
        }
    }
    return ""
}

function write_languages_cache(languages_array    , cmd, cache_json, current_time) {
    current_time = systime()
    # Build cache JSON: {"languages": [...], "timestamp": unix_seconds}
    cache_json = "{\"languages\":" languages_array ",\"timestamp\":" current_time "}"

    # Ensure ~/.unsandbox directory exists
    system("mkdir -p \"" ENVIRON["HOME"] "/.unsandbox\"")

    # Write cache file
    cmd = "cat > '" LANGUAGES_CACHE_FILE "'"
    print cache_json | cmd
    close(cmd)
}

function languages_list(json_output    , timestamp, sig_headers, signature, sig_input, sig_cmd, line, response, i, lang, cache_content, languages_array, content, first, m) {
    # Try to read from cache first
    cache_content = read_languages_cache()
    if (cache_content != "") {
        # Extract languages array from cache
        if (match(cache_content, /"languages":(\[[^\]]*\])/, arr)) {
            languages_array = arr[1]
            if (json_output) {
                print languages_array
            } else {
                # Parse and print one per line
                content = languages_array
                gsub(/[\[\]"]/, "", content)
                n = split(content, langs, ",")
                for (i = 1; i <= n; i++) {
                    if (langs[i] != "") print langs[i]
                }
            }
            return
        }
    }

    # No valid cache, fetch from API
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/languages:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/languages' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Build languages array for caching
    languages_array = ""
    if (match(response, /"languages":\[([^\]]*)\]/, arr)) {
        # Parse out language names from the array
        content = arr[1]
        languages_array = "["
        first = 1
        while (match(content, /"name":"([^"]*)"/, m)) {
            if (!first) languages_array = languages_array ","
            languages_array = languages_array "\"" m[1] "\""
            first = 0
            content = substr(content, RSTART + RLENGTH)
        }
        languages_array = languages_array "]"

        # Save to cache
        write_languages_cache(languages_array)
    }

    if (json_output) {
        # Output raw JSON array of language names
        if (languages_array != "") {
            print languages_array
        } else {
            # Fallback: just print raw response
            print response
        }
    } else {
        # Output one language per line
        if (languages_array != "") {
            content = languages_array
            gsub(/[\[\]"]/, "", content)
            n = split(content, langs, ",")
            for (i = 1; i <= n; i++) {
                if (langs[i] != "") print langs[i]
            }
        }
    }
}

function snapshot_list(    timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/snapshots:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/snapshots' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function snapshot_info(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/snapshots/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function snapshot_delete(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/snapshots/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -w '\\n%{http_code}' -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)

    # Extract HTTP code from last line
    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    # Handle 428 Precondition Required (sudo OTP needed)
    if (http_code == 428) {
        if (handle_sudo_challenge(response, "DELETE", endpoint, "")) {
            return
        }
        exit 1
    }

    if (http_code != 200) {
        print RED "Error: HTTP " http_code RESET > "/dev/stderr"
        exit 1
    }

    print GREEN "Snapshot deleted: " id RESET
}

# Alias for snapshot_info
function snapshot_get(id) {
    snapshot_info(id)
}

function snapshot_lock(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/snapshots/" id "/lock"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers " -d '{}'"
    system(cmd " > /dev/null")
    print GREEN "Snapshot locked: " id RESET
}

function snapshot_unlock(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/snapshots/" id "/unlock"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":{}"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -w '\\n%{http_code}' -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '{}'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)

    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    if (http_code == 428) {
        if (handle_sudo_challenge(response, "POST", endpoint, "{}")) {
            return
        }
        exit 1
    }

    print GREEN "Snapshot unlocked: " id RESET
}

function snapshot_clone(id, clone_type, name    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json, line, response) {
    get_api_keys()
    endpoint = "/snapshots/" id "/clone"
    if (clone_type == "") clone_type = "session"
    json = "{\"clone_type\":\"" clone_type "\""
    if (name != "") {
        json = json ",\"name\":\"" escape_json(name) "\""
    }
    json = json "}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '" json "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    print GREEN "Snapshot cloned" RESET
    print response
}

# Image functions
function image_list(    timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/images:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/images' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function image_info(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/images/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function image_delete(id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/images/" id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -w '\\n%{http_code}' -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)

    # Extract HTTP code from last line
    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    # Handle 428 Precondition Required (sudo OTP needed)
    if (http_code == 428) {
        if (handle_sudo_challenge(response, "DELETE", endpoint, "")) {
            return
        }
        exit 1
    }

    if (http_code != 200) {
        print RED "Error: HTTP " http_code RESET > "/dev/stderr"
        exit 1
    }

    print GREEN "Image deleted: " id RESET
}

function image_lock(id    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    endpoint = "/images/" id "/lock"
    json = "{}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    system(cmd " > /dev/null")
    system("rm -f " tmp)
    print GREEN "Image locked: " id RESET
}

function image_unlock(id    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, cmd, response, line, http_code) {
    get_api_keys()
    endpoint = "/images/" id "/unlock"
    json = "{}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -w '\\n%{http_code}' -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line "\n"
    }
    close(cmd)
    system("rm -f " tmp)

    # Extract HTTP code from last line
    http_code = 0
    if (match(response, /\n([0-9]+)\n?$/, arr)) {
        http_code = arr[1]
    }

    # Handle 428 Precondition Required (sudo OTP needed)
    if (http_code == 428) {
        if (handle_sudo_challenge(response, "POST", endpoint, json)) {
            return
        }
        exit 1
    }

    if (http_code != 200) {
        print RED "Error: HTTP " http_code RESET > "/dev/stderr"
        exit 1
    }

    print GREEN "Image unlocked: " id RESET
}

function image_publish(source_id, source_type, name    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/images/publish"
    json = "{\"source_type\":\"" source_type "\",\"source_id\":\"" source_id "\""
    if (name != "") {
        json = json ",\"name\":\"" escape_json(name) "\""
    }
    json = json "}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print GREEN "Image published" RESET
    print response
}

function image_visibility(id, visibility    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    endpoint = "/images/" id "/visibility"
    json = "{\"visibility\":\"" visibility "\"}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    system(cmd " > /dev/null")
    system("rm -f " tmp)
    print GREEN "Image visibility set to: " visibility RESET
}

function image_spawn(id, name, ports    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/images/" id "/spawn"
    json = "{"
    if (name != "") {
        json = json "\"name\":\"" escape_json(name) "\""
    }
    if (ports != "") {
        if (name != "") json = json ","
        json = json "\"ports\":[" ports "]"
    }
    json = json "}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print GREEN "Service spawned from image" RESET
    print response
}

function image_clone(id, name    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/images/" id "/clone"
    json = "{"
    if (name != "") {
        json = json "\"name\":\"" escape_json(name) "\""
    }
    json = json "}"
    tmp = "/tmp/un_awk_img_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print GREEN "Image cloned" RESET
    print response
}

# Alias for image_visibility
function image_set_visibility(id, visibility) {
    image_visibility(id, visibility)
}

function image_grant_access(image_id, trusted_key    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json, line, response) {
    get_api_keys()
    endpoint = "/images/" image_id "/access"
    json = "{\"api_key\":\"" trusted_key "\"}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '" json "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    print GREEN "Access granted to " trusted_key RESET
}

function image_revoke_access(image_id, trusted_key    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/images/" image_id "/access/" trusted_key
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    system(cmd " > /dev/null")
    print GREEN "Access revoked from " trusted_key RESET
}

function image_list_trusted(image_id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/images/" image_id "/access"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function image_transfer(image_id, to_key    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint, json, line, response) {
    get_api_keys()
    endpoint = "/images/" image_id "/transfer"
    json = "{\"to_api_key\":\"" to_key "\"}"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '" json "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    print GREEN "Image transferred to " to_key RESET
}

# ============================================================================
# Job Functions (5)
# ============================================================================

function execute_async(language, code, network_mode    , json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    if (network_mode == "") network_mode = "zerotrust"
    json = "{\"language\":\"" language "\",\"code\":\"" escape_json(code) "\",\"network_mode\":\"" network_mode "\",\"ttl\":300}"
    tmp = "/tmp/un_awk_async_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:/execute/async:" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE "/execute/async' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '@" tmp "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print response
}

function get_job(job_id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/jobs/" job_id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function cancel_job(job_id    , timestamp, sig_headers, signature, sig_input, sig_cmd, endpoint) {
    get_api_keys()
    endpoint = "/jobs/" job_id
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    system(cmd " > /dev/null")
    print GREEN "Job cancelled: " job_id RESET
}

function list_jobs(    timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:/jobs:"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE "/jobs' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function wait_job(job_id    , delays, i, job_response, status, delay) {
    # Polling delays in milliseconds
    split("300 450 700 900 650 1600 2000", delays, " ")

    for (i = 0; i < 120; i++) {
        # Get job status
        job_response = ""
        get_api_keys()
        endpoint = "/jobs/" job_id
        timestamp = systime()
        sig_headers = ""
        if (GLOBAL_SECRET_KEY != "") {
            sig_input = timestamp ":GET:" endpoint ":"
            sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
            sig_cmd | getline signature
            close(sig_cmd)
            sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
        }
        cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
        while ((cmd | getline line) > 0) {
            job_response = job_response line
        }
        close(cmd)

        # Check status
        if (match(job_response, /"status":"([^"]+)"/, arr)) {
            status = arr[1]
            if (status == "completed") {
                print job_response
                return
            }
            if (status == "failed") {
                set_error("Job failed")
                print RED "Error: Job failed" RESET > "/dev/stderr"
                exit 1
            }
        }

        # Sleep with jitter
        delay = delays[(i % 7) + 1] / 1000
        cmd = "sleep " delay
        system(cmd)
    }

    set_error("Max polls exceeded")
    print RED "Error: Max polls exceeded" RESET > "/dev/stderr"
    exit 1
}

# Alias for get_languages
function get_languages(json_output) {
    languages_list(json_output)
}

# ============================================================================
# PaaS Logs Functions (2)
# ============================================================================

function logs_fetch(source, lines, since, grep_pattern    , timestamp, sig_headers, signature, sig_input, sig_cmd, json, tmp, line, response) {
    get_api_keys()
    if (source == "") source = "all"
    if (lines == "") lines = 100
    if (since == "") since = "1h"

    json = "{\"source\":\"" source "\",\"lines\":" lines ",\"since\":\"" since "\""
    if (grep_pattern != "") {
        json = json ",\"grep\":\"" escape_json(grep_pattern) "\""
    }
    json = json "}"

    tmp = "/tmp/un_awk_logs_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:/paas/logs:" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }
    cmd = "curl -s -X POST '" API_BASE "/paas/logs' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' -H 'Content-Type: application/json' " sig_headers "-d '@" tmp "'"
    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print response
}

function logs_stream() {
    set_error("logs_stream requires async support")
    print RED "Error: logs_stream requires async support" RESET > "/dev/stderr"
    exit 1
}

function session_snapshot(id, name, hot    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/sessions/" id "/snapshot"

    # Build JSON payload
    json = "{"
    if (name != "") {
        json = json "\"name\":\"" escape_json(name) "\""
        if (hot != "") json = json ","
    }
    if (hot != "") {
        json = json "\"hot\":" hot
    }
    json = json "}"

    # Write to temp file
    tmp = "/tmp/un_awk_snap_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    print GREEN "Snapshot created" RESET
    print response
}

function session_restore(snapshot_id    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    get_api_keys()
    endpoint = "/snapshots/" snapshot_id "/restore"

    json = "{}"

    # Write to temp file
    tmp = "/tmp/un_awk_restore_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    print GREEN "Session restored from snapshot" RESET
}

function service_snapshot(id, name, hot    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/services/" id "/snapshot"

    # Build JSON payload
    json = "{"
    if (name != "") {
        json = json "\"name\":\"" escape_json(name) "\""
        if (hot != "") json = json ","
    }
    if (hot != "") {
        json = json "\"hot\":" hot
    }
    json = json "}"

    # Write to temp file
    tmp = "/tmp/un_awk_snap_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    print GREEN "Snapshot created" RESET
    print response
}

function service_restore(snapshot_id    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    get_api_keys()
    endpoint = "/snapshots/" snapshot_id "/restore"

    json = "{}"

    # Write to temp file
    tmp = "/tmp/un_awk_restore_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    # Build HMAC signature if secret key exists
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    # Call curl
    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)

    # Clean up
    system("rm -f " tmp)

    print GREEN "Service restored from snapshot" RESET
}

# Build env content from env_vars array and env_file
function build_env_content(env_vars_str, env_file    , content, n, vars, i, line) {
    content = ""
    # Parse comma-separated env vars
    if (env_vars_str != "") {
        n = split(env_vars_str, vars, ",")
        for (i = 1; i <= n; i++) {
            if (content != "") content = content "\n"
            content = content vars[i]
        }
    }
    # Read env file if provided
    if (env_file != "") {
        while ((getline line < env_file) > 0) {
            # Skip empty lines and comments
            if (line ~ /^[[:space:]]*$/) continue
            if (line ~ /^[[:space:]]*#/) continue
            if (content != "") content = content "\n"
            content = content line
        }
        close(env_file)
    }
    return content
}

function service_env_status(id    , endpoint, timestamp, sig_headers, signature, sig_input, sig_cmd, line) {
    get_api_keys()
    endpoint = "/services/" id "/env"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":GET:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    while ((cmd | getline line) > 0) print line
    close(cmd)
}

function service_env_set(id, content    , endpoint, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/services/" id "/env"

    # Write content to temp file
    tmp = "/tmp/un_awk_env_" PROCINFO["pid"] ".txt"
    print content > tmp
    close(tmp)

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":PUT:" endpoint ":" content
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    cmd = "curl -s -X PUT '" API_BASE endpoint "' " \
          "-H 'Content-Type: text/plain' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "--data-binary '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)
    print response
}

function service_env_export(id    , endpoint, json, tmp, timestamp, sig_headers, signature, sig_input, sig_cmd, line, response) {
    get_api_keys()
    endpoint = "/services/" id "/env/export"
    json = "{}"

    tmp = "/tmp/un_awk_envexp_" PROCINFO["pid"] ".json"
    print json > tmp
    close(tmp)

    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":POST:" endpoint ":" json
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "' "
    }

    cmd = "curl -s -X POST '" API_BASE endpoint "' " \
          "-H 'Content-Type: application/json' " \
          "-H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " \
          sig_headers \
          "-d '@" tmp "'"

    response = ""
    while ((cmd | getline line) > 0) {
        response = response line
    }
    close(cmd)
    system("rm -f " tmp)

    # Extract content field from response
    if (match(response, /"content":"([^"]*)"/, arr)) {
        content = arr[1]
        gsub(/\\n/, "\n", content)
        printf "%s", content
    } else {
        print response
    }
}

function service_env_delete(id    , endpoint, timestamp, sig_headers, signature, sig_input, sig_cmd) {
    get_api_keys()
    endpoint = "/services/" id "/env"
    timestamp = systime()
    sig_headers = ""
    if (GLOBAL_SECRET_KEY != "") {
        sig_input = timestamp ":DELETE:" endpoint ":"
        sig_cmd = "echo -n '" sig_input "' | openssl dgst -sha256 -hmac '" GLOBAL_SECRET_KEY "' | sed 's/^.* //'"
        sig_cmd | getline signature
        close(sig_cmd)
        sig_headers = "-H 'X-Timestamp: " timestamp "' -H 'X-Signature: " signature "'"
    }
    cmd = "curl -s -X DELETE '" API_BASE endpoint "' -H 'Authorization: Bearer " GLOBAL_PUBLIC_KEY "' " sig_headers
    system(cmd)
    print GREEN "Vault deleted: " id RESET
}

function show_help() {
    print "Usage: awk -f un.awk <source_file>"
    print "       awk -f un.awk languages [--json]"
    print "       awk -f un.awk session --list"
    print "       awk -f un.awk session --kill ID"
    print "       awk -f un.awk session [-s SHELL] [-f FILE]..."
    print "       awk -f un.awk session --snapshot SESSION_ID [--snapshot-name NAME] [--hot]"
    print "       awk -f un.awk session --restore SNAPSHOT_ID"
    print "       awk -f un.awk key [--extend]"
    print "       awk -f un.awk service --list"
    print "       awk -f un.awk service --create --name NAME [--ports PORTS] [--domains DOMAINS] [--type TYPE] [--bootstrap CMD] [-e KEY=VAL] [--env-file FILE] [-f FILE]..."
    print "       awk -f un.awk service --destroy ID"
    print "       awk -f un.awk service --resize ID -v VCPU"
    print "       awk -f un.awk service --dump-bootstrap ID [--dump-file FILE]"
    print "       awk -f un.awk service --snapshot SERVICE_ID [--snapshot-name NAME] [--hot]"
    print "       awk -f un.awk service --restore SNAPSHOT_ID"
    print "       awk -f un.awk service env status ID"
    print "       awk -f un.awk service env set ID [-e KEY=VAL]... [--env-file FILE]"
    print "       awk -f un.awk service env export ID"
    print "       awk -f un.awk service env delete ID"
    print "       awk -f un.awk snapshot --list"
    print "       awk -f un.awk snapshot --info ID"
    print "       awk -f un.awk snapshot --delete ID"
    print "       awk -f un.awk image --list"
    print "       awk -f un.awk image --info ID"
    print "       awk -f un.awk image --delete ID"
    print "       awk -f un.awk image --lock ID"
    print "       awk -f un.awk image --unlock ID"
    print "       awk -f un.awk image --publish ID --source-type TYPE [--name NAME]"
    print "       awk -f un.awk image --visibility ID MODE"
    print "       awk -f un.awk image --spawn ID [--name NAME] [--ports PORTS]"
    print "       awk -f un.awk image --clone ID [--name NAME]"
    print ""
    print "Session options:"
    print "  -s, --shell SHELL      Shell to use (default: bash)"
    print "  -f FILE                Input file to upload (can be repeated)"
    print "  --snapshot SESSION_ID  Create snapshot of session"
    print "  --restore SNAPSHOT_ID  Restore from snapshot ID"
    print "  --snapshot-name N      Name for snapshot"
    print "  --hot                  Take snapshot without freezing (live snapshot)"
    print ""
    print "Service options:"
    print "  --name NAME        Service name (required for --create)"
    print "  --ports PORTS      Comma-separated port numbers"
    print "  --domains DOMAINS  Comma-separated domain names"
    print "  --type TYPE        Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)"
    print "  --bootstrap CMD    Bootstrap command or script"
    print "  --destroy ID       Destroy service"
    print "  --resize ID        Resize service (requires -v)"
    print "  --dump-bootstrap ID    Dump bootstrap script from service"
    print "  --dump-file FILE       Save bootstrap to file (with --dump-bootstrap)"
    print "  -e KEY=VAL             Environment variable for vault (can be repeated)"
    print "  --env-file FILE        Load env vars from file for vault"
    print "  -f FILE                Input file to upload (can be repeated)"
    print "  --snapshot SERVICE_ID  Create snapshot of service"
    print "  --restore SNAPSHOT_ID  Restore from snapshot ID"
    print "  --snapshot-name N      Name for snapshot"
    print "  --hot                  Take snapshot without freezing (live snapshot)"
    print ""
    print "Vault options (service env):"
    print "  status ID          Check vault status"
    print "  set ID             Set vault contents"
    print "  export ID          Export vault contents"
    print "  delete ID          Delete vault"
    print ""
    print "Languages options:"
    print "  --json             Output as JSON array (for scripts)"
    print ""
    print "Snapshot options:"
    print "  -l, --list         List all snapshots"
    print "  --info ID          Get snapshot details"
    print "  --delete ID        Delete a snapshot"
    print ""
    print "Image options:"
    print "  -l, --list         List all images"
    print "  --info ID          Get image details"
    print "  --delete ID        Delete an image"
    print "  --lock ID          Lock image to prevent deletion"
    print "  --unlock ID        Unlock image"
    print "  --publish ID       Publish image from service/snapshot (requires --source-type)"
    print "  --source-type TYPE Source type: service or snapshot"
    print "  --visibility ID MODE  Set visibility: private, unlisted, or public"
    print "  --spawn ID         Spawn new service from image"
    print "  --clone ID         Clone an image"
    print "  --name NAME        Name for spawned service or cloned image"
    print "  --ports PORTS      Ports for spawned service"
    print ""
    print "Requires: UNSANDBOX_API_KEY environment variable"
}

# Main logic
{
    # This block processes each input line from files passed as arguments
    # For our CLI, we process ARGV instead
}

END {
    if (ARGC < 2) {
        show_help()
        exit 0
    }

    if (ARGV[1] == "--help" || ARGV[1] == "-h") {
        show_help()
        exit 0
    }

    if (ARGV[1] == "session") {
        if (ARGC >= 3 && ARGV[2] == "--list") {
            session_list()
        } else if (ARGC >= 4 && ARGV[2] == "--kill") {
            session_kill(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--snapshot") {
            # Parse snapshot options
            snapshot_name = ""
            hot = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--snapshot-name" && i + 1 < ARGC) {
                    snapshot_name = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--hot") {
                    hot = "true"
                    i++
                } else {
                    i++
                }
            }
            session_snapshot(ARGV[3], snapshot_name, hot)
        } else if (ARGC >= 4 && ARGV[2] == "--restore") {
            # --restore takes snapshot ID directly
            session_restore(ARGV[3])
        } else {
            # Parse session creation arguments
            shell = ""
            network = ""
            vcpu = ""
            input_files = ""

            i = 2
            while (i < ARGC) {
                if ((ARGV[i] == "--shell" || ARGV[i] == "-s") && i + 1 < ARGC) {
                    shell = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "-n" && i + 1 < ARGC) {
                    network = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "-v" && i + 1 < ARGC) {
                    vcpu = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "-f" && i + 1 < ARGC) {
                    if (input_files != "") input_files = input_files ","
                    input_files = input_files ARGV[i + 1]
                    i += 2
                } else {
                    if (substr(ARGV[i], 1, 1) == "-") {
                        print "Unknown option: " ARGV[i] > "/dev/stderr"
                        usage()
                        exit 1
                    }
                    i++
                }
            }

            session_create(shell, network, vcpu, input_files)
        }
        exit 0
    }

    if (ARGV[1] == "key") {
        do_extend = 0
        if (ARGC >= 3 && ARGV[2] == "--extend") {
            do_extend = 1
        }
        cmd_key(do_extend)
        exit 0
    }

    if (ARGV[1] == "languages") {
        json_output = 0
        if (ARGC >= 3 && ARGV[2] == "--json") {
            json_output = 1
        }
        languages_list(json_output)
        exit 0
    }

    if (ARGV[1] == "snapshot") {
        if (ARGC >= 3 && (ARGV[2] == "--list" || ARGV[2] == "-l")) {
            snapshot_list()
        } else if (ARGC >= 4 && ARGV[2] == "--info") {
            snapshot_info(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--delete") {
            snapshot_delete(ARGV[3])
        } else {
            print "Usage: awk -f un.awk snapshot --list|--info ID|--delete ID"
        }
        exit 0
    }

    if (ARGV[1] == "image") {
        if (ARGC >= 3 && (ARGV[2] == "--list" || ARGV[2] == "-l")) {
            image_list()
        } else if (ARGC >= 4 && ARGV[2] == "--info") {
            image_info(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--delete") {
            image_delete(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--lock") {
            image_lock(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--unlock") {
            image_unlock(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--publish") {
            # Parse --source-type and --name options
            img_source_id = ARGV[3]
            img_source_type = ""
            img_name = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--source-type" && i + 1 < ARGC) {
                    img_source_type = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--name" && i + 1 < ARGC) {
                    img_name = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }
            if (img_source_type == "") {
                print RED "Error: --publish requires --source-type (service or snapshot)" RESET > "/dev/stderr"
                exit 1
            }
            image_publish(img_source_id, img_source_type, img_name)
        } else if (ARGC >= 5 && ARGV[2] == "--visibility") {
            image_visibility(ARGV[3], ARGV[4])
        } else if (ARGC >= 4 && ARGV[2] == "--spawn") {
            # Parse --name and --ports options
            img_id = ARGV[3]
            img_name = ""
            img_ports = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--name" && i + 1 < ARGC) {
                    img_name = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--ports" && i + 1 < ARGC) {
                    img_ports = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }
            image_spawn(img_id, img_name, img_ports)
        } else if (ARGC >= 4 && ARGV[2] == "--clone") {
            # Parse --name option
            img_id = ARGV[3]
            img_name = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--name" && i + 1 < ARGC) {
                    img_name = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }
            image_clone(img_id, img_name)
        } else {
            print "Usage: awk -f un.awk image --list|--info ID|--delete ID|--lock ID|--unlock ID"
            print "       awk -f un.awk image --publish ID --source-type TYPE [--name NAME]"
            print "       awk -f un.awk image --visibility ID MODE"
            print "       awk -f un.awk image --spawn ID [--name NAME] [--ports PORTS]"
            print "       awk -f un.awk image --clone ID [--name NAME]"
        }
        exit 0
    }

    if (ARGV[1] == "service") {
        if (ARGC >= 3 && ARGV[2] == "--list") {
            service_list()
        } else if (ARGC >= 4 && ARGV[2] == "--destroy") {
            service_destroy(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "--resize") {
            # Parse -v for vcpu
            resize_id = ARGV[3]
            resize_vcpu = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "-v" && i + 1 < ARGC) {
                    resize_vcpu = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }
            if (resize_vcpu == "") {
                print RED "Error: --vcpu (-v) is required with --resize" RESET > "/dev/stderr"
                exit 1
            }
            service_resize(resize_id, resize_vcpu)
        } else if (ARGC >= 4 && ARGV[2] == "--set-unfreeze-on-demand") {
            # Parse enabled flag
            set_uod_id = ARGV[3]
            set_uod_enabled = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--enabled" && i + 1 < ARGC) {
                    set_uod_enabled = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }
            if (set_uod_enabled == "") {
                print RED "Error: --enabled (true/false) is required with --set-unfreeze-on-demand" RESET > "/dev/stderr"
                exit 1
            }
            set_unfreeze_on_demand(set_uod_id, (set_uod_enabled == "true"))
        } else if (ARGC >= 4 && ARGV[2] == "--dump-bootstrap") {
            dump_file = ""
            if (ARGC >= 6 && ARGV[4] == "--dump-file") {
                dump_file = ARGV[5]
            }
            service_dump_bootstrap(ARGV[3], dump_file)
        } else if (ARGC >= 4 && ARGV[2] == "--snapshot") {
            # Parse snapshot options
            snapshot_name = ""
            hot = ""
            i = 4
            while (i < ARGC) {
                if (ARGV[i] == "--snapshot-name" && i + 1 < ARGC) {
                    snapshot_name = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--hot") {
                    hot = "true"
                    i++
                } else {
                    i++
                }
            }
            service_snapshot(ARGV[3], snapshot_name, hot)
        } else if (ARGC >= 4 && ARGV[2] == "--restore") {
            # --restore takes snapshot ID directly
            service_restore(ARGV[3])
        } else if (ARGC >= 4 && ARGV[2] == "env") {
            # Service vault commands: service env <action> <id> [options]
            env_action = ARGV[3]
            if (ARGC < 5) {
                print RED "Error: service env requires action and service ID" RESET > "/dev/stderr"
                print "Usage: awk -f un.awk service env <status|set|export|delete> <service_id> [options]" > "/dev/stderr"
                exit 1
            }
            env_service_id = ARGV[4]

            if (env_action == "status") {
                service_env_status(env_service_id)
            } else if (env_action == "set") {
                # Parse -e and --env-file options
                env_vars = ""
                env_file = ""
                i = 5
                while (i < ARGC) {
                    if (ARGV[i] == "-e" && i + 1 < ARGC) {
                        if (env_vars != "") env_vars = env_vars ","
                        env_vars = env_vars ARGV[i + 1]
                        i += 2
                    } else if (ARGV[i] == "--env-file" && i + 1 < ARGC) {
                        env_file = ARGV[i + 1]
                        i += 2
                    } else {
                        i++
                    }
                }
                env_content = build_env_content(env_vars, env_file)
                if (env_content == "") {
                    print RED "Error: No environment variables to set. Use -e KEY=VALUE or --env-file FILE" RESET > "/dev/stderr"
                    exit 1
                }
                service_env_set(env_service_id, env_content)
            } else if (env_action == "export") {
                service_env_export(env_service_id)
            } else if (env_action == "delete") {
                service_env_delete(env_service_id)
            } else {
                print RED "Unknown env action: " env_action RESET > "/dev/stderr"
                print "Usage: awk -f un.awk service env <status|set|export|delete> <service_id>" > "/dev/stderr"
                exit 1
            }
        } else if (ARGV[2] == "--create") {
            # Parse service creation arguments
            name = ""
            ports = ""
            domains = ""
            service_type = ""
            bootstrap = ""
            bootstrap_file = ""
            input_files = ""
            env_vars = ""
            env_file = ""

            i = 3
            while (i < ARGC) {
                if (ARGV[i] == "--name" && i + 1 < ARGC) {
                    name = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--ports" && i + 1 < ARGC) {
                    ports = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--domains" && i + 1 < ARGC) {
                    domains = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--type" && i + 1 < ARGC) {
                    service_type = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--bootstrap" && i + 1 < ARGC) {
                    bootstrap = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--bootstrap-file" && i + 1 < ARGC) {
                    bootstrap_file = ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "-f" && i + 1 < ARGC) {
                    if (input_files != "") input_files = input_files ","
                    input_files = input_files ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "-e" && i + 1 < ARGC) {
                    if (env_vars != "") env_vars = env_vars ","
                    env_vars = env_vars ARGV[i + 1]
                    i += 2
                } else if (ARGV[i] == "--env-file" && i + 1 < ARGC) {
                    env_file = ARGV[i + 1]
                    i += 2
                } else {
                    i++
                }
            }

            if (name == "") {
                print RED "Error: --name is required for service creation" RESET > "/dev/stderr"
                exit 1
            }

            service_create(name, ports, domains, service_type, bootstrap, bootstrap_file, input_files)

            # Auto-set vault if env vars were provided
            env_content = build_env_content(env_vars, env_file)
            if (env_content != "") {
                # Extract service ID from response (stored in LAST_SERVICE_ID global)
                if (LAST_SERVICE_ID != "") {
                    print YELLOW "Setting vault for service..." RESET
                    service_env_set(LAST_SERVICE_ID, env_content)
                }
            }
        } else {
            print "Usage: awk -f un.awk service --list|--create|--destroy ID"
        }
        exit 0
    }

    # Default: execute file
    execute(ARGV[1])
}
