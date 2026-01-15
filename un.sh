#!/bin/bash
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer.
# Learn more: https://www.permacomputer.com
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
#
# unsandbox SDK for Bash - Execute code in secure sandboxes
# https://unsandbox.com | https://api.unsandbox.com/openapi

API_BASE="https://api.unsandbox.com"

# Credential loading
load_accounts_csv() {
    local path="${1:-$HOME/.unsandbox/accounts.csv}"
    [ -f "$path" ] || return 1
    head -1 "$path"
}

get_credentials() {
    # Tier 1: Arguments
    [ -n "$PUBLIC_KEY" ] && [ -n "$SECRET_KEY" ] && echo "$PUBLIC_KEY:$SECRET_KEY" && return

    # Tier 2: Environment
    [ -n "$UNSANDBOX_PUBLIC_KEY" ] && [ -n "$UNSANDBOX_SECRET_KEY" ] && \
        echo "$UNSANDBOX_PUBLIC_KEY:$UNSANDBOX_SECRET_KEY" && return

    # Tier 3: Home directory
    local creds=$(load_accounts_csv "$HOME/.unsandbox/accounts.csv")
    [ -n "$creds" ] && echo "$creds" && return

    # Tier 4: Local directory
    creds=$(load_accounts_csv "./accounts.csv")
    [ -n "$creds" ] && echo "$creds" && return

    echo "No credentials found" >&2
    exit 1
}

# HMAC signature
sign_request() {
    local secret="$1"
    local timestamp="$2"
    local method="$3"
    local endpoint="$4"
    local body="$5"

    local message="$timestamp:$method:$endpoint:$body"
    echo -n "$message" | openssl dgst -sha256 -hmac "$secret" -hex | cut -d' ' -f2
}

# API request
api_request() {
    local method="$1"
    local endpoint="$2"
    local body="$3"

    local creds=$(get_credentials)
    local pk=$(echo "$creds" | cut -d: -f1)
    local sk=$(echo "$creds" | cut -d: -f2)

    local timestamp=$(date +%s)
    local body_str="${body:-{}}"
    local signature=$(sign_request "$sk" "$timestamp" "$method" "$endpoint" "$body_str")

    curl -s -X "$method" "$API_BASE$endpoint" \
        -H "Authorization: Bearer $pk" \
        -H "X-Timestamp: $timestamp" \
        -H "X-Signature: $signature" \
        -H "Content-Type: application/json" \
        -d "$body_str"
}

# Languages with cache
languages() {
    local cache_path="$HOME/.unsandbox/languages.json"
    local cache_ttl=3600

    if [ -f "$cache_path" ]; then
        local age=$(($(date +%s) - $(stat -f%m "$cache_path" 2>/dev/null || stat -c%Y "$cache_path" 2>/dev/null || echo 0)))
        [ "$age" -lt "$cache_ttl" ] && cat "$cache_path" && return
    fi

    local result=$(api_request "GET" "/languages" "")
    mkdir -p "$HOME/.unsandbox"
    echo "$result" | jq '.languages' > "$cache_path"
    echo "$result" | jq '.languages'
}

# Execute functions
execute() {
    local language="$1"
    local code="$2"

    local body=$(cat <<EOF
{
    "language": "$language",
    "code": $(echo "$code" | jq -Rs .),
    "network_mode": "zerotrust",
    "ttl": 60
}
EOF
)
    api_request "POST" "/execute" "$body"
}

execute_async() {
    local language="$1"
    local code="$2"

    local body=$(cat <<EOF
{
    "language": "$language",
    "code": $(echo "$code" | jq -Rs .),
    "network_mode": "zerotrust",
    "ttl": 300
}
EOF
)
    api_request "POST" "/execute/async" "$body"
}

run() {
    local file="$1"
    local code=$(cat "$file")
    local lang=$(detect_language "$file")
    execute "$lang" "$code"
}

# Job management
get_job() {
    local job_id="$1"
    api_request "GET" "/jobs/$job_id" ""
}

wait_job() {
    local job_id="$1"
    local delays=(300 450 700 900 650 1600 2000)

    for i in $(seq 0 119); do
        local job=$(get_job "$job_id")
        local status=$(echo "$job" | jq -r '.status')

        [ "$status" = "completed" ] && echo "$job" && return 0
        [ "$status" = "failed" ] && exit 1

        local delay=${delays[$((i % 7))]}
        sleep $((delay / 1000))
    done

    echo "Max polls exceeded" >&2
    exit 1
}

# Utilities
detect_language() {
    local file="$1"
    case "$file" in
        *.py) echo "python" ;;
        *.sh) echo "bash" ;;
        *.rb) echo "ruby" ;;
        *.js) echo "javascript" ;;
        *) echo "Unknown file type" >&2; exit 1 ;;
    esac
}

# CLI
if [ $# -gt 0 ]; then
    result=$(run "$1")
    echo "$result" | jq -r '.stdout // empty'
    echo "$result" | jq -r '.stderr // empty' >&2
    exit "$(echo "$result" | jq -r '.exit_code // 0')"
else
    echo "Usage: bash un.sh <file>" >&2
    exit 1
fi
