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
    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file" >&2
        exit 1
    fi
    local code=$(cat "$file")
    local lang=$(detect_language "$file")
    execute "$lang" "$code"
}

# Service toggle functions
set_unfreeze_on_demand() {
    local service_id="$1"
    local enabled="$2"
    local body="{\"unfreeze_on_demand\":$enabled}"
    api_request "PATCH" "/services/$service_id" "$body"
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
        *.ts) echo "typescript" ;;
        *.go) echo "go" ;;
        *.rs) echo "rust" ;;
        *.c) echo "c" ;;
        *.cpp|*.cc|*.cxx) echo "cpp" ;;
        *.java) echo "java" ;;
        *.kt) echo "kotlin" ;;
        *.php) echo "php" ;;
        *.pl) echo "perl" ;;
        *.lua) echo "lua" ;;
        *.r|*.R) echo "r" ;;
        *.jl) echo "julia" ;;
        *.hs) echo "haskell" ;;
        *.ml) echo "ocaml" ;;
        *.ex|*.exs) echo "elixir" ;;
        *.erl) echo "erlang" ;;
        *.clj) echo "clojure" ;;
        *.scm) echo "scheme" ;;
        *.lisp) echo "commonlisp" ;;
        *.cs) echo "csharp" ;;
        *.fs) echo "fsharp" ;;
        *.d) echo "d" ;;
        *.nim) echo "nim" ;;
        *.zig) echo "zig" ;;
        *.v) echo "v" ;;
        *.cr) echo "crystal" ;;
        *.dart) echo "dart" ;;
        *.groovy) echo "groovy" ;;
        *.f90|*.f95) echo "fortran" ;;
        *.cob) echo "cobol" ;;
        *.tcl) echo "tcl" ;;
        *.raku) echo "raku" ;;
        *.pro) echo "prolog" ;;
        *.forth|*.4th) echo "forth" ;;
        *.m) echo "objc" ;;
        *) echo "Error: Cannot detect language for $file" >&2; exit 1 ;;
    esac
}

# Languages command
cmd_languages() {
    local json_output=0

    # Parse arguments
    for arg in "$@"; do
        if [ "$arg" = "--json" ]; then
            json_output=1
        fi
    done

    local result=$(languages)

    if [ "$json_output" -eq 1 ]; then
        # JSON array output
        echo "$result" | jq -c '.'
    else
        # One language per line (default)
        echo "$result" | jq -r '.[]'
    fi
}

# Image command
cmd_image() {
    local action=""
    local id=""
    local source_type=""
    local visibility_mode=""
    local name=""
    local ports=""

    while [ $# -gt 0 ]; do
        case "$1" in
            --list|-l)
                action="list"
                shift
                ;;
            --info)
                action="info"
                id="$2"
                shift 2
                ;;
            --delete)
                action="delete"
                id="$2"
                shift 2
                ;;
            --lock)
                action="lock"
                id="$2"
                shift 2
                ;;
            --unlock)
                action="unlock"
                id="$2"
                shift 2
                ;;
            --publish)
                action="publish"
                id="$2"
                shift 2
                ;;
            --source-type)
                source_type="$2"
                shift 2
                ;;
            --visibility)
                action="visibility"
                id="$2"
                visibility_mode="$3"
                shift 3
                ;;
            --spawn)
                action="spawn"
                id="$2"
                shift 2
                ;;
            --clone)
                action="clone"
                id="$2"
                shift 2
                ;;
            --name)
                name="$2"
                shift 2
                ;;
            --ports)
                ports="$2"
                shift 2
                ;;
            *)
                echo "Unknown option: $1" >&2
                exit 1
                ;;
        esac
    done

    case "$action" in
        list)
            result=$(api_request "GET" "/images" "")
            echo "$result" | jq -r '.images[] | "\(.id)\t\(.name // "-")\t\(.visibility)\t\(.created_at)"' 2>/dev/null || echo "No images found"
            ;;
        info)
            result=$(api_request "GET" "/images/$id" "")
            echo "$result" | jq .
            ;;
        delete)
            api_request "DELETE" "/images/$id" ""
            echo "Image deleted successfully"
            ;;
        lock)
            api_request "POST" "/images/$id/lock" "{}"
            echo "Image locked successfully"
            ;;
        unlock)
            api_request "POST" "/images/$id/unlock" "{}"
            echo "Image unlocked successfully"
            ;;
        publish)
            if [ -z "$source_type" ]; then
                echo "Error: --source-type required for --publish" >&2
                exit 1
            fi
            local body="{\"source_type\":\"$source_type\",\"source_id\":\"$id\""
            if [ -n "$name" ]; then
                body="$body,\"name\":\"$name\""
            fi
            body="$body}"
            result=$(api_request "POST" "/images/publish" "$body")
            echo "Image published successfully"
            echo "$result" | jq -r '"Image ID: \(.id)"'
            ;;
        visibility)
            if [ -z "$visibility_mode" ]; then
                echo "Error: visibility mode required" >&2
                exit 1
            fi
            api_request "POST" "/images/$id/visibility" "{\"visibility\":\"$visibility_mode\"}"
            echo "Image visibility set to $visibility_mode"
            ;;
        spawn)
            local body="{"
            local first=1
            if [ -n "$name" ]; then
                body="$body\"name\":\"$name\""
                first=0
            fi
            if [ -n "$ports" ]; then
                if [ "$first" -eq 0 ]; then
                    body="$body,"
                fi
                body="$body\"ports\":[$ports]"
            fi
            body="$body}"
            result=$(api_request "POST" "/images/$id/spawn" "$body")
            echo "Service spawned from image"
            echo "$result" | jq -r '"Service ID: \(.id)"'
            ;;
        clone)
            local body="{"
            if [ -n "$name" ]; then
                body="$body\"name\":\"$name\""
            fi
            body="$body}"
            result=$(api_request "POST" "/images/$id/clone" "$body")
            echo "Image cloned successfully"
            echo "$result" | jq -r '"Image ID: \(.id)"'
            ;;
        *)
            echo "Error: Specify --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID" >&2
            exit 1
            ;;
    esac
}

# CLI
if [ $# -gt 0 ]; then
    case "$1" in
        languages)
            shift
            cmd_languages "$@"
            ;;
        image)
            shift
            cmd_image "$@"
            ;;
        *)
            result=$(run "$1")
            if [ -z "$result" ] || ! echo "$result" | jq -e . >/dev/null 2>&1; then
                echo "Error: Failed to execute $1" >&2
                exit 1
            fi
            echo "$result" | jq -r '.stdout // empty'
            echo "$result" | jq -r '.stderr // empty' >&2
            exit_code=$(echo "$result" | jq -r '.exit_code // 0')
            exit "${exit_code:-0}"
            ;;
    esac
else
    echo "Usage: bash un.sh <file>" >&2
    echo "       bash un.sh languages [--json]" >&2
    echo "       bash un.sh image [options]" >&2
    echo "" >&2
    echo "Languages options:" >&2
    echo "  --json              Output as JSON array" >&2
    echo "" >&2
    echo "Image options:" >&2
    echo "  --list              List all images" >&2
    echo "  --info ID           Get image details" >&2
    echo "  --delete ID         Delete an image" >&2
    echo "  --lock ID           Lock image to prevent deletion" >&2
    echo "  --unlock ID         Unlock image" >&2
    echo "  --publish ID        Publish image from service/snapshot" >&2
    echo "  --source-type TYPE  Source type: service or snapshot" >&2
    echo "  --visibility ID MODE  Set visibility: private, unlisted, public" >&2
    echo "  --spawn ID          Spawn new service from image" >&2
    echo "  --clone ID          Clone an image" >&2
    echo "  --name NAME         Name for spawned service or cloned image" >&2
    echo "  --ports PORTS       Ports for spawned service" >&2
    exit 1
fi
