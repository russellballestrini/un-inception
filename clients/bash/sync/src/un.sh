#!/bin/bash
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# unsandbox.com Bash SDK (Synchronous)
# Full API with execution, sessions, services, snapshots, and images.
#
# Library Usage:
#     source un.sh
#     result=$(execute "python" "print(42)")
#     echo "$result" | jq -r '.stdout'
#
# CLI Usage:
#     bash un.sh script.py
#     bash un.sh -s python 'print(42)'
#     bash un.sh session --list
#     bash un.sh service --list
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf

set -euo pipefail

VERSION="4.2.50"
API_BASE="https://api.unsandbox.com"
PORTAL_BASE="https://unsandbox.com"
LAST_ERROR=""

# Colors
BLUE='\033[34m'
RED='\033[31m'
GREEN='\033[32m'
YELLOW='\033[33m'
RESET='\033[0m'

# ============================================================================
# Utility Functions
# ============================================================================

version() {
    echo "$VERSION"
}

last_error() {
    echo "$LAST_ERROR"
}

set_error() {
    LAST_ERROR="$1"
}

detect_language() {
    local file="$1"
    case "$file" in
        *.py) echo "python" ;;
        *.js) echo "javascript" ;;
        *.ts) echo "typescript" ;;
        *.rb) echo "ruby" ;;
        *.php) echo "php" ;;
        *.pl) echo "perl" ;;
        *.lua) echo "lua" ;;
        *.sh) echo "bash" ;;
        *.go) echo "go" ;;
        *.rs) echo "rust" ;;
        *.c) echo "c" ;;
        *.cpp|*.cc|*.cxx) echo "cpp" ;;
        *.java) echo "java" ;;
        *.kt) echo "kotlin" ;;
        *.cs) echo "csharp" ;;
        *.fs) echo "fsharp" ;;
        *.hs) echo "haskell" ;;
        *.ml) echo "ocaml" ;;
        *.clj) echo "clojure" ;;
        *.scm) echo "scheme" ;;
        *.lisp) echo "commonlisp" ;;
        *.erl) echo "erlang" ;;
        *.ex|*.exs) echo "elixir" ;;
        *.jl) echo "julia" ;;
        *.r|*.R) echo "r" ;;
        *.cr) echo "crystal" ;;
        *.d) echo "d" ;;
        *.nim) echo "nim" ;;
        *.zig) echo "zig" ;;
        *.v) echo "v" ;;
        *.dart) echo "dart" ;;
        *.groovy) echo "groovy" ;;
        *.scala) echo "scala" ;;
        *.f90|*.f95) echo "fortran" ;;
        *.cob) echo "cobol" ;;
        *.tcl) echo "tcl" ;;
        *.raku) echo "raku" ;;
        *.pro) echo "prolog" ;;
        *.forth|*.4th) echo "forth" ;;
        *.m) echo "objc" ;;
        *) return 1 ;;
    esac
}

hmac_sign() {
    local secret="$1"
    local message="$2"
    echo -n "$message" | openssl dgst -sha256 -hmac "$secret" -hex 2>/dev/null | sed 's/^.* //'
}

# ============================================================================
# Credential Management
# ============================================================================

load_accounts_csv() {
    local path="${1:-$HOME/.unsandbox/accounts.csv}"
    [ -f "$path" ] || return 1
    head -1 "$path" 2>/dev/null | grep -v '^#'
}

get_credentials() {
    # Tier 1: Arguments (via PUBLIC_KEY/SECRET_KEY globals)
    if [ -n "${PUBLIC_KEY:-}" ] && [ -n "${SECRET_KEY:-}" ]; then
        echo "$PUBLIC_KEY:$SECRET_KEY"
        return
    fi

    # Tier 2: Environment
    if [ -n "${UNSANDBOX_PUBLIC_KEY:-}" ] && [ -n "${UNSANDBOX_SECRET_KEY:-}" ]; then
        echo "$UNSANDBOX_PUBLIC_KEY:$UNSANDBOX_SECRET_KEY"
        return
    fi

    # Legacy fallback
    if [ -n "${UNSANDBOX_API_KEY:-}" ]; then
        echo "$UNSANDBOX_API_KEY:"
        return
    fi

    # Tier 3: Home directory
    local creds
    creds=$(load_accounts_csv "$HOME/.unsandbox/accounts.csv" 2>/dev/null || true)
    if [ -n "$creds" ]; then
        echo "$creds"
        return
    fi

    # Tier 4: Local directory
    creds=$(load_accounts_csv "./accounts.csv" 2>/dev/null || true)
    if [ -n "$creds" ]; then
        echo "$creds"
        return
    fi

    set_error "No credentials found"
    return 1
}

# ============================================================================
# API Communication
# ============================================================================

api_request() {
    local method="$1"
    local endpoint="$2"
    local body="${3:-}"
    local extra_headers="${4:-}"
    local content_type="${5:-application/json}"

    local creds
    creds=$(get_credentials) || return 1
    local pk="${creds%%:*}"
    local sk="${creds#*:}"

    local timestamp
    timestamp=$(date +%s)
    local body_str="${body:-}"

    local signature=""
    if [ -n "$sk" ]; then
        signature=$(hmac_sign "$sk" "$timestamp:$method:$endpoint:$body_str")
    fi

    local curl_args=(-s -X "$method" "$API_BASE$endpoint"
        -H "Authorization: Bearer $pk"
        -H "Content-Type: $content_type")

    if [ -n "$signature" ]; then
        curl_args+=(-H "X-Timestamp: $timestamp" -H "X-Signature: $signature")
    fi

    if [ -n "$extra_headers" ]; then
        eval "curl_args+=($extra_headers)"
    fi

    if [ -n "$body_str" ]; then
        curl_args+=(-d "$body_str")
    fi

    curl "${curl_args[@]}"
}

api_request_with_sudo() {
    local method="$1"
    local endpoint="$2"
    local body="${3:-}"

    local creds
    creds=$(get_credentials) || return 1
    local pk="${creds%%:*}"
    local sk="${creds#*:}"

    local timestamp
    timestamp=$(date +%s)
    local body_str="${body:-}"

    local signature=""
    if [ -n "$sk" ]; then
        signature=$(hmac_sign "$sk" "$timestamp:$method:$endpoint:$body_str")
    fi

    local result
    result=$(curl -s -w '\n%{http_code}' -X "$method" "$API_BASE$endpoint" \
        -H "Authorization: Bearer $pk" \
        -H "Content-Type: application/json" \
        ${signature:+-H "X-Timestamp: $timestamp" -H "X-Signature: $signature"} \
        ${body_str:+-d "$body_str"})

    local http_code
    http_code=$(echo "$result" | tail -1)
    local response_body
    response_body=$(echo "$result" | sed '$d')

    # Handle 428 - Sudo OTP required
    if [ "$http_code" = "428" ]; then
        local challenge_id
        challenge_id=$(echo "$response_body" | jq -r '.challenge_id // empty' 2>/dev/null || true)

        echo -e "${YELLOW}Confirmation required. Check your email for a one-time code.${RESET}" >&2
        echo -n "Enter OTP: " >&2
        read -r otp

        if [ -z "$otp" ]; then
            set_error "Operation cancelled"
            return 1
        fi

        # Retry with sudo headers
        timestamp=$(date +%s)
        if [ -n "$sk" ]; then
            signature=$(hmac_sign "$sk" "$timestamp:$method:$endpoint:$body_str")
        fi

        result=$(curl -s -w '\n%{http_code}' -X "$method" "$API_BASE$endpoint" \
            -H "Authorization: Bearer $pk" \
            -H "Content-Type: application/json" \
            ${signature:+-H "X-Timestamp: $timestamp" -H "X-Signature: $signature"} \
            -H "X-Sudo-OTP: $otp" \
            ${challenge_id:+-H "X-Sudo-Challenge: $challenge_id"} \
            ${body_str:+-d "$body_str"})

        http_code=$(echo "$result" | tail -1)
        response_body=$(echo "$result" | sed '$d')
    fi

    if [ "$http_code" -lt 200 ] || [ "$http_code" -ge 300 ]; then
        set_error "API error ($http_code)"
        return 1
    fi

    echo "$response_body"
}

# ============================================================================
# Execution Functions (8)
# ============================================================================

execute() {
    local language="$1"
    local code="$2"
    local network_mode="${3:-zerotrust}"

    local body
    body=$(jq -n --arg lang "$language" --arg code "$code" --arg net "$network_mode" \
        '{language: $lang, code: $code, network_mode: $net, ttl: 60}')

    api_request "POST" "/execute" "$body"
}

execute_async() {
    local language="$1"
    local code="$2"
    local network_mode="${3:-zerotrust}"

    local body
    body=$(jq -n --arg lang "$language" --arg code "$code" --arg net "$network_mode" \
        '{language: $lang, code: $code, network_mode: $net, ttl: 300}')

    api_request "POST" "/execute/async" "$body"
}

wait_job() {
    local job_id="$1"
    local delays=(300 450 700 900 650 1600 2000)

    for i in $(seq 0 119); do
        local job
        job=$(get_job "$job_id")
        local status
        status=$(echo "$job" | jq -r '.status')

        [ "$status" = "completed" ] && echo "$job" && return 0
        [ "$status" = "failed" ] && { set_error "Job failed"; return 1; }

        local delay=${delays[$((i % 7))]}
        sleep "$(echo "scale=3; $delay/1000" | bc)"
    done

    set_error "Max polls exceeded"
    return 1
}

get_job() {
    local job_id="$1"
    api_request "GET" "/jobs/$job_id" ""
}

cancel_job() {
    local job_id="$1"
    api_request "DELETE" "/jobs/$job_id" ""
}

list_jobs() {
    api_request "GET" "/jobs" ""
}

get_languages() {
    local cache_path="$HOME/.unsandbox/languages.json"
    local cache_ttl=3600

    if [ -f "$cache_path" ]; then
        local age
        age=$(($(date +%s) - $(stat -f%m "$cache_path" 2>/dev/null || stat -c%Y "$cache_path" 2>/dev/null || echo 0)))
        if [ "$age" -lt "$cache_ttl" ]; then
            cat "$cache_path"
            return
        fi
    fi

    local result
    result=$(api_request "GET" "/languages" "")
    mkdir -p "$HOME/.unsandbox"
    echo "$result" | jq '.languages' > "$cache_path"
    echo "$result" | jq '.languages'
}

# ============================================================================
# Session Functions (9)
# ============================================================================

session_list() {
    api_request "GET" "/sessions" ""
}

session_get() {
    local session_id="$1"
    api_request "GET" "/sessions/$session_id" ""
}

session_create() {
    local shell="${1:-bash}"
    local network="${2:-}"
    local vcpu="${3:-}"

    local body
    body=$(jq -n --arg shell "$shell" '{shell: $shell}')

    if [ -n "$network" ]; then
        body=$(echo "$body" | jq --arg net "$network" '. + {network: $net}')
    fi
    if [ -n "$vcpu" ]; then
        body=$(echo "$body" | jq --argjson vcpu "$vcpu" '. + {vcpu: $vcpu}')
    fi

    api_request "POST" "/sessions" "$body"
}

session_destroy() {
    local session_id="$1"
    api_request "DELETE" "/sessions/$session_id" ""
}

session_freeze() {
    local session_id="$1"
    api_request "POST" "/sessions/$session_id/freeze" "{}"
}

session_unfreeze() {
    local session_id="$1"
    api_request "POST" "/sessions/$session_id/unfreeze" "{}"
}

session_boost() {
    local session_id="$1"
    local vcpu="${2:-2}"
    api_request "POST" "/sessions/$session_id/boost" "{\"vcpu\":$vcpu}"
}

session_unboost() {
    local session_id="$1"
    api_request "POST" "/sessions/$session_id/unboost" "{}"
}

session_execute() {
    local session_id="$1"
    local command="$2"
    api_request "POST" "/sessions/$session_id/execute" "{\"command\":$(echo "$command" | jq -Rs .)}"
}

# ============================================================================
# Service Functions (17)
# ============================================================================

service_list() {
    api_request "GET" "/services" ""
}

service_get() {
    local service_id="$1"
    api_request "GET" "/services/$service_id" ""
}

service_create() {
    local name="$1"
    local ports="${2:-}"
    local bootstrap="${3:-}"

    local body
    body=$(jq -n --arg name "$name" '{name: $name}')

    if [ -n "$ports" ]; then
        body=$(echo "$body" | jq --argjson ports "[$ports]" '. + {ports: $ports}')
    fi
    if [ -n "$bootstrap" ]; then
        body=$(echo "$body" | jq --arg boot "$bootstrap" '. + {bootstrap: $boot}')
    fi

    api_request "POST" "/services" "$body"
}

service_destroy() {
    local service_id="$1"
    api_request_with_sudo "DELETE" "/services/$service_id" ""
}

service_freeze() {
    local service_id="$1"
    api_request "POST" "/services/$service_id/freeze" "{}"
}

service_unfreeze() {
    local service_id="$1"
    api_request "POST" "/services/$service_id/unfreeze" "{}"
}

service_lock() {
    local service_id="$1"
    api_request "POST" "/services/$service_id/lock" "{}"
}

service_unlock() {
    local service_id="$1"
    api_request_with_sudo "POST" "/services/$service_id/unlock" "{}"
}

service_set_unfreeze_on_demand() {
    local service_id="$1"
    local enabled="$2"
    api_request "PATCH" "/services/$service_id" "{\"unfreeze_on_demand\":$enabled}"
}

service_redeploy() {
    local service_id="$1"
    local bootstrap="${2:-}"
    local body="{}"
    if [ -n "$bootstrap" ]; then
        body=$(jq -n --arg boot "$bootstrap" '{bootstrap: $boot}')
    fi
    api_request "POST" "/services/$service_id/redeploy" "$body"
}

service_logs() {
    local service_id="$1"
    local lines="${2:-}"
    local endpoint="/services/$service_id/logs"
    [ -n "$lines" ] && endpoint="$endpoint?lines=$lines"
    api_request "GET" "$endpoint" ""
}

service_execute() {
    local service_id="$1"
    local command="$2"
    api_request "POST" "/services/$service_id/execute" "{\"command\":$(echo "$command" | jq -Rs .)}"
}

service_env_get() {
    local service_id="$1"
    api_request "GET" "/services/$service_id/env" ""
}

service_env_set() {
    local service_id="$1"
    local env_content="$2"
    api_request "PUT" "/services/$service_id/env" "$env_content" "" "text/plain"
}

service_env_delete() {
    local service_id="$1"
    api_request "DELETE" "/services/$service_id/env" ""
}

service_env_export() {
    local service_id="$1"
    api_request "POST" "/services/$service_id/env/export" "{}"
}

service_resize() {
    local service_id="$1"
    local vcpu="$2"
    api_request "PATCH" "/services/$service_id" "{\"vcpu\":$vcpu}"
}

# ============================================================================
# Snapshot Functions (9)
# ============================================================================

snapshot_list() {
    api_request "GET" "/snapshots" ""
}

snapshot_get() {
    local snapshot_id="$1"
    api_request "GET" "/snapshots/$snapshot_id" ""
}

snapshot_session() {
    local session_id="$1"
    local name="${2:-}"
    local hot="${3:-false}"

    local body="{}"
    if [ -n "$name" ] || [ "$hot" = "true" ]; then
        body=$(jq -n --arg name "$name" --argjson hot "$hot" \
            '{name: (if $name != "" then $name else null end), hot: $hot}')
    fi

    api_request "POST" "/sessions/$session_id/snapshot" "$body"
}

snapshot_service() {
    local service_id="$1"
    local name="${2:-}"
    local hot="${3:-false}"

    local body="{}"
    if [ -n "$name" ] || [ "$hot" = "true" ]; then
        body=$(jq -n --arg name "$name" --argjson hot "$hot" \
            '{name: (if $name != "" then $name else null end), hot: $hot}')
    fi

    api_request "POST" "/services/$service_id/snapshot" "$body"
}

snapshot_restore() {
    local snapshot_id="$1"
    api_request "POST" "/snapshots/$snapshot_id/restore" "{}"
}

snapshot_delete() {
    local snapshot_id="$1"
    api_request_with_sudo "DELETE" "/snapshots/$snapshot_id" ""
}

snapshot_lock() {
    local snapshot_id="$1"
    api_request "POST" "/snapshots/$snapshot_id/lock" "{}"
}

snapshot_unlock() {
    local snapshot_id="$1"
    api_request_with_sudo "POST" "/snapshots/$snapshot_id/unlock" "{}"
}

snapshot_clone() {
    local snapshot_id="$1"
    local clone_type="${2:-session}"
    local name="${3:-}"

    local body
    body=$(jq -n --arg type "$clone_type" --arg name "$name" \
        '{clone_type: $type, name: (if $name != "" then $name else null end)}')

    api_request "POST" "/snapshots/$snapshot_id/clone" "$body"
}

# ============================================================================
# Image Functions (13)
# ============================================================================

image_list() {
    local filter="${1:-}"
    local endpoint="/images"
    [ -n "$filter" ] && endpoint="$endpoint?filter=$filter"
    api_request "GET" "$endpoint" ""
}

image_get() {
    local image_id="$1"
    api_request "GET" "/images/$image_id" ""
}

image_publish() {
    local source_type="$1"
    local source_id="$2"
    local name="${3:-}"

    local body
    body=$(jq -n --arg type "$source_type" --arg id "$source_id" --arg name "$name" \
        '{source_type: $type, source_id: $id, name: (if $name != "" then $name else null end)}')

    api_request "POST" "/images/publish" "$body"
}

image_delete() {
    local image_id="$1"
    api_request_with_sudo "DELETE" "/images/$image_id" ""
}

image_lock() {
    local image_id="$1"
    api_request "POST" "/images/$image_id/lock" "{}"
}

image_unlock() {
    local image_id="$1"
    api_request_with_sudo "POST" "/images/$image_id/unlock" "{}"
}

image_set_visibility() {
    local image_id="$1"
    local visibility="$2"
    api_request "POST" "/images/$image_id/visibility" "{\"visibility\":\"$visibility\"}"
}

image_grant_access() {
    local image_id="$1"
    local trusted_key="$2"
    api_request "POST" "/images/$image_id/access" "{\"api_key\":\"$trusted_key\"}"
}

image_revoke_access() {
    local image_id="$1"
    local trusted_key="$2"
    api_request "DELETE" "/images/$image_id/access/$trusted_key" ""
}

image_list_trusted() {
    local image_id="$1"
    api_request "GET" "/images/$image_id/access" ""
}

image_transfer() {
    local image_id="$1"
    local to_key="$2"
    api_request "POST" "/images/$image_id/transfer" "{\"to_api_key\":\"$to_key\"}"
}

image_spawn() {
    local image_id="$1"
    local name="${2:-}"
    local ports="${3:-}"

    local body="{}"
    if [ -n "$name" ] || [ -n "$ports" ]; then
        body="{"
        local first=1
        if [ -n "$name" ]; then
            body="$body\"name\":\"$name\""
            first=0
        fi
        if [ -n "$ports" ]; then
            [ "$first" -eq 0 ] && body="$body,"
            body="$body\"ports\":[$ports]"
        fi
        body="$body}"
    fi

    api_request "POST" "/images/$image_id/spawn" "$body"
}

image_clone() {
    local image_id="$1"
    local name="${2:-}"

    local body="{}"
    if [ -n "$name" ]; then
        body="{\"name\":\"$name\"}"
    fi

    api_request "POST" "/images/$image_id/clone" "$body"
}

# ============================================================================
# PaaS Logs Functions (2)
# ============================================================================

logs_fetch() {
    local source="${1:-all}"
    local lines="${2:-100}"
    local since="${3:-1h}"
    local grep_pattern="${4:-}"

    local body
    body=$(jq -n --arg source "$source" --argjson lines "$lines" --arg since "$since" --arg grep "$grep_pattern" \
        '{source: $source, lines: $lines, since: $since, grep: (if $grep != "" then $grep else null end)}')

    api_request "POST" "/paas/logs" "$body"
}

logs_stream() {
    set_error "logs_stream requires async support"
    return 1
}

# ============================================================================
# Key Validation
# ============================================================================

validate_keys() {
    local creds
    creds=$(get_credentials) || return 1
    local pk="${creds%%:*}"
    local sk="${creds#*:}"

    local timestamp
    timestamp=$(date +%s)

    local signature=""
    if [ -n "$sk" ]; then
        signature=$(hmac_sign "$sk" "$timestamp:POST:/keys/validate:")
    fi

    curl -s -X POST "$PORTAL_BASE/keys/validate" \
        -H "Authorization: Bearer $pk" \
        -H "Content-Type: application/json" \
        ${signature:+-H "X-Timestamp: $timestamp" -H "X-Signature: $signature"}
}

health_check() {
    local result
    result=$(curl -s -o /dev/null -w '%{http_code}' "$API_BASE/health")
    [ "$result" = "200" ]
}

# ============================================================================
# CLI Implementation
# ============================================================================

run_file() {
    local file="$1"
    if [ ! -f "$file" ]; then
        echo -e "${RED}Error: File not found: $file${RESET}" >&2
        exit 1
    fi

    local code
    code=$(cat "$file")
    local lang
    lang=$(detect_language "$file") || {
        echo -e "${RED}Error: Cannot detect language${RESET}" >&2
        exit 1
    }

    local result
    result=$(execute "$lang" "$code")

    if ! echo "$result" | jq -e . >/dev/null 2>&1; then
        echo -e "${RED}Error: Failed to execute${RESET}" >&2
        exit 1
    fi

    echo "$result" | jq -r '.stdout // empty'
    echo "$result" | jq -r '.stderr // empty' >&2
    local exit_code
    exit_code=$(echo "$result" | jq -r '.exit_code // 0')
    exit "${exit_code:-0}"
}

cmd_languages() {
    local json_output=0

    for arg in "$@"; do
        if [ "$arg" = "--json" ]; then
            json_output=1
        fi
    done

    local result
    result=$(get_languages)

    if [ "$json_output" -eq 1 ]; then
        echo "$result" | jq -c '.'
    else
        echo "$result" | jq -r '.[]'
    fi
}

cmd_key() {
    local extend=0

    for arg in "$@"; do
        if [ "$arg" = "--extend" ]; then
            extend=1
        fi
    done

    local result
    result=$(validate_keys)

    local pk
    pk=$(echo "$result" | jq -r '.public_key // empty')

    if [ "$extend" -eq 1 ] && [ -n "$pk" ]; then
        local url="$PORTAL_BASE/keys/extend?pk=$pk"
        echo -e "${BLUE}Opening browser to extend key...${RESET}"
        xdg-open "$url" 2>/dev/null || open "$url" 2>/dev/null &
        return
    fi

    if echo "$result" | jq -e '.expired' >/dev/null 2>&1; then
        echo -e "${RED}Expired${RESET}"
        echo "Public Key: $pk"
        echo "Tier: $(echo "$result" | jq -r '.tier // "N/A"')"
        echo -e "${YELLOW}To renew: Visit $PORTAL_BASE/keys/extend${RESET}"
        exit 1
    fi

    echo -e "${GREEN}Valid${RESET}"
    echo "Public Key: $pk"
    echo "Tier: $(echo "$result" | jq -r '.tier // "N/A"')"
    echo "Status: $(echo "$result" | jq -r '.status // "N/A"')"
    echo "Expires: $(echo "$result" | jq -r '.expires_at // "N/A"')"
    echo "Time Remaining: $(echo "$result" | jq -r '.time_remaining // "N/A"')"
}

cmd_session() {
    local action=""
    local target=""

    while [ $# -gt 0 ]; do
        case "$1" in
            --list|-l) action="list" ;;
            --info) action="info"; target="$2"; shift ;;
            --kill) action="kill"; target="$2"; shift ;;
            --freeze) action="freeze"; target="$2"; shift ;;
            --unfreeze) action="unfreeze"; target="$2"; shift ;;
            --boost) action="boost"; target="$2"; shift ;;
            --unboost) action="unboost"; target="$2"; shift ;;
            *) ;;
        esac
        shift
    done

    case "$action" in
        list)
            local result
            result=$(session_list)
            echo "$result" | jq -r '.sessions[] | "\(.id)\t\(.shell)\t\(.status)\t\(.created_at)"' 2>/dev/null || echo "No sessions"
            ;;
        info)
            session_get "$target" | jq .
            ;;
        kill)
            session_destroy "$target"
            echo -e "${GREEN}Session terminated: $target${RESET}"
            ;;
        freeze)
            session_freeze "$target"
            echo -e "${GREEN}Session frozen: $target${RESET}"
            ;;
        unfreeze)
            session_unfreeze "$target"
            echo -e "${GREEN}Session unfreezing: $target${RESET}"
            ;;
        boost)
            session_boost "$target"
            echo -e "${GREEN}Session boosted: $target${RESET}"
            ;;
        unboost)
            session_unboost "$target"
            echo -e "${GREEN}Session unboosted: $target${RESET}"
            ;;
        *)
            echo "Usage: bash un.sh session --list|--info ID|--kill ID|--freeze ID|--unfreeze ID" >&2
            exit 1
            ;;
    esac
}

cmd_service() {
    local action=""
    local target=""
    local name=""
    local ports=""

    while [ $# -gt 0 ]; do
        case "$1" in
            --list|-l) action="list" ;;
            --info) action="info"; target="$2"; shift ;;
            --destroy) action="destroy"; target="$2"; shift ;;
            --freeze) action="freeze"; target="$2"; shift ;;
            --unfreeze) action="unfreeze"; target="$2"; shift ;;
            --lock) action="lock"; target="$2"; shift ;;
            --unlock) action="unlock"; target="$2"; shift ;;
            --logs) action="logs"; target="$2"; shift ;;
            --name) name="$2"; shift ;;
            --ports) ports="$2"; shift ;;
            *) ;;
        esac
        shift
    done

    case "$action" in
        list)
            local result
            result=$(service_list)
            echo "$result" | jq -r '.services[] | "\(.id)\t\(.name)\t\(.status)\t\(.ports | join(","))"' 2>/dev/null || echo "No services"
            ;;
        info)
            service_get "$target" | jq .
            ;;
        destroy)
            service_destroy "$target"
            echo -e "${GREEN}Service destroyed: $target${RESET}"
            ;;
        freeze)
            service_freeze "$target"
            echo -e "${GREEN}Service frozen: $target${RESET}"
            ;;
        unfreeze)
            service_unfreeze "$target"
            echo -e "${GREEN}Service unfreezing: $target${RESET}"
            ;;
        lock)
            service_lock "$target"
            echo -e "${GREEN}Service locked: $target${RESET}"
            ;;
        unlock)
            service_unlock "$target"
            echo -e "${GREEN}Service unlocked: $target${RESET}"
            ;;
        logs)
            local result
            result=$(service_logs "$target")
            echo "$result" | jq -r '.logs // empty'
            ;;
        *)
            if [ -n "$name" ]; then
                local result
                result=$(service_create "$name" "$ports" "")
                echo -e "${GREEN}Service created${RESET}"
                echo "$result" | jq -r '"ID: \(.id)\nName: \(.name)"'
            else
                echo "Usage: bash un.sh service --list|--info ID|--destroy ID|--name NAME" >&2
                exit 1
            fi
            ;;
    esac
}

cmd_snapshot() {
    local action=""
    local target=""

    while [ $# -gt 0 ]; do
        case "$1" in
            --list|-l) action="list" ;;
            --info) action="info"; target="$2"; shift ;;
            --delete) action="delete"; target="$2"; shift ;;
            --restore) action="restore"; target="$2"; shift ;;
            --lock) action="lock"; target="$2"; shift ;;
            --unlock) action="unlock"; target="$2"; shift ;;
            *) ;;
        esac
        shift
    done

    case "$action" in
        list)
            local result
            result=$(snapshot_list)
            echo "$result" | jq -r '.snapshots[] | "\(.id)\t\(.name)\t\(.type)\t\(.created_at)"' 2>/dev/null || echo "No snapshots"
            ;;
        info)
            snapshot_get "$target" | jq .
            ;;
        delete)
            snapshot_delete "$target"
            echo -e "${GREEN}Snapshot deleted: $target${RESET}"
            ;;
        restore)
            snapshot_restore "$target"
            echo -e "${GREEN}Snapshot restored${RESET}"
            ;;
        lock)
            snapshot_lock "$target"
            echo -e "${GREEN}Snapshot locked: $target${RESET}"
            ;;
        unlock)
            snapshot_unlock "$target"
            echo -e "${GREEN}Snapshot unlocked: $target${RESET}"
            ;;
        *)
            echo "Usage: bash un.sh snapshot --list|--info ID|--delete ID|--restore ID" >&2
            exit 1
            ;;
    esac
}

cmd_image() {
    local action=""
    local target=""
    local source_type=""
    local visibility_mode=""
    local name=""
    local ports=""

    while [ $# -gt 0 ]; do
        case "$1" in
            --list|-l) action="list" ;;
            --info) action="info"; target="$2"; shift ;;
            --delete) action="delete"; target="$2"; shift ;;
            --lock) action="lock"; target="$2"; shift ;;
            --unlock) action="unlock"; target="$2"; shift ;;
            --publish) action="publish"; target="$2"; shift ;;
            --source-type) source_type="$2"; shift ;;
            --visibility) action="visibility"; target="$2"; visibility_mode="$3"; shift 2 ;;
            --spawn) action="spawn"; target="$2"; shift ;;
            --clone) action="clone"; target="$2"; shift ;;
            --name) name="$2"; shift ;;
            --ports) ports="$2"; shift ;;
            *) ;;
        esac
        shift
    done

    case "$action" in
        list)
            local result
            result=$(image_list)
            echo "$result" | jq -r '.images[] | "\(.id)\t\(.name // "-")\t\(.visibility)\t\(.created_at)"' 2>/dev/null || echo "No images"
            ;;
        info)
            image_get "$target" | jq .
            ;;
        delete)
            image_delete "$target"
            echo -e "${GREEN}Image deleted: $target${RESET}"
            ;;
        lock)
            image_lock "$target"
            echo -e "${GREEN}Image locked: $target${RESET}"
            ;;
        unlock)
            image_unlock "$target"
            echo -e "${GREEN}Image unlocked: $target${RESET}"
            ;;
        publish)
            if [ -z "$source_type" ]; then
                echo -e "${RED}Error: --source-type required${RESET}" >&2
                exit 1
            fi
            local result
            result=$(image_publish "$source_type" "$target" "$name")
            echo -e "${GREEN}Image published${RESET}"
            echo "$result" | jq -r '"Image ID: \(.id)"'
            ;;
        visibility)
            image_set_visibility "$target" "$visibility_mode"
            echo -e "${GREEN}Visibility set to $visibility_mode${RESET}"
            ;;
        spawn)
            local result
            result=$(image_spawn "$target" "$name" "$ports")
            echo -e "${GREEN}Service spawned from image${RESET}"
            echo "$result" | jq -r '"Service ID: \(.id)"'
            ;;
        clone)
            local result
            result=$(image_clone "$target" "$name")
            echo -e "${GREEN}Image cloned${RESET}"
            echo "$result" | jq -r '"Image ID: \(.id)"'
            ;;
        *)
            echo "Usage: bash un.sh image --list|--info ID|--delete ID|--publish ID|--spawn ID|--clone ID" >&2
            exit 1
            ;;
    esac
}

show_help() {
    cat << 'EOF'
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  bash un.sh [options] <source_file>
  bash un.sh -s <language> '<code>'
  bash un.sh session [options]
  bash un.sh service [options]
  bash un.sh snapshot [options]
  bash un.sh image [options]
  bash un.sh languages [--json]
  bash un.sh key [--extend]

Commands:
  languages     List available programming languages
  key           Validate API key
  session       Manage interactive sessions
  service       Manage persistent services
  snapshot      Manage snapshots
  image         Manage images

Session options:
  --list        List all sessions
  --info ID     Get session details
  --kill ID     Terminate session
  --freeze ID   Freeze session
  --unfreeze ID Unfreeze session
  --boost ID    Boost session CPU
  --unboost ID  Unboost session CPU

Service options:
  --list        List all services
  --info ID     Get service details
  --destroy ID  Destroy service
  --freeze ID   Freeze service
  --unfreeze ID Unfreeze service
  --lock ID     Lock service
  --unlock ID   Unlock service
  --logs ID     Get service logs
  --name NAME   Create service with name
  --ports PORTS Service ports (comma-separated)

Snapshot options:
  --list        List all snapshots
  --info ID     Get snapshot details
  --delete ID   Delete snapshot
  --restore ID  Restore from snapshot
  --lock ID     Lock snapshot
  --unlock ID   Unlock snapshot

Image options:
  --list        List all images
  --info ID     Get image details
  --delete ID   Delete image
  --lock ID     Lock image
  --unlock ID   Unlock image
  --publish ID  Publish from service/snapshot (needs --source-type)
  --source-type TYPE   Source type (service or snapshot)
  --visibility ID MODE Set visibility (private|unlisted|public)
  --spawn ID    Spawn service from image
  --clone ID    Clone image
  --name NAME   Name for spawned service or cloned image
  --ports PORTS Ports for spawned service

Environment:
  UNSANDBOX_PUBLIC_KEY  API public key
  UNSANDBOX_SECRET_KEY  API secret key
EOF
}

# CLI entry point
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ $# -eq 0 ]; then
        show_help
        exit 1
    fi

    case "$1" in
        languages)
            shift
            cmd_languages "$@"
            ;;
        key)
            shift
            cmd_key "$@"
            ;;
        session)
            shift
            cmd_session "$@"
            ;;
        service)
            shift
            cmd_service "$@"
            ;;
        snapshot)
            shift
            cmd_snapshot "$@"
            ;;
        image)
            shift
            cmd_image "$@"
            ;;
        -s)
            lang="$2"
            code="$3"
            if [ -z "$lang" ] || [ -z "$code" ]; then
                echo -e "${RED}Error: -s requires language and code${RESET}" >&2
                exit 1
            fi
            result=$(execute "$lang" "$code")
            echo "$result" | jq -r '.stdout // empty'
            echo "$result" | jq -r '.stderr // empty' >&2
            exit_code=$(echo "$result" | jq -r '.exit_code // 0')
            exit "${exit_code:-0}"
            ;;
        --help|-h)
            show_help
            ;;
        *)
            run_file "$1"
            ;;
    esac
fi
