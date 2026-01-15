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

#!/usr/bin/env bash
set -euo pipefail

# un.sh - Unsandbox CLI Client (Bash Implementation)
#
# Full-featured CLI matching un.c capabilities:
# - Execute code with env vars, input files, artifacts
# - Interactive sessions with shell/REPL support
# - Persistent services with domains and ports
#
# Usage:
#   un.sh [options] <source_file>
#   un.sh session [options]
#   un.sh service [options]
#
# Requires: UNSANDBOX_API_KEY environment variable, jq, curl

API_BASE="https://api.unsandbox.com"
PORTAL_BASE="https://unsandbox.com"
BLUE="\033[34m"
RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
RESET="\033[0m"

# Extension to language mapping
detect_language() {
    local filename="$1"
    local ext="${filename##*.}"
    ext=$(echo "$ext" | tr '[:upper:]' '[:lower:]')

    case "$ext" in
        py) echo "python" ;;
        js) echo "javascript" ;;
        ts) echo "typescript" ;;
        rb) echo "ruby" ;;
        php) echo "php" ;;
        pl) echo "perl" ;;
        lua) echo "lua" ;;
        sh) echo "bash" ;;
        go) echo "go" ;;
        rs) echo "rust" ;;
        c) echo "c" ;;
        cpp|cc|cxx) echo "cpp" ;;
        java) echo "java" ;;
        kt) echo "kotlin" ;;
        cs) echo "csharp" ;;
        fs) echo "fsharp" ;;
        hs) echo "haskell" ;;
        ml) echo "ocaml" ;;
        clj) echo "clojure" ;;
        scm) echo "scheme" ;;
        lisp) echo "commonlisp" ;;
        erl) echo "erlang" ;;
        ex|exs) echo "elixir" ;;
        jl) echo "julia" ;;
        r|R) echo "r" ;;
        cr) echo "crystal" ;;
        d) echo "d" ;;
        nim) echo "nim" ;;
        zig) echo "zig" ;;
        v) echo "v" ;;
        dart) echo "dart" ;;
        groovy) echo "groovy" ;;
        scala) echo "scala" ;;
        f90|f95) echo "fortran" ;;
        cob) echo "cobol" ;;
        pro) echo "prolog" ;;
        forth|4th) echo "forth" ;;
        tcl) echo "tcl" ;;
        raku) echo "raku" ;;
        m) echo "objc" ;;
        *)
            # Try shebang
            if [[ -f "$filename" ]]; then
                local first_line=$(head -n1 "$filename")
                if [[ "$first_line" =~ ^#! ]]; then
                    [[ "$first_line" =~ python ]] && echo "python" && return
                    [[ "$first_line" =~ node ]] && echo "javascript" && return
                    [[ "$first_line" =~ ruby ]] && echo "ruby" && return
                    [[ "$first_line" =~ perl ]] && echo "perl" && return
                    [[ "$first_line" =~ (bash|/sh) ]] && echo "bash" && return
                    [[ "$first_line" =~ lua ]] && echo "lua" && return
                    [[ "$first_line" =~ php ]] && echo "php" && return
                fi
            fi
            echo -e "${RED}Error: Cannot detect language for $filename${RESET}" >&2
            exit 1
            ;;
    esac
}

api_request() {
    local endpoint="$1"
    local method="${2:-GET}"
    local data="${3:-}"
    local public_key="${4:-${UNSANDBOX_PUBLIC_KEY:-}}"
    local secret_key="${5:-${UNSANDBOX_SECRET_KEY:-}}"

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if [[ -z "$public_key" ]] && [[ -n "${UNSANDBOX_API_KEY:-}" ]]; then
        public_key="${UNSANDBOX_API_KEY}"
        secret_key=""
    fi

    if [[ -z "$public_key" ]]; then
        echo -e "${RED}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set${RESET}" >&2
        exit 1
    fi

    local url="${API_BASE}${endpoint}"
    local timestamp=$(date +%s)
    local body="${data:-}"

    # Build HMAC signature: timestamp:METHOD:path:body
    local sig_input="${timestamp}:${method}:${endpoint}:${body}"
    local signature=""

    if [[ -n "$secret_key" ]]; then
        signature=$(echo -n "$sig_input" | openssl dgst -sha256 -hmac "$secret_key" | sed 's/^.* //')
    fi

    if [[ -n "$data" ]]; then
        local response
        if [[ -n "$signature" ]]; then
            response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
                -H "Authorization: Bearer $public_key" \
                -H "X-Timestamp: $timestamp" \
                -H "X-Signature: $signature" \
                -H "Content-Type: application/json" \
                -d "$data" 2>&1)
        else
            response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
                -H "Authorization: Bearer $public_key" \
                -H "Content-Type: application/json" \
                -d "$data" 2>&1)
        fi
    else
        local response
        if [[ -n "$signature" ]]; then
            response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
                -H "Authorization: Bearer $public_key" \
                -H "X-Timestamp: $timestamp" \
                -H "X-Signature: $signature" \
                -H "Content-Type: application/json" 2>&1)
        else
            response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
                -H "Authorization: Bearer $public_key" \
                -H "Content-Type: application/json" 2>&1)
        fi
    fi

    local http_code=$(echo "$response" | tail -n1)
    local body=$(echo "$response" | head -n-1)

    if [[ "$http_code" -lt 200 || "$http_code" -ge 300 ]]; then
        if [[ "$http_code" == "401" ]] && echo "$body" | grep -qi "timestamp"; then
            echo -e "${RED}Error: Request timestamp expired (must be within 5 minutes of server time)${RESET}" >&2
            echo -e "${YELLOW}Your computer's clock may have drifted.${RESET}" >&2
            echo -e "${YELLOW}Check your system time and sync with NTP if needed:${RESET}" >&2
            echo -e "  Linux:   sudo ntpdate -s time.nist.gov" >&2
            echo -e "  macOS:   sudo sntp -sS time.apple.com" >&2
            echo -e "  Windows: w32tm /resync" >&2
        else
            echo -e "${RED}Error: HTTP $http_code - $body${RESET}" >&2
        fi
        exit 1
    fi

    echo "$body"
}

api_request_text() {
    local endpoint="$1"
    local method="${2:-PUT}"
    local body="${3:-}"
    local public_key="${4:-${UNSANDBOX_PUBLIC_KEY:-}}"
    local secret_key="${5:-${UNSANDBOX_SECRET_KEY:-}}"

    # Fallback to old UNSANDBOX_API_KEY for backwards compat
    if [[ -z "$public_key" ]] && [[ -n "${UNSANDBOX_API_KEY:-}" ]]; then
        public_key="${UNSANDBOX_API_KEY}"
        secret_key=""
    fi

    if [[ -z "$public_key" ]]; then
        echo -e "${RED}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set${RESET}" >&2
        return 1
    fi

    local url="${API_BASE}${endpoint}"
    local timestamp=$(date +%s)

    # Build HMAC signature: timestamp:METHOD:path:body
    local sig_input="${timestamp}:${method}:${endpoint}:${body}"
    local signature=""

    if [[ -n "$secret_key" ]]; then
        signature=$(echo -n "$sig_input" | openssl dgst -sha256 -hmac "$secret_key" | sed 's/^.* //')
    fi

    local response
    if [[ -n "$signature" ]]; then
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $public_key" \
            -H "X-Timestamp: $timestamp" \
            -H "X-Signature: $signature" \
            -H "Content-Type: text/plain" \
            -d "$body" 2>&1)
    else
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $public_key" \
            -H "Content-Type: text/plain" \
            -d "$body" 2>&1)
    fi

    local http_code=$(echo "$response" | tail -n1)
    local resp_body=$(echo "$response" | head -n-1)

    if [[ "$http_code" -lt 200 || "$http_code" -ge 300 ]]; then
        echo -e "${RED}Error: HTTP $http_code - $resp_body${RESET}" >&2
        return 1
    fi

    echo "$resp_body"
}

# ============================================================================
# Environment Secrets Vault Functions
# ============================================================================

MAX_ENV_CONTENT_SIZE=65536  # 64KB max env vault size

service_env_status() {
    local service_id="$1"
    local api_key="${2:-${UNSANDBOX_API_KEY:-}}"

    local result=$(api_request "/services/$service_id/env" "GET" "" "$api_key")
    local has_vault=$(echo "$result" | jq -r '.has_vault // false')

    if [[ "$has_vault" != "true" ]]; then
        echo "Vault exists: no"
        echo "Variable count: 0"
    else
        echo "Vault exists: yes"
        local count=$(echo "$result" | jq -r '.count // 0')
        echo "Variable count: $count"
        local updated_at=$(echo "$result" | jq -r '.updated_at // ""')
        if [[ -n "$updated_at" ]] && [[ "$updated_at" != "null" ]]; then
            local dt=$(date -d "@$updated_at" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || date -r "$updated_at" "+%Y-%m-%d %H:%M:%S" 2>/dev/null || echo "$updated_at")
            echo "Last updated: $dt"
        fi
    fi
}

service_env_set() {
    local service_id="$1"
    local env_content="$2"
    local api_key="${3:-${UNSANDBOX_API_KEY:-}}"

    if [[ -z "$env_content" ]]; then
        echo -e "${RED}Error: No environment content provided${RESET}" >&2
        return 1
    fi

    local content_size=${#env_content}
    if [[ $content_size -gt $MAX_ENV_CONTENT_SIZE ]]; then
        echo -e "${RED}Error: Environment content too large (max $MAX_ENV_CONTENT_SIZE bytes)${RESET}" >&2
        return 1
    fi

    local result
    result=$(api_request_text "/services/$service_id/env" "PUT" "$env_content" "$api_key")
    if [[ $? -ne 0 ]]; then
        return 1
    fi

    local count=$(echo "$result" | jq -r '.count // -1')
    if [[ "$count" != "-1" ]]; then
        local plural="s"
        [[ "$count" == "1" ]] && plural=""
        echo -e "${GREEN}Environment vault updated: $count variable$plural${RESET}"
    else
        echo -e "${GREEN}Environment vault updated${RESET}"
    fi

    local message=$(echo "$result" | jq -r '.message // ""')
    [[ -n "$message" ]] && echo "$message"

    return 0
}

service_env_export() {
    local service_id="$1"
    local api_key="${2:-${UNSANDBOX_API_KEY:-}}"

    local result=$(api_request "/services/$service_id/env/export" "POST" "{}" "$api_key")
    local env_content=$(echo "$result" | jq -r '.env // ""')
    if [[ -n "$env_content" ]]; then
        echo -n "$env_content"
        [[ "${env_content: -1}" != $'\n' ]] && echo
    fi
}

service_env_delete() {
    local service_id="$1"
    local api_key="${2:-${UNSANDBOX_API_KEY:-}}"

    api_request "/services/$service_id/env" "DELETE" "" "$api_key" > /dev/null
    echo -e "${GREEN}Environment vault deleted${RESET}"
}

read_env_file() {
    local filepath="$1"
    if [[ ! -f "$filepath" ]]; then
        echo -e "${RED}Error: Env file not found: $filepath${RESET}" >&2
        exit 1
    fi
    cat "$filepath"
}

build_env_content() {
    local env_file="$1"
    shift
    local -a env_vars=("$@")
    local parts=""

    # Read from env file first
    if [[ -n "$env_file" ]]; then
        parts+=$(read_env_file "$env_file")
    fi

    # Add -e flags (these override/append to file)
    for e in "${env_vars[@]}"; do
        if [[ "$e" == *"="* ]]; then
            [[ -n "$parts" ]] && parts+=$'\n'
            parts+="$e"
        fi
    done

    echo "$parts"
}

cmd_execute() {
    local source_file=""
    local -a env_vars=()
    local -a input_files=()
    local artifacts=false
    local output_dir="."
    local network=""
    local vcpu=""
    local api_key="${UNSANDBOX_API_KEY:-}"
    local exec_shell=""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -s|--shell)
                exec_shell="$2"
                shift 2
                ;;
            -e)
                env_vars+=("$2")
                shift 2
                ;;
            -f)
                input_files+=("$2")
                shift 2
                ;;
            -a)
                artifacts=true
                shift
                ;;
            -o)
                output_dir="$2"
                shift 2
                ;;
            -n)
                network="$2"
                shift 2
                ;;
            -v)
                vcpu="$2"
                shift 2
                ;;
            -k)
                api_key="$2"
                shift 2
                ;;
            -*)
                echo -e "${RED}Unknown option: $1${RESET}" >&2
                exit 1
                ;;
            *)
                source_file="$1"
                shift
                ;;
        esac
    done

    local code
    local language

    # Check for inline mode: -s/--shell specified, or source_file doesn't exist
    if [[ -n "$exec_shell" ]]; then
        # Inline mode with specified language
        code="$source_file"
        language="$exec_shell"
    elif [[ ! -f "$source_file" ]]; then
        # File doesn't exist - treat as inline bash code
        code="$source_file"
        language="bash"
    else
        # Normal file execution
        code=$(cat "$source_file")
        language=$(detect_language "$source_file")
    fi

    # Build JSON payload
    local payload=$(jq -n \
        --arg lang "$language" \
        --arg code "$code" \
        '{language: $lang, code: $code}')

    # Add environment variables
    if [[ ${#env_vars[@]} -gt 0 ]]; then
        local env_json="{"
        for env_var in "${env_vars[@]}"; do
            local key="${env_var%%=*}"
            local val="${env_var#*=}"
            env_json+="\"$key\":\"$val\","
        done
        env_json="${env_json%,}}"
        payload=$(echo "$payload" | jq --argjson env "$env_json" '. + {env: $env}')
    fi

    # Add input files
    if [[ ${#input_files[@]} -gt 0 ]]; then
        local files_json="["
        for file in "${input_files[@]}"; do
            if [[ ! -f "$file" ]]; then
                echo -e "${RED}Error: Input file not found: $file${RESET}" >&2
                exit 1
            fi
            local filename=$(basename "$file")
            local content_b64=$(base64 -w0 < "$file")
            files_json+="{\"filename\":\"$filename\",\"content_base64\":\"$content_b64\"},"
        done
        files_json="${files_json%,}]"
        payload=$(echo "$payload" | jq --argjson files "$files_json" '. + {input_files: $files}')
    fi

    # Add options
    [[ "$artifacts" == true ]] && payload=$(echo "$payload" | jq '. + {return_artifacts: true}')
    [[ -n "$network" ]] && payload=$(echo "$payload" | jq --arg n "$network" '. + {network: $n}')
    [[ -n "$vcpu" ]] && payload=$(echo "$payload" | jq --argjson v "$vcpu" '. + {vcpu: $v}')

    # Execute
    local result=$(api_request "/execute" "POST" "$payload" "$api_key")

    # Print output
    local stdout=$(echo "$result" | jq -r '.stdout // empty')
    local stderr=$(echo "$result" | jq -r '.stderr // empty')
    [[ -n "$stdout" ]] && echo -e "${BLUE}${stdout}${RESET}"
    [[ -n "$stderr" ]] && echo -e "${RED}${stderr}${RESET}" >&2

    # Save artifacts
    if [[ "$artifacts" == true ]]; then
        local artifacts_json=$(echo "$result" | jq -r '.artifacts // []')
        if [[ "$artifacts_json" != "[]" ]]; then
            mkdir -p "$output_dir"
            local num_artifacts=$(echo "$artifacts_json" | jq 'length')
            for ((i=0; i<num_artifacts; i++)); do
                local artifact=$(echo "$artifacts_json" | jq -r ".[$i]")
                local filename=$(echo "$artifact" | jq -r '.filename // "artifact"')
                local content_b64=$(echo "$artifact" | jq -r '.content_base64')
                local filepath="$output_dir/$filename"
                echo "$content_b64" | base64 -d > "$filepath"
                chmod 755 "$filepath"
                echo -e "${GREEN}Saved: $filepath${RESET}" >&2
            done
        fi
    fi

    local exit_code=$(echo "$result" | jq -r '.exit_code // 0')
    exit "$exit_code"
}

cmd_session() {
    local shell="bash"
    local list=false
    local attach=""
    local kill=""
    local audit=false
    local tmux=false
    local screen=false
    local network=""
    local vcpu=""
    local api_key="${UNSANDBOX_API_KEY:-}"
    local -a input_files=()
    local snapshot=""
    local restore=""
    local from_snapshot=""
    local snapshot_name=""
    local hot=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -s|--shell)
                shell="$2"
                shift 2
                ;;
            -l|--list)
                list=true
                shift
                ;;
            --attach)
                attach="$2"
                shift 2
                ;;
            --kill)
                kill="$2"
                shift 2
                ;;
            --audit)
                audit=true
                shift
                ;;
            --tmux)
                tmux=true
                shift
                ;;
            --screen)
                screen=true
                shift
                ;;
            --snapshot)
                snapshot="$2"
                shift 2
                ;;
            --restore)
                restore="$2"
                shift 2
                ;;
            --from)
                from_snapshot="$2"
                shift 2
                ;;
            --snapshot-name)
                snapshot_name="$2"
                shift 2
                ;;
            --hot)
                hot=true
                shift
                ;;
            -f)
                input_files+=("$2")
                shift 2
                ;;
            -n)
                network="$2"
                shift 2
                ;;
            -v)
                vcpu="$2"
                shift 2
                ;;
            -k)
                api_key="$2"
                shift 2
                ;;
            -*)
                echo -e "${RED}Unknown option: $1${RESET}" >&2
                exit 1
                ;;
            *)
                shift
                ;;
        esac
    done

    if [[ "$list" == true ]]; then
        local result=$(api_request "/sessions" "GET" "" "$api_key")
        local sessions=$(echo "$result" | jq -r '.sessions // []')
        if [[ "$sessions" == "[]" ]]; then
            echo "No active sessions"
        else
            printf "%-40s %-10s %-10s %s\n" "ID" "Shell" "Status" "Created"
            echo "$sessions" | jq -r '.[] | "\(.id // "N/A") \(.shell // "N/A") \(.status // "N/A") \(.created_at // "N/A")"' | \
                while read -r id sh status created; do
                    printf "%-40s %-10s %-10s %s\n" "$id" "$sh" "$status" "$created"
                done
        fi
        return
    fi

    if [[ -n "$kill" ]]; then
        api_request "/sessions/$kill" "DELETE" "" "$api_key" > /dev/null
        echo -e "${GREEN}Session terminated: $kill${RESET}"
        return
    fi

    if [[ -n "$snapshot" ]]; then
        local payload=$(jq -n --arg name "$snapshot_name" --argjson hot "$hot" '{name: $name, hot: $hot}')
        echo -e "${YELLOW}Creating snapshot of session $snapshot...${RESET}"
        local result=$(api_request "/sessions/$snapshot/snapshot" "POST" "$payload" "$api_key")
        local snapshot_id=$(echo "$result" | jq -r '.id // "N/A"')
        echo -e "${GREEN}Snapshot created successfully${RESET}"
        echo "Snapshot ID: $snapshot_id"
        return
    fi

    if [[ -n "$restore" ]]; then
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        echo -e "${YELLOW}Restoring from snapshot $restore...${RESET}"
        local result=$(api_request "/snapshots/$restore/restore" "POST" "{}" "$api_key")
        echo -e "${GREEN}Session restored from snapshot${RESET}"
        local new_id=$(echo "$result" | jq -r '.session_id // empty')
        [[ -n "$new_id" ]] && echo "New session ID: $new_id"
        return
    fi

    if [[ -n "$attach" ]]; then
        echo -e "${YELLOW}Attaching to session $attach...${RESET}"
        echo -e "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}"
        return
    fi

    # Create session
    local payload=$(jq -n --arg sh "$shell" '{shell: $sh}')
    [[ -n "$network" ]] && payload=$(echo "$payload" | jq --arg n "$network" '. + {network: $n}')
    [[ -n "$vcpu" ]] && payload=$(echo "$payload" | jq --argjson v "$vcpu" '. + {vcpu: $v}')
    [[ "$tmux" == true ]] && payload=$(echo "$payload" | jq '. + {persistence: "tmux"}')
    [[ "$screen" == true ]] && payload=$(echo "$payload" | jq '. + {persistence: "screen"}')
    [[ "$audit" == true ]] && payload=$(echo "$payload" | jq '. + {audit: true}')

    # Add input files
    if [[ ${#input_files[@]} -gt 0 ]]; then
        local files_json="["
        for file in "${input_files[@]}"; do
            if [[ ! -f "$file" ]]; then
                echo -e "${RED}Error: Input file not found: $file${RESET}" >&2
                exit 1
            fi
            local filename=$(basename "$file")
            local content_b64=$(base64 -w0 < "$file")
            files_json+="{\"filename\":\"$filename\",\"content_base64\":\"$content_b64\"},"
        done
        files_json="${files_json%,}]"
        payload=$(echo "$payload" | jq --argjson files "$files_json" '. + {input_files: $files}')
    fi

    echo -e "${YELLOW}Creating session...${RESET}"
    local result=$(api_request "/sessions" "POST" "$payload" "$api_key")
    local session_id=$(echo "$result" | jq -r '.id // "N/A"')
    echo -e "${GREEN}Session created: $session_id${RESET}"
    echo -e "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}"
}

cmd_service_env() {
    local action="$1"
    local target="$2"
    shift 2

    local api_key="${UNSANDBOX_API_KEY:-}"
    local env_file=""
    local -a env_vars=()

    # Parse remaining args for -e and --env-file
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -e)
                env_vars+=("$2")
                shift 2
                ;;
            --env-file)
                env_file="$2"
                shift 2
                ;;
            -k)
                api_key="$2"
                shift 2
                ;;
            *)
                shift
                ;;
        esac
    done

    if [[ -z "$action" ]]; then
        echo -e "${RED}Error: env action required (status, set, export, delete)${RESET}" >&2
        exit 1
    fi

    if [[ -z "$target" ]]; then
        echo -e "${RED}Error: Service ID required for env command${RESET}" >&2
        exit 1
    fi

    case "$action" in
        status)
            service_env_status "$target" "$api_key"
            ;;
        set)
            local env_content=$(build_env_content "$env_file" "${env_vars[@]}")
            if [[ -z "$env_content" ]]; then
                echo -e "${RED}Error: No env content provided. Use -e KEY=VAL, --env-file, or pipe to stdin${RESET}" >&2
                exit 1
            fi
            service_env_set "$target" "$env_content" "$api_key"
            ;;
        export)
            service_env_export "$target" "$api_key"
            ;;
        delete)
            service_env_delete "$target" "$api_key"
            ;;
        *)
            echo -e "${RED}Error: Unknown env action '$action'. Use: status, set, export, delete${RESET}" >&2
            exit 1
            ;;
    esac
}

cmd_service() {
    local name=""
    local ports=""
    local domains=""
    local service_type=""
    local bootstrap=""
    local bootstrap_file=""
    local list=false
    local info=""
    local logs=""
    local tail=""
    local sleep=""
    local wake=""
    local destroy=""
    local resize=""
    local execute=""
    local command=""
    local network=""
    local vcpu=""
    local api_key="${UNSANDBOX_API_KEY:-}"
    local -a input_files=()
    local -a env_vars=()
    local env_file=""
    local snapshot=""
    local restore=""
    local from_snapshot=""
    local snapshot_name=""
    local hot=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -e)
                env_vars+=("$2")
                shift 2
                ;;
            --env-file)
                env_file="$2"
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
            --domains)
                domains="$2"
                shift 2
                ;;
            --type)
                service_type="$2"
                shift 2
                ;;
            --bootstrap)
                bootstrap="$2"
                shift 2
                ;;
            --bootstrap-file)
                bootstrap_file="$2"
                shift 2
                ;;
            -f)
                input_files+=("$2")
                shift 2
                ;;
            -l|--list)
                list=true
                shift
                ;;
            --info)
                info="$2"
                shift 2
                ;;
            --logs)
                logs="$2"
                shift 2
                ;;
            --tail)
                tail="$2"
                shift 2
                ;;
            --freeze)
                sleep="$2"
                shift 2
                ;;
            --unfreeze)
                wake="$2"
                shift 2
                ;;
            --destroy)
                destroy="$2"
                shift 2
                ;;
            --resize)
                resize="$2"
                shift 2
                ;;
            --execute)
                execute="$2"
                shift 2
                ;;
            --command)
                command="$2"
                shift 2
                ;;
            --dump-bootstrap)
                dump_bootstrap="$2"
                shift 2
                ;;
            --dump-file)
                dump_file="$2"
                shift 2
                ;;
            --snapshot)
                snapshot="$2"
                shift 2
                ;;
            --restore)
                restore="$2"
                shift 2
                ;;
            --from)
                from_snapshot="$2"
                shift 2
                ;;
            --snapshot-name)
                snapshot_name="$2"
                shift 2
                ;;
            --hot)
                hot=true
                shift
                ;;
            -n)
                network="$2"
                shift 2
                ;;
            -v)
                vcpu="$2"
                shift 2
                ;;
            -k)
                api_key="$2"
                shift 2
                ;;
            -*)
                echo -e "${RED}Unknown option: $1${RESET}" >&2
                exit 1
                ;;
            *)
                shift
                ;;
        esac
    done

    if [[ "$list" == true ]]; then
        local result=$(api_request "/services" "GET" "" "$api_key")
        local services=$(echo "$result" | jq -r '.services // []')
        if [[ "$services" == "[]" ]]; then
            echo "No services"
        else
            printf "%-20s %-15s %-10s %-15s %s\n" "ID" "Name" "Status" "Ports" "Domains"
            echo "$services" | jq -r '.[] | "\(.id // "N/A") \(.name // "N/A") \(.status // "N/A") \((.ports // []) | join(",")) \((.domains // []) | join(","))"' | \
                while read -r id name status ports domains; do
                    printf "%-20s %-15s %-10s %-15s %s\n" "$id" "$name" "$status" "$ports" "$domains"
                done
        fi
        return
    fi

    if [[ -n "$snapshot" ]]; then
        local payload=$(jq -n --arg name "$snapshot_name" --argjson hot "$hot" '{name: $name, hot: $hot}')
        echo -e "${YELLOW}Creating snapshot of service $snapshot...${RESET}"
        local result=$(api_request "/services/$snapshot/snapshot" "POST" "$payload" "$api_key")
        local snapshot_id=$(echo "$result" | jq -r '.id // "N/A"')
        echo -e "${GREEN}Snapshot created successfully${RESET}"
        echo "Snapshot ID: $snapshot_id"
        return
    fi

    if [[ -n "$restore" ]]; then
        # --restore takes snapshot ID directly, calls /snapshots/:id/restore
        echo -e "${YELLOW}Restoring from snapshot $restore...${RESET}"
        local result=$(api_request "/snapshots/$restore/restore" "POST" "{}" "$api_key")
        echo -e "${GREEN}Service restored from snapshot${RESET}"
        local new_id=$(echo "$result" | jq -r '.service_id // empty')
        [[ -n "$new_id" ]] && echo "New service ID: $new_id"
        return
    fi

    if [[ -n "$info" ]]; then
        local result=$(api_request "/services/$info" "GET" "" "$api_key")
        echo "$result" | jq '.'
        return
    fi

    if [[ -n "$logs" ]]; then
        local result=$(api_request "/services/$logs/logs" "GET" "" "$api_key")
        echo "$result" | jq -r '.logs // ""'
        return
    fi

    if [[ -n "$tail" ]]; then
        local result=$(api_request "/services/$tail/logs?lines=9000" "GET" "" "$api_key")
        echo "$result" | jq -r '.logs // ""'
        return
    fi

    if [[ -n "$sleep" ]]; then
        api_request "/services/$sleep/freeze" "POST" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service frozen: $sleep${RESET}"
        return
    fi

    if [[ -n "$wake" ]]; then
        api_request "/services/$wake/unfreeze" "POST" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service unfreezing: $wake${RESET}"
        return
    fi

    if [[ -n "$destroy" ]]; then
        api_request "/services/$destroy" "DELETE" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service destroyed: $destroy${RESET}"
        return
    fi

    if [[ -n "$resize" ]]; then
        if [[ -z "$vcpu" ]]; then
            echo -e "${RED}Error: --vcpu is required with --resize${RESET}" >&2
            exit 1
        fi
        local payload=$(jq -n --argjson v "$vcpu" '{vcpu: $v}')
        api_request "/services/$resize" "PATCH" "$payload" "$api_key" > /dev/null
        local ram=$((vcpu * 2))
        echo -e "${GREEN}Service resized to $vcpu vCPU, $ram GB RAM${RESET}"
        return
    fi

    if [[ -n "$execute" ]]; then
        local payload=$(jq -n --arg cmd "$command" '{command: $cmd}')
        local result=$(api_request "/services/$execute/execute" "POST" "$payload" "$api_key")
        local stdout=$(echo "$result" | jq -r '.stdout // empty')
        local stderr=$(echo "$result" | jq -r '.stderr // empty')
        [[ -n "$stdout" ]] && echo -e "${BLUE}${stdout}${RESET}"
        [[ -n "$stderr" ]] && echo -e "${RED}${stderr}${RESET}" >&2
        return
    fi

    if [[ -n "$dump_bootstrap" ]]; then
        echo "Fetching bootstrap script from $dump_bootstrap..." >&2
        local payload=$(jq -n '{command: "cat /tmp/bootstrap.sh"}')
        local result=$(api_request "/services/$dump_bootstrap/execute" "POST" "$payload" "$api_key")
        local bootstrap=$(echo "$result" | jq -r '.stdout // empty')

        if [[ -n "$bootstrap" ]]; then
            if [[ -n "$dump_file" ]]; then
                # Write to file
                echo "$bootstrap" > "$dump_file"
                chmod 755 "$dump_file"
                echo "Bootstrap saved to $dump_file"
            else
                # Print to stdout
                echo -n "$bootstrap"
            fi
        else
            echo -e "${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}" >&2
            exit 1
        fi
        return
    fi

    if [[ -n "$name" ]]; then
        local payload=$(jq -n --arg n "$name" '{name: $n}')

        if [[ -n "$ports" ]]; then
            local ports_json="[$(echo "$ports" | sed 's/,/,/g')]"
            payload=$(echo "$payload" | jq --argjson p "$ports_json" '. + {ports: $p}')
        fi

        if [[ -n "$domains" ]]; then
            local domains_json="[\"$(echo "$domains" | sed 's/,/","/g')\"]"
            payload=$(echo "$payload" | jq --argjson d "$domains_json" '. + {domains: $d}')
        fi

        if [[ -n "$service_type" ]]; then
            payload=$(echo "$payload" | jq --arg t "$service_type" '. + {service_type: $t}')
        fi

        if [[ -n "$bootstrap" ]]; then
            payload=$(echo "$payload" | jq --arg b "$bootstrap" '. + {bootstrap: $b}')
        fi

        if [[ -n "$bootstrap_file" ]]; then
            if [[ ! -f "$bootstrap_file" ]]; then
                echo -e "${RED}Error: Bootstrap file not found: $bootstrap_file${RESET}" >&2
                return 1
            fi
            local file_content=$(cat "$bootstrap_file")
            payload=$(echo "$payload" | jq --arg b "$file_content" '. + {bootstrap_content: $b}')
        fi

        # Add input files
        if [[ ${#input_files[@]} -gt 0 ]]; then
            local files_json="["
            for file in "${input_files[@]}"; do
                if [[ ! -f "$file" ]]; then
                    echo -e "${RED}Error: Input file not found: $file${RESET}" >&2
                    exit 1
                fi
                local filename=$(basename "$file")
                local content_b64=$(base64 -w0 < "$file")
                files_json+="{\"filename\":\"$filename\",\"content_base64\":\"$content_b64\"},"
            done
            files_json="${files_json%,}]"
            payload=$(echo "$payload" | jq --argjson files "$files_json" '. + {input_files: $files}')
        fi

        [[ -n "$network" ]] && payload=$(echo "$payload" | jq --arg n "$network" '. + {network: $n}')
        [[ -n "$vcpu" ]] && payload=$(echo "$payload" | jq --argjson v "$vcpu" '. + {vcpu: $v}')

        local result=$(api_request "/services" "POST" "$payload" "$api_key")
        local service_id=$(echo "$result" | jq -r '.id // "N/A"')
        local service_name=$(echo "$result" | jq -r '.name // "N/A"')
        local service_url=$(echo "$result" | jq -r '.url // ""')

        echo -e "${GREEN}Service created: $service_id${RESET}"
        echo "Name: $service_name"
        [[ -n "$service_url" ]] && echo "URL: $service_url"

        # Set environment vault if -e or --env-file provided
        if [[ "$service_id" != "N/A" ]]; then
            local env_content=$(build_env_content "$env_file" "${env_vars[@]}")
            if [[ -n "$env_content" ]]; then
                echo -e "${YELLOW}Setting environment vault...${RESET}" >&2
                if ! service_env_set "$service_id" "$env_content" "$api_key"; then
                    echo -e "${YELLOW}Warning: Failed to set environment vault${RESET}" >&2
                fi
            fi
        fi
        return
    fi

    echo -e "${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}" >&2
    exit 1
}

cmd_snapshot() {
    local api_key="${UNSANDBOX_API_KEY:-}"
    local list=false
    local info=""
    local delete=""
    local clone=""
    local clone_type=""
    local clone_name=""
    local clone_shell=""
    local clone_ports=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -l|--list)
                list=true
                shift
                ;;
            --info)
                info="$2"
                shift 2
                ;;
            --delete)
                delete="$2"
                shift 2
                ;;
            --clone)
                clone="$2"
                shift 2
                ;;
            --type)
                clone_type="$2"
                shift 2
                ;;
            --name)
                clone_name="$2"
                shift 2
                ;;
            --shell)
                clone_shell="$2"
                shift 2
                ;;
            --ports)
                clone_ports="$2"
                shift 2
                ;;
            -k)
                api_key="$2"
                shift 2
                ;;
            -*)
                echo -e "${RED}Unknown option: $1${RESET}" >&2
                exit 1
                ;;
            *)
                shift
                ;;
        esac
    done

    if [[ "$list" == true ]]; then
        local result=$(api_request "/snapshots" "GET" "" "$api_key")
        echo "$result" | jq '.'
        return
    fi

    if [[ -n "$info" ]]; then
        local result=$(api_request "/snapshots/$info" "GET" "" "$api_key")
        echo "$result" | jq '.'
        return
    fi

    if [[ -n "$delete" ]]; then
        api_request "/snapshots/$delete" "DELETE" "" "$api_key" > /dev/null
        echo -e "${GREEN}Snapshot deleted successfully${RESET}"
        return
    fi

    if [[ -n "$clone" ]]; then
        if [[ -z "$clone_type" ]]; then
            echo -e "${RED}Error: --type required with --clone (session or service)${RESET}" >&2
            exit 1
        fi

        local payload=$(jq -n --arg type "$clone_type" '{type: $type}')
        [[ -n "$clone_name" ]] && payload=$(echo "$payload" | jq --arg n "$clone_name" '. + {name: $n}')
        [[ -n "$clone_shell" ]] && payload=$(echo "$payload" | jq --arg s "$clone_shell" '. + {shell: $s}')
        if [[ -n "$clone_ports" ]]; then
            local ports_json="[$(echo "$clone_ports" | sed 's/,/,/g')]"
            payload=$(echo "$payload" | jq --argjson p "$ports_json" '. + {ports: $p}')
        fi

        echo -e "${YELLOW}Cloning snapshot $clone to create new $clone_type...${RESET}"
        local result=$(api_request "/snapshots/$clone/clone" "POST" "$payload" "$api_key")

        if [[ "$clone_type" == "session" ]]; then
            local session_id=$(echo "$result" | jq -r '.session_id // "N/A"')
            echo -e "${GREEN}Session created from snapshot${RESET}"
            echo "Session ID: $session_id"
        else
            local service_id=$(echo "$result" | jq -r '.service_id // "N/A"')
            echo -e "${GREEN}Service created from snapshot${RESET}"
            echo "Service ID: $service_id"
        fi
        return
    fi

    echo -e "${RED}Error: Specify --list, --info, --delete, or --clone${RESET}" >&2
    exit 1
}

validate_key() {
    local api_key="$1"
    local extend_mode="$2"

    if [[ -z "$api_key" ]]; then
        echo -e "${RED}Error: API key not provided. Use -k flag or set UNSANDBOX_API_KEY${RESET}" >&2
        exit 1
    fi

    # Get keys - api_key might be public key or legacy format
    local public_key="${UNSANDBOX_PUBLIC_KEY:-$api_key}"
    local secret_key="${UNSANDBOX_SECRET_KEY:-}"

    # Generate HMAC signature for portal request
    local timestamp=$(date +%s)
    local endpoint="/keys/validate"
    local body=""
    local sig_input="${timestamp}:POST:${endpoint}:${body}"
    local signature=""

    if [[ -n "$secret_key" ]]; then
        signature=$(echo -n "$sig_input" | openssl dgst -sha256 -hmac "$secret_key" | sed 's/^.* //')
    fi

    # Call portal validation endpoint
    local response
    local http_code
    if [[ -n "$signature" ]]; then
        response=$(curl -s -w "\n%{http_code}" -X POST "${PORTAL_BASE}${endpoint}" \
            -H "Authorization: Bearer $public_key" \
            -H "X-Timestamp: $timestamp" \
            -H "X-Signature: $signature" \
            -H "Content-Type: application/json" 2>&1)
    else
        response=$(curl -s -w "\n%{http_code}" -X POST "${PORTAL_BASE}${endpoint}" \
            -H "Authorization: Bearer $public_key" \
            -H "Content-Type: application/json" 2>&1)
    fi

    http_code=$(echo "$response" | tail -n1)
    local body=$(echo "$response" | head -n-1)

    if [[ "$http_code" -eq 200 ]]; then
        # Valid key - parse response
        if command -v jq &> /dev/null; then
            # Use jq for parsing
            local valid=$(echo "$body" | jq -r '.valid // false')
            local public_key=$(echo "$body" | jq -r '.public_key // "N/A"')
            local tier=$(echo "$body" | jq -r '.tier // "N/A"')
            local expires_at=$(echo "$body" | jq -r '.expires_at // "N/A"')
            local expired=$(echo "$body" | jq -r '.expired // false')

            if [[ "$expired" == "true" ]]; then
                echo -e "${RED}Expired${RESET}"
                echo "Public Key: $public_key"
                echo "Tier: $tier"
                echo "Expired: $expires_at"
                echo -e "${YELLOW}To renew: Visit ${PORTAL_BASE}/keys/extend${RESET}"
                exit 1
            else
                echo -e "${GREEN}Valid${RESET}"
                echo "Public Key: $public_key"
                echo "Tier: $tier"
                echo "Expires: $expires_at"

                # If extend mode, open browser
                if [[ "$extend_mode" == "true" ]]; then
                    local extend_url="${PORTAL_BASE}/keys/extend?pk=${public_key}"
                    echo -e "\n${BLUE}Opening browser to extend key...${RESET}"

                    # Detect platform and open browser
                    if command -v xdg-open &> /dev/null; then
                        xdg-open "$extend_url" &> /dev/null
                    elif command -v open &> /dev/null; then
                        open "$extend_url" &> /dev/null
                    elif command -v start &> /dev/null; then
                        start "$extend_url" &> /dev/null
                    else
                        echo -e "${YELLOW}Cannot detect browser opener. Visit: $extend_url${RESET}"
                    fi
                fi
            fi
        else
            # Fallback: use grep/sed for parsing (no jq available)
            local valid=$(echo "$body" | grep -o '"valid"[[:space:]]*:[[:space:]]*[^,}]*' | sed 's/.*:[[:space:]]*//' | tr -d ' "')
            local public_key=$(echo "$body" | grep -o '"public_key"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*:[[:space:]]*"//' | tr -d '"')
            local tier=$(echo "$body" | grep -o '"tier"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*:[[:space:]]*"//' | tr -d '"')
            local expires_at=$(echo "$body" | grep -o '"expires_at"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*:[[:space:]]*"//' | tr -d '"')
            local expired=$(echo "$body" | grep -o '"expired"[[:space:]]*:[[:space:]]*[^,}]*' | sed 's/.*:[[:space:]]*//' | tr -d ' "')

            [[ -z "$public_key" ]] && public_key="N/A"
            [[ -z "$tier" ]] && tier="N/A"
            [[ -z "$expires_at" ]] && expires_at="N/A"

            if [[ "$expired" == "true" ]]; then
                echo -e "${RED}Expired${RESET}"
                echo "Public Key: $public_key"
                echo "Tier: $tier"
                echo "Expired: $expires_at"
                echo -e "${YELLOW}To renew: Visit ${PORTAL_BASE}/keys/extend${RESET}"
                exit 1
            else
                echo -e "${GREEN}Valid${RESET}"
                echo "Public Key: $public_key"
                echo "Tier: $tier"
                echo "Expires: $expires_at"

                # If extend mode, open browser
                if [[ "$extend_mode" == "true" ]]; then
                    local extend_url="${PORTAL_BASE}/keys/extend?pk=${public_key}"
                    echo -e "\n${BLUE}Opening browser to extend key...${RESET}"

                    # Detect platform and open browser
                    if command -v xdg-open &> /dev/null; then
                        xdg-open "$extend_url" &> /dev/null
                    elif command -v open &> /dev/null; then
                        open "$extend_url" &> /dev/null
                    elif command -v start &> /dev/null; then
                        start "$extend_url" &> /dev/null
                    else
                        echo -e "${YELLOW}Cannot detect browser opener. Visit: $extend_url${RESET}"
                    fi
                fi
            fi
        fi
    else
        # Invalid key or error
        if command -v jq &> /dev/null; then
            local error=$(echo "$body" | jq -r '.error // "Unknown error"')
            echo -e "${RED}Invalid${RESET}"
            echo "Error: $error"
        else
            # Fallback
            local error=$(echo "$body" | grep -o '"error"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*:[[:space:]]*"//' | tr -d '"')
            [[ -z "$error" ]] && error="Unknown error (HTTP $http_code)"
            echo -e "${RED}Invalid${RESET}"
            echo "Error: $error"
        fi
        exit 1
    fi
}

cmd_key() {
    local api_key="${UNSANDBOX_API_KEY:-${UNSANDBOX_PUBLIC_KEY:-}}"
    local extend=false

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -k)
                api_key="$2"
                shift 2
                ;;
            --extend)
                extend=true
                shift
                ;;
            -*)
                echo -e "${RED}Unknown option: $1${RESET}" >&2
                exit 1
                ;;
            *)
                shift
                ;;
        esac
    done

    validate_key "$api_key" "$extend"
}

# Main
show_help() {
    cat <<EOF
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  $0 [options] <source_file>
  $0 session [options]
  $0 service [options]
  $0 snapshot [options]
  $0 key [options]

Execute options:
  -e KEY=VALUE      Environment variable (multiple allowed)
  -f FILE          Input file (multiple allowed)
  -a               Return artifacts
  -o DIR           Output directory for artifacts
  -n MODE          Network mode (zerotrust|semitrusted)
  -v N             vCPU count (1-8)
  -k KEY           API key

Session options:
  -s, --shell NAME  Shell/REPL (default: bash)
  -l, --list       List sessions
  --attach ID      Attach to session
  --kill ID        Terminate session
  --audit          Record session
  --tmux           Enable tmux persistence
  --screen         Enable screen persistence
  --snapshot SESSION_ID  Create snapshot of session
  --restore SNAPSHOT_ID  Restore from snapshot ID
  --snapshot-name NAME  Optional name for snapshot
  --hot            Take snapshot without freezing (live snapshot)

Service options:
  --name NAME      Service name
  --ports PORTS    Comma-separated ports
  --domains DOMAINS Custom domains
  --type TYPE      Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)
  --bootstrap CMD  Bootstrap command or URI
  --bootstrap-file FILE  Upload local file as bootstrap script
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID        Unfreeze service
  --destroy ID     Destroy service
  --resize ID      Resize service (requires -v)
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
  --dump-bootstrap ID  Dump bootstrap script
  --dump-file FILE     File to save bootstrap (with --dump-bootstrap)
  --snapshot SERVICE_ID  Create snapshot of service
  --restore SNAPSHOT_ID  Restore from snapshot ID
  --snapshot-name NAME  Optional name for snapshot
  --hot            Take snapshot without freezing (live snapshot)

Snapshot options:
  -l, --list       List all snapshots
  --info ID        Get snapshot details
  --delete ID      Delete a snapshot
  --clone ID       Clone snapshot to new session/service (--type required)
  --type TYPE      Type for clone: session or service
  --name NAME      Name for cloned session/service
  --shell NAME     Shell for cloned session
  --ports PORTS    Ports for cloned service

Key options:
  -k KEY           API key to validate
  --extend         Validate and open browser to extend key
EOF
}

# Handle help and no args
if [[ $# -eq 0 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    show_help
    exit 0
fi

# Route to command
if [[ "$1" == "session" ]]; then
    shift
    cmd_session "$@"
elif [[ "$1" == "service" ]]; then
    shift
    # Check for env subcommand: service env <action> <target>
    if [[ "${1:-}" == "env" ]]; then
        shift
        cmd_service_env "$@"
    else
        cmd_service "$@"
    fi
elif [[ "$1" == "snapshot" ]]; then
    shift
    cmd_snapshot "$@"
elif [[ "$1" == "key" ]]; then
    shift
    cmd_key "$@"
else
    cmd_execute "$@"
fi
