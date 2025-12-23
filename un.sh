# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - Source code must be open source & freely distributed
#   FREEDOM  - Voluntary participation without corporate control
#   HARMONY  - Systems operating with minimal waste that self-renew
#   LOVE     - Individual rights protected while fostering cooperation
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
    local api_key="${4:-${UNSANDBOX_API_KEY}}"

    if [[ -z "$api_key" ]]; then
        echo -e "${RED}Error: UNSANDBOX_API_KEY not set${RESET}" >&2
        exit 1
    fi

    local url="${API_BASE}${endpoint}"
    local tmpfile=$(mktemp)

    if [[ -n "$data" ]]; then
        local response
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $api_key" \
            -H "Content-Type: application/json" \
            -d "$data" 2>&1)
    else
        local response
        response=$(curl -s -w "\n%{http_code}" -X "$method" "$url" \
            -H "Authorization: Bearer $api_key" \
            -H "Content-Type: application/json" 2>&1)
    fi

    local http_code=$(echo "$response" | tail -n1)
    local body=$(echo "$response" | head -n-1)

    if [[ "$http_code" -lt 200 || "$http_code" -ge 300 ]]; then
        echo -e "${RED}Error: HTTP $http_code - $body${RESET}" >&2
        exit 1
    fi

    echo "$body"
}

cmd_execute() {
    local source_file=""
    local -a env_vars=()
    local -a input_files=()
    local artifacts=false
    local output_dir="."
    local network=""
    local vcpu=""
    local api_key="${UNSANDBOX_API_KEY}"

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
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
            *)
                source_file="$1"
                shift
                ;;
        esac
    done

    if [[ ! -f "$source_file" ]]; then
        echo -e "${RED}Error: File not found: $source_file${RESET}" >&2
        exit 1
    fi

    local code=$(cat "$source_file")
    local language=$(detect_language "$source_file")

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
    local api_key="${UNSANDBOX_API_KEY}"

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

    echo -e "${YELLOW}Creating session...${RESET}"
    local result=$(api_request "/sessions" "POST" "$payload" "$api_key")
    local session_id=$(echo "$result" | jq -r '.id // "N/A"')
    echo -e "${GREEN}Session created: $session_id${RESET}"
    echo -e "${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}"
}

cmd_service() {
    local name=""
    local ports=""
    local domains=""
    local bootstrap=""
    local list=false
    local info=""
    local logs=""
    local tail=""
    local sleep=""
    local wake=""
    local destroy=""
    local execute=""
    local command=""
    local network=""
    local vcpu=""
    local api_key="${UNSANDBOX_API_KEY}"

    while [[ $# -gt 0 ]]; do
        case "$1" in
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
            --bootstrap)
                bootstrap="$2"
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
            --sleep)
                sleep="$2"
                shift 2
                ;;
            --wake)
                wake="$2"
                shift 2
                ;;
            --destroy)
                destroy="$2"
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
        api_request "/services/$sleep/sleep" "POST" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service sleeping: $sleep${RESET}"
        return
    fi

    if [[ -n "$wake" ]]; then
        api_request "/services/$wake/wake" "POST" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service waking: $wake${RESET}"
        return
    fi

    if [[ -n "$destroy" ]]; then
        api_request "/services/$destroy" "DELETE" "" "$api_key" > /dev/null
        echo -e "${GREEN}Service destroyed: $destroy${RESET}"
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

        if [[ -n "$bootstrap" ]]; then
            if [[ -f "$bootstrap" ]]; then
                local bootstrap_content=$(cat "$bootstrap")
                payload=$(echo "$payload" | jq --arg b "$bootstrap_content" '. + {bootstrap: $b}')
            else
                payload=$(echo "$payload" | jq --arg b "$bootstrap" '. + {bootstrap: $b}')
            fi
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
        return
    fi

    echo -e "${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}" >&2
    exit 1
}

# Main
show_help() {
    cat <<EOF
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  $0 [options] <source_file>
  $0 session [options]
  $0 service [options]

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

Service options:
  --name NAME      Service name
  --ports PORTS    Comma-separated ports
  --domains DOMAINS Custom domains
  --bootstrap CMD  Bootstrap command/file
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --sleep ID       Freeze service
  --wake ID        Unfreeze service
  --destroy ID     Destroy service
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
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
    cmd_service "$@"
else
    cmd_execute "$@"
fi
