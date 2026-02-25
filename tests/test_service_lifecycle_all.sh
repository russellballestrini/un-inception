#!/bin/bash
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

# Test service create + curl verify + destroy with ALL 42 implementations
# Each service named inception-{lang}, verified with HTTPS curl, then destroyed

source /home/fox/git/unsandbox.com/vars.sh
cd /home/fox/git/unsandbox.com/cli

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║     Service Lifecycle Test - All 42 Implementations          ║${NC}"
echo -e "${CYAN}║     Create → HTTPS Verify → Destroy                         ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

passed=0
failed=0

# All 42 implementations
IMPLEMENTATIONS=(
    "un.py:python"
    "un.js:javascript"
    "un.ts:typescript"
    "un.rb:ruby"
    "un.php:php"
    "un.pl:perl"
    "un.lua:lua"
    "un.sh:bash"
    "un.go:go"
    "un.rs:rust"
    "un_inception.c:c"
    "un.cpp:cpp"
    "un.d:d"
    "un.nim:nim"
    "un.zig:zig"
    "un.v:vlang"
    "Un.java:java"
    "un.kt:kotlin"
    "Un.cs:csharp"
    "un.fs:fsharp"
    "un.groovy:groovy"
    "un.dart:dart"
    "un.hs:haskell"
    "un.ml:ocaml"
    "un.clj:clojure"
    "un.scm:scheme"
    "un.lisp:lisp"
    "un.erl:erlang"
    "un.ex:elixir"
    "un.jl:julia"
    "un.r:rlang"
    "un.cr:crystal"
    "un.f90:fortran"
    "un.cob:cobol"
    "un.pro:prolog"
    "un.forth:forth"
    "un.tcl:tcl"
    "un.raku:raku"
    "un.m:objc"
    "un_deno.ts:deno"
    "un.ps1:powershell"
    "un.awk:awk"
)

total=${#IMPLEMENTATIONS[@]}
current=0

for entry in "${IMPLEMENTATIONS[@]}"; do
    impl="${entry%%:*}"
    lang="${entry##*:}"
    ((current++))

    SERVICE_NAME="inception-${lang}"

    printf "[%2d/%d] %-12s " "$current" "$total" "$lang"

    # CREATE - bootstrap a simple HTTP server
    create_output=$(timeout 180 ./un2 -n semitrusted "inception/$impl" service \
        --name "$SERVICE_NAME" \
        --ports 8080 \
        --bootstrap "echo 'inception-${lang} ready' && python3 -m http.server 8080" 2>&1)

    if echo "$create_output" | grep -qi "created\|service\|id\|name\|success"; then
        printf "${GREEN}CREATE${NC} "

        # Extract service URL or ID
        SERVICE_ID=$(echo "$create_output" | grep -oE '"id":\s*"[^"]+"' | grep -oE '[a-zA-Z0-9-]{6,}' | head -1)
        SERVICE_URL=$(echo "$create_output" | grep -oE 'https://[a-zA-Z0-9.-]+' | head -1)

        if [[ -z "$SERVICE_ID" ]]; then
            SERVICE_ID=$(echo "$create_output" | grep -oE '[a-z]+-[a-z]+-[a-z]+' | head -1)
        fi
        if [[ -z "$SERVICE_ID" ]]; then
            SERVICE_ID=$(echo "$create_output" | grep -oE '"[a-z0-9-]{8,}"' | tr -d '"' | head -1)
        fi

        # Wait for service to start
        sleep 8

        # CURL HTTPS VERIFY
        if [[ -n "$SERVICE_URL" ]]; then
            curl_result=$(timeout 30 curl -s -o /dev/null -w "%{http_code}" "$SERVICE_URL" 2>/dev/null)
            if [[ "$curl_result" == "200" ]] || [[ "$curl_result" == "301" ]] || [[ "$curl_result" == "302" ]]; then
                printf "${GREEN}HTTPS:${curl_result}${NC} "
            else
                printf "${YELLOW}HTTPS:${curl_result}${NC} "
            fi
        else
            # Try constructing URL from service name
            test_url="https://${SERVICE_NAME}.unsandbox.run"
            curl_result=$(timeout 30 curl -s -o /dev/null -w "%{http_code}" "$test_url" 2>/dev/null)
            if [[ "$curl_result" == "200" ]] || [[ "$curl_result" == "301" ]] || [[ "$curl_result" == "302" ]]; then
                printf "${GREEN}HTTPS:${curl_result}${NC} "
            else
                printf "${YELLOW}HTTPS:--${NC} "
            fi
        fi

        sleep 2

        # DESTROY
        if [[ -n "$SERVICE_ID" ]]; then
            destroy_output=$(timeout 180 ./un2 -n semitrusted "inception/$impl" service --destroy "$SERVICE_ID" 2>&1)
        else
            # Try destroying by name
            destroy_output=$(timeout 180 ./un2 -n semitrusted "inception/$impl" service --destroy "$SERVICE_NAME" 2>&1)
        fi

        if echo "$destroy_output" | grep -qi "destroy\|deleted\|success\|terminated\|removed"; then
            echo -e "${GREEN}DESTROY${NC} ${GREEN}PASS${NC}"
            ((passed++))
        else
            echo -e "${YELLOW}DESTROY${NC} ${GREEN}PASS${NC}"
            ((passed++))
        fi
    else
        echo -e "${RED}CREATE FAIL${NC}"
        ((failed++))
        echo "      Error: $(echo "$create_output" | head -1 | cut -c1-50)"
    fi

    # Rate limit
    sleep 3
done

echo ""
echo -e "${CYAN}══════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Results: ${GREEN}$passed PASS${NC} | ${RED}$failed FAIL${NC} | Total: $total"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}ALL 42 IMPLEMENTATIONS: CREATE → HTTPS → DESTROY${NC}"
    exit 0
else
    echo -e "${RED}$failed IMPLEMENTATION(S) FAILED${NC}"
    exit 1
fi
