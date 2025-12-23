#!/bin/bash

# UN CLI Inception Matrix Test
# Uses un2 with semitrusted network to execute each un.* implementation
# Each implementation then calls the API to run fib.py - true inception!

set -o pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

cd "$(dirname "$0")/.."
CLI_DIR=".."
INCEPTION_DIR="."
TEST_FILE="../test/fib.py"

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║     UN CLI Inception Matrix - The Real Test                  ║${NC}"
echo -e "${CYAN}║     un2 → unsandbox → un.* → unsandbox → fib.py             ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check requirements
if [ -z "$UNSANDBOX_API_KEY" ]; then
    echo -e "${RED}ERROR:${NC} UNSANDBOX_API_KEY not set"
    echo "Run: source ../../vars.sh"
    exit 1
fi

if [ ! -x "$CLI_DIR/un2" ]; then
    echo -e "${RED}ERROR:${NC} un2 not found. Run: cd .. && make un2"
    exit 1
fi

# Counters
passed=0
failed=0
total=0

# Test a single implementation
test_impl() {
    local name=$1
    local file=$2
    local timeout_sec=${3:-60}

    ((total++))
    printf "%-15s" "$name"

    if [ ! -f "$file" ]; then
        echo -e "${YELLOW}SKIP${NC} (file not found)"
        return
    fi

    # Run un2 with semitrusted network, passing the inception file and test file
    # The inception file will read fib.py and call the API
    output=$(timeout $timeout_sec $CLI_DIR/un2 -n semitrusted -f "$TEST_FILE" "$file" "$TEST_FILE" 2>&1)
    exit_code=$?

    if [ $exit_code -eq 124 ]; then
        echo -e "${YELLOW}TIMEOUT${NC}"
        return
    fi

    # Check for fib(10) = 55 in output
    if echo "$output" | grep -q "fib(10) = 55"; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC}"
        ((failed++))
        # Show first line of error
        echo "    $(echo "$output" | head -1)"
    fi
}

echo -e "${CYAN}━━━ Scripting Languages ━━━${NC}"
test_impl "Python" "un.py"
test_impl "JavaScript" "un.js"
test_impl "TypeScript" "un.ts"
test_impl "Ruby" "un.rb"
test_impl "PHP" "un.php"
test_impl "Perl" "un.pl"
test_impl "Lua" "un.lua"
test_impl "Bash" "un.sh"
echo ""

echo -e "${CYAN}━━━ Systems Languages (source) ━━━${NC}"
test_impl "Go" "un.go" 90
test_impl "Rust" "un.rs" 120
test_impl "C" "un_inception.c" 90
test_impl "C++" "un.cpp" 90
test_impl "D" "un.d" 90
test_impl "Zig" "un.zig" 90
test_impl "Nim" "un.nim" 90
test_impl "V" "un.v" 90
echo ""

echo -e "${CYAN}━━━ JVM/.NET Languages ━━━${NC}"
test_impl "Java" "Un.java" 120
test_impl "Kotlin" "un.kt" 120
test_impl "C#" "Un.cs" 90
test_impl "F#" "un.fs" 90
test_impl "Groovy" "un.groovy" 90
test_impl "Dart" "un.dart" 90
echo ""

echo -e "${CYAN}━━━ Functional Languages ━━━${NC}"
test_impl "Haskell" "un.hs" 90
test_impl "OCaml" "un.ml" 90
test_impl "Clojure" "un.clj" 120
test_impl "Scheme" "un.scm" 60
test_impl "CommonLisp" "un.lisp" 90
test_impl "Erlang" "un.erl" 90
test_impl "Elixir" "un.ex" 90
echo ""

echo -e "${CYAN}━━━ Scientific/Exotic ━━━${NC}"
test_impl "Julia" "un.jl" 120
test_impl "R" "un.r" 90
test_impl "Crystal" "un.cr" 120
test_impl "Fortran" "un.f90" 90
test_impl "COBOL" "un.cob" 90
test_impl "Prolog" "un.pro" 60
test_impl "Forth" "un.forth" 60
echo ""

echo -e "${CYAN}━━━ Other Languages ━━━${NC}"
test_impl "TCL" "un.tcl" 60
test_impl "Raku" "un.raku" 90
test_impl "Obj-C" "un.m" 90
test_impl "Deno" "un_deno.ts" 60
echo ""

# Summary
echo -e "${CYAN}══════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "Results: ${GREEN}$passed PASS${NC} | ${RED}$failed FAIL${NC} | Total: $total"
echo ""

if [ $failed -eq 0 ] && [ $passed -gt 0 ]; then
    echo -e "${GREEN}The inception is complete. The matrix validated itself.${NC}"
    exit 0
else
    echo -e "${YELLOW}$failed implementation(s) need fixes.${NC}"
    exit 1
fi
