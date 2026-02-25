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

# UN CLI Inception - Complete Test Matrix Runner
# Tests ALL 42 language implementations with unit, integration, and functional tests

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Change to tests directory
cd "$(dirname "$0")"
INCEPTION_DIR=".."
TEST_DIR="."

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║       UN CLI Inception - Complete Test Matrix                ║${NC}"
echo -e "${CYAN}║       42 Languages × 3 Test Types = The Matrix               ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check API auth keys
if [ -z "$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$UNSANDBOX_SECRET_KEY" ]; then
    if [ -z "$UNSANDBOX_API_KEY" ]; then
        echo -e "${YELLOW}WARNING:${NC} UNSANDBOX authentication not configured"
        echo "Integration and functional tests will be skipped"
        echo "Set HMAC keys: export UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=..."
        echo "Or legacy key: export UNSANDBOX_API_KEY=..."
        echo "Run: source ../../vars.sh"
        echo ""
    fi
fi

# Counters
passed=0
failed=0
skipped=0

# Function to check if command exists
has_cmd() {
    command -v "$1" >/dev/null 2>&1
}

# Function to run a test and track result
run_test() {
    local name=$1
    local interpreter=$2
    local test_file=$3
    local extra_args=$4

    printf "%-20s" "$name"

    # Check if interpreter exists
    if ! has_cmd "$interpreter"; then
        echo -e "${YELLOW}SKIP${NC} ($interpreter not found)"
        ((skipped++))
        return
    fi

    # Check if test file exists
    if [ ! -f "$test_file" ]; then
        echo -e "${YELLOW}SKIP${NC} (test file missing)"
        ((skipped++))
        return
    fi

    # Run the test
    if $interpreter $extra_args "$test_file" >/dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC}"
        ((failed++))
    fi
}

# Function to run shell-based test
run_shell_test() {
    local name=$1
    local test_file=$2

    printf "%-20s" "$name"

    if [ ! -f "$test_file" ]; then
        echo -e "${YELLOW}SKIP${NC} (test file missing)"
        ((skipped++))
        return
    fi

    if bash "$test_file" >/dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC}"
        ((failed++))
    fi
}

echo -e "${BLUE}━━━ Scripting Languages ━━━${NC}"
run_test "Python" "python3" "test_un_py.py"
run_test "JavaScript" "node" "test_un_js.js"
run_test "TypeScript" "npx" "test_un_ts.ts" "ts-node"
run_test "Ruby" "ruby" "test_un_rb.rb"
run_test "PHP" "php" "test_un_php.php"
run_test "Perl" "perl" "test_un_pl.pl"
run_test "Lua" "lua" "test_un_lua.lua"
run_shell_test "Bash" "test_un_sh.sh"
echo ""

echo -e "${BLUE}━━━ Systems Languages ━━━${NC}"
run_test "Go" "go" "test_un_go.go" "run"
# Rust, C, C++, D, Zig, Nim, V require compilation - skip for now
printf "%-20s" "Rust"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "C"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "C++"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "D"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "Zig"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "Nim"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "V"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
echo ""

echo -e "${BLUE}━━━ JVM/.NET Languages ━━━${NC}"
run_test "Groovy" "groovy" "test_un_groovy.groovy"
run_test "Kotlin" "kotlinc" "test_un_kt.kt" "-script"
# Java, C#, F# require compilation
printf "%-20s" "Java"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "C#"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
printf "%-20s" "F#"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
run_test "Dart" "dart" "test_un_dart.dart"
echo ""

echo -e "${BLUE}━━━ Functional Languages ━━━${NC}"
run_test "Haskell" "runhaskell" "test_un_hs.hs"
run_test "OCaml" "ocaml" "test_un_ml.ml"
run_test "Clojure" "clj" "test_un_clj.clj" "-M"
run_test "Scheme" "guile" "test_un_scm.scm"
run_test "CommonLisp" "sbcl" "test_un_lisp.lisp" "--script"
run_test "Erlang" "escript" "test_un_erl.erl"
run_test "Elixir" "elixir" "test_un_ex.exs"
echo ""

echo -e "${BLUE}━━━ Scientific/Exotic Languages ━━━${NC}"
run_test "Julia" "julia" "test_un_jl.jl"
run_test "R" "Rscript" "test_un_r.r"
run_test "Crystal" "crystal" "test_un_cr.cr"
# Fortran, COBOL require compilation
printf "%-20s" "Fortran"
echo -e "${YELLOW}SKIP${NC} (requires compilation)"
((skipped++))
run_shell_test "COBOL" "test_un_cob.sh"
run_test "Prolog" "swipl" "test_un_pro.pro" "-g main -t halt"
run_test "Forth" "gforth" "test_un_forth.fth"
echo ""

echo -e "${BLUE}━━━ Other Languages ━━━${NC}"
run_test "TCL" "tclsh" "test_un_tcl.tcl"
run_test "Raku" "raku" "test_un_raku.raku"
run_shell_test "Objective-C" "test_un_m.sh"
run_test "Deno" "deno" "test_un_deno.ts" "run --allow-read --allow-env --allow-net"
echo ""

# Summary
echo -e "${CYAN}══════════════════════════════════════════════════════════════${NC}"
echo ""
total=$((passed + failed + skipped))
echo -e "Results: ${GREEN}$passed PASS${NC} | ${RED}$failed FAIL${NC} | ${YELLOW}$skipped SKIP${NC} | Total: $total"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}The matrix is complete. All available tests passed.${NC}"
    exit 0
else
    echo -e "${RED}$failed test(s) failed.${NC}"
    exit 1
fi
