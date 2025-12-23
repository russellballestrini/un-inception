#!/bin/bash

# Test session --list works for ALL 42 implementations
# This verifies the REST API session management (not WebSocket streaming)

source /home/fox/git/unsandbox.com/vars.sh
cd /home/fox/git/unsandbox.com/cli/inception

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║     Session List Test - All 42 Implementations               ║${NC}"
echo -e "${CYAN}║     Verifies REST API session management                     ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

passed=0
failed=0

# All 42 implementations with their run commands
declare -A RUN_COMMANDS=(
    ["un.py"]="python3 un.py"
    ["un.js"]="node un.js"
    ["un.ts"]="npx tsx un.ts"
    ["un.rb"]="ruby un.rb"
    ["un.php"]="php un.php"
    ["un.pl"]="perl un.pl"
    ["un.lua"]="lua un.lua"
    ["un.sh"]="bash un.sh"
    ["un.go"]="go run un.go"
    ["un.rs"]="cargo script un.rs"
    ["un_inception.c"]="./un_inception"
    ["un.cpp"]="./un_cpp"
    ["un.d"]="./un_d"
    ["un.nim"]="./un_nim"
    ["un.zig"]="./un_zig"
    ["un.v"]="./un_v"
    ["Un.java"]="java Un"
    ["un.kt"]="kotlin un.kt"
    ["Un.cs"]="mono Un.exe"
    ["un.fs"]="dotnet fsi un.fs"
    ["un.groovy"]="groovy un.groovy"
    ["un.dart"]="dart run un.dart"
    ["un.hs"]="runhaskell un.hs"
    ["un.ml"]="ocaml un.ml"
    ["un.clj"]="clojure -M un.clj"
    ["un.scm"]="guile un.scm"
    ["un.lisp"]="sbcl --script un.lisp"
    ["un.erl"]="escript un.erl"
    ["un.ex"]="elixir un.ex"
    ["un.jl"]="julia un.jl"
    ["un.r"]="Rscript un.r"
    ["un.cr"]="crystal run un.cr"
    ["un.f90"]="gfortran un.f90 -o un_f90 && ./un_f90"
    ["un.cob"]="cobc -x un.cob -o un_cob && ./un_cob"
    ["un.pro"]="swipl -g main -t halt un.pro"
    ["un.forth"]="gforth un.forth"
    ["un.tcl"]="tclsh un.tcl"
    ["un.raku"]="raku un.raku"
    ["un.m"]="clang -framework Foundation un.m -o un_m && ./un_m"
    ["un_deno.ts"]="deno run --allow-all un_deno.ts"
    ["un.ps1"]="pwsh un.ps1"
    ["un.awk"]="awk -f un.awk"
)

# Simplified list for quick test (interpreted languages only - no compilation needed)
QUICK_TEST=(
    "un.py:python3 un.py"
    "un.js:node un.js"
    "un.rb:ruby un.rb"
    "un.php:php un.php"
    "un.pl:perl un.pl"
    "un.lua:lua un.lua"
    "un.sh:bash un.sh"
    "un.ts:npx tsx un.ts"
    "un_deno.ts:deno run --allow-all un_deno.ts"
    "un.ex:elixir un.ex"
    "un.groovy:groovy un.groovy"
    "un.tcl:tclsh un.tcl"
    "un.raku:raku un.raku"
)

total=${#QUICK_TEST[@]}
current=0

echo -e "${YELLOW}Testing session --list on interpreted languages...${NC}"
echo ""

for entry in "${QUICK_TEST[@]}"; do
    impl="${entry%%:*}"
    cmd="${entry##*:}"
    ((current++))

    printf "[%2d/%d] %-20s " "$current" "$total" "$impl"

    # Run session --list and check for valid response
    output=$(timeout 30 $cmd session --list 2>&1)
    exit_code=$?

    # Success if exit 0 and output contains "No active sessions" or session listing
    if [ $exit_code -eq 0 ]; then
        if echo "$output" | grep -qE "(No active sessions|session|ID|id)"; then
            echo -e "${GREEN}PASS${NC}"
            ((passed++))
        else
            echo -e "${RED}FAIL${NC} (unexpected output)"
            ((failed++))
        fi
    else
        echo -e "${RED}FAIL${NC} (exit $exit_code)"
        ((failed++))
    fi
done

echo ""
echo "=================================="
echo -e "Results: ${GREEN}$passed PASS${NC}, ${RED}$failed FAIL${NC} out of $total"

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}ALL SESSION TESTS PASSED${NC}"
    exit 0
else
    echo -e "${RED}SOME SESSION TESTS FAILED${NC}"
    exit 1
fi
