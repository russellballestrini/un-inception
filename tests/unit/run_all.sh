#!/bin/bash
# Run all unit tests for UN CLI implementations
# These tests do NOT call the API - they test internal logic only

set -o pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

cd "$(dirname "$0")"

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║           UN CLI Inception - Unit Tests                      ║${NC}"
echo -e "${CYAN}║           Testing internal logic, no API calls               ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

passed=0
failed=0
skipped=0

run_test() {
    local name="$1"
    local cmd="$2"
    local interpreter="$3"

    printf "%-20s" "$name"

    if [ -n "$interpreter" ] && ! command -v "$interpreter" &> /dev/null; then
        echo -e "${YELLOW}SKIP${NC} ($interpreter not found)"
        skipped=$((skipped + 1))
        return
    fi

    if $cmd > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        passed=$((passed + 1))
    else
        echo -e "${RED}FAIL${NC}"
        failed=$((failed + 1))
    fi
}

echo -e "${CYAN}━━━ Scripting Languages ━━━${NC}"
run_test "Python" "python3 test_python.py" "python3"
run_test "JavaScript" "node test_javascript.js" "node"
run_test "Ruby" "ruby test_ruby.rb" "ruby"
run_test "Lua" "lua test_lua.lua" "lua"
run_test "Bash" "bash test_bash.sh" "bash"
echo ""

echo -e "${CYAN}━━━ Systems Languages ━━━${NC}"
run_test "Go" "go run test_go.go" "go"
echo ""

# Summary
echo -e "${CYAN}══════════════════════════════════════════════════════════════${NC}"
echo ""
total=$((passed + failed + skipped))
echo -e "Results: ${GREEN}$passed PASS${NC} | ${RED}$failed FAIL${NC} | ${YELLOW}$skipped SKIP${NC} | Total: $total"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}All unit tests passed.${NC}"
    exit 0
else
    echo -e "${RED}$failed test(s) failed.${NC}"
    exit 1
fi
