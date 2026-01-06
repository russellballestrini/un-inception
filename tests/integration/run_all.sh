#!/bin/bash
# Run all integration tests for UN CLI implementations
# These tests verify component interactions without making real API calls

set -o pipefail

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

cd "$(dirname "$0")"

echo -e "${CYAN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║         UN CLI Inception - Integration Tests                 ║${NC}"
echo -e "${CYAN}║         Testing component interactions, no real API calls    ║${NC}"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""

passed=0
failed=0
skipped=0

run_test() {
    local name="$1"
    local cmd="$2"
    local interpreter="$3"

    printf "%-30s" "$name"

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

echo -e "${CYAN}━━━ Request Building Tests ━━━${NC}"
run_test "Python Request Building" "python3 test_request_building.py" "python3"
echo ""

# Summary
echo -e "${CYAN}══════════════════════════════════════════════════════════════${NC}"
echo ""
total=$((passed + failed + skipped))
echo -e "Results: ${GREEN}$passed PASS${NC} | ${RED}$failed FAIL${NC} | ${YELLOW}$skipped SKIP${NC} | Total: $total"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}All integration tests passed.${NC}"
    exit 0
else
    echo -e "${RED}$failed test(s) failed.${NC}"
    exit 1
fi
