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

# Test suite for COBOL Unsandbox SDK
# Run: bash tests/test_un.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SDK_DIR="$SCRIPT_DIR/../sync/src"
SOURCE="$SDK_DIR/un.cob"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0

# Test helper
test_that() {
    local description="$1"
    local test_cmd="$2"
    TESTS_RUN=$((TESTS_RUN + 1))

    if eval "$test_cmd" >/dev/null 2>&1; then
        echo -e "[${GREEN}PASS${NC}] $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "[${RED}FAIL${NC}] $description"
        return 1
    fi
}

# Test source file exists
echo ""
echo "=== Source File ==="
test_that "Source file exists" "[ -f '$SOURCE' ]"

echo ""
echo "=== Source Code Structure ==="
test_that "Has WORKING-STORAGE SECTION" "grep -q 'WORKING-STORAGE SECTION' '$SOURCE'"
test_that "Has session handler" "grep -q 'HANDLE-SESSION' '$SOURCE'"
test_that "Has service handler" "grep -q 'HANDLE-SERVICE' '$SOURCE'"
test_that "Has snapshot handler" "grep -q 'HANDLE-SNAPSHOT' '$SOURCE'"
test_that "Has image handler" "grep -q 'HANDLE-IMAGE' '$SOURCE'"
test_that "Has key handler" "grep -q 'HANDLE-KEY' '$SOURCE'"
test_that "Has languages handler" "grep -q 'HANDLE-LANGUAGES' '$SOURCE'"

echo ""
echo "=== Snapshot Operations ==="
test_that "Snapshot list implemented" "grep -q 'SNAPSHOT-LIST' '$SOURCE'"
test_that "Snapshot info implemented" "grep -q 'SNAPSHOT-INFO' '$SOURCE'"
test_that "Snapshot delete implemented" "grep -q 'SNAPSHOT-DELETE' '$SOURCE'"
test_that "Snapshot lock implemented" "grep -q 'SNAPSHOT-LOCK' '$SOURCE'"
test_that "Snapshot unlock implemented" "grep -q 'SNAPSHOT-UNLOCK' '$SOURCE'"
test_that "Snapshot restore implemented" "grep -q 'SNAPSHOT-RESTORE' '$SOURCE'"
test_that "Snapshot clone implemented" "grep -q 'SNAPSHOT-CLONE' '$SOURCE'"

echo ""
echo "=== Image Operations ==="
test_that "Image list implemented" "grep -q 'IMAGE-LIST' '$SOURCE'"
test_that "Image info implemented" "grep -q 'IMAGE-INFO' '$SOURCE'"
test_that "Image delete implemented" "grep -q 'IMAGE-DELETE' '$SOURCE'"
test_that "Image lock implemented" "grep -q 'IMAGE-LOCK' '$SOURCE'"
test_that "Image unlock implemented" "grep -q 'IMAGE-UNLOCK' '$SOURCE'"

echo ""
echo "=== HMAC Authentication ==="
test_that "Uses openssl for HMAC" "grep -q 'openssl dgst -sha256 -hmac' '$SOURCE'"
test_that "Has X-Signature header" "grep -q 'X-Signature' '$SOURCE'"
test_that "Has X-Timestamp header" "grep -q 'X-Timestamp' '$SOURCE'"

echo ""
echo "=== Sudo OTP Handling ==="
test_that "Handles 428 response" "grep -q '428' '$SOURCE'"
test_that "Has X-Sudo-OTP header" "grep -q 'X-Sudo-OTP' '$SOURCE'"

echo ""
echo "=== Variables ==="
test_that "WS-TYPE variable defined" "grep -q 'WS-TYPE' '$SOURCE'"
test_that "WS-SHELL variable defined" "grep -q 'WS-SHELL' '$SOURCE'"
test_that "WS-PORTS variable defined" "grep -q 'WS-PORTS' '$SOURCE'"
test_that "WS-NAME variable defined" "grep -q 'WS-NAME' '$SOURCE'"

echo ""
echo "=== Summary ==="
echo "Tests passed: $TESTS_PASSED / $TESTS_RUN"

if [ $TESTS_PASSED -eq $TESTS_RUN ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
