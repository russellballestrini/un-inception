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

# Test suite for Fortran Unsandbox SDK
# Run: bash tests/test_un.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SDK_DIR="$SCRIPT_DIR/../sync/src"
SOURCE="$SDK_DIR/un.f90"

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

echo ""
echo "=== Source File ==="
test_that "Source file exists" "[ -f '$SOURCE' ]"

echo ""
echo "=== Command Handlers ==="
test_that "Session handler defined" "grep -q 'subroutine handle_session' '$SOURCE'"
test_that "Service handler defined" "grep -q 'subroutine handle_service' '$SOURCE'"
test_that "Snapshot handler defined" "grep -q 'subroutine handle_snapshot' '$SOURCE'"
test_that "Image handler defined" "grep -q 'subroutine handle_image' '$SOURCE'"
test_that "Key handler defined" "grep -q 'subroutine handle_key' '$SOURCE'"
test_that "Languages handler defined" "grep -q 'subroutine handle_languages' '$SOURCE'"

echo ""
echo "=== Command Dispatch ==="
test_that "Session dispatch" "grep -q \"trim(arg) == 'session'\" '$SOURCE'"
test_that "Service dispatch" "grep -q \"trim(arg) == 'service'\" '$SOURCE'"
test_that "Snapshot dispatch" "grep -q \"trim(arg) == 'snapshot'\" '$SOURCE'"
test_that "Image dispatch" "grep -q \"trim(arg) == 'image'\" '$SOURCE'"
test_that "Key dispatch" "grep -q \"trim(arg) == 'key'\" '$SOURCE'"
test_that "Languages dispatch" "grep -q \"trim(arg) == 'languages'\" '$SOURCE'"

echo ""
echo "=== Snapshot Operations ==="
test_that "Snapshot --list" "grep -q \"operation = 'list'\" '$SOURCE' || grep -q 'list_mode' '$SOURCE'"
test_that "Snapshot --info" "grep -q \"operation = 'info'\" '$SOURCE'"
test_that "Snapshot --delete" "grep -q \"operation = 'delete'\" '$SOURCE'"
test_that "Snapshot --lock" "grep -q \"operation = 'lock'\" '$SOURCE'"
test_that "Snapshot --unlock" "grep -q \"operation = 'unlock'\" '$SOURCE'"
test_that "Snapshot --restore" "grep -q \"operation = 'restore'\" '$SOURCE'"
test_that "Snapshot --clone" "grep -q \"operation = 'clone'\" '$SOURCE'"

echo ""
echo "=== Help Text ==="
test_that "Help shows snapshot" "grep -q 'snapshot' '$SOURCE'"
test_that "Help shows --list" "grep -q '\\-\\-list' '$SOURCE'"
test_that "Help shows --info" "grep -q '\\-\\-info' '$SOURCE'"
test_that "Help shows --restore" "grep -q '\\-\\-restore' '$SOURCE'"
test_that "Help shows --clone" "grep -q '\\-\\-clone' '$SOURCE'"

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
echo "=== Module Structure ==="
test_that "Has unsandbox_sdk module" "grep -q 'module unsandbox_sdk' '$SOURCE'"
test_that "Has unsandbox_client type" "grep -q 'type :: unsandbox_client' '$SOURCE'"
test_that "Has execution_result type" "grep -q 'type :: execution_result' '$SOURCE'"

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
