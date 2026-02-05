#!/bin/bash
# Test suite for Prolog Unsandbox SDK
# Run: bash tests/test_un.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SDK_DIR="$SCRIPT_DIR/../sync/src"
SOURCE="$SDK_DIR/un.pro"

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
echo "=== Command Handlers ==="
test_that "Session handler defined" "grep -q 'handle_session' '$SOURCE'"
test_that "Service handler defined" "grep -q 'handle_service' '$SOURCE'"
test_that "Snapshot handler defined" "grep -q 'handle_snapshot' '$SOURCE'"
test_that "Image handler defined" "grep -q 'handle_image' '$SOURCE'"
test_that "Languages handler defined" "grep -q 'handle_languages' '$SOURCE'"
test_that "Key handler defined" "grep -q 'handle_key' '$SOURCE'"

echo ""
echo "=== Snapshot Operations ==="
test_that "Snapshot list implemented" "grep -q \"handle_snapshot_cmd.*--list\" '$SOURCE'"
test_that "Snapshot info implemented" "grep -q \"handle_snapshot_cmd.*--info\" '$SOURCE'"
test_that "Snapshot delete implemented" "grep -q \"handle_snapshot_cmd.*--delete\" '$SOURCE'"
test_that "Snapshot lock implemented" "grep -q \"handle_snapshot_cmd.*--lock\" '$SOURCE'"
test_that "Snapshot unlock implemented" "grep -q \"handle_snapshot_cmd.*--unlock\" '$SOURCE'"
test_that "Snapshot restore implemented" "grep -q \"handle_snapshot_cmd.*--restore\" '$SOURCE'"
test_that "Snapshot clone implemented" "grep -q \"handle_snapshot_cmd.*--clone\" '$SOURCE'"

echo ""
echo "=== Snapshot Clone Helper ==="
test_that "Clone args parser defined" "grep -q 'parse_snapshot_clone_args' '$SOURCE'"
test_that "Clone body builder defined" "grep -q 'build_snapshot_clone_body' '$SOURCE'"

echo ""
echo "=== Language Detection ==="
test_that "Python detection" "grep -q \"ext_lang.*py.*python\" '$SOURCE'"
test_that "JavaScript detection" "grep -q \"ext_lang.*js.*javascript\" '$SOURCE'"
test_that "Julia detection" "grep -q \"ext_lang.*jl.*julia\" '$SOURCE'"
test_that "R detection" "grep -q \"ext_lang.*\\.r.*r\" '$SOURCE'"
test_that "Fortran detection" "grep -q \"ext_lang.*f90.*fortran\" '$SOURCE'"
test_that "COBOL detection" "grep -q \"ext_lang.*cob.*cobol\" '$SOURCE'"
test_that "Prolog detection" "grep -q \"ext_lang.*pro.*prolog\" '$SOURCE'"

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
echo "=== Constants ==="
test_that "Portal base defined" "grep -q 'portal_base' '$SOURCE'"
test_that "Languages cache TTL defined" "grep -q 'languages_cache_ttl' '$SOURCE'"

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
