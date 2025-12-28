#!/usr/bin/env bash
# Test suite for un.sh (Bash implementation)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_SH="$SCRIPT_DIR/../un.sh"
TEST_DIR="$SCRIPT_DIR/../../test"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Test result tracking
test_passed() {
    TESTS_PASSED=$((TESTS_PASSED + 1))
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -e "${GREEN}✓ PASS${NC}: $1"
}

test_failed() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -e "${RED}✗ FAIL${NC}: $1"
    if [ -n "${2:-}" ]; then
        echo -e "${RED}  Error: $2${NC}"
    fi
}

test_skipped() {
    echo -e "${YELLOW}⊘ SKIP${NC}: $1"
}

# Unit Tests
echo -e "${BLUE}=== Unit Tests for un.sh ===${NC}"

# Test: Script exists and is executable
if [ -f "$UN_SH" ] && [ -x "$UN_SH" ]; then
    test_passed "Script exists and is executable"
else
    test_failed "Script exists and is executable" "File not found or not executable"
fi

# Test: Usage message when no arguments
if output=$("$UN_SH" 2>&1) && [ $? -eq 1 ]; then
    if echo "$output" | grep -q "Usage:"; then
        test_passed "Shows usage message with no arguments"
    else
        test_failed "Shows usage message with no arguments" "Expected usage message"
    fi
else
    # Script should exit with 1
    if echo "$output" | grep -q "Usage:"; then
        test_passed "Shows usage message with no arguments"
    else
        test_failed "Shows usage message with no arguments" "Expected usage message"
    fi
fi

# Test: Error on non-existent file
if output=$("$UN_SH" /tmp/nonexistent_file_12345.xyz 2>&1); then
    test_failed "Handles non-existent file" "Should exit with error"
else
    if echo "$output" | grep -qi "not found\|error"; then
        test_passed "Handles non-existent file"
    else
        test_failed "Handles non-existent file" "Expected error message, got: $output"
    fi
fi

# Test: Error on unknown extension
UNKNOWN_FILE="/tmp/test_unknown_ext_$$.unknownext"
echo "test" > "$UNKNOWN_FILE"
if output=$("$UN_SH" "$UNKNOWN_FILE" 2>&1); then
    test_failed "Handles unknown file extension" "Should exit with error"
    rm -f "$UNKNOWN_FILE"
else
    if echo "$output" | grep -qi "cannot detect\|unknown\|error"; then
        test_passed "Handles unknown file extension"
    else
        test_failed "Handles unknown file extension" "Expected error message, got: $output"
    fi
    rm -f "$UNKNOWN_FILE"
fi

# Test: Error when auth not set
has_hmac="${UNSANDBOX_PUBLIC_KEY:-}${UNSANDBOX_SECRET_KEY:-}"
has_legacy="${UNSANDBOX_API_KEY:-}"
if [ -n "$has_hmac" ] || [ -n "$has_legacy" ]; then
    TEST_FILE="$TEST_DIR/fib.py"
    if [ -f "$TEST_FILE" ]; then
        # Temporarily unset auth keys
        OLD_PUB="${UNSANDBOX_PUBLIC_KEY:-}"
        OLD_SEC="${UNSANDBOX_SECRET_KEY:-}"
        OLD_KEY="${UNSANDBOX_API_KEY:-}"
        unset UNSANDBOX_PUBLIC_KEY
        unset UNSANDBOX_SECRET_KEY
        unset UNSANDBOX_API_KEY
        if output=$("$UN_SH" "$TEST_FILE" 2>&1); then
            test_failed "Requires authentication" "Should exit with error when auth not set"
        else
            if echo "$output" | grep -qE "UNSANDBOX_(API_KEY|PUBLIC_KEY|SECRET_KEY)"; then
                test_passed "Requires authentication"
            else
                test_failed "Requires authentication" "Expected auth error message"
            fi
        fi
        [ -n "$OLD_PUB" ] && export UNSANDBOX_PUBLIC_KEY="$OLD_PUB"
        [ -n "$OLD_SEC" ] && export UNSANDBOX_SECRET_KEY="$OLD_SEC"
        [ -n "$OLD_KEY" ] && export UNSANDBOX_API_KEY="$OLD_KEY"
    else
        test_skipped "Requires authentication (test file not found)"
    fi
else
    test_skipped "Requires authentication (auth already not set)"
fi

# Integration Tests (require auth)
has_hmac="${UNSANDBOX_PUBLIC_KEY:-}${UNSANDBOX_SECRET_KEY:-}"
has_legacy="${UNSANDBOX_API_KEY:-}"
if [ -n "$has_hmac" ] || [ -n "$has_legacy" ]; then
    echo -e "\n${BLUE}=== Integration Tests for un.sh ===${NC}"

    # Test: Can execute Python file
    if [ -f "$TEST_DIR/fib.py" ]; then
        if output=$("$UN_SH" "$TEST_DIR/fib.py" 2>&1); then
            if echo "$output" | grep -q "fib(10)"; then
                test_passed "Executes Python file successfully"
            else
                test_failed "Executes Python file successfully" "Expected fibonacci output"
            fi
        else
            test_failed "Executes Python file successfully" "Script failed: $output"
        fi
    else
        test_skipped "Executes Python file successfully (fib.py not found)"
    fi

    # Test: Can execute Bash file
    if [ -f "$TEST_DIR/fib.sh" ]; then
        if output=$("$UN_SH" "$TEST_DIR/fib.sh" 2>&1); then
            if echo "$output" | grep -q "fib(10)"; then
                test_passed "Executes Bash file successfully"
            else
                test_failed "Executes Bash file successfully" "Expected fibonacci output"
            fi
        else
            test_failed "Executes Bash file successfully" "Script failed: $output"
        fi
    else
        test_skipped "Executes Bash file successfully (fib.sh not found)"
    fi
else
    echo -e "\n${YELLOW}Skipping integration tests (UNSANDBOX authentication not configured)${NC}"
fi

# Summary
echo -e "\n${BLUE}=== Test Summary ===${NC}"
echo "Total: $TESTS_RUN | Passed: $TESTS_PASSED | Failed: $TESTS_FAILED"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
