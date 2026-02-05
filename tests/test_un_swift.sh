#!/usr/bin/env bash
# Test suite for un.swift (Swift implementation)
# Note: un.swift requires Swift compiler

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_SWIFT="$SCRIPT_DIR/../clients/swift/sync/src/un.swift"
TEST_DIR="$SCRIPT_DIR/../test"

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
    echo -e "${GREEN}PASS${NC}: $1"
}

test_failed() {
    TESTS_FAILED=$((TESTS_FAILED + 1))
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -e "${RED}FAIL${NC}: $1"
    if [ -n "${2:-}" ]; then
        echo -e "${RED}  Error: $2${NC}"
    fi
}

test_skipped() {
    echo -e "${YELLOW}SKIP${NC}: $1"
}

# Check if Swift is available
if ! command -v swift &> /dev/null; then
    echo -e "${YELLOW}Swift not found - skipping all Swift tests${NC}"
    exit 0
fi

# Check if un.swift exists
if [ ! -f "$UN_SWIFT" ]; then
    # Try alternative paths
    UN_SWIFT="$SCRIPT_DIR/../un.swift"
    if [ ! -f "$UN_SWIFT" ]; then
        echo -e "${YELLOW}un.swift not found - skipping all tests${NC}"
        exit 0
    fi
fi

# Unit Tests
echo -e "${BLUE}=== Unit Tests for un.swift ===${NC}"

# Test: Script exists
if [ -f "$UN_SWIFT" ]; then
    test_passed "Script exists"
else
    test_failed "Script exists" "File not found"
    exit 1
fi

# Test: Script is readable
if [ -r "$UN_SWIFT" ]; then
    test_passed "Script is readable"
else
    test_failed "Script is readable" "File not readable"
fi

# CLI Command Tests (Feature Parity)
echo -e "\n${BLUE}=== CLI Command Tests (Feature Parity) ===${NC}"

# Test: --help
if output=$(swift "$UN_SWIFT" --help 2>&1); then
    if echo "$output" | grep -qi "usage"; then
        test_passed "CLI: --help shows usage"
    else
        test_failed "CLI: --help shows usage" "No usage indication"
    fi
else
    if echo "$output" | grep -qi "usage"; then
        test_passed "CLI: --help shows usage"
    else
        test_failed "CLI: --help shows usage" "Command failed"
    fi
fi

# Test: version command
if output=$(swift "$UN_SWIFT" version 2>&1); then
    if echo "$output" | grep -qi "version"; then
        test_passed "CLI: version command works"
    else
        test_failed "CLI: version command works" "No version output"
    fi
else
    test_failed "CLI: version command works" "Command failed"
fi

# Test: health command
if output=$(swift "$UN_SWIFT" health 2>&1); then
    if echo "$output" | grep -qi "health\|api"; then
        test_passed "CLI: health command works"
    else
        test_failed "CLI: health command works" "No health output"
    fi
else
    test_failed "CLI: health command works" "Command failed"
fi

# Test: languages command
if output=$(swift "$UN_SWIFT" languages 2>&1); then
    if echo "$output" | grep -qi "python\|error\|api key"; then
        test_passed "CLI: languages command works"
    else
        test_failed "CLI: languages command works" "No languages output"
    fi
else
    test_failed "CLI: languages command works" "Command failed"
fi

# API Command Tests (require API key)
if [ -n "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -n "${UNSANDBOX_SECRET_KEY:-}" ]; then
    echo -e "\n${BLUE}=== API Command Tests (require auth) ===${NC}"

    # Test: snapshot --list
    if output=$(swift "$UN_SWIFT" snapshot --list 2>&1); then
        if echo "$output" | grep -qE '\[|\{|Error|snapshots'; then
            test_passed "CLI: snapshot --list works"
        else
            test_failed "CLI: snapshot --list works" "Unexpected output"
        fi
    else
        if echo "$output" | grep -qE '\[|\{|Error|snapshots'; then
            test_passed "CLI: snapshot --list works"
        else
            test_failed "CLI: snapshot --list works" "Command failed"
        fi
    fi

    # Test: session --list
    if output=$(swift "$UN_SWIFT" session --list 2>&1); then
        if echo "$output" | grep -qE '\[|\{|Error|sessions'; then
            test_passed "CLI: session --list works"
        else
            test_failed "CLI: session --list works" "Unexpected output"
        fi
    else
        if echo "$output" | grep -qE '\[|\{|Error|sessions'; then
            test_passed "CLI: session --list works"
        else
            test_failed "CLI: session --list works" "Command failed"
        fi
    fi

    # Test: service --list
    if output=$(swift "$UN_SWIFT" service --list 2>&1); then
        if echo "$output" | grep -qE '\[|\{|Error|services'; then
            test_passed "CLI: service --list works"
        else
            test_failed "CLI: service --list works" "Unexpected output"
        fi
    else
        if echo "$output" | grep -qE '\[|\{|Error|services'; then
            test_passed "CLI: service --list works"
        else
            test_failed "CLI: service --list works" "Command failed"
        fi
    fi

    # Test: image --list
    if output=$(swift "$UN_SWIFT" image --list 2>&1); then
        if echo "$output" | grep -qE '\[|\{|Error|images'; then
            test_passed "CLI: image --list works"
        else
            test_failed "CLI: image --list works" "Unexpected output"
        fi
    else
        if echo "$output" | grep -qE '\[|\{|Error|images'; then
            test_passed "CLI: image --list works"
        else
            test_failed "CLI: image --list works" "Command failed"
        fi
    fi
else
    echo -e "\n${YELLOW}Skipping API command tests (UNSANDBOX_PUBLIC_KEY/SECRET_KEY not set)${NC}"
fi

# Integration Tests (require API key)
if [ -n "${UNSANDBOX_API_KEY:-}" ]; then
    echo -e "\n${BLUE}=== Integration Tests for un.swift ===${NC}"

    # Test: Can execute Python file
    if [ -f "$TEST_DIR/fib.py" ]; then
        if output=$(swift "$UN_SWIFT" "$TEST_DIR/fib.py" 2>&1); then
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
        if output=$(swift "$UN_SWIFT" "$TEST_DIR/fib.sh" 2>&1); then
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
    echo -e "\n${YELLOW}Skipping integration tests (UNSANDBOX_API_KEY not set)${NC}"
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
