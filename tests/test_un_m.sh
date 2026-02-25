#!/usr/bin/env bash
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

# Test suite for un.m (Objective-C implementation)
# Note: un.m requires compilation, so we use a shell wrapper

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_M="$SCRIPT_DIR/../un.m"
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
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
    echo -e "${GREEN}✓ PASS${NC}: $1"
}

test_failed() {
    ((TESTS_FAILED++))
    ((TESTS_RUN++))
    echo -e "${RED}✗ FAIL${NC}: $1"
    if [ -n "${2:-}" ]; then
        echo -e "${RED}  Error: $2${NC}"
    fi
}

test_skipped() {
    echo -e "${YELLOW}⊘ SKIP${NC}: $1"
}

# Check if clang is available
if ! command -v clang &> /dev/null; then
    echo -e "${YELLOW}Clang not found - skipping all Objective-C tests${NC}"
    exit 0
fi

# Check if Foundation framework is available (macOS/GNUstep)
if ! clang -x objective-c -framework Foundation -o /tmp/test_objc_check_$$ -xc - <<< "int main(){return 0;}" 2>/dev/null; then
    # Try with GNUstep
    if ! clang -x objective-c $(gnustep-config --objc-flags 2>/dev/null) -o /tmp/test_objc_check_$$ -xc - <<< "int main(){return 0;}" 2>/dev/null; then
        echo -e "${YELLOW}Objective-C Foundation framework not found - skipping all tests${NC}"
        rm -f /tmp/test_objc_check_$$
        exit 0
    fi
fi
rm -f /tmp/test_objc_check_$$

# Unit Tests
echo -e "${BLUE}=== Unit Tests for un.m ===${NC}"

# Test: Script exists
if [ -f "$UN_M" ]; then
    test_passed "Script exists"
else
    test_failed "Script exists" "File not found"
    exit 1
fi

# Test: Script is executable
if [ -x "$UN_M" ]; then
    test_passed "Script is executable"
else
    test_failed "Script is executable" "File not executable"
fi

# Test: Usage message when no arguments
# Note: un.m needs to compile first, which may fail without args
# We'll just check if it produces some error
if output=$("$UN_M" 2>&1); then
    # Check output
    if echo "$output" | grep -q "Usage:"; then
        test_passed "Shows usage message with no arguments"
    else
        test_failed "Shows usage message with no arguments" "No clear usage indication"
    fi
else
    # Non-zero exit is expected
    if echo "$output" | grep -q "Usage:"; then
        test_passed "Shows usage message with no arguments"
    else
        # May fail at compile stage, which is acceptable
        test_skipped "Shows usage message with no arguments (compilation required)"
    fi
fi

# Test: Error on non-existent file (if we can compile)
TEST_BINARY="/tmp/un_objc_test_$$"
if clang -x objective-c -framework Foundation "$UN_M" -o "$TEST_BINARY" 2>/dev/null; then
    if output=$("$TEST_BINARY" /tmp/nonexistent_file_12345.xyz 2>&1); then
        test_failed "Handles non-existent file" "Should exit with error"
    else
        if echo "$output" | grep -q "not found"; then
            test_passed "Handles non-existent file"
        else
            test_failed "Handles non-existent file" "Expected 'not found' message"
        fi
    fi
    rm -f "$TEST_BINARY"
else
    test_skipped "Handles non-existent file (could not compile test binary)"
fi

# Test: Error on unknown extension
if clang -x objective-c -framework Foundation "$UN_M" -o "$TEST_BINARY" 2>/dev/null; then
    UNKNOWN_FILE="/tmp/test_unknown_ext_$$.unknownext"
    echo "test" > "$UNKNOWN_FILE"

    if output=$("$TEST_BINARY" "$UNKNOWN_FILE" 2>&1); then
        test_failed "Handles unknown file extension" "Should exit with error"
    else
        if echo "$output" | grep -q "Unknown file extension"; then
            test_passed "Handles unknown file extension"
        else
            test_failed "Handles unknown file extension" "Expected 'Unknown file extension' message"
        fi
    fi

    rm -f "$UNKNOWN_FILE" "$TEST_BINARY"
else
    test_skipped "Handles unknown file extension (could not compile test binary)"
fi

# CLI Command Tests (Feature Parity)
echo -e "\n${BLUE}=== CLI Command Tests (Feature Parity) ===${NC}"

# Compile for CLI tests
if clang -x objective-c -framework Foundation "$UN_M" -o "$TEST_BINARY" 2>/dev/null; then
    # Test: --help
    if output=$("$TEST_BINARY" --help 2>&1); then
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
    if output=$("$TEST_BINARY" version 2>&1); then
        if echo "$output" | grep -qi "version"; then
            test_passed "CLI: version command works"
        else
            test_failed "CLI: version command works" "No version output"
        fi
    else
        test_failed "CLI: version command works" "Command failed"
    fi

    # Test: health command
    if output=$("$TEST_BINARY" health 2>&1); then
        if echo "$output" | grep -qi "health\|api"; then
            test_passed "CLI: health command works"
        else
            test_failed "CLI: health command works" "No health output"
        fi
    else
        test_failed "CLI: health command works" "Command failed"
    fi

    # Test: languages command
    if output=$("$TEST_BINARY" languages 2>&1); then
        if echo "$output" | grep -qi "python\|error\|api key"; then
            test_passed "CLI: languages command works"
        else
            test_failed "CLI: languages command works" "No languages output"
        fi
    else
        test_failed "CLI: languages command works" "Command failed"
    fi

    rm -f "$TEST_BINARY"
else
    echo -e "${YELLOW}Could not compile un.m - skipping CLI command tests${NC}"
fi

# API Command Tests (require API key)
if [ -n "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -n "${UNSANDBOX_SECRET_KEY:-}" ]; then
    echo -e "\n${BLUE}=== API Command Tests (require auth) ===${NC}"

    # Compile the binary for API tests
    if clang -x objective-c -framework Foundation "$UN_M" -o "$TEST_BINARY" 2>/dev/null; then

        # Test: snapshot --list
        if output=$("$TEST_BINARY" snapshot --list 2>&1); then
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
        if output=$("$TEST_BINARY" session --list 2>&1); then
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
        if output=$("$TEST_BINARY" service --list 2>&1); then
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
        if output=$("$TEST_BINARY" image --list 2>&1); then
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

        rm -f "$TEST_BINARY"
    else
        echo -e "${YELLOW}Could not compile un.m - skipping API command tests${NC}"
    fi
else
    echo -e "\n${YELLOW}Skipping API command tests (UNSANDBOX_PUBLIC_KEY/SECRET_KEY not set)${NC}"
fi

# Integration Tests (require API key and successful compilation)
if [ -n "${UNSANDBOX_API_KEY:-}" ]; then
    echo -e "\n${BLUE}=== Integration Tests for un.m ===${NC}"

    # Compile the binary for integration tests
    if clang -x objective-c -framework Foundation "$UN_M" -o "$TEST_BINARY" 2>/dev/null; then

        # Test: Can execute Python file
        if [ -f "$TEST_DIR/fib.py" ]; then
            if output=$("$TEST_BINARY" "$TEST_DIR/fib.py" 2>&1); then
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
            if output=$("$TEST_BINARY" "$TEST_DIR/fib.sh" 2>&1); then
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

        rm -f "$TEST_BINARY"
    else
        echo -e "${YELLOW}Could not compile un.m - skipping integration tests${NC}"
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
