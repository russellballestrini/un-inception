#!/bin/bash
# Simple test runner for the core UN CLI implementations
# Tests: Python, JavaScript, TypeScript, Ruby, PHP, Perl, Lua

set -e

cd "$(dirname "$0")"

echo "=========================================="
echo "UN CLI Inception - Basic Test Suite"
echo "=========================================="
echo ""

TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

run_test() {
    local test_file="$1"
    local test_name="$2"

    if [ ! -f "$test_file" ]; then
        echo "⚠  SKIP: $test_name - test file not found"
        return
    fi

    echo "Running: $test_name"
    echo "----------------------------------------"

    TESTS_RUN=$((TESTS_RUN + 1))

    if ./"$test_file"; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        echo "✓ $test_name PASSED"
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        echo "✗ $test_name FAILED"
    fi

    echo ""
}

# Check for API key
if [ -z "$UNSANDBOX_API_KEY" ]; then
    echo "⚠  WARNING: UNSANDBOX_API_KEY not set"
    echo "   Integration and functional tests will be skipped"
    echo ""
fi

# Run tests for each language
run_test "test_un_py.py" "Python"
run_test "test_un_js.js" "JavaScript"
run_test "test_un_rb.rb" "Ruby"
run_test "test_un_pl.pl" "Perl"
run_test "test_un_lua.lua" "Lua"

# TypeScript needs special handling
if command -v ts-node &> /dev/null; then
    run_test "test_un_ts.ts" "TypeScript"
else
    echo "⚠  SKIP: TypeScript - ts-node not installed"
    echo ""
fi

# PHP needs special handling
if command -v php &> /dev/null; then
    run_test "test_un_php.php" "PHP"
else
    echo "⚠  SKIP: PHP - php not installed"
    echo ""
fi

# Summary
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Total:  $TESTS_RUN"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo "=========================================="

if [ $TESTS_FAILED -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ Some tests failed"
    exit 1
fi
