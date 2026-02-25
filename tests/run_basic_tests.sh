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

# Check for API auth keys
if [ -z "$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$UNSANDBOX_SECRET_KEY" ]; then
    if [ -z "$UNSANDBOX_API_KEY" ]; then
        echo "⚠  WARNING: UNSANDBOX authentication not configured"
        echo "   Integration and functional tests will be skipped"
        echo "   Set HMAC keys: export UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=..."
        echo "   Or legacy key: export UNSANDBOX_API_KEY=..."
        echo ""
    fi
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
