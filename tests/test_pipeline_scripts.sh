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

# Test pipeline scripts in isolation
# Validates: syntax, basic functionality, error handling

set -e

TESTS_PASSED=0
TESTS_FAILED=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

test_case() {
    local NAME="$1"
    echo -n "Testing: $NAME... "
}

test_pass() {
    echo -e "${GREEN}✓${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
}

test_fail() {
    local REASON="$1"
    echo -e "${RED}✗${NC} ($REASON)"
    TESTS_FAILED=$((TESTS_FAILED + 1))
}

test_skip() {
    echo -e "${YELLOW}⊘${NC} (skipped)"
}

cd /home/fox/git/un-inception

# ============================================================================
# Test 1: detect-changes.sh syntax
# ============================================================================
test_case "detect-changes.sh has valid bash syntax"
if bash -n scripts/detect-changes.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in detect-changes.sh"
fi

# ============================================================================
# Test 2: generate-matrix.sh syntax
# ============================================================================
test_case "generate-matrix.sh has valid bash syntax"
if bash -n scripts/generate-matrix.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in generate-matrix.sh"
fi

# ============================================================================
# Test 3: build-clients.sh syntax
# ============================================================================
test_case "build-clients.sh has valid bash syntax"
if bash -n scripts/build-clients.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in build-clients.sh"
fi

# ============================================================================
# Test 4: test-sdk.sh syntax
# ============================================================================
test_case "test-sdk.sh has valid bash syntax"
if bash -n scripts/test-sdk.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in test-sdk.sh"
fi

# ============================================================================
# Test 5: filter-results.sh syntax
# ============================================================================
test_case "filter-results.sh has valid bash syntax"
if bash -n scripts/filter-results.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in filter-results.sh"
fi

# ============================================================================
# Test 6: Science job scripts syntax
# ============================================================================
test_case "validate-examples.sh has valid bash syntax"
if bash -n scripts/science/validate-examples.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in validate-examples.sh"
fi

test_case "lint-all-sdks.sh has valid bash syntax"
if bash -n scripts/science/lint-all-sdks.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in lint-all-sdks.sh"
fi

test_case "benchmark-clients.sh has valid bash syntax"
if bash -n scripts/science/benchmark-clients.sh 2>/dev/null; then
    test_pass
else
    test_fail "Bash syntax error in benchmark-clients.sh"
fi

# ============================================================================
# Test 7: detect-changes with no changes
# ============================================================================
test_case "detect-changes handles no SDK changes"
TMPDIR=$(mktemp -d)
cd "$TMPDIR"
git init > /dev/null 2>&1 || true
if bash /home/fox/git/un-inception/scripts/detect-changes.sh 2>/dev/null | \
   jq -r '.test_all' | grep -q "false"; then
    test_pass
    rm -rf "$TMPDIR"
else
    test_fail "detect-changes failed to handle no changes"
fi

cd /home/fox/git/un-inception

# ============================================================================
# Test 8: generate-matrix with empty changes
# ============================================================================
test_case "generate-matrix handles empty changes"
echo '{"changed_langs": [], "test_all": false}' > /tmp/changes.json
if bash scripts/generate-matrix.sh 2>/dev/null | grep -q "No SDK changes"; then
    test_pass
else
    test_fail "generate-matrix didn't handle empty changes correctly"
fi

# ============================================================================
# Test 9: generate-matrix with single language
# ============================================================================
test_case "generate-matrix generates jobs for single language"
echo '{"changed_langs": ["python"], "test_all": false}' > /tmp/changes.json
if bash scripts/generate-matrix.sh 2>/dev/null | grep -q "SDK_LANG: python"; then
    test_pass
else
    test_fail "generate-matrix didn't generate python job"
fi

# ============================================================================
# Test 10: generate-matrix produces valid YAML
# ============================================================================
test_case "generate-matrix output is valid YAML"
echo '{"changed_langs": ["python", "javascript"], "test_all": false}' > /tmp/changes.json
if bash scripts/generate-matrix.sh 2>/dev/null | head -20 | grep -q "matrix:"; then
    test_pass
else
    test_fail "generate-matrix didn't produce valid matrix YAML"
fi

# ============================================================================
# Test 11: build-clients.sh creates build directory
# ============================================================================
test_case "build-clients.sh creates output directory"
rm -rf build/
if bash scripts/build-clients.sh > /dev/null 2>&1 && [ -d build ]; then
    test_pass
    rm -rf build/
else
    test_fail "build-clients.sh didn't create build directory"
fi

# ============================================================================
# Test 12: filter-results.sh creates reports directory
# ============================================================================
test_case "filter-results.sh creates report directory"
rm -rf reports/ final-report.xml
mkdir -p test-results-python/
echo '<?xml version="1.0"?><testsuites tests="1" failures="0"><testsuite name="test"/></testsuites>' > test-results-python/test-results.xml
if bash scripts/filter-results.sh > /dev/null 2>&1 && [ -f final-report.xml ]; then
    test_pass
    rm -rf reports/ final-report.xml test-results-python/
else
    test_fail "filter-results.sh didn't create reports"
fi

# ============================================================================
# Test 13: Scripts don't have hardcoded credentials
# ============================================================================
test_case "Scripts have no hardcoded credentials"
if grep -r "unsb-sk-" scripts/ 2>/dev/null || \
   grep -r "unsb-pk-" scripts/ 2>/dev/null || \
   grep -r "UNSANDBOX_API_KEY=" scripts/ 2>/dev/null; then
    test_fail "Found hardcoded credentials in scripts"
else
    test_pass
fi

# ============================================================================
# Test 14: Scripts properly use environment variables
# ============================================================================
test_case "Scripts reference env variables"
if grep -q "UNSANDBOX_API_KEY" scripts/*.sh scripts/science/*.sh 2>/dev/null; then
    test_pass
else
    test_fail "Scripts don't reference auth environment variables"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "========================================"
echo "Pipeline Script Test Results"
echo "========================================"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo "Total:  $((TESTS_PASSED + TESTS_FAILED))"
echo "========================================"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All script tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi
