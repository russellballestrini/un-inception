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

# Test the GitLab CI pipeline infrastructure
# Validates: detect-changes, generate-matrix, scripts, YAML syntax

set -e

# Change to repo root (tests are run from tests/ directory)
cd "$(dirname "$0")/.."

TESTS_PASSED=0
TESTS_FAILED=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
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

# ============================================================================
# Test 1: Scripts exist and are executable
# ============================================================================
test_case "Scripts exist and are executable"
if [ -f scripts/detect-changes.sh ] && \
   [ -f scripts/generate-matrix.sh ] && \
   [ -f scripts/build-clients.sh ] && \
   [ -f scripts/test-sdk.sh ] && \
   [ -f scripts/filter-results.sh ] && \
   [ -x scripts/detect-changes.sh ] && \
   [ -x scripts/generate-matrix.sh ]; then
    test_pass
else
    test_fail "Missing or non-executable scripts"
fi

# ============================================================================
# Test 2: Science job scripts exist
# ============================================================================
test_case "Science job scripts exist"
if [ -f scripts/science/validate-examples.sh ] && \
   [ -f scripts/science/lint-all-sdks.sh ] && \
   [ -f scripts/science/benchmark-clients.sh ] && \
   [ -x scripts/science/validate-examples.sh ]; then
    test_pass
else
    test_fail "Missing science job scripts"
fi

# ============================================================================
# Test 3: .gitlab-ci.yml exists and has required stages
# ============================================================================
test_case ".gitlab-ci.yml structure"
if [ -f .gitlab-ci.yml ] && \
   grep -q "stages:" .gitlab-ci.yml && \
   grep -q "- pre" .gitlab-ci.yml && \
   grep -q "- test" .gitlab-ci.yml && \
   grep -q "- science" .gitlab-ci.yml && \
   grep -q "- report" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Missing required GitLab CI stages"
fi

# ============================================================================
# Test 4: detect-changes produces valid JSON
# ============================================================================
test_case "detect-changes produces valid JSON"
if bash scripts/detect-changes.sh 2>/dev/null | jq . > /dev/null 2>&1; then
    test_pass
else
    test_fail "detect-changes output is not valid JSON"
fi

# ============================================================================
# Test 5: generate-matrix produces valid YAML
# ============================================================================
test_case "generate-matrix produces valid YAML"
if [ -f /tmp/changes.json ] || echo '{"changed_langs": [], "test_all": false}' > /tmp/changes.json && \
   cd /tmp && \
   bash /home/fox/git/un-inception/scripts/generate-matrix.sh 2>/dev/null | grep -q "test:" ; then
    test_pass
else
    test_fail "generate-matrix output is not valid YAML"
fi

# ============================================================================
# Test 6: Pipeline has detect-changes job
# ============================================================================
test_case "detect-changes job configured"
if grep -q "detect-changes:" .gitlab-ci.yml && \
   grep -q "stage: pre" .gitlab-ci.yml && \
   grep -q "detect-changes.sh" .gitlab-ci.yml; then
    test_pass
else
    test_fail "detect-changes job not properly configured"
fi

# ============================================================================
# Test 7: Pipeline has generate-matrix job
# ============================================================================
test_case "generate-matrix job configured"
if grep -q "generate-matrix:" .gitlab-ci.yml && \
   grep -q "needs:" .gitlab-ci.yml && \
   grep -q "generate-matrix.sh" .gitlab-ci.yml; then
    test_pass
else
    test_fail "generate-matrix job not properly configured"
fi

# ============================================================================
# Test 8: Dynamic matrix include configured
# ============================================================================
test_case "Dynamic matrix include configured"
if grep -q "include:" .gitlab-ci.yml && \
   grep -q "test-matrix.yml" .gitlab-ci.yml && \
   grep -q "optional: true" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Dynamic matrix include not configured"
fi

# ============================================================================
# Test 9: Science jobs configured
# ============================================================================
test_case "Science jobs configured"
if grep -q "science-validate-examples:" .gitlab-ci.yml && \
   grep -q "science-lint-sdks:" .gitlab-ci.yml && \
   grep -q "science-benchmark-clients:" .gitlab-ci.yml && \
   grep -q "allow_failure: true" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Science jobs not properly configured"
fi

# ============================================================================
# Test 10: Report job configured
# ============================================================================
test_case "Report job configured"
if grep -q "report:" .gitlab-ci.yml && \
   grep -q "stage: report" .gitlab-ci.yml && \
   grep -q "filter-results.sh" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Report job not properly configured"
fi

# ============================================================================
# Test 11: Variables configured
# ============================================================================
test_case "Environment variables configured"
if grep -q "UNSANDBOX_API_KEY:" .gitlab-ci.yml && \
   grep -q "UNSANDBOX_PUBLIC_KEY:" .gitlab-ci.yml && \
   grep -q "UNSANDBOX_SECRET_KEY:" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Required environment variables not configured"
fi

# ============================================================================
# Test 12: Artifacts configured for all jobs
# ============================================================================
test_case "Artifacts configured for test jobs"
if grep -A 10 "^detect-changes:" .gitlab-ci.yml | grep -q "artifacts:" && \
   grep -A 10 "^generate-matrix:" .gitlab-ci.yml | grep -q "artifacts:"; then
    test_pass
else
    test_fail "Artifacts not configured"
fi

# ============================================================================
# Test 13: Only/except rules configured
# ============================================================================
test_case "Pipeline triggers configured"
if grep -q "only:" .gitlab-ci.yml && \
   grep -q "- main" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Pipeline triggers not properly configured"
fi

# ============================================================================
# Test 14: Tag-triggered releases configured
# ============================================================================
test_case "Tag-triggered releases configured"
if grep -q "v.*\..*\..*" .gitlab-ci.yml; then
    test_pass
else
    test_fail "Tag-triggered releases not configured"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "========================================"
echo "Pipeline Test Results"
echo "========================================"
echo "Passed: $TESTS_PASSED"
echo "Failed: $TESTS_FAILED"
echo "Total:  $((TESTS_PASSED + TESTS_FAILED))"
echo "========================================"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All pipeline tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi
