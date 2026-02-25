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

# Basic pipeline validation
# Just verify the core files exist and have correct structure

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

PASSED=0
FAILED=0

echo "Pipeline Validation"
echo "=================="
echo ""

# Test 1: .gitlab-ci.yml exists
if [ -f .gitlab-ci.yml ]; then
    echo "✓ .gitlab-ci.yml exists"
    PASSED=$((PASSED + 1))
else
    echo "✗ .gitlab-ci.yml missing"
    FAILED=$((FAILED + 1))
fi

# Test 2: All scripts exist
for SCRIPT in scripts/detect-changes.sh scripts/generate-matrix.sh scripts/build-clients.sh scripts/test-sdk.sh scripts/filter-results.sh scripts/science/{validate-examples,lint-all-sdks,benchmark-clients}.sh; do
    if [ -f "$SCRIPT" ] && [ -x "$SCRIPT" ]; then
        echo "✓ $(basename "$SCRIPT") exists and is executable"
        PASSED=$((PASSED + 1))
    else
        echo "✗ $(basename "$SCRIPT") missing or not executable"
        FAILED=$((FAILED + 1))
    fi
done

# Test 3: detect-changes produces JSON
echo ""
if bash scripts/detect-changes.sh 2>/dev/null | jq . > /dev/null 2>&1; then
    echo "✓ detect-changes.sh produces valid JSON"
    PASSED=$((PASSED + 1))
else
    echo "✗ detect-changes.sh output is not valid JSON"
    FAILED=$((FAILED + 1))
fi

# Test 4: generate-matrix handles empty changes
if echo '{"changed_langs": [], "test_all": false}' > changes.json && \
   bash scripts/generate-matrix.sh 2>/dev/null | head -1 | grep -q "#"; then
    echo "✓ generate-matrix.sh handles empty changes"
    PASSED=$((PASSED + 1))
else
    echo "✗ generate-matrix.sh failed on empty changes"
    FAILED=$((FAILED + 1))
fi

# Test 5: generate-matrix generates matrix for changes
if echo '{"changed_langs": ["python"], "test_all": false}' > changes.json && \
   bash scripts/generate-matrix.sh 2>/dev/null | grep -q "SDK_LANG:"; then
    echo "✓ generate-matrix.sh generates test matrix"
    PASSED=$((PASSED + 1))
else
    echo "✗ generate-matrix.sh doesn't generate matrix"
    FAILED=$((FAILED + 1))
fi

# Cleanup
rm -f changes.json test-matrix.yml

# Test 6: No hardcoded credentials
if ! grep -r "unsb-sk-\|unsb-pk-" scripts/ 2>/dev/null; then
    echo "✓ No hardcoded credentials in scripts"
    PASSED=$((PASSED + 1))
else
    echo "✗ Found hardcoded credentials"
    FAILED=$((FAILED + 1))
fi

# Test 7: Scripts reference env vars
if grep -q "UNSANDBOX_API_KEY\|UNSANDBOX_PUBLIC_KEY" scripts/*.sh scripts/science/*.sh 2>/dev/null; then
    echo "✓ Scripts use environment variables for auth"
    PASSED=$((PASSED + 1))
else
    echo "✗ Scripts don't reference auth env vars"
    FAILED=$((FAILED + 1))
fi

# Test 8: PIPELINE.md documentation exists
if [ -f PIPELINE.md ]; then
    echo "✓ PIPELINE.md documentation exists"
    PASSED=$((PASSED + 1))
else
    echo "✗ PIPELINE.md documentation missing"
    FAILED=$((FAILED + 1))
fi

echo ""
echo "=================="
echo "Total: $PASSED passed, $FAILED failed"
if [ $FAILED -eq 0 ]; then
    echo "✓ All checks passed"
    exit 0
else
    echo "✗ Some checks failed"
    exit 1
fi
