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

# Generate dynamic test matrix based on detected changes
# Reads changes.json, outputs test-matrix.yml as a child pipeline config

set -e

# CRITICAL: Tags ALWAYS run full test matrix, no matter what
# Also support --force-all flag or FORCE_FULL_MATRIX env var for manual triggers
if [ -n "$CI_COMMIT_TAG" ]; then
    echo "Tag detected ($CI_COMMIT_TAG) - forcing full test matrix"
    TEST_ALL="true"
    CHANGED_LANGS=""
elif [ "$1" = "--force-all" ] || [ "$FORCE_FULL_MATRIX" = "true" ]; then
    echo "Manual trigger - forcing full test matrix"
    TEST_ALL="true"
    CHANGED_LANGS=""
else
    # Read changes from detect-changes output
    CHANGES=$(cat changes.json)
    CHANGED_LANGS=$(echo "$CHANGES" | jq -r '.changed_langs[]' 2>/dev/null || echo "")
    TEST_ALL=$(echo "$CHANGES" | jq -r '.test_all' 2>/dev/null || echo "false")
fi

# Fetch ALL supported languages from the unsandbox API (the source of truth)
ALL_SDKS=$(curl -s "https://api.unsandbox.com/languages" | jq -r '.languages[]' | tr '\n' ' ')
if [ -z "$ALL_SDKS" ]; then
    # Fallback if API is unavailable
    ALL_SDKS="python javascript typescript ruby perl php lua bash go rust java kotlin c cpp"
    echo "Warning: Could not fetch languages from API, using fallback list"
fi
echo "Available SDKs from API: $ALL_SDKS"

# Test ALL languages - the sandbox has compilers for compiled languages too!

if [ "$TEST_ALL" = "true" ]; then
    LANGS="$ALL_SDKS"
elif [ -z "$CHANGED_LANGS" ]; then
    # No changes - create minimal valid child pipeline that does nothing
    cat > test-matrix.yml << 'EOF'
# No SDK changes detected - skip tests
stages:
  - skip

skip-tests:
  stage: skip
  tags:
    - build
  script:
    - echo "No SDK changes detected - skipping inception tests"
EOF
    echo "No SDK changes - created skip pipeline"
    cat test-matrix.yml
    exit 0
else
    LANGS="$CHANGED_LANGS"
fi

# Count languages
LANG_COUNT=$(echo $LANGS | wc -w)
echo "Testing $LANG_COUNT languages: $LANGS"

# Generate child pipeline config
cat > test-matrix.yml << 'EOF'
# Dynamically generated child pipeline - inception tests
# Each test runs: build/un → unsandbox → SDK → unsandbox → test code

stages:
  - build
  - cleanup
  - test

default:
  tags:
    - build

# CI variables inherited from parent pipeline

build-cli:
  stage: build
  script:
    - bash scripts/build-clients.sh
  artifacts:
    paths:
      - build/
    expire_in: 1 hour

# Cleanup orphaned test resources before fan-out to avoid concurrency limits
# ONLY targets resources with "test-" in the name to avoid destroying real services
# Uses parallel cleanup script for speed
cleanup-services:
  stage: cleanup
  needs:
    - build-cli
  script:
    - bash scripts/cleanup-test-resources.sh
  allow_failure: true

test:
  stage: test
  needs:
    - build-cli
    - cleanup-services
  parallel:
    matrix:
EOF

# Add each language as a parallel job
for LANG in $LANGS; do
    echo "      - SDK_LANG: $LANG" >> test-matrix.yml
done

# Complete the test job template
cat >> test-matrix.yml << 'EOF'
  script:
    - bash scripts/test-sdk.sh "$SDK_LANG"
  artifacts:
    reports:
      junit: "test-results-$SDK_LANG/test-results.xml"
    paths:
      - "test-results-$SDK_LANG/"
    expire_in: 30 days
    when: always
  allow_failure: true
  retry: 1
EOF

echo ""
echo "Generated test-matrix.yml with $LANG_COUNT parallel jobs:"
cat test-matrix.yml
