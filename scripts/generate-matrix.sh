#!/bin/bash
# Generate dynamic test matrix based on detected changes
# Reads changes.json, outputs test-matrix.yml with parallel jobs

set -e

# Read changes from detect-changes output
CHANGES=$(cat changes.json)
CHANGED_LANGS=$(echo "$CHANGES" | jq -r '.changed_langs[]' 2>/dev/null || echo "")
TEST_ALL=$(echo "$CHANGES" | jq -r '.test_all' 2>/dev/null || echo "false")

# Fetch ALL supported languages from the unsandbox API (the source of truth)
ALL_SDKS=$(curl -s "https://api.unsandbox.com/languages" | jq -r '.languages[]' | tr '\n' ' ')
if [ -z "$ALL_SDKS" ]; then
    # Fallback if API is unavailable
    ALL_SDKS="python javascript typescript ruby perl php lua bash go rust java kotlin c cpp"
    echo "Warning: Could not fetch languages from API, using fallback list"
fi
echo "Available SDKs from API: $ALL_SDKS"

if [ "$TEST_ALL" = "true" ]; then
    LANGS="$ALL_SDKS"
elif [ -z "$CHANGED_LANGS" ]; then
    # No changes - don't generate any test jobs
    echo "# No SDK changes detected - no tests to run" > test-matrix.yml
    cat test-matrix.yml
    exit 0
else
    LANGS="$CHANGED_LANGS"
fi

# Start generating test-matrix.yml
cat > test-matrix.yml << 'EOF'
# Dynamically generated test matrix - inception tests for changed SDKs
# Each test runs: build/un → unsandbox → SDK → unsandbox → test code

test:
  stage: test
  tags:
    - build
  needs:
    - build
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
    - echo "=== Inception Test Matrix ==="
    - echo "Testing SDK: $SDK_LANG"
    - bash scripts/test-sdk.sh "$SDK_LANG"
  artifacts:
    reports:
      junit: "test-results-$SDK_LANG/test-results.xml"
    paths:
      - "test-results-$SDK_LANG/"
    expire_in: 30 days
  allow_failure: true
  retry: 1
EOF

echo ""
echo "Generated test-matrix.yml:"
cat test-matrix.yml
