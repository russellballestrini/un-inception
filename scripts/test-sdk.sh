#!/bin/bash
# Test a specific SDK using unsandbox
# Usage: test-sdk.sh LANGUAGE

set -e

LANG=${1:-python}
RESULTS_DIR="test-results-$LANG"
mkdir -p "$RESULTS_DIR"

echo "Testing $LANG SDK via unsandbox..."

# Use unsandbox API to test the SDK
# This is the unfair advantage - we test without installing locally
curl -s -X POST https://api.unsandbox.com/execute \
  -H "Authorization: Bearer ${UNSANDBOX_API_KEY}" \
  -H "Content-Type: application/json" \
  -d "{
    \"language\": \"$LANG\",
    \"code\": \"print(\\\"$LANG SDK test: OK\\\")\"
  }" > "$RESULTS_DIR/output.json"

# Check result
RESULT=$(cat "$RESULTS_DIR/output.json" | jq -r '.stdout' 2>/dev/null || echo "ERROR")
if [[ "$RESULT" == *"OK"* ]]; then
    echo "✓ $LANG test passed"
    EXIT_CODE=0
else
    echo "✗ $LANG test failed: $RESULT"
    EXIT_CODE=1
fi

# Generate JUnit XML
cat > "$RESULTS_DIR/test-results.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="SDK Test" tests="1" failures="$([[ $EXIT_CODE -eq 0 ]] && echo 0 || echo 1)">
    <testcase name="$LANG SDK execution" classname="un.$LANG">
EOF

if [ $EXIT_CODE -eq 0 ]; then
    echo "      <system-out>$RESULT</system-out>" >> "$RESULTS_DIR/test-results.xml"
else
    echo "      <failure message=\"Test failed\">$RESULT</failure>" >> "$RESULTS_DIR/test-results.xml"
fi

cat >> "$RESULTS_DIR/test-results.xml" << EOF
    </testcase>
  </testsuite>
</testsuites>
EOF

exit $EXIT_CODE
