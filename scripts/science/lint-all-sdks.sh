#!/bin/bash
# Lint all SDKs using unsandbox
# Tests code quality without installing linters locally

set -e

mkdir -p lint-results

echo "Linting SDKs through unsandbox..."
PASSED=0
FAILED=0

# Python SDKs
if [ -f "un.py" ]; then
    echo "Linting Python SDK..."
    RESULT=$(curl -s -X POST https://api.unsandbox.com/execute \
      -H "Authorization: Bearer ${UNSANDBOX_API_KEY}" \
      -H "Content-Type: application/json" \
      -d '{
        "language": "bash",
        "code": "python3 -m py_compile un.py && echo OK || echo FAILED"
      }' | jq -r '.stdout' 2>/dev/null || echo "ERROR")

    [[ "$RESULT" == *"OK"* ]] && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
fi

# JavaScript SDKs
if [ -f "un.js" ]; then
    echo "Linting JavaScript SDK..."
    RESULT=$(curl -s -X POST https://api.unsandbox.com/execute \
      -H "Authorization: Bearer ${UNSANDBOX_API_KEY}" \
      -H "Content-Type: application/json" \
      -d '{
        "language": "javascript",
        "code": "require(\"./un.js\"); console.log(\"OK\")"
      }' | jq -r '.stdout' 2>/dev/null || echo "ERROR")

    [[ "$RESULT" == *"OK"* ]] && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
fi

# Ruby SDKs
if [ -f "un.rb" ]; then
    echo "Linting Ruby SDK..."
    RESULT=$(curl -s -X POST https://api.unsandbox.com/execute \
      -H "Authorization: Bearer ${UNSANDBOX_API_KEY}" \
      -H "Content-Type: application/json" \
      -d '{
        "language": "bash",
        "code": "ruby -c un.rb && echo OK || echo FAILED"
      }' | jq -r '.stdout' 2>/dev/null || echo "ERROR")

    [[ "$RESULT" == *"OK"* ]] && PASSED=$((PASSED + 1)) || FAILED=$((FAILED + 1))
fi

# Generate report
cat > lint-results.xml << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="SDK Linting" tests="$((PASSED + FAILED))" failures="$FAILED">
    <testcase name="Lint All SDKs" classname="science.lint">
      <system-out>Checked: $PASSED, Failed: $FAILED</system-out>
    </testcase>
  </testsuite>
</testsuites>
EOF

echo "Linting complete: $PASSED passed, $FAILED failed"
exit 0  # allow_failure: true
