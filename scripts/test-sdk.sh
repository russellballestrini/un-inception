#!/bin/bash
# Inception test: Use un (C CLI) to test other SDKs through unsandbox
#
# Pattern: build/un → unsandbox API → un.py/un.js/etc → unsandbox API → test code
#
# Usage: test-sdk.sh LANGUAGE

set -e

LANG=${1:-python}
RESULTS_DIR="test-results-$LANG"
mkdir -p "$RESULTS_DIR"

# Map language to SDK file
get_sdk_file() {
    case "$1" in
        python)     echo "clients/python/sync/src/un.py" ;;
        javascript) echo "clients/javascript/sync/src/un.js" ;;
        typescript) echo "clients/typescript/sync/src/un.ts" ;;
        go)         echo "clients/go/sync/src/un.go" ;;
        ruby)       echo "clients/ruby/sync/src/un.rb" ;;
        php)        echo "clients/php/sync/src/un.php" ;;
        java)       echo "clients/java/sync/src/Un.java" ;;
        rust)       echo "clients/rust/sync/src/lib.rs" ;;
        perl)       echo "clients/perl/sync/src/un.pl" ;;
        lua)        echo "clients/lua/sync/src/un.lua" ;;
        bash)       echo "clients/bash/sync/src/un.sh" ;;
        *)          echo "clients/$1/sync/src/un.*" ;;
    esac
}

SDK_FILE=$(get_sdk_file "$LANG")

echo "=== Inception Test: $LANG ==="
echo "SDK: $SDK_FILE"
echo ""

# Check if un CLI exists
if [ ! -x "build/un" ]; then
    echo "ERROR: build/un not found. Run build-clients.sh first."
    exit 1
fi

# Check if SDK file exists
if [ ! -f "$SDK_FILE" ]; then
    echo "SKIP: SDK file not found: $SDK_FILE"
    # Generate skip result
    cat > "$RESULTS_DIR/test-results.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="Inception Test" tests="1" skipped="1">
    <testcase name="$LANG SDK inception" classname="un.$LANG">
      <skipped message="SDK file not found: $SDK_FILE"/>
    </testcase>
  </testsuite>
</testsuites>
EOF
    exit 0
fi

# Run inception test:
# 1. Use build/un to execute the SDK file in unsandbox with network access
# 2. The SDK (un.py, etc.) will then call the API to run a simple test
echo "Running inception test..."
echo "Command: build/un -n semitrusted $SDK_FILE --help"

START_TIME=$(date +%s.%N)

# First test: Can the SDK show help?
if build/un -n semitrusted \
    -e "UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY" \
    -e "UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY" \
    "$SDK_FILE" --help > "$RESULTS_DIR/help_output.txt" 2>&1; then
    HELP_OK=true
    echo "✓ Help command succeeded"
else
    HELP_OK=false
    echo "✗ Help command failed"
fi

# Second test: Can the SDK execute code? (inception within inception)
# The SDK runs inside unsandbox, then calls the API to run Python
echo ""
echo "Testing code execution through $LANG SDK..."

TEST_CODE='print("inception-test-ok")'
if build/un -n semitrusted \
    -e "UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY" \
    -e "UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY" \
    "$SDK_FILE" -s python "$TEST_CODE" > "$RESULTS_DIR/exec_output.txt" 2>&1; then

    if grep -q "inception-test-ok" "$RESULTS_DIR/exec_output.txt"; then
        EXEC_OK=true
        echo "✓ Code execution succeeded"
    else
        EXEC_OK=false
        echo "✗ Code execution returned unexpected output"
        cat "$RESULTS_DIR/exec_output.txt"
    fi
else
    EXEC_OK=false
    echo "✗ Code execution failed"
    cat "$RESULTS_DIR/exec_output.txt"
fi

END_TIME=$(date +%s.%N)
DURATION=$(echo "$END_TIME - $START_TIME" | bc)

# Determine overall result
if [ "$HELP_OK" = true ] && [ "$EXEC_OK" = true ]; then
    FAILURES=0
    STATUS="PASS"
else
    FAILURES=1
    STATUS="FAIL"
fi

echo ""
echo "=== Result: $STATUS (${DURATION}s) ==="

# Generate JUnit XML
cat > "$RESULTS_DIR/test-results.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="Inception Test" tests="2" failures="$FAILURES" time="$DURATION">
    <testcase name="$LANG SDK help" classname="un.$LANG" time="0">
EOF

if [ "$HELP_OK" = true ]; then
    echo '      <system-out>Help command succeeded</system-out>' >> "$RESULTS_DIR/test-results.xml"
else
    echo '      <failure message="Help command failed">See help_output.txt</failure>' >> "$RESULTS_DIR/test-results.xml"
fi

cat >> "$RESULTS_DIR/test-results.xml" << EOF
    </testcase>
    <testcase name="$LANG SDK execution" classname="un.$LANG" time="$DURATION">
EOF

if [ "$EXEC_OK" = true ]; then
    echo '      <system-out>inception-test-ok</system-out>' >> "$RESULTS_DIR/test-results.xml"
else
    echo '      <failure message="Code execution failed">See exec_output.txt</failure>' >> "$RESULTS_DIR/test-results.xml"
fi

cat >> "$RESULTS_DIR/test-results.xml" << EOF
    </testcase>
  </testsuite>
</testsuites>
EOF

# Exit with appropriate code
[ "$STATUS" = "PASS" ] && exit 0 || exit 1
