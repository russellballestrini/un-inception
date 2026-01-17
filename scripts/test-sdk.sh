#!/bin/bash
# Inception test: Use un (C CLI) to test other SDKs through unsandbox
#
# Pattern: build/un → unsandbox API → un.py/un.js/etc → unsandbox API → test code
#
# Tests multiple endpoints:
#   - execute (code execution)
#   - session --list
#   - service --list
#   - snapshot --list
#   - image --list
#   - languages
#   - key
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

# Compiled languages can't be run directly - skip inception test for them
# The unsandbox API can execute scripts, not compiled binaries
is_compiled_language() {
    case "$1" in
        rust|go|c|cpp|java|kotlin|swift|csharp|fsharp|haskell|ocaml|d|nim|zig|crystal|fortran|cobol)
            return 0 ;;
        *)
            return 1 ;;
    esac
}

if is_compiled_language "$LANG"; then
    echo "SKIP: $LANG is a compiled language - inception test not applicable"
    echo "      (Compiled SDKs require separate build+test workflow)"
    cat > "$RESULTS_DIR/test-results.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="Inception Test" tests="1" skipped="1">
    <testcase name="$LANG SDK inception" classname="un.$LANG">
      <skipped message="$LANG is a compiled language - inception test not applicable"/>
    </testcase>
  </testsuite>
</testsuites>
EOF
    exit 0
fi

# Check if un CLI exists
if [ ! -x "build/un" ]; then
    echo "ERROR: build/un not found. Run build-clients.sh first."
    exit 1
fi

# Check if SDK file exists
if [ ! -f "$SDK_FILE" ]; then
    echo "SKIP: SDK file not found: $SDK_FILE"
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

# Helper: Run SDK command through inception and capture result
run_sdk_cmd() {
    local test_name="$1"
    local output_file="$2"
    shift 2
    local args=("$@")

    build/un -n semitrusted \
        -e "UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY" \
        -e "UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY" \
        "$SDK_FILE" "${args[@]}" > "$output_file" 2>&1
}

START_TIME=$(date +%s.%N)

# Track test results
declare -A TEST_RESULTS
TOTAL_TESTS=0
FAILURES=0

# Test function
run_test() {
    local name="$1"
    local output_file="$RESULTS_DIR/${name// /_}.txt"
    local validation="$2"
    shift 2
    local args=("$@")

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -n "Test: $name... "

    if run_sdk_cmd "$name" "$output_file" "${args[@]}"; then
        if [ -n "$validation" ]; then
            if grep -qE "$validation" "$output_file"; then
                TEST_RESULTS["$name"]="pass"
                echo "PASS"
                return 0
            else
                TEST_RESULTS["$name"]="fail"
                FAILURES=$((FAILURES + 1))
                echo "FAIL (validation failed)"
                return 1
            fi
        else
            TEST_RESULTS["$name"]="pass"
            echo "PASS"
            return 0
        fi
    else
        TEST_RESULTS["$name"]="fail"
        FAILURES=$((FAILURES + 1))
        echo "FAIL (command failed)"
        return 1
    fi
}

echo "Running inception tests across multiple endpoints..."
echo ""

# 1. Help command (basic sanity check)
run_test "help" "Usage:|usage:|USAGE" --help || true

# 2. Execute endpoint - inline code execution
echo -n "Test: execute... "
if run_sdk_cmd "execute" "$RESULTS_DIR/execute.txt" -s python 'print("inception-test-ok")'; then
    if grep -q "inception-test-ok" "$RESULTS_DIR/execute.txt"; then
        TEST_RESULTS["execute"]="pass"
        echo "PASS"
    else
        TEST_RESULTS["execute"]="fail"
        FAILURES=$((FAILURES + 1))
        echo "FAIL (output mismatch)"
    fi
else
    TEST_RESULTS["execute"]="fail"
    FAILURES=$((FAILURES + 1))
    echo "FAIL (command failed)"
fi
TOTAL_TESTS=$((TOTAL_TESTS + 1))

# 3. Languages endpoint
run_test "languages" "python|javascript|ruby" languages || true

# 4. Key validation endpoint
run_test "key" "valid|expires|API|key" key || true

# 5. Session list endpoint
run_test "session_list" "" session --list || true

# 6. Service list endpoint
run_test "service_list" "" service --list || true

# 7. Snapshot list endpoint
run_test "snapshot_list" "" snapshot --list || true

# 8. Image list endpoint
run_test "image_list" "" image --list || true

END_TIME=$(date +%s.%N)
DURATION=$(echo "$END_TIME - $START_TIME" | bc)

# Determine overall status
PASSED=$((TOTAL_TESTS - FAILURES))
if [ "$FAILURES" -eq 0 ]; then
    STATUS="PASS"
else
    STATUS="FAIL"
fi

echo ""
echo "=== Result: $STATUS ($PASSED/$TOTAL_TESTS passed, ${DURATION}s) ==="

# Generate JUnit XML
cat > "$RESULTS_DIR/test-results.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="Inception Test - $LANG" tests="$TOTAL_TESTS" failures="$FAILURES" time="$DURATION">
EOF

for test_name in "${!TEST_RESULTS[@]}"; do
    result="${TEST_RESULTS[$test_name]}"
    safe_name="${test_name// /_}"
    cat >> "$RESULTS_DIR/test-results.xml" << EOF
    <testcase name="$test_name" classname="un.$LANG.$safe_name">
EOF
    if [ "$result" = "pass" ]; then
        echo "      <system-out>Test passed</system-out>" >> "$RESULTS_DIR/test-results.xml"
    else
        echo "      <failure message=\"Test failed\">See ${safe_name}.txt for details</failure>" >> "$RESULTS_DIR/test-results.xml"
    fi
    echo "    </testcase>" >> "$RESULTS_DIR/test-results.xml"
done

cat >> "$RESULTS_DIR/test-results.xml" << EOF
  </testsuite>
</testsuites>
EOF

# Exit with appropriate code
[ "$STATUS" = "PASS" ] && exit 0 || exit 1
