#!/bin/bash
# Unit Tests for un.sh Library Functions
#
# Tests the ACTUAL exported functions from Un module.
# NO local re-implementations. NO mocking.
#
# Run: bash tests/test_library.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../sync/src/un.sh" 2>/dev/null || {
    echo "Error: Cannot source un.sh"
    exit 1
}

# Test counters
tests_passed=0
tests_failed=0

PASS() {
    echo -e "  \033[32m[PASS]\033[0m $1"
    tests_passed=$((tests_passed + 1))
}

FAIL() {
    echo -e "  \033[31m[FAIL]\033[0m $1"
    tests_failed=$((tests_failed + 1))
}

assert_equal() {
    local actual="$1"
    local expected="$2"
    local msg="$3"
    if [ "$actual" = "$expected" ]; then
        PASS "$msg"
    else
        FAIL "$msg (expected: $expected, got: $actual)"
    fi
}

assert_not_empty() {
    local value="$1"
    local msg="$2"
    if [ -n "$value" ]; then
        PASS "$msg"
    else
        FAIL "$msg (expected non-empty)"
    fi
}

assert_match() {
    local value="$1"
    local pattern="$2"
    local msg="$3"
    if [[ "$value" =~ $pattern ]]; then
        PASS "$msg"
    else
        FAIL "$msg (value: $value does not match pattern: $pattern)"
    fi
}

# ============================================================================
# Test: version()
# ============================================================================

echo ""
echo "Testing version()..."

ver=$(version)
assert_not_empty "$ver" "version() returns non-empty string"
assert_match "$ver" "^[0-9]+\.[0-9]+\.[0-9]+$" "version() matches X.Y.Z format"
echo "    Version: $ver"

# ============================================================================
# Test: detect_language()
# ============================================================================

echo ""
echo "Testing detect_language()..."

declare -A lang_tests=(
    ["test.py"]="python"
    ["app.js"]="javascript"
    ["main.go"]="go"
    ["script.rb"]="ruby"
    ["lib.rs"]="rust"
    ["main.c"]="c"
    ["app.cpp"]="cpp"
    ["Main.java"]="java"
    ["index.php"]="php"
    ["script.pl"]="perl"
    ["init.lua"]="lua"
    ["run.sh"]="bash"
    ["main.ts"]="typescript"
    ["app.kt"]="kotlin"
    ["lib.ex"]="elixir"
    ["main.hs"]="haskell"
)

for file in "${!lang_tests[@]}"; do
    expected="${lang_tests[$file]}"
    result=$(detect_language "$file" 2>/dev/null || echo "")
    assert_equal "$result" "$expected" "detect_language('$file') -> '$expected'"
done

# Test unknown extension
result=$(detect_language "file.xyz123" 2>/dev/null || echo "")
assert_equal "$result" "" "detect_language(unknown ext) returns empty"

# Test no extension
result=$(detect_language "Makefile" 2>/dev/null || echo "")
assert_equal "$result" "" "detect_language(no ext) returns empty"

# ============================================================================
# Test: hmac_sign()
# ============================================================================

echo ""
echo "Testing hmac_sign()..."

# Test basic signature generation
sig=$(hmac_sign "secret_key" "1234567890:POST:/execute:{}")
assert_not_empty "$sig" "hmac_sign() returns non-nil"
assert_equal "${#sig}" "64" "hmac_sign() returns 64-char hex string"

# Verify hex characters
if [[ "$sig" =~ ^[0-9a-fA-F]+$ ]]; then
    PASS "hmac_sign() returns valid hex"
else
    FAIL "hmac_sign() returns valid hex"
fi

# Test deterministic output
sig1=$(hmac_sign "key" "message")
sig2=$(hmac_sign "key" "message")
assert_equal "$sig1" "$sig2" "hmac_sign() is deterministic"

# Test different keys produce different signatures
sig_a=$(hmac_sign "key_a" "message")
sig_b=$(hmac_sign "key_b" "message")
if [ "$sig_a" != "$sig_b" ]; then
    PASS "Different keys produce different signatures"
else
    FAIL "Different keys produce different signatures"
fi

# Test different messages produce different signatures
sig_m1=$(hmac_sign "key" "message1")
sig_m2=$(hmac_sign "key" "message2")
if [ "$sig_m1" != "$sig_m2" ]; then
    PASS "Different messages produce different signatures"
else
    FAIL "Different messages produce different signatures"
fi

# Test known HMAC value
known_sig=$(hmac_sign "key" "message")
if [[ "$known_sig" == 6e9ef29b75fffc5b7abae527d58fdadb* ]]; then
    PASS "HMAC-SHA256('key', 'message') matches expected prefix"
else
    FAIL "HMAC-SHA256('key', 'message') matches expected prefix (got: $known_sig)"
fi

# ============================================================================
# Test: last_error() / set_error()
# ============================================================================

echo ""
echo "Testing last_error()..."

set_error "test error"
err=$(last_error)
assert_equal "$err" "test error" "last_error() returns set error"

# ============================================================================
# Test: Memory stress test
# ============================================================================

echo ""
echo "Testing Memory Management..."

# Stress test HMAC allocation
for i in $(seq 1 1000); do
    hmac_sign "key" "message" > /dev/null
done
PASS "1000 HMAC calls without crash"

# Stress test language detection
for i in $(seq 1 1000); do
    detect_language "test.py" > /dev/null 2>&1 || true
done
PASS "1000 detect_language calls without crash"

# Stress test version
for i in $(seq 1 1000); do
    version > /dev/null
done
PASS "1000 version calls without crash"

# ============================================================================
# Test: Function existence
# ============================================================================

echo ""
echo "Testing Library function existence..."

functions=(
    # Execution functions (8)
    "execute" "execute_async" "wait_job" "get_job"
    "cancel_job" "list_jobs" "get_languages" "detect_language"

    # Session functions (9)
    "session_list" "session_get" "session_create" "session_destroy"
    "session_freeze" "session_unfreeze" "session_boost" "session_unboost"
    "session_execute"

    # Service functions (17)
    "service_list" "service_get" "service_create" "service_destroy"
    "service_freeze" "service_unfreeze" "service_lock" "service_unlock"
    "service_set_unfreeze_on_demand" "service_redeploy" "service_logs"
    "service_execute" "service_env_get" "service_env_set"
    "service_env_delete" "service_env_export" "service_resize"

    # Snapshot functions (9)
    "snapshot_list" "snapshot_get" "snapshot_session" "snapshot_service"
    "snapshot_restore" "snapshot_delete" "snapshot_lock" "snapshot_unlock"
    "snapshot_clone"

    # Image functions (13)
    "image_list" "image_get" "image_publish" "image_delete"
    "image_lock" "image_unlock" "image_set_visibility"
    "image_grant_access" "image_revoke_access" "image_list_trusted"
    "image_transfer" "image_spawn" "image_clone"

    # PaaS Logs (2)
    "logs_fetch" "logs_stream"

    # Utilities
    "validate_keys" "hmac_sign" "health_check" "version" "last_error"
)

for func in "${functions[@]}"; do
    if declare -f "$func" > /dev/null 2>&1; then
        PASS "$func() exists"
    else
        FAIL "$func() exists"
    fi
done

# ============================================================================
# Summary
# ============================================================================

echo ""
echo "====================================="
echo "Test Summary"
echo "====================================="
echo -e "Passed: \033[32m$tests_passed\033[0m"
echo -e "Failed: \033[31m$tests_failed\033[0m"
echo "====================================="

exit $((tests_failed > 0 ? 1 : 0))
