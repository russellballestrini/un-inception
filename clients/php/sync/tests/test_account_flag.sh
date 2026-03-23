#!/usr/bin/env bash
# Integration test for --account N credential selection in the PHP SDK CLI.
#
# Tests that --account N selects the correct row from accounts.csv,
# taking priority over UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY env vars.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_PHP="${SCRIPT_DIR}/../src/un.php"

PASS=0
FAIL=0
SKIP=0

pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; FAIL=$((FAIL + 1)); }
skip() { echo "SKIP: $1"; SKIP=$((SKIP + 1)); }

# Require real credentials to run meaningful tests
if [ -z "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -z "${UNSANDBOX_SECRET_KEY:-}" ]; then
    skip "UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY not set - cannot run account flag tests"
    echo ""
    echo "Results: ${PASS} passed, ${FAIL} failed, ${SKIP} skipped"
    exit 0
fi

REAL_PK="${UNSANDBOX_PUBLIC_KEY}"
REAL_SK="${UNSANDBOX_SECRET_KEY}"
GARBAGE_PK="unsb-pk-0000-0000-0000-garbage"
GARBAGE_SK="unsb-sk-00000-00000-00000-garbage"

# Create a temporary HOME with accounts.csv: row 0 = garbage, row 1 = real creds
TMPHOME="$(mktemp -d)"
trap 'rm -rf "${TMPHOME}"' EXIT

mkdir -p "${TMPHOME}/.unsandbox"
printf '%s,%s\n' "${GARBAGE_PK}" "${GARBAGE_SK}" > "${TMPHOME}/.unsandbox/accounts.csv"
printf '%s,%s\n' "${REAL_PK}" "${REAL_SK}" >> "${TMPHOME}/.unsandbox/accounts.csv"

# Test 1: --account 1 with garbage env vars should load row 1 (real creds) and succeed
echo "Test 1: --account 1 ignores garbage env vars and uses CSV row 1 (real creds)"
output=$(HOME="${TMPHOME}" \
    UNSANDBOX_PUBLIC_KEY="${GARBAGE_PK}" \
    UNSANDBOX_SECRET_KEY="${GARBAGE_SK}" \
    php "${UN_PHP}" --account 1 key 2>&1) && rc=0 || rc=$?

if [ $rc -eq 0 ]; then
    pass "Test 1: --account 1 succeeded with real creds from CSV row 1"
elif echo "${output}" | grep -qi "401\|unauthorized\|forbidden"; then
    fail "Test 1: got auth error despite real creds at row 1 (output: ${output})"
else
    fail "Test 1: unexpected failure (rc=${rc}, output: ${output})"
fi

# Test 2: --account 0 with real env vars should load row 0 (garbage creds) and get 401
echo "Test 2: --account 0 overrides real env vars and uses CSV row 0 (garbage creds)"
output=$(HOME="${TMPHOME}" \
    UNSANDBOX_PUBLIC_KEY="${REAL_PK}" \
    UNSANDBOX_SECRET_KEY="${REAL_SK}" \
    php "${UN_PHP}" --account 0 key 2>&1) && rc=0 || rc=$?

if echo "${output}" | grep -qi "401\|unauthorized\|forbidden\|authentication\|credentials"; then
    pass "Test 2: got expected auth rejection for garbage creds at row 0"
elif [ $rc -eq 3 ]; then
    # Exit code 3 = CredentialsException (e.g., empty key) - also acceptable
    pass "Test 2: got credentials exception for garbage creds at row 0 (rc=3)"
else
    fail "Test 2: expected 401/auth error but got rc=${rc}, output: ${output}"
fi

# Test 3: No --account flag with real env vars should succeed (env vars take priority over CSV row 0)
echo "Test 3: no --account flag with real env vars should succeed"
output=$(HOME="${TMPHOME}" \
    UNSANDBOX_PUBLIC_KEY="${REAL_PK}" \
    UNSANDBOX_SECRET_KEY="${REAL_SK}" \
    php "${UN_PHP}" key 2>&1) && rc=0 || rc=$?

if [ $rc -eq 0 ]; then
    pass "Test 3: succeeded using env vars when no --account flag set"
elif echo "${output}" | grep -qi "401\|unauthorized\|forbidden"; then
    fail "Test 3: got unexpected auth error with real env vars (output: ${output})"
else
    fail "Test 3: unexpected failure (rc=${rc}, output: ${output})"
fi

echo ""
echo "Results: ${PASS} passed, ${FAIL} failed, ${SKIP} skipped"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
