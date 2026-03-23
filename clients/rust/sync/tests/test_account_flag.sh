#!/usr/bin/env bash
# Test --account N credential selection in the Rust sync SDK CLI.
#
# Tests:
#   1. --account 1 selects row 1 from accounts.csv (garbage env vars present)
#   2. --account 0 selects row 0 (garbage creds), returns 401
#   3. No --account flag falls through to env vars (real creds), succeeds

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../../.." && pwd)"
SDK_DIR="$SCRIPT_DIR/.."
BINARY="$SDK_DIR/target/debug/un"

# ---------------------------------------------------------------------------
# Skip if real credentials are not available
# ---------------------------------------------------------------------------
if [ -z "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -z "${UNSANDBOX_SECRET_KEY:-}" ]; then
    echo "SKIP: UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY not set"
    exit 0
fi

REAL_PK="$UNSANDBOX_PUBLIC_KEY"
REAL_SK="$UNSANDBOX_SECRET_KEY"

# ---------------------------------------------------------------------------
# Build the binary if it doesn't already exist
# ---------------------------------------------------------------------------
if [ ! -f "$BINARY" ]; then
    echo "Building Rust SDK..."
    cd "$SDK_DIR"
    PATH="$HOME/.cargo/bin:$PATH" cargo build 2>&1
fi

if [ ! -f "$BINARY" ]; then
    echo "FAIL: binary not found at $BINARY after build"
    exit 1
fi

# ---------------------------------------------------------------------------
# Set up a temporary HOME with accounts.csv
#   row 0: garbage credentials
#   row 1: real credentials
# ---------------------------------------------------------------------------
TMPHOME="$(mktemp -d)"
trap 'rm -rf "$TMPHOME"' EXIT

mkdir -p "$TMPHOME/.unsandbox"
printf 'unsb-pk-garbage-0000,unsb-sk-garbage-0000000000000000\n%s,%s\n' \
    "$REAL_PK" "$REAL_SK" \
    > "$TMPHOME/.unsandbox/accounts.csv"

PASS=0
FAIL=0

run_key() {
    # Returns the exit code and stdout/stderr of `un key`
    HOME="$TMPHOME" "$BINARY" "$@" key 2>&1
}

# ---------------------------------------------------------------------------
# Test 1: --account 1 picks real creds from row 1, even when env has garbage
# ---------------------------------------------------------------------------
echo "Test 1: --account 1 selects row 1 (real creds) despite garbage env vars"
OUTPUT=$(HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="unsb-pk-garbage-env" \
    UNSANDBOX_SECRET_KEY="unsb-sk-garbage-env-0000000000000" \
    "$BINARY" --account 1 key 2>&1 || true)

if echo "$OUTPUT" | grep -qi "API keys valid"; then
    echo "  PASS"
    PASS=$((PASS + 1))
else
    echo "  FAIL: expected 'API keys valid', got:"
    echo "$OUTPUT" | sed 's/^/    /'
    FAIL=$((FAIL + 1))
fi

# ---------------------------------------------------------------------------
# Test 2: --account 0 picks garbage creds from row 0, should get 401
# ---------------------------------------------------------------------------
echo "Test 2: --account 0 selects row 0 (garbage creds), expect auth failure"
OUTPUT=$(HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
    UNSANDBOX_SECRET_KEY="$REAL_SK" \
    "$BINARY" --account 0 key 2>&1 || true)

if echo "$OUTPUT" | grep -qiE "401|unauthorized|invalid|invalid key|keys invalid"; then
    echo "  PASS"
    PASS=$((PASS + 1))
else
    echo "  FAIL: expected auth failure (401/unauthorized/invalid), got:"
    echo "$OUTPUT" | sed 's/^/    /'
    FAIL=$((FAIL + 1))
fi

# ---------------------------------------------------------------------------
# Test 3: No --account flag, env vars contain real creds → succeed
# ---------------------------------------------------------------------------
echo "Test 3: no --account flag, env vars hold real creds"
OUTPUT=$(HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
    UNSANDBOX_SECRET_KEY="$REAL_SK" \
    "$BINARY" key 2>&1 || true)

if echo "$OUTPUT" | grep -qi "API keys valid"; then
    echo "  PASS"
    PASS=$((PASS + 1))
else
    echo "  FAIL: expected 'API keys valid', got:"
    echo "$OUTPUT" | sed 's/^/    /'
    FAIL=$((FAIL + 1))
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
echo "Results: $PASS passed, $FAIL failed"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
exit 0
