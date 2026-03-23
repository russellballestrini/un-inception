#!/usr/bin/env bash
# Integration test: --account N credential selection in Java SDK CLI
#
# Tests that --account N loads row N from accounts.csv and takes priority
# over environment variables UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY.
#
# Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY set in the environment.
# SKIP if not set.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$(cd "$SCRIPT_DIR/../src" && pwd)"

PASS=0
FAIL=0
SKIP=0

pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; FAIL=$((FAIL + 1)); }
skip() { echo "SKIP: $1"; SKIP=$((SKIP + 1)); }

# ---- Prerequisites ----

if [ -z "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -z "${UNSANDBOX_SECRET_KEY:-}" ]; then
    skip "UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY not set"
    echo ""
    echo "Results: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"
    exit 0
fi

# Compile Un.java if needed
CLASS_FILE="$SRC_DIR/Un.class"
if [ ! -f "$CLASS_FILE" ] || [ "$SRC_DIR/Un.java" -nt "$CLASS_FILE" ]; then
    echo "Compiling Un.java..."
    if ! javac -cp "$SRC_DIR" "$SRC_DIR/Un.java" 2>&1; then
        fail "javac compilation failed"
        echo ""
        echo "Results: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"
        exit 1
    fi
fi

# ---- Temp home setup ----

TMPHOME="$(mktemp -d)"
trap 'rm -rf "$TMPHOME"' EXIT

mkdir -p "$TMPHOME/.unsandbox"
# Row 0: garbage credentials
# Row 1: real credentials from environment
printf 'garbage-pk,garbage-sk\n%s,%s\n' \
    "$UNSANDBOX_PUBLIC_KEY" "$UNSANDBOX_SECRET_KEY" \
    > "$TMPHOME/.unsandbox/accounts.csv"

# ---- Test 1: --account 1 loads real creds, env vars set to garbage ----
# With HOME=TMPHOME, env set to garbage, --account 1 should pick real creds
# and the 'key' subcommand should succeed (validateKeys returns 200).

RESULT=$(
    HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="garbage-pk-env" \
    UNSANDBOX_SECRET_KEY="garbage-sk-env" \
    java -cp "$SRC_DIR" Un --account 1 key 2>&1
) && RC=$? || RC=$?

if [ $RC -eq 0 ]; then
    pass "--account 1 loads row 1 from accounts.csv (real creds), ignores garbage env vars"
else
    fail "--account 1 should have succeeded but exited $RC: $RESULT"
fi

# ---- Test 2: --account 0 loads garbage creds, should get 401/error ----
# With env vars set to real creds, --account 0 should use garbage row 0
# and the API call should fail (unauthorized).

RESULT=$(
    HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="$UNSANDBOX_PUBLIC_KEY" \
    UNSANDBOX_SECRET_KEY="$UNSANDBOX_SECRET_KEY" \
    java -cp "$SRC_DIR" Un --account 0 key 2>&1
) && RC=$? || RC=$?

if [ $RC -ne 0 ]; then
    pass "--account 0 loads garbage creds from row 0, API rejects them (env vars ignored)"
else
    fail "--account 0 should have failed (garbage creds) but succeeded: $RESULT"
fi

# ---- Test 3: no --account flag, env vars set to real creds → success ----

RESULT=$(
    HOME="$TMPHOME" \
    UNSANDBOX_PUBLIC_KEY="$UNSANDBOX_PUBLIC_KEY" \
    UNSANDBOX_SECRET_KEY="$UNSANDBOX_SECRET_KEY" \
    java -cp "$SRC_DIR" Un key 2>&1
) && RC=$? || RC=$?

if [ $RC -eq 0 ]; then
    pass "no --account flag uses env vars (real creds), succeeds"
else
    fail "no --account flag should succeed with real env var creds but exited $RC: $RESULT"
fi

# ---- Summary ----

echo ""
echo "Results: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
exit 0
