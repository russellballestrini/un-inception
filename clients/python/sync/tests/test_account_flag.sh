#!/usr/bin/env bash
# Integration test: --account N flag must take priority over env vars
# Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_PY="$SCRIPT_DIR/../src/un.py"

# Skip if credentials not set
if [[ -z "${UNSANDBOX_PUBLIC_KEY:-}" || -z "${UNSANDBOX_SECRET_KEY:-}" ]]; then
    echo "SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set"
    exit 0
fi

REAL_PK="$UNSANDBOX_PUBLIC_KEY"
REAL_SK="$UNSANDBOX_SECRET_KEY"
GARBAGE_PK="unsb-pk-0000-0000-0000-0000"
GARBAGE_SK="unsb-sk-00000-00000-00000-00000"

TMPHOME="$(mktemp -d)"
trap 'rm -rf "$TMPHOME"' EXIT

UNSANDBOX_DIR="$TMPHOME/.unsandbox"
mkdir -p "$UNSANDBOX_DIR"
chmod 700 "$UNSANDBOX_DIR"

# accounts.csv: row 0 = garbage, row 1 = real creds
cat > "$UNSANDBOX_DIR/accounts.csv" <<EOF
$GARBAGE_PK,$GARBAGE_SK
$REAL_PK,$REAL_SK
EOF
chmod 600 "$UNSANDBOX_DIR/accounts.csv"

PASS=0
FAIL=0

run_test() {
    local name="$1"
    local expected_exit="$2"
    shift 2
    local output
    local actual_exit=0
    output="$(HOME="$TMPHOME" "$@" 2>&1)" || actual_exit=$?
    if [[ "$actual_exit" -eq "$expected_exit" ]]; then
        echo "PASS: $name"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name (expected exit $expected_exit, got $actual_exit)"
        echo "  output: $output"
        FAIL=$((FAIL + 1))
    fi
}

# Test 1: --account 1 with garbage env vars -> should succeed (exit 0)
# --account 1 selects the real creds from row 1, ignoring garbage env vars
run_test \
    "--account 1 overrides garbage env vars" \
    0 \
    env UNSANDBOX_PUBLIC_KEY="$GARBAGE_PK" UNSANDBOX_SECRET_KEY="$GARBAGE_SK" \
    python3 "$UN_PY" --account 1 key

# Test 2: --account 0 with real env vars -> should fail auth (exit 3)
# --account 0 selects garbage creds from row 0, ignoring real env vars
run_test \
    "--account 0 overrides real env vars (expects 401)" \
    3 \
    env UNSANDBOX_PUBLIC_KEY="$REAL_PK" UNSANDBOX_SECRET_KEY="$REAL_SK" \
    python3 "$UN_PY" --account 0 key

# Test 3: no --account with real env vars -> should succeed (exit 0)
run_test \
    "no --account uses env vars" \
    0 \
    env UNSANDBOX_PUBLIC_KEY="$REAL_PK" UNSANDBOX_SECRET_KEY="$REAL_SK" \
    python3 "$UN_PY" key

echo ""
echo "Results: $PASS passed, $FAIL failed"
if [[ "$FAIL" -gt 0 ]]; then
    exit 1
fi
