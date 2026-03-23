#!/usr/bin/env bash
# Integration test: --account N flag must take priority over env vars
#
# Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY (real credentials)
# Run:      make test-integration   OR   bash tests/test_account_flag.sh
#
# The defect this guards against: ResolveCredentials() checked env vars before
# account_index, so --account N was silently ignored when env vars existed.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/../src"
UN_BIN="$SCRIPT_DIR/../un"

RED='\033[31m'
GREEN='\033[32m'
NC='\033[0m'

pass=0
fail=0

check() {
    local desc="$1" result="$2"
    if [ "$result" = "pass" ]; then
        printf "  ${GREEN}✓${NC} %s\n" "$desc"
        pass=$((pass + 1))
    else
        printf "  ${RED}✗${NC} %s\n" "$desc"
        fail=$((fail + 1))
    fi
}

# Require real credentials to be available
if [ -z "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -z "${UNSANDBOX_SECRET_KEY:-}" ]; then
    echo "SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set"
    exit 0
fi

# Build the binary if it doesn't exist or source is newer
if [ ! -x "$UN_BIN" ] || [ "$SRC_DIR/un.go" -nt "$UN_BIN" ]; then
    echo "Building Go binary..."
    (cd "$SRC_DIR" && go build -o "$UN_BIN" .) || {
        echo "FAIL: go build failed"
        exit 1
    }
fi

if [ ! -x "$UN_BIN" ]; then
    echo "FAIL: UN binary not found at $UN_BIN — run go build first"
    exit 1
fi

REAL_PK="$UNSANDBOX_PUBLIC_KEY"
REAL_SK="$UNSANDBOX_SECRET_KEY"

# Temporary HOME with accounts.csv:
#   index 0: garbage credentials (will always 401)
#   index 1: real credentials (will succeed)
TMPHOME="$(mktemp -d)"
mkdir -p "$TMPHOME/.unsandbox"
trap 'rm -rf "$TMPHOME"' EXIT

cat > "$TMPHOME/.unsandbox/accounts.csv" <<CSV
unsb-pk-fake-0000-0000-0000,unsb-sk-fake0-00000-00000-00000
${REAL_PK},${REAL_SK}
CSV

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "INTEGRATION: --account flag priority test"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# --- Test 1: --account 1 should use CSV row 1 (real creds), ignoring env vars ---
# Set env vars to GARBAGE so the test fails if env vars win
OUT=$(HOME="$TMPHOME" \
      UNSANDBOX_PUBLIC_KEY=unsb-pk-fake-0000-0000-0000 \
      UNSANDBOX_SECRET_KEY=unsb-sk-fake0-00000-00000-00000 \
      "$UN_BIN" --account 1 key 2>&1 || true)

if ! echo "$OUT" | grep -qi "401\|unauthorized\|invalid_credential\|No credentials"; then
    check "--account 1 uses CSV row 1 (real creds) over garbage env vars" "pass"
else
    check "--account 1 uses CSV row 1 (real creds) over garbage env vars" "fail"
    echo "    output: $OUT"
fi

# --- Test 2: --account 0 should use CSV row 0 (garbage creds) → 401 ---
# Even though real env vars are set, explicit --account 0 should pick garbage creds
OUT=$(HOME="$TMPHOME" \
      UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
      UNSANDBOX_SECRET_KEY="$REAL_SK" \
      "$UN_BIN" --account 0 key 2>&1 || true)

if echo "$OUT" | grep -qi "401\|unauthorized\|invalid\|error"; then
    check "--account 0 uses CSV row 0 (garbage creds) despite real env vars, 401 as expected" "pass"
else
    check "--account 0 uses CSV row 0 (garbage creds) despite real env vars, 401 as expected" "fail"
    echo "    output: $OUT"
fi

# --- Test 3: no --account flag, real env vars → env vars win over garbage CSV row 0 ---
OUT=$(HOME="$TMPHOME" \
      UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
      UNSANDBOX_SECRET_KEY="$REAL_SK" \
      "$UN_BIN" key 2>&1 || true)

if ! echo "$OUT" | grep -qi "401\|unauthorized\|invalid_credential\|No credentials"; then
    check "No --account flag: env vars used, succeeds" "pass"
else
    check "No --account flag: env vars used, succeeds" "fail"
    echo "    output: $OUT"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
printf "Passed: ${GREEN}%d${NC}  Failed: ${RED}%d${NC}\n" "$pass" "$fail"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

[ "$fail" -eq 0 ]
