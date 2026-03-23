#!/usr/bin/env bash
# Integration test for --account N credential selection in un.js.
#
# Tests that --account N selects the correct row from accounts.csv,
# taking priority over environment variables.
#
# Requires UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY to be set.
# Skips if credentials are not available.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_JS="$SCRIPT_DIR/../src/un.js"

PASS=0
FAIL=0
SKIP=0

pass() { echo "PASS: $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL: $1"; FAIL=$((FAIL + 1)); }
skip() { echo "SKIP: $1"; SKIP=$((SKIP + 1)); }

# Require real credentials from environment
if [ -z "${UNSANDBOX_PUBLIC_KEY:-}" ] || [ -z "${UNSANDBOX_SECRET_KEY:-}" ]; then
  skip "UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY not set"
  echo "Results: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"
  exit 0
fi

REAL_PK="$UNSANDBOX_PUBLIC_KEY"
REAL_SK="$UNSANDBOX_SECRET_KEY"
GARBAGE_PK="unsb-pk-0000-0000-0000-garbage"
GARBAGE_SK="unsb-sk-00000-00000-00000-garbage"

# Build a temporary HOME with accounts.csv: row 0 = garbage, row 1 = real creds
TMPHOME="$(mktemp -d)"
mkdir -p "$TMPHOME/.unsandbox"
# Header + row 0 (garbage) + row 1 (real)
printf 'public_key,secret_key\n%s,%s\n%s,%s\n' \
  "$GARBAGE_PK" "$GARBAGE_SK" \
  "$REAL_PK" "$REAL_SK" \
  > "$TMPHOME/.unsandbox/accounts.csv"

cleanup() {
  rm -rf "$TMPHOME"
}
trap cleanup EXIT

# ---------------------------------------------------------------------------
# Test 1: HOME=TMPHOME, env vars = garbage, --account 1 => row 1 = real creds
#         Expect: key command succeeds (no auth error)
# ---------------------------------------------------------------------------
if HOME="$TMPHOME" \
   UNSANDBOX_PUBLIC_KEY="$GARBAGE_PK" \
   UNSANDBOX_SECRET_KEY="$GARBAGE_SK" \
   node "$UN_JS" --account 1 key 2>&1 | grep -qiE 'Public Key|key_id|account|status'; then
  pass "Test 1: --account 1 selects row 1 (real creds) over env garbage"
else
  # Also accept a successful JSON response (key validates)
  output="$(HOME="$TMPHOME" \
             UNSANDBOX_PUBLIC_KEY="$GARBAGE_PK" \
             UNSANDBOX_SECRET_KEY="$GARBAGE_SK" \
             node "$UN_JS" --account 1 key 2>&1 || true)"
  if echo "$output" | grep -qiE '401|invalid|unauthorized|authentication'; then
    fail "Test 1: --account 1 selected row 1 but auth failed (real creds may be invalid)"
  else
    pass "Test 1: --account 1 selects row 1 (real creds) over env garbage"
  fi
fi

# ---------------------------------------------------------------------------
# Test 2: HOME=TMPHOME, env vars = real, --account 0 => row 0 = garbage creds
#         Expect: 401 / auth error (garbage creds used despite real env vars)
# ---------------------------------------------------------------------------
output2="$(HOME="$TMPHOME" \
           UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
           UNSANDBOX_SECRET_KEY="$REAL_SK" \
           node "$UN_JS" --account 0 key 2>&1 || true)"
if echo "$output2" | grep -qiE '401|invalid|unauthorized|authentication|error'; then
  pass "Test 2: --account 0 selects row 0 (garbage) over env real creds (expected auth failure)"
else
  fail "Test 2: --account 0 should use garbage creds and fail auth, but got: $output2"
fi

# ---------------------------------------------------------------------------
# Test 3: HOME=TMPHOME, env vars = real, no --account => env vars win (real creds)
#         Expect: key command succeeds
# ---------------------------------------------------------------------------
output3="$(HOME="$TMPHOME" \
           UNSANDBOX_PUBLIC_KEY="$REAL_PK" \
           UNSANDBOX_SECRET_KEY="$REAL_SK" \
           node "$UN_JS" key 2>&1 || true)"
if echo "$output3" | grep -qiE '401|invalid|unauthorized|authentication failed'; then
  fail "Test 3: without --account, env real creds should succeed but got auth error: $output3"
else
  pass "Test 3: without --account, env vars (real creds) are used"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
echo "Results: PASS=$PASS FAIL=$FAIL SKIP=$SKIP"

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
exit 0
