#!/usr/bin/env bash
# Offline CLI test: `un key` output and `un key --buy-credit` (portal ticket
# #155, tiers retired), for ANY client.
#
# Usage: bash tests/key_output_cli.sh CMD [ARGS...]
#   bash tests/key_output_cli.sh node clients/javascript/sync/src/un.js
#   bash tests/key_output_cli.sh python3 clients/python/sync/src/un.py
#
# Points our CLI at a local mock API + portal (UNSANDBOX_API_URL,
# UNSANDBOX_PORTAL_URL) answering POST /keys/validate with canned payloads, then
# checks our shared contract (clients/CLI_SPEC.md "Key Command"):
#   - "Billing: <billing>" from `billing`, derived from deprecated `tier` when
#     absent (-1 free, 0 demo, else credit); no "Tier:" line ever
#   - "Concurrency: N" ("N/A" when absent)
#   - "Credit: $X.XX" from credit_balance_usd, credit keys only
#   - expired keys point at <portal>/buy-credit?key=<pk>, never /keys/extend
#   - key --buy-credit prints <portal>/buy-credit?key=<pk> and exits 0
#
# Browser openers (xdg-open, open, ...) are stubbed on PATH; BROWSER=true.
# Needs python3 for our mock, no credentials, no network beyond 127.0.0.1.

set -euo pipefail
export PYTHONUNBUFFERED=1

if [ "$#" -lt 1 ]; then
    echo "usage: $0 CMD [ARGS...]" >&2
    exit 2
fi
UN_CMD=("$@")

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

WORK="$(mktemp -d)"
LOG="$WORK/requests.jsonl"
PORT_FILE="$WORK/port"
REPLY_FILE="$WORK/reply.json"
mkdir -p "$WORK/home" "$WORK/bin"
: > "$LOG"

# Browser stubs: record our URL, open nothing.
for b in xdg-open open sensible-browser x-www-browser gnome-open wslview www-browser; do
    printf '#!/bin/sh\necho "$@" >> "%s/browser.log"\nexit 0\n' "$WORK" > "$WORK/bin/$b"
    chmod +x "$WORK/bin/$b"
done

# Mock: every request is logged; POST /keys/validate answers with the
# {"code": N, "body": {...}} found in reply.json at request time.
cat > "$WORK/mock.py" <<'PY'
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer

log_path, port_path, reply_path = sys.argv[1], sys.argv[2], sys.argv[3]

class H(BaseHTTPRequestHandler):
    def _record(self):
        with open(log_path, "a") as f:
            f.write(json.dumps({"method": self.command, "path": self.path}) + "\n")

    def _reply(self, code, obj):
        data = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        if n:
            self.rfile.read(n)
        self._record()
        if self.path.split("?")[0] == "/keys/validate":
            with open(reply_path) as f:
                r = json.load(f)
            self._reply(r["code"], r["body"])
        else:
            self._reply(404, {"error": "mock"})

    def do_GET(self):
        self._record()
        self._reply(404, {"error": "mock"})

    def log_message(self, *a):
        pass

srv = HTTPServer(("127.0.0.1", 0), H)
with open(port_path, "w") as f:
    f.write(str(srv.server_address[1]))
srv.serve_forever()
PY

python3 "$WORK/mock.py" "$LOG" "$PORT_FILE" "$REPLY_FILE" &
MOCK_PID=$!
trap 'kill $MOCK_PID 2>/dev/null || true; rm -rf "$WORK"' EXIT

for _ in $(seq 1 50); do
    [ -s "$PORT_FILE" ] && break
    sleep 0.1
done
if [ ! -s "$PORT_FILE" ]; then
    echo "FAIL: mock API did not start"
    exit 1
fi
PORT="$(cat "$PORT_FILE")"
BASE="http://127.0.0.1:$PORT"
PK=unsb-pk-test-0000-0000-0000
BUY_CREDIT_URL="$BASE/buy-credit?key=$PK"
FUTURE_MS=$(( ($(date +%s) + 30 * 86400) * 1000 ))

reply() { printf '%s' "$1" > "$REPLY_FILE"; }

# HOME points at a throwaway dir (no real accounts.csv, no cache writes); keep
# per-user package dirs that live under the real HOME reachable.
REAL_HOME="$HOME"
export PYTHONUSERBASE="${PYTHONUSERBASE:-$REAL_HOME/.local}"
export JULIA_DEPOT_PATH="${JULIA_DEPOT_PATH:-$REAL_HOME/.julia:}"
export R_LIBS_USER="${R_LIBS_USER:-$(ls -d "$REAL_HOME"/R/*/* 2>/dev/null | head -1)}"

# Run our CLI against our mock with throwaway credentials. OUT has ANSI colors
# stripped and CR removed so we can match whole lines.
run_un() {
    : > "$LOG"
    set +e
    RAW=$(HOME="$WORK/home" \
          PATH="$WORK/bin:$PATH" \
          BROWSER=true DISPLAY= WAYLAND_DISPLAY= \
          UNSANDBOX_API_URL="$BASE" \
          UNSANDBOX_PORTAL_URL="$BASE" \
          UNSANDBOX_PUBLIC_KEY="$PK" \
          UNSANDBOX_SECRET_KEY=unsb-sk-test0-00000-00000-00000 \
          timeout "${KEY_OUTPUT_TIMEOUT:-60}" "${UN_CMD[@]}" "$@" </dev/null 2>&1)
    RC=$?
    set -e
    OUT=$(printf '%s\n' "$RAW" | sed -e 's/\x1b\[[0-9;]*[A-Za-z]//g' -e 's/\r$//' -e 's/[[:space:]]*$//')
}

has_line() { printf '%s\n' "$OUT" | grep -qxF -- "$1"; }
has_text() { printf '%s\n' "$OUT" | grep -qF -- "$1"; }

# expect DESC CONDITION... : every condition is "line:TEXT", "noline-prefix:TEXT",
# "text:TEXT", "notext:TEXT" or "rc:N" / "rc-nonzero".
expect() {
    local desc="$1"; shift
    local ok=1 why=""
    for c in "$@"; do
        case "$c" in
            line:*)          has_line "${c#line:}" || { ok=0; why="missing line '${c#line:}'"; } ;;
            noline-prefix:*) printf '%s\n' "$OUT" | grep -q "^${c#noline-prefix:}" && { ok=0; why="unexpected line '${c#noline-prefix:}...'"; } ;;
            text:*)          has_text "${c#text:}" || { ok=0; why="missing '${c#text:}'"; } ;;
            notext:*)        has_text "${c#notext:}" && { ok=0; why="unexpected '${c#notext:}'"; } ;;
            rc:*)            [ "$RC" = "${c#rc:}" ] || { ok=0; why="exit $RC, want ${c#rc:}"; } ;;
            rc-nonzero)      [ "$RC" != 0 ] || { ok=0; why="exit 0, want non-zero"; } ;;
        esac
        [ "$ok" = 1 ] || break
    done
    if [ "$ok" = 1 ]; then
        check "$desc" pass
    else
        check "$desc ($why)" fail
        echo "    exit: $RC"
        printf '%s\n' "$OUT" | sed 's/^/    | /'
    fi
}

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "OFFLINE: key output + --buy-credit: ${UN_CMD[*]}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"tier\":5,\"billing\":\"credit\",\"concurrency\":4,\"credit_balance_micros\":12345678,\"credit_balance_usd\":\"12.35\",\"rate_per_minute\":60,\"burst\":10,\"valid_through\":$FUTURE_MS}}"
run_un key
expect "credit key: Billing, Concurrency, Credit, no Tier" \
    rc:0 "line:Public Key: $PK" "line:Billing: credit" "line:Concurrency: 4" 'line:Credit: $12.35' \
    "line:Status: active" noline-prefix:Tier

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"tier\":-1,\"billing\":\"free\",\"concurrency\":1,\"valid_through\":$FUTURE_MS}}"
run_un key
expect "free key: Billing: free, no Credit line" \
    rc:0 "line:Billing: free" "line:Concurrency: 1" noline-prefix:Credit noline-prefix:Tier

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"tier\":-1,\"concurrency\":1,\"valid_through\":$FUTURE_MS}}"
run_un key
expect "no billing field, tier -1: derives free" \
    rc:0 "line:Billing: free" noline-prefix:Tier

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"tier\":0,\"concurrency\":2,\"valid_through\":$FUTURE_MS}}"
run_un key
expect "no billing field, tier 0: derives demo" \
    rc:0 "line:Billing: demo" "line:Concurrency: 2" noline-prefix:Tier

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"tier\":3,\"concurrency\":8,\"valid_through\":$FUTURE_MS}}"
run_un key
expect "no billing field, tier 3: derives credit, no balance means no Credit line" \
    rc:0 "line:Billing: credit" "line:Concurrency: 8" noline-prefix:Credit noline-prefix:Tier

reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"billing\":\"credit\",\"valid_through\":$FUTURE_MS}}"
run_un key
expect "no concurrency field: Concurrency: N/A" \
    rc:0 "line:Billing: credit" "line:Concurrency: N/A"

reply '{"code":401,"body":{"error":":expired","valid":false}}'
run_un key
expect "expired key points at /buy-credit?key=, not /keys/extend" \
    rc-nonzero "line:Public Key: $PK" "text:$BUY_CREDIT_URL" notext:/keys/extend

# --buy-credit needs no request; a valid reply keeps clients that validate first honest.
reply "{\"code\":200,\"body\":{\"valid\":true,\"status\":\"active\",\"billing\":\"credit\",\"concurrency\":4,\"valid_through\":$FUTURE_MS}}"
run_un key --buy-credit
expect "key --buy-credit prints $BASE/buy-credit?key=<pk>, exits 0" \
    rc:0 "text:$BUY_CREDIT_URL" notext:/keys/extend

echo ""
echo "Passed: $pass  Failed: $fail"
[ "$fail" -eq 0 ]
