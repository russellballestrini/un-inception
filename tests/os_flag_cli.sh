#!/usr/bin/env bash
# Offline CLI test: global --os flag (tickets #123, #124), for ANY client.
#
# Usage: bash tests/os_flag_cli.sh CMD [ARGS...]
#   bash tests/os_flag_cli.sh node clients/javascript/sync/src/un.js
#   bash tests/os_flag_cli.sh perl clients/perl/sync/src/un.pl
#
# Points our CLI at a local mock API (UNSANDBOX_API_URL) that records every
# request body, then checks our shared contract (clients/CLI_SPEC.md):
#   - --os freebsd puts "os":"freebsd" in POST /execute, /sessions, /services
#   - no --os flag means no "os" key at all
#   - --os is case-insensitive and accepts --os=VALUE
#   - --os openbsd / --os "" / bare --os exit non-zero with an error naming
#     our value or flag, and send nothing
# --os goes before our subcommand or code, as a global option.
#
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
mkdir -p "$WORK/home"
: > "$LOG"

# Mock API: logs {"method","path","body"} per request, answers with canned JSON.
# Sessions and services answer 400 so clients stop after our POST (no websocket).
cat > "$WORK/mock.py" <<'PY'
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer

log_path, port_path = sys.argv[1], sys.argv[2]

class H(BaseHTTPRequestHandler):
    def _record(self, body):
        with open(log_path, "a") as f:
            f.write(json.dumps({"method": self.command, "path": self.path, "body": body}) + "\n")

    def _reply(self, code, obj):
        data = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(n).decode()
        self._record(body)
        if self.path.startswith("/execute"):
            self._reply(200, {"success": True, "status": "completed", "stdout": "", "stderr": "", "exit_code": 0})
        else:
            self._reply(400, {"error": "mock: not created"})

    def do_GET(self):
        self._record("")
        self._reply(404, {"error": "mock"})

    def log_message(self, *a):
        pass

srv = HTTPServer(("127.0.0.1", 0), H)
with open(port_path, "w") as f:
    f.write(str(srv.server_address[1]))
srv.serve_forever()
PY

python3 "$WORK/mock.py" "$LOG" "$PORT_FILE" &
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

# Run our CLI against our mock with throwaway credentials. Output is kept for diagnostics.
run_un() {
    : > "$LOG"
    set +e
    OUT=$(HOME="$WORK/home" \
          UNSANDBOX_API_URL="http://127.0.0.1:$PORT" \
          UNSANDBOX_PUBLIC_KEY=unsb-pk-test-0000-0000-0000 \
          UNSANDBOX_SECRET_KEY=unsb-sk-test0-00000-00000-00000 \
          timeout "${OS_FLAG_TIMEOUT:-30}" "${UN_CMD[@]}" "$@" </dev/null 2>&1)
    RC=$?
    set -e
}

# Prints the "os" value of our first POST to PATH, "<absent>" when the key is
# missing, "<no-request>" when no such POST was made.
os_of() {
    python3 - "$LOG" "$1" <<'PY'
import json, sys
log, prefix = sys.argv[1], sys.argv[2]
for line in open(log):
    r = json.loads(line)
    if r["method"] == "POST" and r["path"].split("?")[0] == prefix:
        print(json.loads(r["body"]).get("os", "<absent>"))
        break
else:
    print("<no-request>")
PY
}

expect_os() {
    local desc="$1" path="$2" want="$3"
    local got
    got="$(os_of "$path")"
    if [ "$got" = "$want" ]; then
        check "$desc" pass
    else
        check "$desc (got $got, want $want)" fail
        echo "    exit: $RC"
        echo "    output: $OUT"
    fi
}

expect_rejected() {
    local desc="$1" pattern="$2"
    if [ "$RC" -ne 0 ] && echo "$OUT" | grep -q -- "$pattern" && [ ! -s "$LOG" ]; then
        check "$desc" pass
    else
        check "$desc" fail
        echo "    exit: $RC  requests: $(wc -l < "$LOG")"
        echo "    output: $OUT"
    fi
}

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "OFFLINE: --os flag request bodies: ${UN_CMD[*]}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# execute
run_un --os freebsd -s bash 'echo hi'
expect_os "execute --os freebsd sends \"os\":\"freebsd\"" /execute freebsd

run_un -s bash 'echo hi'
expect_os "execute without --os sends no os key" /execute "<absent>"

run_un --os FreeBSD -s bash 'echo hi'
expect_os "execute --os FreeBSD (mixed case) normalizes to freebsd" /execute freebsd

run_un --os=linux -s bash 'echo hi'
expect_os "execute --os=linux sends \"os\":\"linux\"" /execute linux

# session
run_un --os freebsd session
expect_os "session --os freebsd sends \"os\":\"freebsd\"" /sessions freebsd

run_un session
expect_os "session without --os sends no os key" /sessions "<absent>"

# service
run_un --os freebsd service --name ostest --ports 80 --bootstrap true
expect_os "service --os freebsd sends \"os\":\"freebsd\"" /services freebsd

run_un service --name ostest --ports 80 --bootstrap true
expect_os "service without --os sends no os key" /services "<absent>"

# invalid values: non-zero exit, an error naming our flag or value, nothing sent
run_un --os openbsd -s bash 'echo hi'
expect_rejected "--os openbsd exits non-zero with an error and sends nothing" "openbsd"

run_un --os "" -s bash 'echo hi'
expect_rejected "--os '' exits non-zero with an error and sends nothing" "os"

run_un --os
expect_rejected "--os with no value exits non-zero and sends nothing" "--os"

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
printf "Passed: ${GREEN}%d${NC}  Failed: ${RED}%d${NC}\n" "$pass" "$fail"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

[ "$fail" -eq 0 ]
