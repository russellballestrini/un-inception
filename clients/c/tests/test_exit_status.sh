#!/usr/bin/env bash
# Offline test: un's exit status and service info (unsandbox ticket #126)
#
#   - un exits with the program's exit_code (3, 137 for a SIGKILLed program),
#     0 on success, 1 when the API answers an error. It used to exit 0
#     whatever the program did, so scripts and CI could not see failures.
#   - service --info shows the API's "state" and its JSON booleans (it showed
#     "Status: -" and always "no").
#
# Needs no credentials and no network beyond 127.0.0.1.
# Run: make test-exit   OR   bash tests/test_exit_status.sh

set -euo pipefail
export PYTHONUNBUFFERED=1

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UN_BIN="$SCRIPT_DIR/../un"

RED='\033[31m'
GREEN='\033[32m'
NC='\033[0m'
pass=0
fail=0

check() {
    if [ "$2" = "pass" ]; then
        printf "  ${GREEN}✓${NC} %s\n" "$1"; pass=$((pass + 1))
    else
        printf "  ${RED}✗${NC} %s\n" "$1"; fail=$((fail + 1))
    fi
}

[ -x "$UN_BIN" ] || { echo "FAIL: un not found at $UN_BIN, run make first"; exit 1; }

WORK="$(mktemp -d)"
PORT_FILE="$WORK/port"
mkdir -p "$WORK/home"

# Mock API. POST /execute answers exit_code N for code containing EXIT=N,
# an API error for code containing APIERROR. GET /services/<id> answers a
# service the way the API does (state, JSON booleans).
cat > "$WORK/mock.py" <<'PY'
import json, re, sys
from http.server import BaseHTTPRequestHandler, HTTPServer

port_path = sys.argv[1]

class H(BaseHTTPRequestHandler):
    def _reply(self, code, obj):
        data = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        code = json.loads(self.rfile.read(n).decode() or "{}").get("code", "")
        if "APIERROR" in code:
            self._reply(400, {"error": "mock: refused"})
            return
        m = re.search(r"EXIT=(\d+)", code)
        self._reply(200, {"success": True, "stdout": "out\n", "stderr": "",
                          "exit_code": int(m.group(1)) if m else 0})

    def do_GET(self):
        if self.path.startswith("/services/"):
            self._reply(200, {"id": "unsb-service-0000000000000001", "name": "svc",
                              "state": "running", "network_mode": "zerotrust",
                              "locked": True, "unfreeze_on_demand": True, "ports": [8080]})
        else:
            self._reply(404, {"error": "mock"})

    def log_message(self, *a):
        pass

srv = HTTPServer(("127.0.0.1", 0), H)
with open(port_path, "w") as f:
    f.write(str(srv.server_address[1]))
srv.serve_forever()
PY

python3 "$WORK/mock.py" "$PORT_FILE" &
MOCK_PID=$!
trap 'kill $MOCK_PID 2>/dev/null || true; rm -rf "$WORK"' EXIT
for _ in $(seq 1 50); do [ -s "$PORT_FILE" ] && break; sleep 0.1; done
[ -s "$PORT_FILE" ] || { echo "FAIL: mock API did not start"; exit 1; }
PORT="$(cat "$PORT_FILE")"

run_un() {
    set +e
    OUT=$(HOME="$WORK/home" \
          UNSANDBOX_API_URL="http://127.0.0.1:$PORT" \
          UNSANDBOX_PUBLIC_KEY=unsb-pk-test-0000-0000-0000 \
          UNSANDBOX_SECRET_KEY=unsb-sk-test0-00000-00000-00000 \
          timeout 20 "$UN_BIN" "$@" </dev/null 2>&1)
    RC=$?
    set -e
}

expect_rc() {
    local desc="$1" want="$2"; shift 2
    run_un "$@"
    if [ "$RC" = "$want" ]; then check "$desc" pass; else check "$desc (got $RC: $OUT)" fail; fi
}

echo "un exit status (ticket #126)"
expect_rc "program exits 0: un exits 0" 0 -s bash 'echo EXIT=0'
expect_rc "program exits 3: un exits 3" 3 -s bash 'echo EXIT=3'
expect_rc "program killed by SIGKILL (137): un exits 137" 137 -s bash 'echo EXIT=137'
expect_rc "API error: un exits 1" 1 -s bash 'echo APIERROR'

echo "service --info"
run_un service --info unsb-service-0000000000000001
for want in "Status:          running" "Locked:          yes" "Auto-Unfreeze:   yes"; do
    if printf '%s' "$OUT" | grep -qF "$want"; then check "shows '$want'" pass; else check "shows '$want' (got: $OUT)" fail; fi
done

echo ""
echo "Passed: $pass  Failed: $fail"
[ "$fail" -eq 0 ]
