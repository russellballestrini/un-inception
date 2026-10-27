#!/bin/bash
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

# Test suite for Fortran Unsandbox SDK
# Run: bash tests/test_un.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SDK_DIR="$SCRIPT_DIR/../sync/src"
SOURCE="$SDK_DIR/un.f90"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

TESTS_RUN=0
TESTS_PASSED=0

# Test helper
test_that() {
    local description="$1"
    local test_cmd="$2"
    TESTS_RUN=$((TESTS_RUN + 1))

    if eval "$test_cmd" >/dev/null 2>&1; then
        echo -e "[${GREEN}PASS${NC}] $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "[${RED}FAIL${NC}] $description"
        return 1
    fi
}

echo ""
echo "=== Source File ==="
test_that "Source file exists" "[ -f '$SOURCE' ]"

echo ""
echo "=== Command Handlers ==="
test_that "Session handler defined" "grep -q 'subroutine handle_session' '$SOURCE'"
test_that "Service handler defined" "grep -q 'subroutine handle_service' '$SOURCE'"
test_that "Snapshot handler defined" "grep -q 'subroutine handle_snapshot' '$SOURCE'"
test_that "Image handler defined" "grep -q 'subroutine handle_image' '$SOURCE'"
test_that "Key handler defined" "grep -q 'subroutine handle_key' '$SOURCE'"
test_that "Languages handler defined" "grep -q 'subroutine handle_languages' '$SOURCE'"

echo ""
echo "=== Command Dispatch ==="
test_that "Session dispatch" "grep -q \"trim(arg) == 'session'\" '$SOURCE'"
test_that "Service dispatch" "grep -q \"trim(arg) == 'service'\" '$SOURCE'"
test_that "Snapshot dispatch" "grep -q \"trim(arg) == 'snapshot'\" '$SOURCE'"
test_that "Image dispatch" "grep -q \"trim(arg) == 'image'\" '$SOURCE'"
test_that "Key dispatch" "grep -q \"trim(arg) == 'key'\" '$SOURCE'"
test_that "Languages dispatch" "grep -q \"trim(arg) == 'languages'\" '$SOURCE'"

echo ""
echo "=== Snapshot Operations ==="
test_that "Snapshot --list" "grep -q \"operation = 'list'\" '$SOURCE' || grep -q 'list_mode' '$SOURCE'"
test_that "Snapshot --info" "grep -q \"operation = 'info'\" '$SOURCE'"
test_that "Snapshot --delete" "grep -q \"operation = 'delete'\" '$SOURCE'"
test_that "Snapshot --lock" "grep -q \"operation = 'lock'\" '$SOURCE'"
test_that "Snapshot --unlock" "grep -q \"operation = 'unlock'\" '$SOURCE'"
test_that "Snapshot --restore" "grep -q \"operation = 'restore'\" '$SOURCE'"
test_that "Snapshot --clone" "grep -q \"operation = 'clone'\" '$SOURCE'"

echo ""
echo "=== Help Text ==="
test_that "Help shows snapshot" "grep -q 'snapshot' '$SOURCE'"
test_that "Help shows --list" "grep -q '\\-\\-list' '$SOURCE'"
test_that "Help shows --info" "grep -q '\\-\\-info' '$SOURCE'"
test_that "Help shows --restore" "grep -q '\\-\\-restore' '$SOURCE'"
test_that "Help shows --clone" "grep -q '\\-\\-clone' '$SOURCE'"

echo ""
echo "=== HMAC Authentication ==="
test_that "Keeps secret off argv (no openssl -hmac)" "! grep -qE 'dgst[^|]*-hmac' '$SOURCE'"
test_that "Has X-Signature header" "grep -q 'X-Signature' '$SOURCE'"
test_that "Has X-Timestamp header" "grep -q 'X-Timestamp' '$SOURCE'"

echo ""
echo "=== Sudo OTP Handling ==="
test_that "Handles 428 response" "grep -q '428' '$SOURCE'"
test_that "Has X-Sudo-OTP header" "grep -q 'X-Sudo-OTP' '$SOURCE'"

echo ""
echo "=== Module Structure ==="
test_that "Has unsandbox_sdk module" "grep -q 'module unsandbox_sdk' '$SOURCE'"
test_that "Has unsandbox_client type" "grep -q 'type :: unsandbox_client' '$SOURCE'"
test_that "Has execution_result type" "grep -q 'type :: execution_result' '$SOURCE'"

echo ""
echo "=== --os Flag (offline mock API) ==="
# Compiles our CLI and points it at a local recording mock (UNSANDBOX_API_URL)
# with throwaway keys and a temp HOME: no network, no real credentials.
if command -v gfortran >/dev/null 2>&1 && command -v python3 >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
    OS_WORK="$(mktemp -d)"
    cat > "$OS_WORK/mock.py" <<'PY'
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer
log_path, port_path = sys.argv[1], sys.argv[2]
class H(BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length") or 0)).decode()
        with open(log_path, "a") as f:
            f.write(json.dumps({"path": self.path, "body": body}) + "\n")
        data = b'{"stdout":"","stderr":"","exit_code":0}'
        self.send_response(200 if self.path.startswith("/execute") else 400)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)
    do_GET = do_POST
    def log_message(self, *a):
        pass
srv = HTTPServer(("127.0.0.1", 0), H)
open(port_path, "w").write(str(srv.server_address[1]))
srv.serve_forever()
PY
    python3 "$OS_WORK/mock.py" "$OS_WORK/log" "$OS_WORK/port" &
    OS_MOCK=$!
    for _ in $(seq 1 50); do [ -s "$OS_WORK/port" ] && break; sleep 0.1; done
    gfortran -ffree-line-length-none -o "$OS_WORK/un" "$SOURCE" 2>/dev/null
    echo 'echo hi' > "$OS_WORK/hi.sh"
    mkdir -p "$OS_WORK/home"

    # os_run ARGS...: run our CLI against our mock; sets OS_RC
    os_run() {
        : > "$OS_WORK/log"
        (cd "$OS_WORK" && HOME="$OS_WORK/home" \
            UNSANDBOX_API_URL="http://127.0.0.1:$(cat "$OS_WORK/port")" \
            UNSANDBOX_PUBLIC_KEY=unsb-pk-test-0000-0000-0000 \
            UNSANDBOX_SECRET_KEY=unsb-sk-test0-00000-00000-00000 \
            timeout 30 ./un "$@" </dev/null >"$OS_WORK/out" 2>&1)
        OS_RC=$?
    }
    # os_of PATH: "os" of our first POST to PATH, <absent> or <no-request>
    os_of() {
        python3 -c 'import json,sys
for l in open(sys.argv[1]):
    r = json.loads(l)
    if r["path"] == sys.argv[2]:
        print(json.loads(r["body"]).get("os", "<absent>")); break
else:
    print("<no-request>")' "$OS_WORK/log" "$1"
    }

    os_run --os freebsd hi.sh
    test_that "execute --os freebsd sends os freebsd" "[ \"$(os_of /execute)\" = freebsd ]"
    os_run hi.sh
    test_that "execute without --os sends no os key" "[ \"$(os_of /execute)\" = '<absent>' ]"
    os_run --os=FreeBSD hi.sh
    test_that "execute --os=FreeBSD normalizes to freebsd" "[ \"$(os_of /execute)\" = freebsd ]"
    os_run --os freebsd session
    test_that "session --os freebsd sends os freebsd" "[ \"$(os_of /sessions)\" = freebsd ]"
    os_run session
    test_that "session without --os sends no os key" "[ \"$(os_of /sessions)\" = '<absent>' ]"
    os_run --os linux service --name ostest
    test_that "service --os linux sends os linux" "[ \"$(os_of /services)\" = linux ]"
    os_run service --name ostest
    test_that "service without --os sends no os key" "[ \"$(os_of /services)\" = '<absent>' ]"
    os_run --os openbsd hi.sh
    test_that "--os openbsd rejected, nothing sent" "[ $OS_RC -ne 0 ] && grep -q openbsd '$OS_WORK/out' && [ ! -s '$OS_WORK/log' ]"
    os_run --os '' hi.sh
    test_that "--os '' rejected, nothing sent" "[ $OS_RC -ne 0 ] && grep -q 'invalid --os' '$OS_WORK/out' && [ ! -s '$OS_WORK/log' ]"
    os_run --os
    test_that "--os without value rejected, nothing sent" "[ $OS_RC -ne 0 ] && grep -q -- '--os requires a value' '$OS_WORK/out' && [ ! -s '$OS_WORK/log' ]"

    kill "$OS_MOCK" 2>/dev/null
    rm -rf "$OS_WORK"
else
    echo "[SKIP] gfortran, python3 or jq not installed"
fi

echo ""
echo "=== Summary ==="
echo "Tests passed: $TESTS_PASSED / $TESTS_RUN"

if [ $TESTS_PASSED -eq $TESTS_RUN ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
