#!/bin/bash
# Regression test for ticket #78: commands and snapshot names larger than the
# old fixed payload buffers crashed the CLI (SIGSEGV / fortify abort) before
# any request left the machine. Uses throwaway keys, so the API answers 401;
# the test only asserts the CLI survives to receive that answer.
set -u
UN=${1:-./un}
export UNSANDBOX_PUBLIC_KEY=unsb-pk-test-0078 UNSANDBOX_SECRET_KEY=unsb-sk-test-0078
BIG=$(head -c 65536 /dev/zero | tr '\0' 'A')
fail=0

check() {
    local label=$1; shift
    "$UN" "$@" >/dev/null 2>&1
    local rc=$?
    if [ $rc -ge 128 ]; then
        echo "FAIL: $label crashed (exit $rc)"; fail=1
    else
        echo "ok:   $label (exit $rc)"
    fi
}

check "service --execute 64KB command" service --execute nosuch-svc-78 "echo $BIG"
check "service --snapshot 64KB name"   service --snapshot nosuch-svc-78 --snapshot-name "$BIG"
check "session --snapshot 64KB name"   session --snapshot nosuch-sess-78 --snapshot-name "$BIG"

exit $fail
