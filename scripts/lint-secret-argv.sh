#!/bin/bash
# Reject code that puts an API secret on a process command line (ps, /proc
# and audit logs all see argv) or parks a response that may carry one in a
# fixed /tmp path. Every client signs in-process or through pipes; see the
# "No secret on any command line" rule in clients/CLI_SPEC.md.
set -u
cd "$(dirname "$0")/.."

# pattern<TAB>why
RULES='dgst[^|]*-hmac	openssl -hmac takes the key as an argument
hexkey:	openssl -macopt hexkey: takes the key as an argument
--arg[[:space:]]+sk[[:space:]]	jq --arg puts the value in argv; use env.NAME
-e[[:space:]]+UNSANDBOX_SECRET_KEY=	-e KEY=VALUE puts the secret in argv
/tmp/unsandbox_curl_response	fixed /tmp response file can hold a rotated secret'

FILES=$(ls un.* Un.* clients/*/sync/src/* scripts/*.sh .gitlab-ci.yml 2>/dev/null | grep -v '^scripts/lint-secret-argv.sh$')

fail=0
while IFS=$'\t' read -r pat why; do
    hits=$(grep -nE -- "$pat" $FILES 2>/dev/null)
    if [ -n "$hits" ]; then
        echo "FAIL: $why"
        echo "$hits" | sed 's/^/    /' | cut -c1-200
        fail=1
    fi
done <<< "$RULES"

[ "$fail" -eq 0 ] && echo "secret-argv lint: clean"
exit "$fail"
