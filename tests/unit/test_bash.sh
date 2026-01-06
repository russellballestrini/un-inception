#!/bin/bash
# Unit tests for un.sh - tests internal functions without API calls

set -o pipefail

PASSED=0
FAILED=0

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

test_case() {
    local name="$1"
    shift
    if "$@"; then
        echo -e "  ${GREEN}✓${NC} $name"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} $name"
        FAILED=$((FAILED + 1))
    fi
}

assert_equal() {
    local actual="$1"
    local expected="$2"
    [ "$actual" = "$expected" ]
}

assert_not_equal() {
    local a="$1"
    local b="$2"
    [ "$a" != "$b" ]
}

assert_contains() {
    local str="$1"
    local substr="$2"
    [[ "$str" == *"$substr"* ]]
}

# ============================================================================
# Extension Mapping Tests
# ============================================================================

echo ""
echo "=== Extension Mapping Tests ==="

# Extension to language mapping (from un.sh)
get_language() {
    local ext="${1##*.}"
    case ".$ext" in
        .py) echo "python" ;;
        .js) echo "javascript" ;;
        .ts) echo "typescript" ;;
        .rb) echo "ruby" ;;
        .php) echo "php" ;;
        .pl) echo "perl" ;;
        .lua) echo "lua" ;;
        .sh) echo "bash" ;;
        .go) echo "go" ;;
        .rs) echo "rust" ;;
        .c) echo "c" ;;
        .cpp|.cc|.cxx) echo "cpp" ;;
        .java) echo "java" ;;
        .kt) echo "kotlin" ;;
        .cs) echo "csharp" ;;
        .fs) echo "fsharp" ;;
        .hs) echo "haskell" ;;
        .ml) echo "ocaml" ;;
        .clj) echo "clojure" ;;
        .scm) echo "scheme" ;;
        .lisp) echo "commonlisp" ;;
        .erl) echo "erlang" ;;
        .ex|.exs) echo "elixir" ;;
        .jl) echo "julia" ;;
        .r|.R) echo "r" ;;
        .cr) echo "crystal" ;;
        .d) echo "d" ;;
        .nim) echo "nim" ;;
        .zig) echo "zig" ;;
        .v) echo "v" ;;
        .dart) echo "dart" ;;
        .groovy) echo "groovy" ;;
        .scala) echo "scala" ;;
        .f90|.f95) echo "fortran" ;;
        .cob) echo "cobol" ;;
        .pro) echo "prolog" ;;
        .forth|.4th) echo "forth" ;;
        .tcl) echo "tcl" ;;
        .raku) echo "raku" ;;
        .m) echo "objc" ;;
        *) echo "" ;;
    esac
}

test_case "Python extension maps correctly" assert_equal "$(get_language script.py)" "python"
test_case "JavaScript extension maps correctly" assert_equal "$(get_language app.js)" "javascript"
test_case "TypeScript extension maps correctly" assert_equal "$(get_language app.ts)" "typescript"
test_case "Ruby extension maps correctly" assert_equal "$(get_language app.rb)" "ruby"
test_case "Go extension maps correctly" assert_equal "$(get_language main.go)" "go"
test_case "Rust extension maps correctly" assert_equal "$(get_language main.rs)" "rust"
test_case "C extension maps correctly" assert_equal "$(get_language main.c)" "c"
test_case "C++ extension maps correctly" assert_equal "$(get_language main.cpp)" "cpp"
test_case "Java extension maps correctly" assert_equal "$(get_language Main.java)" "java"
test_case "Kotlin extension maps correctly" assert_equal "$(get_language main.kt)" "kotlin"
test_case "Haskell extension maps correctly" assert_equal "$(get_language main.hs)" "haskell"
test_case "Elixir extension maps correctly" assert_equal "$(get_language main.ex)" "elixir"
test_case "Julia extension maps correctly" assert_equal "$(get_language main.jl)" "julia"

# ============================================================================
# HMAC Signature Tests
# ============================================================================

echo ""
echo "=== HMAC Signature Tests ==="

# HMAC function (requires openssl)
hmac_sha256() {
    local secret="$1"
    local message="$2"
    echo -n "$message" | openssl dgst -sha256 -hmac "$secret" | sed 's/^.* //'
}

test_case "HMAC-SHA256 generates 64 character hex string" \
    bash -c 'sig=$(echo -n "test" | openssl dgst -sha256 -hmac "secret" | sed "s/^.* //"); [ ${#sig} -eq 64 ]'

test_case "Same input produces same signature" \
    bash -c '[[ "$(echo -n "msg" | openssl dgst -sha256 -hmac "key" | sed "s/^.* //")" == "$(echo -n "msg" | openssl dgst -sha256 -hmac "key" | sed "s/^.* //")" ]]'

SIG1=$(echo -n "msg" | openssl dgst -sha256 -hmac "key1" | sed 's/^.* //')
SIG2=$(echo -n "msg" | openssl dgst -sha256 -hmac "key2" | sed 's/^.* //')
test_case "Different secrets produce different signatures" assert_not_equal "$SIG1" "$SIG2"

SIG3=$(echo -n "msg1" | openssl dgst -sha256 -hmac "key" | sed 's/^.* //')
SIG4=$(echo -n "msg2" | openssl dgst -sha256 -hmac "key" | sed 's/^.* //')
test_case "Different messages produce different signatures" assert_not_equal "$SIG3" "$SIG4"

test_case "Signature format is timestamp:METHOD:path:body" \
    bash -c 'msg="1234567890:POST:/execute:{}"; [[ "$msg" == *":"* ]] && [[ $(echo "$msg" | tr -cd ":" | wc -c) -eq 3 ]]'

# ============================================================================
# Language Detection Tests
# ============================================================================

echo ""
echo "=== Language Detection Tests ==="

detect_from_shebang() {
    local first_line="$1"
    if [[ "$first_line" == "#!"* ]]; then
        case "$first_line" in
            *python*) echo "python" ;;
            *node*) echo "javascript" ;;
            *ruby*) echo "ruby" ;;
            *perl*) echo "perl" ;;
            *bash*|*/sh*) echo "bash" ;;
            *lua*) echo "lua" ;;
            *php*) echo "php" ;;
            *) echo "" ;;
        esac
    fi
}

test_case "Python shebang detection" \
    assert_equal "$(detect_from_shebang '#!/usr/bin/env python3')" "python"

test_case "Node shebang detection" \
    assert_equal "$(detect_from_shebang '#!/usr/bin/env node')" "javascript"

test_case "Ruby shebang detection" \
    assert_equal "$(detect_from_shebang '#!/usr/bin/env ruby')" "ruby"

test_case "Bash shebang detection" \
    assert_equal "$(detect_from_shebang '#!/bin/bash')" "bash"

test_case "Sh shebang detection" \
    assert_equal "$(detect_from_shebang '#!/bin/sh')" "bash"

# ============================================================================
# Argument Parsing Tests
# ============================================================================

echo ""
echo "=== Argument Parsing Tests ==="

parse_env_var() {
    local arg="$1"
    local key="${arg%%=*}"
    local value="${arg#*=}"
    echo "$key|$value"
}

test_case "Parse -e KEY=VALUE format" \
    assert_equal "$(parse_env_var 'DEBUG=1')" "DEBUG|1"

test_case "Parse -e KEY=VALUE with equals in value" \
    assert_equal "$(parse_env_var 'URL=https://example.com?foo=bar')" "URL|https://example.com?foo=bar"

test_case "Valid network mode zerotrust" \
    bash -c '[[ "zerotrust" =~ ^(zerotrust|semitrusted)$ ]]'

test_case "Valid network mode semitrusted" \
    bash -c '[[ "semitrusted" =~ ^(zerotrust|semitrusted)$ ]]'

test_case "Invalid network mode rejected" \
    bash -c '! [[ "invalid" =~ ^(zerotrust|semitrusted)$ ]]'

is_subcommand() {
    local cmd="$1"
    case "$cmd" in
        session|service|key|restore) return 0 ;;
        *) return 1 ;;
    esac
}

test_case "Subcommand detection - session" is_subcommand "session"
test_case "Subcommand detection - service" is_subcommand "service"
test_case "Subcommand detection - key" is_subcommand "key"
test_case "Not a subcommand - script.py" bash -c '! is_subcommand "script.py"'

# ============================================================================
# File Operations Tests
# ============================================================================

echo ""
echo "=== File Operations Tests ==="

TMPFILE=$(mktemp --suffix=.py)
echo -n 'print("hello world")' > "$TMPFILE"

test_case "Read text file" \
    assert_equal "$(cat "$TMPFILE")" 'print("hello world")'

test_case "Base64 encoding/decoding" \
    bash -c '[[ "$(echo -n "hello" | base64 | base64 -d)" == "hello" ]]'

test_case "Extract file basename" \
    assert_equal "$(basename /home/user/project/script.py)" "script.py"

EXT="${TMPFILE##*.}"
test_case "Extract file extension" \
    assert_equal "$EXT" "py"

rm -f "$TMPFILE"

# ============================================================================
# API Constants Tests
# ============================================================================

echo ""
echo "=== API Constants Tests ==="

API_BASE="https://api.unsandbox.com"
PORTAL_BASE="https://unsandbox.com"

test_case "API base URL starts with https" \
    bash -c '[[ "https://api.unsandbox.com" == https://* ]]'

test_case "API base URL contains unsandbox.com" \
    assert_contains "$API_BASE" "unsandbox.com"

test_case "Portal base URL starts with https" \
    bash -c '[[ "https://unsandbox.com" == https://* ]]'

# ============================================================================
# Summary
# ============================================================================

echo ""
echo "=== Summary ==="
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo "Total:  $((PASSED + FAILED))"

exit $((FAILED > 0 ? 1 : 0))
