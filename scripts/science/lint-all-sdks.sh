#!/bin/bash
# Lint all SDKs using local syntax checkers
# Validates code compiles/parses without runtime execution

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
CLIENTS_DIR="$REPO_ROOT/clients"

mkdir -p lint-results

echo "Linting SDKs..."
PASSED=0
FAILED=0
DETAILS=""

lint_sdk() {
    local name="$1"
    local cmd="$2"
    local file="$3"

    if [ ! -f "$file" ]; then
        echo "  [SKIP] $name - file not found: $file"
        return 0
    fi

    echo "  Linting $name..."
    if OUTPUT=$(eval "$cmd" 2>&1); then
        echo "  [PASS] $name"
        PASSED=$((PASSED + 1))
        DETAILS="$DETAILS\n    <testcase name=\"$name\" classname=\"lint\" />"
    else
        echo "  [FAIL] $name"
        echo "    $OUTPUT" | head -5
        FAILED=$((FAILED + 1))
        DETAILS="$DETAILS\n    <testcase name=\"$name\" classname=\"lint\"><failure message=\"Lint failed\">$OUTPUT</failure></testcase>"
    fi
}

# Python SDKs
echo "Python SDKs:"
lint_sdk "python-sync" "python3 -m py_compile '$CLIENTS_DIR/python/sync/src/un.py'" "$CLIENTS_DIR/python/sync/src/un.py"
lint_sdk "python-async" "python3 -m py_compile '$CLIENTS_DIR/python/async/src/un_async.py'" "$CLIENTS_DIR/python/async/src/un_async.py"

# JavaScript/TypeScript SDKs
echo "JavaScript SDKs:"
if command -v node &>/dev/null; then
    lint_sdk "javascript-sync" "node --check '$CLIENTS_DIR/javascript/sync/src/un.js'" "$CLIENTS_DIR/javascript/sync/src/un.js"
    lint_sdk "javascript-async" "node --check '$CLIENTS_DIR/javascript/async/src/un_async.js'" "$CLIENTS_DIR/javascript/async/src/un_async.js"
else
    echo "  [SKIP] Node.js not installed"
fi

# Ruby SDKs
echo "Ruby SDKs:"
if command -v ruby &>/dev/null; then
    lint_sdk "ruby-sync" "ruby -c '$CLIENTS_DIR/ruby/sync/src/un.rb'" "$CLIENTS_DIR/ruby/sync/src/un.rb"
    lint_sdk "ruby-async" "ruby -c '$CLIENTS_DIR/ruby/async/src/un_async.rb'" "$CLIENTS_DIR/ruby/async/src/un_async.rb"
else
    echo "  [SKIP] Ruby not installed"
fi

# Go SDKs
echo "Go SDKs:"
if command -v go &>/dev/null; then
    lint_sdk "go-sync" "go build -o /dev/null '$CLIENTS_DIR/go/sync/src/un.go'" "$CLIENTS_DIR/go/sync/src/un.go"
else
    echo "  [SKIP] Go not installed"
fi

# Rust SDKs
echo "Rust SDKs:"
if command -v rustc &>/dev/null && [ -f "$CLIENTS_DIR/rust/sync/Cargo.toml" ]; then
    lint_sdk "rust-sync" "cd '$CLIENTS_DIR/rust/sync' && cargo check --quiet" "$CLIENTS_DIR/rust/sync/src/lib.rs"
else
    echo "  [SKIP] Rust not installed or Cargo.toml missing"
fi

# PHP SDKs
echo "PHP SDKs:"
if command -v php &>/dev/null; then
    lint_sdk "php-sync" "php -l '$CLIENTS_DIR/php/sync/src/un.php'" "$CLIENTS_DIR/php/sync/src/un.php"
else
    echo "  [SKIP] PHP not installed"
fi

# Perl SDKs
echo "Perl SDKs:"
if command -v perl &>/dev/null; then
    # perl -c loads modules which may not be installed, so check for missing module errors
    # Capture output and status separately to avoid set -e triggering
    set +e
    PERL_OUTPUT=$(perl -c "$CLIENTS_DIR/perl/sync/src/un.pl" 2>&1)
    PERL_EXIT=$?
    set -e
    if [[ $PERL_EXIT -eq 0 ]]; then
        echo "  [PASS] perl-sync"
        PASSED=$((PASSED + 1))
        DETAILS="$DETAILS\n    <testcase name=\"perl-sync\" classname=\"lint\" />"
    elif echo "$PERL_OUTPUT" | grep -q "Can't locate"; then
        echo "  [SKIP] perl-sync - missing Perl modules (LWP::UserAgent, etc.)"
    else
        echo "  [FAIL] perl-sync"
        echo "    $PERL_OUTPUT" | head -3
        FAILED=$((FAILED + 1))
        DETAILS="$DETAILS\n    <testcase name=\"perl-sync\" classname=\"lint\"><failure message=\"Lint failed\">$PERL_OUTPUT</failure></testcase>"
    fi
else
    echo "  [SKIP] Perl not installed"
fi

# Lua SDKs
echo "Lua SDKs:"
if command -v luac &>/dev/null; then
    lint_sdk "lua-sync" "luac -p '$CLIENTS_DIR/lua/sync/src/un.lua'" "$CLIENTS_DIR/lua/sync/src/un.lua"
elif command -v luac5.4 &>/dev/null; then
    lint_sdk "lua-sync" "luac5.4 -p '$CLIENTS_DIR/lua/sync/src/un.lua'" "$CLIENTS_DIR/lua/sync/src/un.lua"
else
    echo "  [SKIP] Lua not installed"
fi

# Bash SDKs
echo "Bash SDKs:"
lint_sdk "bash-sync" "bash -n '$CLIENTS_DIR/bash/sync/src/un.sh'" "$CLIENTS_DIR/bash/sync/src/un.sh"

# C SDK (compile check)
echo "C SDK:"
if command -v gcc &>/dev/null && [ -f "$CLIENTS_DIR/c/src/un.c" ]; then
    lint_sdk "c-cli" "gcc -fsyntax-only -DUNSANDBOX_CLI '$CLIENTS_DIR/c/src/un.c' 2>&1 | grep -v 'warning:' || true" "$CLIENTS_DIR/c/src/un.c"
else
    echo "  [SKIP] GCC not installed"
fi

# Generate JUnit XML report
cat > lint-results.xml << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="SDK Linting" tests="$((PASSED + FAILED))" failures="$FAILED">
$(echo -e "$DETAILS")
  </testsuite>
</testsuites>
EOF

mkdir -p lint-results
cp lint-results.xml lint-results/

echo ""
echo "========================================"
echo "Linting complete: $PASSED passed, $FAILED failed"
echo "========================================"

# Exit with failure if any lint failed
if [ "$FAILED" -gt 0 ]; then
    exit 1
fi
exit 0
