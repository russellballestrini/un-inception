#!/bin/bash
# Detect which SDKs changed in this commit/PR
# Output: JSON array of changed languages

set -e

# Manual triggers for full matrix:
# 1. CI variable: TEST_ALL_SDKS=true
# 2. Commit message contains [test-all]
# 3. Tag push (releases test everything)

if [ "$TEST_ALL_SDKS" = "true" ]; then
    echo '{"changed_langs": ["all"], "reason": "TEST_ALL_SDKS variable set", "test_all": true}'
    exit 0
fi

COMMIT_MSG=$(git log -1 --pretty=%B 2>/dev/null || echo "")
if echo "$COMMIT_MSG" | grep -qi '\[test-all\]'; then
    echo '{"changed_langs": ["all"], "reason": "Commit message contains [test-all]", "test_all": true}'
    exit 0
fi

if [ -n "$CI_COMMIT_TAG" ]; then
    echo '{"changed_langs": ["all"], "reason": "Tag release - testing all SDKs", "test_all": true}'
    exit 0
fi

# Get the base branch for comparison
if [ -n "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" ]; then
    # MR context
    BASE="origin/$CI_MERGE_REQUEST_TARGET_BRANCH_NAME"
elif [ "$CI_COMMIT_BRANCH" = "main" ]; then
    # Push to main - compare with previous commit
    BASE="HEAD~1"
else
    # Fallback
    BASE="origin/main"
fi

# Get all changed files in this commit
CHANGED_FILES=$(git diff --name-only "$BASE...HEAD" 2>/dev/null || echo "")

# Extract unique languages from TWO sources:
# 1. Root-level files (un.py, un.go, un.c, etc.)
# 2. Client directory files (clients/python/*, clients/go/*, clients/c/*, etc.)

CHANGED_LANGS=$(
  {
    # Root-level implementations (un.py, un.c, etc.)
    echo "$CHANGED_FILES" | grep -E '^un\.' | sed 's/un\.\([^.]*\).*/\1/'
    # Client directory implementations (clients/python/, clients/c/, etc.)
    echo "$CHANGED_FILES" | grep -E '^clients/([^/]+)/' | sed 's|^clients/\([^/]*\)/.*|\1|'
  } | sort -u || echo ""
)

# Map file extensions to language names (for root-level un.* files)
declare -A LANG_MAP=(
    [py]="python"
    [js]="javascript"
    [ts]="typescript"
    [go]="go"
    [rb]="ruby"
    [php]="php"
    [pl]="perl"
    [lua]="lua"
    [sh]="bash"
    [rs]="rust"
    [java]="java"
    [cs]="csharp"
    [cpp]="cpp"
    [c]="c"
    [hs]="haskell"
    [kt]="kotlin"
    [ex]="elixir"
    [erl]="erlang"
    [cr]="crystal"
    [dart]="dart"
    [nim]="nim"
    [jl]="julia"
    [r]="r"
    [groovy]="groovy"
    [clj]="clojure"
    [fs]="fsharp"
    [ml]="ocaml"
    [m]="objc"
    [d]="d"
    [v]="vlang"
    [zig]="zig"
    [f90]="fortran"
    [cob]="cobol"
    [scm]="scheme"
    [lisp]="lisp"
    [tcl]="tcl"
    [awk]="awk"
    [pro]="prolog"
    [forth]="forth"
    [ps1]="powershell"
    [raku]="raku"
)

# Map directory names from clients/ to language names (clients/ already has language names)
declare -A DIR_MAP=(
    [python]="python"
    [javascript]="javascript"
    [typescript]="typescript"
    [go]="go"
    [ruby]="ruby"
    [php]="php"
    [perl]="perl"
    [lua]="lua"
    [bash]="bash"
    [rust]="rust"
    [java]="java"
    [csharp]="csharp"
    [cpp]="cpp"
    [c]="c"
    [haskell]="haskell"
    [kotlin]="kotlin"
    [elixir]="elixir"
    [erlang]="erlang"
    [crystal]="crystal"
    [dart]="dart"
    [nim]="nim"
    [julia]="julia"
    [r]="r"
    [groovy]="groovy"
    [clojure]="clojure"
    [fsharp]="fsharp"
    [ocaml]="ocaml"
    [objc]="objc"
    [d]="d"
    [vlang]="vlang"
    [zig]="zig"
    [fortran]="fortran"
    [cobol]="cobol"
    [scheme]="scheme"
    [lisp]="lisp"
    [tcl]="tcl"
    [awk]="awk"
    [prolog]="prolog"
    [forth]="forth"
    [powershell]="powershell"
    [raku]="raku"
)

# Also check for changes in test files, scripts, or core infra
if echo "$CHANGED_FILES" | grep -qE '^(tests/|scripts/|\.gitlab-ci\.yml|clients/\{.*\}/)'; then
    # If tests, scripts, or multi-language client templates changed, test ALL SDKs
    echo '{"changed_langs": ["all"], "reason": "Core infrastructure changed", "test_all": true}'
    exit 0
fi

# Convert file extensions and directory names to language names
LANGS_JSON="["
FIRST=true
for ITEM in $CHANGED_LANGS; do
    # Try directory map first (for clients/python/, etc.)
    LANG="${DIR_MAP[$ITEM]}"
    # Fallback to extension map (for un.py, etc.)
    LANG="${LANG:-${LANG_MAP[$ITEM]}}"
    # Fallback to item as-is if not in any map
    LANG="${LANG:-$ITEM}"

    if [ "$FIRST" = true ]; then
        LANGS_JSON="$LANGS_JSON\"$LANG\""
        FIRST=false
    else
        LANGS_JSON="$LANGS_JSON,\"$LANG\""
    fi
done
LANGS_JSON="$LANGS_JSON]"

# If no changes detected, test nothing (skip tests)
if [ "$LANGS_JSON" = "[]" ]; then
    echo '{"changed_langs": [], "reason": "No SDK files changed", "test_all": false}'
    exit 0
fi

echo "{\"changed_langs\": $LANGS_JSON, \"test_all\": false}"
