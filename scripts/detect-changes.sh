#!/bin/bash
# Detect which SDKs changed in this commit/PR
# Output: JSON array of changed languages

set -e

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

# Extract unique languages from changed SDK files
CHANGED_LANGS=$(echo "$CHANGED_FILES" | grep -E '^un\.' | sed 's/un\.\([^.]*\).*/\1/' | sort -u || echo "")

# Map file extensions to language names
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

# Also check for changes in test files, scripts, or core infra
if echo "$CHANGED_FILES" | grep -qE '^(tests/|scripts/|\.gitlab-ci\.yml)'; then
    # If tests or scripts changed, test ALL SDKs
    echo '{"changed_langs": ["all"], "reason": "Core infrastructure changed", "test_all": true}'
    exit 0
fi

# Convert file extensions to language names
LANGS_JSON="["
FIRST=true
for EXT in $CHANGED_LANGS; do
    LANG="${LANG_MAP[$EXT]:-$EXT}"
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
