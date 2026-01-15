#!/bin/bash
# Generate dynamic test matrix based on detected changes
# Reads changes.json, outputs test-matrix.yml with parallel jobs

set -e

# Read changes from detect-changes output
CHANGES=$(cat changes.json)
CHANGED_LANGS=$(echo "$CHANGES" | jq -r '.changed_langs[]' 2>/dev/null || echo "")
TEST_ALL=$(echo "$CHANGES" | jq -r '.test_all' 2>/dev/null || echo "false")

# If test_all is true or no changes detected, generate comprehensive matrix
if [ "$TEST_ALL" = "true" ]; then
    LANGS="python javascript typescript go ruby php perl lua bash rust java csharp cpp c haskell kotlin elixir erlang crystal dart nim julia r groovy clojure fsharp ocaml objc d vlang zig fortran cobol scheme lisp tcl awk prolog forth powershell raku"
elif [ -z "$CHANGED_LANGS" ]; then
    # No changes - don't generate any jobs
    echo "# No SDK changes detected"
    exit 0
else
    LANGS="$CHANGED_LANGS"
fi

# Start generating test-matrix.yml
cat > test-matrix.yml << 'EOF'
# Dynamically generated test matrix based on changed SDKs
test:
  stage: test
  image: alpine:latest
  parallel:
    matrix:
EOF

# Add each language as a parallel job
FIRST=true
for LANG in $LANGS; do
    if [ "$FIRST" = true ]; then
        echo "      - SDK_LANG: $LANG" >> test-matrix.yml
        FIRST=false
    else
        echo "      - SDK_LANG: $LANG" >> test-matrix.yml
    fi
done

# Complete the test job template
cat >> test-matrix.yml << 'EOF'
  script:
    - apk add --no-cache curl jq bash
    - export TEST_LANG=$SDK_LANG
    - |
      echo "Testing $TEST_LANG..."
      if [ ! -f "un.py" ]; then
        echo "ERROR: SDKs not found"
        exit 1
      fi
    # Call unsandbox to test the SDK
    - bash scripts/test-sdk.sh "$TEST_LANG"
  artifacts:
    reports:
      junit: "test-results-$SDK_LANG.xml"
    paths:
      - "test-results-$SDK_LANG/"
    expire_in: 30 days
  allow_failure: false
  retry: 1
EOF

cat test-matrix.yml
