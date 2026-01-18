#!/bin/bash
# Set version across VERSION file and all client source files
# Usage: scripts/set-version.sh 4.2.0
# Or: make set-version VERSION=4.2.0

set -e

VERSION="${1:-}"

if [ -z "$VERSION" ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 4.2.0"
    exit 1
fi

# Validate version format (X.Y.Z)
if ! echo "$VERSION" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    echo "Error: Version must be in format X.Y.Z (e.g., 4.2.0)"
    exit 1
fi

echo "Setting version to $VERSION..."

# Track files updated
UPDATED=0

# 1. VERSION file
echo "$VERSION" > VERSION
echo "  ✓ VERSION"
UPDATED=$((UPDATED + 1))

# 2. C SDK - unsandbox_version() return value
if [ -f "clients/c/src/un.c" ]; then
    sed -i "s/return \"[0-9]\+\.[0-9]\+\.[0-9]\+\";/return \"$VERSION\";/" clients/c/src/un.c
    echo "  ✓ clients/c/src/un.c"
    UPDATED=$((UPDATED + 1))
fi

# 3. Python sync __init__.py
if [ -f "clients/python/sync/src/__init__.py" ]; then
    sed -i "s/__version__ = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/__version__ = \"$VERSION\"/" clients/python/sync/src/__init__.py
    echo "  ✓ clients/python/sync/src/__init__.py"
    UPDATED=$((UPDATED + 1))
fi

# 4. Python sync setup.py
if [ -f "clients/python/sync/setup.py" ]; then
    sed -i "s/version=\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/version=\"$VERSION\"/" clients/python/sync/setup.py
    echo "  ✓ clients/python/sync/setup.py"
    UPDATED=$((UPDATED + 1))
fi

# 5. Python async setup.py
if [ -f "clients/python/async/setup.py" ]; then
    sed -i "s/version=\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/version=\"$VERSION\"/" clients/python/async/setup.py
    echo "  ✓ clients/python/async/setup.py"
    UPDATED=$((UPDATED + 1))
fi

# 6. Lua SDK
if [ -f "clients/lua/sync/src/un.lua" ]; then
    sed -i "s/Un\.VERSION = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/Un.VERSION = \"$VERSION\"/" clients/lua/sync/src/un.lua
    echo "  ✓ clients/lua/sync/src/un.lua"
    UPDATED=$((UPDATED + 1))
fi

# 7. Perl SDK
if [ -f "clients/perl/sync/src/un.pl" ]; then
    sed -i "s/our \$VERSION = \"[0-9]\+\.[0-9]\+\.[0-9]\+\";/our \$VERSION = \"$VERSION\";/" clients/perl/sync/src/un.pl
    echo "  ✓ clients/perl/sync/src/un.pl"
    UPDATED=$((UPDATED + 1))
fi

# 8. Groovy SDK (javadoc @version tag)
if [ -f "clients/groovy/sync/src/un.groovy" ]; then
    sed -i "s/@version [0-9]\+\.[0-9]\+\.[0-9]\+/@version $VERSION/" clients/groovy/sync/src/un.groovy
    echo "  ✓ clients/groovy/sync/src/un.groovy"
    UPDATED=$((UPDATED + 1))
fi

# 9. JavaScript package.json (if exists)
if [ -f "clients/javascript/sync/package.json" ]; then
    sed -i "s/\"version\": \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/\"version\": \"$VERSION\"/" clients/javascript/sync/package.json
    echo "  ✓ clients/javascript/sync/package.json"
    UPDATED=$((UPDATED + 1))
fi

if [ -f "clients/javascript/async/package.json" ]; then
    sed -i "s/\"version\": \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/\"version\": \"$VERSION\"/" clients/javascript/async/package.json
    echo "  ✓ clients/javascript/async/package.json"
    UPDATED=$((UPDATED + 1))
fi

# 10. TypeScript package.json (if exists)
if [ -f "clients/typescript/sync/package.json" ]; then
    sed -i "s/\"version\": \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/\"version\": \"$VERSION\"/" clients/typescript/sync/package.json
    echo "  ✓ clients/typescript/sync/package.json"
    UPDATED=$((UPDATED + 1))
fi

# 11. Rust Cargo.toml (if exists)
if [ -f "clients/rust/sync/Cargo.toml" ]; then
    sed -i "s/^version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/version = \"$VERSION\"/" clients/rust/sync/Cargo.toml
    echo "  ✓ clients/rust/sync/Cargo.toml"
    UPDATED=$((UPDATED + 1))
fi

if [ -f "clients/rust/async/Cargo.toml" ]; then
    sed -i "s/^version = \"[0-9]\+\.[0-9]\+\.[0-9]\+\"/version = \"$VERSION\"/" clients/rust/async/Cargo.toml
    echo "  ✓ clients/rust/async/Cargo.toml"
    UPDATED=$((UPDATED + 1))
fi

# 12. Go module (if has version constant)
for gofile in clients/go/sync/src/un.go clients/go/async/src/un_async.go; do
    if [ -f "$gofile" ]; then
        if grep -q 'Version.*=.*"[0-9]\+\.[0-9]\+\.[0-9]\+"' "$gofile"; then
            sed -i "s/Version.*=.*\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/Version = \"$VERSION\"/" "$gofile"
            echo "  ✓ $gofile"
            UPDATED=$((UPDATED + 1))
        fi
    fi
done

# 13. Ruby gemspec or version constant
if [ -f "clients/ruby/sync/src/un.rb" ]; then
    if grep -q 'VERSION.*=.*"[0-9]\+\.[0-9]\+\.[0-9]\+"' "clients/ruby/sync/src/un.rb"; then
        sed -i "s/VERSION.*=.*\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/VERSION = \"$VERSION\"/" clients/ruby/sync/src/un.rb
        echo "  ✓ clients/ruby/sync/src/un.rb"
        UPDATED=$((UPDATED + 1))
    fi
fi

# 14. Java/Kotlin version in source (if exists)
for javafile in clients/java/sync/src/Un.java clients/kotlin/sync/src/un.kt; do
    if [ -f "$javafile" ]; then
        if grep -q 'VERSION.*=.*"[0-9]\+\.[0-9]\+\.[0-9]\+"' "$javafile"; then
            sed -i "s/VERSION.*=.*\"[0-9]\+\.[0-9]\+\.[0-9]\+\"/VERSION = \"$VERSION\"/" "$javafile"
            echo "  ✓ $javafile"
            UPDATED=$((UPDATED + 1))
        fi
    fi
done

echo ""
echo "Done! Updated $UPDATED files to version $VERSION"
echo ""
echo "Verify with:"
echo "  grep -rn '$VERSION' VERSION clients/*/sync/src/* clients/*/async/src/* 2>/dev/null | grep -v node_modules | head -20"
