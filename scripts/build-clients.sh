#!/bin/bash
# Build SDKs - primarily the C CLI for inception testing

set -e

mkdir -p build

echo "Building C CLI for inception testing..."

# Build the C CLI binary
if [ -d "clients/c" ] && [ -f "clients/c/Makefile" ]; then
    make -C clients/c cli
    cp clients/c/un build/un
    echo "✓ Built C CLI: build/un"
else
    echo "ERROR: clients/c not found"
    exit 1
fi

# Verify the binary works
if [ -x "build/un" ]; then
    echo "✓ CLI binary is executable"
    build/un --version || echo "(version check skipped - may need API keys)"
else
    echo "ERROR: build/un not executable"
    exit 1
fi

echo ""
echo "Build complete. The 'un' binary can now run inception tests."
ls -lh build/
