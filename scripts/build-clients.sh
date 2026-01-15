#!/bin/bash
# Build SDKs that changed (or all for tag releases)

set -e

mkdir -p build

# For now, just copy the un.* files to build/
# Real build system would compile C, Go, Rust, etc.
echo "Building SDKs..."

# Interpreted languages just copy
for LANG in python javascript typescript php perl lua bash; do
    if [ -f "un.$LANG" ] || [ -f "un.${LANG:0:2}" ]; then
        cp un.* build/ 2>/dev/null || true
        echo "✓ Copied $LANG SDK"
    fi
done

# Compiled languages would build here
# go: CGO_ENABLED=0 go build -o build/un_go un.go
# rust: cargo build --release --quiet
# c: gcc -O3 un.c -o build/un_c
# etc.

echo "Build complete"
ls -lh build/
