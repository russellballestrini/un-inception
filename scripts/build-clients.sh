#!/bin/bash
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

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
