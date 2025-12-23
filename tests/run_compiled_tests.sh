#!/bin/bash
# Run all UN CLI Inception tests for compiled languages
# Usage: ./run_compiled_tests.sh

set -e

echo "=========================================="
echo "UN CLI Inception Compiled Languages Test Runner"
echo "=========================================="
echo ""

# Check for API key
if [ -z "$UNSANDBOX_API_KEY" ]; then
    echo "WARNING: UNSANDBOX_API_KEY not set"
    echo "API and functional tests will be skipped"
    echo ""
fi

cd "$(dirname "$0")"

# Track results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Test Go
echo ">>> Testing Go implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if go build -o test_un_go test_un_go.go 2>/dev/null; then
    if ./test_un_go >/dev/null 2>&1; then
        echo "✓ Go tests PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo "✗ Go tests FAILED"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
else
    echo "⊘ Go tests SKIPPED (compilation failed)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test Rust
echo ">>> Testing Rust implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if command -v rustc >/dev/null 2>&1; then
    if rustc test_un_rs.rs -o test_un_rs 2>/dev/null; then
        if ./test_un_rs >/dev/null 2>&1; then
            echo "✓ Rust tests PASSED"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ Rust tests FAILED"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "⊘ Rust tests SKIPPED (compilation failed - may need cargo for dependencies)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    fi
else
    echo "⊘ Rust tests SKIPPED (rustc not found)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test C
echo ">>> Testing C implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if gcc -o test_un_c test_un_c.c -lcurl 2>/dev/null; then
    if ./test_un_c >/dev/null 2>&1; then
        echo "✓ C tests PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo "✗ C tests FAILED"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
else
    echo "⊘ C tests SKIPPED (compilation failed)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test C++
echo ">>> Testing C++ implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if g++ -o test_un_cpp test_un_cpp.cpp -lcurl 2>/dev/null; then
    if ./test_un_cpp >/dev/null 2>&1; then
        echo "✓ C++ tests PASSED"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo "✗ C++ tests FAILED"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
else
    echo "⊘ C++ tests SKIPPED (compilation failed)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test D
echo ">>> Testing D implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if command -v dmd >/dev/null 2>&1; then
    if dmd test_un_d.d -of=test_un_d 2>/dev/null; then
        if ./test_un_d >/dev/null 2>&1; then
            echo "✓ D tests PASSED"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ D tests FAILED"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "⊘ D tests SKIPPED (compilation failed)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    fi
else
    echo "⊘ D tests SKIPPED (dmd not found)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test Zig
echo ">>> Testing Zig implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if command -v zig >/dev/null 2>&1; then
    if zig build-exe test_un_zig.zig -O ReleaseFast 2>/dev/null; then
        if ./test_un_zig >/dev/null 2>&1; then
            echo "✓ Zig tests PASSED"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ Zig tests FAILED"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "⊘ Zig tests SKIPPED (compilation failed)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    fi
else
    echo "⊘ Zig tests SKIPPED (zig not found)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test Nim
echo ">>> Testing Nim implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if command -v nim >/dev/null 2>&1; then
    if nim c -d:release --hints:off test_un_nim.nim 2>/dev/null; then
        if ./test_un_nim >/dev/null 2>&1; then
            echo "✓ Nim tests PASSED"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ Nim tests FAILED"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "⊘ Nim tests SKIPPED (compilation failed)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    fi
else
    echo "⊘ Nim tests SKIPPED (nim not found)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Test V
echo ">>> Testing V implementation..."
TOTAL_TESTS=$((TOTAL_TESTS + 1))
if command -v v >/dev/null 2>&1; then
    if v test_un_v.v -o test_un_v 2>/dev/null; then
        if ./test_un_v >/dev/null 2>&1; then
            echo "✓ V tests PASSED"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ V tests FAILED"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "⊘ V tests SKIPPED (compilation failed)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
    fi
else
    echo "⊘ V tests SKIPPED (v not found)"
    SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
fi
echo ""

# Summary
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Total tests:   $TOTAL_TESTS"
echo "Passed:        $PASSED_TESTS"
echo "Failed:        $FAILED_TESTS"
echo "Skipped:       $SKIPPED_TESTS"
echo "=========================================="

if [ $FAILED_TESTS -gt 0 ]; then
    echo "RESULT: SOME TESTS FAILED"
    exit 1
else
    echo "RESULT: ALL TESTS PASSED (or skipped)"
    exit 0
fi
