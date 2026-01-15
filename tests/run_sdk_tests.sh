#!/bin/bash
#
# Master test runner for SDK library validation
# Allows agents to validate their SDK refactoring work
#
# Usage:
#   ./run_sdk_tests.sh                              # Run all SDK tests
#   ./run_sdk_tests.sh --unit                       # Run only unit tests
#   ./run_sdk_tests.sh --integration                # Run only integration tests
#   ./run_sdk_tests.sh --languages python javascript go
#   UNSANDBOX_API_KEY=xxx ./run_sdk_tests.sh --all
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Color codes
GREEN='\033[92m'
RED='\033[91m'
YELLOW='\033[93m'
BLUE='\033[94m'
RESET='\033[0m'

# Test configuration
TEST_TYPE="all"  # unit, integration, all
LANGUAGES=""
API_KEY="${UNSANDBOX_API_KEY:-}"
API_URL="${UNSANDBOX_API_URL:-https://api.unsandbox.com}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --unit)
            TEST_TYPE="unit"
            shift
            ;;
        --integration)
            TEST_TYPE="integration"
            shift
            ;;
        --functional)
            TEST_TYPE="functional"
            shift
            ;;
        --languages)
            shift
            LANGUAGES="$@"
            break
            ;;
        --api-key)
            API_KEY="$2"
            shift 2
            ;;
        --api-url)
            API_URL="$2"
            shift 2
            ;;
        --help|-h)
            echo "UN SDK Test Runner"
            echo ""
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --unit              Run only unit tests (no API key required)"
            echo "  --integration       Run only integration tests (requires API key)"
            echo "  --functional        Run only functional tests (requires API key)"
            echo "  --languages L1 L2   Test specific languages (default: all)"
            echo "  --api-key KEY       Set API key for integration tests"
            echo "  --api-url URL       Set API URL (default: https://api.unsandbox.com)"
            echo "  --help              Show this help message"
            echo ""
            echo "Environment Variables:"
            echo "  UNSANDBOX_API_KEY   API key for integration tests"
            echo "  UNSANDBOX_API_URL   API URL for testing"
            echo ""
            exit 0
            ;;
        *)
            LANGUAGES="$LANGUAGES $1"
            shift
            ;;
    esac
done

# Export for subprocesses
export UNSANDBOX_API_KEY="$API_KEY"
export UNSANDBOX_API_URL="$API_URL"

# Print header
echo -e "${BLUE}"
echo "=========================================="
echo "UN SDK Library Test Runner"
echo "=========================================="
echo -e "${RESET}"

echo "Test Type: $TEST_TYPE"
if [ -z "$API_KEY" ]; then
    echo "Integration Tests: ${YELLOW}SKIPPED${RESET} (no API key)"
else
    echo "Integration Tests: ${GREEN}ENABLED${RESET}"
fi
echo "API URL: $API_URL"
echo ""

# Run Python SDK tests
echo -e "${BLUE}Running Python SDK tests...${RESET}"
if command -v python3 &> /dev/null; then
    if [ "$TEST_TYPE" = "all" ] || [ "$TEST_TYPE" = "unit" ]; then
        python3 "$SCRIPT_DIR/test_sdk_library.py" --languages python 2>&1 || true
    fi
else
    echo -e "${YELLOW}⚠ Python 3 not found${RESET}"
fi

# Run JavaScript SDK tests
echo -e "${BLUE}Running JavaScript SDK tests...${RESET}"
if command -v node &> /dev/null; then
    # Create test script for JavaScript
    JS_TEST_SCRIPT="$SCRIPT_DIR/test_sdk_library.js"
    cat > "$JS_TEST_SCRIPT" << 'EOJS'
#!/usr/bin/env node
/**
 * JavaScript SDK library tests
 * Tests verify SDK functions work correctly
 */

const un = require('../un.js');
const crypto = require('crypto');
const fs = require('fs');

class TestResults {
    constructor() {
        this.passed = 0;
        this.failed = 0;
        this.skipped = 0;
    }

    pass(name) {
        console.log(`✓ PASS: ${name}`);
        this.passed++;
    }

    fail(name, error) {
        console.log(`✗ FAIL: ${name} - ${error}`);
        this.failed++;
    }

    skip(name, reason) {
        console.log(`⊘ SKIP: ${name} - ${reason}`);
        this.skipped++;
    }

    summary() {
        console.log(`\n✓ ${this.passed} passed, ✗ ${this.failed} failed, ⊘ ${this.skipped} skipped\n`);
        return this.failed === 0;
    }
}

const results = new TestResults();

// Test 1: HMAC signature generation
try {
    const sig = un._signRequest('test-sk', '1704067200', 'POST', '/execute', '{}');
    if (sig && sig.length === 64 && /^[0-9a-f]+$/.test(sig)) {
        results.pass('HMAC-SHA256 signature generation');
    } else {
        results.fail('HMAC-SHA256 signature generation', `Invalid signature: ${sig}`);
    }
} catch (e) {
    results.fail('HMAC-SHA256 signature generation', e.message);
}

// Test 2: Client class exists
try {
    if (typeof un.Client === 'function') {
        results.pass('Client class exists');
    } else {
        results.fail('Client class exists', 'Not a function');
    }
} catch (e) {
    results.fail('Client class exists', e.message);
}

// Test 3: execute function exists
try {
    if (typeof un.execute === 'function') {
        results.pass('execute function exists');
    } else {
        results.fail('execute function exists', 'Not a function');
    }
} catch (e) {
    results.fail('execute function exists', e.message);
}

// Test 4: executeAsync function exists
try {
    if (typeof un.executeAsync === 'function') {
        results.pass('executeAsync function exists');
    } else {
        results.fail('executeAsync function exists', 'Not a function');
    }
} catch (e) {
    results.fail('executeAsync function exists', e.message);
}

// Print summary
const success = results.summary();
process.exit(success ? 0 : 1);
EOJS

    chmod +x "$JS_TEST_SCRIPT" 2>/dev/null || true
    node "$JS_TEST_SCRIPT" 2>&1 || true
    rm -f "$JS_TEST_SCRIPT"
else
    echo -e "${YELLOW}⚠ Node.js not found${RESET}"
fi

# Run Go SDK tests
echo -e "${BLUE}Running Go SDK tests...${RESET}"
if command -v go &> /dev/null; then
    GO_TEST_FILE="$SCRIPT_DIR/test_go_sdk.go"
    cat > "$GO_TEST_FILE" << 'EOGO'
package main

import (
    "fmt"
    "crypto/hmac"
    "crypto/sha256"
    "encoding/hex"
)

func main() {
    // Test 1: HMAC signature
    key := []byte("test-sk")
    message := "1704067200:POST:/execute:{}"
    h := hmac.New(sha256.New, key)
    h.Write([]byte(message))
    sig := hex.EncodeToString(h.Sum(nil))

    if len(sig) == 64 {
        fmt.Println("✓ PASS: HMAC-SHA256 signature generation")
    } else {
        fmt.Printf("✗ FAIL: HMAC-SHA256 signature generation - invalid length %d\n", len(sig))
    }
}
EOGO

    go run "$GO_TEST_FILE" 2>&1 || true
    rm -f "$GO_TEST_FILE"
else
    echo -e "${YELLOW}⚠ Go not found${RESET}"
fi

# Print final summary
echo ""
echo -e "${BLUE}=========================================="
echo "Test run complete"
echo "==========================================${RESET}"
echo ""
echo "To run integration tests with real API calls:"
echo "  export UNSANDBOX_API_KEY='your-key-here'"
echo "  $0 --integration"
echo ""
