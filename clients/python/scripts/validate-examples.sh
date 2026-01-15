#!/bin/bash
##
# Validation script for Python SDK examples
#
# This script validates that all example files exist and have proper structure.
# It does NOT require credentials to run - it only checks syntax and structure.
#
# Usage:
#   bash scripts/validate-examples.sh
#
# With actual credentials to test execution:
#   UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... bash scripts/validate-examples.sh --run
##

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SYNC_EXAMPLES="$PROJECT_ROOT/sync/examples"
ASYNC_EXAMPLES="$PROJECT_ROOT/async/examples"
SYNC_SRC="$PROJECT_ROOT/sync/src"
ASYNC_SRC="$PROJECT_ROOT/async/src"

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
RUN_TESTS="${1:-}"

echo "=== Python SDK Examples Validation ==="
echo ""

##
# Check if file exists and is readable
##
check_file_exists() {
    local file="$1"
    local description="$2"
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [[ -f "$file" && -r "$file" ]]; then
        echo -e "${GREEN}✓${NC} $description: $file"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $file (not found or not readable)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

##
# Check if file contains expected string
##
check_file_contains() {
    local file="$1"
    local pattern="$2"
    local description="$3"
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if grep -q "$pattern" "$file"; then
        echo -e "${GREEN}✓${NC} $description: $file"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $file (pattern not found: $pattern)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

##
# Check Python syntax
##
check_syntax() {
    local file="$1"
    local description="$2"
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if python3 -m py_compile "$file" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} $description: $file"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $file (syntax error)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

##
# Try to run example (only if credentials provided)
##
run_example() {
    local file="$1"
    local description="$2"
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    if [[ -z "${UNSANDBOX_PUBLIC_KEY:-}" ]] || [[ -z "${UNSANDBOX_SECRET_KEY:-}" ]]; then
        echo -e "${YELLOW}⊘${NC} $description: $file (skipped - no credentials)"
        return 0
    fi

    # Set timeout to 30 seconds per example
    if timeout 30s python3 "$file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $description: $file"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        return 0
    else
        echo -e "${RED}✗${NC} $description: $file (execution failed)"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        return 1
    fi
}

echo "=== SDK Source Files ==="
check_file_exists "$SYNC_SRC/un.py" "Sync SDK module"
check_file_exists "$ASYNC_SRC/un_async.py" "Async SDK module"
echo ""

echo "=== Synchronous Examples ==="
echo "--- Basic Examples ---"
check_file_exists "$SYNC_EXAMPLES/hello_world.py" "Raw code snippet"
check_file_exists "$SYNC_EXAMPLES/hello_world_client.py" "SDK client wrapper"
check_file_contains "$SYNC_EXAMPLES/hello_world_client.py" "execute_code" "Contains execute_code"
check_syntax "$SYNC_EXAMPLES/hello_world_client.py" "Syntax check"

echo "--- Computational Examples ---"
check_file_exists "$SYNC_EXAMPLES/fibonacci.py" "Raw code snippet"
check_file_exists "$SYNC_EXAMPLES/fibonacci_client.py" "SDK client wrapper"
check_file_contains "$SYNC_EXAMPLES/fibonacci_client.py" "def fib" "Contains fib function"
check_syntax "$SYNC_EXAMPLES/fibonacci_client.py" "Syntax check"

echo "--- Data Processing Examples ---"
check_file_exists "$SYNC_EXAMPLES/http_request.py" "HTTP request example"
check_file_contains "$SYNC_EXAMPLES/http_request.py" "requests" "Uses requests library"
check_syntax "$SYNC_EXAMPLES/http_request.py" "Syntax check"

check_file_exists "$SYNC_EXAMPLES/json_processing.py" "JSON processing example"
check_file_contains "$SYNC_EXAMPLES/json_processing.py" "json.loads" "Parses JSON"
check_syntax "$SYNC_EXAMPLES/json_processing.py" "Syntax check"

check_file_exists "$SYNC_EXAMPLES/file_operations.py" "File operations example"
check_file_contains "$SYNC_EXAMPLES/file_operations.py" "open(" "Uses file I/O"
check_syntax "$SYNC_EXAMPLES/file_operations.py" "Syntax check"

echo ""

echo "=== Asynchronous Examples ==="
echo "--- Basic Examples ---"
check_file_exists "$ASYNC_EXAMPLES/hello_world_async.py" "SDK client wrapper"
check_file_contains "$ASYNC_EXAMPLES/hello_world_async.py" "async def" "Contains async function"
check_syntax "$ASYNC_EXAMPLES/hello_world_async.py" "Syntax check"

echo "--- Concurrent Computation ---"
check_file_exists "$ASYNC_EXAMPLES/fibonacci_async.py" "Fibonacci async example"
check_file_contains "$ASYNC_EXAMPLES/fibonacci_async.py" "asyncio.gather" "Uses asyncio.gather"
check_syntax "$ASYNC_EXAMPLES/fibonacci_async.py" "Syntax check"

echo "--- Network Examples ---"
check_file_exists "$ASYNC_EXAMPLES/concurrent_requests.py" "Concurrent requests example"
check_file_contains "$ASYNC_EXAMPLES/concurrent_requests.py" "httpbin.org" "Uses HTTP endpoints"
check_syntax "$ASYNC_EXAMPLES/concurrent_requests.py" "Syntax check"

echo "--- Stream Processing ---"
check_file_exists "$ASYNC_EXAMPLES/stream_processing.py" "Stream processing example"
check_file_contains "$ASYNC_EXAMPLES/stream_processing.py" "stream_generator" "Uses generators"
check_syntax "$ASYNC_EXAMPLES/stream_processing.py" "Syntax check"

echo "--- Job Management ---"
check_file_exists "$ASYNC_EXAMPLES/async_job_polling.py" "Job polling example"
check_file_contains "$ASYNC_EXAMPLES/async_job_polling.py" "wait_for_job" "Handles job polling"
check_syntax "$ASYNC_EXAMPLES/async_job_polling.py" "Syntax check"

check_file_exists "$ASYNC_EXAMPLES/concurrent_execution.py" "Concurrent execution example"
check_file_contains "$ASYNC_EXAMPLES/concurrent_execution.py" "asyncio.gather" "Uses asyncio.gather"
check_syntax "$ASYNC_EXAMPLES/concurrent_execution.py" "Syntax check"

check_file_exists "$ASYNC_EXAMPLES/sync_blocking_usage.py" "Sync/blocking usage example"
check_file_contains "$ASYNC_EXAMPLES/sync_blocking_usage.py" "detect_language" "Uses helper functions"
check_syntax "$ASYNC_EXAMPLES/sync_blocking_usage.py" "Syntax check"

echo ""

##
# Documentation checks
##
echo "=== Documentation ==="
check_file_exists "$PROJECT_ROOT/EXAMPLES.md" "Examples documentation"
check_file_contains "$PROJECT_ROOT/EXAMPLES.md" "Synchronous Examples" "Documents sync examples"
check_file_contains "$PROJECT_ROOT/EXAMPLES.md" "Asynchronous Examples" "Documents async examples"

echo ""

##
# Optional execution tests
##
if [[ "$RUN_TESTS" == "--run" ]]; then
    echo "=== Execution Tests (Credentials Required) ==="

    if [[ -z "${UNSANDBOX_PUBLIC_KEY:-}" ]] || [[ -z "${UNSANDBOX_SECRET_KEY:-}" ]]; then
        echo -e "${YELLOW}⚠${NC} Credentials not provided, skipping execution tests"
        echo "   Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY to enable"
    else
        echo "Running with credentials..."
        run_example "$SYNC_EXAMPLES/hello_world_client.py" "Sync hello world"
        run_example "$SYNC_EXAMPLES/fibonacci_client.py" "Sync fibonacci"
        run_example "$ASYNC_EXAMPLES/hello_world_async.py" "Async hello world"
    fi
    echo ""
fi

##
# Summary
##
echo "=== Summary ==="
TOTAL=$((PASSED_CHECKS + FAILED_CHECKS))
echo "Checks: $TOTAL"
echo -e "Passed: ${GREEN}$PASSED_CHECKS${NC}"
echo -e "Failed: ${RED}$FAILED_CHECKS${NC}"
echo ""

if [[ $FAILED_CHECKS -eq 0 ]]; then
    echo -e "${GREEN}✓ All validations passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some validations failed!${NC}"
    exit 1
fi
