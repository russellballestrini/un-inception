#!/bin/bash
# Comprehensive tests for un.cob (COBOL UN CLI Inception implementation)
# COBOL is challenging to test directly due to compilation requirements
# This shell wrapper provides test coverage
# Run with: bash test_un_cob.sh

# Color codes
GREEN='\033[32m'
RED='\033[31m'
BLUE='\033[34m'
RESET='\033[0m'

# Test counters
PASSED=0
FAILED=0

print_test() {
    local name="$1"
    local result="$2"

    if [ "$result" = "true" ]; then
        echo -e "${GREEN}✓ PASS${RESET}: $name"
        ((PASSED++))
    else
        echo -e "${RED}✗ FAIL${RESET}: $name"
        ((FAILED++))
    fi
}

echo ""
echo -e "${BLUE}========================================${RESET}"
echo -e "${BLUE}UN CLI Inception Tests - COBOL${RESET}"
echo -e "${BLUE}========================================${RESET}"
echo ""

# Test Suite 1: Extension Detection (using grep to verify COBOL source)
echo -e "${BLUE}Test Suite 1: Extension Detection${RESET}"

UN_COB="../un.cob"
if [ ! -f "$UN_COB" ]; then
    UN_COB="/home/fox/git/unsandbox.com/cli/inception/un.cob"
fi

# Check if un.cob has the extension mappings
if [ -f "$UN_COB" ]; then
    grep -q 'WHEN ".jl".*MOVE "julia"' "$UN_COB" && print_test "Detect .jl as julia" "true" || print_test "Detect .jl as julia" "false"
    grep -q 'WHEN ".r".*MOVE "r"' "$UN_COB" && print_test "Detect .r as r" "true" || print_test "Detect .r as r" "false"
    grep -q 'WHEN ".cr".*MOVE "crystal"' "$UN_COB" && print_test "Detect .cr as crystal" "true" || print_test "Detect .cr as crystal" "false"
    grep -q 'WHEN ".f90".*MOVE "fortran"' "$UN_COB" && print_test "Detect .f90 as fortran" "true" || print_test "Detect .f90 as fortran" "false"
    grep -q 'WHEN ".cob".*MOVE "cobol"' "$UN_COB" && print_test "Detect .cob as cobol" "true" || print_test "Detect .cob as cobol" "false"
    grep -q 'WHEN ".pro".*MOVE "prolog"' "$UN_COB" && print_test "Detect .pro as prolog" "true" || print_test "Detect .pro as prolog" "false"
    grep -q 'WHEN ".forth".*MOVE "forth"' "$UN_COB" && print_test "Detect .forth as forth" "true" || print_test "Detect .forth as forth" "false"
    grep -q 'WHEN ".4th".*MOVE "forth"' "$UN_COB" && print_test "Detect .4th as forth" "true" || print_test "Detect .4th as forth" "false"
    grep -q 'WHEN ".py".*MOVE "python"' "$UN_COB" && print_test "Detect .py as python" "true" || print_test "Detect .py as python" "false"
    grep -q 'WHEN ".rs".*MOVE "rust"' "$UN_COB" && print_test "Detect .rs as rust" "true" || print_test "Detect .rs as rust" "false"
    grep -q 'WHEN OTHER.*MOVE "unknown"' "$UN_COB" && print_test "Detect unknown extension" "true" || print_test "Detect unknown extension" "false"
else
    echo -e "${RED}ERROR: un.cob not found${RESET}"
    exit 1
fi

# Test Suite 2: API Integration
echo ""
echo -e "${BLUE}Test Suite 2: API Integration${RESET}"
if [ -z "$UNSANDBOX_API_KEY" ]; then
    echo -e "${BLUE}ℹ SKIP${RESET}: API integration test (UNSANDBOX_API_KEY not set)"
else
    # Test if COBOL can be compiled
    if command -v cobc &> /dev/null; then
        # Try to compile un.cob
        if cobc -x -o /tmp/test_un_cob "$UN_COB" 2>/dev/null; then
            print_test "COBOL compilation successful" "true"
            rm -f /tmp/test_un_cob
        else
            print_test "COBOL compilation successful" "false"
        fi
    else
        echo -e "${BLUE}ℹ SKIP${RESET}: Compilation test (cobc not available)"
    fi
fi

# Test Suite 3: End-to-End Functional Test
echo ""
echo -e "${BLUE}Test Suite 3: End-to-End Functional Test${RESET}"
if [ -z "$UNSANDBOX_API_KEY" ]; then
    echo -e "${BLUE}ℹ SKIP${RESET}: E2E test (UNSANDBOX_API_KEY not set)"
else
    FIB_FILE="../../test/fib.cob"
    if [ ! -f "$FIB_FILE" ]; then
        FIB_FILE="/home/fox/git/unsandbox.com/cli/test/fib.cob"
    fi

    if [ -f "$FIB_FILE" ]; then
        if command -v cobc &> /dev/null; then
            # Compile and run
            if cobc -x -o /tmp/test_un_cob "$UN_COB" 2>/dev/null; then
                OUTPUT=$(/tmp/test_un_cob "$FIB_FILE" 2>&1)

                echo "$OUTPUT" | grep -q "fib(10) = 55" && print_test "E2E: fib.cob produces fib(10) = 55" "true" || print_test "E2E: fib.cob produces fib(10) = 55" "false"
                echo "$OUTPUT" | grep -q "fib(5) = 5" && print_test "E2E: fib.cob produces fib(5) = 5" "true" || print_test "E2E: fib.cob produces fib(5) = 5" "false"
                echo "$OUTPUT" | grep -q "fib(0) = 0" && print_test "E2E: fib.cob produces fib(0) = 0" "true" || print_test "E2E: fib.cob produces fib(0) = 0" "false"

                rm -f /tmp/test_un_cob
            else
                echo -e "${BLUE}ℹ SKIP${RESET}: E2E test (compilation failed)"
            fi
        else
            echo -e "${BLUE}ℹ SKIP${RESET}: E2E test (cobc not available)"
        fi
    else
        echo -e "${BLUE}ℹ SKIP${RESET}: E2E test (fib.cob not found)"
    fi
fi

# Test Suite 4: Error Handling
echo ""
echo -e "${BLUE}Test Suite 4: Error Handling${RESET}"
grep -q 'WHEN OTHER.*MOVE "unknown"' "$UN_COB" && print_test "Unknown extension handling" "true" || print_test "Unknown extension handling" "false"

# Verify the DETECT-LANGUAGE procedure exists
grep -q 'DETECT-LANGUAGE' "$UN_COB" && print_test "Extension detection procedure exists" "true" || print_test "Extension detection procedure exists" "false"

# Print summary
TOTAL=$((PASSED + FAILED))
echo ""
echo -e "${BLUE}========================================${RESET}"
echo -e "${BLUE}Test Summary${RESET}"
echo -e "${BLUE}========================================${RESET}"
echo -e "${GREEN}Passed: $PASSED${RESET}"
echo -e "${RED}Failed: $FAILED${RESET}"
echo -e "${BLUE}Total:  $TOTAL${RESET}"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo -e "${RED}TESTS FAILED${RESET}"
    exit 1
else
    echo ""
    echo -e "${GREEN}ALL TESTS PASSED${RESET}"
    exit 0
fi
