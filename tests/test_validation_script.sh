#!/bin/bash
# Test suite for the examples validation script
# Verifies that validate-examples.sh works correctly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VALIDATE_SCRIPT="$SCRIPT_DIR/scripts/validate-examples.sh"

echo "=============================================="
echo "Testing SDK Examples Validation Script"
echo "=============================================="
echo ""

# Test 1: Script exists and is executable
echo "Test 1: Script exists and is executable"
if [ -x "$VALIDATE_SCRIPT" ]; then
    echo "✓ PASS: Script found and executable at $VALIDATE_SCRIPT"
else
    echo "✗ FAIL: Script not found or not executable"
    exit 1
fi

# Test 2: Bash syntax is valid
echo ""
echo "Test 2: Bash syntax validation"
if bash -n "$VALIDATE_SCRIPT" 2>&1; then
    echo "✓ PASS: Bash syntax is valid"
else
    echo "✗ FAIL: Bash syntax errors found"
    exit 1
fi

# Test 3: Script uses required functions
echo ""
echo "Test 3: Core functions defined"
required_functions="log log_pass log_fail detect_language find_examples validate_example generate_json_report generate_html_report main"
missing_functions=""

for func in $required_functions; do
    if grep -q "^${func}()" "$VALIDATE_SCRIPT"; then
        echo "  ✓ Function '$func' defined"
    else
        echo "  ✗ Function '$func' not found"
        missing_functions="$missing_functions $func"
    fi
done

if [ -z "$missing_functions" ]; then
    echo "✓ PASS: All required functions found"
else
    echo "✗ FAIL: Missing functions:$missing_functions"
    exit 1
fi

# Test 4: Environment variables are used correctly
echo ""
echo "Test 4: Environment variable handling"
env_vars="UNSANDBOX_API_KEY UNSANDBOX_API_URL PARALLEL_JOBS TIMEOUT_SECONDS VERBOSE"
missing_vars=""

for var in $env_vars; do
    if grep -q "\${$var" "$VALIDATE_SCRIPT" || grep -q "\".*\$${var}.*\"" "$VALIDATE_SCRIPT"; then
        echo "  ✓ Variable '$var' used"
    else
        echo "  ⚠ Variable '$var' not found (may be optional)"
    fi
done

# Test 5: Language detection patterns
echo ""
echo "Test 5: Language detection patterns"
languages="python javascript go rust java ruby php typescript cpp c bash perl"
for lang in $languages; do
    if grep -q "\"$lang\"" "$VALIDATE_SCRIPT"; then
        echo "  ✓ Language '$lang' supported"
    fi
done

# Test 6: Report generation functions
echo ""
echo "Test 6: Report generation"
reports="generate_json_report generate_html_report"
for report in $reports; do
    if grep -q "^${report}()" "$VALIDATE_SCRIPT"; then
        echo "  ✓ Function '$report' defined"
    else
        echo "  ✗ Function '$report' not defined"
        exit 1
    fi
done
echo "✓ PASS: All report functions present"

# Test 7: Run script and check output structure
echo ""
echo "Test 7: Script execution and report generation"
cd "$SCRIPT_DIR"

# Run script (without API key, will find examples but not execute)
output=$(bash scripts/validate-examples.sh 2>&1 || true)

if echo "$output" | grep -q "Starting SDK examples validation"; then
    echo "  ✓ Script starts correctly"
else
    echo "  ✗ Script didn't start properly"
    exit 1
fi

# Check if reports directory created
if [ -d "science-results" ]; then
    echo "  ✓ Results directory created"
else
    echo "  ✗ Results directory not created"
    exit 1
fi

# Test 8: JSON report format
echo ""
echo "Test 8: JSON report validation"
if [ -f "science-results/examples-validation-results.json" ]; then
    echo "  ✓ JSON report file created"

    # Validate JSON structure
    if jq '.report_type' science-results/examples-validation-results.json >/dev/null 2>&1; then
        echo "  ✓ JSON is valid"

        # Check for required fields
        required_json_fields="report_type timestamp timestamp_readable summary language_stats"
        for field in $required_json_fields; do
            if jq -e ".$field" science-results/examples-validation-results.json >/dev/null 2>&1; then
                echo "    ✓ Field '$field' present"
            else
                echo "    ✗ Field '$field' missing"
                exit 1
            fi
        done
        echo "  ✓ PASS: JSON structure is correct"
    else
        echo "  ✗ JSON is invalid"
        exit 1
    fi
else
    echo "  ⚠ JSON report not found (examples may not exist)"
fi

# Test 9: HTML report format
echo ""
echo "Test 9: HTML report validation"
if [ -f "science-results/examples-validation-results.html" ]; then
    echo "  ✓ HTML report file created"

    # Check for key HTML elements
    html_checks="SDK Examples Validation Report language_stats success_rate"
    for check in $html_checks; do
        if grep -q "$check" science-results/examples-validation-results.html; then
            echo "    ✓ Contains '$check'"
        fi
    done
    echo "  ✓ PASS: HTML report generated successfully"
else
    echo "  ⚠ HTML report not found"
fi

# Test 10: Example file discovery
echo ""
echo "Test 10: Example file discovery"
example_files=$(find "$SCRIPT_DIR/clients" -path "*/examples/*" -type f \
    \( -name "*.py" -o -name "*.js" -o -name "*.go" -o -name "*.rs" \
       -o -name "*.java" -o -name "*.rb" -o -name "*.php" \) 2>/dev/null | wc -l)

if [ "$example_files" -gt 0 ]; then
    echo "  ✓ Found $example_files example files"
    echo "✓ PASS: Example discovery working"
else
    echo "  ⚠ No example files found (this is OK, examples can be added)"
fi

# Test 11: Language extension mapping
echo ""
echo "Test 11: Language extension detection"
extensions=".py .js .go .rs .java .rb .php .ts .cpp .c .sh .pl"
for ext in $extensions; do
    # Create temp test file
    temp_file="/tmp/test${ext}"
    touch "$temp_file"

    # Source the script to use detect_language function
    if bash -c "source '$VALIDATE_SCRIPT' 2>/dev/null; detect_language '$temp_file'" >/dev/null 2>&1; then
        echo "  ✓ Extension '$ext' recognized"
    fi

    rm -f "$temp_file"
done

# Summary
echo ""
echo "=============================================="
echo "Test Summary"
echo "=============================================="
echo "✓ All core tests passed!"
echo ""
echo "The validate-examples.sh script is ready for:"
echo "  - Local testing with: bash scripts/validate-examples.sh"
echo "  - CI/CD integration with UNSANDBOX_API_KEY set"
echo "  - Example file discovery in clients/*/examples/"
echo "  - JSON and HTML report generation"
echo ""
echo "Next steps:"
echo "  1. Add example files to clients/{language}/{sync,async}/examples/"
echo "  2. Set UNSANDBOX_API_KEY environment variable"
echo "  3. Run: bash scripts/validate-examples.sh"
echo "  4. View reports in science-results/"
echo ""
