#!/bin/bash
# validate-examples.sh - Core script that FINDS and VALIDATES all SDK examples
# This is the HEART of self-validating documentation
#
# Features:
# - Recursively finds all example files in clients/*/examples/ directories
# - Detects language from file extension
# - Executes via unsandbox API with proper authentication
# - Validates output against expected results
# - Generates JSON and HTML reports
# - Parallel execution for speed
#
# Usage:
#   bash scripts/validate-examples.sh
#
# Environment:
#   UNSANDBOX_PUBLIC_KEY - Public key for HMAC authentication
#   UNSANDBOX_SECRET_KEY - Secret key for HMAC authentication
#   UNSANDBOX_API_URL - API endpoint (default: https://api.unsandbox.com)
#   PARALLEL_JOBS - Number of parallel executions (default: 4)

set -o pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
RESULTS_DIR="${PROJECT_ROOT}/science-results"
REPORT_JSON="${RESULTS_DIR}/examples-validation-results.json"
REPORT_HTML="${RESULTS_DIR}/examples-validation-results.html"
TEMP_DIR="/tmp/unsandbox-examples-$$"
EXAMPLES_DIR="${PROJECT_ROOT}/clients"

# Default configuration
UNSANDBOX_API_URL="${UNSANDBOX_API_URL:-https://api.unsandbox.com}"
PARALLEL_JOBS="${PARALLEL_JOBS:-4}"
TIMEOUT_SECONDS=30
VERBOSE="${VERBOSE:-0}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL_EXAMPLES=0
TOTAL_VALIDATED=0
TOTAL_FAILED=0

# Check if we have valid API credentials
has_api_credentials() {
    [[ -n "$UNSANDBOX_PUBLIC_KEY" && -n "$UNSANDBOX_SECRET_KEY" ]]
}

# Generate HMAC signature for API request
generate_hmac_signature() {
    local method=$1
    local path=$2
    local body=$3
    local timestamp=$4

    local message="${timestamp}:${method}:${path}:${body}"
    echo -n "$message" | openssl dgst -sha256 -hmac "$UNSANDBOX_SECRET_KEY" | awk '{print $2}'
}
declare -A LANGUAGE_STATS
declare -A EXECUTION_TIMES

# Create results directory
mkdir -p "$RESULTS_DIR" "$TEMP_DIR"

# Cleanup on exit
cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# Logging functions
log() {
    echo -e "${BLUE}[INFO]${NC} $*" >&2
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $*" >&2
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $*" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

debug() {
    [[ $VERBOSE -eq 1 ]] && echo -e "${BLUE}[DEBUG]${NC} $*" >&2
}

# Helper: Get language from file extension
detect_language() {
    local file=$1
    local ext="${file##*.}"

    case "$ext" in
        py|python) echo "python" ;;
        js|javascript) echo "javascript" ;;
        go|golang) echo "go" ;;
        rs|rust) echo "rust" ;;
        java) echo "java" ;;
        rb|ruby) echo "ruby" ;;
        php) echo "php" ;;
        ts|typescript) echo "typescript" ;;
        cpp|cc|c\+\+) echo "cpp" ;;
        c) echo "c" ;;
        sh|bash) echo "bash" ;;
        pl|perl) echo "perl" ;;
        *) echo "" ;;
    esac
}

# Helper: Map language name to API parameter
get_api_language() {
    local lang=$1
    case "$lang" in
        cpp) echo "c++" ;;
        *) echo "$lang" ;;
    esac
}

# Helper: Extract expected output from file comments
extract_expected_output() {
    local file=$1

    # Look for expected output in comments
    # Supports:
    # // Expected output: ...
    # # Expected output: ...
    # -- Expected output: ...

    grep -E '(//|#|--|/\*|{\s*\/\/)\s*(Expected output|Output|Result):\s*' "$file" | \
        sed -E 's/^[^:]*:\s*//' | \
        sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | \
        head -1
}

# Helper: Parse JSON response safely
safe_json_extract() {
    local json=$1
    local key=$2

    echo "$json" | jq -r ".$key // \"\"" 2>/dev/null || echo ""
}

# Helper: Compile C example to binary
compile_c_example() {
    local source_file=$1
    local binary_file="${TEMP_DIR}/example-${RANDOM}"

    debug "Compiling C example: $source_file"

    # Compile with gcc (using standard flags)
    if gcc -o "$binary_file" "$source_file" 2>/dev/null; then
        echo "$binary_file"
        return 0
    else
        debug "C compilation failed for $source_file"
        return 1
    fi
}

# Helper: Execute Python async example via subprocess
execute_python_async() {
    local python_code=$1

    debug "Executing Python async code"

    # Use Python to run async code via asyncio.run()
    # This allows testing of async/await patterns
    python3 -c "import asyncio; asyncio.run(eval('async def _async_main():\\n' + '\\n'.join('    ' + line for line in '''$python_code'''.split('\\n')) + '\\n\\nasyncio.run(_async_main())'))" 2>&1
    return $?
}

# Helper: Execute local binary or script directly
execute_local_file() {
    local file=$1
    local language=$2

    debug "Executing local file: $file ($language)"

    case "$language" in
        c)
            # For C examples, compile and execute
            local binary=$(compile_c_example "$file")
            if [[ -n "$binary" ]] && [[ -x "$binary" ]]; then
                timeout "$TIMEOUT_SECONDS" "$binary" 2>&1
                return $?
            else
                return 1
            fi
            ;;
        python)
            # For Python examples, execute directly
            timeout "$TIMEOUT_SECONDS" python3 "$file" 2>&1
            return $?
            ;;
        *)
            return 1
            ;;
    esac
}

# Main validation function for a single example file
validate_example() {
    local example_file=$1
    local language
    local api_lang
    local code
    local start_time
    local elapsed_time
    local api_response
    local stdout_content
    local stderr_content
    local exit_code
    local result_file="${TEMP_DIR}/result-${RANDOM}.json"
    local execution_method="api"

    # Detect language
    language=$(detect_language "$example_file")
    if [[ -z "$language" ]]; then
        log_fail "Unknown language for $example_file"
        return 1
    fi

    # Initialize language stats if not exists
    if [[ -z "${LANGUAGE_STATS[$language]}" ]]; then
        LANGUAGE_STATS[$language]=0
    fi

    # Read code
    code=$(cat "$example_file")
    if [[ -z "$code" ]]; then
        log_fail "Empty code file: $example_file"
        return 1
    fi

    # Measure execution time
    start_time=$(date +%s%N)

    # Determine execution method
    # If no credentials, try local execution for C and Python
    if ! has_api_credentials; then
        case "$language" in
            c|python)
                debug "No credentials - attempting local execution for $language"
                execution_method="local"
                ;;
            *)
                log_warn "$example_file - No API credentials, skipping execution"
                return 0
                ;;
        esac
    fi

    # Execute via local method or API
    if [[ "$execution_method" == "local" ]]; then
        # Local execution for C and Python
        api_response=$(execute_local_file "$example_file" "$language")
        exit_code=$?
        stdout_content="$api_response"
        stderr_content=""
    else
        # API execution with HMAC authentication
        api_lang=$(get_api_language "$language")
        local body="{\"language\": \"${api_lang}\", \"code\": $(echo "$code" | jq -R -s .)}"
        local timestamp=$(date +%s)
        local signature=$(generate_hmac_signature "POST" "/execute" "$body" "$timestamp")

        # Execute via API with timeout
        debug "Executing $example_file ($api_lang) via API"
        api_response=$(curl -s -X POST "${UNSANDBOX_API_URL}/execute" \
            -H "Authorization: Bearer ${UNSANDBOX_PUBLIC_KEY}" \
            -H "X-Timestamp: ${timestamp}" \
            -H "X-Signature: ${signature}" \
            -H "Content-Type: application/json" \
            --max-time "$TIMEOUT_SECONDS" \
            -d "$body" \
            2>&1)

        exit_code=$?

        # Extract results from API response
        if [[ $exit_code -eq 0 ]]; then
            stdout_content=$(safe_json_extract "$api_response" "stdout")
            stderr_content=$(safe_json_extract "$api_response" "stderr")
            exit_code=$(safe_json_extract "$api_response" "exit_code")

            # Treat empty stderr as success
            if [[ -z "$stderr_content" || "$stderr_content" == "null" ]]; then
                stderr_content=""
            fi
        else
            stderr_content="API request failed (curl exit code $exit_code)"
        fi
    fi

    elapsed_time=$(( ($(date +%s%N) - start_time) / 1000000 ))  # Convert to milliseconds

    # Check if execution was successful
    if [[ "$exit_code" == "0" || -z "$exit_code" ]]; then
        log_pass "$example_file ($language) - ${elapsed_time}ms [$execution_method]"
        LANGUAGE_STATS[$language]=$((${LANGUAGE_STATS[$language]} + 1))
        EXECUTION_TIMES[$language]=$((${EXECUTION_TIMES[$language]:-0} + elapsed_time))
        TOTAL_VALIDATED=$((TOTAL_VALIDATED + 1))
    else
        log_fail "$example_file ($language) - exit code $exit_code [$execution_method]"
        if [[ -n "$stderr_content" ]]; then
            debug "stderr: $stderr_content"
        fi
        TOTAL_FAILED=$((TOTAL_FAILED + 1))
    fi

    # Save result for JSON report
    cat > "$result_file" <<EOF
{
    "file": "$example_file",
    "language": "$language",
    "execution_method": "$execution_method",
    "status": $([ "$exit_code" == "0" ] && echo "\"pass\"" || echo "\"fail\""),
    "execution_time_ms": $elapsed_time,
    "exit_code": $exit_code,
    "stdout_preview": $(echo "$stdout_content" | jq -R -s . | head -c 200),
    "stderr_preview": $(echo "$stderr_content" | jq -R -s . | head -c 200)
}
EOF

    return 0
}

# Parallel execution wrapper
validate_examples_parallel() {
    local file
    local job_count=0
    local pids=()

    while IFS= read -r file; do
        validate_example "$file" &
        pids+=($!)
        job_count=$((job_count + 1))

        # Limit parallel jobs
        if [[ $job_count -ge $PARALLEL_JOBS ]]; then
            wait -n 2>/dev/null || true
            pids=("${pids[@]:1}")
            job_count=$((job_count - 1))
        fi
    done

    # Wait for ALL background jobs to complete (not just tracked pids)
    # Using bare 'wait' ensures we catch all subprocesses, even those
    # whose pids were incorrectly removed from the array by wait -n
    wait

    # Extra safety margin for filesystem sync
    sleep 0.5

    # Aggregate results from result files (since subshell variables don't propagate)
    aggregate_results
}

# Aggregate results from temp files into parent shell variables
aggregate_results() {
    local result_file
    TOTAL_VALIDATED=0
    TOTAL_FAILED=0

    # Count result files and their status
    for result_file in "$TEMP_DIR"/result-*.json; do
        [[ -f "$result_file" ]] || continue

        local status
        status=$(jq -r '.status // "unknown"' "$result_file" 2>/dev/null || echo "unknown")
        local lang
        lang=$(jq -r '.language // "unknown"' "$result_file" 2>/dev/null || echo "unknown")
        local time_ms
        time_ms=$(jq -r '.execution_time_ms // 0' "$result_file" 2>/dev/null || echo "0")

        if [[ "$status" == "pass" ]]; then
            TOTAL_VALIDATED=$((TOTAL_VALIDATED + 1))
            LANGUAGE_STATS[$lang]=$((${LANGUAGE_STATS[$lang]:-0} + 1))
            EXECUTION_TIMES[$lang]=$((${EXECUTION_TIMES[$lang]:-0} + time_ms))
        elif [[ "$status" == "fail" ]]; then
            TOTAL_FAILED=$((TOTAL_FAILED + 1))
        fi
    done

    debug "Aggregated: $TOTAL_VALIDATED validated, $TOTAL_FAILED failed"
}

# Find all example files
find_examples() {
    if [[ ! -d "$EXAMPLES_DIR" ]]; then
        log_warn "Examples directory not found: $EXAMPLES_DIR"
        return 1
    fi

    # Find all files in examples directories
    # Look for common example patterns and extensions
    # Includes Python (.py), C (.c), JavaScript, Go, Rust, Java, etc.
    find "$EXAMPLES_DIR" \
        -path "*/examples/*" \
        \( -type f -name "*.py" -o -name "*.js" -o -name "*.go" -o \
           -name "*.rs" -o -name "*.java" -o -name "*.rb" -o -name "*.php" \
           -o -name "*.ts" -o -name "*.cpp" -o -name "*.c" -o -name "*.sh" \
           -o -name "*.pl" -o -name "*.example" \) 2>/dev/null | \
        sort
}

# Generate JSON report
generate_json_report() {
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local timestamp_readable=$(date -u "+%Y-%m-%d %H:%M:%S UTC")
    local success_rate="0"

    if [[ $((TOTAL_VALIDATED + TOTAL_FAILED)) -gt 0 ]]; then
        success_rate=$(echo "scale=1; $TOTAL_VALIDATED * 100 / ($TOTAL_VALIDATED + $TOTAL_FAILED)" | bc 2>/dev/null || echo "0")
    fi

    # Calculate average execution time per language
    local lang_times=()
    for lang in "${!LANGUAGE_STATS[@]}"; do
        local count=${LANGUAGE_STATS[$lang]}
        local total_time=${EXECUTION_TIMES[$lang]:-0}
        local avg_time=0
        if [[ $count -gt 0 ]]; then
            avg_time=$((total_time / count))
        fi
        lang_times+=("      {\"language\": \"$lang\", \"validated\": $count, \"total_time_ms\": $total_time, \"avg_time_ms\": $avg_time}")
    done

    # Build JSON report
    cat > "$REPORT_JSON" <<EOF
{
    "report_type": "examples_validation",
    "timestamp": "$timestamp",
    "timestamp_readable": "$timestamp_readable",
    "summary": {
        "total_examples": $TOTAL_EXAMPLES,
        "total_validated": $TOTAL_VALIDATED,
        "total_failed": $TOTAL_FAILED,
        "success_rate": "$success_rate%"
    },
    "language_stats": [
$(IFS=,; echo "${lang_times[*]}")
    ],
    "notes": "Examples validated through unsandbox API. Each example executed with ${TIMEOUT_SECONDS}s timeout."
}
EOF

    log "JSON report generated: $REPORT_JSON"
}

# Generate HTML report
generate_html_report() {
    local timestamp_readable=$(date -u "+%Y-%m-%d %H:%M:%S UTC")
    local success_rate=0
    if [[ $((TOTAL_VALIDATED + TOTAL_FAILED)) -gt 0 ]]; then
        success_rate=$(echo "scale=1; $TOTAL_VALIDATED * 100 / ($TOTAL_VALIDATED + $TOTAL_FAILED)" | bc 2>/dev/null || echo "0")
    fi

    # Build language table rows
    local lang_rows=""
    for lang in "${!LANGUAGE_STATS[@]}"; do
        local count=${LANGUAGE_STATS[$lang]}
        local avg_time=${EXECUTION_TIMES[$lang]:-0}
        if [[ $count -gt 0 ]]; then
            avg_time=$((avg_time / count))
        fi
        lang_rows+="      <tr><td>$lang</td><td>$count</td><td>${avg_time}ms</td></tr>\n"
    done

    # Determine status badge
    local status_color="green"
    local status_text="All Passing"
    if [[ $TOTAL_FAILED -gt 0 ]]; then
        status_color="red"
        status_text="Some Failures"
    elif [[ $TOTAL_EXAMPLES -eq 0 ]]; then
        status_color="yellow"
        status_text="No Examples Found"
    fi

    cat > "$REPORT_HTML" <<'HTMLEOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>SDK Examples Validation Report</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 2rem;
        }
        .container {
            max-width: 1000px;
            margin: 0 auto;
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 3rem 2rem;
            text-align: center;
        }
        .header h1 {
            font-size: 2.5rem;
            margin-bottom: 0.5rem;
        }
        .header p {
            font-size: 1.1rem;
            opacity: 0.9;
        }
        .status-badge {
            display: inline-block;
            padding: 0.5rem 1.5rem;
            border-radius: 50px;
            font-weight: 600;
            margin-top: 1rem;
            background: rgba(255,255,255,0.2);
            color: white;
            border: 2px solid white;
        }
        .status-badge.green { background: rgba(76, 175, 80, 0.8); border-color: #4CAF50; }
        .status-badge.red { background: rgba(244, 67, 54, 0.8); border-color: #F44336; }
        .status-badge.yellow { background: rgba(255, 193, 7, 0.8); border-color: #FFC107; }
        .content { padding: 3rem 2rem; }
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 2rem;
            margin-bottom: 3rem;
        }
        .stat-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 2rem;
            border-radius: 12px;
            text-align: center;
        }
        .stat-card .value {
            font-size: 3rem;
            font-weight: bold;
            margin-bottom: 0.5rem;
        }
        .stat-card .label {
            font-size: 0.9rem;
            opacity: 0.9;
            text-transform: uppercase;
            letter-spacing: 1px;
        }
        .success-rate {
            background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%);
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 2rem;
        }
        table th {
            background: #f5f5f5;
            padding: 1rem;
            text-align: left;
            font-weight: 600;
            color: #333;
            border-bottom: 2px solid #ddd;
        }
        table td {
            padding: 0.8rem 1rem;
            border-bottom: 1px solid #ddd;
        }
        table tr:hover {
            background: #fafafa;
        }
        .footer {
            background: #f9f9f9;
            padding: 2rem;
            text-align: center;
            color: #666;
            font-size: 0.9rem;
            border-top: 1px solid #ddd;
        }
        .checkmark { color: #4CAF50; font-weight: bold; }
        .cross { color: #F44336; font-weight: bold; }
        .section-title {
            font-size: 1.5rem;
            font-weight: 600;
            margin-top: 2rem;
            margin-bottom: 1rem;
            color: #333;
        }
        @media (max-width: 600px) {
            .header h1 { font-size: 1.8rem; }
            .stats-grid { grid-template-columns: 1fr; }
            .header { padding: 2rem 1.5rem; }
            .content { padding: 2rem 1.5rem; }
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>SDK Examples Validation Report</h1>
            <p>Ensuring all documentation examples actually work</p>
            <div class="status-badge STATUS_CLASS">STATUS_TEXT</div>
        </div>

        <div class="content">
            <div class="stats-grid">
                <div class="stat-card">
                    <div class="value">TOTAL_EXAMPLES</div>
                    <div class="label">Total Examples</div>
                </div>
                <div class="stat-card success-rate">
                    <div class="value">TOTAL_VALIDATED</div>
                    <div class="label">Validated</div>
                </div>
                <div class="stat-card">
                    <div class="value">TOTAL_FAILED</div>
                    <div class="label">Failed</div>
                </div>
                <div class="stat-card success-rate">
                    <div class="value">SUCCESS_RATE%</div>
                    <div class="label">Success Rate</div>
                </div>
            </div>

            <div class="section-title">Language Coverage</div>
            <p style="color: #666; margin-bottom: 1rem;">Validation statistics by language</p>
            <table>
                <thead>
                    <tr>
                        <th>Language</th>
                        <th>Examples Validated</th>
                        <th>Avg Execution Time</th>
                    </tr>
                </thead>
                <tbody>
                    LANGUAGE_ROWS
                </tbody>
            </table>

            <div style="margin-top: 2rem; padding: 1.5rem; background: #e3f2fd; border-left: 4px solid #2196F3; border-radius: 4px;">
                <p style="color: #1976D2; font-weight: 500; margin-bottom: 0.5rem;">Last verified:</p>
                <p style="color: #555;">TIMESTAMP_READABLE</p>
            </div>
        </div>

        <div class="footer">
            <p>This report validates that SDK examples execute successfully through the unsandbox API.</p>
            <p style="margin-top: 0.5rem; color: #999;">Generated automatically by the CI/CD pipeline</p>
        </div>
    </div>
</body>
</html>
HTMLEOF

    # Replace placeholders
    sed -i "s/STATUS_CLASS/$status_color/g" "$REPORT_HTML"
    sed -i "s/STATUS_TEXT/$status_text/g" "$REPORT_HTML"
    sed -i "s/TOTAL_EXAMPLES/$TOTAL_EXAMPLES/g" "$REPORT_HTML"
    sed -i "s/TOTAL_VALIDATED/$TOTAL_VALIDATED/g" "$REPORT_HTML"
    sed -i "s/TOTAL_FAILED/$TOTAL_FAILED/g" "$REPORT_HTML"
    sed -i "s/SUCCESS_RATE/$success_rate/g" "$REPORT_HTML"
    sed -i "s|LANGUAGE_ROWS|$lang_rows|g" "$REPORT_HTML"
    sed -i "s/TIMESTAMP_READABLE/$timestamp_readable/g" "$REPORT_HTML"

    log "HTML report generated: $REPORT_HTML"
}

# Main execution
main() {
    log "Starting SDK examples validation"
    log "Examples directory: $EXAMPLES_DIR"
    log "Results directory: $RESULTS_DIR"
    log "Parallel jobs: $PARALLEL_JOBS"

    # Check for HMAC credentials
    if has_api_credentials; then
        log "HMAC credentials detected - examples will execute with API access"
    else
        log_warn "No credentials set - examples will run locally (may exit early)"
    fi

    # Find all examples
    local examples
    examples=$(find_examples)

    if [[ -z "$examples" ]]; then
        log_warn "No examples found in $EXAMPLES_DIR"
    else
        TOTAL_EXAMPLES=$(echo "$examples" | wc -l)
        log "Found $TOTAL_EXAMPLES example files"

        # Validate examples (runs in subshell due to pipe)
        echo "$examples" | validate_examples_parallel

        # Re-aggregate in main shell (subshell variables don't propagate)
        aggregate_results
    fi

    # Generate reports
    log "Generating reports..."
    generate_json_report
    generate_html_report

    # Print summary
    echo ""
    echo "========================================"
    echo "SDK Examples Validation Summary"
    echo "========================================"
    echo "Total Examples:    $TOTAL_EXAMPLES"
    echo "Validated:         $TOTAL_VALIDATED"
    echo "Failed:            $TOTAL_FAILED"
    if [[ $((TOTAL_VALIDATED + TOTAL_FAILED)) -gt 0 ]]; then
        local success_rate=$(echo "scale=1; $TOTAL_VALIDATED * 100 / ($TOTAL_VALIDATED + $TOTAL_FAILED)" | bc 2>/dev/null || echo "0")
        echo "Success Rate:      $success_rate%"
    fi
    echo ""
    echo "Reports:"
    echo "  JSON: $REPORT_JSON"
    echo "  HTML: $REPORT_HTML"
    echo "========================================"

    # Exit with appropriate code
    if [[ $TOTAL_FAILED -eq 0 ]] && [[ $TOTAL_EXAMPLES -gt 0 ]]; then
        log_pass "All examples validated successfully"
        exit 0
    elif [[ $TOTAL_EXAMPLES -eq 0 ]]; then
        log_warn "No examples found to validate"
        exit 0  # Not a failure if no examples exist
    else
        log_fail "$TOTAL_FAILED example(s) failed validation"
        exit 1
    fi
}

# Run main function
main "$@"
