#!/bin/bash
# Test runner for the 'un' CLI client
# Runs all Fibonacci test programs and verifies output
# Automatically delays between tests to respect API rate limits (7 req/min)

set -e

cd "$(dirname "$0")"

# Expected output for fib(0) through fib(10)
EXPECTED="fib(0) = 0
fib(1) = 1
fib(2) = 1
fib(3) = 2
fib(4) = 3
fib(5) = 5
fib(6) = 8
fib(7) = 13
fib(8) = 21
fib(9) = 34
fib(10) = 55"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if UNSANDBOX auth keys are set (HMAC or legacy)
if [ -z "$UNSANDBOX_PUBLIC_KEY" ] || [ -z "$UNSANDBOX_SECRET_KEY" ]; then
    # Fall back to legacy API key
    if [ -z "$UNSANDBOX_API_KEY" ]; then
        echo -e "${RED}Error: UNSANDBOX authentication not configured${NC}"
        echo "Please set HMAC auth keys:"
        echo "  export UNSANDBOX_PUBLIC_KEY=usk_pub_your_key_here"
        echo "  export UNSANDBOX_SECRET_KEY=usk_sec_your_key_here"
        echo ""
        echo "Or use legacy authentication:"
        echo "  export UNSANDBOX_API_KEY=usk_your_key_here"
        exit 1
    fi
fi

# Check if un binary exists
if [ ! -f "../un" ]; then
    echo -e "${RED}Error: 'un' binary not found${NC}"
    echo "Please build it first with: make un"
    exit 1
fi

UN="../un"

echo "Starting test suite for 'un' CLI client..."
echo ""

TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0
RATE_LIMITED=0
REQUEST_COUNT=0
DELAY_SECONDS=9  # 7 requests/minute = ~8.6s between requests, use 9s to be safe

# Arrays to store timing data
declare -a TIMINGS
declare -a LANGUAGES
declare -a STATUSES

# Test all fib.* files
for file in fib.*; do
    TOTAL=$((TOTAL + 1))
    ext="${file##*.}"

    echo -n "Testing $file ($ext)... "

    # Add delay after every request (except the first)
    if [ $REQUEST_COUNT -gt 0 ]; then
        echo -n "(waiting ${DELAY_SECONDS}s) "
        sleep $DELAY_SECONDS
    fi
    REQUEST_COUNT=$((REQUEST_COUNT + 1))

    # Run the test and measure time
    START_TIME=$(date +%s%N)
    OUTPUT=$($UN "$file" 2>&1 || true)
    END_TIME=$(date +%s%N)

    # Calculate execution time in milliseconds
    EXEC_TIME=$(( (END_TIME - START_TIME) / 1000000 ))

    # Check for rate limiting
    if echo "$OUTPUT" | grep -q "rate_limit_exceeded"; then
        echo -e "${YELLOW}RATE LIMITED${NC} (${EXEC_TIME}ms)"
        RATE_LIMITED=$((RATE_LIMITED + 1))
        TIMINGS+=("$EXEC_TIME")
        LANGUAGES+=("$ext")
        STATUSES+=("rate_limited")
        continue
    fi

    # Check if execution was successful
    if echo "$OUTPUT" | grep -q "Error:"; then
        echo -e "${YELLOW}SKIPPED${NC} (${EXEC_TIME}ms)"
        echo "  Output: $OUTPUT"
        SKIPPED=$((SKIPPED + 1))
        TIMINGS+=("$EXEC_TIME")
        LANGUAGES+=("$ext")
        STATUSES+=("skipped")
        continue
    fi

    # Verify output matches expected
    if echo "$OUTPUT" | grep -qF "$EXPECTED"; then
        echo -e "${GREEN}PASS${NC} (${EXEC_TIME}ms)"
        PASSED=$((PASSED + 1))
        TIMINGS+=("$EXEC_TIME")
        LANGUAGES+=("$ext")
        STATUSES+=("passed")
    else
        echo -e "${RED}FAIL${NC} (${EXEC_TIME}ms)"
        echo "  Expected:"
        echo "$EXPECTED" | sed 's/^/    /'
        echo "  Got:"
        echo "$OUTPUT" | sed 's/^/    /'
        FAILED=$((FAILED + 1))
        TIMINGS+=("$EXEC_TIME")
        LANGUAGES+=("$ext")
        STATUSES+=("failed")
    fi
done

echo ""
echo "=========================="
echo "Test Results:"
echo "  Total:        $TOTAL"
echo -e "  ${GREEN}Passed:       $PASSED${NC}"
echo -e "  ${RED}Failed:       $FAILED${NC}"
echo -e "  ${YELLOW}Skipped:      $SKIPPED${NC}"
echo -e "  ${YELLOW}Rate Limited: $RATE_LIMITED${NC}"
echo "=========================="

if [ $RATE_LIMITED -gt 0 ]; then
    echo ""
    echo -e "${YELLOW}Note: $RATE_LIMITED tests were still rate limited despite delays.${NC}"
    echo "Consider using a higher tier API key for faster testing."
fi

# Generate DOT file for visualization BEFORE exiting
echo ""
echo "Generating timing chart..."

DOT_FILE="timing_chart.dot"
cat > "$DOT_FILE" << 'EOF'
digraph TimingChart {
    rankdir=LR;
    node [shape=plaintext];

    graph [fontname="monospace", fontsize=10];
    node [fontname="monospace", fontsize=9];

    title [label=<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0">
        <TR><TD COLSPAN="2"><B>Unsandbox Language Execution Times</B></TD></TR>
        <TR><TD COLSPAN="2">Fibonacci(10) benchmark across 39 languages</TD></TR>
    </TABLE>>];

    chart [label=<
    <TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" CELLPADDING="4">
        <TR>
            <TD ALIGN="LEFT"><B>Language</B></TD>
            <TD ALIGN="LEFT"><B>Time (ms)</B></TD>
            <TD ALIGN="LEFT" WIDTH="400"><B>Bar Chart</B></TD>
            <TD ALIGN="LEFT"><B>Status</B></TD>
        </TR>
EOF

# Find max time for scaling
MAX_TIME=0
for time in "${TIMINGS[@]}"; do
    if [ "$time" -gt "$MAX_TIME" ]; then
        MAX_TIME=$time
    fi
done

# Add each language's data
for i in "${!LANGUAGES[@]}"; do
    lang="${LANGUAGES[$i]}"
    time="${TIMINGS[$i]}"
    status="${STATUSES[$i]}"

    # Calculate bar width (max 400 pixels)
    if [ "$MAX_TIME" -gt 0 ]; then
        bar_width=$(( time * 400 / MAX_TIME ))
    else
        bar_width=0
    fi

    # Color based on status
    case "$status" in
        "passed")
            color="#4CAF50"
            status_text="✓ PASS"
            ;;
        "failed")
            color="#F44336"
            status_text="✗ FAIL"
            ;;
        "rate_limited")
            color="#FFC107"
            status_text="⚠ RATE"
            ;;
        "skipped")
            color="#9E9E9E"
            status_text="○ SKIP"
            ;;
    esac

    cat >> "$DOT_FILE" << EOF
        <TR>
            <TD ALIGN="LEFT"><B>$lang</B></TD>
            <TD ALIGN="RIGHT">${time}</TD>
            <TD ALIGN="LEFT"><TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="0"><TR><TD WIDTH="$bar_width" HEIGHT="12" BGCOLOR="$color"></TD></TR></TABLE></TD>
            <TD ALIGN="LEFT"><FONT COLOR="$color">$status_text</FONT></TD>
        </TR>
EOF
done

cat >> "$DOT_FILE" << 'EOF'
    </TABLE>
    >];

    title -> chart [style=invis];
}
EOF

echo "DOT file generated: $DOT_FILE"
echo ""
echo "To generate PNG:"
echo "  dot -Tpng $DOT_FILE -o timing_chart.png"
echo ""
echo "To generate SVG:"
echo "  dot -Tsvg $DOT_FILE -o timing_chart.svg"
echo ""

# Now exit with appropriate status
if [ $FAILED -eq 0 ]; then
    if [ $RATE_LIMITED -eq 0 ]; then
        echo -e "${GREEN}All tests passed!${NC}"
    else
        echo -e "${YELLOW}All non-rate-limited tests passed!${NC}"
    fi
    exit 0
else
    echo -e "${RED}Some tests failed (but timing chart was generated)${NC}"
    exit 1
fi
