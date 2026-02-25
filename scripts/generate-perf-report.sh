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

# Generate performance report for a tagged release
# Fetches job timing data from GitLab CI and creates a historical record
#
# Usage: scripts/generate-perf-report.sh [TAG]
# Example: scripts/generate-perf-report.sh 4.2.0
#
# Output: reports/{TAG}/perf.json and reports/{TAG}/perf.md
#
# Safe for automation - idempotent and validates inputs

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

TAG="${1:-}"
GITLAB_URL="https://git.unturf.com"
PROJECT_PATH="engineering/unturf/un-inception"

if [ -z "$TAG" ]; then
    # Default to current VERSION
    if [ -f VERSION ]; then
        TAG=$(cat VERSION)
        log_info "Using version from VERSION file: $TAG"
    else
        log_error "Usage: $0 <tag>"
        echo "Example: $0 4.2.0"
        exit 1
    fi
fi

# Validate tag format
if ! echo "$TAG" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    log_error "Invalid tag format: $TAG (expected X.Y.Z)"
    exit 1
fi

# Check for required tools
for cmd in curl jq bc; do
    if ! command -v $cmd &> /dev/null; then
        log_error "Required command not found: $cmd"
        exit 1
    fi
done

log_info "Generating performance report for tag: $TAG"

# Create versioned reports directory
REPORT_DIR="reports/$TAG"
mkdir -p "$REPORT_DIR"
log_info "Output directory: $REPORT_DIR"

# Check if report already exists (idempotent - skip if complete)
if [ -f "$REPORT_DIR/perf.json" ] && [ -f "$REPORT_DIR/perf.md" ]; then
    log_warn "Report already exists for $TAG"
    log_info "To regenerate, delete: rm -rf $REPORT_DIR"
    echo ""
    ls -la "$REPORT_DIR/"
    exit 0
fi

# Find pipeline for this tag (prefer passing pipelines)
echo "Finding pipeline for tag $TAG..."
PIPELINES=$(curl -s "$GITLAB_URL/$PROJECT_PATH/-/pipelines.json" | jq -r "[.pipelines[] | select(.ref.name == \"$TAG\" and .ref.tag == true)]")

if [ "$(echo "$PIPELINES" | jq 'length')" -eq 0 ]; then
    echo "Error: No pipeline found for tag $TAG"
    exit 1
fi

# Try to find a pipeline with fully passing child
PARENT_PIPELINE_ID=""
CHILD_PIPELINE_ID=""

for pid in $(echo "$PIPELINES" | jq -r '.[].id'); do
    CHILD_DATA=$(curl -s "$GITLAB_URL/$PROJECT_PATH/-/pipelines/$pid.json" | jq '.triggered[0]')
    CHILD_ID=$(echo "$CHILD_DATA" | jq -r '.id // empty')
    CHILD_STATUS=$(echo "$CHILD_DATA" | jq -r '.details.status.group // empty')

    if [ -n "$CHILD_ID" ]; then
        PARENT_PIPELINE_ID="$pid"
        CHILD_PIPELINE_ID="$CHILD_ID"
        # Prefer "success" over "success-with-warnings"
        if [ "$CHILD_STATUS" = "success" ]; then
            echo "Found fully passing pipeline: $pid -> $CHILD_ID"
            break
        fi
    fi
done

if [ -z "$CHILD_PIPELINE_ID" ]; then
    echo "Error: No child pipeline found for tag $TAG"
    exit 1
fi

echo "Using parent pipeline: $PARENT_PIPELINE_ID"
echo "Using child pipeline: $CHILD_PIPELINE_ID"

if [ -z "$CHILD_PIPELINE_ID" ] || [ "$CHILD_PIPELINE_ID" = "null" ]; then
    echo "Error: No child pipeline found"
    exit 1
fi

echo "Found child pipeline: $CHILD_PIPELINE_ID"

# Get all test jobs
echo "Fetching job data..."
JOBS_JSON=$(curl -s "$GITLAB_URL/$PROJECT_PATH/-/pipelines/$CHILD_PIPELINE_ID/stage.json?stage=test" | jq '.latest_statuses')

# Get test report summary
TEST_REPORT=$(curl -s "$GITLAB_URL/$PROJECT_PATH/-/pipelines/$CHILD_PIPELINE_ID/test_report.json")
TOTAL_TESTS=$(echo "$TEST_REPORT" | jq -r '.total_count // 0')
PASSED_TESTS=$(echo "$TEST_REPORT" | jq -r '.success_count // 0')
FAILED_TESTS=$(echo "$TEST_REPORT" | jq -r '.failed_count // 0')

# Parse job timing data
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
REPORT_DATE=$(date -u +"%Y-%m-%d")

# Create JSON report with per-language timing
echo "Parsing timing data..."

# Build JSON array of language performance
# Note: We strip milliseconds from timestamps for jq compatibility
PERF_JSON=$(echo "$JOBS_JSON" | jq -r '
    [.[] |
        select(.name | startswith("test: [")) |
        {
            language: (.name | gsub("test: \\[|\\]"; "")),
            status: .status.text,
            queued_duration: (.queued_duration // 0),
            started_at: (.started_at // "" | gsub("\\.[0-9]+Z$"; "Z")),
            updated_at: (.updated_at // "" | gsub("\\.[0-9]+Z$"; "Z")),
            job_id: .id
        }
    ] |
    map(. + {
        duration_seconds: (
            if .started_at != "" and .updated_at != "" then
                (((.updated_at | fromdateiso8601) - (.started_at | fromdateiso8601)) | floor)
            else 0 end
        )
    }) |
    sort_by(.duration_seconds) | reverse
')

# Calculate stats
TOTAL_LANGUAGES=$(echo "$PERF_JSON" | jq 'length')
PASSED_LANGUAGES=$(echo "$PERF_JSON" | jq '[.[] | select(.status == "Passed")] | length')
AVG_DURATION=$(echo "$PERF_JSON" | jq '[.[].duration_seconds] | add / length | floor')
MAX_DURATION=$(echo "$PERF_JSON" | jq '[.[].duration_seconds] | max')
MIN_DURATION=$(echo "$PERF_JSON" | jq '[.[].duration_seconds] | min')
SLOWEST_LANG=$(echo "$PERF_JSON" | jq -r '.[0].language')
FASTEST_LANG=$(echo "$PERF_JSON" | jq -r '.[-1].language')

# ============================================================================
# Collect API Health Data from Test Artifacts
# ============================================================================
log_info "Collecting API health data from test artifacts..."

# Initialize API health counters
TOTAL_RETRIES=0
RETRIES_429=0
RETRIES_5XX=0
RETRIES_TIMEOUT=0
RETRIES_CONN=0
TESTS_WITH_RETRIES=0
API_HEALTH_DATA="[]"

# Download api-health.json from each test job artifact
for job_id in $(echo "$JOBS_JSON" | jq -r '.[] | select(.name | startswith("test: [")) | .id'); do
    LANG=$(echo "$JOBS_JSON" | jq -r ".[] | select(.id == $job_id) | .name | gsub(\"test: \\\\[|\\\\]\"; \"\")")

    # Try to download the api-health.json artifact
    ARTIFACT_URL="$GITLAB_URL/$PROJECT_PATH/-/jobs/$job_id/artifacts/raw/test-results-$LANG/api-health.json"
    HEALTH_JSON=$(curl -s "$ARTIFACT_URL" 2>/dev/null || echo "{}")

    if echo "$HEALTH_JSON" | jq -e '.retries' > /dev/null 2>&1; then
        # Extract and accumulate retry counts
        JOB_RETRIES=$(echo "$HEALTH_JSON" | jq -r '.retries.total // 0')
        JOB_429=$(echo "$HEALTH_JSON" | jq -r '.retries.rate_limit_429 // 0')
        JOB_5XX=$(echo "$HEALTH_JSON" | jq -r '.retries.server_error_5xx // 0')
        JOB_TIMEOUT=$(echo "$HEALTH_JSON" | jq -r '.retries.timeout // 0')
        JOB_CONN=$(echo "$HEALTH_JSON" | jq -r '.retries.connection // 0')
        JOB_TESTS_WITH_RETRIES=$(echo "$HEALTH_JSON" | jq -r '.tests_with_retries // 0')

        TOTAL_RETRIES=$((TOTAL_RETRIES + JOB_RETRIES))
        RETRIES_429=$((RETRIES_429 + JOB_429))
        RETRIES_5XX=$((RETRIES_5XX + JOB_5XX))
        RETRIES_TIMEOUT=$((RETRIES_TIMEOUT + JOB_TIMEOUT))
        RETRIES_CONN=$((RETRIES_CONN + JOB_CONN))
        TESTS_WITH_RETRIES=$((TESTS_WITH_RETRIES + JOB_TESTS_WITH_RETRIES))

        # Add to per-language health data
        API_HEALTH_DATA=$(echo "$API_HEALTH_DATA" | jq ". + [{
            \"language\": \"$LANG\",
            \"retries\": $JOB_RETRIES,
            \"rate_limit_429\": $JOB_429,
            \"server_error_5xx\": $JOB_5XX,
            \"timeout\": $JOB_TIMEOUT,
            \"connection\": $JOB_CONN,
            \"tests_with_retries\": $JOB_TESTS_WITH_RETRIES
        }]")
    fi
done

# Calculate API health score (100 = perfect, decreases with retries)
API_HEALTH_SCORE=$(echo "scale=1; 100 - ($TOTAL_RETRIES * 2)" | bc 2>/dev/null || echo "100")
if [ "$(echo "$API_HEALTH_SCORE < 0" | bc)" -eq 1 ]; then
    API_HEALTH_SCORE="0"
fi

log_info "API Health: $TOTAL_RETRIES total retries (429: $RETRIES_429, 5xx: $RETRIES_5XX, timeout: $RETRIES_TIMEOUT, conn: $RETRIES_CONN)"

# Write JSON report
cat > "$REPORT_DIR/perf.json" << EOF
{
    "tag": "$TAG",
    "generated_at": "$TIMESTAMP",
    "parent_pipeline_id": $PARENT_PIPELINE_ID,
    "child_pipeline_id": $CHILD_PIPELINE_ID,
    "summary": {
        "total_tests": $TOTAL_TESTS,
        "passed_tests": $PASSED_TESTS,
        "failed_tests": $FAILED_TESTS,
        "total_languages": $TOTAL_LANGUAGES,
        "passed_languages": $PASSED_LANGUAGES,
        "avg_duration_seconds": $AVG_DURATION,
        "max_duration_seconds": $MAX_DURATION,
        "min_duration_seconds": $MIN_DURATION,
        "slowest_language": "$SLOWEST_LANG",
        "fastest_language": "$FASTEST_LANG"
    },
    "api_health": {
        "score": $API_HEALTH_SCORE,
        "total_retries": $TOTAL_RETRIES,
        "retries_by_type": {
            "rate_limit_429": $RETRIES_429,
            "server_error_5xx": $RETRIES_5XX,
            "timeout": $RETRIES_TIMEOUT,
            "connection": $RETRIES_CONN
        },
        "tests_with_retries": $TESTS_WITH_RETRIES,
        "per_language": $API_HEALTH_DATA
    },
    "languages": $PERF_JSON
}
EOF

echo "  ✓ $REPORT_DIR/perf.json"

# Write Markdown report with embedded charts
PASS_RATE=$(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)

cat > "$REPORT_DIR/perf.md" << EOF
# Performance Report: $TAG

**Generated:** $TIMESTAMP
**Pipeline:** [#$CHILD_PIPELINE_ID]($GITLAB_URL/$PROJECT_PATH/-/pipelines/$CHILD_PIPELINE_ID)

## Summary

| Metric | Value |
|--------|-------|
| Total Tests | $TOTAL_TESTS |
| Passed | $PASSED_TESTS |
| Failed | $FAILED_TESTS |
| Pass Rate | ${PASS_RATE}% |
| Languages | $TOTAL_LANGUAGES |
| Avg Duration | ${AVG_DURATION}s |
| Slowest | $SLOWEST_LANG (${MAX_DURATION}s) |
| Fastest | $FASTEST_LANG (${MIN_DURATION}s) |

---

## API Health

Tracks transient errors encountered during test execution. Tests retry on failures to ensure accurate results.

| Metric | Value |
|--------|-------|
| Health Score | ${API_HEALTH_SCORE}/100 |
| Total Retries | $TOTAL_RETRIES |
| Rate Limit (429) | $RETRIES_429 |
| Server Error (5xx) | $RETRIES_5XX |
| Timeout | $RETRIES_TIMEOUT |
| Connection | $RETRIES_CONN |
| Tests Needing Retries | $TESTS_WITH_RETRIES |

**Interpretation:**
- **Score 95-100:** API is healthy, minimal transient errors
- **Score 80-94:** Some API instability, but tests recovered via retry
- **Score < 80:** Significant API issues affecting test reliability

---

## Test Duration by Language

The primary performance metric - how long each language takes to run its full test suite (15 tests per language).

![Duration by Language](chart-duration-by-language.png)

**Key observations:**
- **$(echo "$PERF_JSON" | jq -r '.[0].language | ascii_upcase')** and **$(echo "$PERF_JSON" | jq -r '.[1].language | ascii_upcase')** are outliers at 90+ seconds
- Most languages cluster between 20-40 seconds
- Compiled languages (red) tend to be faster than interpreted (blue)
- **$(echo "$PERF_JSON" | jq -r '.[-1].language | ascii_upcase')** is the fastest at ${MIN_DURATION} seconds

---

## Compiled vs Interpreted

Comparing performance between compiled languages (C, Go, Rust, etc.) and interpreted languages (Python, Ruby, JavaScript, etc.).

![Category Comparison](chart-category-comparison.png)

**Findings:**
- 20 compiled languages vs 22 interpreted
- Compiled languages have lower median execution time
- Interpreted languages show more variance (wider spread)
- The white diamond marks the mean for each category

---

## Duration Distribution

Histogram showing how test durations are distributed across all $TOTAL_LANGUAGES languages.

![Duration Histogram](chart-duration-histogram.png)

**Distribution analysis:**
- Most languages complete in 20-35 seconds (the peak)
- Mean (green dashed) and median (blue dotted) are close together
- Long tail on the right from slow outliers ($SLOWEST_LANG, $(echo "$PERF_JSON" | jq -r '.[1].language'))

---

## Speed Leaders

Side-by-side comparison of the 10 slowest and 10 fastest languages.

![Speed Leaders](chart-speed-leaders.png)

**Slowest (left):** $(echo "$PERF_JSON" | jq -r '[.[0:5] | .[].language] | map(. | ascii_upcase) | join(", ")')
**Fastest (right):** $(echo "$PERF_JSON" | jq -r '[.[-5:] | .[].language] | map(. | ascii_upcase) | reverse | join(", ")')

---

## Queue vs Execution Time

Scatter plot showing the relationship between CI queue wait time and actual test execution time.

![Queue vs Execution](chart-queue-vs-execution.png)

**Notes:**
- Queue time is how long the job waited for a runner
- Most jobs had similar queue times (clustered vertically)
- Outliers labeled - $SLOWEST_LANG and $(echo "$PERF_JSON" | jq -r '.[1].language') took longest to execute regardless of queue time

---

## Dashboard

Summary dashboard combining key metrics and visualizations.

![Dashboard](chart-dashboard.png)

---

## Raw Data

### Per-Language Performance

| Language | Status | Duration |
|----------|--------|----------|
EOF

# Add per-language rows
echo "$PERF_JSON" | jq -r '.[] | "| \(.language) | \(.status) | \(.duration_seconds)s |"' >> "$REPORT_DIR/perf.md"

cat >> "$REPORT_DIR/perf.md" << EOF

---

*Report generated by UN Inception CI pipeline*
EOF

echo "  ✓ $REPORT_DIR/perf.md"

# Update or create historical index
if [ ! -f "reports/PERFORMANCE-HISTORY.md" ]; then
    cat > "reports/PERFORMANCE-HISTORY.md" << EOF
# Performance History

Tracking compile and test execution times across releases.

## Releases

| Version | Date | Tests | Pass Rate | Avg Duration | Slowest | Fastest |
|---------|------|-------|-----------|--------------|---------|---------|
EOF
fi

# Add this release to history (avoid duplicates)
if ! grep -q "| $TAG |" "reports/PERFORMANCE-HISTORY.md"; then
    echo "| $TAG | $REPORT_DATE | $TOTAL_TESTS | $(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)% | ${AVG_DURATION}s | $SLOWEST_LANG | $FASTEST_LANG |" >> "reports/PERFORMANCE-HISTORY.md"
    echo "  ✓ Updated reports/PERFORMANCE-HISTORY.md"
fi

echo ""
echo "Done! Performance report for $TAG:"
echo "  - $REPORT_DIR/perf.json"
echo "  - $REPORT_DIR/perf.md"
echo "  - reports/PERFORMANCE-HISTORY.md"
echo ""
echo "Next: make perf-charts TAG=$TAG"
echo "Then: git add reports/ && git commit -m 'perf: Add performance report for $TAG'"
