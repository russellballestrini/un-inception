#!/bin/bash
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
    "languages": $PERF_JSON
}
EOF

echo "  ✓ $REPORT_DIR/perf.json"

# Write Markdown report
cat > "$REPORT_DIR/perf.md" << EOF
# Performance Report: $TAG

**Generated:** $TIMESTAMP
**Pipeline:** [$CHILD_PIPELINE_ID]($GITLAB_URL/$PROJECT_PATH/-/pipelines/$CHILD_PIPELINE_ID)

## Summary

| Metric | Value |
|--------|-------|
| Total Tests | $TOTAL_TESTS |
| Passed | $PASSED_TESTS |
| Failed | $FAILED_TESTS |
| Pass Rate | $(echo "scale=1; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc)% |
| Languages | $TOTAL_LANGUAGES |
| Avg Duration | ${AVG_DURATION}s |
| Slowest | $SLOWEST_LANG (${MAX_DURATION}s) |
| Fastest | $FASTEST_LANG (${MIN_DURATION}s) |

## Per-Language Performance

| Language | Status | Duration | Tests |
|----------|--------|----------|-------|
EOF

# Add per-language rows
echo "$PERF_JSON" | jq -r '.[] | "| \(.language) | \(.status) | \(.duration_seconds)s | - |"' >> "$REPORT_DIR/perf.md"

cat >> "$REPORT_DIR/perf.md" << EOF

## Timing Distribution

\`\`\`
EOF

# Add a simple ASCII histogram
echo "$PERF_JSON" | jq -r '.[] | "\(.language)|\(.duration_seconds)"' | while IFS='|' read -r lang dur; do
    bar=""
    bars=$((dur / 5))
    for ((i=0; i<bars && i<40; i++)); do bar+="█"; done
    printf "%-12s %3ds %s\n" "$lang" "$dur" "$bar" >> "$REPORT_DIR/perf.md"
done

cat >> "$REPORT_DIR/perf.md" << EOF
\`\`\`

---
*Report generated by \`make perf-report\`*
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
