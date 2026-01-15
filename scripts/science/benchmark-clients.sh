#!/bin/bash
# Benchmark SDK performance across implementations
# Burns pool with parallel execution of stress tests

set -e

mkdir -p benchmark-results

echo "Benchmarking SDK clients in parallel..."

# Run parallel benchmarks using unsandbox
# This burns idle pool capacity with valuable work
PIDS=()
LANGS=(python javascript ruby go rust java)

for LANG in "${LANGS[@]}"; do
    (
        echo "Benchmarking $LANG..."

        # Stress test: fibonacci calculation
        CODE='
def fib(n):
    if n <= 1: return n
    return fib(n-1) + fib(n-2)
print(fib(30))
        '

        START=$(date +%s%N)
        curl -s -X POST https://api.unsandbox.com/execute \
          -H "Authorization: Bearer ${UNSANDBOX_API_KEY}" \
          -H "Content-Type: application/json" \
          -d "{\"language\": \"$LANG\", \"code\": \"$CODE\"}" \
          > "benchmark-results/$LANG.json"
        END=$(date +%s%N)

        ELAPSED=$(( (END - START) / 1000000 ))  # Convert to ms
        RESULT=$(cat "benchmark-results/$LANG.json" | jq -r '.stdout' 2>/dev/null || echo "ERROR")

        echo "$LANG: ${ELAPSED}ms - $RESULT"
        echo "$ELAPSED" > "benchmark-results/$LANG.time"
    ) &
    PIDS+=($!)
done

# Wait for all benchmarks
wait "${PIDS[@]}"

# Aggregate results
TOTAL_TIME=0
SAMPLES=0
for FILE in benchmark-results/*.time; do
    TIME=$(cat "$FILE")
    TOTAL_TIME=$((TOTAL_TIME + TIME))
    SAMPLES=$((SAMPLES + 1))
done

AVG_TIME=$((TOTAL_TIME / SAMPLES))

# Generate report
cat > benchmark-results.xml << EOF
<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="Client Benchmarks" tests="$SAMPLES" failures="0">
    <testcase name="Parallel Execution" classname="science.benchmark">
      <system-out>Average latency: ${AVG_TIME}ms across $SAMPLES languages</system-out>
    </testcase>
  </testsuite>
</testsuites>
EOF

echo "Benchmarking complete: $SAMPLES languages, avg ${AVG_TIME}ms"
