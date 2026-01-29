#!/bin/bash
# Fast parallel cleanup of orphaned test resources
# Usage: scripts/cleanup-test-resources.sh [--dry-run]
#
# Cleans up resources matching pattern: test-{lang}-{pid}-{timestamp}
# Runs destroys in parallel for speed (up to 10 concurrent)
# Retries failed destroys up to 3 times with backoff

set -e

DRY_RUN=false
PARALLEL_JOBS=10
MAX_ROUNDS=3

if [ "$1" = "--dry-run" ]; then
    DRY_RUN=true
    echo "DRY RUN - no changes will be made"
fi

# Check for un CLI
UN_CLI="${UN_CLI:-build/un}"
if [ ! -x "$UN_CLI" ]; then
    UN_CLI="./build/un"
fi
if [ ! -x "$UN_CLI" ]; then
    echo "ERROR: un CLI not found at $UN_CLI"
    echo "Run 'make build' first or set UN_CLI=/path/to/un"
    exit 1
fi

echo "=== Fast Cleanup of Test Resources ==="
echo "Using: $UN_CLI"
echo "Parallel jobs: $PARALLEL_JOBS"
echo ""

# Function to destroy in parallel with retry rounds
parallel_destroy() {
    local resource_type="$1"
    local destroy_cmd="$2"
    local pattern="$3"
    local id_pattern="$4"

    echo "--- Cleaning $resource_type ---"

    local round=1
    local total_destroyed=0

    while [ $round -le $MAX_ROUNDS ]; do
        # Get list of matching resources
        RESOURCES=$($UN_CLI $resource_type --list 2>/dev/null | grep -E "$pattern" || true)

        if [ -z "$RESOURCES" ]; then
            if [ $round -eq 1 ]; then
                echo "No orphaned $resource_type found"
            fi
            break
        fi

        # Extract IDs
        IDS=$(echo "$RESOURCES" | grep -oE "$id_pattern" | sort -u || true)
        COUNT=$(echo "$IDS" | grep -c . || echo 0)

        if [ "$COUNT" -eq 0 ]; then
            break
        fi

        if [ $round -eq 1 ]; then
            echo "Found $COUNT orphaned $resource_type"
        else
            echo "Round $round: $COUNT remaining"
        fi

        if [ "$DRY_RUN" = true ]; then
            echo "$IDS" | head -10
            [ "$COUNT" -gt 10 ] && echo "... and $((COUNT - 10)) more"
            return 0
        fi

        # Destroy in parallel using xargs
        # Capture failures by writing failed IDs to a temp file
        FAIL_FILE=$(mktemp)
        echo "$IDS" | xargs -P "$PARALLEL_JOBS" -I {} sh -c "
            OUTPUT=\$($UN_CLI $resource_type $destroy_cmd '{}' 2>&1)
            if [ \$? -eq 0 ] && ! echo \"\$OUTPUT\" | grep -qiE 'HTTP [45][0-9][0-9]|error|unavailable|failed'; then
                echo 'Destroyed: {}'
            else
                echo '{}' >> '$FAIL_FILE'
                echo 'Failed: {} -' \$(echo \"\$OUTPUT\" | head -1)
            fi
        "

        FAILED_COUNT=0
        if [ -s "$FAIL_FILE" ]; then
            FAILED_COUNT=$(wc -l < "$FAIL_FILE")
        fi
        rm -f "$FAIL_FILE"

        SUCCEEDED=$((COUNT - FAILED_COUNT))
        total_destroyed=$((total_destroyed + SUCCEEDED))

        if [ "$FAILED_COUNT" -eq 0 ]; then
            break
        fi

        echo "Round $round: $SUCCEEDED destroyed, $FAILED_COUNT failed"

        if [ $round -lt $MAX_ROUNDS ]; then
            BACKOFF=$((round * 5))
            echo "Waiting ${BACKOFF}s before retry..."
            sleep $BACKOFF
        fi

        round=$((round + 1))
    done

    echo "$resource_type cleanup complete ($total_destroyed destroyed)"
}

# Cleanup services (test-python-12345-1706000000, etc.)
parallel_destroy "service" "--destroy" "test-[a-z]+-[0-9]+" "unsb-service-[a-f0-9]+"

# Cleanup sessions
parallel_destroy "session" "--kill" "test-[a-z]+-[0-9]+" "unsb-session-[a-f0-9-]+"

# Cleanup snapshots
parallel_destroy "snapshot" "--delete" "test-[a-z]+-[0-9]+" "unsb-snapshot-[a-f0-9-]+"

# Cleanup images
parallel_destroy "image" "--delete" "test-[a-z]+-[0-9]+" "unsb-image-[a-f0-9-]+"

echo ""
echo "=== Cleanup Complete ==="
