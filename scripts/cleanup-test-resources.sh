#!/bin/bash
# Fast parallel cleanup of orphaned test resources
# Usage: scripts/cleanup-test-resources.sh [--dry-run]
#
# Cleans up resources matching pattern: test-{lang}-{pid}-{timestamp}
# Runs destroys in parallel for speed (up to 10 concurrent)

set -e

DRY_RUN=false
PARALLEL_JOBS=10

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

# Function to destroy in parallel
parallel_destroy() {
    local resource_type="$1"
    local destroy_cmd="$2"
    local pattern="$3"
    local id_pattern="$4"

    echo "--- Cleaning $resource_type ---"

    # Get list of matching resources
    RESOURCES=$($UN_CLI $resource_type --list 2>/dev/null | grep -E "$pattern" || true)

    if [ -z "$RESOURCES" ]; then
        echo "No orphaned $resource_type found"
        return 0
    fi

    # Extract IDs
    IDS=$(echo "$RESOURCES" | grep -oE "$id_pattern" | sort -u || true)
    COUNT=$(echo "$IDS" | grep -c . || echo 0)

    echo "Found $COUNT orphaned $resource_type"

    if [ "$DRY_RUN" = true ]; then
        echo "$IDS" | head -10
        [ "$COUNT" -gt 10 ] && echo "... and $((COUNT - 10)) more"
        return 0
    fi

    # Destroy in parallel using xargs
    echo "$IDS" | xargs -P "$PARALLEL_JOBS" -I {} sh -c "
        echo 'Destroying: {}'
        $UN_CLI $resource_type $destroy_cmd '{}' 2>&1 || echo 'Failed: {}'
    "

    echo "$resource_type cleanup complete ($COUNT destroyed)"
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
