#!/bin/bash
# Collect pool metrics during test execution
# Runs in background and samples CPU, memory, latency from /cluster and /pools
#
# Usage: scripts/collect-pool-metrics.sh start|stop|report [RESULTS_DIR]
#
# Metrics collected:
#   - Cluster load average (1m, 5m, 15m)
#   - Memory usage (used_gb, available_gb, used_percent)
#   - Pool availability (available, allocated, spawning)
#   - API latency (ms) - time to hit /languages endpoint
#   - Per-pool breakdown

set -e

ACTION="${1:-start}"
RESULTS_DIR="${2:-pool-metrics}"
METRICS_FILE="$RESULTS_DIR/pool-metrics.json"
PID_FILE="$RESULTS_DIR/.collector.pid"
RAW_FILE="$RESULTS_DIR/raw-samples.csv"

API_BASE="https://api.unsandbox.com"

mkdir -p "$RESULTS_DIR"

get_auth_header() {
    if [ -n "$UNSANDBOX_PUBLIC_KEY" ]; then
        echo "Authorization: Bearer $UNSANDBOX_PUBLIC_KEY"
    else
        echo "Authorization: Bearer none"
    fi
}

# Background collector loop
collector_loop() {
    # Cluster-level samples
    echo "timestamp,load1,load5,load15,mem_used_gb,mem_avail_gb,mem_pct,available,allocated,spawning,latency_ms,http_code" > "$RAW_FILE"
    # Per-pool samples
    POOLS_FILE="$RESULTS_DIR/raw-pools.csv"
    echo "timestamp,pool_id,load1,load5,load15,available,total,status" > "$POOLS_FILE"

    while true; do
        ts=$(date +%s)

        # Get cluster metrics
        start_ns=$(date +%s%N)
        cluster=$(curl -s "$API_BASE/cluster" -H "$(get_auth_header)" 2>/dev/null || echo "{}")
        end_ns=$(date +%s%N)
        latency_ms=$(( (end_ns - start_ns) / 1000000 ))

        # Parse cluster data
        load1=$(echo "$cluster" | jq -r '.load_avg.load1 // 0')
        load5=$(echo "$cluster" | jq -r '.load_avg.load5 // 0')
        load15=$(echo "$cluster" | jq -r '.load_avg.load15 // 0')
        mem_used=$(echo "$cluster" | jq -r '.memory.used_gb // 0')
        mem_avail=$(echo "$cluster" | jq -r '.memory.available_gb // 0')
        mem_pct=$(echo "$cluster" | jq -r '.memory.used_percent // 0')
        available=$(echo "$cluster" | jq -r '.available // 0')
        allocated=$(echo "$cluster" | jq -r '.allocated // 0')
        spawning=$(echo "$cluster" | jq -r '.spawning // 0')

        # Determine HTTP status
        if [ -n "$cluster" ] && [ "$cluster" != "{}" ]; then
            http_code="200"
        else
            http_code="000"
        fi

        echo "$ts,$load1,$load5,$load15,$mem_used,$mem_avail,$mem_pct,$available,$allocated,$spawning,$latency_ms,$http_code" >> "$RAW_FILE"

        # Get per-pool metrics
        pools=$(curl -s "$API_BASE/pools" -H "$(get_auth_header)" 2>/dev/null || echo "{}")
        if [ -n "$pools" ] && [ "$pools" != "{}" ]; then
            echo "$pools" | jq -r --arg ts "$ts" '.pools[] | "\($ts),\(.id),\(.load_avg.load1),\(.load_avg.load5),\(.load_avg.load15),\(.available),\(.total),\(.status)"' >> "$POOLS_FILE" 2>/dev/null || true
        fi

        sleep 5  # Sample every 5 seconds
    done
}

generate_report() {
    if [ ! -f "$RAW_FILE" ]; then
        echo "No metrics collected yet"
        exit 1
    fi

    # Get pools data for the report
    pools=$(curl -s "$API_BASE/pools" -H "$(get_auth_header)" 2>/dev/null || echo "{}")
    POOLS_FILE="$RESULTS_DIR/raw-pools.csv"

    # Parse raw samples and generate stats
    python3 - "$RAW_FILE" "$METRICS_FILE" "$pools" "$POOLS_FILE" << 'PYTHON'
import sys
import json
import statistics

raw_file = sys.argv[1]
output_file = sys.argv[2]
pools_json = sys.argv[3] if len(sys.argv) > 3 else "{}"
pools_csv = sys.argv[4] if len(sys.argv) > 4 else None

try:
    pools_data = json.loads(pools_json)
except:
    pools_data = {}

# Per-pool time series data
pool_samples = {}  # pool_id -> {load1: [], load5: [], ...}

samples = {
    "load1": [], "load5": [], "load15": [],
    "mem_used_gb": [], "mem_avail_gb": [], "mem_pct": [],
    "available": [], "allocated": [], "spawning": [],
    "latency_ms": []
}
timestamps = []
http_codes = {}

with open(raw_file) as f:
    next(f)  # skip header
    for line in f:
        parts = line.strip().split(',')
        if len(parts) >= 12:
            timestamps.append(int(parts[0]))
            samples["load1"].append(float(parts[1]))
            samples["load5"].append(float(parts[2]))
            samples["load15"].append(float(parts[3]))
            samples["mem_used_gb"].append(float(parts[4]))
            samples["mem_avail_gb"].append(float(parts[5]))
            samples["mem_pct"].append(float(parts[6]))
            samples["available"].append(int(parts[7]))
            samples["allocated"].append(int(parts[8]))
            samples["spawning"].append(int(parts[9]))
            samples["latency_ms"].append(int(parts[10]))
            code = parts[11]
            http_codes[code] = http_codes.get(code, 0) + 1

def calc_stats(values):
    if not values:
        return {"min": 0, "max": 0, "avg": 0, "median": 0, "stdev": 0}
    return {
        "min": round(min(values), 2),
        "max": round(max(values), 2),
        "avg": round(statistics.mean(values), 2),
        "median": round(statistics.median(values), 2),
        "stdev": round(statistics.stdev(values), 2) if len(values) > 1 else 0
    }

duration = (max(timestamps) - min(timestamps)) if timestamps else 0

report = {
    "summary": {
        "total_samples": len(timestamps),
        "collection_duration_seconds": duration,
        "success_rate": round(http_codes.get("200", 0) / len(timestamps) * 100, 1) if timestamps else 0
    },
    "cpu": {
        "load1": calc_stats(samples["load1"]),
        "load5": calc_stats(samples["load5"]),
        "load15": calc_stats(samples["load15"])
    },
    "memory": {
        "used_gb": calc_stats(samples["mem_used_gb"]),
        "available_gb": calc_stats(samples["mem_avail_gb"]),
        "used_percent": calc_stats(samples["mem_pct"])
    },
    "pool": {
        "available": calc_stats(samples["available"]),
        "allocated": calc_stats(samples["allocated"]),
        "spawning": calc_stats(samples["spawning"])
    },
    "latency_ms": calc_stats(samples["latency_ms"]),
    "http_codes": http_codes
}

# Parse per-pool time series if available
if pools_csv:
    try:
        import os
        if os.path.exists(pools_csv):
            with open(pools_csv) as f:
                next(f)  # skip header
                for line in f:
                    parts = line.strip().split(',')
                    if len(parts) >= 8:
                        pool_id = parts[1]
                        if pool_id not in pool_samples:
                            pool_samples[pool_id] = {"load1": [], "load5": [], "load15": [], "available": [], "total": []}
                        pool_samples[pool_id]["load1"].append(float(parts[2]))
                        pool_samples[pool_id]["load5"].append(float(parts[3]))
                        pool_samples[pool_id]["load15"].append(float(parts[4]))
                        pool_samples[pool_id]["available"].append(int(parts[5]))
                        pool_samples[pool_id]["total"].append(int(parts[6]))
    except Exception as e:
        print(f"Warning: Could not parse pools CSV: {e}")

# Add pool breakdown with time series stats
if pool_samples:
    report["pools"] = []
    for pool_id, samples in pool_samples.items():
        pool_info = {"id": pool_id}
        # Find host from pools_data
        if pools_data and "pools" in pools_data:
            for p in pools_data["pools"]:
                if p.get("id") == pool_id:
                    pool_info["host"] = p.get("host")
                    pool_info["vcpu_count"] = p.get("vcpu_count")
                    pool_info["status"] = p.get("status")
                    break
        pool_info["load1"] = calc_stats(samples["load1"])
        pool_info["load5"] = calc_stats(samples["load5"])
        pool_info["load15"] = calc_stats(samples["load15"])
        pool_info["available"] = calc_stats(samples["available"])
        pool_info["total"] = calc_stats(samples["total"])
        pool_info["sample_count"] = len(samples["load1"])
        report["pools"].append(pool_info)
elif pools_data and "pools" in pools_data:
    # Fallback to snapshot data
    report["pools"] = []
    for pool in pools_data["pools"]:
        report["pools"].append({
            "id": pool.get("id"),
            "host": pool.get("host"),
            "status": pool.get("status"),
            "vcpu_count": pool.get("vcpu_count"),
            "load_avg": pool.get("load_avg"),
            "available": pool.get("available"),
            "total": pool.get("total")
        })

with open(output_file, 'w') as f:
    json.dump(report, f, indent=2)

print(json.dumps(report, indent=2))
PYTHON
}

case "$ACTION" in
    start)
        if [ -f "$PID_FILE" ] && kill -0 $(cat "$PID_FILE") 2>/dev/null; then
            echo "Collector already running (PID: $(cat $PID_FILE))"
            exit 0
        fi
        echo "Starting pool metrics collector..."
        collector_loop &
        echo $! > "$PID_FILE"
        echo "Collector started (PID: $(cat $PID_FILE))"
        echo "Samples: $RAW_FILE"
        ;;

    stop)
        if [ -f "$PID_FILE" ]; then
            pid=$(cat "$PID_FILE")
            kill "$pid" 2>/dev/null || true
            rm -f "$PID_FILE"
            echo "Collector stopped"
        else
            echo "Collector not running"
        fi
        ;;

    report)
        generate_report
        ;;

    status)
        if [ -f "$PID_FILE" ] && kill -0 $(cat "$PID_FILE") 2>/dev/null; then
            echo "Collector running (PID: $(cat $PID_FILE))"
            wc -l "$RAW_FILE" 2>/dev/null || echo "No samples yet"
        else
            echo "Collector not running"
        fi
        ;;

    snapshot)
        # One-shot snapshot of current state
        curl -s "$API_BASE/cluster" -H "$(get_auth_header)" | jq '{
            load_avg,
            memory: {used_gb: .memory.used_gb, available_gb: .memory.available_gb, used_percent: .memory.used_percent},
            containers: {available, allocated, spawning},
            version
        }'
        ;;

    *)
        echo "Usage: $0 start|stop|report|status|snapshot [RESULTS_DIR]"
        exit 1
        ;;
esac
