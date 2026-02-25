#!/usr/bin/env python3
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

"""
Stream Processing example - standalone version

Demonstrates async generator patterns and streaming data processing.
Shows how to handle potentially large datasets with async/await.

To run:
    python3 stream_processing.py

Expected output:
    Processing stream of data...
    [stream-task-1] Processed 10 items, sum: 45
    [stream-task-2] Processed 10 items, sum: 145
    [stream-task-3] Processed 10 items, sum: 245
    Stream processing completed!
"""

import asyncio


async def run_stream_task(task_num: int, start: int, count: int):
    """Execute stream processing task asynchronously."""

    # Simulate async API call delay
    await asyncio.sleep(0.05)

    # Simulate stream processing with generator
    def stream_generator(start, count):
        for i in range(start, start + count):
            yield i

    # Process stream
    total = 0
    item_count = 0
    for item in stream_generator(start, count):
        total += item
        item_count += 1

    print(f"[stream-task-{task_num}] Processed {item_count} items, sum: {total}")
    return {"task": task_num, "status": "completed"}


async def main():
    """Execute multiple stream processing tasks concurrently."""

    # Create concurrent tasks for stream processing
    print("Processing stream of data...")
    tasks = [
        run_stream_task(1, 0, 10),
        run_stream_task(2, 10, 10),
        run_stream_task(3, 20, 10),
    ]

    # Wait for all tasks to complete
    results = await asyncio.gather(*tasks)

    print("Stream processing completed!")

    # Check results
    all_completed = all(r.get("status") == "completed" for r in results)
    return 0 if all_completed else 1


if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
