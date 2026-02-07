#!/usr/bin/env python3
"""
Stream Processing example for unsandbox Python SDK - Asynchronous Version

Demonstrates async generator patterns and streaming data processing.
Shows how to handle potentially large datasets with async/await.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 stream_processing.py

Expected output:
    Processing stream of data...
    [stream-task-1] Processed 10 items, sum: 45
    [stream-task-2] Processed 10 items, sum: 145
    [stream-task-3] Processed 10 items, sum: 245
    Stream processing completed!
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un_async import execute_code, CredentialsError
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install aiohttp")
    sys.exit(0)  # Exit gracefully for CI


async def run_stream_task(task_num: int, start: int, count: int, public_key: str, secret_key: str):
    """Execute stream processing task asynchronously."""

    code = f"""
# Simulate stream processing with generator
def stream_generator(start, count):
    for i in range(start, start + count):
        yield i

# Process stream
total = 0
item_count = 0
for item in stream_generator({start}, {count}):
    total += item
    item_count += 1

print(f"Processed {{item_count}} items, sum: {{total}}")
"""

    try:
        result = await execute_code("python", code, public_key, secret_key)
        output = result.get("stdout", "").strip()
        print(f"[stream-task-{task_num}] {output}")
        return {"task": task_num, "status": "completed"}
    except Exception as e:
        print(f"[stream-task-{task_num}] Error: {e}")
        return {"task": task_num, "status": "failed"}


async def main():
    """Execute multiple stream processing tasks concurrently."""

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Skipping: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("To run: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            return 0  # Exit gracefully for CI

        # Create concurrent tasks for stream processing
        print("Processing stream of data...")
        tasks = [
            run_stream_task(1, 0, 10, public_key, secret_key),
            run_stream_task(2, 10, 10, public_key, secret_key),
            run_stream_task(3, 20, 10, public_key, secret_key),
        ]

        # Wait for all tasks to complete
        results = await asyncio.gather(*tasks)

        print("Stream processing completed!")

        # Check results
        all_completed = all(r.get("status") == "completed" for r in results)
        return 0 if all_completed else 1

    except CredentialsError as e:
        print(f"Credentials error: {e}")
        return 1
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
