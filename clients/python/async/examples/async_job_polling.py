#!/usr/bin/env python3
"""
Async job polling example - demonstrates fire-and-forget execution

This example shows how to:
1. Start jobs asynchronously with execute_async()
2. Poll job status with get_job()
3. Wait for completion with wait_for_job()
4. Cancel jobs with cancel_job()

Usage:
    python async_job_polling.py

Or with custom credentials:
    UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... python async_job_polling.py
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un_async import execute_async, get_job, wait_for_job, list_jobs
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install aiohttp")
    sys.exit(0)  # Exit gracefully for CI


async def main():
    """Demonstrate async job operations."""
    try:
        print("1. Starting async job...")
        job_id = await execute_async("python", 'print("Job result")')
        print(f"   Job ID: {job_id}")

        print("\n2. Checking job status...")
        job_status = await get_job(job_id)
        print(f"   Status: {job_status.get('status')}")

        print("\n3. Waiting for job completion...")
        result = await wait_for_job(job_id)
        print(f"   Final status: {result.get('status')}")
        print(f"   Output: {result.get('stdout', '').strip()}")

        print("\n4. Listing all jobs...")
        jobs = await list_jobs()
        print(f"   Total jobs: {len(jobs)}")
        if jobs:
            print(f"   Most recent job ID: {jobs[0].get('id')}")

        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
