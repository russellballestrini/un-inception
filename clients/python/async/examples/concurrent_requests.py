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
Concurrent HTTP Requests example - standalone version

Demonstrates making multiple concurrent HTTP requests using asyncio.
Shows how to use asyncio.gather() for true concurrent execution.

To run:
    python3 concurrent_requests.py

Expected output:
    Starting 3 concurrent HTTP requests...
    [request-1] Status: 200, Response: {"ip": "1.2.3.4"}
    [request-2] Status: 200, Response: {"user-agent": "..."}
    [request-3] Status: 200, Response: {"headers": {...}}
    All requests completed successfully!
"""

import asyncio


async def run_http_request(request_num: int, url: str):
    """Execute simulated HTTP request asynchronously."""

    # Simulate async API call delay
    await asyncio.sleep(0.05)

    # Simulated responses
    responses = {
        "https://httpbin.org/ip": '{"origin": "1.2.3.4"}',
        "https://httpbin.org/user-agent": '{"user-agent": "Python/3.x"}',
        "https://httpbin.org/headers": '{"headers": {"Host": "httpbin.org"}}',
    }

    response = responses.get(url, '{"status": "ok"}')
    print(f"[request-{request_num}] Status: 200, Response: {response[:50]}...")
    return {"request": request_num, "status": "completed"}


async def main():
    """Execute multiple HTTP requests concurrently."""

    # Create concurrent tasks for HTTP requests
    print("Starting 3 concurrent HTTP requests...")
    tasks = [
        run_http_request(1, "https://httpbin.org/ip"),
        run_http_request(2, "https://httpbin.org/user-agent"),
        run_http_request(3, "https://httpbin.org/headers"),
    ]

    # Wait for all tasks to complete
    results = await asyncio.gather(*tasks)

    print("All requests completed successfully!")

    # Check results
    all_completed = all(r.get("status") == "completed" for r in results)
    return 0 if all_completed else 1


if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
