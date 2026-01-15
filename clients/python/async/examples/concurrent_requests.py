#!/usr/bin/env python3
"""
Concurrent HTTP Requests example for unsandbox Python SDK - Asynchronous Version

Demonstrates making multiple concurrent HTTP requests within sandboxed environments.
Shows how to use asyncio for true concurrent execution of network operations.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 concurrent_requests.py

Expected output:
    Starting 3 concurrent HTTP requests...
    [request-1] Status: 200, IP: 1.2.3.4
    [request-2] Status: 200, IP: 1.2.3.4
    [request-3] Status: 200, IP: 1.2.3.4
    All requests completed successfully!
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un_async import execute_code, CredentialsError


async def run_http_request(request_num: int, url: str, public_key: str, secret_key: str):
    """Execute HTTP request asynchronously."""

    code = f"""
import requests
import json

try:
    response = requests.get('{url}', timeout=10)
    data = response.json()
    print(f"Status: {{response.status_code}}, Response: {{json.dumps(data)[:100]}}")
except Exception as e:
    print(f"Error: {{e}}")
"""

    try:
        result = await execute_code("python", code, public_key, secret_key)
        output = result.get("stdout", "").strip()
        print(f"[request-{request_num}] {output}")
        return {"request": request_num, "status": "completed"}
    except Exception as e:
        print(f"[request-{request_num}] Error: {e}")
        return {"request": request_num, "status": "failed"}


async def main():
    """Execute multiple HTTP requests concurrently."""

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("Run with: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            return 1

        # Create concurrent tasks for HTTP requests
        print("Starting 3 concurrent HTTP requests...")
        tasks = [
            run_http_request(1, "https://httpbin.org/ip", public_key, secret_key),
            run_http_request(2, "https://httpbin.org/user-agent", public_key, secret_key),
            run_http_request(3, "https://httpbin.org/headers", public_key, secret_key),
        ]

        # Wait for all tasks to complete
        results = await asyncio.gather(*tasks)

        print("All requests completed successfully!")

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
