#!/usr/bin/env python3
"""
Fibonacci example for unsandbox Python SDK - Asynchronous Version

Demonstrates concurrent fibonacci calculations using async/await.
Shows how to run multiple concurrent CPU-bound operations.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 fibonacci_async.py

Expected output:
    Starting 3 concurrent fibonacci calculations...
    [fib-10] Result: fib(10) = 55
    [fib-15] Result: fib(15) = 610
    [fib-12] Result: fib(12) = 144
    All calculations completed!
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


async def run_fibonacci(n: int, label: str, public_key: str, secret_key: str):
    """Execute fibonacci calculation asynchronously."""
    code = f"""
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(f"fib({n}) = {{fib({n})}}")
"""

    try:
        result = await execute_code("python", code, public_key, secret_key)
        output = result.get("stdout", "").strip()
        print(f"[{label}] Result: {output}")
        return {"label": label, "output": output}
    except Exception as e:
        print(f"[{label}] Error: {e}")
        return {"label": label, "error": str(e)}


async def main():
    """Execute multiple fibonacci calculations concurrently."""

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Skipping: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("To run: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            return 0  # Exit gracefully for CI

        # Create concurrent tasks for different fibonacci values
        print("Starting 3 concurrent fibonacci calculations...")
        tasks = [
            run_fibonacci(10, "fib-10", public_key, secret_key),
            run_fibonacci(15, "fib-15", public_key, secret_key),
            run_fibonacci(12, "fib-12", public_key, secret_key),
        ]

        # Wait for all tasks to complete
        results = await asyncio.gather(*tasks)

        print("All calculations completed!")

        # Check for errors
        has_errors = any("error" in result for result in results)
        return 1 if has_errors else 0

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
