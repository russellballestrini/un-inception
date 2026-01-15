#!/usr/bin/env python3
"""
Hello World example for unsandbox Python SDK - Asynchronous Version

This example demonstrates basic async execution with the unsandbox SDK.
Shows how to use asyncio with the async SDK client for simple code execution.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 hello_world_async.py

Expected output:
    Executing code asynchronously...
    Result status: completed
    Output: Hello from async unsandbox!
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un_async import execute_code, CredentialsError


async def main():
    """Execute hello world code asynchronously."""

    # The code to execute
    code = 'print("Hello from async unsandbox!")'

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("Run with: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            return 1

        # Execute the code asynchronously
        print("Executing code asynchronously...")
        result = await execute_code("python", code, public_key, secret_key)

        # Check for errors
        if result.get("status") == "completed":
            print(f"Result status: {result.get('status')}")
            print(f"Output: {result.get('stdout', '').strip()}")
            if result.get("stderr"):
                print(f"Errors: {result.get('stderr', '')}")
            return 0
        else:
            print(f"Execution failed with status: {result.get('status')}")
            print(f"Error: {result.get('error', 'Unknown error')}")
            return 1

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
