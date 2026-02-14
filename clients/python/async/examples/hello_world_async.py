#!/usr/bin/env python3
"""
Hello World example - standalone async version

This example demonstrates basic async execution patterns using asyncio.
Shows how to use async/await for simple asynchronous operations.

To run:
    python3 hello_world_async.py

Expected output:
    Executing code asynchronously...
    Result status: completed
    Output: Hello from async unsandbox!
"""

import asyncio


async def execute_code(language: str, code: str) -> dict:
    """Simulated async code execution."""
    # Simulate API call delay
    await asyncio.sleep(0.05)

    # Return simulated result
    return {
        "status": "completed",
        "stdout": "Hello from async unsandbox!\n",
        "stderr": "",
    }


async def main():
    """Execute hello world code asynchronously."""

    # The code to execute
    code = 'print("Hello from async unsandbox!")'

    # Execute the code asynchronously
    print("Executing code asynchronously...")
    result = await execute_code("python", code)

    # Check for errors
    if result.get("status") == "completed":
        print(f"Result status: {result.get('status')}")
        print(f"Output: {result.get('stdout', '').strip()}")
        return 0
    else:
        print(f"Execution failed with status: {result.get('status')}")
        return 1


if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
