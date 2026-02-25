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
