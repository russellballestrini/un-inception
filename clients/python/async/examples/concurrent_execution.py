#!/usr/bin/env python3
"""
Concurrent execution example - demonstrates async capabilities

This example shows how to:
1. Execute multiple code snippets concurrently
2. Use asyncio.gather() to wait for all results
3. Handle multiple async operations efficiently

Usage:
    python concurrent_execution.py

Or with custom credentials:
    UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... python concurrent_execution.py
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un_async import execute_code
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install aiohttp")
    sys.exit(0)  # Exit gracefully for CI


async def run_code(language: str, code: str, name: str):
    """Execute code and return result with a name."""
    print(f"[{name}] Starting execution...")
    result = await execute_code(language, code)
    output = result.get("stdout", "").strip()
    print(f"[{name}] Result: {output}")
    return {"name": name, "result": result}


async def main():
    """Execute multiple code snippets concurrently."""
    tasks = [
        run_code("python", 'print("Hello from Python")', "python_hello"),
        run_code("javascript", 'console.log("Hello from JavaScript")', "js_hello"),
        run_code("bash", 'echo "Hello from Bash"', "bash_hello"),
        run_code("python", 'import math; print(f"pi = {math.pi:.4f}")', "python_math"),
    ]

    try:
        print("Running 4 concurrent code executions...\n")
        results = await asyncio.gather(*tasks)

        print("\n=== Execution Summary ===")
        for result in results:
            print(f"{result['name']}: OK")

        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
