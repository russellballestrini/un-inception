#!/usr/bin/env python3
"""
Concurrent execution example - standalone version

This example shows how to:
1. Execute multiple code snippets concurrently
2. Use asyncio.gather() to wait for all results
3. Handle multiple async operations efficiently

Usage:
    python concurrent_execution.py

Expected output:
    Running 4 concurrent code executions...

    [python_hello] Starting execution...
    [python_hello] Result: Hello from Python
    [js_hello] Starting execution...
    [js_hello] Result: Hello from JavaScript
    [bash_hello] Starting execution...
    [bash_hello] Result: Hello from Bash
    [python_math] Starting execution...
    [python_math] Result: pi = 3.1416

    === Execution Summary ===
    python_hello: OK
    js_hello: OK
    bash_hello: OK
    python_math: OK
"""

import asyncio
import math


async def run_code(language: str, code: str, name: str):
    """Execute simulated code and return result with a name."""
    print(f"[{name}] Starting execution...")

    # Simulate async API call delay
    await asyncio.sleep(0.05)

    # Simulated outputs based on the name
    outputs = {
        "python_hello": "Hello from Python",
        "js_hello": "Hello from JavaScript",
        "bash_hello": "Hello from Bash",
        "python_math": f"pi = {math.pi:.4f}",
    }

    output = outputs.get(name, "OK")
    print(f"[{name}] Result: {output}")
    return {"name": name, "result": {"stdout": output}}


async def main():
    """Execute multiple code snippets concurrently."""
    tasks = [
        run_code("python", 'print("Hello from Python")', "python_hello"),
        run_code("javascript", 'console.log("Hello from JavaScript")', "js_hello"),
        run_code("bash", 'echo "Hello from Bash"', "bash_hello"),
        run_code("python", 'import math; print(f"pi = {math.pi:.4f}")', "python_math"),
    ]

    print("Running 4 concurrent code executions...\n")
    results = await asyncio.gather(*tasks)

    print("\n=== Execution Summary ===")
    for result in results:
        print(f"{result['name']}: OK")

    return 0


if __name__ == "__main__":
    import sys
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
