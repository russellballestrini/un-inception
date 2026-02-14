#!/usr/bin/env python3
"""
Hello World Client pattern demonstration - standalone version

This example shows the pattern for executing code via the SDK client.
The actual SDK call is simulated since the SDK isn't available in sandbox.

Expected output:
    === SDK Client Pattern Demo ===
    Step 1: Initialize client with credentials
    Step 2: Execute code synchronously
    Step 3: Process result
    Result status: completed
    Output: Hello from unsandbox!
    Demo complete!
"""


def main():
    """Demonstrate SDK client usage pattern."""
    print("=== SDK Client Pattern Demo ===")

    print("Step 1: Initialize client with credentials")
    print("  public_key = os.environ.get('UNSANDBOX_PUBLIC_KEY')")
    print("  secret_key = os.environ.get('UNSANDBOX_SECRET_KEY')")

    print("Step 2: Execute code synchronously")
    print("  result = execute_code('python', code, public_key, secret_key)")

    print("Step 3: Process result")
    # Simulated result
    result = {
        "status": "completed",
        "stdout": "Hello from unsandbox!\n",
        "stderr": "",
        "exit_code": 0
    }

    print(f"Result status: {result['status']}")
    print(f"Output: {result['stdout'].strip()}")

    print("Demo complete!")
    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())
