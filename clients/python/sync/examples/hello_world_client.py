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
