#!/usr/bin/env python3
"""
Hello World Client example for unsandbox Python SDK - Synchronous Version

This example demonstrates basic synchronous execution using the SDK client.
Shows how to execute code from a Python program using the sync SDK.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 hello_world_client.py

Expected output:
    Executing code synchronously...
    Result status: completed
    Output: Hello from unsandbox!
"""

import sys
import os

# Add the SDK path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un import execute_code, CredentialsError, DependencyError
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install requests")
    sys.exit(0)  # Exit gracefully for CI


def main():
    """Execute hello world code using the SDK."""

    # The code to execute
    code = 'print("Hello from unsandbox!")'

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Skipping: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("To run: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            sys.exit(0)  # Exit gracefully for CI

        # Execute the code synchronously
        print("Executing code synchronously...")
        result = execute_code("python", code, public_key, secret_key)

        # Check for errors
        if result.get("status") == "completed":
            print(f"Result status: {result.get('status')}")
            print(f"Output: {result.get('stdout', '').strip()}")
            if result.get("stderr"):
                print(f"Errors: {result.get('stderr', '')}")
        else:
            print(f"Execution failed with status: {result.get('status')}")
            print(f"Error: {result.get('error', 'Unknown error')}")
            sys.exit(1)

    except CredentialsError as e:
        print(f"Credentials error: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
