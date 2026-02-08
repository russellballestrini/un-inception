#!/usr/bin/env python3
"""
HTTP Request example for unsandbox Python SDK - Synchronous Version

This example demonstrates making HTTP requests from within a sandboxed environment.
Uses semitrusted mode which provides internet access through an egress proxy.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 http_request.py

Expected output:
    Status Code: 200
    Response: {"origin": "..."}
"""

import sys
import os

# Add the SDK path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un import execute_code, CredentialsError
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install requests")
    sys.exit(0)  # Exit gracefully for CI


def main():
    """Execute HTTP request code in sandbox."""

    # The code to execute - uses requests library (pre-installed)
    code = """
import requests
import json

try:
    # Make HTTP request to httpbin.org
    response = requests.get('https://httpbin.org/ip', timeout=10)
    print(f"Status Code: {response.status_code}")

    # Parse and display response
    data = response.json()
    print(f"Response: {json.dumps(data)}")

except requests.RequestException as e:
    print(f"Request failed: {e}")
except Exception as e:
    print(f"Error: {e}")
"""

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Skipping: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("To run: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            sys.exit(0)  # Exit gracefully for CI

        # Execute the code
        print("Executing HTTP request in sandbox...")
        result = execute_code("python", code, public_key, secret_key)

        # Check for errors
        if result.get("status") == "completed":
            print("\n=== STDOUT ===")
            print(result.get("stdout", ""))
            if result.get("stderr"):
                print("\n=== STDERR ===")
                print(result.get("stderr", ""))
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
