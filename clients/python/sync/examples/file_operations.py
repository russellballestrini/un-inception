#!/usr/bin/env python3
"""
File Operations example for unsandbox Python SDK - Synchronous Version

This example demonstrates reading and writing files in sandboxed environments.
Shows temporary file creation and manipulation within the sandbox.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 file_operations.py

Expected output:
    File created at: /tmp/example.txt
    File contents:
    Line 1: Hello from the sandbox
    Line 2: This is temporary storage
    Line 3: File operations work!
    Total lines written: 3
"""

import sys
import os

# Add the SDK path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un import execute_code, CredentialsError


def main():
    """Execute file operations code in sandbox."""

    # The code to execute - demonstrates file I/O
    code = """
import os
import tempfile

# Create temporary file
temp_file = "/tmp/example.txt"

try:
    # Write to file
    with open(temp_file, "w") as f:
        f.write("Line 1: Hello from the sandbox\\n")
        f.write("Line 2: This is temporary storage\\n")
        f.write("Line 3: File operations work!\\n")

    print(f"File created at: {temp_file}")

    # Check if file exists
    if os.path.exists(temp_file):
        print(f"File exists: {os.path.isfile(temp_file)}")

        # Get file size
        file_size = os.path.getsize(temp_file)
        print(f"File size: {file_size} bytes")

    # Read from file
    print("File contents:")
    with open(temp_file, "r") as f:
        lines = f.readlines()
        for line in lines:
            print(line.rstrip())

    print(f"Total lines written: {len(lines)}")

except IOError as e:
    print(f"File operation error: {e}")
except Exception as e:
    print(f"Error: {e}")
"""

    try:
        # Resolve credentials from environment
        public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
        secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")

        if not public_key or not secret_key:
            print("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
            print("Run with: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
            sys.exit(1)

        # Execute the code
        print("Executing file operations in sandbox...")
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
