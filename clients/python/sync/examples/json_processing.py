#!/usr/bin/env python3
"""
JSON Processing example for unsandbox Python SDK - Synchronous Version

This example demonstrates JSON parsing and manipulation operations.
Shows how to work with structured data in sandboxed environments.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    python3 json_processing.py

Expected output:
    Original JSON: {"name": "Alice", "age": 30, "skills": ["Python", "JavaScript"]}
    Parsed successfully!
    Name: Alice
    Age: 30
    Skills: Python, JavaScript
"""

import sys
import os

# Add the SDK path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un import execute_code, CredentialsError


def main():
    """Execute JSON processing code in sandbox."""

    # The code to execute - demonstrates JSON parsing and manipulation
    code = """
import json

# Original JSON data
json_string = '{"name": "Alice", "age": 30, "skills": ["Python", "JavaScript"]}'
print(f"Original JSON: {json_string}")

try:
    # Parse JSON
    data = json.loads(json_string)
    print("Parsed successfully!")

    # Access fields
    print(f"Name: {data['name']}")
    print(f"Age: {data['age']}")
    print(f"Skills: {', '.join(data['skills'])}")

    # Modify and re-serialize
    data['age'] = 31
    data['skills'].append("Go")
    modified_json = json.dumps(data, indent=2)
    print(f"\\nModified JSON:\\n{modified_json}")

except json.JSONDecodeError as e:
    print(f"JSON parsing error: {e}")
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
        print("Executing JSON processing in sandbox...")
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
