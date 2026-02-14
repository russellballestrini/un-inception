#!/usr/bin/env python3
"""
HTTP Request example - standalone version

This example demonstrates making HTTP requests (simulated).
Shows how to handle HTTP response processing.

To run:
    python3 http_request.py

Expected output:
    Executing HTTP request in sandbox...
    Status Code: 200
    Response: {"origin": "1.2.3.4"}
"""


def main():
    """Execute simulated HTTP request."""

    print("Executing HTTP request in sandbox...")

    # Simulated HTTP response (would normally call API)
    status_code = 200
    response_data = '{"origin": "1.2.3.4"}'

    print(f"\n=== STDOUT ===")
    print(f"Status Code: {status_code}")
    print(f"Response: {response_data}")


if __name__ == "__main__":
    main()
