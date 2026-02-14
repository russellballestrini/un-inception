#!/usr/bin/env python3
"""
Fibonacci Client example - standalone version

Demonstrates executing CPU-bound calculations.
Shows proper result processing.

To run:
    python3 fibonacci_client.py

Expected output:
    Calculating fibonacci(10)...
    Result status: completed
    Output: fib(10) = 55
"""


def fib(n: int) -> int:
    """Calculate fibonacci number."""
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)


def main():
    """Execute fibonacci calculation."""
    print("Calculating fibonacci(10)...")

    # Calculate fibonacci
    result = fib(10)

    # Print results
    print("Result status: completed")
    print(f"Output: fib(10) = {result}")


if __name__ == "__main__":
    main()
