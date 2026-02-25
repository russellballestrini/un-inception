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
