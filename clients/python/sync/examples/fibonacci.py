#!/usr/bin/env python3
"""
Fibonacci example demonstrating recursive functions
Expected output: fib(10) = 55
"""

def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(f"fib(10) = {fib(10)}")
