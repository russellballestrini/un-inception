#!/usr/bin/env python3
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

for i in range(11):
    print(f"fib({i}) = {fib(i)}")
