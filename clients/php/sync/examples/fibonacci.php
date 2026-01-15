#!/usr/bin/env php
<?php
/**
 * Fibonacci example for unsandbox PHP SDK
 * Expected output:
 * fib(10) = 55
 * fib(20) = 6765
 */

function fib(int $n): int {
    if ($n <= 1) {
        return $n;
    }
    return fib($n - 1) + fib($n - 2);
}

echo "fib(10) = " . fib(10) . "\n";
echo "fib(20) = " . fib(20) . "\n";
