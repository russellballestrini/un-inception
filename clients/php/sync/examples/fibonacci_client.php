#!/usr/bin/env php
<?php
/**
 * Fibonacci Client example - standalone version
 *
 * Demonstrates JavaScript fibonacci calculation patterns.
 * Shows proper output handling and result processing.
 *
 * To run:
 *   php fibonacci_client.php
 *
 * Expected output:
 *   Executing JavaScript Fibonacci...
 *   fib(10) = 55
 *   fib(20) = 6765
 */

echo "Executing JavaScript Fibonacci...\n";

// Fibonacci function in PHP (simulating what would run in JS)
function fib($n) {
    if ($n <= 1) return $n;
    return fib($n - 1) + fib($n - 2);
}

// Calculate and print results
echo "fib(10) = " . fib(10) . "\n";
echo "fib(20) = " . fib(20) . "\n";
