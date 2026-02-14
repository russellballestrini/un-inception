#!/usr/bin/env node
/**
 * Fibonacci example - standalone version
 *
 * Demonstrates concurrent fibonacci calculations using async/await.
 * Shows how to run multiple concurrent operations with Promise.all().
 *
 * To run:
 *   node fibonacci.js
 *
 * Expected output:
 *   Starting 3 concurrent fibonacci calculations...
 *   [fib-10] Result: fib(10) = 55
 *   [fib-15] Result: fib(15) = 610
 *   [fib-12] Result: fib(12) = 144
 *   All calculations completed!
 */

// Simulated fibonacci calculation (would normally call API)
function fib(n) {
  if (n <= 1) return n;
  return fib(n - 1) + fib(n - 2);
}

async function runFibonacci(n, label) {
  // Simulate async API call delay
  await new Promise((resolve) => setTimeout(resolve, 50));

  const result = fib(n);
  const output = `fib(${n}) = ${result}`;
  console.log(`[${label}] Result: ${output}`);
  return { label, output };
}

async function main() {
  console.log('Starting 3 concurrent fibonacci calculations...');

  // Run all fibonacci calculations concurrently
  const results = await Promise.all([
    runFibonacci(10, 'fib-10'),
    runFibonacci(15, 'fib-15'),
    runFibonacci(12, 'fib-12'),
  ]);

  console.log('All calculations completed!');
  return 0;
}

main().then(process.exit);
