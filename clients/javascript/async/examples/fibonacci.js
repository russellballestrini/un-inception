#!/usr/bin/env node
/**
 * Fibonacci example for unsandbox JavaScript SDK - Asynchronous Version
 *
 * Demonstrates concurrent fibonacci calculations using async/await.
 * Shows how to run multiple concurrent operations with Promise.all().
 *
 * To run:
 *   export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *   export UNSANDBOX_SECRET_KEY="your-secret-key"
 *   node fibonacci.js
 *
 * Expected output:
 *   Starting 3 concurrent fibonacci calculations...
 *   [fib-10] Result: fib(10) = 55
 *   [fib-15] Result: fib(15) = 610
 *   [fib-12] Result: fib(12) = 144
 *   All calculations completed!
 */

import { executeCode, CredentialsError } from '../src/un_async.js';

async function runFibonacci(n, label) {
  const code = `
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(f"fib(${n}) = {fib(${n})}")
`;

  try {
    const result = await executeCode('python', code);
    const output = (result.stdout || '').trim();
    console.log(`[${label}] Result: ${output}`);
    return { label, output };
  } catch (e) {
    console.log(`[${label}] Error: ${e.message}`);
    return { label, error: e.message };
  }
}

async function main() {
  try {
    console.log('Starting 3 concurrent fibonacci calculations...');

    // Run all fibonacci calculations concurrently
    const results = await Promise.all([
      runFibonacci(10, 'fib-10'),
      runFibonacci(15, 'fib-15'),
      runFibonacci(12, 'fib-12'),
    ]);

    console.log('All calculations completed!');

    // Check for errors
    const hasErrors = results.some((r) => r.error);
    return hasErrors ? 1 : 0;
  } catch (e) {
    if (e instanceof CredentialsError) {
      console.log(`Credentials error: ${e.message}`);
    } else {
      console.log(`Error: ${e.message}`);
      console.error(e);
    }
    return 1;
  }
}

main().then(process.exit);
