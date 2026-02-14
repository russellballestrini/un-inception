#!/usr/bin/env node
/**
 * Hello World example - standalone version
 *
 * This example demonstrates basic async execution patterns.
 * Shows how to use async/await for simple asynchronous operations.
 *
 * To run:
 *   node hello_world.js
 *
 * Expected output:
 *   Executing code asynchronously...
 *   Result status: completed
 *   Output: Hello from async unsandbox!
 */

// Simulated async execution
async function executeCode(language, code) {
  // Simulate API call delay
  await new Promise((resolve) => setTimeout(resolve, 50));

  // Return simulated result
  return {
    status: 'completed',
    stdout: 'Hello from async unsandbox!\n',
    stderr: '',
  };
}

async function main() {
  // The code to execute
  const code = 'print("Hello from async unsandbox!")';

  console.log('Executing code asynchronously...');
  const result = await executeCode('python', code);

  if (result.status === 'completed') {
    console.log(`Result status: ${result.status}`);
    console.log(`Output: ${(result.stdout || '').trim()}`);
    return 0;
  } else {
    console.log(`Execution failed with status: ${result.status}`);
    return 1;
  }
}

main().then(process.exit);
