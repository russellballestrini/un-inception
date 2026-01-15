#!/usr/bin/env node
/**
 * Hello World example for unsandbox JavaScript SDK - Asynchronous Version
 *
 * This example demonstrates basic async execution with the unsandbox SDK.
 * Shows how to use async/await with the SDK for simple code execution.
 *
 * To run:
 *   export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *   export UNSANDBOX_SECRET_KEY="your-secret-key"
 *   node hello_world.js
 *
 * Expected output:
 *   Executing code asynchronously...
 *   Result status: completed
 *   Output: Hello from async unsandbox!
 */

import { executeCode, CredentialsError } from '../src/un_async.js';

async function main() {
  // The code to execute
  const code = 'print("Hello from async unsandbox!")';

  try {
    console.log('Executing code asynchronously...');
    const result = await executeCode('python', code);

    if (result.status === 'completed') {
      console.log(`Result status: ${result.status}`);
      console.log(`Output: ${(result.stdout || '').trim()}`);
      if (result.stderr) {
        console.log(`Errors: ${result.stderr}`);
      }
      return 0;
    } else {
      console.log(`Execution failed with status: ${result.status}`);
      console.log(`Error: ${result.error || 'Unknown error'}`);
      return 1;
    }
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
