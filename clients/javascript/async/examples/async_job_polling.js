#!/usr/bin/env node
/**
 * Async Job Polling example for unsandbox JavaScript SDK
 *
 * Demonstrates fire-and-forget execution with manual job polling.
 * Shows how to start an async job and poll for its completion.
 *
 * To run:
 *   export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *   export UNSANDBOX_SECRET_KEY="your-secret-key"
 *   node async_job_polling.js
 *
 * Expected output:
 *   Starting async job...
 *   Job ID: job_abc123
 *   Polling for completion...
 *   Poll 1: status = running
 *   Poll 2: status = completed
 *   Final result: 42
 */

import {
  executeAsync,
  getJob,
  waitForJob,
  CredentialsError,
} from '../src/un_async.js';

async function main() {
  try {
    // Long-running code to execute
    const code = `
import time
time.sleep(0.5)  # Simulate some work
print(42)
`;

    console.log('Starting async job...');

    // Start the job (returns immediately with job_id)
    const jobId = await executeAsync('python', code);
    console.log(`Job ID: ${jobId}`);

    // Option 1: Manual polling
    console.log('Polling for completion...');
    let pollCount = 0;
    let result;

    while (true) {
      pollCount++;
      result = await getJob(jobId);
      console.log(`Poll ${pollCount}: status = ${result.status}`);

      if (['completed', 'failed', 'timeout', 'cancelled'].includes(result.status)) {
        break;
      }

      // Wait before next poll
      await new Promise((resolve) => setTimeout(resolve, 300));
    }

    console.log(`Final result: ${(result.stdout || '').trim()}`);
    return result.status === 'completed' ? 0 : 1;

    // Option 2: Use waitForJob (recommended - handles polling automatically)
    // const result = await waitForJob(jobId);
    // console.log(`Result: ${result.stdout}`);
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
