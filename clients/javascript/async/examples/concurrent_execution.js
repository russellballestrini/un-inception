#!/usr/bin/env node
/**
 * Concurrent Execution example for unsandbox JavaScript SDK
 *
 * Demonstrates running code in multiple languages concurrently.
 * Shows the power of async/await with Promise.all() for parallel execution.
 *
 * To run:
 *   export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *   export UNSANDBOX_SECRET_KEY="your-secret-key"
 *   node concurrent_execution.js
 *
 * Expected output:
 *   Starting concurrent execution in 4 languages...
 *   [python] Output: Hello from Python!
 *   [javascript] Output: Hello from JavaScript!
 *   [go] Output: Hello from Go!
 *   [ruby] Output: Hello from Ruby!
 *   All executions completed in Xms
 */

import { executeCode, CredentialsError } from '../src/un_async.js';

const LANGUAGE_CODE = {
  python: 'print("Hello from Python!")',
  javascript: 'console.log("Hello from JavaScript!");',
  go: `package main
import "fmt"
func main() {
    fmt.Println("Hello from Go!")
}`,
  ruby: 'puts "Hello from Ruby!"',
};

async function runCode(language, code) {
  try {
    const result = await executeCode(language, code);
    const output = (result.stdout || '').trim();
    console.log(`[${language}] Output: ${output}`);
    return { language, output, success: true };
  } catch (e) {
    console.log(`[${language}] Error: ${e.message}`);
    return { language, error: e.message, success: false };
  }
}

async function main() {
  try {
    console.log('Starting concurrent execution in 4 languages...');
    const startTime = Date.now();

    // Execute all languages concurrently
    const results = await Promise.all(
      Object.entries(LANGUAGE_CODE).map(([lang, code]) => runCode(lang, code))
    );

    const elapsed = Date.now() - startTime;
    console.log(`All executions completed in ${elapsed}ms`);

    // Check for errors
    const successCount = results.filter((r) => r.success).length;
    console.log(`Success: ${successCount}/${results.length}`);

    return successCount === results.length ? 0 : 1;
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
