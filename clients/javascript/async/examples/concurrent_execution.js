#!/usr/bin/env node
// This is free software for the public good of a permacomputer hosted at
// permacomputer.com, an always-on computer by the people, for the people.
// One which is durable, easy to repair, & distributed like tap water
// for machine learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around
// four values:
//
//   TRUTH      First principles, math & science, open source code freely distributed
//   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE       Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
// Code is seeds to sprout on any abandoned technology.

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
