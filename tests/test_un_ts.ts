#!/usr/bin/env node
// Note: This TypeScript file can be run with ts-node if available,
// or compile with: tsc test_un_ts.ts && node test_un_ts.js
/**
 * Test suite for UN CLI TypeScript implementation (un.ts)
 * Tests extension detection, API calls, and end-to-end functionality
 */

import * as fs from 'fs';
import * as path from 'path';
import { execFile } from 'child_process';
import { promisify } from 'util';
import * as https from 'https';

const execFileAsync = promisify(execFile);

// Test configuration
const UN_SCRIPT = path.join(__dirname, '..', 'un.ts');
const FIB_PY = path.join(__dirname, '..', '..', 'test', 'fib.py');

class TestResults {
  passed: number = 0;
  failed: number = 0;
  skipped: number = 0;

  passTest(name: string): void {
    console.log(`PASS: ${name}`);
    this.passed++;
  }

  failTest(name: string, error: string): void {
    console.log(`FAIL: ${name} - ${error}`);
    this.failed++;
  }

  skipTest(name: string, reason: string): void {
    console.log(`SKIP: ${name} - ${reason}`);
    this.skipped++;
  }
}

const results = new TestResults();

// Load the extension map
const EXTENSION_MAP: Record<string, string> = {
  '.py': 'python', '.js': 'javascript', '.ts': 'typescript', '.rb': 'ruby',
  '.php': 'php', '.pl': 'perl', '.lua': 'lua', '.sh': 'bash',
  '.go': 'go', '.rs': 'rust', '.c': 'c', '.cpp': 'cpp', '.cc': 'cpp',
  '.java': 'java', '.kt': 'kotlin', '.cs': 'csharp', '.hs': 'haskell',
  '.ml': 'ocaml', '.clj': 'clojure', '.ex': 'elixir', '.erl': 'erlang',
  '.swift': 'swift', '.r': 'r', '.jl': 'julia', '.dart': 'dart',
  '.scala': 'scala', '.groovy': 'groovy', '.nim': 'nim', '.cr': 'crystal',
  '.v': 'vlang', '.zig': 'zig', '.fs': 'fsharp', '.vb': 'vb',
  '.pas': 'pascal', '.f90': 'fortran', '.asm': 'assembly', '.d': 'd',
  '.rkt': 'racket', '.scm': 'scheme', '.lisp': 'common_lisp',
  '.sol': 'solidity', '.cob': 'cobol', '.ada': 'ada', '.tcl': 'tcl',
};

function detectLanguage(filename: string): string | undefined {
  const ext = path.extname(filename).toLowerCase();
  return EXTENSION_MAP[ext];
}

interface ExecuteResult {
  stdout?: string;
  stderr?: string;
  exit_code?: number;
}

async function runTests(): Promise<void> {
  // Test 1: Extension detection for Python
  try {
    const lang = detectLanguage('test.py');
    if (lang === 'python') {
      results.passTest('Extension detection: .py -> python');
    } else {
      results.failTest('Extension detection: .py -> python', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .py -> python', (e as Error).message);
  }

  // Test 2: Extension detection for JavaScript
  try {
    const lang = detectLanguage('test.js');
    if (lang === 'javascript') {
      results.passTest('Extension detection: .js -> javascript');
    } else {
      results.failTest('Extension detection: .js -> javascript', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .js -> javascript', (e as Error).message);
  }

  // Test 3: Extension detection for Ruby
  try {
    const lang = detectLanguage('test.rb');
    if (lang === 'ruby') {
      results.passTest('Extension detection: .rb -> ruby');
    } else {
      results.failTest('Extension detection: .rb -> ruby', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .rb -> ruby', (e as Error).message);
  }

  // Test 4: Extension detection for Go
  try {
    const lang = detectLanguage('test.go');
    if (lang === 'go') {
      results.passTest('Extension detection: .go -> go');
    } else {
      results.failTest('Extension detection: .go -> go', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .go -> go', (e as Error).message);
  }

  // Test 5: Extension detection for Rust
  try {
    const lang = detectLanguage('test.rs');
    if (lang === 'rust') {
      results.passTest('Extension detection: .rs -> rust');
    } else {
      results.failTest('Extension detection: .rs -> rust', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .rs -> rust', (e as Error).message);
  }

  // Test 6: Extension detection for unknown extension
  try {
    const lang = detectLanguage('test.unknown');
    if (lang === undefined) {
      results.passTest('Extension detection: .unknown -> undefined');
    } else {
      results.failTest('Extension detection: .unknown -> undefined', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .unknown -> undefined', (e as Error).message);
  }

  // Test 7: API call test (requires UNSANDBOX_API_KEY)
  if (!process.env.UNSANDBOX_API_KEY) {
    results.skipTest('API call test', 'UNSANDBOX_API_KEY not set');
  } else {
    try {
      const apiKey = process.env.UNSANDBOX_API_KEY;
      const payload = JSON.stringify({
        language: 'python',
        code: 'print("Hello from API")'
      });

      const result: ExecuteResult = await new Promise((resolve, reject) => {
        const options: https.RequestOptions = {
          hostname: 'api.unsandbox.com',
          path: '/execute',
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${apiKey}`,
            'Content-Type': 'application/json',
            'Content-Length': Buffer.byteLength(payload)
          }
        };

        const req = https.request(options, (res) => {
          let data = '';
          res.on('data', (chunk) => data += chunk);
          res.on('end', () => {
            if (res.statusCode === 200) {
              resolve(JSON.parse(data));
            } else {
              reject(new Error(`HTTP ${res.statusCode}: ${data}`));
            }
          });
        });

        req.on('error', reject);
        req.write(payload);
        req.end();
      });

      if (result.stdout && result.stdout.includes('Hello from API')) {
        results.passTest('API call test');
      } else {
        results.failTest('API call test', `Unexpected result: ${JSON.stringify(result)}`);
      }
    } catch (e) {
      results.failTest('API call test', (e as Error).message);
    }
  }

  // Test 8: End-to-end test with fib.py
  if (!process.env.UNSANDBOX_API_KEY) {
    results.skipTest('End-to-end fib.py test', 'UNSANDBOX_API_KEY not set');
  } else if (!fs.existsSync(FIB_PY)) {
    results.skipTest('End-to-end fib.py test', `fib.py not found at ${FIB_PY}`);
  } else {
    try {
      const { stdout, stderr } = await execFileAsync(UN_SCRIPT, [FIB_PY], {
        timeout: 30000
      });

      if (stdout.includes('fib(10) = 55')) {
        results.passTest('End-to-end fib.py test');
      } else {
        results.failTest('End-to-end fib.py test',
          `Expected 'fib(10) = 55' in output, got: ${stdout.substring(0, 200)}`);
      }
    } catch (e: any) {
      if (e.killed) {
        results.failTest('End-to-end fib.py test', 'Timeout (30s)');
      } else {
        results.failTest('End-to-end fib.py test', e.message);
      }
    }
  }

  // Print summary
  console.log('\n' + '='.repeat(50));
  console.log('Test Summary:');
  console.log(`  PASSED:  ${results.passed}`);
  console.log(`  FAILED:  ${results.failed}`);
  console.log(`  SKIPPED: ${results.skipped}`);
  console.log(`  TOTAL:   ${results.passed + results.failed + results.skipped}`);
  console.log('='.repeat(50));

  // Exit with appropriate code
  process.exit(results.failed === 0 ? 0 : 1);
}

runTests();
