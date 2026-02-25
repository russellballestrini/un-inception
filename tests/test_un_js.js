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
 * Test suite for UN CLI JavaScript implementation (un.js)
 * Tests extension detection, API calls, and end-to-end functionality
 */

const fs = require('fs');
const path = require('path');
const { execFile } = require('child_process');
const { promisify } = require('util');

const execFileAsync = promisify(execFile);

// Test configuration
const UN_SCRIPT = path.join(__dirname, '..', 'un.js');
const FIB_PY = path.join(__dirname, '..', '..', 'test', 'fib.py');

class TestResults {
  constructor() {
    this.passed = 0;
    this.failed = 0;
    this.skipped = 0;
  }

  passTest(name) {
    console.log(`PASS: ${name}`);
    this.passed++;
  }

  failTest(name, error) {
    console.log(`FAIL: ${name} - ${error}`);
    this.failed++;
  }

  skipTest(name, reason) {
    console.log(`SKIP: ${name} - ${reason}`);
    this.skipped++;
  }
}

const results = new TestResults();

// Load the extension map from un.js
const EXTENSION_MAP = {
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

function detectLanguage(filename) {
  const ext = path.extname(filename).toLowerCase();
  return EXTENSION_MAP[ext];
}

async function runTests() {
  // Test 1: Extension detection for Python
  try {
    const lang = detectLanguage('test.py');
    if (lang === 'python') {
      results.passTest('Extension detection: .py -> python');
    } else {
      results.failTest('Extension detection: .py -> python', `Got ${lang}`);
    }
  } catch (e) {
    results.failTest('Extension detection: .py -> python', e.message);
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
    results.failTest('Extension detection: .js -> javascript', e.message);
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
    results.failTest('Extension detection: .rb -> ruby', e.message);
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
    results.failTest('Extension detection: .go -> go', e.message);
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
    results.failTest('Extension detection: .rs -> rust', e.message);
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
    results.failTest('Extension detection: .unknown -> undefined', e.message);
  }

  // Test 7: API call test (requires UNSANDBOX auth)
  const hasHMAC = process.env.UNSANDBOX_PUBLIC_KEY && process.env.UNSANDBOX_SECRET_KEY;
  const hasLegacy = process.env.UNSANDBOX_API_KEY;
  if (!hasHMAC && !hasLegacy) {
    results.skipTest('API call test', 'UNSANDBOX authentication not configured');
  } else {
    try {
      const https = require('https');
      const crypto = require('crypto');

      // Use HMAC auth if available, otherwise fall back to legacy
      const publicKey = process.env.UNSANDBOX_PUBLIC_KEY || process.env.UNSANDBOX_API_KEY;
      const secretKey = process.env.UNSANDBOX_SECRET_KEY || process.env.UNSANDBOX_API_KEY;

      const payload = JSON.stringify({
        language: 'python',
        code: 'print("Hello from API")'
      });

      const timestamp = Math.floor(Date.now() / 1000).toString();
      const signatureInput = `${timestamp}:POST:/execute:${payload}`;
      const signature = crypto.createHmac('sha256', secretKey)
        .update(signatureInput)
        .digest('hex');

      const result = await new Promise((resolve, reject) => {
        const options = {
          hostname: 'api.unsandbox.com',
          path: '/execute',
          method: 'POST',
          headers: {
            'Authorization': `Bearer ${publicKey}`,
            'X-Timestamp': timestamp,
            'X-Signature': signature,
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
      results.failTest('API call test', e.message);
    }
  }

  // Test 8: End-to-end test with fib.py
  const hasHMAC2 = process.env.UNSANDBOX_PUBLIC_KEY && process.env.UNSANDBOX_SECRET_KEY;
  const hasLegacy2 = process.env.UNSANDBOX_API_KEY;
  if (!hasHMAC2 && !hasLegacy2) {
    results.skipTest('End-to-end fib.py test', 'UNSANDBOX authentication not configured');
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
    } catch (e) {
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
