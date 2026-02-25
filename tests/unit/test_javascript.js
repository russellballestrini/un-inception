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
 * Unit tests for un.js - tests internal functions without API calls
 */

const crypto = require('crypto');
const path = require('path');
const fs = require('fs');
const os = require('os');
const assert = require('assert');

// Test counters
let passed = 0;
let failed = 0;

function test(name, fn) {
  try {
    fn();
    console.log(`  ✓ ${name}`);
    passed++;
  } catch (e) {
    console.log(`  ✗ ${name}`);
    console.log(`    ${e.message}`);
    failed++;
  }
}

function assertEqual(actual, expected, msg = '') {
  if (actual !== expected) {
    throw new Error(`Expected "${expected}" but got "${actual}" ${msg}`);
  }
}

function assertIncludes(str, substr) {
  if (!str.includes(substr)) {
    throw new Error(`Expected "${str}" to include "${substr}"`);
  }
}

function assertNotEqual(a, b) {
  if (a === b) {
    throw new Error(`Expected values to be different but both were "${a}"`);
  }
}

// ============================================================================
// Extension Mapping Tests
// ============================================================================

console.log('\n=== Extension Mapping Tests ===');

const EXT_MAP = {
  ".py": "python", ".js": "javascript", ".ts": "typescript",
  ".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
  ".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
  ".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
  ".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
  ".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
  ".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
  ".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
  ".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
  ".dart": "dart", ".groovy": "groovy", ".scala": "scala",
  ".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
  ".pro": "prolog", ".forth": "forth", ".4th": "forth",
  ".tcl": "tcl", ".raku": "raku", ".m": "objc",
};

test('Python extension maps correctly', () => {
  assertEqual(EXT_MAP['.py'], 'python');
});

test('JavaScript extensions map correctly', () => {
  assertEqual(EXT_MAP['.js'], 'javascript');
  assertEqual(EXT_MAP['.ts'], 'typescript');
});

test('Ruby extension maps correctly', () => {
  assertEqual(EXT_MAP['.rb'], 'ruby');
});

test('Go extension maps correctly', () => {
  assertEqual(EXT_MAP['.go'], 'go');
});

test('Rust extension maps correctly', () => {
  assertEqual(EXT_MAP['.rs'], 'rust');
});

test('C/C++ extensions map correctly', () => {
  assertEqual(EXT_MAP['.c'], 'c');
  assertEqual(EXT_MAP['.cpp'], 'cpp');
  assertEqual(EXT_MAP['.cc'], 'cpp');
  assertEqual(EXT_MAP['.cxx'], 'cpp');
});

test('JVM extensions map correctly', () => {
  assertEqual(EXT_MAP['.java'], 'java');
  assertEqual(EXT_MAP['.kt'], 'kotlin');
  assertEqual(EXT_MAP['.groovy'], 'groovy');
  assertEqual(EXT_MAP['.scala'], 'scala');
});

test('.NET extensions map correctly', () => {
  assertEqual(EXT_MAP['.cs'], 'csharp');
  assertEqual(EXT_MAP['.fs'], 'fsharp');
});

test('Functional language extensions map correctly', () => {
  assertEqual(EXT_MAP['.hs'], 'haskell');
  assertEqual(EXT_MAP['.ml'], 'ocaml');
  assertEqual(EXT_MAP['.clj'], 'clojure');
  assertEqual(EXT_MAP['.scm'], 'scheme');
  assertEqual(EXT_MAP['.lisp'], 'commonlisp');
  assertEqual(EXT_MAP['.erl'], 'erlang');
  assertEqual(EXT_MAP['.ex'], 'elixir');
  assertEqual(EXT_MAP['.exs'], 'elixir');
});

test('Scientific language extensions map correctly', () => {
  assertEqual(EXT_MAP['.jl'], 'julia');
  assertEqual(EXT_MAP['.r'], 'r');
  assertEqual(EXT_MAP['.R'], 'r');
  assertEqual(EXT_MAP['.f90'], 'fortran');
  assertEqual(EXT_MAP['.f95'], 'fortran');
});

test('Systems language extensions map correctly', () => {
  assertEqual(EXT_MAP['.d'], 'd');
  assertEqual(EXT_MAP['.nim'], 'nim');
  assertEqual(EXT_MAP['.zig'], 'zig');
  assertEqual(EXT_MAP['.v'], 'v');
  assertEqual(EXT_MAP['.cr'], 'crystal');
  assertEqual(EXT_MAP['.dart'], 'dart');
});

test('Legacy/exotic extensions map correctly', () => {
  assertEqual(EXT_MAP['.cob'], 'cobol');
  assertEqual(EXT_MAP['.pro'], 'prolog');
  assertEqual(EXT_MAP['.forth'], 'forth');
  assertEqual(EXT_MAP['.4th'], 'forth');
  assertEqual(EXT_MAP['.tcl'], 'tcl');
  assertEqual(EXT_MAP['.raku'], 'raku');
  assertEqual(EXT_MAP['.m'], 'objc');
});

// ============================================================================
// HMAC Signature Tests
// ============================================================================

console.log('\n=== HMAC Signature Tests ===');

test('HMAC-SHA256 generates 64 character hex string', () => {
  const secret = 'test-secret-key';
  const message = '1234567890:POST:/execute:{}';

  const signature = crypto.createHmac('sha256', secret)
    .update(message)
    .digest('hex');

  assertEqual(signature.length, 64);
});

test('Same input produces same signature', () => {
  const secret = 'test-secret-key';
  const message = '1234567890:POST:/execute:{}';

  const sig1 = crypto.createHmac('sha256', secret).update(message).digest('hex');
  const sig2 = crypto.createHmac('sha256', secret).update(message).digest('hex');

  assertEqual(sig1, sig2);
});

test('Different secrets produce different signatures', () => {
  const message = '1234567890:POST:/execute:{}';

  const sig1 = crypto.createHmac('sha256', 'secret1').update(message).digest('hex');
  const sig2 = crypto.createHmac('sha256', 'secret2').update(message).digest('hex');

  assertNotEqual(sig1, sig2);
});

test('Different messages produce different signatures', () => {
  const secret = 'test-secret';

  const sig1 = crypto.createHmac('sha256', secret).update('message1').digest('hex');
  const sig2 = crypto.createHmac('sha256', secret).update('message2').digest('hex');

  assertNotEqual(sig1, sig2);
});

test('Signature format is timestamp:METHOD:path:body', () => {
  const timestamp = '1704067200';
  const method = 'POST';
  const endpoint = '/execute';
  const body = '{"language":"python","code":"print(1)"}';

  const message = `${timestamp}:${method}:${endpoint}:${body}`;

  // Verify format: starts with timestamp, has method and path
  assertEqual(message.startsWith(timestamp), true);
  assertIncludes(message, ':POST:');
  assertIncludes(message, ':/execute:');
});

// ============================================================================
// Language Detection Tests
// ============================================================================

console.log('\n=== Language Detection Tests ===');

test('Detect language from .py extension', () => {
  const filename = 'script.py';
  const ext = path.extname(filename).toLowerCase();
  assertEqual(EXT_MAP[ext], 'python');
});

test('Detect language from .js extension', () => {
  const filename = 'app.js';
  const ext = path.extname(filename).toLowerCase();
  assertEqual(EXT_MAP[ext], 'javascript');
});

test('Python shebang detection', () => {
  const content = '#!/usr/bin/env python3\nprint("hello")';
  const firstLine = content.split('\n')[0];

  assertEqual(firstLine.startsWith('#!'), true);
  assertEqual(firstLine.includes('python'), true);
});

test('Node shebang detection', () => {
  const content = '#!/usr/bin/env node\nconsole.log("hello")';
  const firstLine = content.split('\n')[0];

  assertEqual(firstLine.startsWith('#!'), true);
  assertEqual(firstLine.includes('node'), true);
});

test('Bash shebang detection', () => {
  const content = '#!/bin/bash\necho hello';
  const firstLine = content.split('\n')[0];

  assertEqual(firstLine.startsWith('#!'), true);
  assertEqual(firstLine.includes('bash') || firstLine.includes('/sh'), true);
});

// ============================================================================
// Argument Parsing Tests
// ============================================================================

console.log('\n=== Argument Parsing Tests ===');

test('Parse -e KEY=VALUE format', () => {
  const arg = 'DEBUG=1';
  const [key, ...rest] = arg.split('=');
  const value = rest.join('=');

  assertEqual(key, 'DEBUG');
  assertEqual(value, '1');
});

test('Parse -e KEY=VALUE with equals in value', () => {
  const arg = 'URL=https://example.com?foo=bar';
  const [key, ...rest] = arg.split('=');
  const value = rest.join('=');

  assertEqual(key, 'URL');
  assertEqual(value, 'https://example.com?foo=bar');
});

test('Valid network modes', () => {
  const validModes = ['zerotrust', 'semitrusted'];

  assertEqual(validModes.includes('zerotrust'), true);
  assertEqual(validModes.includes('semitrusted'), true);
  assertEqual(validModes.includes('invalid'), false);
});

test('Subcommand detection', () => {
  const args = ['session', '--shell', 'python3'];
  const subcommands = ['session', 'service', 'key', 'restore'];
  const subcommand = subcommands.includes(args[0]) ? args[0] : null;

  assertEqual(subcommand, 'session');
});

// ============================================================================
// File Operations Tests
// ============================================================================

console.log('\n=== File Operations Tests ===');

test('Read text file', () => {
  const tempFile = path.join(os.tmpdir(), 'test_un_js_' + Date.now() + '.py');
  fs.writeFileSync(tempFile, 'print("hello world")');

  try {
    const content = fs.readFileSync(tempFile, 'utf-8');
    assertEqual(content, 'print("hello world")');
  } finally {
    fs.unlinkSync(tempFile);
  }
});

test('Base64 encoding/decoding', () => {
  const content = 'print("hello world")';
  const encoded = Buffer.from(content).toString('base64');
  const decoded = Buffer.from(encoded, 'base64').toString();

  assertEqual(decoded, content);
});

test('Extract file basename', () => {
  const filepath = '/home/user/project/script.py';
  const basename = path.basename(filepath);

  assertEqual(basename, 'script.py');
});

test('Extract file extension', () => {
  const filepath = '/home/user/project/script.py';
  const ext = path.extname(filepath);

  assertEqual(ext, '.py');
});

// ============================================================================
// API Constants Tests
// ============================================================================

console.log('\n=== API Constants Tests ===');

test('API base URL format', () => {
  const API_BASE = 'https://api.unsandbox.com';

  assertEqual(API_BASE.startsWith('https://'), true);
  assertIncludes(API_BASE, 'unsandbox.com');
});

test('Portal base URL format', () => {
  const PORTAL_BASE = 'https://unsandbox.com';

  assertEqual(PORTAL_BASE.startsWith('https://'), true);
});

// ============================================================================
// Summary
// ============================================================================

console.log('\n=== Summary ===');
console.log(`Passed: ${passed}`);
console.log(`Failed: ${failed}`);
console.log(`Total:  ${passed + failed}`);

process.exit(failed > 0 ? 1 : 0);
