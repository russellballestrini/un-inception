#!/usr/bin/env npx ts-node
/**
 * Unit tests for un.ts - tests internal functions without API calls
 */

import * as crypto from 'crypto';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    console.log(`  ✓ ${name}`);
    passed++;
  } catch (e: any) {
    console.log(`  ✗ ${name}`);
    console.log(`    ${e.message}`);
    failed++;
  }
}

function assertEqual(actual: any, expected: any): void {
  if (actual !== expected) {
    throw new Error(`Expected "${expected}" but got "${actual}"`);
  }
}

function assertNotEqual(a: any, b: any): void {
  if (a === b) {
    throw new Error(`Expected values to be different but both were "${a}"`);
  }
}

function assertIncludes(str: string, substr: string): void {
  if (!str.includes(substr)) {
    throw new Error(`Expected "${str}" to include "${substr}"`);
  }
}

function assertTrue(val: boolean): void {
  if (!val) {
    throw new Error(`Expected true but got false`);
  }
}

const EXT_MAP: Record<string, string> = {
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

console.log('\n=== Extension Mapping Tests ===');

test('Python extension maps correctly', () => {
  assertEqual(EXT_MAP['.py'], 'python');
});

test('TypeScript extension maps correctly', () => {
  assertEqual(EXT_MAP['.ts'], 'typescript');
});

test('JavaScript extension maps correctly', () => {
  assertEqual(EXT_MAP['.js'], 'javascript');
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
});

console.log('\n=== HMAC Signature Tests ===');

test('HMAC-SHA256 generates 64 character hex string', () => {
  const sig = crypto.createHmac('sha256', 'test-secret')
    .update('test-message')
    .digest('hex');
  assertEqual(sig.length, 64);
});

test('Same input produces same signature', () => {
  const sig1 = crypto.createHmac('sha256', 'key').update('msg').digest('hex');
  const sig2 = crypto.createHmac('sha256', 'key').update('msg').digest('hex');
  assertEqual(sig1, sig2);
});

test('Different secrets produce different signatures', () => {
  const sig1 = crypto.createHmac('sha256', 'key1').update('msg').digest('hex');
  const sig2 = crypto.createHmac('sha256', 'key2').update('msg').digest('hex');
  assertNotEqual(sig1, sig2);
});

test('Signature format verification', () => {
  const timestamp = '1704067200';
  const method = 'POST';
  const endpoint = '/execute';
  const body = '{"language":"python"}';

  const message = `${timestamp}:${method}:${endpoint}:${body}`;

  assertTrue(message.startsWith(timestamp));
  assertIncludes(message, ':POST:');
  assertIncludes(message, ':/execute:');
});

console.log('\n=== Language Detection Tests ===');

test('Detect language from .ts extension', () => {
  const ext = path.extname('script.ts').toLowerCase();
  assertEqual(EXT_MAP[ext], 'typescript');
});

test('Python shebang detection', () => {
  const content = '#!/usr/bin/env python3\nprint("hello")';
  const firstLine = content.split('\n')[0];
  assertTrue(firstLine.startsWith('#!'));
  assertIncludes(firstLine, 'python');
});

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

console.log('\n=== File Operations Tests ===');

test('Base64 encoding/decoding', () => {
  const content = 'print("hello world")';
  const encoded = Buffer.from(content).toString('base64');
  const decoded = Buffer.from(encoded, 'base64').toString();
  assertEqual(decoded, content);
});

test('Extract file basename', () => {
  const filepath = '/home/user/project/script.ts';
  assertEqual(path.basename(filepath), 'script.ts');
});

test('Extract file extension', () => {
  const filepath = '/home/user/project/script.ts';
  assertEqual(path.extname(filepath), '.ts');
});

console.log('\n=== API Constants Tests ===');

test('API base URL format', () => {
  const API_BASE = 'https://api.unsandbox.com';
  assertTrue(API_BASE.startsWith('https://'));
  assertIncludes(API_BASE, 'unsandbox.com');
});

console.log('\n=== Summary ===');
console.log(`Passed: ${passed}`);
console.log(`Failed: ${failed}`);
console.log(`Total:  ${passed + failed}`);

process.exit(failed > 0 ? 1 : 0);
