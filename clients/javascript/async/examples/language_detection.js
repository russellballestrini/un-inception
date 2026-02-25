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
 * Language Detection example - standalone version
 *
 * Demonstrates language detection from filenames.
 * This is a pure function that maps file extensions to language identifiers.
 *
 * To run:
 *   node language_detection.js
 *
 * Expected output:
 *   Testing language detection from filenames...
 *   script.py -> python
 *   app.js -> javascript
 *   main.go -> go
 *   Cargo.rs -> rust
 *   Main.java -> java
 *   test.rb -> ruby
 *   index.ts -> typescript
 *   unknown -> null
 *   Language detection complete!
 */

// Inline language detection - same logic as SDK
function detectLanguage(filename) {
  const ext = filename.split('.').pop()?.toLowerCase();
  const extMap = {
    'py': 'python',
    'js': 'javascript',
    'ts': 'typescript',
    'go': 'go',
    'rs': 'rust',
    'java': 'java',
    'rb': 'ruby',
    'php': 'php',
    'c': 'c',
    'cpp': 'cpp',
    'cs': 'csharp',
    'sh': 'bash',
    'pl': 'perl',
    'lua': 'lua',
    'r': 'r',
    'jl': 'julia',
    'hs': 'haskell',
    'ex': 'elixir',
    'erl': 'erlang',
    'swift': 'swift',
    'kt': 'kotlin',
    'scala': 'scala',
  };
  return extMap[ext] || null;
}

const TEST_FILES = [
  'script.py',
  'app.js',
  'main.go',
  'Cargo.rs',
  'Main.java',
  'test.rb',
  'index.ts',
  'unknown',
];

function main() {
  console.log('Testing language detection from filenames...');

  for (const filename of TEST_FILES) {
    const language = detectLanguage(filename);
    console.log(`${filename} -> ${language}`);
  }

  console.log('Language detection complete!');
  return 0;
}

process.exit(main());
