#!/usr/bin/env node
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
