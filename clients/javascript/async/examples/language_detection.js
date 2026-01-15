#!/usr/bin/env node
/**
 * Language Detection example for unsandbox JavaScript SDK
 *
 * Demonstrates automatic language detection from filenames.
 * This is a purely local operation that doesn't require API credentials.
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

import { detectLanguage } from '../src/un_async.js';

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
