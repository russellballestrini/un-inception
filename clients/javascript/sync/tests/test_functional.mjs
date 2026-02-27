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
 * UN JavaScript SDK - Functional Tests
 *
 * Tests library functions against real API.
 * Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
 *
 * Usage:
 *   node clients/javascript/sync/tests/test_functional.mjs
 */

import {
  executeCode,
  getLanguages,
  listSessions,
  createSession,
  deleteSession,
  listServices,
  listSnapshots,
  listImages,
  validateKeys,
  healthCheck,
} from '../src/un.js';

const GREEN = '\x1b[32m';
const RED = '\x1b[31m';
const BLUE = '\x1b[34m';
const YELLOW = '\x1b[33m';
const NC = '\x1b[0m';

let passed = 0;
let failed = 0;

function check(condition, msg) {
  if (condition) {
    console.log(`  ${GREEN}✓${NC} ${msg}`);
    passed++;
  } else {
    console.log(`  ${RED}✗${NC} ${msg}`);
    failed++;
  }
}

async function testHealthCheck() {
  console.log('\nTesting healthCheck()...');
  const result = await healthCheck();
  check(typeof result === 'boolean', 'healthCheck returns boolean');
}

async function testValidateKeys() {
  console.log('\nTesting validateKeys()...');
  const info = await validateKeys();
  check(info != null, 'validateKeys returns non-null');
  check(info.valid === true, 'keys are valid');
  if (info.tier) console.log(`    tier: ${info.tier}`);
}

async function testGetLanguages() {
  console.log('\nTesting getLanguages()...');
  const langs = await getLanguages();
  check(Array.isArray(langs), 'getLanguages returns array');
  check(langs.length > 0, 'at least one language returned');
  check(langs.includes('python'), 'python is in languages list');
  console.log(`    Found ${langs.length} languages`);
}

async function testExecute() {
  console.log('\nTesting executeCode()...');
  const result = await executeCode('python', "print('hello from JS SDK')");
  check(result != null, 'execute returns non-null');
  check(result.stdout && result.stdout.includes('hello from JS SDK'), 'stdout contains expected output');
  check(result.exit_code === 0, 'exit code is 0');
}

async function testExecuteError() {
  console.log('\nTesting executeCode() with error...');
  const result = await executeCode('python', 'import sys; sys.exit(1)');
  check(result != null, 'execute returns non-null');
  check(result.exit_code === 1, 'exit code is 1');
}

async function testSessionList() {
  console.log('\nTesting listSessions()...');
  const sessions = await listSessions();
  check(Array.isArray(sessions), 'listSessions returns array');
  console.log(`    Found ${sessions.length} sessions`);
}

async function testSessionLifecycle() {
  console.log('\nTesting session lifecycle (create, destroy)...');

  const session = await createSession('python');
  check(session != null, 'createSession returns non-null');
  const sessionId = session.session_id || session.id;
  check(sessionId != null, 'session has id');
  console.log(`    session_id: ${sessionId}`);

  if (sessionId) {
    const destroyed = await deleteSession(sessionId);
    check(destroyed != null, 'deleteSession returns non-null');
  }
}

async function testServiceList() {
  console.log('\nTesting listServices()...');
  const services = await listServices();
  check(Array.isArray(services), 'listServices returns array');
  console.log(`    Found ${services.length} services`);
}

async function testSnapshotList() {
  console.log('\nTesting listSnapshots()...');
  const snapshots = await listSnapshots();
  check(Array.isArray(snapshots), 'listSnapshots returns array');
  console.log(`    Found ${snapshots.length} snapshots`);
}

async function testImageList() {
  console.log('\nTesting listImages()...');
  const images = await listImages();
  check(Array.isArray(images), 'listImages returns array');
  console.log(`    Found ${images.length} images`);
}

// Main
async function main() {
  console.log('=====================================');
  console.log('UN JavaScript SDK - Functional Tests');
  console.log('Testing against real API');
  console.log('=====================================');

  if (!process.env.UNSANDBOX_PUBLIC_KEY || !process.env.UNSANDBOX_SECRET_KEY) {
    console.log(`\n${YELLOW}SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set${NC}`);
    process.exit(0);
  }

  const tests = [
    testHealthCheck,
    testValidateKeys,
    testGetLanguages,
    testExecute,
    testExecuteError,
    testSessionList,
    testSessionLifecycle,
    testServiceList,
    testSnapshotList,
    testImageList,
  ];

  for (const test of tests) {
    try {
      await test();
    } catch (err) {
      console.log(`  ${RED}✗${NC} ${test.name}: ${err.message}`);
      failed++;
    }
  }

  console.log('\n=====================================');
  console.log('Test Summary');
  console.log('=====================================');
  console.log(`Passed: ${GREEN}${passed}${NC}`);
  console.log(`Failed: ${RED}${failed}${NC}`);
  console.log('=====================================');

  process.exit(failed > 0 ? 1 : 0);
}

main();
