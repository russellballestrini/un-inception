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
 * Global --os flag (tickets #123, #124, FreeBSD jails backend).
 *
 * "os" goes in POST /execute, /sessions and /services bodies only when a
 * target OS is given. Requests go to a local HTTP server on 127.0.0.1 that
 * records each body: no network, no credentials (HOME is a temp dir).
 */

import { jest } from '@jest/globals';
import http from 'http';
import fs from 'fs';
import os from 'os';
import path from 'path';
import { execFile } from 'child_process';
import { fileURLToPath } from 'url';

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), '..', 'src', 'un.js');

// Each CLI run is a fresh node process; a test that runs two of them can exceed
// jest's 5s default, and a timed-out test would leak requests into the next.
jest.setTimeout(60000);

let server;
let baseUrl;
let home;
let requests = [];
let un;

beforeAll(async () => {
  server = http.createServer((req, res) => {
    let body = '';
    req.on('data', (c) => { body += c; });
    req.on('end', () => {
      requests.push({ method: req.method, path: req.url, body });
      let reply = {};
      if (req.url.startsWith('/execute')) reply = { stdout: '', stderr: '', exit_code: 0 };
      else if (req.url.startsWith('/sessions')) reply = { session_id: 's1' };
      else if (req.url.startsWith('/services')) reply = { service_id: 'svc1' };
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify(reply));
    });
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  baseUrl = `http://127.0.0.1:${server.address().port}`;
  home = fs.mkdtempSync(path.join(os.tmpdir(), 'un-os-'));
  // API_BASE is read at module load; set our mock before importing.
  process.env.UNSANDBOX_API_URL = baseUrl;
  process.env.UNSANDBOX_PUBLIC_KEY = 'unsb-pk-test-0000-0000-0000';
  process.env.UNSANDBOX_SECRET_KEY = 'unsb-sk-test0-00000-00000-00000';
  un = await import('../src/un.js');
});

afterAll(async () => {
  await new Promise((r) => server.close(r));
  fs.rmSync(home, { recursive: true, force: true });
});

beforeEach(() => {
  requests = [];
});

function runCli(args) {
  return new Promise((resolve) => {
    execFile(process.execPath, [SRC, ...args], {
      env: {
        PATH: process.env.PATH,
        HOME: home,
        UNSANDBOX_API_URL: baseUrl,
        UNSANDBOX_PUBLIC_KEY: 'unsb-pk-test-0000-0000-0000',
        UNSANDBOX_SECRET_KEY: 'unsb-sk-test0-00000-00000-00000',
      },
      timeout: 20000,
    }, (err, stdout, stderr) => {
      resolve({ code: err ? err.code : 0, stdout, stderr });
    });
  });
}

function bodyOf(p) {
  const r = requests.find((x) => x.method === 'POST' && x.path.split('?')[0] === p);
  if (!r) throw new Error(`no POST to ${p}: ${JSON.stringify(requests)}`);
  return JSON.parse(r.body);
}

describe('--os CLI flag', () => {
  test('execute --os freebsd sends os', async () => {
    await runCli(['--os', 'freebsd', '-s', 'bash', 'echo hi']);
    expect(bodyOf('/execute').os).toBe('freebsd');
  });

  test('execute without --os sends no os key', async () => {
    await runCli(['-s', 'bash', 'echo hi']);
    expect(bodyOf('/execute')).not.toHaveProperty('os');
  });

  test('--os is case-insensitive and accepts --os=VALUE', async () => {
    await runCli(['--os=FreeBSD', '-s', 'bash', 'echo hi']);
    expect(bodyOf('/execute').os).toBe('freebsd');
  });

  test('session and service carry os', async () => {
    await runCli(['--os', 'freebsd', 'session']);
    await runCli(['--os', 'freebsd', 'service', '--name', 'ostest', '--ports', '80', '--bootstrap', 'true']);
    expect(bodyOf('/sessions').os).toBe('freebsd');
    expect(bodyOf('/services').os).toBe('freebsd');
  });

  test('session and service without --os send no os key', async () => {
    await runCli(['session']);
    await runCli(['service', '--name', 'ostest', '--ports', '80', '--bootstrap', 'true']);
    expect(bodyOf('/sessions')).not.toHaveProperty('os');
    expect(bodyOf('/services')).not.toHaveProperty('os');
  });

  test.each([
    [['--os', 'openbsd', '-s', 'bash', 'echo hi'], 'openbsd'],
    [['--os', '', 'session'], '--os'],
    [['--os'], '--os'],
  ])('invalid %j exits non-zero and sends nothing', async (args, word) => {
    const r = await runCli(args);
    expect(r.code).not.toBe(0);
    expect(r.stderr).toContain(word);
    expect(requests).toEqual([]);
  });
});

describe('os library option', () => {
  test('executeCode opts.os', async () => {
    await un.executeCode('bash', 'echo hi', undefined, undefined, { os: 'FreeBSD' });
    expect(bodyOf('/execute').os).toBe('freebsd');
  });

  test('createSession and createService opts.os', async () => {
    await un.createSession('bash', { os: 'freebsd' });
    await un.createService('t', [80], 'true', { os: 'linux' });
    expect(bodyOf('/sessions').os).toBe('freebsd');
    expect(bodyOf('/services').os).toBe('linux');
  });

  test('no opts.os leaves body without os', async () => {
    await un.executeAsync('bash', 'echo hi');
    expect(bodyOf('/execute')).not.toHaveProperty('os');
  });

  test('invalid opts.os rejects before any request', async () => {
    await expect(un.createSession('bash', { os: 'openbsd' })).rejects.toThrow('openbsd');
    expect(requests).toEqual([]);
  });
});
