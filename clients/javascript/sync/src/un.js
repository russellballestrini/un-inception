/**
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com JavaScript SDK (Synchronous/Async)
 *
 * Library Usage:
 *   const {
 *     // Code execution
 *     executeCode, executeAsync, getJob, waitForJob, cancelJob, listJobs,
 *     getLanguages, detectLanguage,
 *     // Session management
 *     listSessions, getSession, createSession, deleteSession,
 *     freezeSession, unfreezeSession, boostSession, unboostSession, shellSession,
 *     // Service management
 *     listServices, createService, getService, updateService, deleteService,
 *     freezeService, unfreezeService, lockService, unlockService,
 *     getServiceLogs, getServiceEnv, setServiceEnv, deleteServiceEnv,
 *     exportServiceEnv, redeployService, executeInService,
 *     // Snapshot management
 *     sessionSnapshot, serviceSnapshot, listSnapshots, restoreSnapshot,
 *     deleteSnapshot, lockSnapshot, unlockSnapshot, cloneSnapshot,
 *     // Key validation
 *     validateKeys,
 *   } = require('./un.js');
 *
 *   // Execute code asynchronously (returns Promise)
 *   const result = await executeCode('python', 'print("hello")', publicKey, secretKey);
 *   const jobId = await executeAsync('javascript', 'console.log("hello")', publicKey, secretKey);
 *   const result = await waitForJob(jobId, publicKey, secretKey);
 *
 * Authentication Priority (4-tier):
 *   1. Function arguments (publicKey, secretKey)
 *   2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
 *   3. ~/.unsandbox/accounts.csv (if in Node.js)
 *   4. ./accounts.csv (if in Node.js)
 *
 * Request Authentication (HMAC-SHA256):
 *   Authorization: Bearer <publicKey>
 *   X-Timestamp: <unixSeconds>
 *   X-Signature: HMAC-SHA256(secretKey, "timestamp:METHOD:path:body")
 *
 * Languages Cache:
 *   - Cached in ~/.unsandbox/languages.json (Node.js only)
 *   - TTL: 1 hour
 *   - Updated on successful API calls
 */

const crypto = require('crypto');
const fs = require('fs');
const path = require('path');
const https = require('https');

const API_BASE = 'https://api.unsandbox.com';
const POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000];
const LANGUAGES_CACHE_TTL = 3600; // 1 hour

class CredentialsError extends Error {
  constructor(message) {
    super(message);
    this.name = 'CredentialsError';
  }
}

/**
 * Get ~/.unsandbox directory path, creating if necessary.
 */
function getUnsandboxDir() {
  const home = process.env.HOME || process.env.USERPROFILE;
  if (!home) {
    throw new Error('Could not determine home directory');
  }
  const dir = path.join(home, '.unsandbox');
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true, mode: 0o700 });
  }
  return dir;
}

/**
 * Load credentials from CSV file (public_key,secret_key per line).
 */
function loadCredentialsFromCsv(csvPath, accountIndex = 0) {
  if (!fs.existsSync(csvPath)) {
    return null;
  }

  try {
    const lines = fs.readFileSync(csvPath, 'utf-8').split('\n');
    let currentIndex = 0;
    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#')) {
        continue;
      }
      if (currentIndex === accountIndex) {
        const parts = trimmed.split(',');
        if (parts.length >= 2) {
          return [parts[0].trim(), parts[1].trim()];
        }
      }
      currentIndex++;
    }
  } catch (e) {
    // Ignore read errors
  }

  return null;
}

/**
 * Resolve credentials from 4-tier priority system.
 *
 * Priority:
 *   1. Function arguments
 *   2. Environment variables
 *   3. ~/.unsandbox/accounts.csv
 *   4. ./accounts.csv
 */
function resolveCredentials(publicKey, secretKey, accountIndex) {
  // Tier 1: Function arguments
  if (publicKey && secretKey) {
    return [publicKey, secretKey];
  }

  // Tier 2: Environment variables
  const envPk = process.env.UNSANDBOX_PUBLIC_KEY;
  const envSk = process.env.UNSANDBOX_SECRET_KEY;
  if (envPk && envSk) {
    return [envPk, envSk];
  }

  // Determine account index
  if (accountIndex === undefined) {
    accountIndex = parseInt(process.env.UNSANDBOX_ACCOUNT || '0', 10);
  }

  // Tier 3: ~/.unsandbox/accounts.csv
  try {
    const unsandboxDir = getUnsandboxDir();
    const creds = loadCredentialsFromCsv(path.join(unsandboxDir, 'accounts.csv'), accountIndex);
    if (creds) {
      return creds;
    }
  } catch (e) {
    // Continue to next tier
  }

  // Tier 4: ./accounts.csv
  const creds = loadCredentialsFromCsv('accounts.csv', accountIndex);
  if (creds) {
    return creds;
  }

  throw new CredentialsError(
    'No credentials found. Please provide via:\n' +
    '  1. Function arguments (publicKey, secretKey)\n' +
    '  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n' +
    '  3. ~/.unsandbox/accounts.csv\n' +
    '  4. ./accounts.csv'
  );
}

/**
 * Sign a request using HMAC-SHA256.
 *
 * Message format: "timestamp:METHOD:path:body"
 * Returns: 64-character hex string
 */
function signRequest(secretKey, timestamp, method, path, body) {
  const bodyStr = body || '';
  const message = `${timestamp}:${method}:${path}:${bodyStr}`;
  return crypto
    .createHmac('sha256', secretKey)
    .update(message)
    .digest('hex');
}

/**
 * Make an authenticated HTTP request to the API.
 *
 * Returns: Promise<Object> (parsed JSON response)
 * Throws: Error on network errors or non-JSON response
 */
function makeRequest(method, path, publicKey, secretKey, data) {
  return new Promise((resolve, reject) => {
    const url = new URL(API_BASE + path);
    const timestamp = Math.floor(Date.now() / 1000);
    const body = data ? JSON.stringify(data) : '';

    const signature = signRequest(secretKey, timestamp, method, path, body || null);

    const options = {
      hostname: url.hostname,
      port: url.port,
      path: url.pathname + url.search,
      method: method,
      headers: {
        'Authorization': `Bearer ${publicKey}`,
        'X-Timestamp': timestamp.toString(),
        'X-Signature': signature,
        'Content-Type': 'application/json',
        'User-Agent': 'un-js/2.0',
      },
      timeout: 120000, // 120 seconds
    };

    // Set Content-Length for methods with body
    if (['POST', 'PUT', 'PATCH', 'DELETE'].includes(method) && body) {
      options.headers['Content-Length'] = Buffer.byteLength(body);
    }

    const req = https.request(options, (res) => {
      let data = '';

      res.on('data', (chunk) => {
        data += chunk;
      });

      res.on('end', () => {
        if (res.statusCode < 200 || res.statusCode >= 300) {
          reject(new Error(`HTTP ${res.statusCode}: ${data}`));
          return;
        }

        try {
          resolve(JSON.parse(data));
        } catch (e) {
          reject(new Error(`Failed to parse response: ${e.message}`));
        }
      });
    });

    req.on('error', reject);
    req.on('timeout', () => {
      req.destroy();
      reject(new Error('Request timeout'));
    });

    // Write body for methods that support it
    if (['POST', 'PUT', 'PATCH', 'DELETE'].includes(method) && body) {
      req.write(body);
    }

    req.end();
  });
}

/**
 * Get path to languages cache file.
 */
function getLanguagesCachePath() {
  return path.join(getUnsandboxDir(), 'languages.json');
}

/**
 * Load languages from cache if valid (< 1 hour old).
 */
function loadLanguagesCache() {
  try {
    const cachePath = getLanguagesCachePath();
    if (!fs.existsSync(cachePath)) {
      return null;
    }

    const stat = fs.statSync(cachePath);
    const ageSeconds = (Date.now() - stat.mtimeMs) / 1000;
    if (ageSeconds >= LANGUAGES_CACHE_TTL) {
      return null;
    }

    const data = JSON.parse(fs.readFileSync(cachePath, 'utf-8'));
    return data.languages || null;
  } catch (e) {
    return null;
  }
}

/**
 * Save languages to cache.
 */
function saveLanguagesCache(languages) {
  try {
    const cachePath = getLanguagesCachePath();
    const data = {
      languages,
      timestamp: Math.floor(Date.now() / 1000),
    };
    fs.writeFileSync(cachePath, JSON.stringify(data, null, 2), 'utf-8');
  } catch (e) {
    // Cache failures are non-fatal
  }
}

/**
 * Execute code synchronously (awaits until completion).
 *
 * Args:
 *   language: Programming language (e.g., "python", "javascript", "go")
 *   code: Source code to execute
 *   publicKey: Optional API key (uses credentials resolution if not provided)
 *   secretKey: Optional API secret (uses credentials resolution if not provided)
 *
 * Returns: Promise<Object> with stdout, stderr, exit code, etc.
 */
async function executeCode(language, code, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('POST', '/execute', publicKey, secretKey, {
    language,
    code,
  });

  // If we got a job_id, poll until completion
  const jobId = response.job_id;
  const status = response.status;

  if (jobId && ['pending', 'running'].includes(status)) {
    return waitForJob(jobId, publicKey, secretKey);
  }

  return response;
}

/**
 * Execute code asynchronously (returns immediately with job_id).
 *
 * Returns: Promise<string> (job ID)
 */
async function executeAsync(language, code, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('POST', '/execute', publicKey, secretKey, {
    language,
    code,
  });
  return response.job_id;
}

/**
 * Get current status/result of a job (single poll, no waiting).
 *
 * Returns: Promise<Object> (job response)
 */
async function getJob(jobId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('GET', `/jobs/${jobId}`, publicKey, secretKey);
}

/**
 * Wait for job completion with exponential backoff polling.
 *
 * Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
 * Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+
 *
 * Returns: Promise<Object> (final job result when status is terminal)
 */
async function waitForJob(jobId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  let pollCount = 0;

  while (true) {
    // Sleep before polling
    const delayIdx = Math.min(pollCount, POLL_DELAYS_MS.length - 1);
    await new Promise((resolve) => setTimeout(resolve, POLL_DELAYS_MS[delayIdx]));
    pollCount++;

    const response = await getJob(jobId, publicKey, secretKey);
    const status = response.status;

    if (['completed', 'failed', 'timeout', 'cancelled'].includes(status)) {
      return response;
    }

    // Still running, continue polling
  }
}

/**
 * Cancel a running job.
 *
 * Returns: Promise<Object> (cancellation confirmation)
 */
async function cancelJob(jobId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('DELETE', `/jobs/${jobId}`, publicKey, secretKey);
}

/**
 * List all jobs for the authenticated account.
 *
 * Returns: Promise<Array> (list of job dicts)
 */
async function listJobs(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/jobs', publicKey, secretKey);
  return response.jobs || [];
}

/**
 * Get list of supported programming languages.
 *
 * Results are cached for 1 hour in ~/.unsandbox/languages.json
 *
 * Returns: Promise<Array> (list of language identifiers)
 */
async function getLanguages(publicKey, secretKey) {
  // Try cache first
  const cached = loadLanguagesCache();
  if (cached) {
    return cached;
  }

  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/languages', publicKey, secretKey);
  const languages = response.languages || [];

  // Cache the result
  saveLanguagesCache(languages);
  return languages;
}

/**
 * Language detection mapping (file extension -> language).
 */
const LANGUAGE_MAP = {
  py: 'python',
  js: 'javascript',
  ts: 'typescript',
  rb: 'ruby',
  php: 'php',
  pl: 'perl',
  sh: 'bash',
  r: 'r',
  lua: 'lua',
  go: 'go',
  rs: 'rust',
  c: 'c',
  cpp: 'cpp',
  cc: 'cpp',
  cxx: 'cpp',
  java: 'java',
  kt: 'kotlin',
  m: 'objc',
  cs: 'csharp',
  fs: 'fsharp',
  hs: 'haskell',
  ml: 'ocaml',
  clj: 'clojure',
  scm: 'scheme',
  ss: 'scheme',
  erl: 'erlang',
  ex: 'elixir',
  exs: 'elixir',
  jl: 'julia',
  d: 'd',
  nim: 'nim',
  zig: 'zig',
  v: 'v',
  cr: 'crystal',
  dart: 'dart',
  groovy: 'groovy',
  f90: 'fortran',
  f95: 'fortran',
  lisp: 'commonlisp',
  lsp: 'commonlisp',
  cob: 'cobol',
  tcl: 'tcl',
  raku: 'raku',
  pro: 'prolog',
  p: 'prolog',
  '4th': 'forth',
  forth: 'forth',
  fth: 'forth',
};

/**
 * Detect programming language from filename extension.
 *
 * Args:
 *   filename: Filename to detect language from (e.g., "script.py")
 *
 * Returns:
 *   Language identifier (e.g., "python") or null if unknown
 *
 * Examples:
 *   detectLanguage("hello.py")   // -> "python"
 *   detectLanguage("script.js")  // -> "javascript"
 *   detectLanguage("main.go")    // -> "go"
 *   detectLanguage("unknown")    // -> null
 */
function detectLanguage(filename) {
  if (!filename || !filename.includes('.')) {
    return null;
  }

  const ext = filename.split('.').pop().toLowerCase();
  return LANGUAGE_MAP[ext] || null;
}

/**
 * Create a snapshot of a session (NEW).
 *
 * Returns: Promise<string> (snapshot ID)
 */
async function sessionSnapshot(sessionId, publicKey, secretKey, name, hot = false) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {
    session_id: sessionId,
    hot,
  };
  if (name) {
    data.name = name;
  }

  const response = await makeRequest('POST', '/snapshots', publicKey, secretKey, data);
  return response.snapshot_id;
}

/**
 * Create a snapshot of a service (NEW).
 *
 * Returns: Promise<string> (snapshot ID)
 */
async function serviceSnapshot(serviceId, publicKey, secretKey, name, hot = false) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {
    service_id: serviceId,
    hot,
  };
  if (name) {
    data.name = name;
  }

  const response = await makeRequest('POST', '/snapshots', publicKey, secretKey, data);
  return response.snapshot_id;
}

/**
 * List all snapshots (NEW).
 *
 * Returns: Promise<Array> (list of snapshot dicts)
 */
async function listSnapshots(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/snapshots', publicKey, secretKey);
  return response.snapshots || [];
}

/**
 * Restore a snapshot (NEW).
 *
 * Returns: Promise<Object> (response with restored resource info)
 */
async function restoreSnapshot(snapshotId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/snapshots/${snapshotId}/restore`, publicKey, secretKey, {});
}

/**
 * Delete a snapshot (NEW).
 *
 * Returns: Promise<Object> (deletion confirmation)
 */
async function deleteSnapshot(snapshotId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('DELETE', `/snapshots/${snapshotId}`, publicKey, secretKey);
}

// ============================================================================
// Session Management Functions
// ============================================================================

/**
 * List all active sessions.
 *
 * Returns: Promise<Array> (list of session objects)
 */
async function listSessions(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/sessions', publicKey, secretKey);
  return response.sessions || [];
}

/**
 * Get details of a specific session.
 *
 * Args:
 *   sessionId: Session ID to retrieve
 *
 * Returns: Promise<Object> (session details)
 */
async function getSession(sessionId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('GET', `/sessions/${sessionId}`, publicKey, secretKey);
}

/**
 * Create a new interactive session.
 *
 * Args:
 *   language: Optional programming language/shell (default: "bash")
 *   opts: Optional settings:
 *     - networkMode: "zerotrust" (default) or "semitrusted"
 *     - shell: Shell to use (e.g., "python3", "bash")
 *     - multiplexer: "tmux", "screen", or null
 *     - vcpu: Number of vCPUs (1-8)
 *     - ttl: Time-to-live in seconds
 *
 * Returns: Promise<Object> (session info with session_id, container_name)
 */
async function createSession(language, opts = {}, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {
    network_mode: opts.networkMode || 'zerotrust',
    ttl: opts.ttl || 3600,
  };
  if (language) data.shell = language;
  if (opts.shell) data.shell = opts.shell;
  if (opts.multiplexer) data.multiplexer = opts.multiplexer;
  if (opts.vcpu && opts.vcpu > 1) data.vcpu = opts.vcpu;

  return makeRequest('POST', '/sessions', publicKey, secretKey, data);
}

/**
 * Delete/terminate a session.
 *
 * Args:
 *   sessionId: Session ID to terminate
 *
 * Returns: Promise<Object> (deletion confirmation)
 */
async function deleteSession(sessionId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('DELETE', `/sessions/${sessionId}`, publicKey, secretKey);
}

/**
 * Freeze a session (pause execution, preserve state).
 *
 * Args:
 *   sessionId: Session ID to freeze
 *
 * Returns: Promise<Object> (freeze confirmation)
 */
async function freezeSession(sessionId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/sessions/${sessionId}/freeze`, publicKey, secretKey, {});
}

/**
 * Unfreeze a session (resume execution).
 *
 * Args:
 *   sessionId: Session ID to unfreeze
 *
 * Returns: Promise<Object> (unfreeze confirmation)
 */
async function unfreezeSession(sessionId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/sessions/${sessionId}/unfreeze`, publicKey, secretKey, {});
}

/**
 * Boost a session's resources (increase vCPU, memory).
 *
 * Args:
 *   sessionId: Session ID to boost
 *   vcpu: Number of vCPUs (default: 2)
 *
 * Returns: Promise<Object> (boost confirmation)
 */
async function boostSession(sessionId, vcpu = 2, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/sessions/${sessionId}/boost`, publicKey, secretKey, { vcpu });
}

/**
 * Unboost a session (return to base resources).
 *
 * Args:
 *   sessionId: Session ID to unboost
 *
 * Returns: Promise<Object> (unboost confirmation)
 */
async function unboostSession(sessionId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/sessions/${sessionId}/unboost`, publicKey, secretKey, {});
}

/**
 * Execute a shell command in a session.
 *
 * Note: This initiates a WebSocket connection for interactive shell.
 * For simple command execution, this sends the command via the shell endpoint.
 *
 * Args:
 *   sessionId: Session ID
 *   command: Command to execute
 *
 * Returns: Promise<Object> (command result)
 */
async function shellSession(sessionId, command, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/sessions/${sessionId}/shell`, publicKey, secretKey, { command });
}

// ============================================================================
// Service Management Functions
// ============================================================================

/**
 * List all services.
 *
 * Returns: Promise<Array> (list of service objects)
 */
async function listServices(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/services', publicKey, secretKey);
  return response.services || [];
}

/**
 * Create a new service (persistent container).
 *
 * Args:
 *   name: Service name
 *   ports: Array of port numbers to expose (e.g., [80, 443])
 *   bootstrap: Bootstrap script content or URL
 *   opts: Optional settings:
 *     - networkMode: "zerotrust" or "semitrusted"
 *     - vcpu: Number of vCPUs (1-8)
 *     - domains: Array of custom domains
 *     - serviceType: Service type for SRV records (minecraft, mumble, etc.)
 *
 * Returns: Promise<Object> (service info with service_id)
 */
async function createService(name, ports, bootstrap, opts = {}, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {};
  if (name) data.name = name;
  if (ports && ports.length > 0) data.ports = ports;
  if (bootstrap) {
    // If bootstrap starts with http, treat as URL, otherwise as content
    if (bootstrap.startsWith('http://') || bootstrap.startsWith('https://')) {
      data.bootstrap = bootstrap;
    } else {
      data.bootstrap_content = bootstrap;
    }
  }
  if (opts.networkMode) data.network_mode = opts.networkMode;
  if (opts.vcpu && opts.vcpu > 1) data.vcpu = opts.vcpu;
  if (opts.domains) data.custom_domains = opts.domains;
  if (opts.serviceType) data.service_type = opts.serviceType;

  return makeRequest('POST', '/services', publicKey, secretKey, data);
}

/**
 * Get details of a specific service.
 *
 * Args:
 *   serviceId: Service ID to retrieve
 *
 * Returns: Promise<Object> (service details)
 */
async function getService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('GET', `/services/${serviceId}`, publicKey, secretKey);
}

/**
 * Update a service (resize vCPU/memory).
 *
 * Args:
 *   serviceId: Service ID to update
 *   opts: Update options:
 *     - vcpu: New vCPU count (1-8)
 *
 * Returns: Promise<Object> (update confirmation)
 */
async function updateService(serviceId, opts = {}, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {};
  if (opts.vcpu) data.vcpu = opts.vcpu;
  return makeRequest('PATCH', `/services/${serviceId}`, publicKey, secretKey, data);
}

/**
 * Delete/destroy a service.
 *
 * Args:
 *   serviceId: Service ID to destroy
 *
 * Returns: Promise<Object> (deletion confirmation)
 */
async function deleteService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('DELETE', `/services/${serviceId}`, publicKey, secretKey);
}

/**
 * Freeze a service (stop container, preserve disk).
 *
 * Args:
 *   serviceId: Service ID to freeze
 *
 * Returns: Promise<Object> (freeze confirmation)
 */
async function freezeService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/services/${serviceId}/freeze`, publicKey, secretKey, {});
}

/**
 * Unfreeze a service (restart container).
 *
 * Args:
 *   serviceId: Service ID to unfreeze
 *
 * Returns: Promise<Object> (unfreeze confirmation)
 */
async function unfreezeService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/services/${serviceId}/unfreeze`, publicKey, secretKey, {});
}

/**
 * Lock a service to prevent deletion.
 *
 * Args:
 *   serviceId: Service ID to lock
 *
 * Returns: Promise<Object> (lock confirmation)
 */
async function lockService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/services/${serviceId}/lock`, publicKey, secretKey, {});
}

/**
 * Unlock a service to allow deletion.
 *
 * Args:
 *   serviceId: Service ID to unlock
 *
 * Returns: Promise<Object> (unlock confirmation)
 */
async function unlockService(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/services/${serviceId}/unlock`, publicKey, secretKey, {});
}

/**
 * Get service logs.
 *
 * Args:
 *   serviceId: Service ID
 *   all: If true, get all logs; if false, get last ~9000 lines (default: false)
 *
 * Returns: Promise<Object> (log data)
 */
async function getServiceLogs(serviceId, all = false, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const path = all ? `/services/${serviceId}/logs?all=true` : `/services/${serviceId}/logs`;
  return makeRequest('GET', path, publicKey, secretKey);
}

/**
 * Get service environment vault status.
 *
 * Args:
 *   serviceId: Service ID
 *
 * Returns: Promise<Object> (vault status with has_vault, count, updated_at)
 */
async function getServiceEnv(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('GET', `/services/${serviceId}/env`, publicKey, secretKey);
}

/**
 * Set service environment vault.
 *
 * Args:
 *   serviceId: Service ID
 *   env: Environment content as string (KEY=VALUE format, newline separated)
 *        or object { KEY: "value", KEY2: "value2" }
 *
 * Returns: Promise<Object> (set confirmation)
 */
async function setServiceEnv(serviceId, env, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  // Convert object to KEY=VALUE format if needed
  let envContent = env;
  if (typeof env === 'object' && !Array.isArray(env)) {
    envContent = Object.entries(env)
      .map(([key, value]) => `${key}=${value}`)
      .join('\n');
  }
  // Note: This endpoint uses PUT with text/plain body
  // The makeRequest function sends JSON, so we need to handle this specially
  return makeRequest('PUT', `/services/${serviceId}/env`, publicKey, secretKey, { content: envContent });
}

/**
 * Delete service environment vault.
 *
 * Args:
 *   serviceId: Service ID
 *   keys: Optional array of specific keys to delete (deletes all if not specified)
 *
 * Returns: Promise<Object> (deletion confirmation)
 */
async function deleteServiceEnv(serviceId, keys = null, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = keys ? { keys } : {};
  return makeRequest('DELETE', `/services/${serviceId}/env`, publicKey, secretKey, data);
}

/**
 * Export service environment vault.
 *
 * Args:
 *   serviceId: Service ID
 *
 * Returns: Promise<Object> (exported environment data)
 */
async function exportServiceEnv(serviceId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/services/${serviceId}/env/export`, publicKey, secretKey, {});
}

/**
 * Redeploy a service with new bootstrap script.
 *
 * Args:
 *   serviceId: Service ID to redeploy
 *   bootstrap: Optional new bootstrap script content or URL
 *
 * Returns: Promise<Object> (redeploy confirmation)
 */
async function redeployService(serviceId, bootstrap = null, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {};
  if (bootstrap) {
    if (bootstrap.startsWith('http://') || bootstrap.startsWith('https://')) {
      data.bootstrap = bootstrap;
    } else {
      data.bootstrap_content = bootstrap;
    }
  }
  return makeRequest('POST', `/services/${serviceId}/redeploy`, publicKey, secretKey, data);
}

/**
 * Execute a command in a running service container.
 *
 * Args:
 *   serviceId: Service ID
 *   command: Command to execute
 *   timeout: Optional timeout in milliseconds (default: 30000)
 *
 * Returns: Promise<Object> (execution result with stdout, stderr, exit_code)
 */
async function executeInService(serviceId, command, timeout = 30000, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('POST', `/services/${serviceId}/execute`, publicKey, secretKey, {
    command,
    timeout,
  });

  // If we got a job_id, poll until completion
  const jobId = response.job_id;
  if (jobId) {
    return waitForJob(jobId, publicKey, secretKey);
  }

  return response;
}

// ============================================================================
// Additional Snapshot Functions
// ============================================================================

/**
 * Lock a snapshot to prevent deletion.
 *
 * Args:
 *   snapshotId: Snapshot ID to lock
 *
 * Returns: Promise<Object> (lock confirmation)
 */
async function lockSnapshot(snapshotId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/snapshots/${snapshotId}/lock`, publicKey, secretKey, {});
}

/**
 * Unlock a snapshot to allow deletion.
 *
 * Args:
 *   snapshotId: Snapshot ID to unlock
 *
 * Returns: Promise<Object> (unlock confirmation)
 */
async function unlockSnapshot(snapshotId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/snapshots/${snapshotId}/unlock`, publicKey, secretKey, {});
}

/**
 * Clone a snapshot to create a new session or service.
 *
 * Args:
 *   snapshotId: Snapshot ID to clone
 *   name: Name for the new resource
 *   opts: Optional settings:
 *     - type: "session" or "service" (default: inferred from snapshot)
 *     - shell: Shell for session clones
 *     - ports: Ports array for service clones
 *
 * Returns: Promise<Object> (clone result with new session_id or service_id)
 */
async function cloneSnapshot(snapshotId, name, opts = {}, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {};
  if (name) data.name = name;
  if (opts.type) data.type = opts.type;
  if (opts.shell) data.shell = opts.shell;
  if (opts.ports) data.ports = opts.ports;
  return makeRequest('POST', `/snapshots/${snapshotId}/clone`, publicKey, secretKey, data);
}

// ============================================================================
// Key Validation
// ============================================================================

/**
 * Validate API keys.
 *
 * Returns: Promise<Object> (validation result with valid, tier, expires_at, etc.)
 */
async function validateKeys(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  // Note: This endpoint is on the portal (unsandbox.com), not the API
  // For SDK purposes, we'll call the API endpoint if available
  return makeRequest('POST', '/keys/validate', publicKey, secretKey, {});
}

// Export all functions
module.exports = {
  // Code execution
  executeCode,
  executeAsync,
  getJob,
  waitForJob,
  cancelJob,
  listJobs,
  getLanguages,
  detectLanguage,
  // Session management
  listSessions,
  getSession,
  createSession,
  deleteSession,
  freezeSession,
  unfreezeSession,
  boostSession,
  unboostSession,
  shellSession,
  // Service management
  listServices,
  createService,
  getService,
  updateService,
  deleteService,
  freezeService,
  unfreezeService,
  lockService,
  unlockService,
  getServiceLogs,
  getServiceEnv,
  setServiceEnv,
  deleteServiceEnv,
  exportServiceEnv,
  redeployService,
  executeInService,
  // Snapshot management
  sessionSnapshot,
  serviceSnapshot,
  listSnapshots,
  restoreSnapshot,
  deleteSnapshot,
  lockSnapshot,
  unlockSnapshot,
  cloneSnapshot,
  // Key validation
  validateKeys,
  // Errors
  CredentialsError,
};
