/**
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com JavaScript SDK (Asynchronous with native fetch)
 * Isomorphic: Works in Node.js (CLI + SDK) and Browser environments
 *
 * Library Usage:
 *   import {
 *     // Code execution
 *     executeCode, executeAsync, getJob, waitForJob, cancelJob, listJobs,
 *     getLanguages, detectLanguage,
 *     // Session management
 *     listSessions, getSession, createSession, deleteSession,
 *     freezeSession, unfreezeSession, boostSession, unboostSession, shellSession,
 *     // Service management
 *     listServices, createService, getService, updateService, deleteService,
 *     freezeService, unfreezeService, lockService, unlockService, setUnfreezeOnDemand,
 *     getServiceLogs, getServiceEnv, setServiceEnv, deleteServiceEnv,
 *     exportServiceEnv, redeployService, executeInService,
 *     // Snapshot management
 *     sessionSnapshot, serviceSnapshot, listSnapshots, restoreSnapshot,
 *     deleteSnapshot, lockSnapshot, unlockSnapshot, cloneSnapshot,
 *     // Key validation
 *     validateKeys,
 *   } from './un_async.js';
 *
 *   // Execute code (awaits until completion)
 *   const result = await executeCode('python', 'print("hello")', publicKey, secretKey);
 *
 *   // Execute asynchronously (returns job_id immediately)
 *   const jobId = await executeAsync('javascript', 'console.log("hello")', publicKey, secretKey);
 *
 *   // Wait for job completion with exponential backoff
 *   const result = await waitForJob(jobId, publicKey, secretKey);
 *
 *   // Snapshot operations
 *   const snapshotId = await sessionSnapshot(sessionId, publicKey, secretKey, 'my-snapshot');
 *   const snapshots = await listSnapshots(publicKey, secretKey);
 *
 * Authentication Priority (5-tier):
 *   1. Function arguments (publicKey, secretKey)
 *   2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY) [Node.js]
 *   3. localStorage (unsandboxPublicKey, unsandboxSecretKey) [Browser]
 *   4. ~/.unsandbox/accounts.csv [Node.js]
 *   5. ./accounts.csv [Node.js]
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
 *
 * Browser Usage:
 *   - Import as ES module: <script type="module">
 *   - Configure credentials via localStorage:
 *       localStorage.setItem('useUnsandbox', 'true');
 *       localStorage.setItem('unsandboxPublicKey', 'unsb-pk-...');
 *       localStorage.setItem('unsandboxSecretKey', 'unsb-sk-...');
 *   - Or pass credentials directly to functions
 *   - Uses Web Crypto API for HMAC-SHA256 signing
 */

// Environment detection for isomorphic support (Node.js + Browser)
const IS_BROWSER = typeof window !== 'undefined' && typeof window.document !== 'undefined';
const IS_NODE = typeof process !== 'undefined' && process.versions != null && process.versions.node != null;

// Conditional imports for Node.js (null in browser)
let crypto = null;
let fs = null;
let path = null;

if (IS_NODE) {
  // Dynamic imports for Node.js
  crypto = await import('crypto').then(m => m.default || m);
  fs = await import('fs').then(m => m.default || m);
  path = await import('path').then(m => m.default || m);
}

const API_BASE = 'https://api.unsandbox.com';
const POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000];
const LANGUAGES_CACHE_TTL = 3600; // 1 hour

class CredentialsError extends Error {
  constructor(message) {
    super(message);
    this.name = 'CredentialsError';
  }
}

class TimeoutError extends Error {
  constructor(message) {
    super(message);
    this.name = 'TimeoutError';
  }
}

/**
 * Get ~/.unsandbox directory path, creating if necessary. [Node.js only]
 */
function getUnsandboxDir() {
  if (!IS_NODE) return null;
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
 * Load credentials from CSV file (public_key,secret_key per line). [Node.js only]
 */
function loadCredentialsFromCsv(csvPath, accountIndex = 0) {
  if (!IS_NODE || !fs) return null;
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
 * Load credentials from localStorage. [Browser only]
 */
function loadCredentialsFromStorage() {
  if (!IS_BROWSER) return null;

  try {
    const useUnsandbox = localStorage.getItem('useUnsandbox') === 'true';
    if (!useUnsandbox) return null;

    const publicKey = localStorage.getItem('unsandboxPublicKey');
    const secretKey = localStorage.getItem('unsandboxSecretKey');

    if (publicKey && secretKey) {
      return [publicKey, secretKey];
    }
  } catch (e) {
    // localStorage not available
  }

  return null;
}

/**
 * Resolve credentials from 5-tier priority system.
 *
 * Priority:
 *   1. Function arguments
 *   2. Environment variables (Node.js)
 *   3. localStorage (Browser)
 *   4. ~/.unsandbox/accounts.csv (Node.js)
 *   5. ./accounts.csv (Node.js)
 */
function resolveCredentials(publicKey, secretKey, accountIndex) {
  // Tier 1: Function arguments
  if (publicKey && secretKey) {
    return [publicKey, secretKey];
  }

  // Tier 2: Environment variables (Node.js only)
  if (IS_NODE) {
    const envPk = process.env.UNSANDBOX_PUBLIC_KEY;
    const envSk = process.env.UNSANDBOX_SECRET_KEY;
    if (envPk && envSk) {
      return [envPk, envSk];
    }
  }

  // Tier 3: localStorage (Browser only)
  if (IS_BROWSER) {
    const storageCreds = loadCredentialsFromStorage();
    if (storageCreds) {
      return storageCreds;
    }
  }

  // Tier 4 & 5: File-based credentials (Node.js only)
  if (IS_NODE && fs && path) {
    // Determine account index
    if (accountIndex === undefined) {
      accountIndex = parseInt(process.env.UNSANDBOX_ACCOUNT || '0', 10);
    }

    // Tier 4: ~/.unsandbox/accounts.csv
    try {
      const unsandboxDir = getUnsandboxDir();
      const creds = loadCredentialsFromCsv(path.join(unsandboxDir, 'accounts.csv'), accountIndex);
      if (creds) {
        return creds;
      }
    } catch (e) {
      // Continue to next tier
    }

    // Tier 5: ./accounts.csv
    const creds = loadCredentialsFromCsv('accounts.csv', accountIndex);
    if (creds) {
      return creds;
    }
  }

  const envHint = IS_NODE
    ? '  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n' +
      '  3. ~/.unsandbox/accounts.csv\n' +
      '  4. ./accounts.csv'
    : '  2. localStorage (enable "Use unsandbox.com API keys" in settings)';

  throw new CredentialsError(
    'No credentials found. Please provide via:\n' +
    '  1. Function arguments (publicKey, secretKey)\n' +
    envHint
  );
}

/**
 * Sign a request using HMAC-SHA256.
 * Isomorphic: uses Node.js crypto or Web Crypto API.
 *
 * Message format: "timestamp:METHOD:path:body"
 * Returns: 64-character hex string
 */
async function signRequest(secretKey, timestamp, method, urlPath, body) {
  const bodyStr = body || '';
  const message = `${timestamp}:${method}:${urlPath}:${bodyStr}`;

  if (IS_NODE && crypto) {
    // Node.js: synchronous HMAC
    return crypto
      .createHmac('sha256', secretKey)
      .update(message)
      .digest('hex');
  } else {
    // Browser: Web Crypto API (async)
    const encoder = new TextEncoder();
    const keyData = encoder.encode(secretKey);
    const messageData = encoder.encode(message);

    const cryptoKey = await window.crypto.subtle.importKey(
      'raw',
      keyData,
      { name: 'HMAC', hash: 'SHA-256' },
      false,
      ['sign']
    );

    const signature = await window.crypto.subtle.sign('HMAC', cryptoKey, messageData);
    const hashArray = Array.from(new Uint8Array(signature));
    return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
  }
}

/**
 * Sleep for a specified number of milliseconds.
 */
function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Make an authenticated HTTP request to the API using native fetch.
 *
 * Returns: Promise<Object> (parsed JSON response)
 * Throws: Error on network errors or non-JSON response
 */
async function makeRequest(method, urlPath, publicKey, secretKey, data) {
  const url = `${API_BASE}${urlPath}`;
  const timestamp = Math.floor(Date.now() / 1000);
  const body = data ? JSON.stringify(data) : '';

  const signature = await signRequest(secretKey, timestamp, method, urlPath, body || null);

  const headers = {
    'Authorization': `Bearer ${publicKey}`,
    'X-Timestamp': timestamp.toString(),
    'X-Signature': signature,
    'Content-Type': 'application/json',
    'User-Agent': 'un-js-async/2.0',
  };

  const options = {
    method,
    headers,
    signal: AbortSignal.timeout(120000), // 120 seconds timeout
  };

  // Add body for methods that support it
  if (['POST', 'PUT', 'PATCH', 'DELETE'].includes(method) && body) {
    options.body = body;
  }

  const response = await fetch(url, options);

  if (!response.ok) {
    const text = await response.text();
    throw new Error(`HTTP ${response.status}: ${text}`);
  }

  return response.json();
}

/**
 * Get path to languages cache file. [Node.js only]
 */
function getLanguagesCachePath() {
  if (!IS_NODE) return null;
  const dir = getUnsandboxDir();
  if (!dir) return null;
  return path.join(dir, 'languages.json');
}

/**
 * Load languages from cache if valid (< 1 hour old). [Node.js only]
 */
function loadLanguagesCache() {
  if (!IS_NODE || !fs) return null;
  try {
    const cachePath = getLanguagesCachePath();
    if (!cachePath || !fs.existsSync(cachePath)) {
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
 * Save languages to cache. [Node.js only]
 */
function saveLanguagesCache(languages) {
  if (!IS_NODE || !fs) return;
  try {
    const cachePath = getLanguagesCachePath();
    if (!cachePath) return;
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
 * Args:
 *   jobId: Job ID from executeAsync()
 *   publicKey: Optional API key
 *   secretKey: Optional API secret
 *   timeout: Optional maximum wait time in seconds (null = wait indefinitely)
 *
 * Returns: Promise<Object> (final job result when status is terminal)
 * Throws: TimeoutError if timeout is exceeded before job completes
 */
async function waitForJob(jobId, publicKey, secretKey, timeout = null) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  let pollCount = 0;
  const startTime = Date.now();

  while (true) {
    // Check timeout
    if (timeout !== null) {
      const elapsed = (Date.now() - startTime) / 1000;
      if (elapsed >= timeout) {
        throw new TimeoutError(`Job ${jobId} did not complete within ${timeout} seconds`);
      }
    }

    // Sleep before polling
    const delayIdx = Math.min(pollCount, POLL_DELAYS_MS.length - 1);
    await sleep(POLL_DELAYS_MS[delayIdx]);
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
 * Create a snapshot of a session.
 *
 * Args:
 *   sessionId: Session ID to snapshot
 *   publicKey: Optional API key
 *   secretKey: Optional API secret
 *   name: Optional snapshot name
 *   ephemeral: If true, snapshot is temporary and may be auto-deleted
 *
 * Returns: Promise<string> (snapshot ID)
 */
async function sessionSnapshot(sessionId, publicKey, secretKey, name = null, ephemeral = false) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {
    session_id: sessionId,
    ephemeral,
  };
  if (name) {
    data.name = name;
  }

  const response = await makeRequest('POST', '/snapshots', publicKey, secretKey, data);
  return response.snapshot_id;
}

/**
 * Create a snapshot of a service.
 *
 * Args:
 *   serviceId: Service ID to snapshot
 *   publicKey: Optional API key
 *   secretKey: Optional API secret
 *   name: Optional snapshot name
 *
 * Returns: Promise<string> (snapshot ID)
 */
async function serviceSnapshot(serviceId, publicKey, secretKey, name = null) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const data = {
    service_id: serviceId,
  };
  if (name) {
    data.name = name;
  }

  const response = await makeRequest('POST', '/snapshots', publicKey, secretKey, data);
  return response.snapshot_id;
}

/**
 * List all snapshots.
 *
 * Returns: Promise<Array> (list of snapshot dicts)
 */
async function listSnapshots(publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', '/snapshots', publicKey, secretKey);
  return response.snapshots || [];
}

/**
 * Restore a snapshot.
 *
 * Returns: Promise<Object> (response with restored resource info)
 */
async function restoreSnapshot(snapshotId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/snapshots/${snapshotId}/restore`, publicKey, secretKey, {});
}

/**
 * Delete a snapshot.
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
 *     - unfreezeOnDemand: If true, frozen services wake automatically on HTTP traffic
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
  if (opts.unfreezeOnDemand) data.unfreeze_on_demand = true;

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
 * Set unfreeze-on-demand for a service.
 *
 * When enabled, frozen services will automatically wake when HTTP traffic arrives.
 *
 * Args:
 *   serviceId: Service ID to update
 *   enabled: true to enable, false to disable
 *
 * Returns: Promise<Object> (update confirmation)
 */
async function setUnfreezeOnDemand(serviceId, enabled, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('PATCH', `/services/${serviceId}`, publicKey, secretKey, { unfreeze_on_demand: enabled });
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

// ============================================================================
// Image Generation
// ============================================================================

/**
 * Generate images from text prompt using AI.
 *
 * Args:
 *   prompt: Text description of the image to generate
 *   options: Generation options:
 *     - model: Model to use (optional)
 *     - size: Image size (default: "1024x1024")
 *     - quality: "standard" or "hd" (default: "standard")
 *     - n: Number of images (default: 1)
 *     - publicKey: API public key
 *     - secretKey: API secret key
 *
 * Returns: Promise<Object> (result with images array and created_at)
 */
async function image(prompt, options = {}) {
  const { model, size = "1024x1024", quality = "standard", n = 1, publicKey, secretKey } = options;
  const [pk, sk] = resolveCredentials(publicKey, secretKey);

  const payload = { prompt, size, quality, n };
  if (model) payload.model = model;

  return makeRequest('POST', '/image', pk, sk, payload);
}

// ES Module exports
export {
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
  setUnfreezeOnDemand,
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
  // Image generation
  image,
  // Errors
  CredentialsError,
  TimeoutError,
  // CLI
  cliMain,
};

// Default export for convenience
export default {
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
  setUnfreezeOnDemand,
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
  // Image generation
  image,
  // Errors
  CredentialsError,
  TimeoutError,
  // CLI
  cliMain,
};

// ============================================================================
// CLI Implementation
// ============================================================================

const HELP_TEXT = `
unsandbox CLI - Secure code execution platform (async version)

USAGE:
  node un_async.js [options] <source_file>     Execute code file
  node un_async.js -s <lang> '<code>'          Execute inline code
  node un_async.js session [options]           Interactive session management
  node un_async.js service [options]           Service management
  node un_async.js snapshot [options]          Snapshot management
  node un_async.js key                         Check API key validity

GLOBAL OPTIONS:
  -s, --shell <lang>      Language for inline code execution
  -e, --env <KEY=VAL>     Set environment variable (can be repeated)
  -f, --file <path>       Add input file to /tmp/ (can be repeated)
  -F, --file-path <path>  Add input file with path preserved
  -a, --artifacts         Return compiled artifacts
  -o, --output <dir>      Output directory for artifacts
  -p, --public-key <key>  API public key
  -k, --secret-key <key>  API secret key
  -n, --network <mode>    Network mode: zerotrust (default) or semitrusted
  -v, --vcpu <n>          vCPU count (1-8)
  -y, --yes               Skip confirmation prompts
  -h, --help              Show this help message

SESSION COMMANDS:
  node un_async.js session                     Start interactive bash session
  node un_async.js session --shell python3     Start Python REPL
  node un_async.js session --tmux              Persistent session with tmux
  node un_async.js session --screen            Persistent session with screen
  node un_async.js session --list              List active sessions
  node un_async.js session --attach <id>       Reconnect to session
  node un_async.js session --kill <id>         Terminate session
  node un_async.js session --freeze <id>       Pause session
  node un_async.js session --unfreeze <id>     Resume session
  node un_async.js session --boost <id>        Add resources
  node un_async.js session --unboost <id>      Remove boost
  node un_async.js session --snapshot <id>     Create snapshot

SERVICE COMMANDS:
  node un_async.js service --list              List all services
  node un_async.js service --name <n> --ports <p> --bootstrap <cmd>
                                               Create new service
  node un_async.js service --info <id>         Get service details
  node un_async.js service --logs <id>         Get service logs
  node un_async.js service --tail <id>         Get last 9000 lines
  node un_async.js service --freeze <id>       Pause service
  node un_async.js service --unfreeze <id>     Resume service
  node un_async.js service --destroy <id>      Delete service
  node un_async.js service --lock <id>         Prevent deletion
  node un_async.js service --unlock <id>       Allow deletion
  node un_async.js service --execute <id> <cmd> Run command in service
  node un_async.js service --redeploy <id>     Re-run bootstrap
  node un_async.js service --snapshot <id>     Create snapshot

SERVICE ENV COMMANDS:
  node un_async.js service env status <id>     Show vault status
  node un_async.js service env set <id>        Set from --env-file or stdin
  node un_async.js service env export <id>     Export to stdout
  node un_async.js service env delete <id>     Delete vault

SNAPSHOT COMMANDS:
  node un_async.js snapshot --list             List all snapshots
  node un_async.js snapshot --info <id>        Get snapshot details
  node un_async.js snapshot --delete <id>      Delete snapshot
  node un_async.js snapshot --lock <id>        Prevent deletion
  node un_async.js snapshot --unlock <id>      Allow deletion
  node un_async.js snapshot --clone <id>       Clone snapshot to new resource

EXAMPLES:
  node un_async.js script.py                   Execute Python script
  node un_async.js -s bash 'echo hello'        Run bash command
  node un_async.js -e DEBUG=1 script.py        Execute with env var
  node un_async.js -n semitrusted crawler.py   Execute with network access
  node un_async.js session --tmux              Start persistent session
  node un_async.js service --list              List all services
`;

/**
 * Parse command line arguments manually.
 * Returns object with parsed options and positional args.
 */
function parseArgs(args) {
  const result = {
    command: null,        // session, service, snapshot, key, or null (execute)
    subcommand: null,     // env (for service env commands)
    positional: [],
    shell: null,
    env: [],
    files: [],
    filesWithPath: [],
    artifacts: false,
    output: null,
    publicKey: null,
    secretKey: null,
    network: 'zerotrust',
    vcpu: 1,
    yes: false,
    help: false,
    // Session options
    list: false,
    attach: null,
    kill: null,
    freeze: null,
    unfreeze: null,
    boost: null,
    unboost: null,
    snapshot: null,
    snapshotName: null,
    hot: false,
    audit: false,
    tmux: false,
    screen: false,
    // Service options
    name: null,
    ports: null,
    domains: null,
    type: null,
    bootstrap: null,
    bootstrapFile: null,
    envFile: null,
    info: null,
    logs: null,
    tail: null,
    destroy: null,
    lock: null,
    unlock: null,
    resize: null,
    redeploy: null,
    execute: null,
    executeCmd: null,
    // Snapshot options
    delete: null,
    clone: null,
  };

  let i = 0;
  while (i < args.length) {
    const arg = args[i];

    // Check for subcommands first
    if (arg === 'session' && result.command === null) {
      result.command = 'session';
      i++;
      continue;
    }
    if (arg === 'service' && result.command === null) {
      result.command = 'service';
      i++;
      continue;
    }
    if (arg === 'snapshot' && result.command === null) {
      result.command = 'snapshot';
      i++;
      continue;
    }
    if (arg === 'key' && result.command === null) {
      result.command = 'key';
      i++;
      continue;
    }
    // Service env subcommand
    if (arg === 'env' && result.command === 'service') {
      result.subcommand = 'env';
      i++;
      continue;
    }
    // Service env operations (status, set, export, delete)
    if (result.command === 'service' && result.subcommand === 'env') {
      if (['status', 'set', 'export', 'delete'].includes(arg)) {
        result.envOperation = arg;
        i++;
        continue;
      }
    }

    // Parse options
    if (arg === '-h' || arg === '--help') {
      result.help = true;
      i++;
    } else if (arg === '-s' || arg === '--shell') {
      result.shell = args[++i];
      i++;
    } else if (arg === '-e' || arg === '--env') {
      result.env.push(args[++i]);
      i++;
    } else if (arg === '-f' || arg === '--file') {
      result.files.push(args[++i]);
      i++;
    } else if (arg === '-F' || arg === '--file-path') {
      result.filesWithPath.push(args[++i]);
      i++;
    } else if (arg === '-a' || arg === '--artifacts') {
      result.artifacts = true;
      i++;
    } else if (arg === '-o' || arg === '--output') {
      result.output = args[++i];
      i++;
    } else if (arg === '-p' || arg === '--public-key') {
      result.publicKey = args[++i];
      i++;
    } else if (arg === '-k' || arg === '--secret-key') {
      result.secretKey = args[++i];
      i++;
    } else if (arg === '-n' || arg === '--network') {
      result.network = args[++i];
      i++;
    } else if (arg === '-v' || arg === '--vcpu') {
      result.vcpu = parseInt(args[++i], 10);
      i++;
    } else if (arg === '-y' || arg === '--yes') {
      result.yes = true;
      i++;
    } else if (arg === '-l' || arg === '--list') {
      result.list = true;
      i++;
    } else if (arg === '--attach') {
      result.attach = args[++i];
      i++;
    } else if (arg === '--kill') {
      result.kill = args[++i];
      i++;
    } else if (arg === '--freeze') {
      result.freeze = args[++i];
      i++;
    } else if (arg === '--unfreeze') {
      result.unfreeze = args[++i];
      i++;
    } else if (arg === '--boost') {
      result.boost = args[++i];
      i++;
    } else if (arg === '--unboost') {
      result.unboost = args[++i];
      i++;
    } else if (arg === '--snapshot') {
      result.snapshot = args[++i];
      i++;
    } else if (arg === '--snapshot-name') {
      result.snapshotName = args[++i];
      i++;
    } else if (arg === '--hot') {
      result.hot = true;
      i++;
    } else if (arg === '--audit') {
      result.audit = true;
      i++;
    } else if (arg === '--tmux') {
      result.tmux = true;
      i++;
    } else if (arg === '--screen') {
      result.screen = true;
      i++;
    } else if (arg === '--name') {
      result.name = args[++i];
      i++;
    } else if (arg === '--ports') {
      result.ports = args[++i];
      i++;
    } else if (arg === '--domains') {
      result.domains = args[++i];
      i++;
    } else if (arg === '--type') {
      result.type = args[++i];
      i++;
    } else if (arg === '--bootstrap') {
      result.bootstrap = args[++i];
      i++;
    } else if (arg === '--bootstrap-file') {
      result.bootstrapFile = args[++i];
      i++;
    } else if (arg === '--env-file') {
      result.envFile = args[++i];
      i++;
    } else if (arg === '--info') {
      result.info = args[++i];
      i++;
    } else if (arg === '--logs') {
      result.logs = args[++i];
      i++;
    } else if (arg === '--tail') {
      result.tail = args[++i];
      i++;
    } else if (arg === '--destroy') {
      result.destroy = args[++i];
      i++;
    } else if (arg === '--lock') {
      result.lock = args[++i];
      i++;
    } else if (arg === '--unlock') {
      result.unlock = args[++i];
      i++;
    } else if (arg === '--resize') {
      result.resize = args[++i];
      i++;
    } else if (arg === '--redeploy') {
      result.redeploy = args[++i];
      i++;
    } else if (arg === '--execute') {
      result.execute = args[++i];
      // Next arg is the command to execute
      if (i + 1 < args.length && !args[i + 1].startsWith('-')) {
        result.executeCmd = args[++i];
      }
      i++;
    } else if (arg === '--delete') {
      result.delete = args[++i];
      i++;
    } else if (arg === '--clone') {
      result.clone = args[++i];
      i++;
    } else if (arg.startsWith('-')) {
      console.error(`Error: Unknown option: ${arg}`);
      process.exit(2);
    } else {
      result.positional.push(arg);
      i++;
    }
  }

  return result;
}

/**
 * Format timestamp for display.
 */
function formatTimestamp(ts) {
  if (!ts) return 'N/A';
  const date = new Date(ts * 1000);
  return date.toISOString().replace('T', ' ').substring(0, 19);
}

/**
 * Format list output in table format.
 */
function formatTable(items, columns) {
  if (!items || items.length === 0) {
    console.log('No items found.');
    return;
  }

  // Calculate column widths
  const widths = {};
  for (const col of columns) {
    widths[col.key] = col.label.length;
    for (const item of items) {
      const val = String(col.getter ? col.getter(item) : (item[col.key] || 'N/A'));
      widths[col.key] = Math.max(widths[col.key], val.length);
    }
  }

  // Print header
  let header = '';
  for (const col of columns) {
    header += col.label.padEnd(widths[col.key] + 2);
  }
  console.log(header);

  // Print rows
  for (const item of items) {
    let row = '';
    for (const col of columns) {
      const val = String(col.getter ? col.getter(item) : (item[col.key] || 'N/A'));
      row += val.padEnd(widths[col.key] + 2);
    }
    console.log(row);
  }
}

/**
 * Handle session commands.
 */
async function handleSession(opts) {
  const pk = opts.publicKey;
  const sk = opts.secretKey;

  // List sessions
  if (opts.list) {
    const sessions = await listSessions(pk, sk);
    formatTable(sessions, [
      { key: 'session_id', label: 'ID' },
      { key: 'name', label: 'NAME' },
      { key: 'status', label: 'STATUS' },
      { key: 'created_at', label: 'CREATED', getter: (s) => formatTimestamp(s.created_at) },
    ]);
    return;
  }

  // Attach to session
  if (opts.attach) {
    const session = await getSession(opts.attach, pk, sk);
    console.log(`Session ${opts.attach}:`);
    console.log(JSON.stringify(session, null, 2));
    console.log('\nNote: Interactive attach requires WebSocket connection (not supported in this CLI)');
    return;
  }

  // Kill session
  if (opts.kill) {
    await deleteSession(opts.kill, pk, sk);
    console.log(`Session ${opts.kill} terminated.`);
    return;
  }

  // Freeze session
  if (opts.freeze) {
    await freezeSession(opts.freeze, pk, sk);
    console.log(`Session ${opts.freeze} frozen.`);
    return;
  }

  // Unfreeze session
  if (opts.unfreeze) {
    await unfreezeSession(opts.unfreeze, pk, sk);
    console.log(`Session ${opts.unfreeze} unfrozen.`);
    return;
  }

  // Boost session
  if (opts.boost) {
    await boostSession(opts.boost, opts.vcpu || 2, pk, sk);
    console.log(`Session ${opts.boost} boosted.`);
    return;
  }

  // Unboost session
  if (opts.unboost) {
    await unboostSession(opts.unboost, pk, sk);
    console.log(`Session ${opts.unboost} unboosted.`);
    return;
  }

  // Snapshot session
  if (opts.snapshot) {
    const snapshotId = await sessionSnapshot(opts.snapshot, pk, sk, opts.snapshotName, opts.hot);
    console.log(`Snapshot created: ${snapshotId}`);
    return;
  }

  // Create new session
  const sessionOpts = {
    networkMode: opts.network,
    vcpu: opts.vcpu,
  };
  if (opts.tmux) sessionOpts.multiplexer = 'tmux';
  if (opts.screen) sessionOpts.multiplexer = 'screen';

  const session = await createSession(opts.shell || 'bash', sessionOpts, pk, sk);
  console.log(`Session created: ${session.session_id}`);
  console.log(JSON.stringify(session, null, 2));
  console.log('\nNote: Interactive session requires WebSocket connection (not supported in this CLI)');
}

/**
 * Handle service commands.
 */
async function handleService(opts) {
  const pk = opts.publicKey;
  const sk = opts.secretKey;

  // Handle env subcommand
  if (opts.subcommand === 'env') {
    const serviceId = opts.positional[0];
    if (!serviceId) {
      console.error('Error: Service ID required for env commands');
      process.exit(2);
    }

    switch (opts.envOperation) {
      case 'status': {
        const status = await getServiceEnv(serviceId, pk, sk);
        console.log(JSON.stringify(status, null, 2));
        break;
      }
      case 'set': {
        let envContent;
        if (opts.envFile) {
          envContent = fs.readFileSync(opts.envFile, 'utf-8');
        } else {
          // Read from stdin
          envContent = fs.readFileSync(0, 'utf-8');
        }
        await setServiceEnv(serviceId, envContent, pk, sk);
        console.log('Environment vault updated.');
        break;
      }
      case 'export': {
        const exported = await exportServiceEnv(serviceId, pk, sk);
        if (exported.content) {
          console.log(exported.content);
        } else {
          console.log(JSON.stringify(exported, null, 2));
        }
        break;
      }
      case 'delete': {
        await deleteServiceEnv(serviceId, null, pk, sk);
        console.log('Environment vault deleted.');
        break;
      }
      default:
        console.error('Error: Unknown env operation. Use: status, set, export, delete');
        process.exit(2);
    }
    return;
  }

  // List services
  if (opts.list) {
    const services = await listServices(pk, sk);
    formatTable(services, [
      { key: 'service_id', label: 'ID' },
      { key: 'name', label: 'NAME' },
      { key: 'status', label: 'STATUS' },
      { key: 'created_at', label: 'CREATED', getter: (s) => formatTimestamp(s.created_at) },
    ]);
    return;
  }

  // Get service info
  if (opts.info) {
    const service = await getService(opts.info, pk, sk);
    console.log(JSON.stringify(service, null, 2));
    return;
  }

  // Get service logs
  if (opts.logs) {
    const logs = await getServiceLogs(opts.logs, true, pk, sk);
    if (logs.logs) {
      console.log(logs.logs);
    } else if (logs.stdout) {
      console.log(logs.stdout);
    } else {
      console.log(JSON.stringify(logs, null, 2));
    }
    return;
  }

  // Get service tail
  if (opts.tail) {
    const logs = await getServiceLogs(opts.tail, false, pk, sk);
    if (logs.logs) {
      console.log(logs.logs);
    } else if (logs.stdout) {
      console.log(logs.stdout);
    } else {
      console.log(JSON.stringify(logs, null, 2));
    }
    return;
  }

  // Freeze service
  if (opts.freeze) {
    await freezeService(opts.freeze, pk, sk);
    console.log(`Service ${opts.freeze} frozen.`);
    return;
  }

  // Unfreeze service
  if (opts.unfreeze) {
    await unfreezeService(opts.unfreeze, pk, sk);
    console.log(`Service ${opts.unfreeze} unfrozen.`);
    return;
  }

  // Destroy service
  if (opts.destroy) {
    await deleteService(opts.destroy, pk, sk);
    console.log(`Service ${opts.destroy} destroyed.`);
    return;
  }

  // Lock service
  if (opts.lock) {
    await lockService(opts.lock, pk, sk);
    console.log(`Service ${opts.lock} locked.`);
    return;
  }

  // Unlock service
  if (opts.unlock) {
    await unlockService(opts.unlock, pk, sk);
    console.log(`Service ${opts.unlock} unlocked.`);
    return;
  }

  // Resize service
  if (opts.resize) {
    await updateService(opts.resize, { vcpu: opts.vcpu }, pk, sk);
    console.log(`Service ${opts.resize} resized.`);
    return;
  }

  // Redeploy service
  if (opts.redeploy) {
    let bootstrap = opts.bootstrap;
    if (opts.bootstrapFile) {
      bootstrap = fs.readFileSync(opts.bootstrapFile, 'utf-8');
    }
    await redeployService(opts.redeploy, bootstrap, pk, sk);
    console.log(`Service ${opts.redeploy} redeployed.`);
    return;
  }

  // Execute command in service
  if (opts.execute) {
    if (!opts.executeCmd) {
      console.error('Error: Command required for --execute');
      process.exit(2);
    }
    const result = await executeInService(opts.execute, opts.executeCmd, 30000, pk, sk);
    if (result.stdout) {
      process.stdout.write(result.stdout);
    }
    if (result.stderr) {
      process.stderr.write(result.stderr);
    }
    if (result.exit_code !== undefined) {
      console.log('---');
      console.log(`Exit code: ${result.exit_code}`);
    }
    return;
  }

  // Snapshot service
  if (opts.snapshot) {
    const snapshotId = await serviceSnapshot(opts.snapshot, pk, sk, opts.snapshotName);
    console.log(`Snapshot created: ${snapshotId}`);
    return;
  }

  // Create new service
  if (opts.name) {
    let ports = [];
    if (opts.ports) {
      ports = opts.ports.split(',').map((p) => parseInt(p.trim(), 10));
    }

    let bootstrap = opts.bootstrap;
    if (opts.bootstrapFile) {
      bootstrap = fs.readFileSync(opts.bootstrapFile, 'utf-8');
    }

    const serviceOpts = {
      networkMode: opts.network,
      vcpu: opts.vcpu,
    };
    if (opts.domains) {
      serviceOpts.domains = opts.domains.split(',').map((d) => d.trim());
    }
    if (opts.type) {
      serviceOpts.serviceType = opts.type;
    }

    const service = await createService(opts.name, ports, bootstrap, serviceOpts, pk, sk);
    console.log(`Service created: ${service.service_id}`);
    console.log(JSON.stringify(service, null, 2));
    return;
  }

  // No action specified
  console.error('Error: No service action specified. Use --list, --name, --info, etc.');
  process.exit(2);
}

/**
 * Handle snapshot commands.
 */
async function handleSnapshot(opts) {
  const pk = opts.publicKey;
  const sk = opts.secretKey;

  // List snapshots
  if (opts.list) {
    const snapshots = await listSnapshots(pk, sk);
    formatTable(snapshots, [
      { key: 'snapshot_id', label: 'ID' },
      { key: 'name', label: 'NAME' },
      { key: 'type', label: 'TYPE' },
      { key: 'created_at', label: 'CREATED', getter: (s) => formatTimestamp(s.created_at) },
    ]);
    return;
  }

  // Get snapshot info
  if (opts.info) {
    // Use GET /snapshots/{id}
    const [resolvedPk, resolvedSk] = resolveCredentials(pk, sk);
    const snapshot = await makeRequest('GET', `/snapshots/${opts.info}`, resolvedPk, resolvedSk);
    console.log(JSON.stringify(snapshot, null, 2));
    return;
  }

  // Delete snapshot
  if (opts.delete) {
    await deleteSnapshot(opts.delete, pk, sk);
    console.log(`Snapshot ${opts.delete} deleted.`);
    return;
  }

  // Lock snapshot
  if (opts.lock) {
    await lockSnapshot(opts.lock, pk, sk);
    console.log(`Snapshot ${opts.lock} locked.`);
    return;
  }

  // Unlock snapshot
  if (opts.unlock) {
    await unlockSnapshot(opts.unlock, pk, sk);
    console.log(`Snapshot ${opts.unlock} unlocked.`);
    return;
  }

  // Clone snapshot
  if (opts.clone) {
    const cloneOpts = {};
    if (opts.type) cloneOpts.type = opts.type;
    if (opts.shell) cloneOpts.shell = opts.shell;
    if (opts.ports) {
      cloneOpts.ports = opts.ports.split(',').map((p) => parseInt(p.trim(), 10));
    }

    const result = await cloneSnapshot(opts.clone, opts.name, cloneOpts, pk, sk);
    console.log('Snapshot cloned:');
    console.log(JSON.stringify(result, null, 2));
    return;
  }

  // No action specified
  console.error('Error: No snapshot action specified. Use --list, --info, --delete, --clone, etc.');
  process.exit(2);
}

/**
 * Handle key command.
 */
async function handleKey(opts) {
  try {
    const result = await validateKeys(opts.publicKey, opts.secretKey);
    console.log('API Key Status:');
    console.log(JSON.stringify(result, null, 2));
  } catch (err) {
    // If validate endpoint doesn't exist, just show that credentials were resolved
    const [pk] = resolveCredentials(opts.publicKey, opts.secretKey);
    console.log(`Public Key: ${pk}`);
    console.log('Key validation endpoint returned error - key may still be valid.');
  }
}

/**
 * Handle execute command (default).
 */
async function handleExecute(opts) {
  const pk = opts.publicKey;
  const sk = opts.secretKey;

  let code;
  let language;

  // Inline code with -s flag
  if (opts.shell && opts.positional.length > 0) {
    language = opts.shell;
    code = opts.positional[0];
  }
  // File execution
  else if (opts.positional.length > 0) {
    const filePath = opts.positional[0];
    language = detectLanguage(filePath);
    if (!language) {
      console.error(`Error: Could not detect language for file: ${filePath}`);
      console.error('Use -s <language> to specify explicitly.');
      process.exit(2);
    }
    code = fs.readFileSync(filePath, 'utf-8');
  } else {
    console.error('Error: No source file or inline code provided.');
    console.error('Use: node un_async.js <file> or node un_async.js -s <lang> "<code>"');
    process.exit(2);
  }

  // Build execution options
  const execPayload = {
    language,
    code,
  };

  // Add environment variables
  if (opts.env.length > 0) {
    execPayload.env = {};
    for (const e of opts.env) {
      const idx = e.indexOf('=');
      if (idx > 0) {
        execPayload.env[e.substring(0, idx)] = e.substring(idx + 1);
      }
    }
  }

  // Add network mode if not default
  if (opts.network !== 'zerotrust') {
    execPayload.network_mode = opts.network;
  }

  // Execute code
  const [resolvedPk, resolvedSk] = resolveCredentials(pk, sk);
  const result = await makeRequest('POST', '/execute', resolvedPk, resolvedSk, execPayload);

  // If job_id returned, wait for completion
  let finalResult = result;
  if (result.job_id && ['pending', 'running'].includes(result.status)) {
    finalResult = await waitForJob(result.job_id, resolvedPk, resolvedSk);
  }

  // Output results
  if (finalResult.stdout) {
    process.stdout.write(finalResult.stdout);
  }
  if (finalResult.stderr) {
    process.stderr.write(finalResult.stderr);
  }

  // Print summary
  console.log('---');
  if (finalResult.exit_code !== undefined) {
    console.log(`Exit code: ${finalResult.exit_code}`);
  }
  if (finalResult.execution_time_ms !== undefined) {
    console.log(`Execution time: ${finalResult.execution_time_ms}ms`);
  } else if (finalResult.duration_ms !== undefined) {
    console.log(`Execution time: ${finalResult.duration_ms}ms`);
  }

  // Exit with code's exit code
  if (finalResult.exit_code && finalResult.exit_code !== 0) {
    process.exit(1);
  }
}

/**
 * Main CLI entry point.
 */
async function cliMain() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log(HELP_TEXT);
    process.exit(0);
  }

  const opts = parseArgs(args);

  if (opts.help) {
    console.log(HELP_TEXT);
    process.exit(0);
  }

  try {
    switch (opts.command) {
      case 'session':
        await handleSession(opts);
        break;
      case 'service':
        await handleService(opts);
        break;
      case 'snapshot':
        await handleSnapshot(opts);
        break;
      case 'key':
        await handleKey(opts);
        break;
      default:
        await handleExecute(opts);
    }
  } catch (err) {
    if (err instanceof CredentialsError) {
      console.error(`Error: ${err.message}`);
      process.exit(3);
    } else if (err instanceof TimeoutError) {
      console.error(`Error: ${err.message}`);
      process.exit(5);
    } else if (err.message && err.message.includes('HTTP 401')) {
      console.error('Error: Authentication failed. Check your API keys.');
      process.exit(3);
    } else if (err.message && err.message.includes('HTTP 403')) {
      console.error('Error: Access denied.');
      process.exit(3);
    } else if (err.message && err.message.includes('HTTP 404')) {
      console.error('Error: Resource not found.');
      process.exit(4);
    } else if (err.message && err.message.includes('timeout')) {
      console.error('Error: Request timeout.');
      process.exit(5);
    } else {
      console.error(`Error: ${err.message}`);
      process.exit(1);
    }
  }
}

// CLI entry point - detect if running as main module (ESM) [Node.js only]
// In ESM, we use import.meta.url to check if this is the main module
if (IS_NODE) {
  const isMain = process.argv[1] && (
    process.argv[1].endsWith('/un_async.js') ||
    process.argv[1].endsWith('\\un_async.js') ||
    import.meta.url === `file://${process.argv[1]}`
  );

  if (isMain) {
    cliMain().catch((err) => {
      console.error('Error:', err.message);
      process.exit(1);
    });
  }
}
