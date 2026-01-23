/**
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com JavaScript SDK (Synchronous/Async)
 * Isomorphic: Works in Node.js (CLI + SDK) and Browser environments
 *
 * Library Usage (ES Modules):
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
 *     // Images API (LXD container images)
 *     imagePublish, listImages, getImage, deleteImage,
 *     lockImage, unlockImage, setImageVisibility,
 *     grantImageAccess, revokeImageAccess, listImageTrusted,
 *     transferImage, spawnFromImage, cloneImage,
 *     // Key validation
 *     validateKeys,
 *   } from './un.js';
 *
 *   // Execute code asynchronously (returns Promise)
 *   const result = await executeCode('python', 'print("hello")', publicKey, secretKey);
 *   const jobId = await executeAsync('javascript', 'console.log("hello")', publicKey, secretKey);
 *   const result = await waitForJob(jobId, publicKey, secretKey);
 *
 * Authentication Priority (5-tier):
 *   1. Function arguments (publicKey, secretKey)
 *   2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY) [Node.js]
 *   3. Encrypted vault or localStorage [Browser] (vault preferred if CryptoJS available)
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
 *   - Credentials can be stored in encrypted vault (requires CryptoJS):
 *       UnsandboxVault.createVault('mypassword');
 *       UnsandboxVault.saveKeysToVault(vaultId, [{publicKey, secretKey}], 'mypassword');
 *   - Or configure via plain localStorage (legacy):
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

// ============================================================================
// Vault System for Encrypted Credential Storage [Browser only]
// Requires CryptoJS library for AES encryption
// ============================================================================

/**
 * UnsandboxVault - Encrypted credential storage using AES-256
 * Compatible with unsandbox.com portal vault format
 */
const UnsandboxVault = {
  /**
   * Check if CryptoJS is available for encryption
   */
  isAvailable() {
    return IS_BROWSER && typeof CryptoJS !== 'undefined';
  },

  /**
   * Get or create stable device salt (32 bytes, stored in localStorage)
   */
  getDeviceSalt() {
    if (!IS_BROWSER) return null;
    let salt = localStorage.getItem('unsandbox_device_salt');
    if (!salt) {
      const randomBytes = CryptoJS.lib.WordArray.random(32);
      salt = randomBytes.toString();
      localStorage.setItem('unsandbox_device_salt', salt);
    }
    return salt;
  },

  /**
   * Encrypt data with password using AES-256
   */
  encrypt(data, password) {
    if (!this.isAvailable()) return null;
    try {
      return CryptoJS.AES.encrypt(JSON.stringify(data), password).toString();
    } catch (e) {
      return null;
    }
  },

  /**
   * Decrypt data with password
   */
  decrypt(encryptedData, password) {
    if (!this.isAvailable()) return null;
    try {
      const bytes = CryptoJS.AES.decrypt(encryptedData, password);
      const decryptedStr = bytes.toString(CryptoJS.enc.Utf8);
      if (!decryptedStr) return null;
      return JSON.parse(decryptedStr);
    } catch (e) {
      return null;
    }
  },

  /**
   * Generate vault ID from password (deterministic per device)
   */
  getVaultId(password) {
    if (!this.isAvailable()) return null;
    const salt = this.getDeviceSalt();
    return CryptoJS.SHA256(password + salt).toString();
  },

  /**
   * Get all vaults from localStorage
   */
  getAllVaults() {
    if (!IS_BROWSER) return {};
    try {
      const vaultsJson = localStorage.getItem('unsandbox_vaults');
      return vaultsJson ? JSON.parse(vaultsJson) : {};
    } catch (e) {
      return {};
    }
  },

  /**
   * Save all vaults to localStorage
   */
  saveAllVaults(vaults) {
    if (!IS_BROWSER) return;
    localStorage.setItem('unsandbox_vaults', JSON.stringify(vaults));
  },

  /**
   * Check if any vaults exist
   */
  hasVaults() {
    return Object.keys(this.getAllVaults()).length > 0;
  },

  /**
   * Unlock vault with password
   * Returns: { success, vaultId, keys, activeKeyIndex, error }
   */
  unlockVault(password) {
    if (!this.isAvailable()) {
      return { success: false, error: 'CryptoJS not available' };
    }

    const vaultId = this.getVaultId(password);
    const vaults = this.getAllVaults();

    if (!vaults[vaultId]) {
      return { success: false, error: 'No vault found for this password' };
    }

    const decrypted = this.decrypt(vaults[vaultId].encrypted_keys, password);
    if (!decrypted) {
      return { success: false, error: 'Failed to decrypt vault' };
    }

    const activeIndex = vaults[vaultId].active_key_index || 0;
    return { success: true, vaultId, keys: decrypted, activeKeyIndex: activeIndex };
  },

  /**
   * Create new vault with password
   * Returns: { success, vaultId, error }
   */
  createVault(password) {
    if (!this.isAvailable()) {
      return { success: false, error: 'CryptoJS not available' };
    }
    if (password.length < 8) {
      return { success: false, error: 'Password must be at least 8 characters' };
    }

    const vaultId = this.getVaultId(password);
    const vaults = this.getAllVaults();

    if (vaults[vaultId]) {
      // Vault exists, try to unlock instead
      const decrypted = this.decrypt(vaults[vaultId].encrypted_keys, password);
      if (decrypted) {
        return { success: true, vaultId, keys: decrypted, unlocked: true };
      }
      return { success: false, error: 'Vault exists but failed to decrypt' };
    }

    const encrypted = this.encrypt([], password);
    if (!encrypted) {
      return { success: false, error: 'Failed to encrypt vault' };
    }

    vaults[vaultId] = {
      encrypted_keys: encrypted,
      created_at: new Date().toISOString(),
      active_key_index: 0
    };

    this.saveAllVaults(vaults);
    return { success: true, vaultId };
  },

  /**
   * Save keys to vault
   */
  saveKeysToVault(vaultId, keys, password, activeIndex = 0) {
    if (!this.isAvailable()) return false;

    const vaults = this.getAllVaults();
    if (!vaults[vaultId]) return false;

    const encrypted = this.encrypt(keys, password);
    if (!encrypted) return false;

    vaults[vaultId].encrypted_keys = encrypted;
    vaults[vaultId].active_key_index = activeIndex;
    vaults[vaultId].updated_at = new Date().toISOString();

    this.saveAllVaults(vaults);
    return true;
  },

  /**
   * Get active key from unlocked vault via global helper (if available)
   * This integrates with unsandbox.com portal's vault UI
   */
  getActiveKey() {
    if (!IS_BROWSER) return null;
    // Check if portal's getActiveApiKey function is available
    if (typeof window.getActiveApiKey === 'function') {
      return window.getActiveApiKey();
    }
    return null;
  }
};

// Make vault available globally in browser
if (IS_BROWSER) {
  window.UnsandboxVault = UnsandboxVault;
}

class CredentialsError extends Error {
  constructor(message) {
    super(message);
    this.name = 'CredentialsError';
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
 * Load credentials from vault. [Browser only]
 * Priority:
 *   1. UnsandboxVault (own encrypted vault, requires unlocked via portal UI)
 *   2. External vaults (window.UncloseVault for uncloseai.com integration)
 */
function loadCredentialsFromStorage() {
  if (!IS_BROWSER) return null;

  // Priority 1: Get active key from UnsandboxVault (requires portal UI to unlock)
  try {
    const activeKey = UnsandboxVault.getActiveKey();
    if (activeKey && activeKey.publicKey && (activeKey.secretKey || activeKey.key)) {
      return [activeKey.publicKey, activeKey.secretKey || activeKey.key];
    }
  } catch (e) {
    // Vault not available or not unlocked
  }

  // Priority 2: Check external vaults (e.g., UncloseVault from uncloseai.com)
  try {
    if (window.UncloseVault && window.UncloseVault.isUnlocked()) {
      const useUnsandbox = window.UncloseVault.get('useUnsandbox', false);
      if (useUnsandbox) {
        const publicKey = window.UncloseVault.get('unsandboxPublicKey');
        const secretKey = window.UncloseVault.get('unsandboxSecretKey');
        if (publicKey && secretKey) {
          return [publicKey, secretKey];
        }
      }
    }
  } catch (e) {
    // External vault not available
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
    'User-Agent': 'un-js/2.0',
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
// Images API (LXD Container Images)
// ============================================================================

/**
 * Publish an LXD container image from a session or service.
 *
 * Args:
 *   sourceType: Source type - "session" or "service"
 *   sourceId: Session ID or Service ID to publish from
 *   options: Optional settings:
 *     - name: Image name/alias
 *     - description: Image description
 *     - publicKey: API public key
 *     - secretKey: API secret key
 *
 * Returns: Promise<Object> (image info with image_id, fingerprint, etc.)
 */
async function imagePublish(sourceType, sourceId, options = {}) {
  const { name, description, publicKey, secretKey } = options;
  const [pk, sk] = resolveCredentials(publicKey, secretKey);

  const data = {
    source_type: sourceType,
    source_id: sourceId,
  };
  if (name) data.name = name;
  if (description) data.description = description;

  return makeRequest('POST', '/images', pk, sk, data);
}

/**
 * List LXD container images.
 *
 * Args:
 *   filterType: Optional filter - "owned", "shared", "public", or null for all accessible
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Array> (list of image objects)
 */
async function listImages(filterType, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const path = filterType ? `/images/${filterType}` : '/images';
  const response = await makeRequest('GET', path, publicKey, secretKey);
  return response.images || [];
}

/**
 * Get details of a specific image.
 *
 * Args:
 *   imageId: Image ID to retrieve
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (image details)
 */
async function getImage(imageId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('GET', `/images/${imageId}`, publicKey, secretKey);
}

/**
 * Delete an image.
 *
 * Args:
 *   imageId: Image ID to delete
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (deletion confirmation)
 */
async function deleteImage(imageId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('DELETE', `/images/${imageId}`, publicKey, secretKey);
}

/**
 * Lock an image to prevent deletion.
 *
 * Args:
 *   imageId: Image ID to lock
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (lock confirmation)
 */
async function lockImage(imageId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/lock`, publicKey, secretKey, {});
}

/**
 * Unlock an image to allow deletion.
 *
 * Args:
 *   imageId: Image ID to unlock
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (unlock confirmation)
 */
async function unlockImage(imageId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/unlock`, publicKey, secretKey, {});
}

/**
 * Set image visibility (public/private).
 *
 * Args:
 *   imageId: Image ID to update
 *   visibility: "public" or "private"
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (visibility update confirmation)
 */
async function setImageVisibility(imageId, visibility, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/visibility`, publicKey, secretKey, { visibility });
}

/**
 * Grant access to an image for another API key (share with trusted user).
 *
 * Args:
 *   imageId: Image ID to share
 *   trustedApiKey: Public key of the user to grant access to
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (grant confirmation)
 */
async function grantImageAccess(imageId, trustedApiKey, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/grant`, publicKey, secretKey, { trusted_api_key: trustedApiKey });
}

/**
 * Revoke access to an image for another API key.
 *
 * Args:
 *   imageId: Image ID to revoke access from
 *   trustedApiKey: Public key of the user to revoke access from
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (revoke confirmation)
 */
async function revokeImageAccess(imageId, trustedApiKey, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/revoke`, publicKey, secretKey, { trusted_api_key: trustedApiKey });
}

/**
 * List users who have been granted access to an image.
 *
 * Args:
 *   imageId: Image ID to list trusted users for
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Array> (list of trusted API keys)
 */
async function listImageTrusted(imageId, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  const response = await makeRequest('GET', `/images/${imageId}/trusted`, publicKey, secretKey);
  return response.trusted || [];
}

/**
 * Transfer ownership of an image to another API key.
 *
 * Args:
 *   imageId: Image ID to transfer
 *   toApiKey: Public key of the new owner
 *   publicKey: API public key
 *   secretKey: API secret key
 *
 * Returns: Promise<Object> (transfer confirmation)
 */
async function transferImage(imageId, toApiKey, publicKey, secretKey) {
  [publicKey, secretKey] = resolveCredentials(publicKey, secretKey);
  return makeRequest('POST', `/images/${imageId}/transfer`, publicKey, secretKey, { to_api_key: toApiKey });
}

/**
 * Spawn a new session or service from an image.
 *
 * Args:
 *   imageId: Image ID to spawn from
 *   options: Spawn options:
 *     - type: "session" or "service" (default: "session")
 *     - name: Name for the new resource
 *     - shell: Shell for session (e.g., "bash", "python3")
 *     - ports: Ports array for service
 *     - networkMode: "zerotrust" or "semitrusted"
 *     - vcpu: Number of vCPUs (1-8)
 *     - publicKey: API public key
 *     - secretKey: API secret key
 *
 * Returns: Promise<Object> (spawn result with session_id or service_id)
 */
async function spawnFromImage(imageId, options = {}) {
  const { type, name, shell, ports, networkMode, vcpu, publicKey, secretKey } = options;
  const [pk, sk] = resolveCredentials(publicKey, secretKey);

  const data = {};
  if (type) data.type = type;
  if (name) data.name = name;
  if (shell) data.shell = shell;
  if (ports) data.ports = ports;
  if (networkMode) data.network_mode = networkMode;
  if (vcpu) data.vcpu = vcpu;

  return makeRequest('POST', `/images/${imageId}/spawn`, pk, sk, data);
}

/**
 * Clone an image to create a copy owned by the caller.
 *
 * Args:
 *   imageId: Image ID to clone
 *   options: Clone options:
 *     - name: Name for the cloned image
 *     - description: Description for the cloned image
 *     - publicKey: API public key
 *     - secretKey: API secret key
 *
 * Returns: Promise<Object> (clone result with new image_id)
 */
async function cloneImage(imageId, options = {}) {
  const { name, description, publicKey, secretKey } = options;
  const [pk, sk] = resolveCredentials(publicKey, secretKey);

  const data = {};
  if (name) data.name = name;
  if (description) data.description = description;

  return makeRequest('POST', `/images/${imageId}/clone`, pk, sk, data);
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
  // Images API (LXD container images)
  imagePublish,
  listImages,
  getImage,
  deleteImage,
  lockImage,
  unlockImage,
  setImageVisibility,
  grantImageAccess,
  revokeImageAccess,
  listImageTrusted,
  transferImage,
  spawnFromImage,
  cloneImage,
  // Key validation
  validateKeys,
  // Image generation
  image,
  // Errors
  CredentialsError,
  // CLI
  cliMain,
  // Vault (browser only)
  UnsandboxVault,
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
  // Images API (LXD container images)
  imagePublish,
  listImages,
  getImage,
  deleteImage,
  lockImage,
  unlockImage,
  setImageVisibility,
  grantImageAccess,
  revokeImageAccess,
  listImageTrusted,
  transferImage,
  spawnFromImage,
  cloneImage,
  // Key validation
  validateKeys,
  // Image generation
  image,
  // Errors
  CredentialsError,
  // CLI
  cliMain,
  // Vault (browser only)
  UnsandboxVault,
};

// ============================================================================
// CLI Implementation
// ============================================================================

const HELP_TEXT = `
unsandbox CLI - Secure code execution platform

USAGE:
  node un.js [options] <source_file>     Execute code file
  node un.js -s <lang> '<code>'          Execute inline code
  node un.js session [options]           Interactive session management
  node un.js service [options]           Service management
  node un.js snapshot [options]          Snapshot management
  node un.js key                         Check API key validity
  node un.js languages [--json]          List available languages

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
  node un.js session                     Start interactive bash session
  node un.js session --shell python3     Start Python REPL
  node un.js session --tmux              Persistent session with tmux
  node un.js session --screen            Persistent session with screen
  node un.js session --list              List active sessions
  node un.js session --attach <id>       Reconnect to session
  node un.js session --kill <id>         Terminate session
  node un.js session --freeze <id>       Pause session
  node un.js session --unfreeze <id>     Resume session
  node un.js session --boost <id>        Add resources
  node un.js session --unboost <id>      Remove boost
  node un.js session --snapshot <id>     Create snapshot

SERVICE COMMANDS:
  node un.js service --list              List all services
  node un.js service --name <n> --ports <p> --bootstrap <cmd>
                                         Create new service
  node un.js service --info <id>         Get service details
  node un.js service --logs <id>         Get service logs
  node un.js service --tail <id>         Get last 9000 lines
  node un.js service --freeze <id>       Pause service
  node un.js service --unfreeze <id>     Resume service
  node un.js service --destroy <id>      Delete service
  node un.js service --lock <id>         Prevent deletion
  node un.js service --unlock <id>       Allow deletion
  node un.js service --execute <id> <cmd> Run command in service
  node un.js service --redeploy <id>     Re-run bootstrap
  node un.js service --snapshot <id>     Create snapshot

SERVICE ENV COMMANDS:
  node un.js service env status <id>     Show vault status
  node un.js service env set <id>        Set from --env-file or stdin
  node un.js service env export <id>     Export to stdout
  node un.js service env delete <id>     Delete vault

SNAPSHOT COMMANDS:
  node un.js snapshot --list             List all snapshots
  node un.js snapshot --info <id>        Get snapshot details
  node un.js snapshot --delete <id>      Delete snapshot
  node un.js snapshot --lock <id>        Prevent deletion
  node un.js snapshot --unlock <id>      Allow deletion
  node un.js snapshot --clone <id>       Clone snapshot to new resource

EXAMPLES:
  node un.js script.py                   Execute Python script
  node un.js -s bash 'echo hello'        Run bash command
  node un.js -e DEBUG=1 script.py        Execute with env var
  node un.js -n semitrusted crawler.py   Execute with network access
  node un.js session --tmux              Start persistent session
  node un.js service --list              List all services
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
    // Image options
    publish: null,
    sourceType: null,
    visibility: null,
    visibilityMode: null,
    spawn: null,
    // Languages options
    json: false,
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
      result.command = null;
      // Check for snapshot subcommand
      result.command = 'snapshot';
      i++;
      continue;
    }
    if (arg === 'key' && result.command === null) {
      result.command = 'key';
      i++;
      continue;
    }
    if (arg === 'languages' && result.command === null) {
      result.command = 'languages';
      i++;
      continue;
    }
    if (arg === 'image' && result.command === null) {
      result.command = 'image';
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
    } else if (arg === '--json') {
      result.json = true;
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
    } else if (arg === '--publish') {
      result.publish = args[++i];
      i++;
    } else if (arg === '--source-type') {
      result.sourceType = args[++i];
      i++;
    } else if (arg === '--visibility') {
      result.visibility = args[++i];
      // Next arg might be the mode
      if (i + 1 < args.length && !args[i + 1].startsWith('-')) {
        result.visibilityMode = args[++i];
      }
      i++;
    } else if (arg === '--spawn') {
      result.spawn = args[++i];
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
    const snapshotId = await serviceSnapshot(opts.snapshot, pk, sk, opts.snapshotName, opts.hot);
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
    // Use GET /snapshots/{id} - need to add this function or use makeRequest directly
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
 * Handle image command.
 */
async function handleImage(opts) {
  const pk = opts.publicKey;
  const sk = opts.secretKey;

  // List images
  if (opts.list) {
    const images = await listImages(null, pk, sk);
    formatTable(images, [
      { key: 'image_id', label: 'ID' },
      { key: 'name', label: 'NAME' },
      { key: 'visibility', label: 'VISIBILITY' },
      { key: 'source_type', label: 'SOURCE' },
      { key: 'created_at', label: 'CREATED', getter: (s) => formatTimestamp(s.created_at) },
    ]);
    return;
  }

  // Get image info
  if (opts.info) {
    const image = await getImage(opts.info, pk, sk);
    console.log(JSON.stringify(image, null, 2));
    return;
  }

  // Delete image
  if (opts.delete) {
    await deleteImage(opts.delete, pk, sk);
    console.log(`Image ${opts.delete} deleted.`);
    return;
  }

  // Lock image
  if (opts.lock) {
    await lockImage(opts.lock, pk, sk);
    console.log(`Image ${opts.lock} locked.`);
    return;
  }

  // Unlock image
  if (opts.unlock) {
    await unlockImage(opts.unlock, pk, sk);
    console.log(`Image ${opts.unlock} unlocked.`);
    return;
  }

  // Publish image
  if (opts.publish) {
    if (!opts.sourceType) {
      console.error('Error: --source-type required for --publish');
      process.exit(2);
    }
    const pubOpts = { publicKey: pk, secretKey: sk };
    if (opts.name) pubOpts.name = opts.name;
    const result = await imagePublish(opts.sourceType, opts.publish, pubOpts);
    const imageId = result.image_id || result.id;
    console.log(`Image published: ${imageId}`);
    return;
  }

  // Set visibility
  if (opts.visibility && opts.visibilityMode) {
    if (!['private', 'unlisted', 'public'].includes(opts.visibilityMode)) {
      console.error('Error: visibility must be private, unlisted, or public');
      process.exit(2);
    }
    await setImageVisibility(opts.visibility, opts.visibilityMode, pk, sk);
    console.log(`Image ${opts.visibility} visibility set to ${opts.visibilityMode}.`);
    return;
  }

  // Spawn from image
  if (opts.spawn) {
    if (!opts.name) {
      console.error('Error: --name required for --spawn');
      process.exit(2);
    }
    const spawnOpts = { publicKey: pk, secretKey: sk, name: opts.name };
    if (opts.ports) {
      spawnOpts.ports = opts.ports.split(',').map((p) => parseInt(p.trim(), 10));
    }
    const result = await spawnFromImage(opts.spawn, spawnOpts);
    const serviceId = result.service_id || result.id;
    console.log(`Service spawned: ${serviceId}`);
    return;
  }

  // Clone image
  if (opts.clone) {
    const cloneOpts = { publicKey: pk, secretKey: sk };
    if (opts.name) cloneOpts.name = opts.name;
    const result = await cloneImage(opts.clone, cloneOpts);
    const imageId = result.image_id || result.id;
    console.log(`Image cloned: ${imageId}`);
    return;
  }

  // No action specified
  console.error('Error: No image action specified. Use --list, --info, --delete, --publish, --spawn, --clone, etc.');
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
 * Handle languages command.
 */
async function handleLanguages(opts) {
  const languages = await getLanguages(opts.publicKey, opts.secretKey);

  if (opts.json) {
    // Output as JSON array
    console.log(JSON.stringify(languages));
  } else {
    // Output one language per line (pipe-friendly)
    for (const lang of languages) {
      console.log(lang);
    }
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
    console.error('Use: node un.js <file> or node un.js -s <lang> "<code>"');
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
      case 'image':
        await handleImage(opts);
        break;
      case 'key':
        await handleKey(opts);
        break;
      case 'languages':
        await handleLanguages(opts);
        break;
      default:
        await handleExecute(opts);
    }
  } catch (err) {
    if (err instanceof CredentialsError) {
      console.error(`Error: ${err.message}`);
      process.exit(3);
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
    process.argv[1].endsWith('/un.js') ||
    process.argv[1].endsWith('\\un.js') ||
    import.meta.url === `file://${process.argv[1]}`
  );

  if (isMain) {
    cliMain().catch((err) => {
      console.error('Error:', err.message);
      process.exit(1);
    });
  }
}
