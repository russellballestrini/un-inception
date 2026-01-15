#!/usr/bin/env node
// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software
//
// unsandbox SDK for JavaScript/Node.js - Execute code in secure sandboxes
// https://unsandbox.com | https://api.unsandbox.com/openapi
//
// Library Usage:
//   const un = require('./un.js');
//   const result = await un.execute("javascript", 'console.log("Hello")');
//   const job = await un.executeAsync("javascript", code);
//   const result = await un.wait(job.job_id);
//
// CLI Usage:
//   node un.js script.js
//   node un.js -s javascript 'console.log("Hello")'
//   node un.js session --shell node
//
// Authentication (in priority order):
//   1. Function arguments: execute(..., { publicKey, secretKey })
//   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
//   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

/**
 * unsandbox - Secure Code Execution SDK for JavaScript
 *
 * @example Simple execution
 * const un = require('./un.js');
 * const result = await un.execute("javascript", 'console.log("Hello World")');
 * console.log(result.stdout);
 *
 * @example Async execution
 * const job = await un.executeAsync("javascript", longRunningCode);
 * const result = await un.wait(job.job_id);
 *
 * @example Client class
 * const client = new un.Client({ publicKey: "unsb-pk-...", secretKey: "unsb-sk-..." });
 * const result = await client.execute("javascript", code);
 */

const crypto = require('crypto');
const https = require('https');
const fs = require('fs');
const path = require('path');
const os = require('os');

// ============================================================================
// Configuration
// ============================================================================

const API_BASE = "https://api.unsandbox.com";
const PORTAL_BASE = "https://unsandbox.com";
const DEFAULT_TIMEOUT = 300000; // 5 minutes in ms
const DEFAULT_TTL = 60; // 1 minute execution limit

// Polling delays (ms) - exponential backoff matching un.c
const POLL_DELAYS = [300, 450, 700, 900, 650, 1600, 2000];

// Extension to language mapping
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
    ".tcl": "tcl", ".raku": "raku", ".m": "objc", ".awk": "awk",
};

// ANSI colors
const BLUE = "\x1b[34m";
const RED = "\x1b[31m";
const GREEN = "\x1b[32m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

// ============================================================================
// Exceptions
// ============================================================================

class UnsandboxError extends Error {
    constructor(message) {
        super(message);
        this.name = 'UnsandboxError';
    }
}

class AuthenticationError extends UnsandboxError {
    constructor(message) {
        super(message);
        this.name = 'AuthenticationError';
    }
}

class ExecutionError extends UnsandboxError {
    constructor(message, exitCode = null, stderr = null) {
        super(message);
        this.name = 'ExecutionError';
        this.exitCode = exitCode;
        this.stderr = stderr;
    }
}

class APIError extends UnsandboxError {
    constructor(message, statusCode = null, response = null) {
        super(message);
        this.name = 'APIError';
        this.statusCode = statusCode;
        this.response = response;
    }
}

class TimeoutError extends UnsandboxError {
    constructor(message) {
        super(message);
        this.name = 'TimeoutError';
    }
}

// ============================================================================
// HMAC Authentication
// ============================================================================

/**
 * Generate HMAC-SHA256 signature for API request.
 * Signature = HMAC-SHA256(secretKey, "timestamp:METHOD:path:body")
 */
function signRequest(secretKey, timestamp, method, path, body = "") {
    const message = `${timestamp}:${method}:${path}:${body}`;
    return crypto.createHmac('sha256', secretKey)
        .update(message)
        .digest('hex');
}

/**
 * Load credentials from accounts.csv file.
 * @param {string} filepath - Path to accounts.csv
 * @param {number} accountIndex - Account index (0-based)
 * @returns {Object|null} { publicKey, secretKey } or null
 */
function loadAccountsCsv(filepath, accountIndex = 0) {
    if (!fs.existsSync(filepath)) return null;
    try {
        const lines = fs.readFileSync(filepath, 'utf-8').trim().split('\n');
        const validAccounts = [];
        for (const line of lines) {
            const trimmed = line.trim();
            if (!trimmed || trimmed.startsWith('#')) continue;
            if (trimmed.includes(',')) {
                const [pk, sk] = trimmed.split(',', 2);
                if (pk.startsWith('unsb-pk-') && sk.startsWith('unsb-sk-')) {
                    validAccounts.push({ publicKey: pk, secretKey: sk });
                }
            }
        }
        if (validAccounts.length > accountIndex) {
            return validAccounts[accountIndex];
        }
    } catch (e) {
        // Ignore file read errors
    }
    return null;
}

/**
 * Get API credentials in priority order:
 * 1. Function arguments
 * 2. Environment variables
 * 3. ~/.unsandbox/accounts.csv
 * 4. ./accounts.csv (same directory as this SDK)
 */
function getCredentials(publicKey = null, secretKey = null, accountIndex = 0) {
    // Priority 1: Function arguments
    if (publicKey && secretKey) {
        return { publicKey, secretKey };
    }

    // Priority 2: Environment variables
    const envPk = process.env.UNSANDBOX_PUBLIC_KEY;
    const envSk = process.env.UNSANDBOX_SECRET_KEY;
    if (envPk && envSk) {
        return { publicKey: envPk, secretKey: envSk };
    }

    // Priority 3: ~/.unsandbox/accounts.csv
    const homeAccounts = path.join(os.homedir(), '.unsandbox', 'accounts.csv');
    let result = loadAccountsCsv(homeAccounts, accountIndex);
    if (result) return result;

    // Priority 4: ./accounts.csv (same directory as SDK)
    const localAccounts = path.join(__dirname, 'accounts.csv');
    result = loadAccountsCsv(localAccounts, accountIndex);
    if (result) return result;

    throw new AuthenticationError(
        "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, " +
        "or create ~/.unsandbox/accounts.csv or ./accounts.csv, or pass credentials to function."
    );
}

// ============================================================================
// HTTP Client
// ============================================================================

/**
 * Make authenticated API request with HMAC signature.
 */
function apiRequest(endpoint, options = {}) {
    return new Promise((resolve, reject) => {
        const {
            method = "GET",
            data = null,
            bodyText = null,
            contentType = "application/json",
            publicKey = null,
            secretKey = null,
            timeout = DEFAULT_TIMEOUT,
        } = options;

        const creds = getCredentials(publicKey, secretKey);
        const url = new URL(API_BASE + endpoint);

        // Prepare body
        let body = "";
        if (bodyText !== null) {
            body = bodyText;
        } else if (data !== null) {
            body = JSON.stringify(data);
        }

        // Generate signature
        const timestamp = Math.floor(Date.now() / 1000);
        const signature = signRequest(creds.secretKey, timestamp, method, endpoint, body);

        const reqOptions = {
            hostname: url.hostname,
            port: 443,
            path: url.pathname + url.search,
            method: method,
            headers: {
                'Authorization': `Bearer ${creds.publicKey}`,
                'X-Timestamp': timestamp.toString(),
                'X-Signature': signature,
                'Content-Type': contentType,
            },
            timeout: timeout,
        };

        const req = https.request(reqOptions, (res) => {
            let responseBody = '';
            res.on('data', chunk => responseBody += chunk);
            res.on('end', () => {
                if (res.statusCode >= 200 && res.statusCode < 300) {
                    try {
                        resolve(responseBody ? JSON.parse(responseBody) : {});
                    } catch (e) {
                        resolve(responseBody);
                    }
                } else if (res.statusCode === 401) {
                    if (responseBody.toLowerCase().includes('timestamp')) {
                        reject(new AuthenticationError(
                            "Request timestamp expired. Your system clock may be out of sync. " +
                            "Run: sudo ntpdate -s time.nist.gov"
                        ));
                    } else {
                        reject(new AuthenticationError(`Authentication failed: ${responseBody}`));
                    }
                } else if (res.statusCode === 429) {
                    reject(new APIError(`Rate limit exceeded: ${responseBody}`, res.statusCode, responseBody));
                } else {
                    reject(new APIError(`HTTP ${res.statusCode}: ${responseBody}`, res.statusCode, responseBody));
                }
            });
        });

        req.on('error', (e) => {
            reject(new APIError(`Connection failed: ${e.message}`));
        });

        req.on('timeout', () => {
            req.destroy();
            reject(new TimeoutError('Request timeout'));
        });

        if (body) {
            req.write(body);
        }
        req.end();
    });
}

// ============================================================================
// Core Execution Functions
// ============================================================================

/**
 * Execute code synchronously and return results.
 *
 * @param {string} language - Programming language (python, javascript, go, rust, etc.)
 * @param {string} code - Source code to execute
 * @param {Object} options - Optional parameters
 * @param {Object} options.env - Environment variables dict
 * @param {Array} options.inputFiles - List of {filename, content} or {filename, contentBase64}
 * @param {string} options.networkMode - "zerotrust" (no network) or "semitrusted" (internet access)
 * @param {number} options.ttl - Execution timeout in seconds (1-900, default 60)
 * @param {number} options.vcpu - Virtual CPUs (1-8, default 1)
 * @param {boolean} options.returnArtifact - Return compiled binary
 * @param {boolean} options.returnWasmArtifact - Compile to WebAssembly
 * @param {string} options.publicKey - API public key
 * @param {string} options.secretKey - API secret key
 * @param {number} options.timeout - HTTP request timeout in ms
 * @returns {Promise<Object>} Result with stdout, stderr, exit_code, etc.
 *
 * @example
 * const result = await un.execute("javascript", 'console.log("Hello World")');
 * console.log(result.stdout);
 */
async function execute(language, code, options = {}) {
    const {
        env = null,
        inputFiles = null,
        networkMode = "zerotrust",
        ttl = DEFAULT_TTL,
        vcpu = 1,
        returnArtifact = false,
        returnWasmArtifact = false,
        publicKey = null,
        secretKey = null,
        timeout = DEFAULT_TIMEOUT,
    } = options;

    const payload = {
        language,
        code,
        network_mode: networkMode,
        ttl,
        vcpu,
    };

    if (env) payload.env = env;

    if (inputFiles) {
        payload.input_files = inputFiles.map(f => {
            if (f.contentBase64 || f.content_base64) {
                return { filename: f.filename, content_base64: f.contentBase64 || f.content_base64 };
            } else if (f.content) {
                return {
                    filename: f.filename,
                    content_base64: Buffer.from(f.content).toString('base64')
                };
            }
            return f;
        });
    }

    if (returnArtifact) payload.return_artifact = true;
    if (returnWasmArtifact) payload.return_wasm_artifact = true;

    return apiRequest("/execute", {
        method: "POST",
        data: payload,
        publicKey,
        secretKey,
        timeout,
    });
}

/**
 * Execute code asynchronously. Returns immediately with job_id for polling.
 *
 * @param {string} language - Programming language
 * @param {string} code - Source code to execute
 * @param {Object} options - Same options as execute()
 * @returns {Promise<Object>} Result with job_id, status ("pending")
 *
 * @example
 * const job = await un.executeAsync("javascript", longRunningCode);
 * console.log(`Job submitted: ${job.job_id}`);
 * const result = await un.wait(job.job_id);
 */
async function executeAsync(language, code, options = {}) {
    const {
        env = null,
        inputFiles = null,
        networkMode = "zerotrust",
        ttl = DEFAULT_TTL,
        vcpu = 1,
        returnArtifact = false,
        returnWasmArtifact = false,
        publicKey = null,
        secretKey = null,
    } = options;

    const payload = {
        language,
        code,
        network_mode: networkMode,
        ttl,
        vcpu,
    };

    if (env) payload.env = env;

    if (inputFiles) {
        payload.input_files = inputFiles.map(f => {
            if (f.contentBase64 || f.content_base64) {
                return { filename: f.filename, content_base64: f.contentBase64 || f.content_base64 };
            } else if (f.content) {
                return {
                    filename: f.filename,
                    content_base64: Buffer.from(f.content).toString('base64')
                };
            }
            return f;
        });
    }

    if (returnArtifact) payload.return_artifact = true;
    if (returnWasmArtifact) payload.return_wasm_artifact = true;

    return apiRequest("/execute/async", {
        method: "POST",
        data: payload,
        publicKey,
        secretKey,
    });
}

/**
 * Execute code with automatic language detection from shebang.
 *
 * @param {string} code - Source code with shebang (e.g., #!/usr/bin/env node)
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with detected_language, stdout, stderr, etc.
 *
 * @example
 * const code = '#!/usr/bin/env node\nconsole.log("Auto-detected!")';
 * const result = await un.run(code);
 * console.log(result.detected_language); // "javascript"
 */
async function run(code, options = {}) {
    const {
        env = null,
        networkMode = "zerotrust",
        ttl = DEFAULT_TTL,
        publicKey = null,
        secretKey = null,
        timeout = DEFAULT_TIMEOUT,
    } = options;

    let endpoint = `/run?ttl=${ttl}&network_mode=${networkMode}`;
    if (env) {
        endpoint += `&env=${encodeURIComponent(JSON.stringify(env))}`;
    }

    return apiRequest(endpoint, {
        method: "POST",
        bodyText: code,
        contentType: "text/plain",
        publicKey,
        secretKey,
        timeout,
    });
}

/**
 * Execute code asynchronously with automatic language detection.
 *
 * @param {string} code - Source code with shebang
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with job_id, detected_language, status ("pending")
 */
async function runAsync(code, options = {}) {
    const {
        env = null,
        networkMode = "zerotrust",
        ttl = DEFAULT_TTL,
        publicKey = null,
        secretKey = null,
    } = options;

    let endpoint = `/run/async?ttl=${ttl}&network_mode=${networkMode}`;
    if (env) {
        endpoint += `&env=${encodeURIComponent(JSON.stringify(env))}`;
    }

    return apiRequest(endpoint, {
        method: "POST",
        bodyText: code,
        contentType: "text/plain",
        publicKey,
        secretKey,
    });
}

// ============================================================================
// Job Management
// ============================================================================

/**
 * Get job status and results.
 *
 * @param {string} jobId - Job ID from executeAsync or runAsync
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Job status with keys: job_id, status, result (if completed)
 */
async function getJob(jobId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/jobs/${jobId}`, {
        method: "GET",
        publicKey,
        secretKey,
    });
}

/**
 * Wait for job completion with exponential backoff polling.
 *
 * @param {string} jobId - Job ID from executeAsync or runAsync
 * @param {Object} options - Optional parameters
 * @param {number} options.maxPolls - Maximum number of poll attempts (default 100)
 * @returns {Promise<Object>} Final job result
 *
 * @example
 * const job = await un.executeAsync("javascript", code);
 * const result = await un.wait(job.job_id);
 * console.log(result.stdout);
 */
async function wait(jobId, options = {}) {
    const {
        maxPolls = 100,
        publicKey = null,
        secretKey = null,
    } = options;

    const terminalStates = new Set(['completed', 'failed', 'timeout', 'cancelled']);

    for (let i = 0; i < maxPolls; i++) {
        // Exponential backoff delay
        const delayIdx = Math.min(i, POLL_DELAYS.length - 1);
        await new Promise(resolve => setTimeout(resolve, POLL_DELAYS[delayIdx]));

        const result = await getJob(jobId, { publicKey, secretKey });
        const status = result.status || "";

        if (terminalStates.has(status)) {
            if (status === 'failed') {
                throw new ExecutionError(
                    `Job failed: ${result.error || 'Unknown error'}`,
                    result.exit_code,
                    result.stderr
                );
            }
            if (status === 'timeout') {
                throw new TimeoutError(`Job timed out: ${jobId}`);
            }
            return result;
        }
    }

    throw new TimeoutError(`Max polls (${maxPolls}) exceeded for job ${jobId}`);
}

/**
 * Cancel a running job.
 *
 * @param {string} jobId - Job ID to cancel
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Partial output and artifacts collected before cancellation
 */
async function cancelJob(jobId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/jobs/${jobId}`, {
        method: "DELETE",
        publicKey,
        secretKey,
    });
}

/**
 * List all active jobs for this API key.
 *
 * @param {Object} options - Optional parameters
 * @returns {Promise<Array>} List of job summaries
 */
async function listJobs(options = {}) {
    const { publicKey = null, secretKey = null } = options;
    const result = await apiRequest("/jobs", {
        method: "GET",
        publicKey,
        secretKey,
    });
    return result.jobs || [];
}

// ============================================================================
// Image Generation
// ============================================================================

/**
 * Generate images from text prompt.
 *
 * @param {string} prompt - Text description of the image to generate
 * @param {Object} options - Optional parameters
 * @param {string} options.model - Model to use (optional)
 * @param {string} options.size - Image size (e.g., "1024x1024")
 * @param {string} options.quality - "standard" or "hd"
 * @param {number} options.n - Number of images to generate
 * @returns {Promise<Object>} Result with images array
 *
 * @example
 * const result = await un.image("A sunset over mountains");
 * console.log(result.images[0]);
 */
async function image(prompt, options = {}) {
    const {
        model = null,
        size = "1024x1024",
        quality = "standard",
        n = 1,
        publicKey = null,
        secretKey = null,
    } = options;

    const payload = { prompt, size, quality, n };
    if (model) payload.model = model;

    return apiRequest("/image", {
        method: "POST",
        data: payload,
        publicKey,
        secretKey,
    });
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Get list of supported programming languages.
 *
 * Results are cached in ~/.unsandbox/languages.json for 1 hour.
 *
 * @param {Object} options - Optional parameters
 * @param {boolean} options.forceRefresh - Bypass cache and fetch fresh data
 * @returns {Promise<Object>} Result with languages array, count, aliases
 */
async function languages(options = {}) {
    const { publicKey = null, secretKey = null, forceRefresh = false } = options;
    const cachePath = path.join(os.homedir(), '.unsandbox', 'languages.json');
    const cacheMaxAge = 3600 * 1000; // 1 hour in ms

    // Check cache unless force refresh
    if (!forceRefresh && fs.existsSync(cachePath)) {
        try {
            const stat = fs.statSync(cachePath);
            if (Date.now() - stat.mtimeMs < cacheMaxAge) {
                return JSON.parse(fs.readFileSync(cachePath, 'utf-8'));
            }
        } catch (e) {
            // Cache read failed, fetch from API
        }
    }

    // Fetch from API
    const result = await apiRequest("/languages", {
        method: "GET",
        publicKey,
        secretKey,
    });

    // Save to cache
    try {
        const cacheDir = path.dirname(cachePath);
        if (!fs.existsSync(cacheDir)) {
            fs.mkdirSync(cacheDir, { recursive: true });
        }
        fs.writeFileSync(cachePath, JSON.stringify(result));
    } catch (e) {
        // Cache write failed, continue anyway
    }

    return result;
}

/**
 * Detect programming language from file extension or shebang.
 *
 * @param {string} filename - File path
 * @returns {string|null} Language name or null if undetected
 */
function detectLanguage(filename) {
    const ext = path.extname(filename).toLowerCase();
    if (EXT_MAP[ext]) return EXT_MAP[ext];

    // Try shebang
    try {
        const content = fs.readFileSync(filename, 'utf-8');
        const firstLine = content.split('\n')[0];
        if (firstLine.startsWith('#!')) {
            if (firstLine.includes('python')) return 'python';
            if (firstLine.includes('node')) return 'javascript';
            if (firstLine.includes('ruby')) return 'ruby';
            if (firstLine.includes('perl')) return 'perl';
            if (firstLine.includes('bash') || firstLine.includes('/sh')) return 'bash';
            if (firstLine.includes('lua')) return 'lua';
            if (firstLine.includes('php')) return 'php';
        }
    } catch (e) {
        // File read failed
    }

    return null;
}

// ============================================================================
// Snapshots (Save/Restore Session & Service State)
// ============================================================================

/**
 * Create a snapshot of a session's current state.
 *
 * @param {string} sessionId - ID of the session to snapshot
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with snapshot_id, created_at, status
 */
async function sessionSnapshot(sessionId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/sessions/${sessionId}/snapshot`, {
        method: "POST",
        data: {},
        publicKey,
        secretKey,
    });
}

/**
 * Create a snapshot of a service's current state.
 *
 * @param {string} serviceId - ID of the service to snapshot
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with snapshot_id, created_at, status
 */
async function serviceSnapshot(serviceId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/services/${serviceId}/snapshot`, {
        method: "POST",
        data: {},
        publicKey,
        secretKey,
    });
}

/**
 * List all available snapshots.
 *
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with snapshots array, count
 */
async function listSnapshots(options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest("/snapshots", {
        method: "GET",
        publicKey,
        secretKey,
    });
}

/**
 * Restore a session or service from a snapshot.
 *
 * @param {string} snapshotId - ID of the snapshot to restore
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with restored_id, status
 */
async function restoreSnapshot(snapshotId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/snapshots/${snapshotId}/restore`, {
        method: "POST",
        data: {},
        publicKey,
        secretKey,
    });
}

/**
 * Delete a snapshot.
 *
 * @param {string} snapshotId - ID of the snapshot to delete
 * @param {Object} options - Optional parameters
 * @returns {Promise<Object>} Result with status
 */
async function deleteSnapshot(snapshotId, options = {}) {
    const { publicKey = null, secretKey = null } = options;
    return apiRequest(`/snapshots/${snapshotId}`, {
        method: "DELETE",
        publicKey,
        secretKey,
    });
}

// ============================================================================
// Client Class
// ============================================================================

/**
 * Unsandbox API client with stored credentials.
 *
 * @example
 * const client = new un.Client({ publicKey: "unsb-pk-...", secretKey: "unsb-sk-..." });
 * const result = await client.execute("javascript", 'console.log("Hello")');
 *
 * // Or load from environment/config automatically:
 * const client = new un.Client();
 * const result = await client.execute("javascript", code);
 */
class Client {
    /**
     * Initialize client with credentials.
     *
     * @param {Object} options - Optional parameters
     * @param {string} options.publicKey - API public key (unsb-pk-...)
     * @param {string} options.secretKey - API secret key (unsb-sk-...)
     * @param {number} options.accountIndex - Account index in ~/.unsandbox/accounts.csv
     */
    constructor(options = {}) {
        const { publicKey = null, secretKey = null, accountIndex = 0 } = options;
        const creds = getCredentials(publicKey, secretKey, accountIndex);
        this.publicKey = creds.publicKey;
        this.secretKey = creds.secretKey;
    }

    async execute(language, code, options = {}) {
        return execute(language, code, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async executeAsync(language, code, options = {}) {
        return executeAsync(language, code, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async run(code, options = {}) {
        return run(code, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async runAsync(code, options = {}) {
        return runAsync(code, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async getJob(jobId) {
        return getJob(jobId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async wait(jobId, options = {}) {
        return wait(jobId, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async cancelJob(jobId) {
        return cancelJob(jobId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async listJobs() {
        return listJobs({ publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async image(prompt, options = {}) {
        return image(prompt, {
            ...options,
            publicKey: this.publicKey,
            secretKey: this.secretKey,
        });
    }

    async languages(options = {}) {
        return languages({ ...options, publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async sessionSnapshot(sessionId) {
        return sessionSnapshot(sessionId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async serviceSnapshot(serviceId) {
        return serviceSnapshot(serviceId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async listSnapshots() {
        return listSnapshots({ publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async restoreSnapshot(snapshotId) {
        return restoreSnapshot(snapshotId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }

    async deleteSnapshot(snapshotId) {
        return deleteSnapshot(snapshotId, { publicKey: this.publicKey, secretKey: this.secretKey });
    }
}

// ============================================================================
// CLI Interface
// ============================================================================

async function cliMain() {
    const args = process.argv.slice(2);

    if (args.length === 0 || args[0] === '-h' || args[0] === '--help') {
        console.log(`unsandbox - Execute code in secure sandboxes

Usage:
  node un.js [options] <source_file>
  node un.js -s <language> '<code>'

Options:
  -s, --shell LANG    Execute inline code with specified language
  -e KEY=VALUE        Set environment variable (multiple allowed)
  -f FILE             Add input file (multiple allowed)
  -n MODE             Network mode: zerotrust (default) or semitrusted
  -v N                vCPU count (1-8, default 1)
  --ttl N             Execution timeout in seconds (default 60)
  -a, --artifacts     Return artifacts
  -o DIR              Output directory for artifacts
  -p KEY              API public key
  -k KEY              API secret key
  --async             Execute asynchronously

Examples:
  node un.js script.js                    Execute JavaScript file
  node un.js -s python 'print("Hello")'   Execute inline Python
  node un.js -e DEBUG=1 script.js         With environment variable
  node un.js -n semitrusted script.js     With network access
`);
        process.exit(args.length === 0 ? 1 : 0);
    }

    // Parse arguments
    let source = null;
    let inlineLang = null;
    const env = {};
    const files = [];
    let networkMode = "zerotrust";
    let vcpu = 1;
    let ttl = 60;
    let artifacts = false;
    let outputDir = null;
    let publicKey = null;
    let secretKey = null;
    let asyncMode = false;

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        if (arg === '-s' || arg === '--shell') {
            inlineLang = args[++i];
        } else if (arg === '-e') {
            const [k, v] = args[++i].split('=', 2);
            env[k] = v || '';
        } else if (arg === '-f') {
            files.push(args[++i]);
        } else if (arg === '-n' || arg === '--network') {
            networkMode = args[++i];
        } else if (arg === '-v' || arg === '--vcpu') {
            vcpu = parseInt(args[++i]);
        } else if (arg === '--ttl') {
            ttl = parseInt(args[++i]);
        } else if (arg === '-a' || arg === '--artifacts') {
            artifacts = true;
        } else if (arg === '-o' || arg === '--output') {
            outputDir = args[++i];
        } else if (arg === '-p' || arg === '--public-key') {
            publicKey = args[++i];
        } else if (arg === '-k' || arg === '--secret-key') {
            secretKey = args[++i];
        } else if (arg === '--async') {
            asyncMode = true;
        } else if (!arg.startsWith('-')) {
            source = arg;
        }
    }

    try {
        // Determine language and code
        let language, code;
        if (inlineLang) {
            language = inlineLang;
            code = source || "";
        } else if (!source) {
            console.error(`${RED}Error: No source file or code provided${RESET}`);
            process.exit(1);
        } else if (!fs.existsSync(source)) {
            // Treat as inline bash
            language = "bash";
            code = source;
        } else {
            language = detectLanguage(source);
            if (!language) {
                console.error(`${RED}Error: Cannot detect language for ${source}${RESET}`);
                process.exit(1);
            }
            code = fs.readFileSync(source, 'utf-8');
        }

        // Load input files
        const inputFiles = files.map(filepath => {
            if (!fs.existsSync(filepath)) {
                console.error(`${RED}Error: File not found: ${filepath}${RESET}`);
                process.exit(1);
            }
            return {
                filename: path.basename(filepath),
                contentBase64: fs.readFileSync(filepath).toString('base64')
            };
        });

        // Execute
        if (asyncMode) {
            const result = await executeAsync(language, code, {
                env: Object.keys(env).length ? env : null,
                inputFiles: inputFiles.length ? inputFiles : null,
                networkMode,
                ttl,
                vcpu,
                returnArtifact: artifacts,
                publicKey,
                secretKey,
            });
            console.log(`${GREEN}Job submitted: ${result.job_id}${RESET}`);
            console.log(`Status: ${result.status}`);
            console.log(`\nPoll with: node un.js job ${result.job_id}`);
        } else {
            const result = await execute(language, code, {
                env: Object.keys(env).length ? env : null,
                inputFiles: inputFiles.length ? inputFiles : null,
                networkMode,
                ttl,
                vcpu,
                returnArtifact: artifacts,
                publicKey,
                secretKey,
            });

            // Print output
            if (result.stdout) process.stdout.write(result.stdout);
            if (result.stderr) process.stderr.write(`${RED}${result.stderr}${RESET}`);

            // Save artifacts
            if (artifacts && result.artifacts) {
                const outDir = outputDir || '.';
                if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });
                for (const artifact of result.artifacts) {
                    const filename = artifact.filename || 'artifact';
                    const content = Buffer.from(artifact.content_base64, 'base64');
                    const filepath = path.join(outDir, filename);
                    fs.writeFileSync(filepath, content);
                    fs.chmodSync(filepath, 0o755);
                    console.error(`${GREEN}Saved: ${filepath}${RESET}`);
                }
            }

            process.exit(result.exit_code || 0);
        }
    } catch (e) {
        if (e instanceof AuthenticationError) {
            console.error(`${RED}Authentication error: ${e.message}${RESET}`);
        } else if (e instanceof ExecutionError) {
            console.error(`${RED}Execution error: ${e.message}${RESET}`);
            if (e.stderr) console.error(`${RED}${e.stderr}${RESET}`);
        } else if (e instanceof APIError) {
            console.error(`${RED}API error: ${e.message}${RESET}`);
        } else if (e instanceof TimeoutError) {
            console.error(`${RED}Timeout: ${e.message}${RESET}`);
            process.exit(124);
        } else {
            console.error(`${RED}Error: ${e.message}${RESET}`);
        }
        process.exit(1);
    }
}

// ============================================================================
// Module Exports
// ============================================================================

module.exports = {
    // Core execution
    execute,
    executeAsync,
    run,
    runAsync,

    // Job management
    getJob,
    wait,
    cancelJob,
    listJobs,

    // Image generation
    image,

    // Snapshots
    sessionSnapshot,
    serviceSnapshot,
    listSnapshots,
    restoreSnapshot,
    deleteSnapshot,

    // Utilities
    languages,
    detectLanguage,

    // Client class
    Client,

    // Exceptions
    UnsandboxError,
    AuthenticationError,
    ExecutionError,
    APIError,
    TimeoutError,

    // Constants
    API_BASE,
    PORTAL_BASE,
    EXT_MAP,

    // Internal functions (for testing/library usage)
    _signRequest: signRequest,
    _getCredentials: getCredentials,
    _apiRequest: apiRequest,
};

// Run CLI if called directly
if (require.main === module) {
    cliMain().catch(e => {
        console.error(`${RED}${e.message}${RESET}`);
        process.exit(1);
    });
}
