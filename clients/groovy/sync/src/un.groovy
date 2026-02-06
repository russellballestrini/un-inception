#!/usr/bin/env groovy
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


#!/usr/bin/env groovy
/**
 * unsandbox SDK for Groovy - Execute code in secure sandboxes
 * https://unsandbox.com | https://api.unsandbox.com/openapi
 *
 * <h2>Library Usage:</h2>
 * <pre>{@code
 * import un
 *
 * // Simple execution
 * def result = un.execute("python", 'print("Hello")')
 * println result.stdout
 *
 * // Async execution
 * def job = un.executeAsync("python", longCode)
 * def result = un.wait(job.job_id)
 *
 * // Using Client class
 * def client = new un.Client(publicKey: "unsb-pk-...", secretKey: "unsb-sk-...")
 * def result = client.execute("python", code)
 * }</pre>
 *
 * <h2>CLI Usage:</h2>
 * <pre>
 * groovy un.groovy script.py
 * groovy un.groovy -s python 'print("Hello")'
 * groovy un.groovy session --shell python3
 * </pre>
 *
 * <h2>Authentication (in priority order):</h2>
 * <ol>
 *   <li>Function arguments: execute(..., publicKey: "...", secretKey: "...")</li>
 *   <li>Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY</li>
 *   <li>Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)</li>
 * </ol>
 *
 * @author Permacomputer Project
 * @version 4.3.0
 */

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import groovy.json.JsonSlurper
import groovy.json.JsonOutput

// ============================================================================
// Configuration
// ============================================================================

/** API base URL for unsandbox */
def API_BASE = 'https://api.unsandbox.com'

/** Portal base URL for unsandbox */
def PORTAL_BASE = 'https://unsandbox.com'

/** Default execution timeout in seconds */
def DEFAULT_TIMEOUT = 300

/** Default TTL for code execution */
def DEFAULT_TTL = 60

/** Maximum vault content size (64KB) */
def MAX_ENV_CONTENT_SIZE = 65536

/** Polling delays (ms) - exponential backoff */
def POLL_DELAYS = [300, 450, 700, 900, 650, 1600, 2000]

// ANSI colors
def BLUE = '\033[34m'
def RED = '\033[31m'
def GREEN = '\033[32m'
def YELLOW = '\033[33m'
def RESET = '\033[0m'

/** Extension to language mapping */
def EXT_MAP = [
    '.java': 'java', '.kt': 'kotlin', '.cs': 'csharp', '.fs': 'fsharp',
    '.groovy': 'groovy', '.dart': 'dart', '.scala': 'scala',
    '.py': 'python', '.js': 'javascript', '.ts': 'typescript',
    '.rb': 'ruby', '.go': 'go', '.rs': 'rust', '.cpp': 'cpp', '.c': 'c',
    '.sh': 'bash', '.pl': 'perl', '.lua': 'lua', '.php': 'php',
    '.hs': 'haskell', '.ml': 'ocaml', '.clj': 'clojure', '.scm': 'scheme',
    '.lisp': 'commonlisp', '.erl': 'erlang', '.ex': 'elixir',
    '.jl': 'julia', '.r': 'r', '.cr': 'crystal', '.f90': 'fortran',
    '.cob': 'cobol', '.pro': 'prolog', '.forth': 'forth', '.tcl': 'tcl',
    '.raku': 'raku', '.d': 'd', '.nim': 'nim', '.zig': 'zig', '.v': 'v',
    '.awk': 'awk', '.m': 'objc'
]

// ============================================================================
// Exceptions
// ============================================================================

/**
 * Base exception for unsandbox errors.
 */
class UnsandboxError extends Exception {
    UnsandboxError(String message) {
        super(message)
    }
}

/**
 * Authentication failed - invalid or missing credentials.
 */
class AuthenticationError extends UnsandboxError {
    AuthenticationError(String message) {
        super(message)
    }
}

/**
 * Code execution failed.
 */
class ExecutionError extends UnsandboxError {
    Integer exitCode
    String stderr

    ExecutionError(String message, Integer exitCode = null, String stderr = null) {
        super(message)
        this.exitCode = exitCode
        this.stderr = stderr
    }
}

/**
 * API request failed.
 */
class APIError extends UnsandboxError {
    Integer statusCode
    String response

    APIError(String message, Integer statusCode = null, String response = null) {
        super(message)
        this.statusCode = statusCode
        this.response = response
    }
}

/**
 * Execution timed out.
 */
class TimeoutError extends UnsandboxError {
    TimeoutError(String message) {
        super(message)
    }
}

// ============================================================================
// HMAC Authentication
// ============================================================================

/**
 * Generate HMAC-SHA256 signature for API request.
 *
 * <p>Signature format: HMAC-SHA256(secretKey, "timestamp:METHOD:path:body")</p>
 *
 * @param secretKey The secret key for HMAC
 * @param timestamp Unix timestamp
 * @param method HTTP method (GET, POST, etc.)
 * @param path API endpoint path
 * @param body Request body (empty string if none)
 * @return Hex-encoded signature
 */
def signRequest(String secretKey, long timestamp, String method, String path, String body = "") {
    def message = "${timestamp}:${method}:${path}:${body}"
    def mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
    return mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()
}

/**
 * Get API credentials in priority order.
 *
 * <ol>
 *   <li>Function arguments</li>
 *   <li>Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)</li>
 *   <li>Config file (~/.unsandbox/accounts.csv)</li>
 * </ol>
 *
 * @param publicKey Optional public key argument
 * @param secretKey Optional secret key argument
 * @param accountIndex Account index in config file (default 0)
 * @return Tuple of [publicKey, secretKey]
 * @throws AuthenticationError if no credentials found
 */
def getCredentials(String publicKey = null, String secretKey = null, int accountIndex = 0) {
    // Priority 1: Function arguments
    if (publicKey && secretKey) {
        return [publicKey, secretKey]
    }

    // Priority 2: Environment variables
    def envPk = System.getenv('UNSANDBOX_PUBLIC_KEY')
    def envSk = System.getenv('UNSANDBOX_SECRET_KEY')
    if (envPk && envSk) {
        return [envPk, envSk]
    }

    // Priority 3: Config file
    def accountsPath = new File(System.getProperty('user.home'), '.unsandbox/accounts.csv')
    if (accountsPath.exists()) {
        try {
            def lines = accountsPath.text.trim().split('\n')
            def validAccounts = []
            lines.each { line ->
                def trimmed = line.trim()
                if (!trimmed || trimmed.startsWith('#')) return
                if (trimmed.contains(',')) {
                    def parts = trimmed.split(',', 2)
                    def pk = parts[0]
                    def sk = parts[1]
                    if (pk.startsWith('unsb-pk-') && sk.startsWith('unsb-sk-')) {
                        validAccounts << [pk, sk]
                    }
                }
            }
            if (validAccounts && accountIndex < validAccounts.size()) {
                return validAccounts[accountIndex]
            }
        } catch (Exception e) {
            // Ignore file read errors
        }
    }

    throw new AuthenticationError(
        "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, " +
        "or create ~/.unsandbox/accounts.csv, or pass credentials to function."
    )
}

// Legacy compatibility
def getApiKeys(argsKey) {
    def publicKey = System.getenv('UNSANDBOX_PUBLIC_KEY')
    def secretKey = System.getenv('UNSANDBOX_SECRET_KEY')

    if (!publicKey || !secretKey) {
        def legacyKey = argsKey ?: System.getenv('UNSANDBOX_API_KEY')
        if (!legacyKey) {
            System.err.println("${RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set${RESET}")
            System.exit(1)
        }
        return [legacyKey, null]
    }

    return [publicKey, secretKey]
}

// ============================================================================
// HTTP Client
// ============================================================================

/**
 * Make authenticated API request with HMAC signature.
 *
 * @param endpoint API endpoint path
 * @param method HTTP method
 * @param data Request body data (will be JSON-encoded if Map)
 * @param publicKey API public key
 * @param secretKey API secret key
 * @param timeout Request timeout in seconds
 * @param contentType Content-Type header
 * @return Parsed JSON response as Map
 * @throws APIError on request failure
 */
def apiRequest(String endpoint, String method, data, String publicKey, String secretKey,
               int timeout = DEFAULT_TIMEOUT, String contentType = 'application/json') {
    def tempFile = File.createTempFile('un_request_', '.json')
    try {
        def body = ""
        if (data) {
            body = data instanceof Map ? JsonOutput.toJson(data) : data.toString()
            tempFile.text = body
        }

        def curlCmd = ['curl', '-s', '-X', method, "${API_BASE}${endpoint}",
                       '-H', "Content-Type: ${contentType}"]

        // Add HMAC authentication headers if secretKey is provided
        if (secretKey) {
            def timestamp = (System.currentTimeMillis() / 1000) as long
            def signature = signRequest(secretKey, timestamp, method, endpoint, body)

            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
            curlCmd += ['-H', "X-Timestamp: ${timestamp}"]
            curlCmd += ['-H', "X-Signature: ${signature}"]
        } else {
            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
        }

        if (data) {
            curlCmd += ['-d', "@${tempFile.absolutePath}"]
        }

        def proc = curlCmd.execute()
        def output = proc.text
        proc.waitFor()

        if (proc.exitValue() != 0) {
            throw new APIError("curl failed with exit code ${proc.exitValue()}")
        }

        // Check for timestamp authentication errors
        if (output.toLowerCase().contains('timestamp') &&
            (output.contains('401') || output.toLowerCase().contains('expired') || output.toLowerCase().contains('invalid'))) {
            throw new AuthenticationError(
                "Request timestamp expired. Your system clock may be out of sync. " +
                "Run: sudo ntpdate -s time.nist.gov"
            )
        }

        try {
            return new JsonSlurper().parseText(output)
        } catch (Exception e) {
            return [raw: output]
        }
    } finally {
        tempFile.delete()
    }
}

def apiRequestPatch(endpoint, data, publicKey, secretKey) {
    return apiRequest(endpoint, 'PATCH', data, publicKey, secretKey)
}

/**
 * Exception for 428 Sudo Challenge requiring OTP confirmation.
 */
class SudoChallengeError extends UnsandboxError {
    String challengeId
    String responseBody

    SudoChallengeError(String challengeId, String responseBody) {
        super("Sudo challenge required")
        this.challengeId = challengeId
        this.responseBody = responseBody
    }
}

/**
 * Make API request for destructive operations with 428 handling.
 * Uses curl with -w to capture HTTP status code.
 */
def apiRequestDestructive(String endpoint, String method, data, String publicKey, String secretKey) {
    def tempFile = File.createTempFile('un_request_', '.json')
    def statusFile = File.createTempFile('un_status_', '.txt')
    try {
        def body = ""
        if (data) {
            body = data instanceof Map ? JsonOutput.toJson(data) : data.toString()
            tempFile.text = body
        }

        def timestamp = (System.currentTimeMillis() / 1000) as long
        def signature = signRequest(secretKey, timestamp, method, endpoint, body)

        def curlCmd = ['curl', '-s', '-X', method, "${API_BASE}${endpoint}",
                       '-H', "Content-Type: application/json",
                       '-H', "Authorization: Bearer ${publicKey}",
                       '-H', "X-Timestamp: ${timestamp}",
                       '-H', "X-Signature: ${signature}",
                       '-w', '\\n%{http_code}',
                       '-o', statusFile.absolutePath]

        if (data) {
            curlCmd += ['-d', "@${tempFile.absolutePath}"]
        }

        def proc = curlCmd.execute()
        def statusOutput = proc.text.trim()
        proc.waitFor()

        def responseBody = statusFile.exists() ? statusFile.text : ""
        def httpCode = 0
        try {
            httpCode = statusOutput.toInteger()
        } catch (Exception e) {
            // Failed to parse status code
        }

        if (httpCode == 428) {
            // Extract challenge_id from response
            def challengeId = null
            try {
                def parsed = new JsonSlurper().parseText(responseBody)
                challengeId = parsed?.challenge_id
            } catch (Exception e) {
                // Ignore parse errors
            }
            throw new SudoChallengeError(challengeId, responseBody)
        }

        if (httpCode < 200 || httpCode >= 300) {
            throw new APIError("HTTP ${httpCode} - ${responseBody}", httpCode, responseBody)
        }

        try {
            return new JsonSlurper().parseText(responseBody)
        } catch (Exception e) {
            return [raw: responseBody]
        }
    } finally {
        tempFile.delete()
        statusFile.delete()
    }
}

/**
 * Make API request with sudo OTP headers.
 */
def apiRequestWithSudo(String endpoint, String method, data, String publicKey, String secretKey, String otp, String challengeId) {
    def tempFile = File.createTempFile('un_request_', '.json')
    def statusFile = File.createTempFile('un_status_', '.txt')
    try {
        def body = ""
        if (data) {
            body = data instanceof Map ? JsonOutput.toJson(data) : data.toString()
            tempFile.text = body
        }

        def timestamp = (System.currentTimeMillis() / 1000) as long
        def signature = signRequest(secretKey, timestamp, method, endpoint, body)

        def curlCmd = ['curl', '-s', '-X', method, "${API_BASE}${endpoint}",
                       '-H', "Content-Type: application/json",
                       '-H', "Authorization: Bearer ${publicKey}",
                       '-H', "X-Timestamp: ${timestamp}",
                       '-H', "X-Signature: ${signature}",
                       '-H', "X-Sudo-OTP: ${otp}",
                       '-w', '\\n%{http_code}',
                       '-o', statusFile.absolutePath]

        if (challengeId) {
            curlCmd += ['-H', "X-Sudo-Challenge: ${challengeId}"]
        }

        if (data) {
            curlCmd += ['-d', "@${tempFile.absolutePath}"]
        }

        def proc = curlCmd.execute()
        def statusOutput = proc.text.trim()
        proc.waitFor()

        def responseBody = statusFile.exists() ? statusFile.text : ""
        def httpCode = 0
        try {
            httpCode = statusOutput.toInteger()
        } catch (Exception e) {
            // Failed to parse status code
        }

        if (httpCode < 200 || httpCode >= 300) {
            throw new APIError("HTTP ${httpCode} - ${responseBody}", httpCode, responseBody)
        }

        try {
            return new JsonSlurper().parseText(responseBody)
        } catch (Exception e) {
            return [raw: responseBody]
        }
    } finally {
        tempFile.delete()
        statusFile.delete()
    }
}

/**
 * Handle sudo challenge by prompting for OTP and retrying.
 */
def handleSudoChallenge(String challengeId, String method, String endpoint, data, String publicKey, String secretKey) {
    System.err.println("${YELLOW}Confirmation required. Check your email for a one-time code.${RESET}")
    System.err.print("Enter OTP: ")
    System.err.flush()

    def reader = new BufferedReader(new InputStreamReader(System.in))
    def otp = reader.readLine()?.trim()

    if (!otp) {
        throw new RuntimeException("Operation cancelled - no OTP provided")
    }

    return apiRequestWithSudo(endpoint, method, data, publicKey, secretKey, otp, challengeId)
}

/**
 * Execute a destructive operation with 428 sudo challenge handling.
 */
def executeDestructive(String endpoint, String method, data, String publicKey, String secretKey) {
    try {
        return apiRequestDestructive(endpoint, method, data, publicKey, secretKey)
    } catch (SudoChallengeError e) {
        return handleSudoChallenge(e.challengeId, method, endpoint, data, publicKey, secretKey)
    }
}

def apiRequestText(endpoint, method, body, publicKey, secretKey) {
    def tempFile = File.createTempFile('un_env_', '.txt')
    try {
        if (body) {
            tempFile.text = body
        }

        def curlCmd = ['curl', '-s', '-X', method, "${API_BASE}${endpoint}",
                       '-H', 'Content-Type: text/plain']

        if (secretKey) {
            def timestamp = (System.currentTimeMillis() / 1000) as long
            def message = "${timestamp}:${method}:${endpoint}:${body ?: ''}"

            def mac = Mac.getInstance("HmacSHA256")
            mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
            def signature = mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()

            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
            curlCmd += ['-H', "X-Timestamp: ${timestamp}"]
            curlCmd += ['-H', "X-Signature: ${signature}"]
        } else {
            curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
        }

        if (body) {
            curlCmd += ['--data-binary', "@${tempFile.absolutePath}"]
        }

        def proc = curlCmd.execute()
        def output = proc.text
        proc.waitFor()

        return proc.exitValue() == 0
    } finally {
        tempFile.delete()
    }
}

// ============================================================================
// Core Library Functions
// ============================================================================

/**
 * Execute code synchronously and return results.
 *
 * @param language Programming language (python, javascript, go, rust, etc.)
 * @param code Source code to execute
 * @param options Optional parameters:
 *   <ul>
 *     <li>env: Map of environment variables</li>
 *     <li>inputFiles: List of [filename: "...", content: "..."] or [filename: "...", contentBase64: "..."]</li>
 *     <li>networkMode: "zerotrust" (no network) or "semitrusted" (internet access)</li>
 *     <li>ttl: Execution timeout in seconds (1-900, default 60)</li>
 *     <li>vcpu: Virtual CPUs (1-8, default 1)</li>
 *     <li>returnArtifact: Return compiled binary</li>
 *     <li>returnWasmArtifact: Compile to WebAssembly</li>
 *     <li>publicKey: API public key</li>
 *     <li>secretKey: API secret key</li>
 *   </ul>
 * @return Map with keys: success, stdout, stderr, exit_code, language, job_id, total_time_ms, network_mode, artifacts
 * @throws AuthenticationError Invalid or missing credentials
 * @throws ExecutionError Code execution failed
 * @throws APIError API request failed
 *
 * <pre>{@code
 * def result = un.execute("python", 'print("Hello World")')
 * println result.stdout  // "Hello World\n"
 * }</pre>
 */
def execute(String language, String code, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(
        options.publicKey,
        options.secretKey,
        options.accountIndex ?: 0
    )

    def payload = [
        language: language,
        code: code,
        network_mode: options.networkMode ?: 'zerotrust',
        ttl: options.ttl ?: DEFAULT_TTL,
        vcpu: options.vcpu ?: 1
    ]

    if (options.env) {
        payload.env = options.env
    }

    if (options.inputFiles) {
        payload.input_files = options.inputFiles.collect { f ->
            if (f.contentBase64 || f.content_base64) {
                return [filename: f.filename, content_base64: f.contentBase64 ?: f.content_base64]
            } else if (f.content) {
                return [filename: f.filename, content_base64: f.content.bytes.encodeBase64().toString()]
            }
            return f
        }
    }

    if (options.returnArtifact) payload.return_artifact = true
    if (options.returnWasmArtifact) payload.return_wasm_artifact = true

    return apiRequest('/execute', 'POST', payload, publicKey, secretKey)
}

/**
 * Execute code asynchronously. Returns immediately with job_id for polling.
 *
 * @param language Programming language
 * @param code Source code to execute
 * @param options Same options as execute()
 * @return Map with keys: job_id, status ("pending")
 *
 * <pre>{@code
 * def job = un.executeAsync("python", longRunningCode)
 * println "Job submitted: ${job.job_id}"
 * def result = un.wait(job.job_id)
 * }</pre>
 */
def executeAsync(String language, String code, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(
        options.publicKey,
        options.secretKey,
        options.accountIndex ?: 0
    )

    def payload = [
        language: language,
        code: code,
        network_mode: options.networkMode ?: 'zerotrust',
        ttl: options.ttl ?: DEFAULT_TTL,
        vcpu: options.vcpu ?: 1
    ]

    if (options.env) payload.env = options.env
    if (options.inputFiles) {
        payload.input_files = options.inputFiles.collect { f ->
            if (f.contentBase64 || f.content_base64) {
                return [filename: f.filename, content_base64: f.contentBase64 ?: f.content_base64]
            } else if (f.content) {
                return [filename: f.filename, content_base64: f.content.bytes.encodeBase64().toString()]
            }
            return f
        }
    }
    if (options.returnArtifact) payload.return_artifact = true
    if (options.returnWasmArtifact) payload.return_wasm_artifact = true

    return apiRequest('/execute/async', 'POST', payload, publicKey, secretKey)
}

/**
 * Execute code with automatic language detection from shebang.
 *
 * @param code Source code with shebang (e.g., #!/usr/bin/env python3)
 * @param options Optional parameters (env, networkMode, ttl, publicKey, secretKey)
 * @return Map with keys: success, stdout, stderr, exit_code, detected_language, ...
 *
 * <pre>{@code
 * def code = '''#!/usr/bin/env python3
 * print("Auto-detected!")
 * '''
 * def result = un.run(code)
 * println result.detected_language  // "python"
 * }</pre>
 */
def run(String code, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(
        options.publicKey,
        options.secretKey,
        options.accountIndex ?: 0
    )

    def ttl = options.ttl ?: DEFAULT_TTL
    def networkMode = options.networkMode ?: 'zerotrust'
    def endpoint = "/run?ttl=${ttl}&network_mode=${networkMode}"

    if (options.env) {
        endpoint += "&env=${URLEncoder.encode(JsonOutput.toJson(options.env), 'UTF-8')}"
    }

    return apiRequest(endpoint, 'POST', code, publicKey, secretKey, DEFAULT_TIMEOUT, 'text/plain')
}

/**
 * Execute code asynchronously with automatic language detection.
 *
 * @param code Source code with shebang
 * @param options Optional parameters
 * @return Map with keys: job_id, detected_language, status ("pending")
 */
def runAsync(String code, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(
        options.publicKey,
        options.secretKey,
        options.accountIndex ?: 0
    )

    def ttl = options.ttl ?: DEFAULT_TTL
    def networkMode = options.networkMode ?: 'zerotrust'
    def endpoint = "/run/async?ttl=${ttl}&network_mode=${networkMode}"

    if (options.env) {
        endpoint += "&env=${URLEncoder.encode(JsonOutput.toJson(options.env), 'UTF-8')}"
    }

    return apiRequest(endpoint, 'POST', code, publicKey, secretKey, DEFAULT_TIMEOUT, 'text/plain')
}

// ============================================================================
// Job Management
// ============================================================================

/**
 * Get job status and results.
 *
 * @param jobId Job ID from executeAsync or runAsync
 * @param options Optional parameters (publicKey, secretKey)
 * @return Map with keys: job_id, status, result (if completed), timestamps
 *
 * <p>Status values: pending, running, completed, failed, timeout, cancelled</p>
 */
def getJob(String jobId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/jobs/${jobId}", 'GET', null, publicKey, secretKey)
}

/**
 * Wait for job completion with exponential backoff polling.
 *
 * @param jobId Job ID from executeAsync or runAsync
 * @param options Optional parameters:
 *   <ul>
 *     <li>maxPolls: Maximum number of poll attempts (default 100)</li>
 *     <li>publicKey: API public key</li>
 *     <li>secretKey: API secret key</li>
 *   </ul>
 * @return Final job result Map
 * @throws TimeoutError Max polls exceeded
 * @throws ExecutionError Job failed
 *
 * <pre>{@code
 * def job = un.executeAsync("python", code)
 * def result = un.wait(job.job_id)
 * println result.stdout
 * }</pre>
 */
def wait(String jobId, Map options = [:]) {
    def maxPolls = options.maxPolls ?: 100
    def terminalStates = ['completed', 'failed', 'timeout', 'cancelled'] as Set

    for (int i = 0; i < maxPolls; i++) {
        // Exponential backoff delay
        def delayIdx = Math.min(i, POLL_DELAYS.size() - 1)
        Thread.sleep(POLL_DELAYS[delayIdx])

        def result = getJob(jobId, options)
        def status = result.status ?: ''

        if (status in terminalStates) {
            if (status == 'failed') {
                throw new ExecutionError(
                    "Job failed: ${result.error ?: 'Unknown error'}",
                    result.exit_code,
                    result.stderr
                )
            }
            if (status == 'timeout') {
                throw new TimeoutError("Job timed out: ${jobId}")
            }
            return result
        }
    }

    throw new TimeoutError("Max polls (${maxPolls}) exceeded for job ${jobId}")
}

/**
 * Cancel a running job.
 *
 * @param jobId Job ID to cancel
 * @param options Optional parameters (publicKey, secretKey)
 * @return Partial output and artifacts collected before cancellation
 */
def cancelJob(String jobId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/jobs/${jobId}", 'DELETE', null, publicKey, secretKey)
}

/**
 * List all active jobs for this API key.
 *
 * @param options Optional parameters (publicKey, secretKey)
 * @return List of job summary Maps with keys: job_id, language, status, submitted_at
 */
def listJobs(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest('/jobs', 'GET', null, publicKey, secretKey)
    return result.jobs ?: []
}

// ============================================================================
// Image Generation
// ============================================================================

/**
 * Generate images from text prompt.
 *
 * @param prompt Text description of the image to generate
 * @param options Optional parameters:
 *   <ul>
 *     <li>model: Model to use (optional, uses default)</li>
 *     <li>size: Image size (e.g., "1024x1024", "512x512")</li>
 *     <li>quality: "standard" or "hd"</li>
 *     <li>n: Number of images to generate</li>
 *     <li>publicKey: API public key</li>
 *     <li>secretKey: API secret key</li>
 *   </ul>
 * @return Map with keys: images (list of base64 or URLs), created_at
 *
 * <pre>{@code
 * def result = un.image("A sunset over mountains")
 * println result.images[0]
 * }</pre>
 */
def image(String prompt, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)

    def payload = [
        prompt: prompt,
        size: options.size ?: '1024x1024',
        quality: options.quality ?: 'standard',
        n: options.n ?: 1
    ]
    if (options.model) payload.model = options.model

    return apiRequest('/image', 'POST', payload, publicKey, secretKey)
}

// ============================================================================
// Utility Functions
// ============================================================================

/** Cache max age for languages (1 hour in milliseconds) */
def LANGUAGES_CACHE_MAX_AGE = 3600000

/**
 * Get list of supported programming languages.
 *
 * <p>Results are cached in ~/.unsandbox/languages.json for 1 hour.</p>
 *
 * @param options Optional parameters:
 *   <ul>
 *     <li>forceRefresh: Bypass cache and fetch fresh data</li>
 *     <li>publicKey: API public key</li>
 *     <li>secretKey: API secret key</li>
 *   </ul>
 * @return Map with keys: languages (list), count, aliases (map)
 */
def languages(Map options = [:]) {
    def cachePath = new File(System.getProperty('user.home'), '.unsandbox/languages.json')

    // Check cache unless force refresh
    if (!options.forceRefresh && cachePath.exists()) {
        try {
            def cacheAge = System.currentTimeMillis() - cachePath.lastModified()
            if (cacheAge < LANGUAGES_CACHE_MAX_AGE) {
                return new JsonSlurper().parseText(cachePath.text)
            }
        } catch (Exception e) {
            // Cache read failed, fetch from API
        }
    }

    // Fetch from API
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest('/languages', 'GET', null, publicKey, secretKey)

    // Save to cache
    try {
        cachePath.parentFile.mkdirs()
        cachePath.text = JsonOutput.toJson(result)
    } catch (Exception e) {
        // Cache write failed, continue anyway
    }

    return result
}

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Get SDK version string.
 */
def version() {
    return "4.2.0"
}

/**
 * Check API health status.
 */
def healthCheck() {
    try {
        def url = new URL("${API_BASE}/health")
        def connection = url.openConnection() as java.net.HttpURLConnection
        connection.requestMethod = "GET"
        connection.connectTimeout = 5000
        connection.readTimeout = 5000
        return connection.responseCode == 200
    } catch (Exception e) {
        return false
    }
}

/**
 * Generate HMAC-SHA256 signature.
 */
def hmacSign(String secretKey, String message) {
    def mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
    return mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()
}

// ============================================================================
// Session Functions
// ============================================================================

/**
 * List all sessions.
 */
def sessionList(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest('/sessions', 'GET', null, publicKey, secretKey)
    return result.sessions ?: []
}

/**
 * Get session details.
 */
def sessionGet(String sessionId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}", 'GET', null, publicKey, secretKey)
}

/**
 * Create a new session.
 */
def sessionCreate(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [
        network_mode: options.networkMode ?: 'zerotrust',
        shell: options.shell ?: 'bash'
    ]
    if (options.vcpu) payload.vcpu = options.vcpu
    return apiRequest('/sessions', 'POST', payload, publicKey, secretKey)
}

/**
 * Destroy a session.
 */
def sessionDestroy(String sessionId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}", 'DELETE', null, publicKey, secretKey)
}

/**
 * Freeze a session.
 */
def sessionFreeze(String sessionId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}/freeze", 'POST', null, publicKey, secretKey)
}

/**
 * Unfreeze a session.
 */
def sessionUnfreeze(String sessionId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}/unfreeze", 'POST', null, publicKey, secretKey)
}

/**
 * Boost a session.
 */
def sessionBoost(String sessionId, int vcpu = 2, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}/boost", 'POST', [vcpu: vcpu], publicKey, secretKey)
}

/**
 * Unboost a session.
 */
def sessionUnboost(String sessionId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}/unboost", 'POST', null, publicKey, secretKey)
}

/**
 * Execute command in a session.
 */
def sessionExecute(String sessionId, String command, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/sessions/${sessionId}/shell", 'POST', [command: command], publicKey, secretKey)
}

// ============================================================================
// Service Functions
// ============================================================================

/**
 * List all services.
 */
def serviceList(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest('/services', 'GET', null, publicKey, secretKey)
    return result.services ?: []
}

/**
 * Get service details.
 */
def serviceGet(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}", 'GET', null, publicKey, secretKey)
}

/**
 * Create a new service.
 */
def serviceCreate(String name, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [name: name]
    if (options.ports) payload.ports = options.ports.split(',').collect { it.trim().toInteger() }
    if (options.domains) payload.domains = options.domains
    if (options.bootstrap) payload.bootstrap = options.bootstrap
    if (options.networkMode) payload.network_mode = options.networkMode
    def result = apiRequest('/services', 'POST', payload, publicKey, secretKey)
    return result.id
}

/**
 * Destroy a service.
 */
def serviceDestroy(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/services/${serviceId}", 'DELETE', null, publicKey, secretKey)
}

/**
 * Freeze a service.
 */
def serviceFreeze(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/freeze", 'POST', null, publicKey, secretKey)
}

/**
 * Unfreeze a service.
 */
def serviceUnfreeze(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/unfreeze", 'POST', null, publicKey, secretKey)
}

/**
 * Lock a service.
 */
def serviceLock(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/lock", 'POST', null, publicKey, secretKey)
}

/**
 * Unlock a service.
 */
def serviceUnlock(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/services/${serviceId}/unlock", 'POST', null, publicKey, secretKey)
}

/**
 * Set unfreeze on demand for a service.
 */
def serviceSetUnfreezeOnDemand(String serviceId, boolean enabled, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequestPatch("/services/${serviceId}", [unfreeze_on_demand: enabled], publicKey, secretKey)
}

/**
 * Redeploy a service.
 */
def serviceRedeploy(String serviceId, String bootstrap = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = bootstrap ? [bootstrap: bootstrap] : [:]
    return apiRequest("/services/${serviceId}/redeploy", 'POST', payload, publicKey, secretKey)
}

/**
 * Get service logs.
 */
def serviceLogs(String serviceId, boolean allLogs = false, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def path = allLogs ? "/services/${serviceId}/logs?all=true" : "/services/${serviceId}/logs"
    def result = apiRequest(path, 'GET', null, publicKey, secretKey)
    return result.logs
}

/**
 * Execute command in a service.
 */
def serviceExecute(String serviceId, String command, int timeoutMs = 0, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [command: command]
    if (timeoutMs > 0) payload.timeout = timeoutMs
    return apiRequest("/services/${serviceId}/execute", 'POST', payload, publicKey, secretKey)
}

/**
 * Get service environment vault status.
 */
def serviceEnvGet(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/env", 'GET', null, publicKey, secretKey)
}

/**
 * Set service environment vault.
 */
def serviceEnvSet(String serviceId, String envContent, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequestText("/services/${serviceId}/env", 'PUT', envContent, publicKey, secretKey)
}

/**
 * Delete service environment vault.
 */
def serviceEnvDelete(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/env", 'DELETE', null, publicKey, secretKey)
}

/**
 * Export service environment vault.
 */
def serviceEnvExport(String serviceId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/services/${serviceId}/env/export", 'POST', [:], publicKey, secretKey)
}

/**
 * Resize a service.
 */
def serviceResize(String serviceId, int vcpu, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequestPatch("/services/${serviceId}", [vcpu: vcpu], publicKey, secretKey)
}

// ============================================================================
// Snapshot Functions
// ============================================================================

/**
 * List all snapshots.
 */
def snapshotList(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest('/snapshots', 'GET', null, publicKey, secretKey)
    return result.snapshots ?: []
}

/**
 * Get snapshot details.
 */
def snapshotGet(String snapshotId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/snapshots/${snapshotId}", 'GET', null, publicKey, secretKey)
}

/**
 * Create snapshot from session.
 */
def snapshotSession(String sessionId, String name = null, boolean hot = false, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [session_id: sessionId, hot: hot]
    if (name) payload.name = name
    def result = apiRequest('/snapshots', 'POST', payload, publicKey, secretKey)
    return result.snapshot_id
}

/**
 * Create snapshot from service.
 */
def snapshotService(String serviceId, String name = null, boolean hot = false, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [service_id: serviceId, hot: hot]
    if (name) payload.name = name
    def result = apiRequest('/snapshots', 'POST', payload, publicKey, secretKey)
    return result.snapshot_id
}

/**
 * Restore a snapshot.
 */
def snapshotRestore(String snapshotId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/snapshots/${snapshotId}/restore", 'POST', [:], publicKey, secretKey)
}

/**
 * Delete a snapshot.
 */
def snapshotDelete(String snapshotId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/snapshots/${snapshotId}", 'DELETE', null, publicKey, secretKey)
}

/**
 * Lock a snapshot.
 */
def snapshotLock(String snapshotId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/snapshots/${snapshotId}/lock", 'POST', null, publicKey, secretKey)
}

/**
 * Unlock a snapshot.
 */
def snapshotUnlock(String snapshotId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/snapshots/${snapshotId}/unlock", 'POST', null, publicKey, secretKey)
}

/**
 * Clone a snapshot.
 */
def snapshotClone(String snapshotId, String cloneType, String name = null, String ports = null, String shell = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [type: cloneType]
    if (name) payload.name = name
    if (ports) payload.ports = ports.split(',').collect { it.trim().toInteger() }
    if (shell) payload.shell = shell
    def result = apiRequest("/snapshots/${snapshotId}/clone", 'POST', payload, publicKey, secretKey)
    return result.session_id ?: result.service_id
}

// ============================================================================
// Image Functions
// ============================================================================

/**
 * List all images.
 */
def imageList(String filter = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def path = filter ? "/images/${filter}" : '/images'
    def result = apiRequest(path, 'GET', null, publicKey, secretKey)
    return result.images ?: []
}

/**
 * Get image details.
 */
def imageGet(String imageId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}", 'GET', null, publicKey, secretKey)
}

/**
 * Publish an image.
 */
def imagePublish(String sourceType, String sourceId, String name = null, String description = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [source_type: sourceType, source_id: sourceId]
    if (name) payload.name = name
    if (description) payload.description = description
    def result = apiRequest('/images', 'POST', payload, publicKey, secretKey)
    return result.image_id
}

/**
 * Delete an image.
 */
def imageDelete(String imageId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/images/${imageId}", 'DELETE', null, publicKey, secretKey)
}

/**
 * Lock an image.
 */
def imageLock(String imageId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}/lock", 'POST', null, publicKey, secretKey)
}

/**
 * Unlock an image.
 */
def imageUnlock(String imageId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return executeDestructive("/images/${imageId}/unlock", 'POST', null, publicKey, secretKey)
}

/**
 * Set image visibility.
 */
def imageSetVisibility(String imageId, String visibility, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}/visibility", 'POST', [visibility: visibility], publicKey, secretKey)
}

/**
 * Grant access to an image.
 */
def imageGrantAccess(String imageId, String trustedApiKey, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}/grant", 'POST', [trusted_api_key: trustedApiKey], publicKey, secretKey)
}

/**
 * Revoke access to an image.
 */
def imageRevokeAccess(String imageId, String trustedApiKey, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}/revoke", 'POST', [trusted_api_key: trustedApiKey], publicKey, secretKey)
}

/**
 * List trusted keys for an image.
 */
def imageListTrusted(String imageId, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def result = apiRequest("/images/${imageId}/trusted", 'GET', null, publicKey, secretKey)
    return result.trusted ?: []
}

/**
 * Transfer image ownership.
 */
def imageTransfer(String imageId, String toApiKey, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    return apiRequest("/images/${imageId}/transfer", 'POST', [to_api_key: toApiKey], publicKey, secretKey)
}

/**
 * Spawn a service from an image.
 */
def imageSpawn(String imageId, String name = null, String ports = null, String bootstrap = null, String networkMode = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [:]
    if (name) payload.name = name
    if (ports) payload.ports = ports.split(',').collect { it.trim().toInteger() }
    if (bootstrap) payload.bootstrap = bootstrap
    if (networkMode) payload.network_mode = networkMode
    def result = apiRequest("/images/${imageId}/spawn", 'POST', payload, publicKey, secretKey)
    return result.service_id
}

/**
 * Clone an image.
 */
def imageClone(String imageId, String name = null, String description = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def payload = [:]
    if (name) payload.name = name
    if (description) payload.description = description
    def result = apiRequest("/images/${imageId}/clone", 'POST', payload, publicKey, secretKey)
    return result.image_id
}

// ============================================================================
// PaaS Logs Functions
// ============================================================================

/**
 * Fetch batch logs.
 */
def logsFetch(String source = 'all', int lines = 100, String since = null, String grep = null, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)
    def params = ["source=${source}", "lines=${lines}"]
    if (since) params << "since=${since}"
    if (grep) params << "grep=${URLEncoder.encode(grep, 'UTF-8')}"
    return apiRequest("/paas/logs?${params.join('&')}", 'GET', null, publicKey, secretKey)
}

/**
 * Callback interface for log streaming.
 */
interface LogCallback {
    void onLogLine(String source, String line)
}

/**
 * Stream logs via SSE. Blocks until interrupted or server closes.
 *
 * @param source Log source ('all', 'api', 'portal', 'pool/cammy', 'pool/ai')
 * @param grep Optional filter pattern
 * @param callback Callback for each log line
 * @param options Optional parameters (publicKey, secretKey)
 * @return true on clean shutdown, false on error
 */
def logsStream(String source = 'all', String grep = null, LogCallback callback, Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)

    def path = "/paas/logs/stream?source=${source ?: 'all'}"
    if (grep) {
        path += "&grep=${URLEncoder.encode(grep, 'UTF-8')}"
    }

    def timestamp = (System.currentTimeMillis() / 1000) as long
    def signature = signRequest(secretKey, timestamp, 'GET', path, '')

    def url = new URL("${API_BASE}${path}")
    def connection = url.openConnection() as java.net.HttpURLConnection

    connection.requestMethod = 'GET'
    connection.setRequestProperty('Authorization', "Bearer ${publicKey}")
    connection.setRequestProperty('X-Timestamp', timestamp.toString())
    connection.setRequestProperty('X-Signature', signature)
    connection.setRequestProperty('Accept', 'text/event-stream')
    connection.connectTimeout = 30000
    connection.readTimeout = 0  // No timeout for streaming

    if (connection.responseCode != 200) {
        return false
    }

    try {
        def reader = new BufferedReader(new InputStreamReader(connection.inputStream, 'UTF-8'))
        def currentSource = source ?: 'all'
        def line

        while ((line = reader.readLine()) != null) {
            if (line.startsWith('data: ')) {
                def data = line.substring(6)
                if (callback) {
                    callback.onLogLine(currentSource, data)
                }
            } else if (line.startsWith('event: ')) {
                currentSource = line.substring(7)
            }
        }
        return true
    } catch (Exception e) {
        return false
    }
}

/**
 * Validate API keys.
 */
def validateKeys(Map options = [:]) {
    def (publicKey, secretKey) = getCredentials(options.publicKey, options.secretKey)

    def timestamp = (System.currentTimeMillis() / 1000) as long
    def message = "${timestamp}:POST:/keys/validate:{}"
    def signature = signRequest(secretKey, timestamp, 'POST', '/keys/validate', '{}')

    def url = new URL("${PORTAL_BASE}/keys/validate")
    def connection = url.openConnection() as java.net.HttpURLConnection

    connection.requestMethod = 'POST'
    connection.setRequestProperty('Authorization', "Bearer ${publicKey}")
    connection.setRequestProperty('X-Timestamp', timestamp.toString())
    connection.setRequestProperty('X-Signature', signature)
    connection.setRequestProperty('Content-Type', 'application/json')
    connection.connectTimeout = 30000
    connection.readTimeout = 30000
    connection.doOutput = true
    connection.outputStream.withWriter { it.write('{}') }

    if (connection.responseCode !in 200..299) {
        throw new APIError("HTTP ${connection.responseCode}")
    }

    return new JsonSlurper().parseText(connection.inputStream.text)
}

/**
 * Detect programming language from file extension or shebang.
 *
 * @param filename File path
 * @return Language name or null if undetected
 */
def detectLanguage(String filename) {
    def dotIndex = filename.lastIndexOf('.')
    if (dotIndex == -1) return null

    def ext = filename.substring(dotIndex)
    def language = EXT_MAP[ext]
    if (language) return language

    // Try shebang
    try {
        def file = new File(filename)
        if (file.exists()) {
            def firstLine = file.readLines()[0]
            if (firstLine?.startsWith('#!')) {
                if (firstLine.contains('python')) return 'python'
                if (firstLine.contains('node')) return 'javascript'
                if (firstLine.contains('ruby')) return 'ruby'
                if (firstLine.contains('perl')) return 'perl'
                if (firstLine.contains('bash') || firstLine.contains('/sh')) return 'bash'
                if (firstLine.contains('lua')) return 'lua'
                if (firstLine.contains('php')) return 'php'
            }
        }
    } catch (Exception e) {
        // Ignore file read errors
    }

    return null
}

// ============================================================================
// Client Class
// ============================================================================

/**
 * Unsandbox API client with stored credentials.
 *
 * <p>Use the Client class when making multiple API calls to avoid
 * repeated credential resolution.</p>
 *
 * <pre>{@code
 * // With explicit credentials
 * def client = new un.Client(publicKey: "unsb-pk-...", secretKey: "unsb-sk-...")
 * def result = client.execute("python", 'print("Hello")')
 *
 * // Or load from environment/config automatically
 * def client = new un.Client()
 * def result = client.execute("python", code)
 * }</pre>
 *
 * @author Permacomputer Project
 */
class Client {
    String publicKey
    String secretKey

    /**
     * Initialize client with credentials.
     *
     * @param options Optional parameters:
     *   <ul>
     *     <li>publicKey: API public key (unsb-pk-...)</li>
     *     <li>secretKey: API secret key (unsb-sk-...)</li>
     *     <li>accountIndex: Account index in ~/.unsandbox/accounts.csv (default 0)</li>
     *   </ul>
     */
    Client(Map options = [:]) {
        def creds = getCredentialsStatic(
            options.publicKey,
            options.secretKey,
            options.accountIndex ?: 0
        )
        this.publicKey = creds[0]
        this.secretKey = creds[1]
    }

    private static getCredentialsStatic(String publicKey, String secretKey, int accountIndex) {
        if (publicKey && secretKey) {
            return [publicKey, secretKey]
        }

        def envPk = System.getenv('UNSANDBOX_PUBLIC_KEY')
        def envSk = System.getenv('UNSANDBOX_SECRET_KEY')
        if (envPk && envSk) {
            return [envPk, envSk]
        }

        def accountsPath = new File(System.getProperty('user.home'), '.unsandbox/accounts.csv')
        if (accountsPath.exists()) {
            try {
                def lines = accountsPath.text.trim().split('\n')
                def validAccounts = []
                lines.each { line ->
                    def trimmed = line.trim()
                    if (!trimmed || trimmed.startsWith('#')) return
                    if (trimmed.contains(',')) {
                        def parts = trimmed.split(',', 2)
                        def pk = parts[0]
                        def sk = parts[1]
                        if (pk.startsWith('unsb-pk-') && sk.startsWith('unsb-sk-')) {
                            validAccounts << [pk, sk]
                        }
                    }
                }
                if (validAccounts && accountIndex < validAccounts.size()) {
                    return validAccounts[accountIndex]
                }
            } catch (Exception e) {
                // Ignore
            }
        }

        throw new AuthenticationError(
            "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY."
        )
    }

    /**
     * Execute code synchronously.
     * @see #execute(String, String, Map)
     */
    def execute(String language, String code, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.execute(language, code, options)
    }

    /**
     * Execute code asynchronously.
     * @see #executeAsync(String, String, Map)
     */
    def executeAsync(String language, String code, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.executeAsync(language, code, options)
    }

    /**
     * Execute with auto-detect.
     * @see #run(String, Map)
     */
    def run(String code, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.run(code, options)
    }

    /**
     * Execute async with auto-detect.
     * @see #runAsync(String, Map)
     */
    def runAsync(String code, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.runAsync(code, options)
    }

    /**
     * Get job status.
     * @see #getJob(String, Map)
     */
    def getJob(String jobId) {
        return binding.getJob(jobId, [publicKey: this.publicKey, secretKey: this.secretKey])
    }

    /**
     * Wait for job completion.
     * @see #wait(String, Map)
     */
    def wait(String jobId, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.wait(jobId, options)
    }

    /**
     * Cancel a job.
     * @see #cancelJob(String, Map)
     */
    def cancelJob(String jobId) {
        return binding.cancelJob(jobId, [publicKey: this.publicKey, secretKey: this.secretKey])
    }

    /**
     * List active jobs.
     * @see #listJobs(Map)
     */
    def listJobs() {
        return binding.listJobs([publicKey: this.publicKey, secretKey: this.secretKey])
    }

    /**
     * Generate image.
     * @see #image(String, Map)
     */
    def image(String prompt, Map options = [:]) {
        options.publicKey = this.publicKey
        options.secretKey = this.secretKey
        return binding.image(prompt, options)
    }

    /**
     * Get supported languages.
     * @see #languages(Map)
     */
    def languages() {
        return binding.languages([publicKey: this.publicKey, secretKey: this.secretKey])
    }
}

// ============================================================================
// CLI Support Classes and Functions
// ============================================================================

class Args {
    String command = null
    String sourceFile = null
    String inlineLang = null
    String apiKey = null
    String network = null
    Integer vcpu = 0
    List<String> env = []
    List<String> files = []
    Boolean artifacts = false
    String outputDir = null
    Boolean sessionList = false
    String sessionShell = null
    String sessionKill = null
    String sessionSnapshot = null
    String sessionRestore = null
    String sessionFrom = null
    String sessionSnapshotName = null
    Boolean sessionHot = false
    Boolean serviceList = false
    String serviceName = null
    String servicePorts = null
    String serviceType = null
    String serviceBootstrap = null
    String serviceBootstrapFile = null
    String serviceInfo = null
    String serviceLogs = null
    String serviceTail = null
    String serviceSleep = null
    String serviceWake = null
    String serviceDestroy = null
    String serviceExecute = null
    String serviceCommand = null
    String serviceDumpBootstrap = null
    String serviceDumpFile = null
    String serviceResize = null
    String serviceSetUnfreezeOnDemand = null
    String serviceUnfreezeOnDemandValue = null
    String serviceSnapshot = null
    String serviceRestore = null
    String serviceFrom = null
    String serviceSnapshotName = null
    Boolean serviceHot = false
    Boolean snapshotList = false
    String snapshotInfo = null
    String snapshotDelete = null
    String snapshotClone = null
    String snapshotType = null
    String snapshotName = null
    String snapshotShell = null
    String snapshotPorts = null
    Boolean keyExtend = false
    Boolean imageList = false
    String imageInfo = null
    String imageDelete = null
    String imageLock = null
    String imageUnlock = null
    String imagePublish = null
    String imageSourceType = null
    String imageVisibility = null
    String imageVisibilityMode = null
    String imageSpawn = null
    String imageClone = null
    String imageName = null
    String imagePorts = null
    List<String> svcEnvs = []
    String svcEnvFile = null
    String envAction = null
    String envTarget = null
    Boolean jsonOutput = false
}

def readEnvFile(filename) {
    def file = new File(filename)
    if (!file.exists()) {
        System.err.println("${RED}Error: Cannot read env file: ${filename}${RESET}")
        return ''
    }
    return file.text
}

def buildEnvContent(envs, envFile) {
    def result = new StringBuilder()

    envs.each { env ->
        result.append(env).append('\n')
    }

    if (envFile) {
        def content = readEnvFile(envFile)
        content.split('\n').each { line ->
            def trimmed = line.trim()
            if (trimmed && !trimmed.startsWith('#')) {
                result.append(trimmed).append('\n')
            }
        }
    }

    return result.toString()
}

def serviceEnvSet(serviceId, content, publicKey, secretKey) {
    return apiRequestText("/services/${serviceId}/env", 'PUT', content, publicKey, secretKey)
}

def cmdServiceEnv(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    switch (args.envAction) {
        case 'status':
            def output = apiRequest("/services/${args.envTarget}/env", 'GET', null, publicKey, secretKey)
            println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
            break
        case 'set':
            if (!args.svcEnvs && !args.svcEnvFile) {
                System.err.println("${RED}Error: No environment variables specified. Use -e KEY=VALUE or --env-file FILE${RESET}")
                return
            }
            def content = buildEnvContent(args.svcEnvs, args.svcEnvFile)
            if (content.length() > MAX_ENV_CONTENT_SIZE) {
                System.err.println("${RED}Error: Environment content exceeds 64KB limit${RESET}")
                return
            }
            if (serviceEnvSet(args.envTarget, content, publicKey, secretKey)) {
                println("${GREEN}Vault updated for service ${args.envTarget}${RESET}")
            }
            break
        case 'export':
            def output = apiRequest("/services/${args.envTarget}/env/export", 'POST', null, publicKey, secretKey)
            println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
            break
        case 'delete':
            apiRequest("/services/${args.envTarget}/env", 'DELETE', null, publicKey, secretKey)
            println("${GREEN}Vault deleted for service ${args.envTarget}${RESET}")
            break
        default:
            System.err.println("${RED}Error: Unknown env action: ${args.envAction}${RESET}")
            System.err.println("Usage: un service env <status|set|export|delete> <service_id>")
    }
}

def cmdExecute(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    String code
    String language

    if (args.inlineLang) {
        language = args.inlineLang
        code = args.sourceFile ?: ""
    } else {
        def file = new File(args.sourceFile)
        if (!file.exists()) {
            System.err.println("${RED}Error: File not found: ${args.sourceFile}${RESET}")
            System.exit(1)
        }
        code = file.text
        language = detectLanguage(args.sourceFile)
        if (!language) {
            System.err.println("${RED}Error: Cannot detect language for ${args.sourceFile}${RESET}")
            System.exit(1)
        }
    }

    def options = [
        networkMode: args.network ?: 'zerotrust',
        vcpu: args.vcpu > 0 ? args.vcpu : 1,
        publicKey: publicKey,
        secretKey: secretKey
    ]

    if (args.env) {
        def envMap = [:]
        args.env.each { e ->
            def parts = e.split('=', 2)
            if (parts.size() == 2) {
                envMap[parts[0]] = parts[1]
            }
        }
        if (envMap) options.env = envMap
    }

    if (args.files) {
        options.inputFiles = args.files.collect { filepath ->
            def f = new File(filepath)
            if (!f.exists()) {
                System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                System.exit(1)
            }
            return [filename: f.name, contentBase64: f.bytes.encodeBase64().toString()]
        }
    }

    if (args.artifacts) {
        options.returnArtifact = true
    }

    def result = execute(language, code, options)

    if (result.stdout) {
        print("${BLUE}${result.stdout}${RESET}")
    }
    if (result.stderr) {
        System.err.print("${RED}${result.stderr}${RESET}")
    }

    if (args.artifacts && result.artifacts) {
        def outDir = args.outputDir ?: '.'
        new File(outDir).mkdirs()
        result.artifacts.each { artifact ->
            def filename = artifact.filename ?: 'artifact'
            def content = artifact.content_base64.decodeBase64()
            def filepath = new File(outDir, filename)
            filepath.bytes = content
            "chmod 755 ${filepath.absolutePath}".execute().waitFor()
            System.err.println("${GREEN}Saved: ${filepath.absolutePath}${RESET}")
        }
    }

    System.exit(result.exit_code ?: 0)
}

def cmdSession(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.sessionSnapshot) {
        def payload = [:]
        if (args.sessionSnapshotName) payload.name = args.sessionSnapshotName
        if (args.sessionHot) payload.hot = true
        def output = apiRequest("/sessions/${args.sessionSnapshot}/snapshot", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Snapshot created${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.sessionRestore) {
        def output = apiRequest("/snapshots/${args.sessionRestore}/restore", 'POST', [:], publicKey, secretKey)
        println("${GREEN}Session restored from snapshot${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.sessionList) {
        def output = apiRequest('/sessions', 'GET', null, publicKey, secretKey)
        def sessions = output.sessions ?: []
        if (sessions.isEmpty()) {
            println("No active sessions")
        } else {
            println(String.format("%-40s %-10s %-10s %s", "ID", "Shell", "Status", "Created"))
            sessions.each { s ->
                println(String.format("%-40s %-10s %-10s %s",
                    s.id ?: '', s.shell ?: '', s.status ?: '', s.created_at ?: ''))
            }
        }
        return
    }

    if (args.sessionKill) {
        apiRequest("/sessions/${args.sessionKill}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Session terminated: ${args.sessionKill}${RESET}")
        return
    }

    def payload = [shell: args.sessionShell ?: 'bash']
    if (args.network) payload.network = args.network
    if (args.vcpu > 0) payload.vcpu = args.vcpu

    if (args.files) {
        payload.input_files = args.files.collect { filepath ->
            def f = new File(filepath)
            if (!f.exists()) {
                System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                System.exit(1)
            }
            return [filename: f.name, content_base64: f.bytes.encodeBase64().toString()]
        }
    }

    println("${YELLOW}Creating session...${RESET}")
    def output = apiRequest('/sessions', 'POST', payload, publicKey, secretKey)
    println("${GREEN}Session created: ${output.id ?: 'unknown'}${RESET}")
    println("${YELLOW}(Interactive sessions require WebSocket - use un2 for full support)${RESET}")
}

def openBrowser(url) {
    def osName = System.getProperty('os.name').toLowerCase()
    try {
        if (osName.contains('linux')) {
            Runtime.runtime.exec(['xdg-open', url] as String[])
        } else if (osName.contains('mac')) {
            Runtime.runtime.exec(['open', url] as String[])
        } else if (osName.contains('win')) {
            Runtime.runtime.exec(['cmd', '/c', 'start', url] as String[])
        }
    } catch (Exception e) {
        System.err.println("${RED}Error opening browser: ${e.message}${RESET}")
    }
}

def cmdSnapshot(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.snapshotList) {
        def output = apiRequest('/snapshots', 'GET', null, publicKey, secretKey)
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.snapshotInfo) {
        def output = apiRequest("/snapshots/${args.snapshotInfo}", 'GET', null, publicKey, secretKey)
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.snapshotDelete) {
        executeDestructive("/snapshots/${args.snapshotDelete}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Snapshot deleted: ${args.snapshotDelete}${RESET}")
        return
    }

    if (args.snapshotClone) {
        if (!args.snapshotType) {
            System.err.println("${RED}Error: --type required (session or service)${RESET}")
            System.exit(1)
        }
        def payload = [type: args.snapshotType]
        if (args.snapshotName) payload.name = args.snapshotName
        if (args.snapshotShell) payload.shell = args.snapshotShell
        if (args.snapshotPorts) payload.ports = args.snapshotPorts.split(',').collect { it.trim().toInteger() }
        def output = apiRequest("/snapshots/${args.snapshotClone}/clone", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Created from snapshot${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    System.err.println("Error: Use --list, --info ID, --delete ID, or --clone ID --type TYPE")
    System.exit(1)
}

def cmdImage(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.imageList) {
        def output = apiRequest('/images', 'GET', null, publicKey, secretKey)
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.imageInfo) {
        def output = apiRequest("/images/${args.imageInfo}", 'GET', null, publicKey, secretKey)
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.imageDelete) {
        executeDestructive("/images/${args.imageDelete}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Image deleted: ${args.imageDelete}${RESET}")
        return
    }

    if (args.imageLock) {
        apiRequest("/images/${args.imageLock}/lock", 'POST', null, publicKey, secretKey)
        println("${GREEN}Image locked: ${args.imageLock}${RESET}")
        return
    }

    if (args.imageUnlock) {
        executeDestructive("/images/${args.imageUnlock}/unlock", 'POST', null, publicKey, secretKey)
        println("${GREEN}Image unlocked: ${args.imageUnlock}${RESET}")
        return
    }

    if (args.imagePublish) {
        if (!args.imageSourceType) {
            System.err.println("${RED}Error: --source-type required (service or snapshot)${RESET}")
            System.exit(1)
        }
        def payload = [source_type: args.imageSourceType, source_id: args.imagePublish]
        if (args.imageName) payload.name = args.imageName
        def output = apiRequest("/images/publish", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Image published${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.imageVisibility) {
        if (!args.imageVisibilityMode) {
            System.err.println("${RED}Error: --visibility requires MODE (private, unlisted, or public)${RESET}")
            System.exit(1)
        }
        def payload = [visibility: args.imageVisibilityMode]
        apiRequest("/images/${args.imageVisibility}/visibility", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Image visibility set to ${args.imageVisibilityMode}: ${args.imageVisibility}${RESET}")
        return
    }

    if (args.imageSpawn) {
        def payload = [:]
        if (args.imageName) payload.name = args.imageName
        if (args.imagePorts) payload.ports = args.imagePorts.split(',').collect { it.trim().toInteger() }
        def output = apiRequest("/images/${args.imageSpawn}/spawn", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Service spawned from image${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.imageClone) {
        def payload = [:]
        if (args.imageName) payload.name = args.imageName
        def output = apiRequest("/images/${args.imageClone}/clone", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Image cloned${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    System.err.println("${RED}Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID${RESET}")
    System.exit(1)
}

def cmdLanguages(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    def result = languages([publicKey: publicKey, secretKey: secretKey, forceRefresh: true])
    def langList = result.languages ?: []

    if (args.jsonOutput) {
        println(JsonOutput.toJson(langList))
    } else {
        langList.each { lang ->
            println(lang)
        }
    }
}

def cmdKey(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    def curlCmd = ['curl', '-s', '-X', 'POST', "${PORTAL_BASE}/keys/validate",
                   '-H', 'Content-Type: application/json']

    if (secretKey) {
        def timestamp = (System.currentTimeMillis() / 1000) as long
        def message = "${timestamp}:POST:/keys/validate:{}"

        def mac = Mac.getInstance("HmacSHA256")
        mac.init(new SecretKeySpec(secretKey.getBytes("UTF-8"), "HmacSHA256"))
        def signature = mac.doFinal(message.getBytes("UTF-8")).encodeHex().toString()

        curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
        curlCmd += ['-H', "X-Timestamp: ${timestamp}"]
        curlCmd += ['-H', "X-Signature: ${signature}"]
    } else {
        curlCmd += ['-H', "Authorization: Bearer ${publicKey}"]
    }

    curlCmd += ['-d', '{}']

    def proc = curlCmd.execute()
    def output = proc.text
    proc.waitFor()

    if (proc.exitValue() != 0) {
        println("${RED}Invalid${RESET}")
        System.err.println("${RED}Error: Failed to validate key${RESET}")
        System.exit(1)
    }

    def result = new JsonSlurper().parseText(output)

    def fetchedPublicKey = result.public_key ?: 'N/A'
    def tier = result.tier ?: 'N/A'
    def status = result.status ?: 'N/A'
    def expiresAt = result.expires_at ?: 'N/A'
    def timeRemaining = result.time_remaining ?: 'N/A'
    def rateLimit = result.rate_limit ?: 'N/A'
    def burst = result.burst ?: 'N/A'
    def concurrency = result.concurrency ?: 'N/A'
    def expired = result.expired ?: false

    if (args.keyExtend && fetchedPublicKey != 'N/A') {
        def extendUrl = "${PORTAL_BASE}/keys/extend?pk=${fetchedPublicKey}"
        println("${BLUE}Opening browser to extend key...${RESET}")
        openBrowser(extendUrl)
        return
    }

    if (expired) {
        println("${RED}Expired${RESET}")
        println("Public Key: ${fetchedPublicKey}")
        println("Tier: ${tier}")
        println("Expired: ${expiresAt}")
        println("${YELLOW}To renew: Visit https://unsandbox.com/keys/extend${RESET}")
        System.exit(1)
    }

    println("${GREEN}Valid${RESET}")
    println("Public Key: ${fetchedPublicKey}")
    println("Tier: ${tier}")
    println("Status: ${status}")
    println("Expires: ${expiresAt}")
    println("Time Remaining: ${timeRemaining}")
    println("Rate Limit: ${rateLimit}")
    println("Burst: ${burst}")
    println("Concurrency: ${concurrency}")
}

def cmdService(args) {
    def (publicKey, secretKey) = getApiKeys(args.apiKey)

    if (args.serviceSnapshot) {
        def payload = [:]
        if (args.serviceSnapshotName) payload.name = args.serviceSnapshotName
        if (args.serviceHot) payload.hot = true
        def output = apiRequest("/services/${args.serviceSnapshot}/snapshot", 'POST', payload, publicKey, secretKey)
        println("${GREEN}Snapshot created${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.serviceRestore) {
        def output = apiRequest("/snapshots/${args.serviceRestore}/restore", 'POST', [:], publicKey, secretKey)
        println("${GREEN}Service restored from snapshot${RESET}")
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.serviceList) {
        def output = apiRequest('/services', 'GET', null, publicKey, secretKey)
        def services = output.services ?: []
        if (services.isEmpty()) {
            println("No services")
        } else {
            println(String.format("%-20s %-15s %-10s %-15s %s", "ID", "Name", "Status", "Ports", "Domains"))
            services.each { s ->
                def ports = (s.ports ?: []).join(',')
                def domains = (s.domains ?: []).join(',')
                println(String.format("%-20s %-15s %-10s %-15s %s",
                    s.id ?: '', s.name ?: '', s.status ?: '', ports, domains))
            }
        }
        return
    }

    if (args.serviceInfo) {
        def output = apiRequest("/services/${args.serviceInfo}", 'GET', null, publicKey, secretKey)
        println(JsonOutput.prettyPrint(JsonOutput.toJson(output)))
        return
    }

    if (args.serviceLogs) {
        def output = apiRequest("/services/${args.serviceLogs}/logs", 'GET', null, publicKey, secretKey)
        println(output.logs ?: '')
        return
    }

    if (args.serviceTail) {
        def output = apiRequest("/services/${args.serviceTail}/logs?lines=9000", 'GET', null, publicKey, secretKey)
        println(output.logs ?: '')
        return
    }

    if (args.serviceSleep) {
        apiRequest("/services/${args.serviceSleep}/freeze", 'POST', null, publicKey, secretKey)
        println("${GREEN}Service frozen: ${args.serviceSleep}${RESET}")
        return
    }

    if (args.serviceWake) {
        apiRequest("/services/${args.serviceWake}/unfreeze", 'POST', null, publicKey, secretKey)
        println("${GREEN}Service unfreezing: ${args.serviceWake}${RESET}")
        return
    }

    if (args.serviceDestroy) {
        executeDestructive("/services/${args.serviceDestroy}", 'DELETE', null, publicKey, secretKey)
        println("${GREEN}Service destroyed: ${args.serviceDestroy}${RESET}")
        return
    }

    if (args.serviceResize) {
        if (args.vcpu <= 0) {
            System.err.println("${RED}Error: --resize requires --vcpu N (1-8)${RESET}")
            System.exit(1)
        }
        apiRequestPatch("/services/${args.serviceResize}", [vcpu: args.vcpu], publicKey, secretKey)
        def ram = args.vcpu * 2
        println("${GREEN}Service resized to ${args.vcpu} vCPU, ${ram} GB RAM${RESET}")
        return
    }

    if (args.serviceSetUnfreezeOnDemand) {
        def enabledStr = (args.serviceUnfreezeOnDemandValue ?: 'false').toLowerCase()
        def enabled = enabledStr in ['true', '1', 'yes', 'on']
        apiRequestPatch("/services/${args.serviceSetUnfreezeOnDemand}", [unfreeze_on_demand: enabled], publicKey, secretKey)
        println("${GREEN}Service unfreeze_on_demand set to ${enabled}: ${args.serviceSetUnfreezeOnDemand}${RESET}")
        return
    }

    if (args.serviceExecute) {
        def output = apiRequest("/services/${args.serviceExecute}/execute", 'POST',
            [command: args.serviceCommand], publicKey, secretKey)
        if (output.stdout) print("${BLUE}${output.stdout}${RESET}")
        if (output.stderr) System.err.print("${RED}${output.stderr}${RESET}")
        return
    }

    if (args.serviceDumpBootstrap) {
        System.err.println("Fetching bootstrap script from ${args.serviceDumpBootstrap}...")
        def output = apiRequest("/services/${args.serviceDumpBootstrap}/execute", 'POST',
            [command: 'cat /tmp/bootstrap.sh'], publicKey, secretKey)

        if (output.stdout) {
            if (args.serviceDumpFile) {
                try {
                    new File(args.serviceDumpFile).text = output.stdout
                    "chmod 755 ${args.serviceDumpFile}".execute().waitFor()
                    println("Bootstrap saved to ${args.serviceDumpFile}")
                } catch (Exception e) {
                    System.err.println("${RED}Error: Could not write to ${args.serviceDumpFile}: ${e.message}${RESET}")
                    System.exit(1)
                }
            } else {
                print(output.stdout)
            }
        } else {
            System.err.println("${RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${RESET}")
            System.exit(1)
        }
        return
    }

    if (args.serviceName) {
        def payload = [name: args.serviceName]

        if (args.servicePorts) {
            payload.ports = args.servicePorts.split(',').collect { it.trim().toInteger() }
        }
        if (args.serviceType) payload.service_type = args.serviceType
        if (args.serviceBootstrap) payload.bootstrap = args.serviceBootstrap
        if (args.serviceBootstrapFile) {
            def file = new File(args.serviceBootstrapFile)
            if (file.exists()) {
                payload.bootstrap_content = file.text
            } else {
                System.err.println("${RED}Error: Bootstrap file not found: ${args.serviceBootstrapFile}${RESET}")
                System.exit(1)
            }
        }
        if (args.network) payload.network = args.network
        if (args.vcpu > 0) payload.vcpu = args.vcpu

        if (args.files) {
            payload.input_files = args.files.collect { filepath ->
                def f = new File(filepath)
                if (!f.exists()) {
                    System.err.println("${RED}Error: Input file not found: ${filepath}${RESET}")
                    System.exit(1)
                }
                return [filename: f.name, content_base64: f.bytes.encodeBase64().toString()]
            }
        }

        def output = apiRequest('/services', 'POST', payload, publicKey, secretKey)
        def serviceId = output.id
        println("${GREEN}Service created: ${serviceId ?: 'unknown'}${RESET}")
        println("Name: ${output.name ?: ''}")
        if (output.url) println("URL: ${output.url}")

        // Auto-set vault if -e or --env-file provided
        if (serviceId && (args.svcEnvs || args.svcEnvFile)) {
            def envContent = buildEnvContent(args.svcEnvs, args.svcEnvFile)
            if (envContent) {
                if (serviceEnvSet(serviceId, envContent, publicKey, secretKey)) {
                    println("${GREEN}Vault configured for service ${serviceId}${RESET}")
                }
            }
        }
        return
    }

    System.err.println("${RED}Error: Specify --name to create a service, or use --list, --info, etc.${RESET}")
    System.exit(1)
}

def parseArgs(argv) {
    def args = new Args()
    def i = 0
    while (i < argv.size()) {
        switch (argv[i]) {
            case 'languages':
                args.command = 'languages'
                break
            case 'session':
                args.command = 'session'
                break
            case 'service':
                args.command = 'service'
                break
            case 'env':
                if (args.command == 'service' && i + 2 < argv.size()) {
                    args.envAction = argv[++i]
                    args.envTarget = argv[++i]
                }
                break
            case 'snapshot':
                args.command = 'snapshot'
                break
            case 'image':
                args.command = 'image'
                break
            case 'key':
                args.command = 'key'
                break
            case '-s':
                args.inlineLang = argv[++i]
                break
            case '-k':
            case '--api-key':
                args.apiKey = argv[++i]
                break
            case '-p':
            case '--public-key':
                args.apiKey = argv[++i]  // For compatibility
                break
            case '-n':
            case '--network':
                args.network = argv[++i]
                break
            case '-v':
            case '--vcpu':
                args.vcpu = argv[++i].toInteger()
                break
            case '-e':
            case '--env':
                def envVal = argv[++i]
                args.env << envVal
                if (args.command == 'service') {
                    args.svcEnvs << envVal
                }
                break
            case '--env-file':
                args.svcEnvFile = argv[++i]
                break
            case '-f':
            case '--files':
                args.files << argv[++i]
                break
            case '-a':
            case '--artifacts':
                args.artifacts = true
                break
            case '-o':
            case '--output-dir':
                args.outputDir = argv[++i]
                break
            case '-l':
            case '--list':
                if (args.command == 'session') args.sessionList = true
                else if (args.command == 'service') args.serviceList = true
                else if (args.command == 'snapshot') args.snapshotList = true
                else if (args.command == 'image') args.imageList = true
                break
            case '--shell':
                if (args.command == 'snapshot') args.snapshotShell = argv[++i]
                else args.sessionShell = argv[++i]
                break
            case '--kill':
                args.sessionKill = argv[++i]
                break
            case '--snapshot':
                if (args.command == 'session') args.sessionSnapshot = argv[++i]
                else if (args.command == 'service') args.serviceSnapshot = argv[++i]
                break
            case '--restore':
                if (args.command == 'session') args.sessionRestore = argv[++i]
                else if (args.command == 'service') args.serviceRestore = argv[++i]
                break
            case '--from':
                if (args.command == 'session') args.sessionFrom = argv[++i]
                else if (args.command == 'service') args.serviceFrom = argv[++i]
                break
            case '--snapshot-name':
                if (args.command == 'session') args.sessionSnapshotName = argv[++i]
                else if (args.command == 'service') args.serviceSnapshotName = argv[++i]
                break
            case '--hot':
                if (args.command == 'session') args.sessionHot = true
                else if (args.command == 'service') args.serviceHot = true
                break
            case '--info':
                if (args.command == 'snapshot') args.snapshotInfo = argv[++i]
                else if (args.command == 'image') args.imageInfo = argv[++i]
                else args.serviceInfo = argv[++i]
                break
            case '--delete':
                if (args.command == 'snapshot') args.snapshotDelete = argv[++i]
                else if (args.command == 'image') args.imageDelete = argv[++i]
                break
            case '--clone':
                if (args.command == 'image') args.imageClone = argv[++i]
                else args.snapshotClone = argv[++i]
                break
            case '--lock':
                if (args.command == 'image') args.imageLock = argv[++i]
                break
            case '--unlock':
                if (args.command == 'image') args.imageUnlock = argv[++i]
                break
            case '--publish':
                if (args.command == 'image') args.imagePublish = argv[++i]
                break
            case '--source-type':
                args.imageSourceType = argv[++i]
                break
            case '--visibility':
                if (args.command == 'image') {
                    args.imageVisibility = argv[++i]
                    if (i + 1 < argv.size() && !argv[i + 1].startsWith('-')) {
                        args.imageVisibilityMode = argv[++i]
                    }
                }
                break
            case '--spawn':
                if (args.command == 'image') args.imageSpawn = argv[++i]
                break
            case '--type':
                if (args.command == 'snapshot') args.snapshotType = argv[++i]
                else args.serviceType = argv[++i]
                break
            case '--name':
                if (args.command == 'snapshot') args.snapshotName = argv[++i]
                else if (args.command == 'image') args.imageName = argv[++i]
                else args.serviceName = argv[++i]
                break
            case '--ports':
                if (args.command == 'snapshot') args.snapshotPorts = argv[++i]
                else if (args.command == 'image') args.imagePorts = argv[++i]
                else args.servicePorts = argv[++i]
                break
            case '--bootstrap':
                args.serviceBootstrap = argv[++i]
                break
            case '--bootstrap-file':
                args.serviceBootstrapFile = argv[++i]
                break
            case '--logs':
                args.serviceLogs = argv[++i]
                break
            case '--tail':
                args.serviceTail = argv[++i]
                break
            case '--freeze':
                args.serviceSleep = argv[++i]
                break
            case '--unfreeze':
                args.serviceWake = argv[++i]
                break
            case '--destroy':
                args.serviceDestroy = argv[++i]
                break
            case '--resize':
                args.serviceResize = argv[++i]
                break
            case '--set-unfreeze-on-demand':
                args.serviceSetUnfreezeOnDemand = argv[++i]
                if (i + 1 < argv.size() && !argv[i + 1].startsWith('-')) {
                    args.serviceUnfreezeOnDemandValue = argv[++i]
                }
                break
            case '--execute':
                args.serviceExecute = argv[++i]
                break
            case '--command':
                args.serviceCommand = argv[++i]
                break
            case '--dump-bootstrap':
                args.serviceDumpBootstrap = argv[++i]
                break
            case '--dump-file':
                args.serviceDumpFile = argv[++i]
                break
            case '--extend':
                args.keyExtend = true
                break
            case '--json':
                args.jsonOutput = true
                break
            default:
                if (argv[i].startsWith('-')) {
                    System.err.println("${RED}Unknown option: ${argv[i]}${RESET}")
                    System.exit(1)
                } else {
                    args.sourceFile = argv[i]
                }
        }
        i++
    }
    return args
}

def printHelp() {
    println '''unsandbox SDK for Groovy - Execute code in secure sandboxes
https://unsandbox.com | https://api.unsandbox.com/openapi

Usage: groovy un.groovy [options] <source_file>
       groovy un.groovy -s <language> '<code>'
       groovy un.groovy session [options]
       groovy un.groovy service [options]
       groovy un.groovy service env <action> <service_id> [options]
       groovy un.groovy image [options]
       groovy un.groovy languages [--json]
       groovy un.groovy key [options]

Execute options:
  -s LANG             Execute inline code with specified language
  -e KEY=VALUE        Set environment variable
  -f FILE             Add input file
  -a                  Return artifacts
  -o DIR              Output directory for artifacts
  -n MODE             Network mode (zerotrust/semitrusted)
  -v N                vCPU count (1-8)
  -k KEY              API key (legacy)
  -p KEY              Public key

Session options:
  --list              List active sessions
  --shell NAME        Shell/REPL to use
  --kill ID           Terminate session
  --snapshot ID       Create snapshot of session
  --restore ID        Restore session from snapshot

Service options:
  --list              List services
  --name NAME         Service name (creates service)
  --ports PORTS       Comma-separated ports
  --type TYPE         Service type
  --bootstrap CMD     Bootstrap command
  -e KEY=VALUE        Set env var in vault (when creating)
  --env-file FILE     Load env vars from file
  --info ID           Get service details
  --logs ID           Get all logs
  --tail ID           Get last 9000 lines
  --freeze ID         Freeze service
  --unfreeze ID       Unfreeze service
  --destroy ID        Destroy service
  --resize ID         Resize service (requires --vcpu N)
  --set-unfreeze-on-demand ID true|false
                      Set unfreeze_on_demand for service
  --execute ID        Execute command in service
  --command CMD       Command to execute (with --execute)
  --dump-bootstrap ID Dump bootstrap script
  --dump-file FILE    File to save bootstrap

Vault commands:
  service env status <id>   Check vault status
  service env set <id>      Set vault (-e KEY=VAL or --env-file FILE)
  service env export <id>   Export vault contents
  service env delete <id>   Delete vault

Key options:
  --extend            Open browser to extend key

Image options:
  --list              List images
  --info ID           Get image details
  --delete ID         Delete an image
  --lock ID           Lock image to prevent deletion
  --unlock ID         Unlock image
  --publish ID        Publish image from service/snapshot (requires --source-type)
  --source-type TYPE  Source type: service or snapshot
  --visibility ID MODE  Set visibility: private, unlisted, or public
  --spawn ID          Spawn new service from image
  --clone ID          Clone an image
  --name NAME         Name for spawned service or cloned image
  --ports PORTS       Ports for spawned service

Languages options:
  --json              Output as JSON array

Library Usage:
  import un
  def result = un.execute("python", 'print("Hello")')
  def client = new un.Client(publicKey: "unsb-pk-...", secretKey: "unsb-sk-...")
'''
}

// ============================================================================
// Main Execution (CLI)
// ============================================================================

try {
    def args = parseArgs(this.args as List)

    if (args.command == 'languages') {
        cmdLanguages(args)
    } else if (args.command == 'session') {
        cmdSession(args)
    } else if (args.command == 'service') {
        if (args.envAction && args.envTarget) {
            cmdServiceEnv(args)
        } else {
            cmdService(args)
        }
    } else if (args.command == 'snapshot') {
        cmdSnapshot(args)
    } else if (args.command == 'image') {
        cmdImage(args)
    } else if (args.command == 'key') {
        cmdKey(args)
    } else if (args.sourceFile || args.inlineLang) {
        cmdExecute(args)
    } else {
        printHelp()
        System.exit(1)
    }
} catch (UnsandboxError e) {
    System.err.println("${RED}Error: ${e.message}${RESET}")
    System.exit(1)
} catch (Exception e) {
    System.err.println("${RED}Error: ${e.message}${RESET}")
    System.exit(1)
}
