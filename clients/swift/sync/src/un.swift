/*
PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

unsandbox.com Swift SDK (Synchronous)

Library Usage:
    import Foundation
    // Note: In a real project, compile this file and import as module

    // Execute code synchronously
    let result = try executeCode(language: "python", code: "print('hello')")

    // Execute asynchronously (returns job_id)
    let jobId = try executeAsync(language: "javascript", code: "console.log('hello')")

    // Wait for job completion
    let result = try waitForJob(jobId)

    // List all jobs
    let jobs = try listJobs()

    // Get supported languages
    let languages = try getLanguages()

    // Detect language from filename
    let lang = detectLanguage("script.py")  // Returns "python"

    // Session operations
    let sessions = try listSessions()
    let session = try createSession(shell: "python3")
    try deleteSession(sessionId)

    // Service operations
    let services = try listServices()
    let service = try createService(name: "myapp", ports: [80])
    try deleteService(serviceId)

    // Snapshot operations
    let snapshotId = try sessionSnapshot(sessionId, name: "my-snapshot")
    let snapshots = try listSnapshots()
    try deleteSnapshot(snapshotId)

    // Key validation
    let validation = try validateKeys()

    // Image generation
    let result = try image(prompt: "A sunset over mountains")

Authentication Priority (4-tier):
    1. Function arguments (publicKey, secretKey)
    2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
    4. Local directory (./accounts.csv, line 0 by default)

    Format: public_key,secret_key (one per line)
    Account selection: UNSANDBOX_ACCOUNT=N env var (0-based index)

Request Authentication (HMAC-SHA256):
    Authorization: Bearer <public_key>                  (identifies account)
    X-Timestamp: <unix_seconds>                         (replay prevention)
    X-Signature: HMAC-SHA256(secret_key, msg)           (proves secret + body integrity)

    Message format: "timestamp:METHOD:path:body"
    - timestamp: seconds since epoch
    - METHOD: GET, POST, DELETE, etc. (uppercase)
    - path: e.g., "/execute", "/jobs/123"
    - body: JSON payload (empty string for GET/DELETE)

Languages Cache:
    - Cached in ~/.unsandbox/languages.json
    - TTL: 1 hour
    - Updated on successful API calls
*/

import Foundation
#if canImport(CommonCrypto)
import CommonCrypto
#endif

// MARK: - Constants

let API_BASE = "https://api.unsandbox.com"
let PORTAL_BASE = "https://unsandbox.com"
let POLL_DELAYS_MS: [Int] = [300, 450, 700, 900, 650, 1600, 2000]
let LANGUAGES_CACHE_TTL: TimeInterval = 3600  // 1 hour

// MARK: - Errors

enum UnsandboxError: Error, CustomStringConvertible {
    case credentialsNotFound(String)
    case networkError(String)
    case apiError(Int, String)
    case invalidResponse(String)
    case timeout(String)
    case invalidArgument(String)
    case fileNotFound(String)

    var description: String {
        switch self {
        case .credentialsNotFound(let msg): return "Credentials error: \(msg)"
        case .networkError(let msg): return "Network error: \(msg)"
        case .apiError(let code, let msg): return "API error (\(code)): \(msg)"
        case .invalidResponse(let msg): return "Invalid response: \(msg)"
        case .timeout(let msg): return "Timeout: \(msg)"
        case .invalidArgument(let msg): return "Invalid argument: \(msg)"
        case .fileNotFound(let msg): return "File not found: \(msg)"
        }
    }
}

// MARK: - Credentials Resolution

/// Get ~/.unsandbox directory path, creating if necessary
func getUnsandboxDir() -> URL {
    let home = FileManager.default.homeDirectoryForCurrentUser
    let unsandboxDir = home.appendingPathComponent(".unsandbox")

    if !FileManager.default.fileExists(atPath: unsandboxDir.path) {
        try? FileManager.default.createDirectory(at: unsandboxDir, withIntermediateDirectories: true, attributes: [.posixPermissions: 0o700])
    }

    return unsandboxDir
}

/// Load credentials from CSV file (public_key,secret_key per line)
func loadCredentialsFromCSV(_ path: URL, accountIndex: Int = 0) -> (String, String)? {
    guard FileManager.default.fileExists(atPath: path.path) else { return nil }

    do {
        let content = try String(contentsOf: path, encoding: .utf8)
        let lines = content.components(separatedBy: .newlines)
        var index = 0

        for line in lines {
            let trimmed = line.trimmingCharacters(in: .whitespaces)
            if trimmed.isEmpty || trimmed.hasPrefix("#") { continue }

            if index == accountIndex {
                let parts = trimmed.components(separatedBy: ",")
                if parts.count >= 2 {
                    return (parts[0].trimmingCharacters(in: .whitespaces),
                            parts[1].trimmingCharacters(in: .whitespaces))
                }
            }
            index += 1
        }
    } catch {
        return nil
    }

    return nil
}

/// Resolve credentials from 4-tier priority system
func resolveCredentials(publicKey: String? = nil, secretKey: String? = nil, accountIndex: Int? = nil) throws -> (String, String) {
    // Tier 1: Function arguments
    if let pk = publicKey, let sk = secretKey, !pk.isEmpty, !sk.isEmpty {
        return (pk, sk)
    }

    // Tier 2: Environment variables
    if let envPk = ProcessInfo.processInfo.environment["UNSANDBOX_PUBLIC_KEY"],
       let envSk = ProcessInfo.processInfo.environment["UNSANDBOX_SECRET_KEY"],
       !envPk.isEmpty, !envSk.isEmpty {
        return (envPk, envSk)
    }

    // Determine account index
    let idx = accountIndex ?? Int(ProcessInfo.processInfo.environment["UNSANDBOX_ACCOUNT"] ?? "0") ?? 0

    // Tier 3: ~/.unsandbox/accounts.csv
    let unsandboxDir = getUnsandboxDir()
    if let creds = loadCredentialsFromCSV(unsandboxDir.appendingPathComponent("accounts.csv"), accountIndex: idx) {
        return creds
    }

    // Tier 4: ./accounts.csv
    let localPath = URL(fileURLWithPath: FileManager.default.currentDirectoryPath).appendingPathComponent("accounts.csv")
    if let creds = loadCredentialsFromCSV(localPath, accountIndex: idx) {
        return creds
    }

    throw UnsandboxError.credentialsNotFound(
        """
        No credentials found. Please provide via:
          1. Function arguments (publicKey, secretKey)
          2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
          3. ~/.unsandbox/accounts.csv
          4. ./accounts.csv
        """
    )
}

// MARK: - HMAC-SHA256 Signing

/// Sign a request using HMAC-SHA256
func signRequest(secretKey: String, timestamp: Int, method: String, path: String, body: String?) -> String {
    let bodyStr = body ?? ""
    let message = "\(timestamp):\(method):\(path):\(bodyStr)"

    guard let keyData = secretKey.data(using: .utf8),
          let messageData = message.data(using: .utf8) else {
        return ""
    }

    var hmac = [UInt8](repeating: 0, count: Int(CC_SHA256_DIGEST_LENGTH))
    keyData.withUnsafeBytes { keyPtr in
        messageData.withUnsafeBytes { msgPtr in
            CCHmac(CCHmacAlgorithm(kCCHmacAlgSHA256),
                   keyPtr.baseAddress, keyData.count,
                   msgPtr.baseAddress, messageData.count,
                   &hmac)
        }
    }

    return hmac.map { String(format: "%02x", $0) }.joined()
}

// MARK: - HTTP Client

/// Make an authenticated HTTP request to the API
func makeRequest(method: String, path: String, publicKey: String, secretKey: String, data: [String: Any]? = nil) throws -> [String: Any] {
    let url = URL(string: "\(API_BASE)\(path)")!
    var request = URLRequest(url: url)
    request.httpMethod = method
    request.timeoutInterval = 120

    let timestamp = Int(Date().timeIntervalSince1970)
    var bodyStr: String? = nil

    if let data = data {
        let jsonData = try JSONSerialization.data(withJSONObject: data)
        bodyStr = String(data: jsonData, encoding: .utf8)
        request.httpBody = jsonData
    }

    let signature = signRequest(secretKey: secretKey, timestamp: timestamp, method: method, path: path, body: method != "GET" && method != "DELETE" ? bodyStr : nil)

    request.setValue("Bearer \(publicKey)", forHTTPHeaderField: "Authorization")
    request.setValue("\(timestamp)", forHTTPHeaderField: "X-Timestamp")
    request.setValue(signature, forHTTPHeaderField: "X-Signature")
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")

    var result: [String: Any]?
    var requestError: Error?

    let semaphore = DispatchSemaphore(value: 0)

    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        defer { semaphore.signal() }

        if let error = error {
            requestError = UnsandboxError.networkError(error.localizedDescription)
            return
        }

        guard let httpResponse = response as? HTTPURLResponse else {
            requestError = UnsandboxError.invalidResponse("No HTTP response")
            return
        }

        guard let data = data else {
            requestError = UnsandboxError.invalidResponse("No data received")
            return
        }

        if httpResponse.statusCode >= 400 {
            let body = String(data: data, encoding: .utf8) ?? "Unknown error"
            requestError = UnsandboxError.apiError(httpResponse.statusCode, body)
            return
        }

        do {
            if let json = try JSONSerialization.jsonObject(with: data) as? [String: Any] {
                result = json
            } else {
                requestError = UnsandboxError.invalidResponse("Response is not a JSON object")
            }
        } catch {
            requestError = UnsandboxError.invalidResponse("Failed to parse JSON: \(error)")
        }
    }

    task.resume()
    semaphore.wait()

    if let error = requestError {
        throw error
    }

    return result ?? [:]
}

// MARK: - Languages Cache

func getLanguagesCachePath() -> URL {
    return getUnsandboxDir().appendingPathComponent("languages.json")
}

func loadLanguagesCache() -> [String]? {
    let cachePath = getLanguagesCachePath()

    guard FileManager.default.fileExists(atPath: cachePath.path) else { return nil }

    do {
        let attrs = try FileManager.default.attributesOfItem(atPath: cachePath.path)
        guard let mtime = attrs[.modificationDate] as? Date else { return nil }

        let age = Date().timeIntervalSince(mtime)
        if age >= LANGUAGES_CACHE_TTL { return nil }

        let data = try Data(contentsOf: cachePath)
        if let json = try JSONSerialization.jsonObject(with: data) as? [String: Any],
           let languages = json["languages"] as? [String] {
            return languages
        }
    } catch {
        return nil
    }

    return nil
}

func saveLanguagesCache(_ languages: [String]) {
    let cachePath = getLanguagesCachePath()
    let data: [String: Any] = [
        "languages": languages,
        "timestamp": Int(Date().timeIntervalSince1970)
    ]

    do {
        let jsonData = try JSONSerialization.data(withJSONObject: data)
        try jsonData.write(to: cachePath)
    } catch {
        // Cache failures are non-fatal
    }
}

// MARK: - Language Detection

let LANGUAGE_MAP: [String: String] = [
    "py": "python",
    "js": "javascript",
    "ts": "typescript",
    "rb": "ruby",
    "php": "php",
    "pl": "perl",
    "sh": "bash",
    "r": "r",
    "lua": "lua",
    "go": "go",
    "rs": "rust",
    "c": "c",
    "cpp": "cpp",
    "cc": "cpp",
    "cxx": "cpp",
    "java": "java",
    "kt": "kotlin",
    "m": "objc",
    "cs": "csharp",
    "fs": "fsharp",
    "hs": "haskell",
    "ml": "ocaml",
    "clj": "clojure",
    "scm": "scheme",
    "ss": "scheme",
    "erl": "erlang",
    "ex": "elixir",
    "exs": "elixir",
    "jl": "julia",
    "d": "d",
    "nim": "nim",
    "zig": "zig",
    "v": "v",
    "cr": "crystal",
    "dart": "dart",
    "groovy": "groovy",
    "f90": "fortran",
    "f95": "fortran",
    "lisp": "commonlisp",
    "lsp": "commonlisp",
    "cob": "cobol",
    "tcl": "tcl",
    "raku": "raku",
    "pro": "prolog",
    "p": "prolog",
    "4th": "forth",
    "forth": "forth",
    "fth": "forth",
]

/// Detect programming language from filename extension
func detectLanguage(_ filename: String) -> String? {
    guard !filename.isEmpty, filename.contains(".") else { return nil }

    let ext = (filename as NSString).pathExtension.lowercased()
    return LANGUAGE_MAP[ext]
}

// MARK: - Execution Functions

/// Execute code synchronously (blocks until completion)
func executeCode(language: String, code: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "POST", path: "/execute", publicKey: pk, secretKey: sk, data: [
        "language": language,
        "code": code
    ])

    // If we got a job_id, poll until completion
    if let jobId = response["job_id"] as? String,
       let status = response["status"] as? String,
       status == "pending" || status == "running" {
        return try waitForJob(jobId, publicKey: pk, secretKey: sk)
    }

    return response
}

/// Execute code with additional options
func executeCodeWithOptions(
    language: String,
    code: String,
    env: [String: String]? = nil,
    files: [[String: String]]? = nil,
    networkMode: String = "zerotrust",
    vcpu: Int = 1,
    artifacts: Bool = false,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)

    var data: [String: Any] = [
        "language": language,
        "code": code,
        "network_mode": networkMode
    ]

    if let env = env, !env.isEmpty {
        data["env"] = env
    }
    if let files = files, !files.isEmpty {
        data["files"] = files
    }
    if vcpu > 1 {
        data["vcpu"] = vcpu
    }
    if artifacts {
        data["artifacts"] = true
    }

    let response = try makeRequest(method: "POST", path: "/execute", publicKey: pk, secretKey: sk, data: data)

    if let jobId = response["job_id"] as? String,
       let status = response["status"] as? String,
       status == "pending" || status == "running" {
        return try waitForJob(jobId, publicKey: pk, secretKey: sk)
    }

    return response
}

/// Execute code asynchronously (returns immediately with job_id)
func executeAsync(language: String, code: String, publicKey: String? = nil, secretKey: String? = nil) throws -> String {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "POST", path: "/execute", publicKey: pk, secretKey: sk, data: [
        "language": language,
        "code": code
    ])
    return response["job_id"] as? String ?? ""
}

/// Get current status/result of a job (single poll, no waiting)
func getJob(_ jobId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/jobs/\(jobId)", publicKey: pk, secretKey: sk)
}

/// Wait for job completion with exponential backoff polling
func waitForJob(_ jobId: String, publicKey: String? = nil, secretKey: String? = nil, timeout: TimeInterval? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var pollCount = 0
    let startTime = Date()

    while true {
        // Check timeout
        if let timeout = timeout {
            let elapsed = Date().timeIntervalSince(startTime)
            if elapsed >= timeout {
                throw UnsandboxError.timeout("Job \(jobId) did not complete within \(timeout) seconds")
            }
        }

        // Sleep before polling
        let delayIdx = min(pollCount, POLL_DELAYS_MS.count - 1)
        Thread.sleep(forTimeInterval: Double(POLL_DELAYS_MS[delayIdx]) / 1000.0)
        pollCount += 1

        let response = try getJob(jobId, publicKey: pk, secretKey: sk)
        if let status = response["status"] as? String,
           ["completed", "failed", "timeout", "cancelled"].contains(status) {
            return response
        }
    }
}

/// Cancel a running job
func cancelJob(_ jobId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/jobs/\(jobId)", publicKey: pk, secretKey: sk)
}

/// List all jobs for the authenticated account
func listJobs(publicKey: String? = nil, secretKey: String? = nil) throws -> [[String: Any]] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "GET", path: "/jobs", publicKey: pk, secretKey: sk)
    return response["jobs"] as? [[String: Any]] ?? []
}

/// Get list of supported programming languages
func getLanguages(publicKey: String? = nil, secretKey: String? = nil) throws -> [String] {
    // Try cache first
    if let cached = loadLanguagesCache() {
        return cached
    }

    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "GET", path: "/languages", publicKey: pk, secretKey: sk)
    let languages = response["languages"] as? [String] ?? []

    // Cache the result
    saveLanguagesCache(languages)
    return languages
}

// MARK: - Session Functions

/// List all sessions for the authenticated account
func listSessions(publicKey: String? = nil, secretKey: String? = nil) throws -> [[String: Any]] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "GET", path: "/sessions", publicKey: pk, secretKey: sk)
    return response["sessions"] as? [[String: Any]] ?? []
}

/// Get details of a specific session
func getSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/sessions/\(sessionId)", publicKey: pk, secretKey: sk)
}

/// Create a new interactive session
func createSession(
    language: String? = nil,
    networkMode: String = "zerotrust",
    ttl: Int = 3600,
    shell: String? = nil,
    multiplexer: String? = nil,
    vcpu: Int = 1,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)

    var data: [String: Any] = [
        "network_mode": networkMode,
        "ttl": ttl
    ]

    if let language = language {
        data["language"] = language
    }
    if let shell = shell {
        data["shell"] = shell
    }
    if let multiplexer = multiplexer {
        data["multiplexer"] = multiplexer
    }
    if vcpu > 1 {
        data["vcpu"] = vcpu
    }

    return try makeRequest(method: "POST", path: "/sessions", publicKey: pk, secretKey: sk, data: data)
}

/// Delete/terminate a session
func deleteSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/sessions/\(sessionId)", publicKey: pk, secretKey: sk)
}

/// Freeze a session (pause execution, preserve state)
func freezeSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/sessions/\(sessionId)/freeze", publicKey: pk, secretKey: sk, data: [:])
}

/// Unfreeze a session (resume execution)
func unfreezeSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/sessions/\(sessionId)/unfreeze", publicKey: pk, secretKey: sk, data: [:])
}

/// Boost a session (increase resources)
func boostSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/sessions/\(sessionId)/boost", publicKey: pk, secretKey: sk, data: [:])
}

/// Unboost a session (return to normal resources)
func unboostSession(_ sessionId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/sessions/\(sessionId)/unboost", publicKey: pk, secretKey: sk, data: [:])
}

/// Execute a shell command in a session
func shellSession(_ sessionId: String, command: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/sessions/\(sessionId)/shell", publicKey: pk, secretKey: sk, data: ["command": command])
}

// MARK: - Service Functions

/// List all services for the authenticated account
func listServices(publicKey: String? = nil, secretKey: String? = nil) throws -> [[String: Any]] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "GET", path: "/services", publicKey: pk, secretKey: sk)
    return response["services"] as? [[String: Any]] ?? []
}

/// Create a new persistent service
func createService(
    name: String,
    ports: [Int],
    bootstrap: String? = nil,
    networkMode: String = "semitrusted",
    customDomains: [String]? = nil,
    vcpu: Int = 1,
    serviceType: String? = nil,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)

    var data: [String: Any] = [
        "name": name,
        "ports": ports,
        "network_mode": networkMode
    ]

    if let bootstrap = bootstrap {
        if bootstrap.hasPrefix("http://") || bootstrap.hasPrefix("https://") {
            data["bootstrap"] = bootstrap
        } else {
            data["bootstrap_content"] = bootstrap
        }
    }
    if let customDomains = customDomains {
        data["custom_domains"] = customDomains
    }
    if vcpu > 1 {
        data["vcpu"] = vcpu
    }
    if let serviceType = serviceType {
        data["service_type"] = serviceType
    }

    return try makeRequest(method: "POST", path: "/services", publicKey: pk, secretKey: sk, data: data)
}

/// Get details of a specific service
func getService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/services/\(serviceId)", publicKey: pk, secretKey: sk)
}

/// Update a service (e.g., resize vCPU/memory)
func updateService(_ serviceId: String, vcpu: Int? = nil, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = [:]
    if let vcpu = vcpu {
        data["vcpu"] = vcpu
    }
    return try makeRequest(method: "PATCH", path: "/services/\(serviceId)", publicKey: pk, secretKey: sk, data: data)
}

/// Delete/destroy a service
func deleteService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/services/\(serviceId)", publicKey: pk, secretKey: sk)
}

/// Freeze a service (pause execution, preserve state)
func freezeService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/freeze", publicKey: pk, secretKey: sk, data: [:])
}

/// Unfreeze a service (resume execution)
func unfreezeService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/unfreeze", publicKey: pk, secretKey: sk, data: [:])
}

/// Lock a service to prevent accidental deletion
func lockService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/lock", publicKey: pk, secretKey: sk, data: [:])
}

/// Unlock a service to allow deletion
func unlockService(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/unlock", publicKey: pk, secretKey: sk, data: [:])
}

/// Get bootstrap/runtime logs for a service
func getServiceLogs(_ serviceId: String, allLogs: Bool = false, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var path = "/services/\(serviceId)/logs"
    if allLogs {
        path += "?all=true"
    }
    return try makeRequest(method: "GET", path: path, publicKey: pk, secretKey: sk)
}

/// Get environment vault status for a service
func getServiceEnv(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/services/\(serviceId)/env", publicKey: pk, secretKey: sk)
}

/// Set environment variables for a service
func setServiceEnv(_ serviceId: String, envDict: [String: String], publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let envContent = envDict.map { "\($0.key)=\($0.value)" }.joined(separator: "\n")
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/env", publicKey: pk, secretKey: sk, data: ["env": envContent])
}

/// Delete environment vault from a service
func deleteServiceEnv(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/services/\(serviceId)/env", publicKey: pk, secretKey: sk)
}

/// Export environment vault secrets for a service
func exportServiceEnv(_ serviceId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/env/export", publicKey: pk, secretKey: sk, data: [:])
}

/// Redeploy a service (re-run bootstrap script)
func redeployService(_ serviceId: String, bootstrap: String? = nil, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = [:]
    if let bootstrap = bootstrap {
        if bootstrap.hasPrefix("http://") || bootstrap.hasPrefix("https://") {
            data["bootstrap"] = bootstrap
        } else {
            data["bootstrap_content"] = bootstrap
        }
    }
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/redeploy", publicKey: pk, secretKey: sk, data: data)
}

/// Execute a command in a running service container
func executeInService(_ serviceId: String, command: String, timeout: Int = 30000, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/services/\(serviceId)/execute", publicKey: pk, secretKey: sk, data: ["command": command, "timeout": timeout])
}

// MARK: - Snapshot Functions

/// Create a snapshot of a session
func sessionSnapshot(_ sessionId: String, name: String? = nil, ephemeral: Bool = false, publicKey: String? = nil, secretKey: String? = nil) throws -> String {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = ["session_id": sessionId, "ephemeral": ephemeral]
    if let name = name {
        data["name"] = name
    }
    let response = try makeRequest(method: "POST", path: "/snapshots", publicKey: pk, secretKey: sk, data: data)
    return response["snapshot_id"] as? String ?? ""
}

/// Create a snapshot of a service
func serviceSnapshot(_ serviceId: String, name: String? = nil, publicKey: String? = nil, secretKey: String? = nil) throws -> String {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = ["service_id": serviceId]
    if let name = name {
        data["name"] = name
    }
    let response = try makeRequest(method: "POST", path: "/snapshots", publicKey: pk, secretKey: sk, data: data)
    return response["snapshot_id"] as? String ?? ""
}

/// List all snapshots
func listSnapshots(publicKey: String? = nil, secretKey: String? = nil) throws -> [[String: Any]] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    let response = try makeRequest(method: "GET", path: "/snapshots", publicKey: pk, secretKey: sk)
    return response["snapshots"] as? [[String: Any]] ?? []
}

/// Restore a snapshot
func restoreSnapshot(_ snapshotId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/snapshots/\(snapshotId)/restore", publicKey: pk, secretKey: sk, data: [:])
}

/// Delete a snapshot
func deleteSnapshot(_ snapshotId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/snapshots/\(snapshotId)", publicKey: pk, secretKey: sk)
}

/// Lock a snapshot to prevent accidental deletion
func lockSnapshot(_ snapshotId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/snapshots/\(snapshotId)/lock", publicKey: pk, secretKey: sk, data: [:])
}

/// Unlock a snapshot to allow deletion
func unlockSnapshot(_ snapshotId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/snapshots/\(snapshotId)/unlock", publicKey: pk, secretKey: sk, data: [:])
}

/// Clone a snapshot to create a new session or service
func cloneSnapshot(
    _ snapshotId: String,
    cloneType: String = "session",
    name: String? = nil,
    shell: String? = nil,
    ports: [Int]? = nil,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = ["type": cloneType]
    if let name = name {
        data["name"] = name
    }
    if let shell = shell {
        data["shell"] = shell
    }
    if let ports = ports {
        data["ports"] = ports
    }
    return try makeRequest(method: "POST", path: "/snapshots/\(snapshotId)/clone", publicKey: pk, secretKey: sk, data: data)
}

// MARK: - Image Functions

/// Publish a service or snapshot as a portable LXD image
func imagePublish(
    sourceType: String,
    sourceId: String,
    name: String? = nil,
    description: String? = nil,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = ["source_type": sourceType, "source_id": sourceId]
    if let name = name {
        data["name"] = name
    }
    if let description = description {
        data["description"] = description
    }
    return try makeRequest(method: "POST", path: "/images", publicKey: pk, secretKey: sk, data: data)
}

/// List images accessible to this API key
func listImages(filterType: String? = nil, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var endpoint = "/images"
    if let filterType = filterType {
        endpoint = "/images/\(filterType)"
    }
    return try makeRequest(method: "GET", path: endpoint, publicKey: pk, secretKey: sk)
}

/// Get details of a specific image
func getImage(_ imageId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/images/\(imageId)", publicKey: pk, secretKey: sk)
}

/// Delete an image
func deleteImage(_ imageId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "DELETE", path: "/images/\(imageId)", publicKey: pk, secretKey: sk)
}

/// Lock an image to prevent accidental deletion
func lockImage(_ imageId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/lock", publicKey: pk, secretKey: sk, data: [:])
}

/// Unlock an image to allow deletion
func unlockImage(_ imageId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/unlock", publicKey: pk, secretKey: sk, data: [:])
}

/// Set image visibility
func setImageVisibility(_ imageId: String, visibility: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/visibility", publicKey: pk, secretKey: sk, data: ["visibility": visibility])
}

/// Grant access to an image for another API key
func grantImageAccess(_ imageId: String, trustedApiKey: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/grant", publicKey: pk, secretKey: sk, data: ["trusted_api_key": trustedApiKey])
}

/// Revoke access to an image from another API key
func revokeImageAccess(_ imageId: String, trustedApiKey: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/revoke", publicKey: pk, secretKey: sk, data: ["trusted_api_key": trustedApiKey])
}

/// List all API keys that have access to an image
func listImageTrusted(_ imageId: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "GET", path: "/images/\(imageId)/trusted", publicKey: pk, secretKey: sk)
}

/// Transfer image ownership to another API key
func transferImage(_ imageId: String, toApiKey: String, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    return try makeRequest(method: "POST", path: "/images/\(imageId)/transfer", publicKey: pk, secretKey: sk, data: ["to_api_key": toApiKey])
}

/// Create a new service from an image
func spawnFromImage(
    _ imageId: String,
    name: String? = nil,
    ports: [Int]? = nil,
    bootstrap: String? = nil,
    networkMode: String = "zerotrust",
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = ["network_mode": networkMode]
    if let name = name {
        data["name"] = name
    }
    if let ports = ports {
        data["ports"] = ports
    }
    if let bootstrap = bootstrap {
        data["bootstrap"] = bootstrap
    }
    return try makeRequest(method: "POST", path: "/images/\(imageId)/spawn", publicKey: pk, secretKey: sk, data: data)
}

/// Clone an image to create a copy owned by you
func cloneImage(_ imageId: String, name: String? = nil, description: String? = nil, publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var data: [String: Any] = [:]
    if let name = name {
        data["name"] = name
    }
    if let description = description {
        data["description"] = description
    }
    return try makeRequest(method: "POST", path: "/images/\(imageId)/clone", publicKey: pk, secretKey: sk, data: data)
}

// MARK: - Key Validation

/// Validate API keys against the portal
func validateKeys(publicKey: String? = nil, secretKey: String? = nil) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)

    let url = URL(string: "\(PORTAL_BASE)/keys/validate")!
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.timeoutInterval = 30

    let timestamp = Int(Date().timeIntervalSince1970)
    let signature = signRequest(secretKey: sk, timestamp: timestamp, method: "POST", path: "/keys/validate", body: "")

    request.setValue("Bearer \(pk)", forHTTPHeaderField: "Authorization")
    request.setValue("\(timestamp)", forHTTPHeaderField: "X-Timestamp")
    request.setValue(signature, forHTTPHeaderField: "X-Signature")
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")

    var result: [String: Any]?
    var requestError: Error?

    let semaphore = DispatchSemaphore(value: 0)

    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        defer { semaphore.signal() }

        if let error = error {
            requestError = UnsandboxError.networkError(error.localizedDescription)
            return
        }

        guard let httpResponse = response as? HTTPURLResponse else {
            requestError = UnsandboxError.invalidResponse("No HTTP response")
            return
        }

        guard let data = data else {
            requestError = UnsandboxError.invalidResponse("No data received")
            return
        }

        if httpResponse.statusCode >= 400 {
            let body = String(data: data, encoding: .utf8) ?? "Unknown error"
            requestError = UnsandboxError.apiError(httpResponse.statusCode, body)
            return
        }

        do {
            if let json = try JSONSerialization.jsonObject(with: data) as? [String: Any] {
                result = json
            }
        } catch {
            requestError = UnsandboxError.invalidResponse("Failed to parse JSON")
        }
    }

    task.resume()
    semaphore.wait()

    if let error = requestError {
        throw error
    }

    return result ?? [:]
}

// MARK: - Image Generation (AI)

/// Generate images from text prompt using AI
func image(
    prompt: String,
    model: String? = nil,
    size: String = "1024x1024",
    quality: String = "standard",
    n: Int = 1,
    publicKey: String? = nil,
    secretKey: String? = nil
) throws -> [String: Any] {
    let (pk, sk) = try resolveCredentials(publicKey: publicKey, secretKey: secretKey)
    var payload: [String: Any] = [
        "prompt": prompt,
        "size": size,
        "quality": quality,
        "n": n
    ]
    if let model = model {
        payload["model"] = model
    }
    return try makeRequest(method: "POST", path: "/image", publicKey: pk, secretKey: sk, data: payload)
}

// MARK: - CLI Implementation

/// Parse a .env file into a dictionary
func parseEnvFile(_ filePath: String) throws -> [String: String] {
    var envDict: [String: String] = [:]

    let url = URL(fileURLWithPath: filePath)
    guard FileManager.default.fileExists(atPath: url.path) else {
        throw UnsandboxError.fileNotFound(filePath)
    }

    let content = try String(contentsOf: url, encoding: .utf8)
    let lines = content.components(separatedBy: .newlines)

    for line in lines {
        let trimmed = line.trimmingCharacters(in: .whitespaces)
        if trimmed.isEmpty || trimmed.hasPrefix("#") { continue }

        if let eqIndex = trimmed.firstIndex(of: "=") {
            let key = String(trimmed[..<eqIndex]).trimmingCharacters(in: .whitespaces)
            var value = String(trimmed[trimmed.index(after: eqIndex)...]).trimmingCharacters(in: .whitespaces)

            // Handle quoted values
            if (value.hasPrefix("\"") && value.hasSuffix("\"")) ||
               (value.hasPrefix("'") && value.hasSuffix("'")) {
                value = String(value.dropFirst().dropLast())
            }
            envDict[key] = value
        }
    }

    return envDict
}

/// Format list output in table format
func formatListOutput(_ items: [[String: Any]], resourceType: String) -> String {
    if items.isEmpty {
        return "No \(resourceType)s found."
    }

    var headers: [String]
    var rows: [[String]] = []

    switch resourceType {
    case "session":
        headers = ["ID", "STATUS", "SHELL", "CREATED"]
        for item in items {
            let id = (item["id"] as? String ?? item["session_id"] as? String ?? "").prefix(36)
            let status = item["status"] as? String ?? "unknown"
            let shell = item["shell"] as? String ?? "bash"
            let created = (item["created_at"] as? String ?? "").prefix(19)
            rows.append([String(id), status, shell, String(created)])
        }
    case "service":
        headers = ["ID", "NAME", "STATUS", "PORTS", "CREATED"]
        for item in items {
            let id = (item["id"] as? String ?? item["service_id"] as? String ?? "").prefix(36)
            let name = (item["name"] as? String ?? "").prefix(20)
            let status = item["status"] as? String ?? "unknown"
            let ports = (item["ports"] as? [Int] ?? []).map { String($0) }.joined(separator: ",").prefix(15)
            let created = (item["created_at"] as? String ?? "").prefix(19)
            rows.append([String(id), String(name), status, String(ports), String(created)])
        }
    case "snapshot":
        headers = ["ID", "NAME", "TYPE", "SIZE", "CREATED"]
        for item in items {
            let id = (item["id"] as? String ?? item["snapshot_id"] as? String ?? "").prefix(36)
            let name = (item["name"] as? String ?? "").prefix(20)
            let sourceType = item["source_type"] as? String ?? "unknown"
            let size = item["size"] as? String ?? ""
            let created = (item["created_at"] as? String ?? "").prefix(19)
            rows.append([String(id), String(name), sourceType, size, String(created)])
        }
    default:
        headers = ["ID", "STATUS"]
        for item in items {
            rows.append([item["id"] as? String ?? "", item["status"] as? String ?? ""])
        }
    }

    // Calculate column widths
    var widths = headers.map { $0.count }
    for row in rows {
        for (i, cell) in row.enumerated() {
            widths[i] = max(widths[i], cell.count)
        }
    }

    // Build output
    var lines: [String] = []
    let headerLine = zip(headers, widths).map { $0.0.padding(toLength: $0.1, withPad: " ", startingAt: 0) }.joined(separator: "  ")
    lines.append(headerLine)

    for row in rows {
        let line = zip(row, widths).map { $0.0.padding(toLength: $0.1, withPad: " ", startingAt: 0) }.joined(separator: "  ")
        lines.append(line)
    }

    return lines.joined(separator: "\n")
}

/// Print usage help
func printHelp() {
    let help = """
    Unsandbox CLI - Execute code in secure containers

    USAGE:
        un [options] <source_file>        Execute code file
        un session [options]              Interactive session
        un service [options]              Manage services
        un service-env <action> <id>      Manage service environment
        un snapshot [options]             Manage snapshots
        un key                            Check API key

    GLOBAL OPTIONS:
        -s, --shell LANG      Language for inline code
        -e, --env KEY=VAL     Set environment variable
        -f, --file FILE       Add input file to /tmp/
        -F, --file-path FILE  Add input file with path preserved
        -a, --artifacts       Return compiled artifacts
        -o, --output DIR      Output directory for artifacts
        -p, --public-key KEY  API public key
        -k, --secret-key KEY  API secret key
        -n, --network MODE    Network: zerotrust or semitrusted
        -v, --vcpu N          vCPU count (1-8)
        -y, --yes             Skip confirmation prompts
        -h, --help            Show help

    SESSION OPTIONS:
        -l, --list            List active sessions
        --attach ID           Reconnect to existing session
        --kill ID             Terminate a session
        --freeze ID           Pause session
        --unfreeze ID         Resume session
        --boost ID            Add resources to session
        --unboost ID          Remove boost from session
        --snapshot ID         Create snapshot of session
        --shell SHELL         Shell/REPL to use (default: bash)
        --tmux                Enable persistence with tmux
        --screen              Enable persistence with screen

    SERVICE OPTIONS:
        -l, --list            List all services
        --info ID             Get service details
        --logs ID             Get all logs
        --tail ID             Get last 9000 lines of logs
        --freeze ID           Pause service
        --unfreeze ID         Resume service
        --destroy ID          Delete service
        --lock ID             Prevent deletion
        --unlock ID           Allow deletion
        --resize ID           Resize service (with --vcpu)
        --redeploy ID         Re-run bootstrap
        --execute ID CMD      Run command in service
        --snapshot ID         Create snapshot of service
        --name NAME           Service name (creates new)
        --ports PORTS         Comma-separated ports
        --bootstrap CMD       Bootstrap command
        --bootstrap-file FILE Bootstrap from file
        --env-file FILE       Load env from .env file

    SERVICE-ENV ACTIONS:
        status                Show vault status
        set                   Set from --env-file or stdin
        export                Export to stdout
        delete                Delete vault

    SNAPSHOT OPTIONS:
        -l, --list            List all snapshots
        --info ID             Get snapshot details
        --delete ID           Delete snapshot
        --lock ID             Prevent deletion
        --unlock ID           Allow deletion
        --clone ID            Clone snapshot
        --type TYPE           Clone type: session or service
        --name NAME           Name for cloned resource

    EXAMPLES:
        un script.py                           Execute Python script
        un -s bash 'echo hello'                Inline bash command
        un session --list                      List active sessions
        un service --list                      List all services
        un snapshot --list                     List all snapshots
        un key                                 Check API key
    """
    print(help)
}

/// CLI argument parser
class CLIArgs {
    var command: String?
    var source: String?
    var shell: String?
    var env: [String] = []
    var files: [String] = []
    var filesPath: [String] = []
    var artifacts: Bool = false
    var output: String?
    var publicKey: String?
    var secretKey: String?
    var networkMode: String = "zerotrust"
    var vcpu: Int = 1
    var yes: Bool = false

    // Session options
    var listFlag: Bool = false
    var attach: String?
    var kill: String?
    var freeze: String?
    var unfreeze: String?
    var boost: String?
    var unboost: String?
    var snapshot: String?
    var snapshotName: String?
    var hot: Bool = false
    var audit: Bool = false
    var tmux: Bool = false
    var screen: Bool = false

    // Service options
    var info: String?
    var logs: String?
    var tail: String?
    var destroy: String?
    var lock: String?
    var unlock: String?
    var resize: String?
    var redeploy: String?
    var execute: (String, String)?
    var name: String?
    var ports: String?
    var domains: String?
    var serviceType: String?
    var bootstrap: String?
    var bootstrapFile: String?
    var envFile: String?

    // Snapshot options
    var delete: String?
    var clone: String?
    var cloneType: String?

    // Service-env
    var serviceEnvAction: String?
    var serviceEnvId: String?

    func parse(_ args: [String]) {
        var i = 0
        let args = Array(args.dropFirst()) // Skip program name

        while i < args.count {
            let arg = args[i]

            switch arg {
            case "-h", "--help":
                printHelp()
                exit(0)
            case "-s", "--shell":
                i += 1
                if i < args.count { shell = args[i] }
            case "-e", "--env":
                i += 1
                if i < args.count { env.append(args[i]) }
            case "-f", "--file":
                i += 1
                if i < args.count { files.append(args[i]) }
            case "-F", "--file-path":
                i += 1
                if i < args.count { filesPath.append(args[i]) }
            case "-a", "--artifacts":
                artifacts = true
            case "-o", "--output":
                i += 1
                if i < args.count { output = args[i] }
            case "-p", "--public-key":
                i += 1
                if i < args.count { publicKey = args[i] }
            case "-k", "--secret-key":
                i += 1
                if i < args.count { secretKey = args[i] }
            case "-n", "--network":
                i += 1
                if i < args.count { networkMode = args[i] }
            case "-v", "--vcpu":
                i += 1
                if i < args.count { vcpu = Int(args[i]) ?? 1 }
            case "-y", "--yes":
                yes = true
            case "-l", "--list":
                listFlag = true
            case "--attach":
                i += 1
                if i < args.count { attach = args[i] }
            case "--kill":
                i += 1
                if i < args.count { kill = args[i] }
            case "--freeze":
                i += 1
                if i < args.count { freeze = args[i] }
            case "--unfreeze":
                i += 1
                if i < args.count { unfreeze = args[i] }
            case "--boost":
                i += 1
                if i < args.count { boost = args[i] }
            case "--unboost":
                i += 1
                if i < args.count { unboost = args[i] }
            case "--snapshot":
                i += 1
                if i < args.count { snapshot = args[i] }
            case "--snapshot-name":
                i += 1
                if i < args.count { snapshotName = args[i] }
            case "--hot":
                hot = true
            case "--audit":
                audit = true
            case "--tmux":
                tmux = true
            case "--screen":
                screen = true
            case "--info":
                i += 1
                if i < args.count { info = args[i] }
            case "--logs":
                i += 1
                if i < args.count { logs = args[i] }
            case "--tail":
                i += 1
                if i < args.count { tail = args[i] }
            case "--destroy":
                i += 1
                if i < args.count { destroy = args[i] }
            case "--lock":
                i += 1
                if i < args.count { lock = args[i] }
            case "--unlock":
                i += 1
                if i < args.count { unlock = args[i] }
            case "--resize":
                i += 1
                if i < args.count { resize = args[i] }
            case "--redeploy":
                i += 1
                if i < args.count { redeploy = args[i] }
            case "--execute":
                i += 1
                if i + 1 < args.count {
                    execute = (args[i], args[i + 1])
                    i += 1
                }
            case "--name":
                i += 1
                if i < args.count { name = args[i] }
            case "--ports":
                i += 1
                if i < args.count { ports = args[i] }
            case "--domains":
                i += 1
                if i < args.count { domains = args[i] }
            case "--type":
                i += 1
                if i < args.count {
                    if args[i] == "session" || args[i] == "service" {
                        cloneType = args[i]
                    } else {
                        serviceType = args[i]
                    }
                }
            case "--bootstrap":
                i += 1
                if i < args.count { bootstrap = args[i] }
            case "--bootstrap-file":
                i += 1
                if i < args.count { bootstrapFile = args[i] }
            case "--env-file":
                i += 1
                if i < args.count { envFile = args[i] }
            case "--delete":
                i += 1
                if i < args.count { delete = args[i] }
            case "--clone":
                i += 1
                if i < args.count { clone = args[i] }
            case "session", "service", "snapshot", "key":
                command = arg
            case "service-env":
                command = "service-env"
                i += 1
                if i < args.count { serviceEnvAction = args[i] }
                i += 1
                if i < args.count { serviceEnvId = args[i] }
            default:
                if arg.hasPrefix("-") {
                    fputs("Error: Unknown option \(arg)\n", stderr)
                    exit(2)
                } else if command == nil && (arg == "session" || arg == "service" || arg == "snapshot" || arg == "key" || arg == "service-env") {
                    command = arg
                } else if source == nil {
                    source = arg
                }
            }
            i += 1
        }
    }
}

/// Handle execute command
func handleExecuteCommand(_ args: CLIArgs, _ pk: String, _ sk: String) throws {
    var language: String
    var code: String

    if let shell = args.shell {
        // Inline code mode
        guard let source = args.source else {
            fputs("Error: Code required with -s/--shell\n", stderr)
            exit(2)
        }
        language = shell
        code = source
    } else {
        // File mode
        guard let source = args.source else {
            fputs("Error: Source file required\n", stderr)
            exit(2)
        }

        // Detect language from filename
        guard let detected = detectLanguage(source) else {
            fputs("Error: Cannot detect language from '\(source)'\n", stderr)
            exit(2)
        }
        language = detected

        // Read source file
        let url = URL(fileURLWithPath: source)
        guard FileManager.default.fileExists(atPath: url.path) else {
            fputs("Error: File not found: \(source)\n", stderr)
            exit(1)
        }

        do {
            code = try String(contentsOf: url, encoding: .utf8)
        } catch {
            fputs("Error: Failed to read file: \(error)\n", stderr)
            exit(1)
        }
    }

    // Parse environment variables
    var envDict: [String: String]? = nil
    if !args.env.isEmpty {
        envDict = [:]
        for envVar in args.env {
            if let eqIndex = envVar.firstIndex(of: "=") {
                let key = String(envVar[..<eqIndex])
                let value = String(envVar[envVar.index(after: eqIndex)...])
                envDict?[key] = value
            }
        }
    }

    // Execute code
    let result = try executeCodeWithOptions(
        language: language,
        code: code,
        env: envDict,
        networkMode: args.networkMode,
        vcpu: args.vcpu,
        artifacts: args.artifacts,
        publicKey: pk,
        secretKey: sk
    )

    // Output result
    let stdout = result["stdout"] as? String ?? ""
    let stderr_out = result["stderr"] as? String ?? ""
    let exitCode = result["exit_code"] as? Int ?? 0
    let executionTime = result["execution_time_ms"] as? Int ?? 0

    if !stdout.isEmpty {
        print(stdout, terminator: stdout.hasSuffix("\n") ? "" : "\n")
    }

    if !stderr_out.isEmpty {
        fputs(stderr_out, stderr)
        if !stderr_out.hasSuffix("\n") {
            fputs("\n", stderr)
        }
    }

    print("---")
    print("Exit code: \(exitCode)")
    print("Execution time: \(executionTime)ms")

    exit(Int32(exitCode))
}

/// Handle session command
func handleSessionCommand(_ args: CLIArgs, _ pk: String, _ sk: String) throws {
    if args.listFlag {
        let sessions = try listSessions(publicKey: pk, secretKey: sk)
        print(formatListOutput(sessions, resourceType: "session"))
    } else if let attachId = args.attach {
        let session = try getSession(attachId, publicKey: pk, secretKey: sk)
        let sessionId = session["id"] as? String ?? session["session_id"] as? String ?? ""
        print("Session ID: \(sessionId)")
        print("Status: \(session["status"] as? String ?? "unknown")")
        print("WebSocket URL: wss://api.unsandbox.com/sessions/\(attachId)/shell")
        print("\nUse a WebSocket client to connect interactively.")
    } else if let killId = args.kill {
        _ = try deleteSession(killId, publicKey: pk, secretKey: sk)
        print("Session \(killId) terminated")
    } else if let freezeId = args.freeze {
        _ = try freezeSession(freezeId, publicKey: pk, secretKey: sk)
        print("Session \(freezeId) frozen")
    } else if let unfreezeId = args.unfreeze {
        _ = try unfreezeSession(unfreezeId, publicKey: pk, secretKey: sk)
        print("Session \(unfreezeId) unfrozen")
    } else if let boostId = args.boost {
        _ = try boostSession(boostId, publicKey: pk, secretKey: sk)
        print("Session \(boostId) boosted")
    } else if let unboostId = args.unboost {
        _ = try unboostSession(unboostId, publicKey: pk, secretKey: sk)
        print("Session \(unboostId) unboosted")
    } else if let snapshotId = args.snapshot {
        let snapId = try sessionSnapshot(snapshotId, name: args.snapshotName, ephemeral: !args.hot, publicKey: pk, secretKey: sk)
        print("Snapshot created: \(snapId)")
    } else {
        // Create new session
        var multiplexer: String? = nil
        if args.tmux { multiplexer = "tmux" }
        else if args.screen { multiplexer = "screen" }

        let result = try createSession(
            shell: args.shell,
            networkMode: args.networkMode,
            multiplexer: multiplexer,
            publicKey: pk,
            secretKey: sk
        )

        let sessionId = result["session_id"] as? String ?? result["id"] as? String ?? ""
        print("Session created: \(sessionId)")
        print("WebSocket URL: wss://api.unsandbox.com/sessions/\(sessionId)/shell")
    }
}

/// Handle service command
func handleServiceCommand(_ args: CLIArgs, _ pk: String, _ sk: String) throws {
    if args.listFlag {
        let services = try listServices(publicKey: pk, secretKey: sk)
        print(formatListOutput(services, resourceType: "service"))
    } else if let infoId = args.info {
        let service = try getService(infoId, publicKey: pk, secretKey: sk)
        let jsonData = try JSONSerialization.data(withJSONObject: service, options: .prettyPrinted)
        print(String(data: jsonData, encoding: .utf8) ?? "{}")
    } else if let logsId = args.logs {
        let result = try getServiceLogs(logsId, allLogs: true, publicKey: pk, secretKey: sk)
        print(result["log"] as? String ?? "")
    } else if let tailId = args.tail {
        let result = try getServiceLogs(tailId, allLogs: false, publicKey: pk, secretKey: sk)
        print(result["log"] as? String ?? "")
    } else if let freezeId = args.freeze {
        _ = try freezeService(freezeId, publicKey: pk, secretKey: sk)
        print("Service \(freezeId) frozen")
    } else if let unfreezeId = args.unfreeze {
        _ = try unfreezeService(unfreezeId, publicKey: pk, secretKey: sk)
        print("Service \(unfreezeId) unfrozen")
    } else if let destroyId = args.destroy {
        _ = try deleteService(destroyId, publicKey: pk, secretKey: sk)
        print("Service \(destroyId) destroyed")
    } else if let lockId = args.lock {
        _ = try lockService(lockId, publicKey: pk, secretKey: sk)
        print("Service \(lockId) locked")
    } else if let unlockId = args.unlock {
        _ = try unlockService(unlockId, publicKey: pk, secretKey: sk)
        print("Service \(unlockId) unlocked")
    } else if let resizeId = args.resize {
        _ = try updateService(resizeId, vcpu: args.vcpu, publicKey: pk, secretKey: sk)
        print("Service \(resizeId) resized to \(args.vcpu) vCPU(s)")
    } else if let redeployId = args.redeploy {
        var bootstrapContent: String? = nil
        if let bootstrapFile = args.bootstrapFile {
            bootstrapContent = try String(contentsOfFile: bootstrapFile, encoding: .utf8)
        } else if let bootstrap = args.bootstrap {
            bootstrapContent = bootstrap
        }
        _ = try redeployService(redeployId, bootstrap: bootstrapContent, publicKey: pk, secretKey: sk)
        print("Service \(redeployId) redeployed")
    } else if let (serviceId, command) = args.execute {
        let result = try executeInService(serviceId, command: command, publicKey: pk, secretKey: sk)
        if let jobId = result["job_id"] as? String {
            let jobResult = try waitForJob(jobId, publicKey: pk, secretKey: sk)
            let stdout = jobResult["stdout"] as? String ?? ""
            let stderr_out = jobResult["stderr"] as? String ?? ""
            if !stdout.isEmpty { print(stdout, terminator: "") }
            if !stderr_out.isEmpty { fputs(stderr_out, stderr) }
        } else {
            let stdout = result["stdout"] as? String ?? ""
            let stderr_out = result["stderr"] as? String ?? ""
            if !stdout.isEmpty { print(stdout, terminator: "") }
            if !stderr_out.isEmpty { fputs(stderr_out, stderr) }
        }
    } else if let snapshotId = args.snapshot {
        let snapId = try serviceSnapshot(snapshotId, name: args.snapshotName, publicKey: pk, secretKey: sk)
        print("Snapshot created: \(snapId)")
    } else if let name = args.name {
        // Create new service
        guard let portsStr = args.ports else {
            fputs("Error: --ports required when creating service\n", stderr)
            exit(2)
        }

        let ports = portsStr.components(separatedBy: ",").compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }

        var bootstrapContent: String? = nil
        if let bootstrapFile = args.bootstrapFile {
            bootstrapContent = try String(contentsOfFile: bootstrapFile, encoding: .utf8)
        } else if let bootstrap = args.bootstrap {
            bootstrapContent = bootstrap
        }

        var customDomains: [String]? = nil
        if let domains = args.domains {
            customDomains = domains.components(separatedBy: ",").map { $0.trimmingCharacters(in: .whitespaces) }
        }

        let result = try createService(
            name: name,
            ports: ports,
            bootstrap: bootstrapContent,
            customDomains: customDomains,
            vcpu: args.vcpu,
            serviceType: args.serviceType,
            publicKey: pk,
            secretKey: sk
        )

        let serviceId = result["service_id"] as? String ?? result["id"] as? String ?? ""
        print("Service created: \(serviceId)")
        print("URL: https://\(name).on.unsandbox.com")
    } else {
        fputs("Error: No action specified for service command\n", stderr)
        exit(2)
    }
}

/// Handle service-env command
func handleServiceEnvCommand(_ args: CLIArgs, _ pk: String, _ sk: String) throws {
    guard let action = args.serviceEnvAction, let serviceId = args.serviceEnvId else {
        fputs("Error: service-env requires action and service ID\n", stderr)
        exit(2)
    }

    switch action {
    case "status":
        let result = try getServiceEnv(serviceId, publicKey: pk, secretKey: sk)
        print("Has vault: \(result["has_vault"] as? Bool ?? false)")
        print("Variable count: \(result["count"] as? Int ?? 0)")
        if let updatedAt = result["updated_at"] as? String {
            print("Updated at: \(updatedAt)")
        }
    case "set":
        var envDict: [String: String]
        if let envFile = args.envFile {
            envDict = try parseEnvFile(envFile)
        } else {
            // Read from stdin
            fputs("Enter environment variables (KEY=VALUE), one per line. Ctrl+D to finish:\n", stderr)
            envDict = [:]
            while let line = readLine() {
                let trimmed = line.trimmingCharacters(in: .whitespaces)
                if !trimmed.isEmpty, let eqIndex = trimmed.firstIndex(of: "=") {
                    let key = String(trimmed[..<eqIndex]).trimmingCharacters(in: .whitespaces)
                    let value = String(trimmed[trimmed.index(after: eqIndex)...]).trimmingCharacters(in: .whitespaces)
                    envDict[key] = value
                }
            }
        }
        let result = try setServiceEnv(serviceId, envDict: envDict, publicKey: pk, secretKey: sk)
        print("Environment set: \(result["count"] as? Int ?? envDict.count) variables")
    case "export":
        let result = try exportServiceEnv(serviceId, publicKey: pk, secretKey: sk)
        print(result["env"] as? String ?? "")
    case "delete":
        _ = try deleteServiceEnv(serviceId, publicKey: pk, secretKey: sk)
        print("Environment vault deleted for service \(serviceId)")
    default:
        fputs("Error: Unknown service-env action: \(action)\n", stderr)
        exit(2)
    }
}

/// Handle snapshot command
func handleSnapshotCommand(_ args: CLIArgs, _ pk: String, _ sk: String) throws {
    if args.listFlag {
        let snapshots = try listSnapshots(publicKey: pk, secretKey: sk)
        print(formatListOutput(snapshots, resourceType: "snapshot"))
    } else if let infoId = args.info {
        let snapshots = try listSnapshots(publicKey: pk, secretKey: sk)
        if let snapshot = snapshots.first(where: { ($0["id"] as? String) == infoId || ($0["snapshot_id"] as? String) == infoId }) {
            let jsonData = try JSONSerialization.data(withJSONObject: snapshot, options: .prettyPrinted)
            print(String(data: jsonData, encoding: .utf8) ?? "{}")
        } else {
            fputs("Error: Snapshot \(infoId) not found\n", stderr)
            exit(1)
        }
    } else if let deleteId = args.delete {
        _ = try deleteSnapshot(deleteId, publicKey: pk, secretKey: sk)
        print("Snapshot \(deleteId) deleted")
    } else if let lockId = args.lock {
        _ = try lockSnapshot(lockId, publicKey: pk, secretKey: sk)
        print("Snapshot \(lockId) locked")
    } else if let unlockId = args.unlock {
        _ = try unlockSnapshot(unlockId, publicKey: pk, secretKey: sk)
        print("Snapshot \(unlockId) unlocked")
    } else if let cloneId = args.clone {
        let cloneType = args.cloneType ?? "session"
        var ports: [Int]? = nil
        if let portsStr = args.ports {
            ports = portsStr.components(separatedBy: ",").compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }
        }

        let result = try cloneSnapshot(
            cloneId,
            cloneType: cloneType,
            name: args.name,
            shell: args.shell,
            ports: ports,
            publicKey: pk,
            secretKey: sk
        )

        if cloneType == "session" {
            print("Session created: \(result["session_id"] as? String ?? result["id"] as? String ?? "")")
        } else {
            print("Service created: \(result["service_id"] as? String ?? result["id"] as? String ?? "")")
        }
    } else {
        fputs("Error: No action specified for snapshot command\n", stderr)
        exit(2)
    }
}

/// Handle key command
func handleKeyCommand(_ pk: String, _ sk: String) throws {
    let result = try validateKeys(publicKey: pk, secretKey: sk)

    print("Public key: \(pk)")
    print("Valid: \(result["valid"] as? Bool ?? false)")
    if let tier = result["tier"] as? String {
        print("Tier: \(tier)")
    }
    if let expiresAt = result["expires_at"] as? String {
        print("Expires: \(expiresAt)")
    }
    if let reason = result["reason"] as? String {
        print("Reason: \(reason)")
    }
}

/// Main CLI entry point
@main
struct UnCLI {
    static func main() {
        let args = CLIArgs()
        args.parse(CommandLine.arguments)

        // Resolve credentials
        let pk: String
        let sk: String
        do {
            (pk, sk) = try resolveCredentials(publicKey: args.publicKey, secretKey: args.secretKey)
        } catch {
            fputs("Error: \(error)\n", stderr)
            exit(3)
        }

        do {
            // Handle subcommands
            switch args.command {
            case "session":
                try handleSessionCommand(args, pk, sk)
            case "service":
                try handleServiceCommand(args, pk, sk)
            case "service-env":
                try handleServiceEnvCommand(args, pk, sk)
            case "snapshot":
                try handleSnapshotCommand(args, pk, sk)
            case "key":
                try handleKeyCommand(pk, sk)
            default:
                if args.source != nil || args.shell != nil {
                    try handleExecuteCommand(args, pk, sk)
                } else {
                    printHelp()
                    exit(2)
                }
            }
        } catch let error as UnsandboxError {
            switch error {
            case .credentialsNotFound:
                fputs("Error: \(error)\n", stderr)
                exit(3)
            case .apiError(let code, _) where code == 401:
                fputs("Error: Authentication failed\n", stderr)
                exit(3)
            case .apiError:
                fputs("Error: API error - \(error)\n", stderr)
                exit(4)
            case .networkError:
                fputs("Error: Network error - \(error)\n", stderr)
                exit(1)
            case .timeout:
                fputs("Error: \(error)\n", stderr)
                exit(5)
            default:
                fputs("Error: \(error)\n", stderr)
                exit(1)
            }
        } catch {
            fputs("Error: \(error)\n", stderr)
            exit(1)
        }
    }
}
