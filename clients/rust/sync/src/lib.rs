// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// unsandbox.com Rust SDK (Synchronous)
//
// Library Usage:
//     use un::{Credentials, execute_code, resolve_credentials};
//
//     // Resolve credentials (4-tier priority)
//     let creds = resolve_credentials(None, None)?;
//
//     // Execute code synchronously
//     let result = execute_code("python", r#"print("hello")"#, &creds)?;
//     println!("Output: {}", result.output);
//
//     // Execute asynchronously
//     let job_id = execute_async("javascript", r#"console.log("hello")"#, &creds)?;
//
//     // Wait for job completion
//     let result = wait_for_job(&job_id, &creds, None)?;
//
//     // List all jobs
//     let jobs = list_jobs(&creds)?;
//
//     // Get supported languages (cached 1 hour)
//     let languages = get_languages(&creds)?;
//
//     // Detect language from filename
//     let lang = detect_language("script.py");  // Some("python")
//
//     // Snapshot operations
//     let snapshot = session_snapshot(&session_id, &creds, Some("my_snapshot"), false)?;
//     let snapshots = list_snapshots(&creds)?;
//     let result = restore_snapshot(&snapshot_id, &creds)?;
//     delete_snapshot(&snapshot_id, &creds)?;
//
// Authentication Priority (4-tier):
//     1. Function arguments (public_key, secret_key)
//     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
//     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
//     4. Local directory (./accounts.csv, line 0 by default)
//
//     Format: public_key,secret_key (one per line)
//     Account selection: UNSANDBOX_ACCOUNT=N env var (0-based index)
//
// Request Authentication (HMAC-SHA256):
//     Authorization: Bearer <public_key>                  (identifies account)
//     X-Timestamp: <unix_seconds>                         (replay prevention)
//     X-Signature: HMAC-SHA256(secret_key, msg)           (proves secret + body integrity)
//
//     Message format: "timestamp:METHOD:path:body"
//     - timestamp: seconds since epoch
//     - METHOD: GET, POST, DELETE, etc. (uppercase)
//     - path: e.g., "/execute", "/jobs/123"
//     - body: JSON payload (empty string for GET/DELETE)
//
// Languages Cache:
//     - Cached in ~/.unsandbox/languages.json
//     - TTL: 1 hour
//     - Updated on successful API calls

use hmac::{Hmac, Mac};
use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// API base URL
const API_BASE: &str = "https://api.unsandbox.com";

/// Languages cache TTL in seconds (1 hour)
const LANGUAGES_CACHE_TTL: u64 = 3600;

/// Polling delays in milliseconds for exponential backoff
const POLL_DELAYS_MS: &[u64] = &[300, 450, 700, 900, 650, 1600, 2000];

/// Default timeout for wait_for_job in seconds
const DEFAULT_TIMEOUT_SECS: u64 = 300;

type HmacSha256 = Hmac<Sha256>;

// =============================================================================
// Error Types
// =============================================================================

/// Error type for unsandbox SDK operations
#[derive(Debug, thiserror::Error)]
pub enum UnsandboxError {
    /// No credentials found in any of the 4 tiers
    #[error("No credentials found. Please provide via:\n  1. Function arguments (public_key, secret_key)\n  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n  3. ~/.unsandbox/accounts.csv\n  4. ./accounts.csv")]
    NoCredentials,

    /// HTTP request failed
    #[error("HTTP request failed: {0}")]
    HttpError(#[from] reqwest::Error),

    /// API returned an error response
    #[error("API error (HTTP {status}): {message}")]
    ApiError { status: u16, message: String },

    /// JSON serialization/deserialization failed
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// I/O error (file operations)
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Job timed out while waiting
    #[error("Job timed out after {0} seconds")]
    Timeout(u64),

    /// Missing expected field in response
    #[error("Missing field in response: {0}")]
    MissingField(String),
}

/// Result type for unsandbox SDK operations
pub type Result<T> = std::result::Result<T, UnsandboxError>;

// =============================================================================
// Credentials
// =============================================================================

/// API credentials for authentication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Credentials {
    /// Public key (unsb-pk-xxxx-xxxx-xxxx-xxxx) - used as Bearer token
    pub public_key: String,
    /// Secret key (unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx) - used for HMAC signing, never transmitted
    pub secret_key: String,
}

impl Credentials {
    /// Create new credentials from public and secret keys
    pub fn new(public_key: impl Into<String>, secret_key: impl Into<String>) -> Self {
        Self {
            public_key: public_key.into(),
            secret_key: secret_key.into(),
        }
    }
}

// =============================================================================
// Response Types
// =============================================================================

/// Result of code execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecuteResult {
    /// Job ID
    pub job_id: String,
    /// Execution status: "completed", "failed", "timeout", "cancelled"
    pub status: String,
    /// Combined stdout/stderr output
    #[serde(default)]
    pub output: String,
    /// Exit code (0 = success)
    #[serde(default)]
    pub exit_code: i32,
    /// Execution time in milliseconds
    #[serde(default)]
    pub execution_time_ms: u64,
}

/// Job status from /jobs/{id} endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobStatus {
    /// Job ID
    pub job_id: String,
    /// Current status: "pending", "running", "completed", "failed", "timeout", "cancelled"
    pub status: String,
    /// Language used
    #[serde(default)]
    pub language: String,
    /// Combined output (available when completed)
    #[serde(default)]
    pub output: String,
    /// Exit code (available when completed)
    #[serde(default)]
    pub exit_code: i32,
    /// Execution time in milliseconds
    #[serde(default)]
    pub execution_time_ms: u64,
    /// Created timestamp
    #[serde(default)]
    pub created_at: String,
}

/// Job summary from /jobs list endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Job {
    /// Job ID
    pub job_id: String,
    /// Current status
    pub status: String,
    /// Language used
    #[serde(default)]
    pub language: String,
    /// Created timestamp
    #[serde(default)]
    pub created_at: String,
}

/// Snapshot information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    /// Snapshot ID
    pub snapshot_id: String,
    /// Snapshot name
    #[serde(default)]
    pub name: String,
    /// Source type: "session" or "service"
    #[serde(default)]
    pub source_type: String,
    /// Source ID (session_id or service_id)
    #[serde(default)]
    pub source_id: String,
    /// Whether this is a hot (ephemeral) snapshot
    #[serde(default)]
    pub hot: bool,
    /// Created timestamp
    #[serde(default)]
    pub created_at: String,
    /// Size in bytes
    #[serde(default)]
    pub size_bytes: u64,
}

/// Result of restoring a snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RestoreResult {
    /// New session or service ID
    pub id: String,
    /// Type: "session" or "service"
    #[serde(rename = "type")]
    pub restore_type: String,
    /// Status message
    #[serde(default)]
    pub message: String,
}

/// Session information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    /// Session ID
    pub session_id: String,
    /// Container name (e.g., "unsb-vm-abc123")
    #[serde(default)]
    pub container_name: String,
    /// Session status: "running", "frozen", "stopped"
    #[serde(default)]
    pub status: String,
    /// Network mode: "zerotrust" or "semitrusted"
    #[serde(default)]
    pub network_mode: String,
    /// Shell type (e.g., "bash", "python3")
    #[serde(default)]
    pub shell: String,
    /// Number of vCPUs
    #[serde(default)]
    pub vcpu: u32,
    /// Memory in MB
    #[serde(default)]
    pub memory_mb: u32,
    /// Whether the session is boosted
    #[serde(default)]
    pub boosted: bool,
    /// Created timestamp
    #[serde(default)]
    pub created_at: String,
    /// Last activity timestamp
    #[serde(default)]
    pub last_activity: String,
}

/// Service information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Service {
    /// Service ID
    pub service_id: String,
    /// Service name
    #[serde(default)]
    pub name: String,
    /// Container name
    #[serde(default)]
    pub container_name: String,
    /// Service status: "running", "frozen", "stopped", "locked"
    #[serde(default)]
    pub status: String,
    /// Exposed ports
    #[serde(default)]
    pub ports: Vec<u16>,
    /// Custom domains
    #[serde(default)]
    pub domains: Vec<String>,
    /// Network mode
    #[serde(default)]
    pub network_mode: String,
    /// Number of vCPUs
    #[serde(default)]
    pub vcpu: u32,
    /// Memory in MB
    #[serde(default)]
    pub memory_mb: u32,
    /// Whether the service is locked (cannot be modified)
    #[serde(default)]
    pub locked: bool,
    /// Public URL for the service
    #[serde(default)]
    pub url: String,
    /// Created timestamp
    #[serde(default)]
    pub created_at: String,
}

/// Result of shell command execution in a session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShellResult {
    /// Command output (stdout + stderr)
    #[serde(default)]
    pub output: String,
    /// Exit code
    #[serde(default)]
    pub exit_code: i32,
}

/// Result of validating API keys
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeysValid {
    /// Whether the keys are valid
    pub valid: bool,
    /// Account ID associated with the keys
    #[serde(default)]
    pub account_id: String,
    /// Account email (if available)
    #[serde(default)]
    pub email: String,
    /// Account plan/tier
    #[serde(default)]
    pub plan: String,
    /// Error message if invalid
    #[serde(default)]
    pub error: String,
}

/// Options for creating a session
#[derive(Debug, Clone, Default)]
pub struct SessionCreateOptions {
    /// Network mode: "zerotrust" (default) or "semitrusted"
    pub network_mode: Option<String>,
    /// Shell to use (e.g., "bash", "python3")
    pub shell: Option<String>,
    /// Number of vCPUs (default: 1)
    pub vcpu: Option<u32>,
    /// Whether to use tmux multiplexer
    pub tmux: Option<bool>,
    /// Whether to use screen multiplexer
    pub screen: Option<bool>,
}

/// Options for creating a service
#[derive(Debug, Clone, Default)]
pub struct ServiceCreateOptions {
    /// Network mode: "zerotrust" (default) or "semitrusted"
    pub network_mode: Option<String>,
    /// Number of vCPUs (default: 1)
    pub vcpu: Option<u32>,
    /// Custom domains for the service
    pub domains: Option<Vec<String>>,
    /// Bootstrap script content
    pub bootstrap: Option<String>,
    /// Bootstrap script URL
    pub bootstrap_url: Option<String>,
}

/// Options for updating a service
#[derive(Debug, Clone, Default)]
pub struct ServiceUpdateOptions {
    /// New service name
    pub name: Option<String>,
    /// New ports
    pub ports: Option<Vec<u16>>,
    /// New domains
    pub domains: Option<Vec<String>>,
    /// New vCPU count
    pub vcpu: Option<u32>,
}

// =============================================================================
// Internal Response Types
// =============================================================================

#[derive(Debug, Deserialize)]
struct ExecuteResponse {
    job_id: String,
    status: String,
    #[serde(default)]
    output: String,
    #[serde(default)]
    exit_code: i32,
    #[serde(default)]
    execution_time_ms: u64,
}

#[derive(Debug, Deserialize)]
struct JobsListResponse {
    jobs: Vec<Job>,
}

#[derive(Debug, Deserialize)]
struct LanguagesResponse {
    languages: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct SnapshotsListResponse {
    snapshots: Vec<Snapshot>,
}

#[derive(Debug, Deserialize)]
struct SnapshotCreateResponse {
    snapshot_id: String,
    #[serde(default)]
    name: String,
    #[serde(default)]
    source_type: String,
    #[serde(default)]
    source_id: String,
    #[serde(default)]
    hot: bool,
    #[serde(default)]
    created_at: String,
    #[serde(default)]
    size_bytes: u64,
}

#[derive(Debug, Serialize, Deserialize)]
struct LanguagesCache {
    languages: Vec<String>,
    timestamp: u64,
}

#[derive(Debug, Deserialize)]
struct SessionsListResponse {
    sessions: Vec<Session>,
}

#[derive(Debug, Deserialize)]
struct ServicesListResponse {
    services: Vec<Service>,
}

#[derive(Debug, Deserialize)]
struct EnvResponse {
    env: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct EnvExportResponse {
    content: String,
}

// =============================================================================
// Language Detection
// =============================================================================

/// Language extension mapping
fn get_language_map() -> HashMap<&'static str, &'static str> {
    [
        ("py", "python"),
        ("js", "javascript"),
        ("ts", "typescript"),
        ("rb", "ruby"),
        ("php", "php"),
        ("pl", "perl"),
        ("sh", "bash"),
        ("r", "r"),
        ("lua", "lua"),
        ("go", "go"),
        ("rs", "rust"),
        ("c", "c"),
        ("cpp", "cpp"),
        ("cc", "cpp"),
        ("cxx", "cpp"),
        ("java", "java"),
        ("kt", "kotlin"),
        ("m", "objc"),
        ("cs", "csharp"),
        ("fs", "fsharp"),
        ("hs", "haskell"),
        ("ml", "ocaml"),
        ("clj", "clojure"),
        ("scm", "scheme"),
        ("ss", "scheme"),
        ("erl", "erlang"),
        ("ex", "elixir"),
        ("exs", "elixir"),
        ("jl", "julia"),
        ("d", "d"),
        ("nim", "nim"),
        ("zig", "zig"),
        ("v", "v"),
        ("cr", "crystal"),
        ("dart", "dart"),
        ("groovy", "groovy"),
        ("f90", "fortran"),
        ("f95", "fortran"),
        ("lisp", "commonlisp"),
        ("lsp", "commonlisp"),
        ("cob", "cobol"),
        ("tcl", "tcl"),
        ("raku", "raku"),
        ("pro", "prolog"),
        ("p", "prolog"),
        ("4th", "forth"),
        ("forth", "forth"),
        ("fth", "forth"),
    ]
    .into_iter()
    .collect()
}

/// Detect programming language from filename extension.
///
/// # Arguments
/// * `filename` - Filename to detect language from (e.g., "script.py")
///
/// # Returns
/// Language identifier (e.g., "python") or None if unknown
///
/// # Examples
/// ```
/// use un::detect_language;
///
/// assert_eq!(detect_language("hello.py"), Some("python"));
/// assert_eq!(detect_language("script.js"), Some("javascript"));
/// assert_eq!(detect_language("main.go"), Some("go"));
/// assert_eq!(detect_language("unknown"), None);
/// ```
pub fn detect_language(filename: &str) -> Option<&'static str> {
    let ext = filename.rsplit('.').next()?;
    if ext == filename {
        return None; // No extension found
    }
    let ext_lower = ext.to_lowercase();
    get_language_map().get(ext_lower.as_str()).copied()
}

// =============================================================================
// Credentials Resolution
// =============================================================================

/// Get the ~/.unsandbox directory path
fn get_unsandbox_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".unsandbox"))
}

/// Ensure ~/.unsandbox directory exists
fn ensure_unsandbox_dir() -> Option<PathBuf> {
    let dir = get_unsandbox_dir()?;
    fs::create_dir_all(&dir).ok()?;
    Some(dir)
}

/// Load credentials from a CSV file (public_key,secret_key per line)
fn load_credentials_from_csv(path: &PathBuf, account_index: usize) -> Option<Credentials> {
    let file = fs::File::open(path).ok()?;
    let reader = BufReader::new(file);
    let mut current_index = 0;

    for line in reader.lines().map_while(|l| l.ok()) {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        if current_index == account_index {
            let parts: Vec<&str> = line.split(',').collect();
            if parts.len() >= 2 {
                let pk = parts[0].trim();
                let sk = parts[1].trim();
                if pk.starts_with("unsb-pk-") && sk.starts_with("unsb-sk-") {
                    return Some(Credentials::new(pk, sk));
                }
            }
        }
        current_index += 1;
    }

    None
}

/// Resolve credentials using 4-tier priority system.
///
/// # Priority
/// 1. Function arguments (if both provided)
/// 2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
/// 3. ~/.unsandbox/accounts.csv
/// 4. ./accounts.csv
///
/// # Arguments
/// * `public_key` - Optional public key from function argument
/// * `secret_key` - Optional secret key from function argument
///
/// # Returns
/// Credentials if found, UnsandboxError::NoCredentials otherwise
///
/// # Examples
/// ```ignore
/// // Use environment variables or config file
/// let creds = resolve_credentials(None, None)?;
///
/// // Use explicit credentials
/// let creds = resolve_credentials(
///     Some("unsb-pk-xxxx"),
///     Some("unsb-sk-xxxx")
/// )?;
/// ```
pub fn resolve_credentials(
    public_key: Option<&str>,
    secret_key: Option<&str>,
) -> Result<Credentials> {
    // Tier 1: Function arguments
    if let (Some(pk), Some(sk)) = (public_key, secret_key) {
        if !pk.is_empty() && !sk.is_empty() {
            return Ok(Credentials::new(pk, sk));
        }
    }

    // Tier 2: Environment variables
    let env_pk = env::var("UNSANDBOX_PUBLIC_KEY").ok();
    let env_sk = env::var("UNSANDBOX_SECRET_KEY").ok();
    if let (Some(pk), Some(sk)) = (env_pk, env_sk) {
        if !pk.is_empty() && !sk.is_empty() {
            return Ok(Credentials::new(pk, sk));
        }
    }

    // Determine account index
    let account_index: usize = env::var("UNSANDBOX_ACCOUNT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    // Tier 3: ~/.unsandbox/accounts.csv
    if let Some(dir) = get_unsandbox_dir() {
        let csv_path = dir.join("accounts.csv");
        if let Some(creds) = load_credentials_from_csv(&csv_path, account_index) {
            return Ok(creds);
        }
    }

    // Tier 4: ./accounts.csv
    let local_csv = PathBuf::from("accounts.csv");
    if let Some(creds) = load_credentials_from_csv(&local_csv, account_index) {
        return Ok(creds);
    }

    Err(UnsandboxError::NoCredentials)
}

// =============================================================================
// HMAC Signing
// =============================================================================

/// Sign a request using HMAC-SHA256.
///
/// Message format: "timestamp:METHOD:path:body"
fn sign_request(secret_key: &str, timestamp: u64, method: &str, path: &str, body: &str) -> String {
    let message = format!("{}:{}:{}:{}", timestamp, method, path, body);
    let mut mac = HmacSha256::new_from_slice(secret_key.as_bytes())
        .expect("HMAC can take key of any size");
    mac.update(message.as_bytes());
    hex::encode(mac.finalize().into_bytes())
}

/// Get current Unix timestamp in seconds
fn get_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs()
}

// =============================================================================
// HTTP Client
// =============================================================================

/// Make an authenticated HTTP request to the API
fn make_request<T: for<'de> Deserialize<'de>>(
    method: &str,
    path: &str,
    creds: &Credentials,
    body: Option<&impl Serialize>,
) -> Result<T> {
    let client = Client::builder()
        .timeout(Duration::from_secs(120))
        .build()?;

    let url = format!("{}{}", API_BASE, path);
    let timestamp = get_timestamp();

    let body_str = match body {
        Some(b) => serde_json::to_string(b)?,
        None => String::new(),
    };

    let signature = sign_request(&creds.secret_key, timestamp, method, path, &body_str);

    let mut request = match method {
        "GET" => client.get(&url),
        "POST" => client.post(&url),
        "PATCH" => client.patch(&url),
        "PUT" => client.put(&url),
        "DELETE" => client.delete(&url),
        _ => client.get(&url),
    };

    request = request
        .header("Authorization", format!("Bearer {}", creds.public_key))
        .header("X-Timestamp", timestamp.to_string())
        .header("X-Signature", signature)
        .header("Content-Type", "application/json")
        .header("User-Agent", "un-rust-sync/2.0");

    if !body_str.is_empty() {
        request = request.body(body_str);
    }

    let response = request.send()?;
    let status = response.status().as_u16();
    let response_text = response.text()?;

    if status < 200 || status >= 300 {
        return Err(UnsandboxError::ApiError {
            status,
            message: response_text,
        });
    }

    let result: T = serde_json::from_str(&response_text)?;
    Ok(result)
}

// =============================================================================
// Languages Cache
// =============================================================================

/// Get path to languages cache file
fn get_languages_cache_path() -> Option<PathBuf> {
    get_unsandbox_dir().map(|d| d.join("languages.json"))
}

/// Load languages from cache if valid (< 1 hour old)
fn load_languages_cache() -> Option<Vec<String>> {
    let cache_path = get_languages_cache_path()?;
    let content = fs::read_to_string(&cache_path).ok()?;
    let cache: LanguagesCache = serde_json::from_str(&content).ok()?;

    let now = get_timestamp();
    if now - cache.timestamp < LANGUAGES_CACHE_TTL {
        Some(cache.languages)
    } else {
        None
    }
}

/// Save languages to cache
fn save_languages_cache(languages: &[String]) {
    if let Some(cache_path) = get_languages_cache_path() {
        let _ = ensure_unsandbox_dir();
        let cache = LanguagesCache {
            languages: languages.to_vec(),
            timestamp: get_timestamp(),
        };
        if let Ok(content) = serde_json::to_string_pretty(&cache) {
            let _ = fs::write(cache_path, content);
        }
    }
}

// =============================================================================
// Public API Functions
// =============================================================================

/// Execute code synchronously (blocks until completion).
///
/// # Arguments
/// * `language` - Programming language (e.g., "python", "javascript")
/// * `code` - Source code to execute
/// * `creds` - API credentials
///
/// # Returns
/// ExecuteResult with output and exit code
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let result = execute_code("python", r#"print("Hello, World!")"#, &creds)?;
/// println!("Output: {}", result.output);
/// println!("Exit code: {}", result.exit_code);
/// ```
pub fn execute_code(language: &str, code: &str, creds: &Credentials) -> Result<ExecuteResult> {
    let body = serde_json::json!({
        "language": language,
        "code": code
    });

    let response: ExecuteResponse = make_request("POST", "/execute", creds, Some(&body))?;

    // If job is still pending/running, poll until completion
    if response.status == "pending" || response.status == "running" {
        return wait_for_job(&response.job_id, creds, None);
    }

    Ok(ExecuteResult {
        job_id: response.job_id,
        status: response.status,
        output: response.output,
        exit_code: response.exit_code,
        execution_time_ms: response.execution_time_ms,
    })
}

/// Execute code asynchronously (returns immediately with job_id).
///
/// # Arguments
/// * `language` - Programming language
/// * `code` - Source code to execute
/// * `creds` - API credentials
///
/// # Returns
/// Job ID string for polling
///
/// # Examples
/// ```ignore
/// let job_id = execute_async("python", "import time; time.sleep(5); print('done')", &creds)?;
/// // Do other work...
/// let result = wait_for_job(&job_id, &creds, None)?;
/// ```
pub fn execute_async(language: &str, code: &str, creds: &Credentials) -> Result<String> {
    let body = serde_json::json!({
        "language": language,
        "code": code
    });

    let response: ExecuteResponse = make_request("POST", "/execute", creds, Some(&body))?;
    Ok(response.job_id)
}

/// Get current status/result of a job (single poll, no waiting).
///
/// # Arguments
/// * `job_id` - Job ID from execute_async
/// * `creds` - API credentials
///
/// # Returns
/// JobStatus with current state
pub fn get_job(job_id: &str, creds: &Credentials) -> Result<JobStatus> {
    let path = format!("/jobs/{}", job_id);
    make_request("GET", &path, creds, None::<&()>)
}

/// Wait for job completion with exponential backoff polling.
///
/// Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
///
/// # Arguments
/// * `job_id` - Job ID from execute_async
/// * `creds` - API credentials
/// * `timeout` - Optional timeout in seconds (default: 300)
///
/// # Returns
/// ExecuteResult when job completes
///
/// # Errors
/// Returns UnsandboxError::Timeout if job doesn't complete within timeout
pub fn wait_for_job(
    job_id: &str,
    creds: &Credentials,
    timeout: Option<u64>,
) -> Result<ExecuteResult> {
    let timeout_secs = timeout.unwrap_or(DEFAULT_TIMEOUT_SECS);
    let start = std::time::Instant::now();
    let mut poll_count = 0;

    loop {
        // Check timeout
        if start.elapsed().as_secs() >= timeout_secs {
            return Err(UnsandboxError::Timeout(timeout_secs));
        }

        // Sleep before polling
        let delay_idx = poll_count.min(POLL_DELAYS_MS.len() - 1);
        thread::sleep(Duration::from_millis(POLL_DELAYS_MS[delay_idx]));
        poll_count += 1;

        let status = get_job(job_id, creds)?;

        match status.status.as_str() {
            "completed" | "failed" | "timeout" | "cancelled" => {
                return Ok(ExecuteResult {
                    job_id: status.job_id,
                    status: status.status,
                    output: status.output,
                    exit_code: status.exit_code,
                    execution_time_ms: status.execution_time_ms,
                });
            }
            _ => continue, // Still running, continue polling
        }
    }
}

/// Cancel a running job.
///
/// # Arguments
/// * `job_id` - Job ID to cancel
/// * `creds` - API credentials
pub fn cancel_job(job_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/jobs/{}", job_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>)?;
    Ok(())
}

/// List all jobs for the authenticated account.
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of Job summaries
pub fn list_jobs(creds: &Credentials) -> Result<Vec<Job>> {
    let response: JobsListResponse = make_request("GET", "/jobs", creds, None::<&()>)?;
    Ok(response.jobs)
}

/// Get list of supported programming languages.
///
/// Results are cached for 1 hour in ~/.unsandbox/languages.json
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of language identifiers
pub fn get_languages(creds: &Credentials) -> Result<Vec<String>> {
    // Try cache first
    if let Some(cached) = load_languages_cache() {
        return Ok(cached);
    }

    let response: LanguagesResponse = make_request("GET", "/languages", creds, None::<&()>)?;

    // Cache the result
    save_languages_cache(&response.languages);

    Ok(response.languages)
}

/// Create a snapshot of a session.
///
/// # Arguments
/// * `session_id` - Session ID to snapshot
/// * `creds` - API credentials
/// * `name` - Optional snapshot name
/// * `ephemeral` - If true, create a hot (ephemeral) snapshot
///
/// # Returns
/// Snapshot information
pub fn session_snapshot(
    session_id: &str,
    creds: &Credentials,
    name: Option<&str>,
    ephemeral: bool,
) -> Result<Snapshot> {
    let mut body = serde_json::json!({
        "session_id": session_id,
        "hot": ephemeral
    });

    if let Some(n) = name {
        body["name"] = serde_json::json!(n);
    }

    let response: SnapshotCreateResponse = make_request("POST", "/snapshots", creds, Some(&body))?;

    Ok(Snapshot {
        snapshot_id: response.snapshot_id,
        name: response.name,
        source_type: response.source_type,
        source_id: response.source_id,
        hot: response.hot,
        created_at: response.created_at,
        size_bytes: response.size_bytes,
    })
}

/// Create a snapshot of a service.
///
/// # Arguments
/// * `service_id` - Service ID to snapshot
/// * `creds` - API credentials
/// * `name` - Optional snapshot name
///
/// # Returns
/// Snapshot information
pub fn service_snapshot(
    service_id: &str,
    creds: &Credentials,
    name: Option<&str>,
) -> Result<Snapshot> {
    let mut body = serde_json::json!({
        "service_id": service_id,
        "hot": false
    });

    if let Some(n) = name {
        body["name"] = serde_json::json!(n);
    }

    let response: SnapshotCreateResponse = make_request("POST", "/snapshots", creds, Some(&body))?;

    Ok(Snapshot {
        snapshot_id: response.snapshot_id,
        name: response.name,
        source_type: response.source_type,
        source_id: response.source_id,
        hot: response.hot,
        created_at: response.created_at,
        size_bytes: response.size_bytes,
    })
}

/// List all snapshots for the authenticated account.
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of Snapshot information
pub fn list_snapshots(creds: &Credentials) -> Result<Vec<Snapshot>> {
    let response: SnapshotsListResponse = make_request("GET", "/snapshots", creds, None::<&()>)?;
    Ok(response.snapshots)
}

/// Restore a snapshot to create a new session or service.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to restore
/// * `creds` - API credentials
///
/// # Returns
/// RestoreResult with new session/service ID
pub fn restore_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<RestoreResult> {
    let path = format!("/snapshots/{}/restore", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Delete a snapshot.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to delete
/// * `creds` - API credentials
pub fn delete_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/snapshots/{}", snapshot_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>)?;
    Ok(())
}

/// Lock a snapshot to prevent deletion.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to lock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Snapshot information
pub fn lock_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/lock", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Unlock a snapshot to allow deletion.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to unlock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Snapshot information
pub fn unlock_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/unlock", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Clone a snapshot to create a new snapshot with a different name.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to clone
/// * `name` - Name for the new snapshot
/// * `creds` - API credentials
///
/// # Returns
/// New Snapshot information
pub fn clone_snapshot(snapshot_id: &str, name: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/clone", snapshot_id);
    let body = serde_json::json!({
        "name": name
    });
    make_request("POST", &path, creds, Some(&body))
}

// =============================================================================
// Session API Functions
// =============================================================================

/// List all sessions for the authenticated account.
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of Session information
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let sessions = list_sessions(&creds)?;
/// for session in sessions {
///     println!("{}: {} ({})", session.session_id, session.container_name, session.status);
/// }
/// ```
pub fn list_sessions(creds: &Credentials) -> Result<Vec<Session>> {
    let response: SessionsListResponse = make_request("GET", "/sessions", creds, None::<&()>)?;
    Ok(response.sessions)
}

/// Get details of a specific session.
///
/// # Arguments
/// * `session_id` - Session ID to retrieve
/// * `creds` - API credentials
///
/// # Returns
/// Session information
pub fn get_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}", session_id);
    make_request("GET", &path, creds, None::<&()>)
}

/// Create a new interactive session.
///
/// # Arguments
/// * `language` - Programming language/shell (e.g., "bash", "python")
/// * `creds` - API credentials
/// * `opts` - Optional session creation options
///
/// # Returns
/// Created Session information
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
///
/// // Create a basic bash session
/// let session = create_session("bash", &creds, None)?;
///
/// // Create a session with options
/// let opts = SessionCreateOptions {
///     network_mode: Some("semitrusted".to_string()),
///     tmux: Some(true),
///     ..Default::default()
/// };
/// let session = create_session("bash", &creds, Some(opts))?;
/// ```
pub fn create_session(
    language: &str,
    creds: &Credentials,
    opts: Option<SessionCreateOptions>,
) -> Result<Session> {
    let mut body = serde_json::json!({
        "language": language
    });

    if let Some(opts) = opts {
        if let Some(network_mode) = opts.network_mode {
            body["network_mode"] = serde_json::json!(network_mode);
        }
        if let Some(shell) = opts.shell {
            body["shell"] = serde_json::json!(shell);
        }
        if let Some(vcpu) = opts.vcpu {
            body["vcpu"] = serde_json::json!(vcpu);
        }
        if let Some(tmux) = opts.tmux {
            body["tmux"] = serde_json::json!(tmux);
        }
        if let Some(screen) = opts.screen {
            body["screen"] = serde_json::json!(screen);
        }
    }

    make_request("POST", "/sessions", creds, Some(&body))
}

/// Delete (terminate) a session.
///
/// # Arguments
/// * `session_id` - Session ID to delete
/// * `creds` - API credentials
pub fn delete_session(session_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/sessions/{}", session_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>)?;
    Ok(())
}

/// Freeze a session to save resources while preserving state.
///
/// Frozen sessions can be unfrozen later to resume work.
///
/// # Arguments
/// * `session_id` - Session ID to freeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub fn freeze_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/freeze", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Unfreeze a frozen session to resume work.
///
/// # Arguments
/// * `session_id` - Session ID to unfreeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub fn unfreeze_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/unfreeze", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Boost a session's resources (increase vCPU and memory).
///
/// Memory is derived from vCPU: vcpu * 2048MB.
///
/// # Arguments
/// * `session_id` - Session ID to boost
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub fn boost_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/boost", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Remove boost from a session (return to base resources).
///
/// # Arguments
/// * `session_id` - Session ID to unboost
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub fn unboost_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/unboost", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Execute a shell command in a session.
///
/// # Arguments
/// * `session_id` - Session ID to execute command in
/// * `command` - Command to execute
/// * `creds` - API credentials
///
/// # Returns
/// ShellResult with output and exit code
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let result = shell_session("session-123", "ls -la", &creds)?;
/// println!("Output: {}", result.output);
/// println!("Exit code: {}", result.exit_code);
/// ```
pub fn shell_session(session_id: &str, command: &str, creds: &Credentials) -> Result<ShellResult> {
    let path = format!("/sessions/{}/shell", session_id);
    let body = serde_json::json!({
        "command": command
    });
    make_request("POST", &path, creds, Some(&body))
}

// =============================================================================
// Service API Functions
// =============================================================================

/// List all services for the authenticated account.
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of Service information
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let services = list_services(&creds)?;
/// for service in services {
///     println!("{}: {} ({}) - {}", service.service_id, service.name, service.status, service.url);
/// }
/// ```
pub fn list_services(creds: &Credentials) -> Result<Vec<Service>> {
    let response: ServicesListResponse = make_request("GET", "/services", creds, None::<&()>)?;
    Ok(response.services)
}

/// Create a new persistent service.
///
/// # Arguments
/// * `name` - Service name (used in URL: name.on.unsandbox.com)
/// * `ports` - Ports to expose
/// * `bootstrap` - Bootstrap script content to run on startup
/// * `creds` - API credentials
/// * `opts` - Optional service creation options
///
/// # Returns
/// Created Service information
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
///
/// // Create a simple web service
/// let service = create_service(
///     "myapp",
///     &[8080],
///     "python3 -m http.server 8080",
///     &creds,
///     None
/// )?;
/// println!("Service URL: {}", service.url);
///
/// // Create with options
/// let opts = ServiceCreateOptions {
///     network_mode: Some("semitrusted".to_string()),
///     vcpu: Some(2),
///     domains: Some(vec!["example.com".to_string()]),
///     ..Default::default()
/// };
/// let service = create_service("myapp", &[80, 443], bootstrap, &creds, Some(opts))?;
/// ```
pub fn create_service(
    name: &str,
    ports: &[u16],
    bootstrap: &str,
    creds: &Credentials,
    opts: Option<ServiceCreateOptions>,
) -> Result<Service> {
    let mut body = serde_json::json!({
        "name": name,
        "ports": ports,
        "bootstrap": bootstrap
    });

    if let Some(opts) = opts {
        if let Some(network_mode) = opts.network_mode {
            body["network_mode"] = serde_json::json!(network_mode);
        }
        if let Some(vcpu) = opts.vcpu {
            body["vcpu"] = serde_json::json!(vcpu);
        }
        if let Some(domains) = opts.domains {
            body["domains"] = serde_json::json!(domains);
        }
        if let Some(bootstrap_url) = opts.bootstrap_url {
            body["bootstrap_url"] = serde_json::json!(bootstrap_url);
        }
    }

    make_request("POST", "/services", creds, Some(&body))
}

/// Get details of a specific service.
///
/// # Arguments
/// * `service_id` - Service ID to retrieve
/// * `creds` - API credentials
///
/// # Returns
/// Service information
pub fn get_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}", service_id);
    make_request("GET", &path, creds, None::<&()>)
}

/// Update a service's configuration.
///
/// # Arguments
/// * `service_id` - Service ID to update
/// * `creds` - API credentials
/// * `opts` - Update options (name, ports, domains, vcpu)
///
/// # Returns
/// Updated Service information
pub fn update_service(
    service_id: &str,
    creds: &Credentials,
    opts: ServiceUpdateOptions,
) -> Result<Service> {
    let path = format!("/services/{}", service_id);
    let mut body = serde_json::Map::new();

    if let Some(name) = opts.name {
        body.insert("name".to_string(), serde_json::json!(name));
    }
    if let Some(ports) = opts.ports {
        body.insert("ports".to_string(), serde_json::json!(ports));
    }
    if let Some(domains) = opts.domains {
        body.insert("domains".to_string(), serde_json::json!(domains));
    }
    if let Some(vcpu) = opts.vcpu {
        body.insert("vcpu".to_string(), serde_json::json!(vcpu));
    }

    make_request("PATCH", &path, creds, Some(&serde_json::Value::Object(body)))
}

/// Delete (destroy) a service.
///
/// # Arguments
/// * `service_id` - Service ID to delete
/// * `creds` - API credentials
pub fn delete_service(service_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/services/{}", service_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>)?;
    Ok(())
}

/// Freeze a service to save resources while preserving state.
///
/// # Arguments
/// * `service_id` - Service ID to freeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub fn freeze_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/freeze", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Unfreeze a frozen service to resume operation.
///
/// # Arguments
/// * `service_id` - Service ID to unfreeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub fn unfreeze_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/unfreeze", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Lock a service to prevent modifications.
///
/// # Arguments
/// * `service_id` - Service ID to lock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub fn lock_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/lock", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Unlock a service to allow modifications.
///
/// # Arguments
/// * `service_id` - Service ID to unlock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub fn unlock_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/unlock", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Get bootstrap logs for a service.
///
/// # Arguments
/// * `service_id` - Service ID to get logs for
/// * `all` - If true, get all logs; if false, get last 9000 lines
/// * `creds` - API credentials
///
/// # Returns
/// Log content as string
pub fn get_service_logs(service_id: &str, all: bool, creds: &Credentials) -> Result<String> {
    let path = if all {
        format!("/services/{}/logs?all=true", service_id)
    } else {
        format!("/services/{}/logs", service_id)
    };

    #[derive(Deserialize)]
    struct LogsResponse {
        #[serde(default)]
        logs: String,
    }

    let response: LogsResponse = make_request("GET", &path, creds, None::<&()>)?;
    Ok(response.logs)
}

/// Get environment variables for a service.
///
/// # Arguments
/// * `service_id` - Service ID to get env for
/// * `creds` - API credentials
///
/// # Returns
/// HashMap of environment variable key-value pairs
pub fn get_service_env(service_id: &str, creds: &Credentials) -> Result<HashMap<String, String>> {
    let path = format!("/services/{}/env", service_id);
    let response: EnvResponse = make_request("GET", &path, creds, None::<&()>)?;
    Ok(response.env)
}

/// Set environment variables for a service.
///
/// # Arguments
/// * `service_id` - Service ID to set env for
/// * `env` - HashMap of environment variable key-value pairs
/// * `creds` - API credentials
pub fn set_service_env(
    service_id: &str,
    env: &HashMap<String, String>,
    creds: &Credentials,
) -> Result<()> {
    let path = format!("/services/{}/env", service_id);

    // Convert to .env format
    let content: String = env
        .iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect::<Vec<_>>()
        .join("\n");

    // Use PUT with text/plain content type
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(120))
        .build()?;

    let url = format!("{}{}", API_BASE, path);
    let timestamp = get_timestamp();
    let signature = sign_request(&creds.secret_key, timestamp, "PUT", &path, &content);

    let response = client
        .put(&url)
        .header("Authorization", format!("Bearer {}", creds.public_key))
        .header("X-Timestamp", timestamp.to_string())
        .header("X-Signature", signature)
        .header("Content-Type", "text/plain")
        .header("User-Agent", "un-rust-sync/2.0")
        .body(content)
        .send()?;

    let status = response.status().as_u16();
    if status < 200 || status >= 300 {
        let response_text = response.text()?;
        return Err(UnsandboxError::ApiError {
            status,
            message: response_text,
        });
    }

    Ok(())
}

/// Delete environment variables for a service.
///
/// # Arguments
/// * `service_id` - Service ID to delete env for
/// * `keys` - List of environment variable keys to delete
/// * `creds` - API credentials
pub fn delete_service_env(
    service_id: &str,
    keys: &[&str],
    creds: &Credentials,
) -> Result<()> {
    let path = format!("/services/{}/env", service_id);
    let body = serde_json::json!({
        "keys": keys
    });
    let _: serde_json::Value = make_request("DELETE", &path, creds, Some(&body))?;
    Ok(())
}

/// Export environment variables for a service in .env format.
///
/// # Arguments
/// * `service_id` - Service ID to export env for
/// * `creds` - API credentials
///
/// # Returns
/// Environment variables in .env format string
pub fn export_service_env(service_id: &str, creds: &Credentials) -> Result<String> {
    let path = format!("/services/{}/env/export", service_id);
    let body = serde_json::json!({});
    let response: EnvExportResponse = make_request("POST", &path, creds, Some(&body))?;
    Ok(response.content)
}

/// Redeploy a service with a new bootstrap script.
///
/// # Arguments
/// * `service_id` - Service ID to redeploy
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub fn redeploy_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/redeploy", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body))
}

/// Execute a command in a service container.
///
/// # Arguments
/// * `service_id` - Service ID to execute command in
/// * `command` - Command to execute
/// * `creds` - API credentials
///
/// # Returns
/// ExecuteResult with output and exit code
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let result = execute_in_service("service-123", "ls -la /app", &creds)?;
/// println!("Output: {}", result.output);
/// ```
pub fn execute_in_service(
    service_id: &str,
    command: &str,
    creds: &Credentials,
) -> Result<ExecuteResult> {
    let path = format!("/services/{}/execute", service_id);
    let body = serde_json::json!({
        "command": command,
        "timeout": 30000
    });
    make_request("POST", &path, creds, Some(&body))
}

// =============================================================================
// Key Validation API Functions
// =============================================================================

/// Validate API keys.
///
/// # Arguments
/// * `creds` - API credentials to validate
///
/// # Returns
/// KeysValid with validation result and account info
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let result = validate_keys(&creds)?;
/// if result.valid {
///     println!("Keys valid for account: {}", result.account_id);
/// } else {
///     println!("Invalid keys: {}", result.error);
/// }
/// ```
pub fn validate_keys(creds: &Credentials) -> Result<KeysValid> {
    // Note: This endpoint is on the portal, not the API
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(30))
        .build()?;

    let url = "https://unsandbox.com/keys/validate";
    let path = "/keys/validate";
    let timestamp = get_timestamp();
    let body_str = "";
    let signature = sign_request(&creds.secret_key, timestamp, "POST", path, body_str);

    let response = client
        .post(url)
        .header("Authorization", format!("Bearer {}", creds.public_key))
        .header("X-Timestamp", timestamp.to_string())
        .header("X-Signature", signature)
        .header("Content-Type", "application/json")
        .header("User-Agent", "un-rust-sync/2.0")
        .body("")
        .send()?;

    let status = response.status().as_u16();
    let response_text = response.text()?;

    if status < 200 || status >= 300 {
        return Err(UnsandboxError::ApiError {
            status,
            message: response_text,
        });
    }

    let result: KeysValid = serde_json::from_str(&response_text)?;
    Ok(result)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_language() {
        assert_eq!(detect_language("hello.py"), Some("python"));
        assert_eq!(detect_language("script.js"), Some("javascript"));
        assert_eq!(detect_language("main.go"), Some("go"));
        assert_eq!(detect_language("test.rs"), Some("rust"));
        assert_eq!(detect_language("app.ts"), Some("typescript"));
        assert_eq!(detect_language("Makefile"), None);
        assert_eq!(detect_language("unknown"), None);
        assert_eq!(detect_language("file.unknown_ext"), None);
    }

    #[test]
    fn test_sign_request() {
        let signature = sign_request(
            "test-secret",
            1234567890,
            "POST",
            "/execute",
            r#"{"language":"python","code":"print(42)"}"#,
        );
        // Signature should be 64 hex characters
        assert_eq!(signature.len(), 64);
        assert!(signature.chars().all(|c| c.is_ascii_hexdigit()));
    }

    #[test]
    fn test_credentials_new() {
        let creds = Credentials::new("unsb-pk-test", "unsb-sk-test");
        assert_eq!(creds.public_key, "unsb-pk-test");
        assert_eq!(creds.secret_key, "unsb-sk-test");
    }

    #[test]
    fn test_get_timestamp() {
        let ts = get_timestamp();
        // Should be a reasonable Unix timestamp (after 2024)
        assert!(ts > 1700000000);
    }
}
