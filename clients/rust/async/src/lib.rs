// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// unsandbox.com Rust SDK (Asynchronous)
//
// Library Usage:
//     use un_async::{Credentials, execute_code, resolve_credentials};
//
//     #[tokio::main]
//     async fn main() -> Result<(), un_async::UnsandboxError> {
//         // Resolve credentials (4-tier priority)
//         let creds = resolve_credentials(None, None)?;
//
//         // Execute code asynchronously
//         let result = execute_code("python", r#"print("hello")"#, &creds).await?;
//         println!("Output: {}", result.output);
//
//         // Execute async (returns job_id immediately)
//         let job_id = execute_async("javascript", r#"console.log("hello")"#, &creds).await?;
//
//         // Wait for job completion
//         let result = wait_for_job(&job_id, &creds, None).await?;
//
//         // List all jobs
//         let jobs = list_jobs(&creds).await?;
//
//         // Get supported languages (cached 1 hour)
//         let languages = get_languages(&creds).await?;
//
//         // Detect language from filename (sync, no network)
//         let lang = detect_language("script.py");  // Some("python")
//
//         // Snapshot operations
//         let snapshot = session_snapshot(&session_id, &creds, Some("my_snapshot"), false).await?;
//         let snapshots = list_snapshots(&creds).await?;
//         let result = restore_snapshot(&snapshot_id, &creds).await?;
//         delete_snapshot(&snapshot_id, &creds).await?;
//
//         Ok(())
//     }
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
use reqwest::Client;
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::fs;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::time::sleep;

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
    /// Whether the service automatically unfreezes on incoming HTTP requests
    #[serde(default)]
    pub unfreeze_on_demand: bool,
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
    /// Whether to enable automatic unfreezing on incoming HTTP requests
    pub unfreeze_on_demand: Option<bool>,
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

/// Options for AI image generation
#[derive(Debug, Clone, Default)]
pub struct ImageOptions {
    /// Model to use (optional)
    pub model: Option<String>,
    /// Image size (default: "1024x1024")
    pub size: Option<String>,
    /// Quality: "standard" or "hd" (default: "standard")
    pub quality: Option<String>,
    /// Number of images to generate (default: 1)
    pub n: Option<i32>,
}

/// Result of image generation
#[derive(Debug, Clone, Deserialize)]
pub struct ImageResult {
    /// Generated images (base64 or URLs)
    pub images: Vec<String>,
    /// Timestamp when images were created
    pub created_at: String,
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
/// This function is synchronous (no network call needed).
///
/// # Arguments
/// * `filename` - Filename to detect language from (e.g., "script.py")
///
/// # Returns
/// Language identifier (e.g., "python") or None if unknown
///
/// # Examples
/// ```
/// use un_async::detect_language;
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
async fn ensure_unsandbox_dir() -> Option<PathBuf> {
    let dir = get_unsandbox_dir()?;
    fs::create_dir_all(&dir).await.ok()?;
    Some(dir)
}

/// Load credentials from a CSV file (public_key,secret_key per line) - async version
async fn load_credentials_from_csv_async(path: &PathBuf, account_index: usize) -> Option<Credentials> {
    let file = fs::File::open(path).await.ok()?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();
    let mut current_index = 0;

    while let Ok(Some(line)) = lines.next_line().await {
        let line = line.trim().to_string();
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

/// Load credentials from a CSV file (public_key,secret_key per line) - sync version for resolve_credentials
fn load_credentials_from_csv_sync(path: &PathBuf, account_index: usize) -> Option<Credentials> {
    use std::io::{BufRead, BufReader};
    let file = std::fs::File::open(path).ok()?;
    let reader = BufReader::new(file);
    let mut current_index = 0;

    for line in reader.lines().map_while(|l| l.ok()) {
        let line = line.trim().to_string();
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
/// This function is synchronous since it only reads files and env vars.
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
        if let Some(creds) = load_credentials_from_csv_sync(&csv_path, account_index) {
            return Ok(creds);
        }
    }

    // Tier 4: ./accounts.csv
    let local_csv = PathBuf::from("accounts.csv");
    if let Some(creds) = load_credentials_from_csv_sync(&local_csv, account_index) {
        return Ok(creds);
    }

    Err(UnsandboxError::NoCredentials)
}

/// Async version of resolve_credentials
pub async fn resolve_credentials_async(
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
        if let Some(creds) = load_credentials_from_csv_async(&csv_path, account_index).await {
            return Ok(creds);
        }
    }

    // Tier 4: ./accounts.csv
    let local_csv = PathBuf::from("accounts.csv");
    if let Some(creds) = load_credentials_from_csv_async(&local_csv, account_index).await {
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
async fn make_request<T: for<'de> Deserialize<'de>>(
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
        .header("User-Agent", "un-rust-async/2.0");

    if !body_str.is_empty() {
        request = request.body(body_str);
    }

    let response = request.send().await?;
    let status = response.status().as_u16();
    let response_text = response.text().await?;

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
async fn load_languages_cache() -> Option<Vec<String>> {
    let cache_path = get_languages_cache_path()?;
    let content = fs::read_to_string(&cache_path).await.ok()?;
    let cache: LanguagesCache = serde_json::from_str(&content).ok()?;

    let now = get_timestamp();
    if now - cache.timestamp < LANGUAGES_CACHE_TTL {
        Some(cache.languages)
    } else {
        None
    }
}

/// Save languages to cache
async fn save_languages_cache(languages: &[String]) {
    if let Some(cache_path) = get_languages_cache_path() {
        let _ = ensure_unsandbox_dir().await;
        let cache = LanguagesCache {
            languages: languages.to_vec(),
            timestamp: get_timestamp(),
        };
        if let Ok(content) = serde_json::to_string_pretty(&cache) {
            let _ = fs::write(cache_path, content).await;
        }
    }
}

// =============================================================================
// Public API Functions
// =============================================================================

/// Execute code and wait for completion.
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
/// let result = execute_code("python", r#"print("Hello, World!")"#, &creds).await?;
/// println!("Output: {}", result.output);
/// println!("Exit code: {}", result.exit_code);
/// ```
pub async fn execute_code(language: &str, code: &str, creds: &Credentials) -> Result<ExecuteResult> {
    let body = serde_json::json!({
        "language": language,
        "code": code
    });

    let response: ExecuteResponse = make_request("POST", "/execute", creds, Some(&body)).await?;

    // If job is still pending/running, poll until completion
    if response.status == "pending" || response.status == "running" {
        return wait_for_job(&response.job_id, creds, None).await;
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
/// let job_id = execute_async("python", "import time; time.sleep(5); print('done')", &creds).await?;
/// // Do other async work...
/// let result = wait_for_job(&job_id, &creds, None).await?;
/// ```
pub async fn execute_async(language: &str, code: &str, creds: &Credentials) -> Result<String> {
    let body = serde_json::json!({
        "language": language,
        "code": code
    });

    let response: ExecuteResponse = make_request("POST", "/execute", creds, Some(&body)).await?;
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
pub async fn get_job(job_id: &str, creds: &Credentials) -> Result<JobStatus> {
    let path = format!("/jobs/{}", job_id);
    make_request("GET", &path, creds, None::<&()>).await
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
pub async fn wait_for_job(
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

        // Sleep before polling (async)
        let delay_idx = poll_count.min(POLL_DELAYS_MS.len() - 1);
        sleep(Duration::from_millis(POLL_DELAYS_MS[delay_idx])).await;
        poll_count += 1;

        let status = get_job(job_id, creds).await?;

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
pub async fn cancel_job(job_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/jobs/{}", job_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>).await?;
    Ok(())
}

/// List all jobs for the authenticated account.
///
/// # Arguments
/// * `creds` - API credentials
///
/// # Returns
/// Vector of Job summaries
pub async fn list_jobs(creds: &Credentials) -> Result<Vec<Job>> {
    let response: JobsListResponse = make_request("GET", "/jobs", creds, None::<&()>).await?;
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
pub async fn get_languages(creds: &Credentials) -> Result<Vec<String>> {
    // Try cache first
    if let Some(cached) = load_languages_cache().await {
        return Ok(cached);
    }

    let response: LanguagesResponse = make_request("GET", "/languages", creds, None::<&()>).await?;

    // Cache the result
    save_languages_cache(&response.languages).await;

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
pub async fn session_snapshot(
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

    let response: SnapshotCreateResponse = make_request("POST", "/snapshots", creds, Some(&body)).await?;

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
pub async fn service_snapshot(
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

    let response: SnapshotCreateResponse = make_request("POST", "/snapshots", creds, Some(&body)).await?;

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
pub async fn list_snapshots(creds: &Credentials) -> Result<Vec<Snapshot>> {
    let response: SnapshotsListResponse = make_request("GET", "/snapshots", creds, None::<&()>).await?;
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
pub async fn restore_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<RestoreResult> {
    let path = format!("/snapshots/{}/restore", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Delete a snapshot.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to delete
/// * `creds` - API credentials
pub async fn delete_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/snapshots/{}", snapshot_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>).await?;
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
pub async fn lock_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/lock", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Unlock a snapshot to allow deletion.
///
/// # Arguments
/// * `snapshot_id` - Snapshot ID to unlock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Snapshot information
pub async fn unlock_snapshot(snapshot_id: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/unlock", snapshot_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
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
pub async fn clone_snapshot(snapshot_id: &str, name: &str, creds: &Credentials) -> Result<Snapshot> {
    let path = format!("/snapshots/{}/clone", snapshot_id);
    let body = serde_json::json!({
        "name": name
    });
    make_request("POST", &path, creds, Some(&body)).await
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
/// let sessions = list_sessions(&creds).await?;
/// for session in sessions {
///     println!("{}: {} ({})", session.session_id, session.container_name, session.status);
/// }
/// ```
pub async fn list_sessions(creds: &Credentials) -> Result<Vec<Session>> {
    let response: SessionsListResponse = make_request("GET", "/sessions", creds, None::<&()>).await?;
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
pub async fn get_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}", session_id);
    make_request("GET", &path, creds, None::<&()>).await
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
/// let session = create_session("bash", &creds, None).await?;
///
/// // Create a session with options
/// let opts = SessionCreateOptions {
///     network_mode: Some("semitrusted".to_string()),
///     tmux: Some(true),
///     ..Default::default()
/// };
/// let session = create_session("bash", &creds, Some(opts)).await?;
/// ```
pub async fn create_session(
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

    make_request("POST", "/sessions", creds, Some(&body)).await
}

/// Delete (terminate) a session.
///
/// # Arguments
/// * `session_id` - Session ID to delete
/// * `creds` - API credentials
pub async fn delete_session(session_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/sessions/{}", session_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>).await?;
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
pub async fn freeze_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/freeze", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Unfreeze a frozen session to resume work.
///
/// # Arguments
/// * `session_id` - Session ID to unfreeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub async fn unfreeze_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/unfreeze", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
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
pub async fn boost_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/boost", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Remove boost from a session (return to base resources).
///
/// # Arguments
/// * `session_id` - Session ID to unboost
/// * `creds` - API credentials
///
/// # Returns
/// Updated Session information
pub async fn unboost_session(session_id: &str, creds: &Credentials) -> Result<Session> {
    let path = format!("/sessions/{}/unboost", session_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
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
/// let result = shell_session("session-123", "ls -la", &creds).await?;
/// println!("Output: {}", result.output);
/// println!("Exit code: {}", result.exit_code);
/// ```
pub async fn shell_session(session_id: &str, command: &str, creds: &Credentials) -> Result<ShellResult> {
    let path = format!("/sessions/{}/shell", session_id);
    let body = serde_json::json!({
        "command": command
    });
    make_request("POST", &path, creds, Some(&body)).await
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
/// let services = list_services(&creds).await?;
/// for service in services {
///     println!("{}: {} ({}) - {}", service.service_id, service.name, service.status, service.url);
/// }
/// ```
pub async fn list_services(creds: &Credentials) -> Result<Vec<Service>> {
    let response: ServicesListResponse = make_request("GET", "/services", creds, None::<&()>).await?;
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
/// ).await?;
/// println!("Service URL: {}", service.url);
///
/// // Create with options
/// let opts = ServiceCreateOptions {
///     network_mode: Some("semitrusted".to_string()),
///     vcpu: Some(2),
///     domains: Some(vec!["example.com".to_string()]),
///     ..Default::default()
/// };
/// let service = create_service("myapp", &[80, 443], bootstrap, &creds, Some(opts)).await?;
/// ```
pub async fn create_service(
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
        if let Some(unfreeze_on_demand) = opts.unfreeze_on_demand {
            body["unfreeze_on_demand"] = serde_json::json!(unfreeze_on_demand);
        }
    }

    make_request("POST", "/services", creds, Some(&body)).await
}

/// Get details of a specific service.
///
/// # Arguments
/// * `service_id` - Service ID to retrieve
/// * `creds` - API credentials
///
/// # Returns
/// Service information
pub async fn get_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}", service_id);
    make_request("GET", &path, creds, None::<&()>).await
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
pub async fn update_service(
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

    make_request("PATCH", &path, creds, Some(&serde_json::Value::Object(body))).await
}

/// Delete (destroy) a service.
///
/// # Arguments
/// * `service_id` - Service ID to delete
/// * `creds` - API credentials
pub async fn delete_service(service_id: &str, creds: &Credentials) -> Result<()> {
    let path = format!("/services/{}", service_id);
    let _: serde_json::Value = make_request("DELETE", &path, creds, None::<&()>).await?;
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
pub async fn freeze_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/freeze", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Unfreeze a frozen service to resume operation.
///
/// # Arguments
/// * `service_id` - Service ID to unfreeze
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub async fn unfreeze_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/unfreeze", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Lock a service to prevent modifications.
///
/// # Arguments
/// * `service_id` - Service ID to lock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub async fn lock_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/lock", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Unlock a service to allow modifications.
///
/// # Arguments
/// * `service_id` - Service ID to unlock
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub async fn unlock_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/unlock", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
}

/// Set the unfreeze_on_demand flag for a service.
///
/// When enabled, the service will automatically unfreeze when it receives
/// an incoming HTTP request while frozen.
///
/// # Arguments
/// * `service_id` - Service ID to update
/// * `enabled` - Whether to enable automatic unfreezing on demand
/// * `creds` - API credentials
///
/// # Returns
/// Updated Service information
pub async fn set_unfreeze_on_demand(service_id: &str, enabled: bool, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}", service_id);
    let body = serde_json::json!({
        "unfreeze_on_demand": enabled
    });
    make_request("PATCH", &path, creds, Some(&body)).await
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
pub async fn get_service_logs(service_id: &str, all: bool, creds: &Credentials) -> Result<String> {
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

    let response: LogsResponse = make_request("GET", &path, creds, None::<&()>).await?;
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
pub async fn get_service_env(service_id: &str, creds: &Credentials) -> Result<HashMap<String, String>> {
    let path = format!("/services/{}/env", service_id);
    let response: EnvResponse = make_request("GET", &path, creds, None::<&()>).await?;
    Ok(response.env)
}

/// Set environment variables for a service.
///
/// # Arguments
/// * `service_id` - Service ID to set env for
/// * `env` - HashMap of environment variable key-value pairs
/// * `creds` - API credentials
pub async fn set_service_env(
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
    let client = Client::builder()
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
        .header("User-Agent", "un-rust-async/2.0")
        .body(content)
        .send()
        .await?;

    let status = response.status().as_u16();
    if status < 200 || status >= 300 {
        let response_text = response.text().await?;
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
pub async fn delete_service_env(
    service_id: &str,
    keys: &[&str],
    creds: &Credentials,
) -> Result<()> {
    let path = format!("/services/{}/env", service_id);
    let body = serde_json::json!({
        "keys": keys
    });
    let _: serde_json::Value = make_request("DELETE", &path, creds, Some(&body)).await?;
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
pub async fn export_service_env(service_id: &str, creds: &Credentials) -> Result<String> {
    let path = format!("/services/{}/env/export", service_id);
    let body = serde_json::json!({});
    let response: EnvExportResponse = make_request("POST", &path, creds, Some(&body)).await?;
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
pub async fn redeploy_service(service_id: &str, creds: &Credentials) -> Result<Service> {
    let path = format!("/services/{}/redeploy", service_id);
    let body = serde_json::json!({});
    make_request("POST", &path, creds, Some(&body)).await
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
/// let result = execute_in_service("service-123", "ls -la /app", &creds).await?;
/// println!("Output: {}", result.output);
/// ```
pub async fn execute_in_service(
    service_id: &str,
    command: &str,
    creds: &Credentials,
) -> Result<ExecuteResult> {
    let path = format!("/services/{}/execute", service_id);
    let body = serde_json::json!({
        "command": command,
        "timeout": 30000
    });
    make_request("POST", &path, creds, Some(&body)).await
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
/// let result = validate_keys(&creds).await?;
/// if result.valid {
///     println!("Keys valid for account: {}", result.account_id);
/// } else {
///     println!("Invalid keys: {}", result.error);
/// }
/// ```
pub async fn validate_keys(creds: &Credentials) -> Result<KeysValid> {
    // Note: This endpoint is on the portal, not the API
    let client = Client::builder()
        .timeout(Duration::from_secs(30))
        .build()?;

    let url = "https://api.unsandbox.com/keys/validate";
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
        .header("User-Agent", "un-rust-async/2.0")
        .body("")
        .send()
        .await?;

    let status = response.status().as_u16();
    let response_text = response.text().await?;

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
// AI Image Generation API Functions
// =============================================================================

/// Generate images from a text prompt using AI.
///
/// # Arguments
/// * `prompt` - Text description of the image to generate
/// * `creds` - API credentials
/// * `opts` - Optional generation parameters
///
/// # Returns
/// * `Result<ImageResult>` - Generated images
///
/// # Examples
/// ```ignore
/// let creds = resolve_credentials(None, None)?;
/// let result = image("A sunset over mountains", &creds, None).await?;
/// for img in result.images {
///     println!("Image: {}", img);
/// }
///
/// // With options
/// let opts = ImageOptions {
///     size: Some("512x512".to_string()),
///     quality: Some("hd".to_string()),
///     n: Some(2),
///     ..Default::default()
/// };
/// let result = image("A futuristic city", &creds, Some(opts)).await?;
/// ```
pub async fn image(prompt: &str, creds: &Credentials, opts: Option<ImageOptions>) -> Result<ImageResult> {
    let opts = opts.unwrap_or_default();

    let payload = serde_json::json!({
        "prompt": prompt,
        "size": opts.size.unwrap_or_else(|| "1024x1024".to_string()),
        "quality": opts.quality.unwrap_or_else(|| "standard".to_string()),
        "n": opts.n.unwrap_or(1),
        "model": opts.model,
    });

    make_request("POST", "/image", creds, Some(&payload)).await
}

// =============================================================================
// CLI Exit Codes
// =============================================================================

/// Exit code for success
pub const EXIT_SUCCESS: i32 = 0;
/// Exit code for general error
pub const EXIT_ERROR: i32 = 1;
/// Exit code for invalid arguments
pub const EXIT_INVALID_ARGS: i32 = 2;
/// Exit code for authentication error
pub const EXIT_AUTH_ERROR: i32 = 3;
/// Exit code for API error
pub const EXIT_API_ERROR: i32 = 4;
/// Exit code for timeout
pub const EXIT_TIMEOUT: i32 = 5;

// =============================================================================
// CLI Implementation
// =============================================================================

/// CLI options parsed from command line arguments
#[derive(Debug, Default)]
struct CliOptions {
    // Global options
    shell: Option<String>,         // -s, --shell
    env_vars: Vec<(String, String)>, // -e, --env
    files: Vec<String>,            // -f, --file
    file_paths: Vec<String>,       // -F, --file-path
    artifacts: bool,               // -a, --artifacts
    output_dir: Option<String>,    // -o, --output
    public_key: Option<String>,    // -p, --public-key
    secret_key: Option<String>,    // -k, --secret-key
    network: Option<String>,       // -n, --network
    vcpu: Option<u32>,             // -v, --vcpu
    yes: bool,                     // -y, --yes
    help: bool,                    // -h, --help

    // Command and positional args
    command: Option<String>,
    subcommand: Option<String>,
    positional: Vec<String>,

    // Session options
    session_list: bool,
    session_attach: Option<String>,
    session_kill: Option<String>,
    session_freeze: Option<String>,
    session_unfreeze: Option<String>,
    session_boost: Option<String>,
    session_unboost: Option<String>,
    session_snapshot: Option<String>,
    session_tmux: bool,
    session_screen: bool,
    snapshot_name: Option<String>,
    snapshot_hot: bool,
    audit: bool,

    // Service options
    service_list: bool,
    service_name: Option<String>,
    service_ports: Option<String>,
    service_domains: Option<String>,
    service_type: Option<String>,
    service_bootstrap: Option<String>,
    service_bootstrap_file: Option<String>,
    service_env_file: Option<String>,
    service_info: Option<String>,
    service_logs: Option<String>,
    service_tail: Option<String>,
    service_freeze: Option<String>,
    service_unfreeze: Option<String>,
    service_destroy: Option<String>,
    service_lock: Option<String>,
    service_unlock: Option<String>,
    service_resize: Option<String>,
    service_redeploy: Option<String>,
    service_execute: Option<String>,
    service_execute_cmd: Option<String>,
    service_snapshot: Option<String>,

    // Snapshot options
    snapshot_list: bool,
    snapshot_info: Option<String>,
    snapshot_delete: Option<String>,
    snapshot_lock: Option<String>,
    snapshot_unlock: Option<String>,
    snapshot_clone: Option<String>,
    clone_type: Option<String>,
    clone_name: Option<String>,
    clone_shell: Option<String>,
    clone_ports: Option<String>,
}

fn print_help() {
    println!("un - unsandbox.com CLI (Rust async)

USAGE:
    un [OPTIONS] <source_file>           Execute code file
    un [OPTIONS] -s LANG 'code'          Execute inline code
    un session [OPTIONS]                 Interactive session
    un service [OPTIONS]                 Manage services
    un snapshot [OPTIONS]                Manage snapshots
    un key                               Check API key

GLOBAL OPTIONS:
    -s, --shell LANG       Language for inline code execution
    -e, --env KEY=VAL      Set environment variable (can repeat)
    -f, --file FILE        Add input file to /tmp/
    -F, --file-path FILE   Add input file with path preserved
    -a, --artifacts        Return compiled artifacts
    -o, --output DIR       Output directory for artifacts
    -p, --public-key KEY   API public key
    -k, --secret-key KEY   API secret key
    -n, --network MODE     Network mode: zerotrust or semitrusted
    -v, --vcpu N           vCPU count (1-8)
    -y, --yes              Skip confirmation prompts
    -h, --help             Show this help

SESSION COMMANDS:
    un session                     Start interactive bash session
    un session --shell python3     Start Python REPL
    un session --tmux              Persistent session with tmux
    un session --screen            Persistent session with screen
    un session --list              List active sessions
    un session --attach ID         Reconnect to session
    un session --kill ID           Terminate session
    un session --freeze ID         Pause session
    un session --unfreeze ID       Resume session
    un session --boost ID          Add resources
    un session --unboost ID        Remove boost
    un session --snapshot ID       Create snapshot

SERVICE COMMANDS:
    un service --list                          List all services
    un service --name NAME --ports P           Create service
    un service --info ID                       Get service details
    un service --logs ID                       Get all logs
    un service --tail ID                       Get last 9000 lines
    un service --freeze ID                     Pause service
    un service --unfreeze ID                   Resume service
    un service --destroy ID                    Delete service
    un service --lock ID                       Prevent deletion
    un service --unlock ID                     Allow deletion
    un service --execute ID 'cmd'              Run command
    un service --redeploy ID                   Re-run bootstrap
    un service --snapshot ID                   Create snapshot
    un service env status ID                   Show vault status
    un service env set ID                      Set env vars
    un service env export ID                   Export env vars
    un service env delete ID                   Delete vault

SNAPSHOT COMMANDS:
    un snapshot --list             List all snapshots
    un snapshot --info ID          Get snapshot details
    un snapshot --delete ID        Delete snapshot
    un snapshot --lock ID          Prevent deletion
    un snapshot --unlock ID        Allow deletion
    un snapshot --clone ID         Clone snapshot

EXAMPLES:
    un script.py                   Execute Python script
    un -s bash 'echo hello'        Run inline bash command
    un -n semitrusted crawler.py   Execute with network access
    un session --tmux              Start persistent session
    un service --name web --ports 80 --bootstrap 'python -m http.server 80'
");
}

fn parse_args(args: &[String]) -> CliOptions {
    let mut opts = CliOptions::default();
    let mut i = 1; // Skip program name

    while i < args.len() {
        let arg = &args[i];

        match arg.as_str() {
            "-h" | "--help" => {
                opts.help = true;
                i += 1;
            }
            "-s" | "--shell" => {
                if i + 1 < args.len() {
                    opts.shell = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-e" | "--env" => {
                if i + 1 < args.len() {
                    let kv = &args[i + 1];
                    if let Some(pos) = kv.find('=') {
                        let key = kv[..pos].to_string();
                        let val = kv[pos + 1..].to_string();
                        opts.env_vars.push((key, val));
                    }
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-f" | "--file" => {
                if i + 1 < args.len() {
                    opts.files.push(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-F" | "--file-path" => {
                if i + 1 < args.len() {
                    opts.file_paths.push(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-a" | "--artifacts" => {
                opts.artifacts = true;
                i += 1;
            }
            "-o" | "--output" => {
                if i + 1 < args.len() {
                    opts.output_dir = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-p" | "--public-key" => {
                if i + 1 < args.len() {
                    opts.public_key = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-k" | "--secret-key" => {
                if i + 1 < args.len() {
                    opts.secret_key = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-n" | "--network" => {
                if i + 1 < args.len() {
                    opts.network = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-v" | "--vcpu" => {
                if i + 1 < args.len() {
                    opts.vcpu = args[i + 1].parse().ok();
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "-y" | "--yes" => {
                opts.yes = true;
                i += 1;
            }
            "-l" | "--list" => {
                // Used by session, service, snapshot
                opts.session_list = true;
                opts.service_list = true;
                opts.snapshot_list = true;
                i += 1;
            }
            "--attach" => {
                if i + 1 < args.len() {
                    opts.session_attach = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--kill" => {
                if i + 1 < args.len() {
                    opts.session_kill = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--freeze" => {
                if i + 1 < args.len() {
                    // Context-dependent: session or service
                    opts.session_freeze = Some(args[i + 1].clone());
                    opts.service_freeze = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--unfreeze" => {
                if i + 1 < args.len() {
                    opts.session_unfreeze = Some(args[i + 1].clone());
                    opts.service_unfreeze = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--boost" => {
                if i + 1 < args.len() {
                    opts.session_boost = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--unboost" => {
                if i + 1 < args.len() {
                    opts.session_unboost = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--tmux" => {
                opts.session_tmux = true;
                i += 1;
            }
            "--screen" => {
                opts.session_screen = true;
                i += 1;
            }
            "--snapshot" => {
                if i + 1 < args.len() {
                    opts.session_snapshot = Some(args[i + 1].clone());
                    opts.service_snapshot = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--snapshot-name" => {
                if i + 1 < args.len() {
                    opts.snapshot_name = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--hot" => {
                opts.snapshot_hot = true;
                i += 1;
            }
            "--audit" => {
                opts.audit = true;
                i += 1;
            }
            "--name" => {
                if i + 1 < args.len() {
                    opts.service_name = Some(args[i + 1].clone());
                    opts.clone_name = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--ports" => {
                if i + 1 < args.len() {
                    opts.service_ports = Some(args[i + 1].clone());
                    opts.clone_ports = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--domains" => {
                if i + 1 < args.len() {
                    opts.service_domains = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--type" => {
                if i + 1 < args.len() {
                    opts.service_type = Some(args[i + 1].clone());
                    opts.clone_type = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--bootstrap" => {
                if i + 1 < args.len() {
                    opts.service_bootstrap = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--bootstrap-file" => {
                if i + 1 < args.len() {
                    opts.service_bootstrap_file = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--env-file" => {
                if i + 1 < args.len() {
                    opts.service_env_file = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--info" => {
                if i + 1 < args.len() {
                    opts.service_info = Some(args[i + 1].clone());
                    opts.snapshot_info = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--logs" => {
                if i + 1 < args.len() {
                    opts.service_logs = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--tail" => {
                if i + 1 < args.len() {
                    opts.service_tail = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--destroy" => {
                if i + 1 < args.len() {
                    opts.service_destroy = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--lock" => {
                if i + 1 < args.len() {
                    opts.service_lock = Some(args[i + 1].clone());
                    opts.snapshot_lock = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--unlock" => {
                if i + 1 < args.len() {
                    opts.service_unlock = Some(args[i + 1].clone());
                    opts.snapshot_unlock = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--resize" => {
                if i + 1 < args.len() {
                    opts.service_resize = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--redeploy" => {
                if i + 1 < args.len() {
                    opts.service_redeploy = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--execute" => {
                if i + 1 < args.len() {
                    opts.service_execute = Some(args[i + 1].clone());
                    if i + 2 < args.len() && !args[i + 2].starts_with('-') {
                        opts.service_execute_cmd = Some(args[i + 2].clone());
                        i += 3;
                    } else {
                        i += 2;
                    }
                } else {
                    i += 1;
                }
            }
            "--delete" => {
                if i + 1 < args.len() {
                    opts.snapshot_delete = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            "--clone" => {
                if i + 1 < args.len() {
                    opts.snapshot_clone = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    i += 1;
                }
            }
            _ => {
                // Positional argument or subcommand
                if opts.command.is_none() && !arg.starts_with('-') {
                    match arg.as_str() {
                        "session" | "service" | "snapshot" | "key" | "env" => {
                            if opts.command.is_some() {
                                opts.subcommand = Some(arg.clone());
                            } else {
                                opts.command = Some(arg.clone());
                            }
                        }
                        _ => {
                            opts.positional.push(arg.clone());
                        }
                    }
                } else if !arg.starts_with('-') {
                    opts.positional.push(arg.clone());
                }
                i += 1;
            }
        }
    }

    opts
}

fn get_credentials(opts: &CliOptions) -> Result<Credentials> {
    resolve_credentials(
        opts.public_key.as_deref(),
        opts.secret_key.as_deref(),
    )
}

fn format_session_list(sessions: &[Session]) {
    if sessions.is_empty() {
        println!("No active sessions.");
        return;
    }
    println!("{:<40} {:<20} {:<10} {}", "ID", "NAME", "STATUS", "CREATED");
    for s in sessions {
        println!(
            "{:<40} {:<20} {:<10} {}",
            s.session_id, s.container_name, s.status, s.created_at
        );
    }
}

fn format_service_list(services: &[Service]) {
    if services.is_empty() {
        println!("No active services.");
        return;
    }
    println!("{:<40} {:<20} {:<10} {}", "ID", "NAME", "STATUS", "URL");
    for s in services {
        println!(
            "{:<40} {:<20} {:<10} {}",
            s.service_id, s.name, s.status, s.url
        );
    }
}

fn format_snapshot_list(snapshots: &[Snapshot]) {
    if snapshots.is_empty() {
        println!("No snapshots.");
        return;
    }
    println!("{:<40} {:<20} {:<10} {}", "ID", "NAME", "TYPE", "CREATED");
    for s in snapshots {
        println!(
            "{:<40} {:<20} {:<10} {}",
            s.snapshot_id, s.name, s.source_type, s.created_at
        );
    }
}

async fn cmd_execute(opts: &CliOptions) -> i32 {
    let creds = match get_credentials(opts) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return EXIT_AUTH_ERROR;
        }
    };

    // Determine language and code
    let (language, code) = if let Some(ref shell) = opts.shell {
        // Inline code: -s LANG 'code'
        let code = opts.positional.first().cloned().unwrap_or_default();
        (shell.clone(), code)
    } else if let Some(ref file) = opts.positional.first() {
        // File execution
        let lang = match detect_language(file) {
            Some(l) => l.to_string(),
            None => {
                eprintln!("Error: Cannot detect language from file: {}", file);
                return EXIT_INVALID_ARGS;
            }
        };
        let code = match fs::read_to_string(file).await {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Error: Cannot read file '{}': {}", file, e);
                return EXIT_ERROR;
            }
        };
        (lang, code)
    } else {
        eprintln!("Error: No source file or inline code provided");
        print_help();
        return EXIT_INVALID_ARGS;
    };

    // Execute the code
    match execute_code(&language, &code, &creds).await {
        Ok(result) => {
            print!("{}", result.output);
            println!("---");
            println!("Exit code: {}", result.exit_code);
            println!("Execution time: {}ms", result.execution_time_ms);
            if result.exit_code != 0 {
                EXIT_ERROR
            } else {
                EXIT_SUCCESS
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            match e {
                UnsandboxError::Timeout(_) => EXIT_TIMEOUT,
                UnsandboxError::ApiError { .. } => EXIT_API_ERROR,
                UnsandboxError::NoCredentials => EXIT_AUTH_ERROR,
                _ => EXIT_ERROR,
            }
        }
    }
}

async fn cmd_session(opts: &CliOptions) -> i32 {
    let creds = match get_credentials(opts) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return EXIT_AUTH_ERROR;
        }
    };

    // Handle session subcommands
    if opts.session_list {
        match list_sessions(&creds).await {
            Ok(sessions) => {
                format_session_list(&sessions);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_attach {
        match get_session(id, &creds).await {
            Ok(session) => {
                println!("Session: {}", session.session_id);
                println!("Container: {}", session.container_name);
                println!("Status: {}", session.status);
                println!("Shell: {}", session.shell);
                println!("\nNote: Interactive attach requires terminal support not available in this SDK.");
                println!("Use 'un session --attach {}' with the C CLI for full functionality.", id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_kill {
        match delete_session(id, &creds).await {
            Ok(()) => {
                println!("Session {} terminated.", id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_freeze {
        match freeze_session(id, &creds).await {
            Ok(session) => {
                println!("Session {} frozen.", session.session_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_unfreeze {
        match unfreeze_session(id, &creds).await {
            Ok(session) => {
                println!("Session {} unfrozen.", session.session_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_boost {
        match boost_session(id, &creds).await {
            Ok(session) => {
                println!("Session {} boosted.", session.session_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_unboost {
        match unboost_session(id, &creds).await {
            Ok(session) => {
                println!("Session {} unboosted.", session.session_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.session_snapshot {
        match session_snapshot(id, &creds, opts.snapshot_name.as_deref(), opts.snapshot_hot).await {
            Ok(snapshot) => {
                println!("Snapshot created: {}", snapshot.snapshot_id);
                if !snapshot.name.is_empty() {
                    println!("Name: {}", snapshot.name);
                }
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    // Create new session
    let shell = opts.shell.clone().unwrap_or_else(|| "bash".to_string());
    let session_opts = SessionCreateOptions {
        network_mode: opts.network.clone(),
        shell: Some(shell.clone()),
        vcpu: opts.vcpu,
        tmux: if opts.session_tmux { Some(true) } else { None },
        screen: if opts.session_screen { Some(true) } else { None },
    };

    match create_session(&shell, &creds, Some(session_opts)).await {
        Ok(session) => {
            println!("Session created: {}", session.session_id);
            println!("Container: {}", session.container_name);
            println!("Status: {}", session.status);
            println!("\nNote: Interactive session requires terminal support not available in this SDK.");
            println!("Use the C CLI for interactive sessions.");
            EXIT_SUCCESS
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            EXIT_API_ERROR
        }
    }
}

async fn cmd_service(opts: &CliOptions) -> i32 {
    let creds = match get_credentials(opts) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return EXIT_AUTH_ERROR;
        }
    };

    // Handle service env subcommand
    if opts.command.as_deref() == Some("service") {
        if let Some(ref subcmd) = opts.subcommand {
            if subcmd == "env" {
                return cmd_service_env(opts, &creds).await;
            }
        }
        // Check if first positional is "env"
        if opts.positional.first().map(|s| s.as_str()) == Some("env") {
            return cmd_service_env(opts, &creds).await;
        }
    }

    // Handle service subcommands
    if opts.service_list {
        match list_services(&creds).await {
            Ok(services) => {
                format_service_list(&services);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_info {
        match get_service(id, &creds).await {
            Ok(service) => {
                println!("Service ID: {}", service.service_id);
                println!("Name: {}", service.name);
                println!("Status: {}", service.status);
                println!("URL: {}", service.url);
                println!("Ports: {:?}", service.ports);
                println!("Domains: {:?}", service.domains);
                println!("vCPU: {}", service.vcpu);
                println!("Memory: {} MB", service.memory_mb);
                println!("Locked: {}", service.locked);
                println!("Created: {}", service.created_at);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_logs {
        match get_service_logs(id, true, &creds).await {
            Ok(logs) => {
                print!("{}", logs);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_tail {
        match get_service_logs(id, false, &creds).await {
            Ok(logs) => {
                print!("{}", logs);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_freeze {
        match freeze_service(id, &creds).await {
            Ok(service) => {
                println!("Service {} frozen.", service.service_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_unfreeze {
        match unfreeze_service(id, &creds).await {
            Ok(service) => {
                println!("Service {} unfrozen.", service.service_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_destroy {
        match delete_service(id, &creds).await {
            Ok(()) => {
                println!("Service {} destroyed.", id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_lock {
        match lock_service(id, &creds).await {
            Ok(service) => {
                println!("Service {} locked.", service.service_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_unlock {
        match unlock_service(id, &creds).await {
            Ok(service) => {
                println!("Service {} unlocked.", service.service_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_redeploy {
        match redeploy_service(id, &creds).await {
            Ok(service) => {
                println!("Service {} redeployed.", service.service_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_execute {
        let cmd = opts.service_execute_cmd.clone().unwrap_or_default();
        if cmd.is_empty() {
            eprintln!("Error: --execute requires a command");
            return EXIT_INVALID_ARGS;
        }
        match execute_in_service(id, &cmd, &creds).await {
            Ok(result) => {
                print!("{}", result.output);
                return if result.exit_code == 0 { EXIT_SUCCESS } else { EXIT_ERROR };
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.service_snapshot {
        match service_snapshot(id, &creds, opts.snapshot_name.as_deref()).await {
            Ok(snapshot) => {
                println!("Snapshot created: {}", snapshot.snapshot_id);
                if !snapshot.name.is_empty() {
                    println!("Name: {}", snapshot.name);
                }
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    // Create new service
    if let Some(ref name) = opts.service_name {
        let ports_str = opts.service_ports.clone().unwrap_or_default();
        let ports: Vec<u16> = ports_str
            .split(',')
            .filter_map(|s| s.trim().parse().ok())
            .collect();

        if ports.is_empty() {
            eprintln!("Error: --ports is required for service creation");
            return EXIT_INVALID_ARGS;
        }

        let bootstrap = if let Some(ref cmd) = opts.service_bootstrap {
            cmd.clone()
        } else if let Some(ref file) = opts.service_bootstrap_file {
            match fs::read_to_string(file).await {
                Ok(content) => content,
                Err(e) => {
                    eprintln!("Error: Cannot read bootstrap file '{}': {}", file, e);
                    return EXIT_ERROR;
                }
            }
        } else {
            String::new()
        };

        let service_opts = ServiceCreateOptions {
            network_mode: opts.network.clone(),
            vcpu: opts.vcpu,
            domains: opts.service_domains.as_ref().map(|d| {
                d.split(',').map(|s| s.trim().to_string()).collect()
            }),
            ..Default::default()
        };

        match create_service(name, &ports, &bootstrap, &creds, Some(service_opts)).await {
            Ok(service) => {
                println!("Service created: {}", service.service_id);
                println!("Name: {}", service.name);
                println!("URL: {}", service.url);
                println!("Status: {}", service.status);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    // No action specified, show list
    match list_services(&creds).await {
        Ok(services) => {
            format_service_list(&services);
            EXIT_SUCCESS
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            EXIT_API_ERROR
        }
    }
}

async fn cmd_service_env(opts: &CliOptions, creds: &Credentials) -> i32 {
    // Parse: un service env <action> <service_id>
    // positional[0] = "env", positional[1] = action, positional[2] = service_id
    let action = opts.positional.get(1).map(|s| s.as_str()).unwrap_or("");
    let service_id = opts.positional.get(2).cloned().unwrap_or_default();

    if service_id.is_empty() && action != "status" {
        eprintln!("Error: Service ID required");
        return EXIT_INVALID_ARGS;
    }

    match action {
        "status" => {
            if service_id.is_empty() {
                eprintln!("Error: Service ID required");
                return EXIT_INVALID_ARGS;
            }
            match get_service_env(&service_id, creds).await {
                Ok(env) => {
                    if env.is_empty() {
                        println!("No environment variables set.");
                    } else {
                        println!("Environment variables ({} total):", env.len());
                        for (k, _) in &env {
                            println!("  {}", k);
                        }
                    }
                    EXIT_SUCCESS
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    EXIT_API_ERROR
                }
            }
        }
        "set" => {
            // Read from --env-file or stdin
            let content = if let Some(ref file) = opts.service_env_file {
                match fs::read_to_string(file).await {
                    Ok(c) => c,
                    Err(e) => {
                        eprintln!("Error: Cannot read env file '{}': {}", file, e);
                        return EXIT_ERROR;
                    }
                }
            } else {
                eprintln!("Error: --env-file required for 'set' command");
                return EXIT_INVALID_ARGS;
            };

            let mut env = HashMap::new();
            for line in content.lines() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }
                if let Some(pos) = line.find('=') {
                    let key = line[..pos].to_string();
                    let val = line[pos + 1..].to_string();
                    env.insert(key, val);
                }
            }

            match set_service_env(&service_id, &env, creds).await {
                Ok(()) => {
                    println!("Environment variables set ({} variables).", env.len());
                    EXIT_SUCCESS
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    EXIT_API_ERROR
                }
            }
        }
        "export" => {
            match export_service_env(&service_id, creds).await {
                Ok(content) => {
                    print!("{}", content);
                    EXIT_SUCCESS
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    EXIT_API_ERROR
                }
            }
        }
        "delete" => {
            // Delete all env vars
            match get_service_env(&service_id, creds).await {
                Ok(env) => {
                    let keys: Vec<&str> = env.keys().map(|s| s.as_str()).collect();
                    if keys.is_empty() {
                        println!("No environment variables to delete.");
                        return EXIT_SUCCESS;
                    }
                    match delete_service_env(&service_id, &keys, creds).await {
                        Ok(()) => {
                            println!("Environment vault deleted.");
                            EXIT_SUCCESS
                        }
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            EXIT_API_ERROR
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    EXIT_API_ERROR
                }
            }
        }
        _ => {
            eprintln!("Error: Unknown env command '{}'. Use: status, set, export, delete", action);
            EXIT_INVALID_ARGS
        }
    }
}

async fn cmd_snapshot(opts: &CliOptions) -> i32 {
    let creds = match get_credentials(opts) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return EXIT_AUTH_ERROR;
        }
    };

    if opts.snapshot_list {
        match list_snapshots(&creds).await {
            Ok(snapshots) => {
                format_snapshot_list(&snapshots);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.snapshot_info {
        // Get snapshot details - reuse list and filter
        match list_snapshots(&creds).await {
            Ok(snapshots) => {
                if let Some(snapshot) = snapshots.iter().find(|s| s.snapshot_id == *id) {
                    println!("Snapshot ID: {}", snapshot.snapshot_id);
                    println!("Name: {}", snapshot.name);
                    println!("Source Type: {}", snapshot.source_type);
                    println!("Source ID: {}", snapshot.source_id);
                    println!("Hot: {}", snapshot.hot);
                    println!("Size: {} bytes", snapshot.size_bytes);
                    println!("Created: {}", snapshot.created_at);
                    return EXIT_SUCCESS;
                } else {
                    eprintln!("Error: Snapshot not found: {}", id);
                    return EXIT_API_ERROR;
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.snapshot_delete {
        match delete_snapshot(id, &creds).await {
            Ok(()) => {
                println!("Snapshot {} deleted.", id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.snapshot_lock {
        match lock_snapshot(id, &creds).await {
            Ok(snapshot) => {
                println!("Snapshot {} locked.", snapshot.snapshot_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.snapshot_unlock {
        match unlock_snapshot(id, &creds).await {
            Ok(snapshot) => {
                println!("Snapshot {} unlocked.", snapshot.snapshot_id);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    if let Some(ref id) = opts.snapshot_clone {
        let name = opts.clone_name.clone().unwrap_or_else(|| format!("{}-clone", id));
        match clone_snapshot(id, &name, &creds).await {
            Ok(snapshot) => {
                println!("Snapshot cloned: {}", snapshot.snapshot_id);
                println!("Name: {}", snapshot.name);
                return EXIT_SUCCESS;
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                return EXIT_API_ERROR;
            }
        }
    }

    // Default: list snapshots
    match list_snapshots(&creds).await {
        Ok(snapshots) => {
            format_snapshot_list(&snapshots);
            EXIT_SUCCESS
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            EXIT_API_ERROR
        }
    }
}

async fn cmd_key(opts: &CliOptions) -> i32 {
    let creds = match get_credentials(opts) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {}", e);
            return EXIT_AUTH_ERROR;
        }
    };

    match validate_keys(&creds).await {
        Ok(result) => {
            if result.valid {
                println!("API keys valid.");
                println!("Account ID: {}", result.account_id);
                if !result.email.is_empty() {
                    println!("Email: {}", result.email);
                }
                if !result.plan.is_empty() {
                    println!("Plan: {}", result.plan);
                }
                EXIT_SUCCESS
            } else {
                eprintln!("Error: Invalid API keys");
                if !result.error.is_empty() {
                    eprintln!("Details: {}", result.error);
                }
                EXIT_AUTH_ERROR
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            EXIT_API_ERROR
        }
    }
}

async fn cli_main_async() -> i32 {
    let args: Vec<String> = env::args().collect();
    let opts = parse_args(&args);

    if opts.help {
        print_help();
        return EXIT_SUCCESS;
    }

    // Dispatch based on command
    match opts.command.as_deref() {
        Some("session") => cmd_session(&opts).await,
        Some("service") => cmd_service(&opts).await,
        Some("snapshot") => cmd_snapshot(&opts).await,
        Some("key") => cmd_key(&opts).await,
        None => {
            // Default: execute code
            if opts.positional.is_empty() && opts.shell.is_none() {
                print_help();
                EXIT_SUCCESS
            } else {
                cmd_execute(&opts).await
            }
        }
        Some(cmd) => {
            // Treat unknown command as file to execute
            let mut new_opts = opts;
            new_opts.positional.insert(0, cmd.to_string());
            new_opts.command = None;
            cmd_execute(&new_opts).await
        }
    }
}

/// CLI entry point. Call this from main() to run the CLI.
///
/// This function creates a tokio runtime and runs the async CLI.
///
/// # Examples
/// ```ignore
/// fn main() {
///     std::process::exit(un_async::cli_main());
/// }
/// ```
pub fn cli_main() -> i32 {
    let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
    rt.block_on(cli_main_async())
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

    #[test]
    fn test_parse_args_help() {
        let args = vec!["un".to_string(), "--help".to_string()];
        let opts = parse_args(&args);
        assert!(opts.help);
    }

    #[test]
    fn test_parse_args_execute() {
        let args = vec!["un".to_string(), "script.py".to_string()];
        let opts = parse_args(&args);
        assert_eq!(opts.positional, vec!["script.py"]);
    }

    #[test]
    fn test_parse_args_inline() {
        let args = vec![
            "un".to_string(),
            "-s".to_string(),
            "python".to_string(),
            "print(1)".to_string(),
        ];
        let opts = parse_args(&args);
        assert_eq!(opts.shell, Some("python".to_string()));
        assert_eq!(opts.positional, vec!["print(1)"]);
    }

    #[test]
    fn test_parse_args_session() {
        let args = vec!["un".to_string(), "session".to_string(), "--list".to_string()];
        let opts = parse_args(&args);
        assert_eq!(opts.command, Some("session".to_string()));
        assert!(opts.session_list);
    }

    #[test]
    fn test_parse_args_service() {
        let args = vec![
            "un".to_string(),
            "service".to_string(),
            "--name".to_string(),
            "myapp".to_string(),
            "--ports".to_string(),
            "80".to_string(),
        ];
        let opts = parse_args(&args);
        assert_eq!(opts.command, Some("service".to_string()));
        assert_eq!(opts.service_name, Some("myapp".to_string()));
        assert_eq!(opts.service_ports, Some("80".to_string()));
    }

    #[tokio::test]
    async fn test_async_functions_compile() {
        // This test just verifies the async functions compile correctly
        // Actual API tests would require credentials
        let creds = Credentials::new("unsb-pk-test", "unsb-sk-test");
        assert_eq!(creds.public_key, "unsb-pk-test");
    }
}
