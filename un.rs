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
// unsandbox SDK for Rust - Execute code in secure sandboxes
// https://unsandbox.com | https://api.unsandbox.com/openapi
//
// Library Usage:
//   use un::{execute, execute_async, wait, get_job, cancel_job, list_jobs};
//   let result = execute("rust", "println!(\"Hello\")", Default::default()).unwrap();
//   let job = execute_async("rust", code, Default::default()).unwrap();
//   let result = wait(&job.job_id, Default::default()).unwrap();
//
// CLI Usage:
//   un script.rs
//   un -s rust 'println!("Hello")'

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};
use std::io::Write;

pub const API_BASE: &str = "https://api.unsandbox.com";
pub const PORTAL_BASE: &str = "https://unsandbox.com";
pub const DEFAULT_TIMEOUT: u64 = 300;
pub const DEFAULT_TTL: u64 = 60;
pub const POLL_DELAYS: &[u64] = &[300, 450, 700, 900, 650, 1600, 2000];

const BLUE: &str = "\x1b[34m";
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const RESET: &str = "\x1b[0m";

// ============================================================================
// Exceptions / Error Types
// ============================================================================

#[derive(Debug)]
pub enum UnError {
    AuthenticationError(String),
    ExecutionError { message: String, exit_code: Option<i32>, stderr: Option<String> },
    APIError { message: String, status_code: Option<i32>, response: Option<String> },
    TimeoutError(String),
}

impl std::fmt::Display for UnError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnError::AuthenticationError(msg) => write!(f, "AuthenticationError: {}", msg),
            UnError::ExecutionError { message, .. } => write!(f, "ExecutionError: {}", message),
            UnError::APIError { message, .. } => write!(f, "APIError: {}", message),
            UnError::TimeoutError(msg) => write!(f, "TimeoutError: {}", msg),
        }
    }
}

pub type Result<T> = std::result::Result<T, UnError>;

// ============================================================================
// Credential System (4-tier)
// ============================================================================

fn load_accounts_csv(path: &Path) -> Option<Vec<(String, String)>> {
    if !path.exists() {
        return None;
    }

    let content = fs::read_to_string(path).ok()?;
    let mut accounts = Vec::new();

    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some((pk, sk)) = line.split_once(',') {
            let pk = pk.trim().to_string();
            let sk = sk.trim().to_string();
            if pk.starts_with("unsb-pk-") && sk.starts_with("unsb-sk-") {
                accounts.push((pk, sk));
            }
        }
    }

    if accounts.is_empty() { None } else { Some(accounts) }
}

pub fn get_credentials(
    public_key: Option<&str>,
    secret_key: Option<&str>,
    account_index: usize,
) -> Result<(String, String)> {
    // Tier 1: Function arguments
    if let (Some(pk), Some(sk)) = (public_key, secret_key) {
        return Ok((pk.to_string(), sk.to_string()));
    }

    // Tier 2: Environment variables
    if let (Ok(pk), Ok(sk)) = (env::var("UNSANDBOX_PUBLIC_KEY"), env::var("UNSANDBOX_SECRET_KEY")) {
        return Ok((pk, sk));
    }

    // Tier 3: ~/.unsandbox/accounts.csv
    if let Some(home) = dirs::home_dir() {
        let accounts_path = home.join(".unsandbox").join("accounts.csv");
        if let Some(accounts) = load_accounts_csv(&accounts_path) {
            if account_index < accounts.len() {
                return Ok(accounts[account_index].clone());
            }
        }
    }

    // Tier 4: ./accounts.csv (local directory)
    let local_accounts_path = PathBuf::from("accounts.csv");
    if let Some(accounts) = load_accounts_csv(&local_accounts_path) {
        if account_index < accounts.len() {
            return Ok(accounts[account_index].clone());
        }
    }

    Err(UnError::AuthenticationError(
        "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, \
         or create ~/.unsandbox/accounts.csv or ./accounts.csv, or pass credentials to function."
            .to_string(),
    ))
}

// ============================================================================
// HMAC-SHA256 Signature
// ============================================================================

fn sign_request(
    secret_key: &str,
    timestamp: &str,
    method: &str,
    path: &str,
    body: &str,
) -> Result<String> {
    let message = format!("{}:{}:{}:{}", timestamp, method, path, body);

    // Use openssl for HMAC-SHA256
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!(
            "printf '%s' '{}' | openssl dgst -sha256 -hmac '{}' | cut -d' ' -f2",
            message, secret_key
        ))
        .output()
        .map_err(|e| UnError::APIError {
            message: format!("Failed to compute HMAC: {}", e),
            status_code: None,
            response: None,
        })?;

    let sig = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(sig)
}

// ============================================================================
// JSON Helper Functions
// ============================================================================

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn extract_json_string(json: &str, key: &str) -> String {
    let search = format!("\"{}\":\"", key);
    if let Some(start) = json.find(&search) {
        let start = start + search.len();
        let chars: Vec<char> = json.chars().collect();
        let mut end = start;
        while end < chars.len() {
            if chars[end] == '"' && (end == 0 || chars[end - 1] != '\\') {
                break;
            }
            end += 1;
        }
        return json[start..end].to_string();
    }
    String::new()
}

fn extract_json_int(json: &str, key: &str) -> i32 {
    let search = format!("\"{}\":", key);
    if let Some(pos) = json.find(&search) {
        let start = pos + search.len();
        let rest = &json[start..];
        let num_str: String = rest.chars().take_while(|c| c.is_numeric()).collect();
        return num_str.parse().unwrap_or(0);
    }
    0
}

// ============================================================================
// HTTP Client
// ============================================================================

fn api_request(
    endpoint: &str,
    method: &str,
    body: Option<&str>,
    public_key: &str,
    secret_key: &str,
) -> Result<String> {
    let url = format!("{}{}", API_BASE, endpoint);
    let body_str = body.unwrap_or("");

    // Compute HMAC signature
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
        .to_string();
    let signature = sign_request(secret_key, &timestamp, method, endpoint, body_str)?;

    let mut cmd = Command::new("curl");
    cmd.arg("-s")
        .arg("-X")
        .arg(method)
        .arg(&url)
        .arg("-H")
        .arg("Content-Type: application/json")
        .arg("-H")
        .arg(format!("Authorization: Bearer {}", public_key))
        .arg("-H")
        .arg(format!("X-Timestamp: {}", timestamp))
        .arg("-H")
        .arg(format!("X-Signature: {}", signature));

    if let Some(b) = body {
        cmd.arg("-d").arg(b);
    }

    let output = cmd.output().map_err(|e| UnError::APIError {
        message: format!("Failed to run curl: {}", e),
        status_code: None,
        response: None,
    })?;

    let result = String::from_utf8_lossy(&output.stdout).to_string();

    if result.contains("timestamp")
        && (result.contains("401") || result.contains("expired") || result.contains("invalid"))
    {
        return Err(UnError::AuthenticationError(
            "Request timestamp expired. Your system clock may be out of sync.".to_string(),
        ));
    }

    Ok(result)
}

// ============================================================================
// Core Library Functions (public API)
// ============================================================================

#[derive(Debug, Clone, Default)]
pub struct ExecuteOptions {
    pub env: Option<HashMap<String, String>>,
    pub input_files: Option<Vec<InputFile>>,
    pub network_mode: Option<String>,
    pub ttl: Option<u64>,
    pub vcpu: Option<i32>,
    pub return_artifact: Option<bool>,
    pub return_wasm_artifact: Option<bool>,
    pub public_key: Option<String>,
    pub secret_key: Option<String>,
}

#[derive(Debug, Clone)]
pub struct InputFile {
    pub filename: String,
    pub content_base64: String,
}

#[derive(Debug)]
pub struct ExecutionResult {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
    pub job_id: String,
}

#[derive(Debug)]
pub struct JobResult {
    pub job_id: String,
    pub status: String,
}

pub fn execute(
    language: &str,
    code: &str,
    opts: ExecuteOptions,
) -> Result<ExecutionResult> {
    let (pk, sk) = get_credentials(opts.public_key.as_deref(), opts.secret_key.as_deref(), 0)?;

    let network_mode = opts.network_mode.unwrap_or_else(|| "zerotrust".to_string());
    let ttl = opts.ttl.unwrap_or(DEFAULT_TTL);
    let vcpu = opts.vcpu.unwrap_or(1);

    let mut json = format!(
        r#"{{"language":"{}","code":"{}","network_mode":"{}","ttl":{},"vcpu":{}"#,
        language,
        escape_json(code),
        network_mode,
        ttl,
        vcpu
    );

    if let Some(env) = &opts.env {
        json.push_str(r#","env":{"#);
        let mut first = true;
        for (k, v) in env {
            if !first {
                json.push(',');
            }
            json.push_str(&format!(r#""{}":"{}""#, k, escape_json(v)));
            first = false;
        }
        json.push('}');
    }

    if let Some(files) = &opts.input_files {
        json.push_str(r#","input_files":["#);
        for (i, f) in files.iter().enumerate() {
            if i > 0 {
                json.push(',');
            }
            json.push_str(&format!(
                r#"{{"filename":"{}","content_base64":"{}"}}"#,
                f.filename, f.content_base64
            ));
        }
        json.push(']');
    }

    if opts.return_artifact.unwrap_or(false) {
        json.push_str(r#","return_artifact":true"#);
    }
    if opts.return_wasm_artifact.unwrap_or(false) {
        json.push_str(r#","return_wasm_artifact":true"#);
    }

    json.push('}');

    let result = api_request("/execute", "POST", Some(&json), &pk, &sk)?;

    let stdout = extract_json_string(&result, "stdout");
    let stderr = extract_json_string(&result, "stderr");
    let exit_code = extract_json_int(&result, "exit_code");
    let job_id = extract_json_string(&result, "job_id");

    Ok(ExecutionResult {
        success: exit_code == 0,
        stdout,
        stderr,
        exit_code,
        job_id,
    })
}

pub fn execute_async(
    language: &str,
    code: &str,
    opts: ExecuteOptions,
) -> Result<JobResult> {
    let (pk, sk) = get_credentials(opts.public_key.as_deref(), opts.secret_key.as_deref(), 0)?;

    let network_mode = opts.network_mode.unwrap_or_else(|| "zerotrust".to_string());
    let ttl = opts.ttl.unwrap_or(DEFAULT_TTL);
    let vcpu = opts.vcpu.unwrap_or(1);

    let mut json = format!(
        r#"{{"language":"{}","code":"{}","network_mode":"{}","ttl":{},"vcpu":{}"#,
        language,
        escape_json(code),
        network_mode,
        ttl,
        vcpu
    );

    if let Some(env) = &opts.env {
        json.push_str(r#","env":{"#);
        let mut first = true;
        for (k, v) in env {
            if !first {
                json.push(',');
            }
            json.push_str(&format!(r#""{}":"{}""#, k, escape_json(v)));
            first = false;
        }
        json.push('}');
    }

    if let Some(files) = &opts.input_files {
        json.push_str(r#","input_files":["#);
        for (i, f) in files.iter().enumerate() {
            if i > 0 {
                json.push(',');
            }
            json.push_str(&format!(
                r#"{{"filename":"{}","content_base64":"{}"}}"#,
                f.filename, f.content_base64
            ));
        }
        json.push(']');
    }

    json.push('}');

    let result = api_request("/execute/async", "POST", Some(&json), &pk, &sk)?;
    let job_id = extract_json_string(&result, "job_id");
    let status = extract_json_string(&result, "status");

    Ok(JobResult { job_id, status })
}

#[derive(Debug)]
pub struct JobStatus {
    pub job_id: String,
    pub status: String,
    pub result: Option<ExecutionResult>,
}

pub fn get_job(job_id: &str, public_key: Option<&str>, secret_key: Option<&str>) -> Result<JobStatus> {
    let (pk, sk) = get_credentials(public_key, secret_key, 0)?;
    let result = api_request(&format!("/jobs/{}", job_id), "GET", None, &pk, &sk)?;

    let status = extract_json_string(&result, "status");
    let job_id_ret = extract_json_string(&result, "job_id");
    let stdout = extract_json_string(&result, "stdout");
    let stderr = extract_json_string(&result, "stderr");
    let exit_code = extract_json_int(&result, "exit_code");

    let result_opt = if status == "completed" || status == "failed" {
        Some(ExecutionResult {
            success: exit_code == 0,
            stdout,
            stderr,
            exit_code,
            job_id: job_id_ret.clone(),
        })
    } else {
        None
    };

    Ok(JobStatus {
        job_id: job_id_ret,
        status,
        result: result_opt,
    })
}

#[derive(Debug, Clone, Default)]
pub struct WaitOptions {
    pub max_polls: Option<usize>,
    pub public_key: Option<String>,
    pub secret_key: Option<String>,
}

pub fn wait(job_id: &str, opts: WaitOptions) -> Result<ExecutionResult> {
    let max_polls = opts.max_polls.unwrap_or(100);
    let terminal_states = ["completed", "failed", "timeout", "cancelled"];

    for i in 0..max_polls {
        let delay_idx = std::cmp::min(i, POLL_DELAYS.len() - 1);
        std::thread::sleep(std::time::Duration::from_millis(POLL_DELAYS[delay_idx]));

        let job_status = get_job(job_id, opts.public_key.as_deref(), opts.secret_key.as_deref())?;

        if terminal_states.contains(&job_status.status.as_str()) {
            if job_status.status == "failed" {
                return Err(UnError::ExecutionError {
                    message: "Job failed".to_string(),
                    exit_code: None,
                    stderr: job_status.result.as_ref().map(|r| r.stderr.clone()),
                });
            }
            if job_status.status == "timeout" {
                return Err(UnError::TimeoutError(format!("Job timed out: {}", job_id)));
            }
            if let Some(result) = job_status.result {
                return Ok(result);
            }
        }
    }

    Err(UnError::TimeoutError(format!(
        "Max polls ({}) exceeded for job {}",
        max_polls, job_id
    )))
}

pub fn cancel_job(job_id: &str, public_key: Option<&str>, secret_key: Option<&str>) -> Result<()> {
    let (pk, sk) = get_credentials(public_key, secret_key, 0)?;
    api_request(&format!("/jobs/{}", job_id), "DELETE", None, &pk, &sk)?;
    Ok(())
}

#[derive(Debug)]
pub struct JobSummary {
    pub job_id: String,
    pub language: String,
    pub status: String,
}

pub fn list_jobs(public_key: Option<&str>, secret_key: Option<&str>) -> Result<Vec<JobSummary>> {
    let (pk, sk) = get_credentials(public_key, secret_key, 0)?;
    let result = api_request("/jobs", "GET", None, &pk, &sk)?;

    // Simple parsing of jobs array - would need proper JSON parser for production
    let mut jobs = Vec::new();
    if result.contains("\"job_id\"") {
        jobs.push(JobSummary {
            job_id: extract_json_string(&result, "job_id"),
            language: extract_json_string(&result, "language"),
            status: extract_json_string(&result, "status"),
        });
    }

    Ok(jobs)
}

// ============================================================================
// Languages Cache (1-hour TTL)
// ============================================================================

pub fn languages(
    public_key: Option<&str>,
    secret_key: Option<&str>,
    cache_ttl: Option<u64>,
) -> Result<String> {
    let (pk, sk) = get_credentials(public_key, secret_key, 0)?;
    let cache_ttl = cache_ttl.unwrap_or(3600);

    // Check cache
    if let Some(home) = dirs::home_dir() {
        let cache_path = home.join(".unsandbox").join("languages.json");
        if cache_path.exists() {
            if let Ok(metadata) = fs::metadata(&cache_path) {
                if let Ok(modified) = metadata.modified() {
                    if let Ok(elapsed) = modified.elapsed() {
                        if elapsed.as_secs() < cache_ttl {
                            if let Ok(content) = fs::read_to_string(&cache_path) {
                                return Ok(content);
                            }
                        }
                    }
                }
            }
        }
    }

    // Fetch from API
    let result = api_request("/languages", "GET", None, &pk, &sk)?;

    // Save to cache
    if let Some(home) = dirs::home_dir() {
        let cache_dir = home.join(".unsandbox");
        let cache_path = cache_dir.join("languages.json");
        let _ = fs::create_dir_all(&cache_dir);
        let _ = fs::write(&cache_path, &result);
    }

    Ok(result)
}

// ============================================================================
// Language Detection
// ============================================================================

pub fn detect_language(filename: &str) -> Option<&'static str> {
    let ext = Path::new(filename)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");

    match ext {
        "py" => Some("python"),
        "js" => Some("javascript"),
        "ts" => Some("typescript"),
        "rb" => Some("ruby"),
        "php" => Some("php"),
        "pl" => Some("perl"),
        "lua" => Some("lua"),
        "sh" => Some("bash"),
        "go" => Some("go"),
        "rs" => Some("rust"),
        "c" => Some("c"),
        "cpp" | "cc" | "cxx" => Some("cpp"),
        "java" => Some("java"),
        "kt" => Some("kotlin"),
        "cs" => Some("csharp"),
        "fs" => Some("fsharp"),
        "hs" => Some("haskell"),
        "ml" => Some("ocaml"),
        "clj" => Some("clojure"),
        "scm" => Some("scheme"),
        "lisp" => Some("commonlisp"),
        "erl" => Some("erlang"),
        "ex" | "exs" => Some("elixir"),
        "jl" => Some("julia"),
        "r" | "R" => Some("r"),
        "cr" => Some("crystal"),
        "d" => Some("d"),
        "nim" => Some("nim"),
        "zig" => Some("zig"),
        "v" => Some("v"),
        "dart" => Some("dart"),
        "groovy" => Some("groovy"),
        "scala" => Some("scala"),
        "f90" | "f95" => Some("fortran"),
        "cob" => Some("cobol"),
        "pro" => Some("prolog"),
        "forth" | "4th" => Some("forth"),
        "tcl" => Some("tcl"),
        "raku" => Some("raku"),
        "m" => Some("objc"),
        "awk" => Some("awk"),
        _ => None,
    }
}

// ============================================================================
// CLI Interface
// ============================================================================

fn cmd_execute(
    source_file: &str,
    envs: Vec<String>,
    files: Vec<String>,
    artifacts: bool,
    _output_dir: Option<&str>,
    network: Option<&str>,
    vcpu: Option<i32>,
    public_key: Option<String>,
    secret_key: Option<String>,
) {
    let code = match fs::read_to_string(source_file) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("{}Error reading file: {}{}", RED, e, RESET);
            process::exit(1);
        }
    };

    let language = match detect_language(source_file) {
        Some(l) => l,
        None => {
            eprintln!("{}Error: Cannot detect language{}", RED, RESET);
            process::exit(1);
        }
    };

    let mut opts = ExecuteOptions {
        network_mode: network.map(|s| s.to_string()),
        ttl: Some(60),
        vcpu,
        return_artifact: if artifacts { Some(true) } else { None },
        public_key,
        secret_key,
        ..Default::default()
    };

    // Parse environment variables
    if !envs.is_empty() {
        let mut env_map = HashMap::new();
        for e in envs {
            if let Some((k, v)) = e.split_once('=') {
                env_map.insert(k.to_string(), v.to_string());
            }
        }
        opts.env = Some(env_map);
    }

    // Load input files
    if !files.is_empty() {
        let mut input_files = Vec::new();
        for f in files {
            let content = match fs::read(&f) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("{}Error reading input file: {}{}", RED, e, RESET);
                    process::exit(1);
                }
            };
            let b64 = base64_encode(&content);
            let filename = Path::new(&f)
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default();
            input_files.push(InputFile {
                filename,
                content_base64: b64,
            });
        }
        opts.input_files = Some(input_files);
    }

    match execute(language, &code, opts) {
        Ok(result) => {
            if !result.stdout.is_empty() {
                print!("{}", result.stdout);
            }
            if !result.stderr.is_empty() {
                eprint!("{}{}{}", RED, result.stderr, RESET);
            }
            process::exit(result.exit_code);
        }
        Err(e) => {
            eprintln!("{}Error: {}{}", RED, e, RESET);
            process::exit(1);
        }
    }
}

fn base64_encode(input: &[u8]) -> String {
    const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut result = String::new();
    let mut i = 0;

    while i < input.len() {
        let b1 = input[i];
        let b2 = if i + 1 < input.len() { input[i + 1] } else { 0 };
        let b3 = if i + 2 < input.len() { input[i + 2] } else { 0 };

        result.push(CHARS[(b1 >> 2) as usize] as char);
        result.push(CHARS[(((b1 & 0x03) << 4) | (b2 >> 4)) as usize] as char);
        result.push(if i + 1 < input.len() {
            CHARS[(((b2 & 0x0f) << 2) | (b3 >> 6)) as usize] as char
        } else {
            '='
        });
        result.push(if i + 2 < input.len() {
            CHARS[(b3 & 0x3f) as usize] as char
        } else {
            '='
        });

        i += 3;
    }

    result
}

// Stub for dirs crate
mod dirs {
    use std::path::PathBuf;

    pub fn home_dir() -> Option<PathBuf> {
        std::env::var_os("HOME")
            .and_then(|h| if h.is_empty() { None } else { Some(h) })
            .map(PathBuf::from)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} [options] <source_file>", args[0]);
        eprintln!("       {} session [options]", args[0]);
        eprintln!("       {} service [options]", args[0]);
        eprintln!("       {} key [--extend]", args[0]);
        eprintln!("\nLibrary usage: un.rs exports execute(), execute_async(), wait(), etc.");
        process::exit(1);
    }

    // Parse arguments
    let mut public_key: Option<String> = None;
    let mut secret_key: Option<String> = None;
    let mut network: Option<String> = None;
    let mut vcpu: Option<i32> = None;
    let mut envs: Vec<String> = Vec::new();
    let mut files: Vec<String> = Vec::new();
    let mut artifacts = false;
    let mut output_dir: Option<String> = None;
    let mut source_file: Option<String> = None;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-p" | "--public-key" => {
                i += 1;
                if i < args.len() {
                    public_key = Some(args[i].clone());
                }
            }
            "-k" | "--secret-key" => {
                i += 1;
                if i < args.len() {
                    secret_key = Some(args[i].clone());
                }
            }
            "-n" | "--network" => {
                i += 1;
                if i < args.len() {
                    network = Some(args[i].clone());
                }
            }
            "-v" | "--vcpu" => {
                i += 1;
                if i < args.len() {
                    vcpu = args[i].parse().ok();
                }
            }
            "-e" | "--env" => {
                i += 1;
                if i < args.len() {
                    envs.push(args[i].clone());
                }
            }
            "-f" | "--file" => {
                i += 1;
                if i < args.len() {
                    files.push(args[i].clone());
                }
            }
            "-a" | "--artifacts" => artifacts = true,
            "-o" | "--output" => {
                i += 1;
                if i < args.len() {
                    output_dir = Some(args[i].clone());
                }
            }
            "-s" | "--shell" => {
                i += 1;
                if i < args.len() {
                    let lang = args[i].clone();
                    i += 1;
                    let code = if i < args.len() { args[i].clone() } else { String::new() };

                    let mut opts = ExecuteOptions {
                        public_key: public_key.clone(),
                        secret_key: secret_key.clone(),
                        network_mode: network,
                        ttl: Some(60),
                        vcpu,
                        ..Default::default()
                    };

                    if !envs.is_empty() {
                        let mut env_map = HashMap::new();
                        for e in &envs {
                            if let Some((k, v)) = e.split_once('=') {
                                env_map.insert(k.to_string(), v.to_string());
                            }
                        }
                        opts.env = Some(env_map);
                    }

                    match execute(&lang, &code, opts) {
                        Ok(result) => {
                            if !result.stdout.is_empty() {
                                print!("{}", result.stdout);
                            }
                            if !result.stderr.is_empty() {
                                eprint!("{}{}{}", RED, result.stderr, RESET);
                            }
                            process::exit(result.exit_code);
                        }
                        Err(e) => {
                            eprintln!("{}Error: {}{}", RED, e, RESET);
                            process::exit(1);
                        }
                    }
                }
            }
            _ => {
                if !args[i].starts_with('-') {
                    source_file = Some(args[i].clone());
                }
            }
        }
        i += 1;
    }

    if let Some(file) = source_file {
        cmd_execute(&file, envs, files, artifacts, output_dir.as_deref(), network.as_deref(), vcpu, public_key, secret_key);
    } else {
        eprintln!("{}Error: No source file specified{}", RED, RESET);
        process::exit(1);
    }
}
