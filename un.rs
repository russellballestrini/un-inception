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


// UN CLI - Rust Implementation
// Note: This uses curl subprocess to avoid requiring external crates
// Compile: rustc un.rs -o un_rust
// Usage:
//   un.rs script.py
//   un.rs -e KEY=VALUE -f data.txt script.py
//   un.rs session --list
//   un.rs service --name web --ports 8080

use std::env;
use std::fs;
use std::path::Path;
use std::process::{self, Command};
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

const API_BASE: &str = "https://api.unsandbox.com";
const PORTAL_BASE: &str = "https://unsandbox.com";
const BLUE: &str = "\x1b[34m";
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const YELLOW: &str = "\x1b[33m";
const RESET: &str = "\x1b[0m";

fn detect_language(filename: &str) -> Option<&'static str> {
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
        _ => None,
    }
}

fn get_api_keys(key_arg: Option<&str>) -> (String, String) {
    let public_key = env::var("UNSANDBOX_PUBLIC_KEY").ok();
    let secret_key = env::var("UNSANDBOX_SECRET_KEY").ok();

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if public_key.is_none() || secret_key.is_none() {
        let fallback_key = if let Some(k) = key_arg {
            k.to_string()
        } else {
            env::var("UNSANDBOX_API_KEY").unwrap_or_else(|_| {
                eprintln!("{}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat){}", RED, RESET);
                process::exit(1);
            })
        };
        return (fallback_key.clone(), fallback_key);
    }

    (public_key.unwrap(), secret_key.unwrap())
}

fn compute_hmac(secret_key: &str, timestamp: &str, method: &str, path: &str, body: &str) -> String {
    use std::process::Command;
    let message = format!("{}:{}:{}:{}", timestamp, method, path, body);

    // Use openssl for HMAC-SHA256
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("printf '%s' '{}' | openssl dgst -sha256 -hmac '{}' | cut -d' ' -f2", message, secret_key))
        .output()
        .expect("Failed to compute HMAC");

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

fn unescape_json(s: &str) -> String {
    s.replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\\"", "\"")
        .replace("\\\\", "\\")
}

fn extract_json_string(json: &str, key: &str) -> String {
    let search = format!("\"{}\":\"", key);
    if let Some(start) = json.find(&search) {
        let start = start + search.len();
        let mut end = start;
        let chars: Vec<char> = json.chars().collect();
        while end < chars.len() {
            if chars[end] == '"' && (end == 0 || chars[end - 1] != '\\') {
                break;
            }
            end += 1;
        }
        return unescape_json(&json[start..end]);
    }
    String::new()
}

fn extract_json_int(json: &str, key: &str) -> i32 {
    let search = format!("\"{}\":", key);
    if let Some(pos) = json.find(&search) {
        let start = pos + search.len();
        let rest = &json[start..];
        let num_str: String = rest.chars().take_while(|c| c.is_numeric()).collect();
        return num_str.parse().unwrap_or(1);
    }
    1
}

fn api_request(endpoint: &str, method: &str, body: Option<&str>, public_key: &str, secret_key: &str) -> String {
    let url = format!("{}{}", API_BASE, endpoint);
    let body_str = body.unwrap_or("");

    // Compute HMAC signature
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
        .to_string();
    let signature = compute_hmac(secret_key, &timestamp, method, endpoint, body_str);

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

    let output = cmd.output().unwrap_or_else(|e| {
        eprintln!("{}Error running curl: {}{}", RED, e, RESET);
        process::exit(1);
    });

    if !output.status.success() {
        eprintln!("{}Error: HTTP request failed{}", RED, RESET);
        process::exit(1);
    }

    let result = String::from_utf8_lossy(&output.stdout).to_string();

    // Check for timestamp authentication errors
    if result.contains("timestamp") && (result.contains("401") || result.contains("expired") || result.contains("invalid")) {
        eprintln!("{}Error: Request timestamp expired (must be within 5 minutes of server time){}", RED, RESET);
        eprintln!("{}Your computer's clock may have drifted.{}", YELLOW, RESET);
        eprintln!("Check your system time and sync with NTP if needed:");
        eprintln!("  Linux:   sudo ntpdate -s time.nist.gov");
        eprintln!("  macOS:   sudo sntp -sS time.apple.com");
        eprintln!("  Windows: w32tm /resync");
        process::exit(1);
    }

    result
}

fn cmd_execute(
    source_file: &str,
    envs: Vec<String>,
    files: Vec<String>,
    artifacts: bool,
    output_dir: Option<&str>,
    network: Option<&str>,
    vcpu: Option<i32>,
    public_key: &str,
    secret_key: &str,
) {
    let code = fs::read_to_string(source_file).unwrap_or_else(|e| {
        eprintln!("{}Error reading file: {}{}", RED, e, RESET);
        process::exit(1);
    });

    let language = detect_language(source_file).unwrap_or_else(|| {
        eprintln!("{}Error: Cannot detect language{}", RED, RESET);
        process::exit(1);
    });

    let mut json = format!(
        r#"{{"language":"{}","code":"{}""#,
        language,
        escape_json(&code)
    );

    // Environment variables
    if !envs.is_empty() {
        json.push_str(r#","env":{"#);
        for (i, e) in envs.iter().enumerate() {
            if let Some((k, v)) = e.split_once('=') {
                if i > 0 {
                    json.push(',');
                }
                json.push_str(&format!(r#""{}":"{}""#, k, escape_json(v)));
            }
        }
        json.push('}');
    }

    // Input files
    if !files.is_empty() {
        json.push_str(r#","input_files":["#);
        for (i, f) in files.iter().enumerate() {
            let content = fs::read(f).unwrap_or_else(|e| {
                eprintln!("{}Error reading input file: {}{}", RED, e, RESET);
                process::exit(1);
            });
            let b64 = base64::encode(&content);
            if i > 0 {
                json.push(',');
            }
            json.push_str(&format!(
                r#"{{"filename":"{}","content_base64":"{}"}}"#,
                Path::new(f).file_name().unwrap().to_str().unwrap(),
                b64
            ));
        }
        json.push(']');
    }

    if artifacts {
        json.push_str(r#","return_artifacts":true"#);
    }
    if let Some(n) = network {
        json.push_str(&format!(r#","network":"{}""#, n));
    }
    if let Some(v) = vcpu {
        json.push_str(&format!(r#","vcpu":{}"#, v));
    }

    json.push('}');

    let result = api_request("/execute", "POST", Some(&json), public_key, secret_key);

    // Print output
    let stdout_str = extract_json_string(&result, "stdout");
    let stderr_str = extract_json_string(&result, "stderr");
    let exit_code = extract_json_int(&result, "exit_code");

    if !stdout_str.is_empty() {
        print!("{}{}{}", BLUE, stdout_str, RESET);
    }
    if !stderr_str.is_empty() {
        eprint!("{}{}{}", RED, stderr_str, RESET);
    }

    // Artifacts (simplified - would need full JSON parsing)
    if artifacts && result.contains("artifacts") {
        eprintln!("{}Note: Artifact saving not fully implemented in Rust version{}", YELLOW, RESET);
    }

    process::exit(exit_code);
}

fn cmd_session(
    list: bool,
    kill: Option<&str>,
    shell: Option<&str>,
    network: Option<&str>,
    vcpu: Option<i32>,
    tmux: bool,
    screen: bool,
    files: &[String],
    public_key: &str,
    secret_key: &str,
) {
    if list {
        let result = api_request("/sessions", "GET", None, public_key, secret_key);
        println!("{}", result);
        return;
    }

    if let Some(id) = kill {
        api_request(&format!("/sessions/{}", id), "DELETE", None, public_key, secret_key);
        println!("{}Session terminated: {}{}", GREEN, id, RESET);
        return;
    }

    // Create session
    let mut json = format!(
        r#"{{"shell":"{}""#,
        shell.unwrap_or("bash")
    );

    if let Some(n) = network {
        json.push_str(&format!(r#","network":"{}""#, n));
    }
    if let Some(v) = vcpu {
        json.push_str(&format!(r#","vcpu":{}"#, v));
    }
    if tmux {
        json.push_str(r#","persistence":"tmux""#);
    }
    if screen {
        json.push_str(r#","persistence":"screen""#);
    }

    // Input files
    if !files.is_empty() {
        json.push_str(r#","input_files":["#);
        for (i, f) in files.iter().enumerate() {
            if i > 0 {
                json.push(',');
            }
            let content = fs::read(f).unwrap_or_else(|e| {
                eprintln!("{}Error reading input file {}: {}{}", RED, f, e, RESET);
                process::exit(1);
            });
            let b64 = base64::encode(&content);
            let filename = Path::new(f).file_name().map(|n| n.to_string_lossy()).unwrap_or_default();
            json.push_str(&format!(r#"{{"filename":"{}","content_base64":"{}"}}"#, filename, b64));
        }
        json.push(']');
    }

    json.push('}');

    println!("{}Creating session...{}", YELLOW, RESET);
    let result = api_request("/sessions", "POST", Some(&json), public_key, secret_key);
    let id = extract_json_string(&result, "id");
    println!("{}Session created: {}{}", GREEN, id, RESET);
}

fn cmd_service(
    name: Option<&str>,
    ports: Option<&str>,
    domains: Option<&str>,
    service_type: Option<&str>,
    bootstrap: Option<&str>,
    bootstrap_file: Option<&str>,
    files: &[String],
    list: bool,
    info: Option<&str>,
    logs: Option<&str>,
    tail: Option<&str>,
    sleep: Option<&str>,
    wake: Option<&str>,
    destroy: Option<&str>,
    execute: Option<&str>,
    command: Option<&str>,
    dump_bootstrap: Option<&str>,
    dump_file: Option<&str>,
    network: Option<&str>,
    vcpu: Option<i32>,
    public_key: &str,
    secret_key: &str,
) {
    if list {
        let result = api_request("/services", "GET", None, public_key, secret_key);
        println!("{}", result);
        return;
    }

    if let Some(id) = info {
        let result = api_request(&format!("/services/{}", id), "GET", None, public_key, secret_key);
        println!("{}", result);
        return;
    }

    if let Some(id) = logs {
        let result = api_request(&format!("/services/{}/logs", id), "GET", None, public_key, secret_key);
        println!("{}", extract_json_string(&result, "logs"));
        return;
    }

    if let Some(id) = tail {
        let result = api_request(&format!("/services/{}/logs?lines=9000", id), "GET", None, public_key, secret_key);
        println!("{}", extract_json_string(&result, "logs"));
        return;
    }

    if let Some(id) = sleep {
        api_request(&format!("/services/{}/sleep", id), "POST", None, public_key, secret_key);
        println!("{}Service sleeping: {}{}", GREEN, id, RESET);
        return;
    }

    if let Some(id) = wake {
        api_request(&format!("/services/{}/wake", id), "POST", None, public_key, secret_key);
        println!("{}Service waking: {}{}", GREEN, id, RESET);
        return;
    }

    if let Some(id) = destroy {
        api_request(&format!("/services/{}", id), "DELETE", None, public_key, secret_key);
        println!("{}Service destroyed: {}{}", GREEN, id, RESET);
        return;
    }

    if let Some(id) = execute {
        let cmd = command.unwrap_or("");
        let json = format!(r#"{{"command":"{}"}}"#, escape_json(cmd));
        let result = api_request(&format!("/services/{}/execute", id), "POST", Some(&json), public_key, secret_key);
        let stdout_str = extract_json_string(&result, "stdout");
        let stderr_str = extract_json_string(&result, "stderr");
        if !stdout_str.is_empty() {
            print!("{}{}{}", BLUE, stdout_str, RESET);
        }
        if !stderr_str.is_empty() {
            eprint!("{}{}{}", RED, stderr_str, RESET);
        }
        return;
    }

    if let Some(id) = dump_bootstrap {
        eprintln!("Fetching bootstrap script from {}...", id);
        let json = r#"{"command":"cat /tmp/bootstrap.sh"}"#;
        let result = api_request(&format!("/services/{}/execute", id), "POST", Some(json), public_key, secret_key);
        let bootstrap = extract_json_string(&result, "stdout");

        if !bootstrap.is_empty() {
            if let Some(file) = dump_file {
                match fs::write(file, &bootstrap) {
                    Ok(_) => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            let _ = fs::set_permissions(file, fs::Permissions::from_mode(0o755));
                        }
                        println!("Bootstrap saved to {}", file);
                    }
                    Err(e) => {
                        eprintln!("{}Error: Could not write to {}: {}{}", RED, file, e, RESET);
                        process::exit(1);
                    }
                }
            } else {
                print!("{}", bootstrap);
            }
        } else {
            eprintln!("{}Error: Failed to fetch bootstrap (service not running or no bootstrap file){}", RED, RESET);
            process::exit(1);
        }
        return;
    }

    // Create service
    if let Some(n) = name {
        let mut json = format!(r#"{{"name":"{}""#, n);

        if let Some(p) = ports {
            json.push_str(r#","ports":["#);
            let ports_vec: Vec<&str> = p.split(',').collect();
            for (i, port) in ports_vec.iter().enumerate() {
                if i > 0 {
                    json.push(',');
                }
                json.push_str(port.trim());
            }
            json.push(']');
        }

        if let Some(d) = domains {
            json.push_str(r#","domains":["#);
            let domains_vec: Vec<&str> = d.split(',').collect();
            for (i, domain) in domains_vec.iter().enumerate() {
                if i > 0 {
                    json.push(',');
                }
                json.push_str(&format!(r#""{}""#, domain.trim()));
            }
            json.push(']');
        }

        if let Some(t) = service_type {
            json.push_str(&format!(r#","service_type":"{}""#, t));
        }

        if let Some(b) = bootstrap {
            json.push_str(&format!(r#","bootstrap":"{}""#, escape_json(b)));
        }

        if let Some(bf) = bootstrap_file {
            if Path::new(bf).exists() {
                let content = fs::read_to_string(bf).unwrap_or_else(|e| {
                    eprintln!("{}Error reading bootstrap file: {}{}", RED, e, RESET);
                    process::exit(1);
                });
                json.push_str(&format!(r#","bootstrap_content":"{}""#, escape_json(&content)));
            } else {
                eprintln!("{}Error: Bootstrap file not found: {}{}", RED, bf, RESET);
                process::exit(1);
            }
        }

        // Input files
        if !files.is_empty() {
            json.push_str(r#","input_files":["#);
            for (i, f) in files.iter().enumerate() {
                if i > 0 {
                    json.push(',');
                }
                let content = fs::read(f).unwrap_or_else(|e| {
                    eprintln!("{}Error reading input file {}: {}{}", RED, f, e, RESET);
                    process::exit(1);
                });
                let b64 = base64::encode(&content);
                let filename = Path::new(f).file_name().map(|n| n.to_string_lossy()).unwrap_or_default();
                json.push_str(&format!(r#"{{"filename":"{}","content_base64":"{}"}}"#, filename, b64));
            }
            json.push(']');
        }

        if let Some(net) = network {
            json.push_str(&format!(r#","network":"{}""#, net));
        }
        if let Some(v) = vcpu {
            json.push_str(&format!(r#","vcpu":{}"#, v));
        }

        json.push('}');

        let result = api_request("/services", "POST", Some(&json), public_key, secret_key);
        let id = extract_json_string(&result, "id");
        println!("{}Service created: {}{}", GREEN, id, RESET);
        return;
    }

    eprintln!("{}Error: Specify --name to create a service{}", RED, RESET);
    process::exit(1);
}

fn cmd_key(extend: bool, public_key: &str, secret_key: &str) {
    let result = api_request("/keys/validate", "POST", Some("{}"), public_key, secret_key);

    let status = extract_json_string(&result, "status");
    let public_key = extract_json_string(&result, "public_key");
    let tier = extract_json_string(&result, "tier");
    let expired_at = extract_json_string(&result, "expired_at");

    if extend && !public_key.is_empty() {
        let url = format!("{}/keys/extend?pk={}", PORTAL_BASE, public_key);
        println!("{}Opening browser: {}{}", YELLOW, url, RESET);

        // Try xdg-open (Linux), open (macOS), or start (Windows)
        let _ = Command::new("xdg-open")
            .arg(&url)
            .spawn()
            .or_else(|_| Command::new("open").arg(&url).spawn())
            .or_else(|_| Command::new("cmd").args(&["/c", "start", &url]).spawn());

        return;
    }

    match status.as_str() {
        "valid" => {
            println!("{}Valid{}", GREEN, RESET);
            println!("Public Key: {}", public_key);
            println!("Tier: {}", tier);
            if !expired_at.is_empty() {
                println!("Expires: {}", expired_at);
            }
        }
        "expired" => {
            println!("{}Expired{}", RED, RESET);
            println!("Public Key: {}", public_key);
            println!("Tier: {}", tier);
            if !expired_at.is_empty() {
                println!("Expired: {}", expired_at);
            }
            println!("{}To renew: Visit {}/keys/extend{}", YELLOW, PORTAL_BASE, RESET);
        }
        "invalid" => {
            println!("{}Invalid{}", RED, RESET);
        }
        _ => {
            println!("{}Unknown status: {}{}", YELLOW, status, RESET);
        }
    }
}

// Minimal base64 encoding
mod base64 {
    const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    pub fn encode(input: &[u8]) -> String {
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
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} [options] <source_file>", args[0]);
        eprintln!("       {} session [options]", args[0]);
        eprintln!("       {} service [options]", args[0]);
        eprintln!("       {} key [--extend]", args[0]);
        process::exit(1);
    }

    // Parse arguments (simplified)
    let mut api_key: Option<String> = None;
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
            "-k" => {
                i += 1;
                if i < args.len() {
                    api_key = Some(args[i].clone());
                }
            }
            "-n" => {
                i += 1;
                if i < args.len() {
                    network = Some(args[i].clone());
                }
            }
            "-v" => {
                i += 1;
                if i < args.len() {
                    vcpu = args[i].parse().ok();
                }
            }
            "-e" => {
                i += 1;
                if i < args.len() {
                    envs.push(args[i].clone());
                }
            }
            "-f" => {
                i += 1;
                if i < args.len() {
                    files.push(args[i].clone());
                }
            }
            "-a" => artifacts = true,
            "-o" => {
                i += 1;
                if i < args.len() {
                    output_dir = Some(args[i].clone());
                }
            }
            "session" => {
                let (public_key, secret_key) = get_api_keys(api_key.as_deref());
                // Collect -f files for session
                let mut session_files: Vec<String> = Vec::new();
                let mut j = i + 1;
                while j < args.len() {
                    if args[j] == "-f" && j + 1 < args.len() {
                        session_files.push(args[j + 1].clone());
                        j += 2;
                    } else {
                        j += 1;
                    }
                }
                cmd_session(
                    args.contains(&"--list".to_string()),
                    args.iter().position(|x| x == "--kill").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--shell").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    network.as_deref(),
                    vcpu,
                    args.contains(&"--tmux".to_string()),
                    args.contains(&"--screen".to_string()),
                    &session_files,
                    &public_key,
                    &secret_key,
                );
                return;
            }
            "service" => {
                let (public_key, secret_key) = get_api_keys(api_key.as_deref());
                // Collect -f files for service
                let mut service_files: Vec<String> = Vec::new();
                let mut j = i + 1;
                while j < args.len() {
                    if args[j] == "-f" && j + 1 < args.len() {
                        service_files.push(args[j + 1].clone());
                        j += 2;
                    } else {
                        j += 1;
                    }
                }
                cmd_service(
                    args.iter().position(|x| x == "--name").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--ports").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--domains").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--type").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--bootstrap").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--bootstrap-file").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    &service_files,
                    args.contains(&"--list".to_string()),
                    args.iter().position(|x| x == "--info").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--logs").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--tail").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--freeze").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--unfreeze").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--destroy").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--execute").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--command").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--dump-bootstrap").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    args.iter().position(|x| x == "--dump-file").and_then(|p| args.get(p + 1)).map(|s| s.as_str()),
                    network.as_deref(),
                    vcpu,
                    &public_key,
                    &secret_key,
                );
                return;
            }
            "key" => {
                let (public_key, secret_key) = get_api_keys(api_key.as_deref());
                cmd_key(
                    args.contains(&"--extend".to_string()),
                    &public_key,
                    &secret_key,
                );
                return;
            }
            _ => {
                if !args[i].starts_with('-') {
                    source_file = Some(args[i].clone());
                }
            }
        }
        i += 1;
    }

    // Execute mode
    if let Some(file) = source_file {
        let (public_key, secret_key) = get_api_keys(api_key.as_deref());
        cmd_execute(
            &file,
            envs,
            files,
            artifacts,
            output_dir.as_deref(),
            network.as_deref(),
            vcpu,
            &public_key,
            &secret_key,
        );
    } else {
        eprintln!("{}Error: No source file specified{}", RED, RESET);
        process::exit(1);
    }
}
