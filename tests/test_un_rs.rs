// Test suite for UN CLI Rust implementation
// Compile: rustc test_un_rs.rs -o test_un_rs
// Run: ./test_un_rs
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

use std::env;
use std::fs;
use std::path::Path;
use std::process::{self, Command};

// Copy of detect_language from un.rs for testing
fn detect_language(filename: &str) -> Option<&'static str> {
    let ext = Path::new(filename)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");

    match ext {
        "py" => Some("python"),
        "js" => Some("javascript"),
        "go" => Some("go"),
        "rs" => Some("rust"),
        "c" => Some("c"),
        "cpp" => Some("cpp"),
        "d" => Some("d"),
        "zig" => Some("zig"),
        "nim" => Some("nim"),
        "v" => Some("v"),
        _ => None,
    }
}

fn test_extension_detection() -> bool {
    println!("=== Test 1: Extension Detection ===");

    let tests = vec![
        ("script.py", Some("python")),
        ("app.js", Some("javascript")),
        ("main.go", Some("go")),
        ("program.rs", Some("rust")),
        ("code.c", Some("c")),
        ("app.cpp", Some("cpp")),
        ("prog.d", Some("d")),
        ("main.zig", Some("zig")),
        ("script.nim", Some("nim")),
        ("app.v", Some("v")),
        ("unknown.xyz", None),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (filename, expected) in tests {
        let result = detect_language(filename);
        if result == expected {
            println!("  PASS: {} -> {:?}", filename, result);
            passed += 1;
        } else {
            println!("  FAIL: {} -> got {:?}, expected {:?}", filename, result, expected);
            failed += 1;
        }
    }

    println!("Extension Detection: {} passed, {} failed\n", passed, failed);
    failed == 0
}

fn test_api_connection() -> bool {
    println!("=== Test 2: API Connection ===");

    let api_key = match env::var("UNSANDBOX_API_KEY") {
        Ok(key) => key,
        Err(_) => {
            println!("  SKIP: UNSANDBOX_API_KEY not set");
            println!("API Connection: skipped\n");
            return true;
        }
    };

    // Simple Python script to test API
    let code = "print('Hello from API test')";

    let client = reqwest::blocking::Client::new();
    let request_body = serde_json::json!({
        "language": "python",
        "code": code
    });

    let response = match client
        .post("https://api.unsandbox.com/execute")
        .header("Authorization", format!("Bearer {}", api_key))
        .json(&request_body)
        .send()
    {
        Ok(resp) => resp,
        Err(e) => {
            println!("  FAIL: HTTP request error: {}", e);
            return false;
        }
    };

    if !response.status().is_success() {
        println!("  FAIL: HTTP status {}", response.status());
        return false;
    }

    let result: serde_json::Value = match response.json() {
        Ok(json) => json,
        Err(e) => {
            println!("  FAIL: JSON parse error: {}", e);
            return false;
        }
    };

    let stdout_str = result["stdout"].as_str().unwrap_or("");
    if !stdout_str.contains("Hello from API test") {
        println!("  FAIL: Unexpected output: {}", stdout_str);
        return false;
    }

    println!("  PASS: API connection successful");
    println!("API Connection: passed\n");
    true
}

fn test_fib_execution() -> bool {
    println!("=== Test 3: Functional Test (fib.go) ===");

    let api_key = match env::var("UNSANDBOX_API_KEY") {
        Ok(_) => {},
        Err(_) => {
            println!("  SKIP: UNSANDBOX_API_KEY not set");
            println!("Functional Test: skipped\n");
            return true;
        }
    };

    // Check if un_rust binary exists
    let un_binary = "../un_rust";
    if !Path::new(un_binary).exists() {
        println!("  SKIP: {} binary not found (run: cd .. && rustc un.rs -o un_rust)", un_binary);
        println!("Functional Test: skipped\n");
        return true;
    }

    // Check if fib.go exists
    let fib_file = "fib.go";
    if !Path::new(fib_file).exists() {
        println!("  SKIP: {} not found", fib_file);
        println!("Functional Test: skipped\n");
        return true;
    }

    // Run un_rust with fib.go
    let output = match Command::new(un_binary)
        .arg(fib_file)
        .output()
    {
        Ok(out) => out,
        Err(e) => {
            println!("  FAIL: Execution error: {}", e);
            return false;
        }
    };

    if !output.status.success() {
        println!("  FAIL: Command failed with exit code: {:?}", output.status.code());
        println!("  STDERR: {}", String::from_utf8_lossy(&output.stderr));
        return false;
    }

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    if !stdout_str.contains("fib(10) = 55") {
        println!("  FAIL: Expected output to contain 'fib(10) = 55', got: {}", stdout_str);
        return false;
    }

    println!("  PASS: fib.go executed successfully");
    print!("  Output: {}", stdout_str);
    println!("Functional Test: passed\n");
    true
}

fn main() {
    println!("UN CLI Rust Implementation Test Suite");
    println!("======================================\n");

    let mut all_passed = true;

    if !test_extension_detection() {
        all_passed = false;
    }

    if !test_api_connection() {
        all_passed = false;
    }

    if !test_fib_execution() {
        all_passed = false;
    }

    println!("======================================");
    if all_passed {
        println!("RESULT: ALL TESTS PASSED");
        process::exit(0);
    } else {
        println!("RESULT: SOME TESTS FAILED");
        process::exit(1);
    }
}

// Note: This test requires the following dependencies if compiled with cargo:
// [dependencies]
// reqwest = { version = "0.11", features = ["blocking", "json"] }
// serde_json = "1.0"
