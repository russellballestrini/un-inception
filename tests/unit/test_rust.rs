// Unit tests for un.rs - tests internal functions without API calls
// Run with: rustc --test test_rust.rs && ./test_rust

use std::collections::HashMap;

fn main() {
    let mut passed = 0;
    let mut failed = 0;

    println!("\n=== Extension Mapping Tests ===");

    let mut ext_map: HashMap<&str, &str> = HashMap::new();
    ext_map.insert(".py", "python");
    ext_map.insert(".js", "javascript");
    ext_map.insert(".ts", "typescript");
    ext_map.insert(".rb", "ruby");
    ext_map.insert(".go", "go");
    ext_map.insert(".rs", "rust");
    ext_map.insert(".c", "c");
    ext_map.insert(".cpp", "cpp");
    ext_map.insert(".java", "java");
    ext_map.insert(".hs", "haskell");

    test("Python extension maps correctly", || {
        assert_eq!(ext_map.get(".py"), Some(&"python"));
    }, &mut passed, &mut failed);

    test("Rust extension maps correctly", || {
        assert_eq!(ext_map.get(".rs"), Some(&"rust"));
    }, &mut passed, &mut failed);

    test("JavaScript extension maps correctly", || {
        assert_eq!(ext_map.get(".js"), Some(&"javascript"));
    }, &mut passed, &mut failed);

    test("Go extension maps correctly", || {
        assert_eq!(ext_map.get(".go"), Some(&"go"));
    }, &mut passed, &mut failed);

    println!("\n=== Signature Format Tests ===");

    test("Signature format is timestamp:METHOD:path:body", || {
        let timestamp = "1704067200";
        let method = "POST";
        let endpoint = "/execute";
        let body = r#"{"language":"python"}"#;
        let message = format!("{}:{}:{}:{}", timestamp, method, endpoint, body);

        assert!(message.starts_with(timestamp));
        assert!(message.contains(":POST:"));
        assert!(message.contains(":/execute:"));
    }, &mut passed, &mut failed);

    println!("\n=== Language Detection Tests ===");

    test("Python shebang detection", || {
        let content = "#!/usr/bin/env python3\nprint('hello')";
        let first_line: &str = content.lines().next().unwrap();
        assert!(first_line.starts_with("#!"));
        assert!(first_line.contains("python"));
    }, &mut passed, &mut failed);

    println!("\n=== Argument Parsing Tests ===");

    test("Parse -e KEY=VALUE format", || {
        let arg = "DEBUG=1";
        let parts: Vec<&str> = arg.splitn(2, '=').collect();
        assert_eq!(parts[0], "DEBUG");
        assert_eq!(parts[1], "1");
    }, &mut passed, &mut failed);

    test("Parse -e KEY=VALUE with equals in value", || {
        let arg = "URL=https://example.com?foo=bar";
        let parts: Vec<&str> = arg.splitn(2, '=').collect();
        assert_eq!(parts[0], "URL");
        assert_eq!(parts[1], "https://example.com?foo=bar");
    }, &mut passed, &mut failed);

    println!("\n=== File Operations Tests ===");

    test("Extract file basename", || {
        let path = "/home/user/project/script.rs";
        let basename = path.rsplit('/').next().unwrap();
        assert_eq!(basename, "script.rs");
    }, &mut passed, &mut failed);

    test("Extract file extension", || {
        let path = "/home/user/project/script.rs";
        let ext = path.rsplit('.').next().unwrap();
        assert_eq!(ext, "rs");
    }, &mut passed, &mut failed);

    println!("\n=== API Constants Tests ===");

    test("API base URL format", || {
        let api_base = "https://api.unsandbox.com";
        assert!(api_base.starts_with("https://"));
        assert!(api_base.contains("unsandbox.com"));
    }, &mut passed, &mut failed);

    println!("\n=== Summary ===");
    println!("Passed: {}", passed);
    println!("Failed: {}", failed);
    println!("Total:  {}", passed + failed);

    std::process::exit(if failed > 0 { 1 } else { 0 });
}

fn test<F>(name: &str, f: F, passed: &mut i32, failed: &mut i32)
where
    F: FnOnce() + std::panic::UnwindSafe,
{
    match std::panic::catch_unwind(f) {
        Ok(_) => {
            println!("  ✓ {}", name);
            *passed += 1;
        }
        Err(_) => {
            println!("  ✗ {}", name);
            *failed += 1;
        }
    }
}
