// Tests for the Rust unsandbox SDK
// Run with: cargo test

use un::*;
use std::env;

// ============================================================================
// Unit Tests - Test exported library functions
// ============================================================================

#[test]
fn test_detect_language() {
    assert_eq!(detect_language("script.py"), Some("python"));
    assert_eq!(detect_language("script.js"), Some("javascript"));
    assert_eq!(detect_language("script.ts"), Some("typescript"));
    assert_eq!(detect_language("script.rb"), Some("ruby"));
    assert_eq!(detect_language("script.go"), Some("go"));
    assert_eq!(detect_language("script.rs"), Some("rust"));
    assert_eq!(detect_language("script.c"), Some("c"));
    assert_eq!(detect_language("script.cpp"), Some("cpp"));
    assert_eq!(detect_language("script.d"), Some("d"));
    assert_eq!(detect_language("script.zig"), Some("zig"));
    assert_eq!(detect_language("script.sh"), Some("bash"));
    assert_eq!(detect_language("script.lua"), Some("lua"));
    assert_eq!(detect_language("script.php"), Some("php"));
    assert_eq!(detect_language("script.unknown"), None);
    assert_eq!(detect_language("script"), None);
}

#[test]
fn test_hmac_sign() {
    let secret_key = "test-secret";
    let message = "test-message";

    let result = hmac_sign(secret_key, message);

    // Should return a 64-character hex string
    assert_eq!(result.len(), 64, "HMAC signature should be 64 characters");

    // Should be deterministic
    let result2 = hmac_sign(secret_key, message);
    assert_eq!(result, result2, "HMAC sign is not deterministic");

    // Different inputs should produce different outputs
    let result3 = hmac_sign(secret_key, "different-message");
    assert_ne!(result, result3, "Different inputs should produce different signatures");
}

#[test]
fn test_version() {
    let v = version();
    assert!(!v.is_empty(), "Version should not be empty");
    // Should be in semver format (at least "0.0.0")
    assert!(v.len() >= 5, "Version should be in semver format");
}

#[test]
fn test_last_error() {
    // Set an error
    set_last_error("test error message");

    // Retrieve it
    let err = last_error();
    assert_eq!(err, "test error message");

    // Clear it
    set_last_error("");
    let err = last_error();
    assert!(err.is_empty(), "Error should be cleared");
}

#[test]
fn test_credentials_new() {
    let pk = "unsb-pk-test-test-test-test";
    let sk = "unsb-sk-test1-test2-test3-test4";

    let creds = Credentials::new(pk, sk);

    assert_eq!(creds.public_key, pk);
    assert_eq!(creds.secret_key, sk);
}

// ============================================================================
// Integration Tests - Test SDK internal consistency
// ============================================================================

#[test]
fn test_resolve_credentials_from_env() {
    // Save original env vars
    let orig_pk = env::var("UNSANDBOX_PUBLIC_KEY").ok();
    let orig_sk = env::var("UNSANDBOX_SECRET_KEY").ok();

    // Set test env vars
    let test_pk = "unsb-pk-test-test-test-test";
    let test_sk = "unsb-sk-test1-test2-test3-test4";
    env::set_var("UNSANDBOX_PUBLIC_KEY", test_pk);
    env::set_var("UNSANDBOX_SECRET_KEY", test_sk);

    // Test
    let creds = resolve_credentials(None, None).expect("Should resolve from env");
    assert_eq!(creds.public_key, test_pk);
    assert_eq!(creds.secret_key, test_sk);

    // Restore original env vars
    match orig_pk {
        Some(v) => env::set_var("UNSANDBOX_PUBLIC_KEY", v),
        None => env::remove_var("UNSANDBOX_PUBLIC_KEY"),
    }
    match orig_sk {
        Some(v) => env::set_var("UNSANDBOX_SECRET_KEY", v),
        None => env::remove_var("UNSANDBOX_SECRET_KEY"),
    }
}

#[test]
fn test_resolve_credentials_from_args() {
    let test_pk = "unsb-pk-arg1-arg2-arg3-arg4";
    let test_sk = "unsb-sk-arg11-arg22-arg33-arg44";

    let creds = resolve_credentials(Some(test_pk), Some(test_sk))
        .expect("Should resolve from args");
    assert_eq!(creds.public_key, test_pk);
    assert_eq!(creds.secret_key, test_sk);
}

// ============================================================================
// Functional Tests - Test against real API (requires credentials)
// ============================================================================

fn get_test_credentials() -> Option<Credentials> {
    resolve_credentials(None, None).ok()
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_health_check() {
    let healthy = health_check();
    if !healthy {
        eprintln!("API health check returned unhealthy (API may be unreachable)");
    }
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_get_languages() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let languages = get_languages(&creds).expect("GetLanguages failed");

    assert!(!languages.is_empty(), "Languages list should not be empty");

    // Should include common languages
    assert!(
        languages.contains(&"python".to_string()),
        "Languages should include python"
    );
    assert!(
        languages.contains(&"javascript".to_string()),
        "Languages should include javascript"
    );
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_validate_keys() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let result = validate_keys(&creds).expect("ValidateKeys failed");

    assert!(result.valid, "Keys should be valid");
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_execute_code() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let result = execute_code("python", "print('hello from rust test')", &creds)
        .expect("ExecuteCode failed");

    assert!(!result.output.is_empty(), "Output should not be empty");
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_list_sessions() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let _sessions = list_sessions(&creds).expect("ListSessions failed");
    // Should return a list (possibly empty)
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_list_services() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let _services = list_services(&creds).expect("ListServices failed");
    // Should return a list (possibly empty)
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_list_snapshots() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let _snapshots = list_snapshots(&creds).expect("ListSnapshots failed");
    // Should return a list (possibly empty)
}

#[test]
#[ignore] // Run with: cargo test -- --ignored
fn test_list_images() {
    let creds = get_test_credentials().expect("No credentials for functional test");
    let _images = list_images(None, &creds).expect("ListImages failed");
    // Should return a list (possibly empty)
}
