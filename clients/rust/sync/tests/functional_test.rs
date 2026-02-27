// This is free software for the public good of a permacomputer hosted at
// permacomputer.com, an always-on computer by the people, for the people.
// One which is durable, easy to repair, & distributed like tap water
// for machine learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around
// four values:
//
//   TRUTH      First principles, math & science, open source code freely distributed
//   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE       Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
// Code is seeds to sprout on any abandoned technology.

//! UN Rust SDK - Functional Tests
//!
//! Tests library functions against real API.
//! Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
//!
//! Usage:
//!   cd clients/rust/sync && cargo test --test functional_test

use un::*;
use std::env;

fn get_creds() -> Option<Credentials> {
    let pk = env::var("UNSANDBOX_PUBLIC_KEY").ok()?;
    let sk = env::var("UNSANDBOX_SECRET_KEY").ok()?;
    if pk.is_empty() || sk.is_empty() {
        return None;
    }
    Some(Credentials::new(pk, sk))
}

macro_rules! skip_no_creds {
    () => {
        match get_creds() {
            Some(c) => c,
            None => {
                eprintln!("SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required");
                return;
            }
        }
    };
}

#[test]
fn functional_health_check() {
    let _ = skip_no_creds!();
    let result = health_check();
    // Just verify it returns without panic
    eprintln!("health_check: {}", result);
}

#[test]
fn functional_validate_keys() {
    let creds = skip_no_creds!();
    let info = validate_keys(&creds).expect("validate_keys should succeed");
    assert!(info.valid, "keys should be valid");
}

#[test]
fn functional_get_languages() {
    let creds = skip_no_creds!();
    let langs = get_languages(&creds).expect("get_languages should succeed");
    assert!(!langs.is_empty(), "should have at least one language");
    assert!(langs.contains(&"python".to_string()), "python should be in languages list");
    eprintln!("Found {} languages", langs.len());
}

#[test]
fn functional_execute() {
    let creds = skip_no_creds!();
    let result = execute_code("python", "print('hello from Rust SDK')", &creds)
        .expect("execute should succeed");
    assert!(result.stdout.contains("hello from Rust SDK"), "stdout should contain expected output");
    assert_eq!(result.exit_code, 0, "exit code should be 0");
}

#[test]
fn functional_execute_error() {
    let creds = skip_no_creds!();
    let result = execute_code("python", "import sys; sys.exit(1)", &creds)
        .expect("execute should return result even on error");
    assert_eq!(result.exit_code, 1, "exit code should be 1");
}

#[test]
fn functional_session_list() {
    let creds = skip_no_creds!();
    let sessions = list_sessions(&creds).expect("list_sessions should succeed");
    eprintln!("Found {} sessions", sessions.len());
}

#[test]
fn functional_session_lifecycle() {
    let creds = skip_no_creds!();

    // Create
    let session = create_session("python", &creds, None)
        .expect("create_session should succeed");
    assert!(!session.id.is_empty(), "session should have id");
    eprintln!("Created session: {}", session.id);

    // Destroy
    delete_session(&session.id, &creds).expect("delete_session should succeed");
}

#[test]
fn functional_service_list() {
    let creds = skip_no_creds!();
    let services = list_services(&creds).expect("list_services should succeed");
    eprintln!("Found {} services", services.len());
}

#[test]
fn functional_snapshot_list() {
    let creds = skip_no_creds!();
    let snapshots = list_snapshots(&creds).expect("list_snapshots should succeed");
    eprintln!("Found {} snapshots", snapshots.len());
}

#[test]
fn functional_image_list() {
    let creds = skip_no_creds!();
    let images = list_images(None, &creds).expect("list_images should succeed");
    eprintln!("Found {} images", images.len());
}
