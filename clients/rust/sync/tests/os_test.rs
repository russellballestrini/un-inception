// Offline tests for the target OS option (tickets #123, #124).
//
// A local recording HTTP server stands in for our API (UNSANDBOX_API_URL),
// so these tests need no network and no credentials.
//
// Run with: cargo test --test os_test

use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpListener;
use std::sync::{Arc, Mutex};
use std::thread;
use un::*;

type Log = Arc<Mutex<Vec<(String, serde_json::Value)>>>;

/// Start a server that records (path, JSON body) of every POST and answers
/// /execute with a completed job, everything else with HTTP 400.
fn start_recorder() -> (String, Log) {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let url = format!("http://{}", listener.local_addr().unwrap());
    let log: Log = Arc::new(Mutex::new(Vec::new()));
    let log_thread = Arc::clone(&log);
    thread::spawn(move || {
        for stream in listener.incoming() {
            let mut stream = match stream {
                Ok(s) => s,
                Err(_) => continue,
            };
            let mut reader = BufReader::new(stream.try_clone().unwrap());
            let mut request_line = String::new();
            reader.read_line(&mut request_line).unwrap();
            let path = request_line.split_whitespace().nth(1).unwrap_or("").to_string();
            let mut length = 0usize;
            loop {
                let mut line = String::new();
                reader.read_line(&mut line).unwrap();
                if line == "\r\n" || line.is_empty() {
                    break;
                }
                if let Some((k, v)) = line.split_once(':') {
                    if k.eq_ignore_ascii_case("content-length") {
                        length = v.trim().parse().unwrap_or(0);
                    }
                }
            }
            let mut body = vec![0u8; length];
            reader.read_exact(&mut body).unwrap();
            let json = serde_json::from_slice(&body).unwrap_or(serde_json::Value::Null);
            log_thread.lock().unwrap().push((path.clone(), json));
            let (status, reply) = if path.starts_with("/execute") {
                ("200 OK", r#"{"job_id":"job-1","status":"completed","output":"","exit_code":0}"#)
            } else {
                ("400 Bad Request", r#"{"error":"mock: not created"}"#)
            };
            let _ = write!(
                stream,
                "HTTP/1.1 {}\r\nContent-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
                status,
                reply.len(),
                reply
            );
        }
    });
    (url, log)
}

fn last_body(log: &Log, path: &str) -> serde_json::Value {
    log.lock()
        .unwrap()
        .iter()
        .rev()
        .find(|(p, _)| p == path)
        .map(|(_, b)| b.clone())
        .expect("no request recorded")
}

#[test]
fn test_normalize_os() {
    assert_eq!(normalize_os("linux").unwrap(), "linux");
    assert_eq!(normalize_os("FreeBSD").unwrap(), "freebsd");
    assert!(matches!(normalize_os("openbsd"), Err(UnsandboxError::InvalidOs(_))));
    assert!(matches!(normalize_os(""), Err(UnsandboxError::InvalidOs(_))));
}

// One test owns UNSANDBOX_API_URL so parallel tests never race on it.
#[test]
fn test_os_field_in_request_bodies() {
    let (url, log) = start_recorder();
    std::env::set_var("UNSANDBOX_API_URL", &url);
    let creds = Credentials::new("unsb-pk-test-0000-0000-0000", "unsb-sk-test0-00000-00000-00000");

    // execute: present with os, absent without
    execute_code_with_os("bash", "echo hi", Some("FreeBSD"), &creds).unwrap();
    assert_eq!(last_body(&log, "/execute")["os"], "freebsd");
    execute_code("bash", "echo hi", &creds).unwrap();
    assert!(last_body(&log, "/execute").get("os").is_none());
    execute_async_with_os("bash", "echo hi", Some("linux"), &creds).unwrap();
    assert_eq!(last_body(&log, "/execute")["os"], "linux");

    // session
    let opts = SessionCreateOptions { os: Some("freebsd".into()), ..Default::default() };
    let _ = create_session("bash", &creds, Some(opts));
    assert_eq!(last_body(&log, "/sessions")["os"], "freebsd");
    let _ = create_session("bash", &creds, None);
    assert!(last_body(&log, "/sessions").get("os").is_none());

    // service
    let opts = ServiceCreateOptions { os: Some("freebsd".into()), ..Default::default() };
    let _ = create_service("ostest", &[80], "true", &creds, Some(opts));
    assert_eq!(last_body(&log, "/services")["os"], "freebsd");
    let _ = create_service("ostest", &[80], "true", &creds, None);
    assert!(last_body(&log, "/services").get("os").is_none());

    // invalid values are rejected before any request is sent
    let before = log.lock().unwrap().len();
    assert!(matches!(
        execute_code_with_os("bash", "echo hi", Some("openbsd"), &creds),
        Err(UnsandboxError::InvalidOs(_))
    ));
    let opts = SessionCreateOptions { os: Some(String::new()), ..Default::default() };
    assert!(matches!(create_session("bash", &creds, Some(opts)), Err(UnsandboxError::InvalidOs(_))));
    let opts = ServiceCreateOptions { os: Some("windows".into()), ..Default::default() };
    assert!(matches!(
        create_service("ostest", &[80], "true", &creds, Some(opts)),
        Err(UnsandboxError::InvalidOs(_))
    ));
    assert_eq!(log.lock().unwrap().len(), before, "invalid os must send nothing");
}
