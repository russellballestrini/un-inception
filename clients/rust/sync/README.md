# Unsandbox Rust SDK (Synchronous)

A synchronous Rust client library for [unsandbox.com](https://unsandbox.com) - secure, multi-language code execution.

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
un-sync = { git = "https://github.com/unsandbox/un-inception", version = "2.0" }
```

Or from local path:

```toml
[dependencies]
un = { path = "../un-inception/clients/rust/sync" }
```

## Quick Start

```rust
use un::{execute_code, resolve_credentials};

fn main() -> Result<(), un::UnsandboxError> {
    // Resolve credentials from environment or config files
    let creds = resolve_credentials(None, None)?;

    // Execute Python code
    let result = execute_code("python", r#"print("Hello from unsandbox!")"#, &creds)?;
    println!("Output: {}", result.output);

    Ok(())
}
```

## Authentication

The SDK supports 4-tier credential resolution:

1. **Function arguments** - Pass directly to `resolve_credentials()`
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
3. **Config file** - `~/.unsandbox/accounts.csv` (line 0 by default)
4. **Local directory** - `./accounts.csv` (line 0 by default)

### Setting up credentials

Create `~/.unsandbox/accounts.csv`:

```csv
unsb-pk-xxxx-xxxx-xxxx-xxxx,unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx
```

Or use environment variables:

```bash
export UNSANDBOX_PUBLIC_KEY="unsb-pk-xxxx-xxxx-xxxx-xxxx"
export UNSANDBOX_SECRET_KEY="unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx"
```

### Multiple accounts

Store multiple accounts in the CSV file (one per line):

```csv
unsb-pk-account1,unsb-sk-account1-secret
unsb-pk-account2,unsb-sk-account2-secret
```

Select account via environment variable:

```bash
export UNSANDBOX_ACCOUNT=1  # Use second account (0-indexed)
```

## API Reference

### Synchronous Execution

Execute code and wait for completion:

```rust
use un::{execute_code, resolve_credentials, Credentials};

let creds = resolve_credentials(None, None)?;
let result = execute_code("python", "print('hello')", &creds)?;

println!("Status: {}", result.status);           // "completed", "failed", etc.
println!("Output: {}", result.output);           // stdout/stderr combined
println!("Exit code: {}", result.exit_code);     // 0 = success
println!("Time: {}ms", result.execution_time_ms);
```

### Asynchronous Execution

Start execution and get a job ID:

```rust
use un::{execute_async, wait_for_job, resolve_credentials};

let creds = resolve_credentials(None, None)?;

// Start execution (returns immediately)
let job_id = execute_async("python", "import time; time.sleep(5); print('done')", &creds)?;

// Do other work...

// Wait for completion (with optional timeout in seconds)
let result = wait_for_job(&job_id, &creds, Some(60))?;
```

### Job Management

```rust
use un::{get_job, cancel_job, list_jobs, resolve_credentials};

let creds = resolve_credentials(None, None)?;

// Get single job status
let job = get_job("job_123", &creds)?;

// List all jobs for the account
let jobs = list_jobs(&creds)?;

// Cancel a running job
cancel_job("job_123", &creds)?;
```

### Languages

```rust
use un::{get_languages, detect_language, resolve_credentials};

let creds = resolve_credentials(None, None)?;

// Get list of supported languages (cached for 1 hour)
let languages = get_languages(&creds)?;
// Returns: ["python", "javascript", "go", "rust", ...]

// Detect language from filename
let lang = detect_language("script.py");  // Some("python")
let lang = detect_language("main.go");    // Some("go")
let lang = detect_language("unknown");    // None
```

### Snapshots

```rust
use un::{session_snapshot, list_snapshots, restore_snapshot, delete_snapshot, resolve_credentials};

let creds = resolve_credentials(None, None)?;

// Create a snapshot of a session
let snapshot = session_snapshot("session_123", &creds, Some("checkpoint"), false)?;

// List all snapshots
let snapshots = list_snapshots(&creds)?;

// Restore a snapshot
let result = restore_snapshot(&snapshot.snapshot_id, &creds)?;

// Delete a snapshot
delete_snapshot(&snapshot.snapshot_id, &creds)?;
```

## HMAC-SHA256 Authentication

All API requests are authenticated using HMAC-SHA256 signatures:

```
Authorization: Bearer <public_key>      # Identifies account
X-Timestamp: <unix_seconds>             # Replay prevention
X-Signature: HMAC-SHA256(secret, msg)   # Proves secret + body integrity

Message format: "timestamp:METHOD:path:body"
```

The SDK handles this automatically - you just provide credentials.

## Language Support

The SDK supports 50+ programming languages including:

- **Interpreted**: Python, JavaScript, Ruby, PHP, Perl, Bash, Lua, etc.
- **Compiled**: C, C++, Go, Rust, Java, Kotlin, etc.
- **Functional**: Haskell, OCaml, F#, Scheme, Clojure, etc.
- **Other**: WASM, Prolog, Forth, etc.

Use `get_languages()` for the complete list.

## Caching

The languages list is cached locally for 1 hour in `~/.unsandbox/languages.json`. This reduces API calls and improves startup performance.

To force a refresh, delete the cache file:

```bash
rm ~/.unsandbox/languages.json
```

## Error Handling

```rust
use un::{execute_code, resolve_credentials, UnsandboxError};

fn main() {
    match resolve_credentials(None, None) {
        Ok(creds) => {
            match execute_code("python", "print('hello')", &creds) {
                Ok(result) => println!("Output: {}", result.output),
                Err(UnsandboxError::ApiError { status, message }) => {
                    eprintln!("API error ({}): {}", status, message);
                }
                Err(UnsandboxError::HttpError(e)) => {
                    eprintln!("Network error: {}", e);
                }
                Err(UnsandboxError::Timeout(secs)) => {
                    eprintln!("Job timed out after {}s", secs);
                }
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        Err(UnsandboxError::NoCredentials) => {
            eprintln!("No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY");
        }
        Err(e) => eprintln!("Error: {}", e),
    }
}
```

## Examples

See the `examples/` directory for complete working examples:

- `hello_world.rs` - Simple print example
- `fibonacci.rs` - Recursive function example
- `multi_language.rs` - Execute code in multiple languages
- `async_polling.rs` - Async job submission and polling

Run an example:

```bash
export UNSANDBOX_PUBLIC_KEY="your-key"
export UNSANDBOX_SECRET_KEY="your-secret"
cargo run --example hello_world
```

## Testing

Run the test suite:

```bash
cargo test
```

With verbose output:

```bash
cargo test -- --nocapture
```

## Public Domain License

This code is released into the PUBLIC DOMAIN with NO WARRANTY and NO LICENSE.

You are free to:
- Use for any purpose
- Modify and distribute
- Use commercially
- Use privately

## Support

For issues or questions:
- GitHub Issues: https://github.com/unsandbox/un-inception/issues
- Website: https://unsandbox.com
