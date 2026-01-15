// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Async polling example for unsandbox Rust SDK - Synchronous Version
//
// This example demonstrates async job submission and polling.
// Shows how to submit a job and poll for its completion.
//
// To run:
//     export UNSANDBOX_PUBLIC_KEY="your-public-key"
//     export UNSANDBOX_SECRET_KEY="your-secret-key"
//     cargo run --example async_polling
//
// Expected output:
//     Submitting async job...
//     Job ID: job_xxxxxxxx
//     Waiting for completion...
//     Job completed!
//     Status: completed
//     Output: Task completed after 2 seconds
//     Execution time: XXXXms

use un::{execute_async, resolve_credentials, wait_for_job, UnsandboxError};

fn main() -> Result<(), UnsandboxError> {
    // Long-running code to execute
    let code = r#"
import time
time.sleep(2)
print("Task completed after 2 seconds")
"#;

    // Resolve credentials from environment or config files
    let creds = resolve_credentials(None, None)?;

    // Submit job asynchronously
    println!("Submitting async job...");
    let job_id = execute_async("python", code, &creds)?;
    println!("Job ID: {}", job_id);

    // Wait for completion with custom timeout (30 seconds)
    println!("Waiting for completion...");
    let result = wait_for_job(&job_id, &creds, Some(30))?;

    // Display results
    println!("Job completed!");
    println!("Status: {}", result.status);
    println!("Output: {}", result.output.trim());
    println!("Execution time: {}ms", result.execution_time_ms);

    if result.exit_code != 0 {
        eprintln!("Exit code: {}", result.exit_code);
        std::process::exit(1);
    }

    Ok(())
}
