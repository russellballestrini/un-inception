// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Hello World example for unsandbox Rust SDK - Synchronous Version
//
// This example demonstrates basic synchronous execution using the SDK.
// Shows how to execute code from a Rust program using the sync SDK.
//
// To run:
//     export UNSANDBOX_PUBLIC_KEY="your-public-key"
//     export UNSANDBOX_SECRET_KEY="your-secret-key"
//     cargo run --example hello_world
//
// Expected output:
//     Executing code synchronously...
//     Result status: completed
//     Output: Hello from unsandbox!

use un::{execute_code, resolve_credentials, UnsandboxError};

fn main() -> Result<(), UnsandboxError> {
    // The code to execute
    let code = r#"print("Hello from unsandbox!")"#;

    // Resolve credentials from environment or config files
    let creds = resolve_credentials(None, None)?;

    // Execute the code synchronously
    println!("Executing code synchronously...");
    let result = execute_code("python", code, &creds)?;

    // Check the result
    println!("Result status: {}", result.status);
    println!("Output: {}", result.output.trim());

    if result.exit_code != 0 {
        eprintln!("Exit code: {}", result.exit_code);
        std::process::exit(1);
    }

    Ok(())
}
