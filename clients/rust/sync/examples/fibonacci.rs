// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Fibonacci example for unsandbox Rust SDK - Synchronous Version
//
// This example demonstrates executing a recursive algorithm remotely.
// Shows how to run complex computations using the sync SDK.
//
// To run:
//     export UNSANDBOX_PUBLIC_KEY="your-public-key"
//     export UNSANDBOX_SECRET_KEY="your-secret-key"
//     cargo run --example fibonacci
//
// Expected output:
//     Executing Fibonacci code...
//     Result status: completed
//     Output:
//     fib(0) = 0
//     fib(1) = 1
//     fib(2) = 1
//     fib(3) = 2
//     fib(4) = 3
//     fib(5) = 5
//     fib(6) = 8
//     fib(7) = 13
//     fib(8) = 21
//     fib(9) = 34
//     fib(10) = 55
//     Execution time: XXXms

use un::{execute_code, resolve_credentials, UnsandboxError};

fn main() -> Result<(), UnsandboxError> {
    // Fibonacci code to execute remotely
    let code = r#"
def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

for i in range(11):
    print(f"fib({i}) = {fib(i)}")
"#;

    // Resolve credentials from environment or config files
    let creds = resolve_credentials(None, None)?;

    // Execute the code synchronously
    println!("Executing Fibonacci code...");
    let result = execute_code("python", code, &creds)?;

    // Check the result
    println!("Result status: {}", result.status);
    println!("Output:");
    println!("{}", result.output);
    println!("Execution time: {}ms", result.execution_time_ms);

    if result.exit_code != 0 {
        eprintln!("Execution failed with exit code: {}", result.exit_code);
        std::process::exit(1);
    }

    Ok(())
}
