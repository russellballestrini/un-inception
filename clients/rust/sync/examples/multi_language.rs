// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Multi-language example for unsandbox Rust SDK - Synchronous Version
//
// This example demonstrates executing code in multiple programming languages.
// Shows the versatility of unsandbox's language support.
//
// To run:
//     export UNSANDBOX_PUBLIC_KEY="your-public-key"
//     export UNSANDBOX_SECRET_KEY="your-secret-key"
//     cargo run --example multi_language
//
// Expected output:
//     === Python ===
//     Status: completed
//     Output: Hello from Python!
//
//     === JavaScript ===
//     Status: completed
//     Output: Hello from JavaScript!
//
//     === Ruby ===
//     Status: completed
//     Output: Hello from Ruby!
//
//     === Go ===
//     Status: completed
//     Output: Hello from Go!
//
//     All 4 languages executed successfully!

use un::{execute_code, resolve_credentials, UnsandboxError};

fn main() -> Result<(), UnsandboxError> {
    // Define code snippets in different languages
    let languages = vec![
        ("python", r#"print("Hello from Python!")"#),
        ("javascript", r#"console.log("Hello from JavaScript!")"#),
        ("ruby", r#"puts "Hello from Ruby!""#),
        (
            "go",
            r#"package main
import "fmt"
func main() { fmt.Println("Hello from Go!") }"#,
        ),
    ];

    // Resolve credentials from environment or config files
    let creds = resolve_credentials(None, None)?;

    let mut success_count = 0;

    // Execute each language
    for (lang, code) in &languages {
        println!("=== {} ===", lang.to_uppercase());

        match execute_code(lang, code, &creds) {
            Ok(result) => {
                println!("Status: {}", result.status);
                println!("Output: {}", result.output.trim());
                if result.status == "completed" && result.exit_code == 0 {
                    success_count += 1;
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
            }
        }

        println!();
    }

    println!(
        "All {} languages executed successfully!",
        languages.len()
    );

    if success_count != languages.len() {
        eprintln!(
            "Warning: Only {}/{} executions succeeded",
            success_count,
            languages.len()
        );
        std::process::exit(1);
    }

    Ok(())
}
