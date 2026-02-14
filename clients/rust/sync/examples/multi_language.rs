// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Multi-language example - standalone version
//
// This example demonstrates executing code in multiple programming languages.
// Shows the versatility of unsandbox's language support (simulated).
//
// To run:
//     rustc multi_language.rs && ./multi_language
//
// Expected output:
//     === PYTHON ===
//     Status: completed
//     Output: Hello from Python!
//
//     === JAVASCRIPT ===
//     Status: completed
//     Output: Hello from JavaScript!
//
//     === RUBY ===
//     Status: completed
//     Output: Hello from Ruby!
//
//     === GO ===
//     Status: completed
//     Output: Hello from Go!
//
//     All 4 languages executed successfully!

fn main() {
    // Define code snippets in different languages (simulated outputs)
    let languages = vec![
        ("python", "Hello from Python!"),
        ("javascript", "Hello from JavaScript!"),
        ("ruby", "Hello from Ruby!"),
        ("go", "Hello from Go!"),
    ];

    let mut success_count = 0;

    // "Execute" each language (simulate results)
    for (lang, output) in &languages {
        println!("=== {} ===", lang.to_uppercase());
        println!("Status: completed");
        println!("Output: {}", output);
        println!();
        success_count += 1;
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
}
