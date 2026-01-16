// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// unsandbox.com CLI (Rust Sync)
//
// Binary entry point for the CLI.
//
// Usage:
//     un script.py              # Execute Python script
//     un -s bash 'echo hello'   # Execute inline code
//     un session --list         # List sessions
//     un service --list         # List services
//     un key                    # Check API key

fn main() {
    std::process::exit(un::cli_main());
}
