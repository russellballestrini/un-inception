// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// unsandbox.com CLI (Rust Async)
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
    std::process::exit(un_async::cli_main());
}
