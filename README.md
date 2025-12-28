# UN CLI Inception

The UN CLI written in every language it can execute. **42 implementations, one unified interface.**

> "If you replace every line of un.c with a different programming language, but it still executes code on unsandbox... is it still un?"

## Quick Start

```bash
# Clone all 42 implementations
git clone https://github.com/russellballestrini/un-inception.git
cd un-inception

# Set your API keys (HMAC authentication)
export UNSANDBOX_PUBLIC_KEY="unsb-pk-xxxx-xxxx-xxxx-xxxx"
export UNSANDBOX_SECRET_KEY="unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx"

# Run any implementation
python3 un.py test/fib.py
node un.js test/fib.py
ruby un.rb test/fib.py
go run un.go test/fib.py

# They all produce the same output:
# fib(10) = 55
```

## HMAC Authentication

All implementations use HMAC-SHA256 for request signing:

```
Authorization: Bearer {public_key}
X-Timestamp: {unix_seconds}
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
```

### HMAC Dependencies by Language

| Language | HMAC Library | Notes |
|----------|--------------|-------|
| **Python** | `hmac`, `hashlib` | Standard library |
| **JavaScript** | `crypto` | Node.js built-in |
| **TypeScript** | `crypto` | Node.js built-in |
| **Ruby** | `openssl` | Standard library |
| **Go** | `crypto/hmac`, `crypto/sha256` | Standard library |
| **Rust** | `hmac`, `sha2` | Crates (add to Cargo.toml) |
| **PHP** | `hash_hmac()` | Built-in function |
| **Perl** | `Digest::SHA` | Core module |
| **Lua** | `openssl` CLI | Shells out to `openssl dgst` |
| **Bash** | `openssl` CLI | Uses `openssl dgst -sha256 -hmac` |
| **C/C++** | OpenSSL | Link with `-lssl -lcrypto` |
| **Java** | `javax.crypto` | Standard library |
| **C#** | `System.Security.Cryptography` | .NET built-in |
| **Haskell** | `cryptonite` | Hackage package |
| **Clojure** | `buddy-core` | Clojars dependency |
| **Erlang/Elixir** | `:crypto` | OTP built-in |
| **Julia** | `SHA` | Standard library |
| **R** | `openssl` | CRAN package |

Most languages have HMAC-SHA256 in their standard library. Languages without native support (Lua, Bash, AWK) shell out to the `openssl` command-line tool.

## Implementations

| Language | File | Category |
|----------|------|----------|
| Python | `un.py` | Scripting |
| JavaScript | `un.js` | Scripting |
| TypeScript | `un.ts` | Scripting |
| Ruby | `un.rb` | Scripting |
| PHP | `un.php` | Scripting |
| Perl | `un.pl` | Scripting |
| Lua | `un.lua` | Scripting |
| Bash | `un.sh` | Scripting |
| Go | `un.go` | Systems |
| Rust | `un.rs` | Systems |
| C | `un_inception.c` | Systems |
| C++ | `un.cpp` | Systems |
| D | `un.d` | Systems |
| Nim | `un.nim` | Systems |
| Zig | `un.zig` | Systems |
| V | `un.v` | Systems |
| Java | `Un.java` | JVM/.NET |
| Kotlin | `un.kt` | JVM/.NET |
| C# | `Un.cs` | JVM/.NET |
| F# | `un.fs` | JVM/.NET |
| Groovy | `un.groovy` | JVM/.NET |
| Dart | `un.dart` | JVM/.NET |
| Haskell | `un.hs` | Functional |
| OCaml | `un.ml` | Functional |
| Clojure | `un.clj` | Functional |
| Scheme | `un.scm` | Functional |
| Common Lisp | `un.lisp` | Functional |
| Erlang | `un.erl` | Functional |
| Elixir | `un.ex` | Functional |
| Julia | `un.jl` | Scientific |
| R | `un.r` | Scientific |
| Crystal | `un.cr` | Scientific |
| Fortran | `un.f90` | Scientific |
| COBOL | `un.cob` | Legacy |
| Prolog | `un.pro` | Logic |
| Forth | `un.forth` | Stack |
| Tcl | `un.tcl` | Other |
| Raku | `un.raku` | Other |
| Objective-C | `un.m` | Other |
| Deno | `un_deno.ts` | Other |
| PowerShell | `un.ps1` | Other |
| AWK | `un.awk` | Other |

## Features

Each implementation supports:

- **Execute code files** via the unsandbox API
- **Auto-detect language** from file extensions
- **Environment variables** (`-e KEY=VALUE`)
- **Input files** (`-f file.txt`)
- **Artifact collection** (`-a -o ./output`)
- **Interactive sessions** (`session` subcommand)
- **Persistent services** (`service` subcommand)
- **API key management** (`key` subcommand)
- **Network modes** (`-n zerotrust` or `-n semitrusted`)

## Usage

```bash
# Execute a script
./un.py script.py

# With environment variables
./un.py -e DEBUG=1 -e NAME=World script.py

# With input files
./un.py -f data.csv -f config.json process.py

# Collect artifacts
./un.py -a -o ./output main.c

# Interactive session
./un.py session --shell python3

# Persistent service
./un.py service --name myapp --ports 8080 --bootstrap "python3 -m http.server 8080"

# Check API key status
./un.py key

# Extend/renew API key (opens browser)
./un.py key --extend
```

## Testing

```bash
# Run a quick test with any implementation
./un.py test/fib.py
# Output: fib(10) = 55

# Run the full test matrix (requires API key)
./tests/run_all_tests.sh
```

## The Inception Matrix

**Use un to run un inside un!** Don't have a language installed locally? Run its implementation through unsandbox using `un` (the C implementation):

```bash
# Pass API keys via -e so the inner un.* can call the API
# Use -n semitrusted so inner script can reach api.unsandbox.com

# Don't have PHP? Run un.php through unsandbox!
un -n semitrusted -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY un.php test/fib.py

# Don't have Julia? Run un.jl through unsandbox!
un -n semitrusted -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY un.jl test/fib.py

# Test ALL implementations regardless of local interpreters
for impl in un.py un.js un.rb un.go un.php un.pl un.lua; do
    echo "Testing $impl..."
    un -n semitrusted \
       -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
       -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
       "$impl" test/fib.py
done
```

This is the **inception** - each layer executes through unsandbox's remote API, so you can test any implementation using `un` (the canonical C implementation) as the runner.

## Links

- [unsandbox.com](https://unsandbox.com) - Remote code execution API
- [CLI Inception Gallery](https://unsandbox.com/cli/inception) - Browse all implementations
- [API Documentation](https://unsandbox.com/docs) - Full API reference

## License

**PUBLIC DOMAIN - NO LICENSE, NO WARRANTY**

This is free public domain software for the public good of a permacomputer hosted at permacomputer.com - an always-on computer by the people, for the people. One which is durable, easy to repair, and distributed like tap water for machine learning intelligence.

The permacomputer is community-owned infrastructure optimized around four values:

```
TRUTH    - First principles, math & science, open source code freely distributed
FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
LOVE     - Be yourself without hurting others, cooperation through natural law
```

This software contributes to that vision by enabling code execution across all 42 programming languages through a unified interface, accessible to all. Code is seeds to sprout on any abandoned technology.

Learn more: [permacomputer.com](https://www.permacomputer.com)

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a compiled binary, for any purpose, commercial or non-commercial, and by any means.

**NO WARRANTY.** THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.

---

Copyright 2025 [TimeHexOn](https://www.timehexon.com) & [foxhop](https://www.foxhop.net) & [russell@unturf](https://www.unturf.com/software)
