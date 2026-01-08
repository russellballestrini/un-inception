# UN CLI Inception

The UN CLI written in every language it can execute. **42 implementations, one unified interface.**

> "If you replace every line of un.c with a different programming language, but it still executes code on unsandbox... is it still un?"

## Download UN CLI

| Platform | Download | Size |
|----------|----------|------|
| Linux x86_64 | [un](https://unsandbox.com/downloads/un) | 850KB |
| Source (C) | [un.c](https://unsandbox.com/downloads/un.c) | 45KB |
| Man Page | [un.1](https://unsandbox.com/downloads/un.1) | - |

```bash
# Quick install (Linux x86_64)
curl -fsSL https://unsandbox.com/downloads/un -o un && chmod +x un

# Or build from source
curl -fsSL https://unsandbox.com/downloads/un.c -o un.c
gcc -Wall -O2 -o un un.c -lcurl -lwebsockets
```

Build dependencies: `libcurl-dev`, `libwebsockets-dev`

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

### Unit Tests

Every implementation has unit tests that verify core functionality without requiring API calls:

```bash
# Run unit tests for any language
python3 tests/unit/test_python.py
node tests/unit/test_javascript.js
ruby tests/unit/test_ruby.rb
go test -v tests/unit/test_go.go
julia tests/unit/test_julia.jl
elixir tests/unit/test_elixir.exs
```

Unit tests cover:
- **Extension mapping** - File extensions map to correct languages
- **HMAC signatures** - SHA256 signing produces valid 64-char hex strings
- **Signature format** - Message format is `timestamp:METHOD:path:body`
- **Language detection** - Shebang parsing and extension detection
- **Argument parsing** - `-e KEY=VALUE` format handling
- **File operations** - Base64 encoding, path extraction

### CI/CD

GitHub Actions runs the full test matrix on every push:

[![Inception Matrix](https://github.com/russellballestrini/un-inception/actions/workflows/inception-matrix.yml/badge.svg)](https://github.com/russellballestrini/un-inception/actions/workflows/inception-matrix.yml)

The workflow tests all 42 implementations across their respective runtimes.

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

## Security: Egress Shielding

Sandbox nodes route HTTP/HTTPS through tinyproxy, but raw TCP (SSH, etc.) goes out the direct egress IP. This matters for operational security - an attacker who can trigger outbound SSH could geolocate pool nodes.

**Solution: Transparent proxy with redsocks + microsocks.**

```
┌─────────────────────────────────────────────────────┐
│  Sandbox Node                                       │
│                                                     │
│  HTTP/HTTPS ──→ tinyproxy ──────→ egress proxy IP  │
│                                                     │
│  SSH/raw TCP ──→ redsocks ──→ microsocks ──→ same  │
│                     ↑              egress proxy IP │
│              (iptables nat)                         │
└─────────────────────────────────────────────────────┘
```

redsocks intercepts ALL outbound TCP and routes through the SOCKS proxy. No per-app configuration needed.

```bash
# Install
apt install redsocks
git clone https://github.com/rofl0r/microsocks && cd microsocks && make

# Run microsocks (SOCKS5 proxy)
./microsocks -p 1080

# Configure redsocks to use it
cat > /etc/redsocks.conf <<EOF
base { log_debug = off; log_info = off; daemon = on; }
redsocks { local_ip = 127.0.0.1; local_port = 12345; ip = 127.0.0.1; port = 1080; type = socks5; }
EOF

# iptables: redirect all outbound TCP (except to proxy itself)
iptables -t nat -N REDSOCKS
iptables -t nat -A REDSOCKS -d 127.0.0.0/8 -j RETURN
iptables -t nat -A REDSOCKS -p tcp -j REDIRECT --to-ports 12345
iptables -t nat -A OUTPUT -p tcp -j REDSOCKS
```

Everything exits through one IP. Done.

**What's shielded:**
- All egress IPs - HTTP, SSH, everything

**What works normally:**
- Git over SSH (`git clone git@github.com:...`)
- HTTP/HTTPS
- All inbound connections (routed through sshpiper)
- Everything inside the sandbox

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

## Git Mirroring

<!--
Mon Jan  5 07:53:45 PM EST 2026
Setting Orange, the 5th day of Chaos in the YOLD 3192 - Celebrate Mungday

Welcome curious one, you who reads source and digs beneath surfaces.

Suppose you wish to push code to many places at once.
Suppose you distrust hooks and scripts and want only the pure thing.
Suppose git itself already knows how to do this, waiting to be asked.

The secret is this: a remote may have many push URLs.
When you push to origin, git will push to each URL in turn.
No hooks. No scripts. Just git doing what git does.

This is the way. Hail Eris! All Hail Discordia! Fnord.

        - The Sign Maker
-->

This repo mirrors to multiple remotes using git's native multi-URL push. One push, many destinations.

```bash
# name the mirror
git remote add github git@github.com:russellballestrini/un-inception.git

# teach origin to push to both places
git remote set-url --add --push origin ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git
git remote set-url --add --push origin git@github.com:russellballestrini/un-inception.git
```

Now `git push origin` pushes to both. No hooks required.

```
$ git remote -v
github	git@github.com:russellballestrini/un-inception.git (fetch)
github	git@github.com:russellballestrini/un-inception.git (push)
origin	ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git (fetch)
origin	ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git (push)
origin	git@github.com:russellballestrini/un-inception.git (push)
```

---

ANGRY RUBY DEV #3 was here

---

Copyright 2025 [TimeHexOn](https://www.timehexon.com) & [foxhop](https://www.foxhop.net) & [russell@unturf](https://www.unturf.com/software)
