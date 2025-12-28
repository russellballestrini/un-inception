# Claude AI Instructions for un-inception

## Project Overview

UN CLI Inception - The UN CLI written in every language it can execute. 42 implementations, one unified interface.

## Authentication

**HMAC Authentication** (current):
```bash
export UNSANDBOX_PUBLIC_KEY="unsb-pk-xxxx-xxxx-xxxx-xxxx"
export UNSANDBOX_SECRET_KEY="unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx"
```

The auth pattern for all implementations:
- `Authorization: Bearer {public_key}`
- `X-Timestamp: {unix_seconds}`
- `X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")`

Legacy `UNSANDBOX_API_KEY` is still supported as fallback.

### HMAC Dependencies

Each implementation needs HMAC-SHA256 capability:

| Language | Dependency | Install |
|----------|------------|---------|
| Python | `hmac`, `hashlib` | Built-in |
| JavaScript/TS | `crypto` | Built-in (Node.js) |
| Ruby | `openssl` | Built-in |
| Go | `crypto/hmac` | Built-in |
| PHP | `hash_hmac()` | Built-in |
| Perl | `Digest::SHA` | Core module |
| Lua | `openssl` CLI | `apt install openssl` |
| Bash | `openssl` CLI | `apt install openssl` |
| Rust | `hmac`, `sha2` | `cargo add hmac sha2` |
| C/C++ | OpenSSL | `apt install libssl-dev` + `-lssl -lcrypto` |
| Java | `javax.crypto` | Built-in |
| C# | `System.Security.Cryptography` | Built-in |
| Haskell | `cryptonite` | `cabal install cryptonite` |
| Clojure | `buddy-core` | Add to deps.edn |
| Erlang/Elixir | `:crypto` | OTP built-in |
| Julia | `SHA` | Built-in |
| R | `openssl` | `install.packages("openssl")` |

**Note**: Languages without native HMAC (Lua, Bash, AWK, Forth) shell out to `openssl dgst -sha256 -hmac`.

## The Inception Matrix - Testing Languages Without Local Interpreters

**CRITICAL INSIGHT**: Use `un` (the C implementation) to run tests for languages not installed locally!

If a language isn't available on the local machine (e.g., PHP, Julia, Haskell), run the UN implementation through unsandbox itself:

```bash
# Key flags:
# -n semitrusted  = allow network access so inner script can call API
# -e KEY=VALUE    = pass API keys to inner script

# Don't have PHP installed? Run un.php through unsandbox!
un -n semitrusted -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY un.php test/fib.py

# Don't have Julia? Run un.jl through unsandbox!
un -n semitrusted -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY un.jl test/fib.py
```

This is the **inception** - using un to run un to run code. Each layer executes through unsandbox's remote execution API.

### Inception Test Matrix

To test ALL 42 implementations regardless of local interpreters:

```bash
# Use un (C implementation) to test all others through unsandbox
for impl in un.py un.js un.rb un.go un.php un.pl un.lua; do
    echo "Testing $impl..."
    un -n semitrusted \
       -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
       -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
       "$impl" test/fib.py
done
```

The test suite in `tests/run_all_tests.sh` currently skips languages without local interpreters. Use the inception pattern with `un` to achieve 100% test coverage.

## Directory Structure

- `un.*` - 42 UN CLI implementations (un.py, un.js, un.rb, un.go, etc.)
- `tests/` - Test suites for each implementation
- `test/` - Shared test files (fib.py, fib.sh, etc.)

## Running Tests

```bash
# Set auth
export UNSANDBOX_PUBLIC_KEY="unsb-pk-zhi3-b6cv-jvqc-uven"
export UNSANDBOX_SECRET_KEY="unsb-sk-z4a93-a33xy-7u7eh-pngpg"

# Run all available tests
./tests/run_all_tests.sh

# Run individual test
python3 tests/test_un_py.py
lua tests/test_un_lua.lua
bash tests/test_un_sh.sh
```

## Common Test Fixes

### Bash arithmetic in `set -e` mode
The pattern `((VAR++))` returns exit code 1 when VAR is 0. Use `VAR=$((VAR + 1))` instead.

### Script directory detection
- **Lua**: `arg[0]:match("(.*/)") or "./"`
- **TypeScript**: `path.dirname(process.argv[1] || __filename)`
- **Bash**: `SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"`

### Shebang lines
Shebang MUST be on line 1, not buried in license headers.

## Related Repos

- `~/git/unsandbox.com/` - Portal (contains un.c CLI at cli/un.c)
- `~/git/api.unsandbox.com/` - API server
