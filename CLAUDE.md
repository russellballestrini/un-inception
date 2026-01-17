# Claude AI Instructions for un-inception

## ⚠️ CRITICAL: NEVER USE RAW LXC COMMANDS

**ALWAYS use `un` CLI commands. NEVER use raw `lxc` commands on production.**

```bash
# ✅ CORRECT - use un commands
un service --list
un service --destroy <id>
un service --execute <id> 'command'

# ❌ FORBIDDEN - raw lxc commands bypass auth and state sync
lxc list / lxc delete / lxc stop / lxc exec
```

On 2026-01-11, raw `lxc delete` destroyed 8 production services causing complete data loss.

## Commit Messages

**NEVER add Claude attribution to commit messages.** No robot emoji, no "Generated with Claude Code", no "Co-Authored-By: Claude". Just write the commit message like a human wrote it.

## Project Overview

UN CLI Inception - The UN CLI written in every language it can execute. 42+ implementations, one unified interface.

### SDK Architecture

**Directory Structure**:
```
clients/
├── c/
│   └── sync/src/un.c, un.h     # REFERENCE IMPL - 6,354 lines (libcurl + libwebsockets)
├── python/
│   ├── sync/src/un.py          # Synchronous (requests) - 2,698 lines
│   └── async/src/un_async.py   # Asynchronous (aiohttp) - 2,333 lines
├── javascript/
│   ├── sync/src/un.js          # Synchronous (https) - 2,307 lines
│   └── async/src/un_async.js   # Asynchronous (fetch) - 2,131 lines
├── go/
│   ├── sync/src/un.go          # Synchronous (net/http) - 2,652 lines
│   └── async/src/un_async.go   # Asynchronous (goroutines) - 3,011 lines
├── java/
│   ├── sync/src/Un.java        # Synchronous (HttpURLConnection) - 3,051 lines
│   └── async/src/UnsandboxAsync.java  # Asynchronous (CompletableFuture) - 2,685 lines
├── ruby/
│   ├── sync/src/un.rb          # Synchronous (net/http) - 2,423 lines
│   └── async/src/un_async.rb   # Asynchronous (Future) - 2,441 lines
├── rust/
│   ├── sync/src/lib.rs         # Synchronous (reqwest blocking) - 3,665 lines
│   └── async/src/lib.rs        # Asynchronous (reqwest + tokio) - 3,375 lines
├── php/
│   ├── sync/src/un.php         # Synchronous (cURL) - 2,818 lines
│   └── async/src/UnsandboxAsync.php  # Asynchronous (Guzzle promises) - 2,457 lines
├── swift/
│   └── sync/src/un.swift       # Synchronous (URLSession) - 1,893 lines
├── CLI_SPEC.md                 # Full CLI specification (all SDKs must match)
└── README.md                   # SDK documentation
```

**Total: ~46,000 lines across 17 SDK files (8 languages + Swift sync)**

**Each SDK is BOTH a library AND a CLI tool** (see `clients/CLI_SPEC.md`):
```bash
# Library usage
python -c "from un import execute_code; print(execute_code('python', 'print(1)'))"

# CLI usage (identical across all languages)
python un.py script.py              # Execute code file
python un.py -s bash 'echo hello'   # Inline code
python un.py session --tmux         # Interactive session
python un.py service --list         # Manage services
```

**Every SDK implements**:
- **43+ API functions** (execute, jobs, sessions, services, snapshots, utilities)
- **Full CLI** (execute, session, service, service env, snapshot, key subcommands)
- **4-tier credential resolution** (args > env > ~/.unsandbox/accounts.csv > ./accounts.csv)
- **HMAC-SHA256 request signing**
- **1-hour languages cache** (~/.unsandbox/languages.json)

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

### Local Testing

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

# Run client-specific tests (after migration to clients/)
make test-python
make test-go
make test-javascript
```

### CI Testing Strategy (Smart Detection)

When changes are pushed:
1. **Change Detection** - `detect-changes.sh` identifies which files changed
2. **Per-Language Tests** - Only language tests for CHANGED clients run (e.g., modify clients/python/ → pytest runs)
3. **Cross-Language Tests** - All affected clients validated against API
4. **Science Jobs** - Pool burning with real workloads

**Example**: Modify `clients/python/un.py`:
```
✓ Python pytest runs
✓ Python type checking (if applicable)
✓ Python integration tests with API
✓ Python embedding tests (can import in other code)
⚠ Go, Ruby, JavaScript tests SKIP (unchanged)
```

See **TESTING-STRATEGY.md** for complete testing matrix.

## Common Test Fixes

### Bash arithmetic in `set -e` mode
The pattern `((VAR++))` returns exit code 1 when VAR is 0. Use `VAR=$((VAR + 1))` instead.

### Script directory detection
- **Lua**: `arg[0]:match("(.*/)") or "./"`
- **TypeScript**: `path.dirname(process.argv[1] || __filename)`
- **Bash**: `SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"`

### Shebang lines
Shebang MUST be on line 1, not buried in license headers.

## Keeping Implementations in Sync

**CRITICAL: ALL 38 implementations must have feature parity.**

When adding a new feature to the CLI (e.g., new flag, new command):
1. Update the canonical C implementation at `~/git/unsandbox.com/cli/un.c`
2. Update ALL 38 implementations in this repo - not just "main" ones, ALL of them
3. Use the Task agent to batch update if needed

Current implementations (ALL must be updated):
```
un.awk un.clj un.cob un.cpp un.cr un.d un.dart un.erl un.ex un.f90
un.forth un.fs un.go un.groovy un.hs un.jl un.js un.kt un.lisp un.lua
un.m un.ml un.nim un.php un.pl un.pro un.ps1 un.py un.r un.raku
un.rb un.rs un.scm un.sh un.tcl un.ts un.v un.zig
```

### Feature Checklist

Each implementation must support:
- **Execute**: `un file.py` - run code with `-e ENV=val`, `-f FILE`, `-n MODE`, `-a` artifacts
- **Session**: `un session` - interactive shell with `-f FILE`, `--tmux`, `--screen`, `--list`, `--attach`, `--kill`
- **Service**: `un service` - persistent services with `-f FILE`, `--name`, `--ports`, `--bootstrap`, `--bootstrap-file`, `--list`, `--info`, `--logs`, `--destroy`

The `-f FILE` flag must work for ALL three commands (execute, session, service) - files go to `/tmp/` in the container.

## Git Remotes & Mirroring

This repo pushes to multiple remotes automatically. No git hooks needed - just uses git's built-in multi-push URL feature.

**Remotes:**
- `origin` - Primary (fetches from unturf, pushes to both)
- `github` - Public mirror at GitHub

**Configuration:**
```
origin	ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git (fetch)
origin	ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git (push)
origin	git@github.com:russellballestrini/un-inception.git (push)
github	git@github.com:russellballestrini/un-inception.git (fetch/push)
```

**Usage:**
- `git push origin` - Pushes to both unturf AND GitHub automatically
- `git push github` - Pushes only to GitHub (if needed)
- `git pull` - Pulls from unturf (origin)

**To replicate this setup:**
```bash
git remote add github git@github.com:russellballestrini/un-inception.git
git remote set-url --add --push origin ssh://git@git.unturf.com:2222/engineering/unturf/un-inception.git
git remote set-url --add --push origin git@github.com:russellballestrini/un-inception.git
```

## SDK Testing Philosophy

**SDKs are LIBRARIES for embedding in other people's code.** They are NOT just CLIs.

### Three Testing Levels (ALL REQUIRED)

| Level | What It Tests | How |
|-------|--------------|-----|
| **Unit** | Exported library functions | Test actual exports in native language. NO MOCKING. NO RE-IMPLEMENTING. |
| **Integration** | SDK components work together | Internal SDK tests (auth + request + response parsing) |
| **Functional** | Real API lifecycle | Actually call api.unsandbox.com - execute, sessions, services |

### CRITICAL: No Mocking or Local Re-implementation

**FORBIDDEN**: Re-implementing functions locally to "test" them.

```c
// ❌ WRONG - test_library.c re-implements SHA-256 locally
static void sha256_transform(...) { /* local copy */ }
void test_sha256() { /* tests local copy, not actual SDK */ }

// ✅ CORRECT - test actual exported SDK functions
#include "un.h"
void test_sha256() {
    // Call the REAL exported function from un.c
    char *result = unsandbox_hmac_sign("key", "message");
    assert(strcmp(result, expected) == 0);
    free(result);
}
```

### SDK Export Requirements

Each SDK MUST export functions that can be:
1. **Imported** - Other code can `import`/`require`/`use` the SDK
2. **Tested** - Unit tests can call exported functions directly
3. **Documented** - Public API is clear and documented

**Example (C SDK)**:
```c
// un.h declares public API
unsandbox_result_t *unsandbox_execute(const char *lang, const char *code, ...);
char *unsandbox_hmac_sign(const char *secret, const char *message);

// un.c implements with NON-STATIC functions (when built as library)
#ifndef UNSANDBOX_CLI_ONLY
unsandbox_result_t *unsandbox_execute(...) { /* real implementation */ }
char *unsandbox_hmac_sign(...) { /* real implementation */ }
#endif
```

### Test File Structure

```
clients/{language}/
├── src/           # Source files
├── tests/
│   ├── unit/      # Unit tests - test exported functions
│   ├── integration/  # Integration tests - SDK internal consistency
│   └── functional/   # Functional tests - real API calls
├── Makefile       # Build + test targets
└── README.md
```

### Makefile Test Targets

Every client Makefile MUST have:
```makefile
test: test-cli test-library test-integration test-functional

test-cli:          # CLI binary works (--help, --version)
test-library:      # Unit tests of exported library functions
test-integration:  # SDK internal consistency tests
test-functional:   # Real API calls (requires UNSANDBOX_* env vars)
```

See **docs/TESTING.md** for complete testing guidelines.

## SDK Migration Status (Updated 2026-01-16)

### Overview

All 42 language implementations have been migrated to `clients/`. The C implementation (`clients/c/src/un.c`) is our **north star** - all other SDKs should match its CLI and library API.

### Migration Progress

| Language | Location | Status |
|----------|----------|--------|
| **C** | `clients/c/src/` | Reference impl - CLI + full library API (43 functions) |
| **Python** | `clients/python/sync/src/` | Complete - full CLI + library |
| **Go** | `clients/go/sync/src/` | Complete - full CLI + library |
| **JavaScript** | `clients/javascript/sync/src/` | Complete - full CLI + library |
| **Java** | `clients/java/sync/src/` | Complete - full CLI + library |
| **Ruby** | `clients/ruby/sync/src/` | Complete - full CLI + library |
| **Rust** | `clients/rust/sync/src/` | Complete - full CLI + library |
| **PHP** | `clients/php/sync/src/` | Complete - full CLI + library |
| **TypeScript** | `clients/typescript/sync/src/` | CLI only |
| **+33 more** | `clients/*/sync/src/` | CLI implementations |

**Total: 42 languages in `clients/`, 1 missing (scala)**

### C SDK Library API Status (un.h) - COMPLETE

The C SDK implements **43+ library functions** with full JSON parsing:

**Execution (7 functions):**
- ✅ `unsandbox_execute()` - Synchronous code execution
- ✅ `unsandbox_execute_async()` - Async execution, returns job_id
- ✅ `unsandbox_wait_job()` - Poll job until complete
- ✅ `unsandbox_get_job()` - Get job status
- ✅ `unsandbox_cancel_job()` - Cancel running job
- ✅ `unsandbox_list_jobs()` - List all jobs
- ✅ `unsandbox_get_languages()` - Get available languages

**Sessions (9 functions):**
- ✅ `unsandbox_session_list()` - List sessions
- ✅ `unsandbox_session_get()` - Get session details
- ✅ `unsandbox_session_create()` - Create new session
- ✅ `unsandbox_session_destroy/freeze/unfreeze/boost/unboost()`
- ✅ `unsandbox_session_execute()` - Run command in session

**Services (17 functions):**
- ✅ `unsandbox_service_list()` - List services
- ✅ `unsandbox_service_get()` - Get service details
- ✅ `unsandbox_service_create()` - Create service
- ✅ `unsandbox_service_execute()` - Run command in service
- ✅ `unsandbox_service_env_get/set/delete/export()` - Env vault
- ✅ `unsandbox_service_destroy/freeze/unfreeze/lock/unlock/redeploy/resize()`

**Snapshots (9 functions):**
- ✅ `unsandbox_snapshot_list()` - List snapshots
- ✅ `unsandbox_snapshot_get()` - Get snapshot details
- ✅ `unsandbox_snapshot_session/service()` - Create snapshots
- ✅ `unsandbox_snapshot_restore/delete/lock/unlock/clone()`

**Images (15 functions) - NEW:**
- ✅ `unsandbox_image_list()` - List images
- ✅ `unsandbox_image_get()` - Get image details
- ✅ `unsandbox_image_publish()` - Publish from service/snapshot
- ✅ `unsandbox_image_delete/lock/unlock()`
- ✅ `unsandbox_image_set_visibility()` - private/unlisted/public
- ✅ `unsandbox_image_grant_access/revoke_access/list_trusted()`
- ✅ `unsandbox_image_transfer/spawn/clone()`

**Utilities:**
- ✅ `unsandbox_hmac_sign()` - HMAC-SHA256 signing
- ✅ `unsandbox_validate_keys()` - Key validation
- ✅ `unsandbox_detect_language()` - File extension detection
- ✅ `unsandbox_version()` - Version string
- ✅ `unsandbox_health_check()` - API health check
- ✅ `unsandbox_last_error()` - Thread-local error storage
- ✅ All `unsandbox_free_*()` memory management functions

### Functional Test Results (C SDK)

```
Tests Passed: 28/30
- Execute: PASS
- Languages: PASS (42 languages)
- Sessions: PASS (list, create, destroy)
- Services: PASS (list)
- Snapshots: PASS (list)
- Images: PASS (list)
```

### Directory Structure

```
clients/
├── {language}/
│   ├── sync/src/       # Synchronous implementation
│   ├── async/src/      # Async implementation (some languages)
│   ├── tests/          # Test files
│   └── Makefile        # Build + test targets
```

### Next Steps

1. **Sync other SDKs** - Ensure Python, Go, JS, etc. have library APIs matching C's 43 functions
2. **Add functional tests** - Each SDK needs functional test coverage
3. **Create scala SDK** - Only missing language

2. **Add Images API to un.h** - The CLI supports images but library API doesn't expose them

3. **Migrate next language** - Use this template:
   ```bash
   mkdir -p clients/{lang}/sync/src clients/{lang}/async/src
   # Copy from root, update paths, add Makefile
   ```

4. **Test with inception pattern**:
   ```bash
   # Test any SDK through unsandbox itself
   un -n semitrusted -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
      -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
      clients/python/sync/src/un.py --help
   ```

## Related Repos

- `~/git/unsandbox.com/` - Portal (contains un.c CLI at cli/un.c)
- `~/git/api.unsandbox.com/` - API server
- https://github.com/russellballestrini/un-inception - Public GitHub mirror
