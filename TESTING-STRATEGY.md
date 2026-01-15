# Testing Strategy for Multi-Language UN Clients

## Overview

UN Inception implements the UN CLI in 42+ languages. Each implementation serves as:
1. **Standalone CLI** - Full featured command-line tool
2. **Client Library** - Importable SDK for other projects
3. **Embeddable** - Can be bundled/copied into other projects

**Goal**: Ensure all clients work correctly as the codebase evolves while only running necessary tests.

---

## Smart Change Detection

The CI pipeline uses **smart change detection** to run only relevant tests:

### How it Works

1. **Pre-stage: detect-changes.sh**
   - Compares HEAD with origin/main
   - Identifies which languages/clients changed
   - Generates `changes.json` with affected languages

2. **Pre-stage: generate-matrix.sh**
   - Reads `changes.json`
   - Generates `test-matrix.yml` with only needed test jobs
   - Includes cross-language validation for all clients

3. **Test Stage**
   - Dynamic job inclusion from `test-matrix.yml`
   - Only language-specific tests run when that language changed
   - Always includes integration tests (API contract validation)

### Example: Changes in clients/python/

```json
{
  "changed_clients": ["python"],
  "root_implementations": [],
  "test_matrix": {
    "python": {
      "test_script": "pytest tests/test_un_py.py -v",
      "environment": ["python3"],
      "should_run": true
    },
    "integration": {
      "test_script": "bash tests/integration-all-clients.sh",
      "should_run": true
    }
  }
}
```

### Supported Change Patterns

| Changed File | Triggered Tests |
|---|---|
| `clients/python/*` | Python tests + integration |
| `clients/go/*` | Go tests + integration |
| `clients/javascript/*` | Node.js/TypeScript tests + integration |
| `clients/*/tests/*` | Language tests + integration |
| `test/` (shared tests) | All language tests + integration |
| `un.c` (root) | C tests + integration + all clients (fundamental change) |
| `un.py` (root) | Python tests + integration |
| `CLAUDE.md` | Documentation validation only |

---

## Testing Matrix by Language

### Tier 1: Compiled Languages (Fast)

These languages have fast compilation and test cycles.

#### Go
- **File**: `clients/go/un.go`
- **Tests**: `go test ./...`
- **Integration**: `bash tests/integration-go-client.sh`
- **Time**: ~5 seconds

```bash
cd clients/go
go test -v
go run un.go test/fib.py
```

#### Rust
- **File**: `clients/rust/un.rs`
- **Tests**: `cargo test`
- **Integration**: `bash tests/integration-rust-client.sh`
- **Time**: ~10 seconds (cached)

```bash
cd clients/rust
cargo test --release
cargo run -- test/fib.py
```

#### C/C++
- **File**: `clients/c/un.c` OR `clients/cpp/un.cpp`
- **Build**: `gcc un.c -o un -lssl -lcrypto` OR `g++ un.cpp -o un`
- **Tests**: `bash tests/test_un_c.sh`
- **Integration**: `./un test/fib.py`
- **Time**: ~2 seconds

#### Java
- **File**: `clients/java/Un.java`
- **Compile**: `javac Un.java`
- **Tests**: `java -cp . Un test/fib.py`
- **Integration**: Full feature test suite
- **Time**: ~8 seconds (warm JVM)

### Tier 2: Interpreted Languages (Medium)

These have runtime interpretation but fast test cycles.

#### Python
- **File**: `clients/python/un.py`
- **Tests**:
  ```bash
  pytest tests/test_un_py.py -v
  python3 -m py_compile clients/python/un.py  # Syntax check
  python3 clients/python/un.py test/fib.py   # Integration
  ```
- **Async Support**: Tests both sync and async client modes
- **Time**: ~3 seconds

#### JavaScript/TypeScript
- **File**: `clients/javascript/un.js` (Node.js)
- **Tests**:
  ```bash
  node tests/test_un_js.js
  npx eslint clients/javascript/un.js  # Linting
  node clients/javascript/un.js test/fib.py
  ```
- **Time**: ~2 seconds

#### Ruby
- **File**: `clients/ruby/un.rb`
- **Tests**:
  ```bash
  ruby -w clients/ruby/un.rb test/fib.py  # Syntax + execution
  ruby tests/test_un_rb.rb
  ```
- **Time**: ~2 seconds

#### PHP
- **File**: `clients/php/un.php`
- **Tests**:
  ```bash
  php -l clients/php/un.php  # Syntax check
  php clients/php/un.php test/fib.py  # Integration
  php -d display_errors=1 tests/test_un_php.php
  ```
- **Time**: ~2 seconds

#### Bash/Shell
- **File**: `clients/bash/un.sh`
- **Tests**:
  ```bash
  bash -n clients/bash/un.sh  # Syntax check
  shellcheck clients/bash/un.sh  # Linting
  bash clients/bash/un.sh test/fib.py
  ```
- **Time**: ~1 second

### Tier 3: Languages Requiring Inception

These languages may not be installed locally. Use the **Inception Pattern** to test via unsandbox itself.

#### Haskell, Julia, Clojure, Erlang, etc.
- **Pattern**: Use `un` (C implementation) to execute the language implementation through unsandbox
- **Command**:
  ```bash
  un -n semitrusted \
     -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
     -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
     clients/haskell/un.hs test/fib.py
  ```
- **Time**: ~6 seconds (network + unsandbox execution)

---

## Test Categories

### 1. Unit Tests
**Test**: Language-specific syntax and core logic

```bash
# Each language tests its specific features
pytest tests/test_un_py.py
go test ./clients/go/...
cargo test -p un_client
ruby tests/test_un_rb.rb
```

**What fails here**: Syntax errors, missing dependencies, core logic bugs

### 2. Integration Tests (API Contract)
**Test**: All clients communicate correctly with the API

```bash
# Test EVERY client can:
# ✓ Authenticate (HMAC signature generation)
# ✓ Execute code (run test/fib.py via API)
# ✓ Handle responses
# ✓ Respect error codes

bash tests/integration-all-clients.sh
```

**What fails here**: Authentication issues, API schema changes, network problems

### 3. Embedding Tests
**Test**: Client can be imported/embedded in other code

```python
# Python: Can import the client
from clients.python.un import UnsandboxClient
client = UnsandboxClient()
```

```javascript
// JavaScript: Can require the module
const { UnsandboxClient } = require('./clients/javascript/un.js');
```

**What fails here**: Export/import structure, module API incompatibility

### 4. Parity Tests (CI/Feature Checklist)
**Test**: All clients have feature parity

Verify all 42+ clients support:
- ✓ Execute: `un file.py`
- ✓ Session: `un session`
- ✓ Service: `un service`
- ✓ All flags: `-e`, `-f`, `-n`, `-a`, `--tmux`, `--screen`, etc.

```bash
bash tests/feature-parity-matrix.sh
```

**What fails here**: Missing features, argument parsing bugs, logic differences

---

## CI Pipeline Structure

### Stage: Pre (Change Detection)
```
detect-changes.sh     → changes.json
generate-matrix.sh    → test-matrix.yml (dynamic jobs)
```

### Stage: Build
```
build-clients.sh      → Compile/prepare all changed clients
```

### Stage: Test (Dynamic)
```
include: test-matrix.yml  ← Dynamically generated per-language jobs
  test-python.sh        (only if clients/python/* changed)
  test-go.sh           (only if clients/go/* changed)
  test-javascript.sh   (only if clients/javascript/* changed)
  ...
  integration-all.sh   (always runs - validates all clients)
```

### Stage: Science (Pool Burning)
```
lint-all-sdks.sh      → Code quality for all clients
benchmark-clients.sh  → Performance comparison
validate-examples.sh  → Real-world examples work
```

### Stage: Validate & Report
```
Summary of test results, coverage, and quality metrics
```

---

## Local Development

### Running All Tests Locally

```bash
# Set environment
export UNSANDBOX_PUBLIC_KEY="unsb-pk-..."
export UNSANDBOX_SECRET_KEY="unsb-sk-..."

# Run everything
./tests/run_all_tests.sh

# Or run specific language
make test-python
make test-go
make test-javascript
make test-ruby
make test-php
```

### Testing a Specific Client After Changes

```bash
# After editing clients/python/un.py
cd clients/python
pytest -v
python3 -m py_compile un.py
python3 un.py test/fib.py
python3 un.py session  # Interactive test
python3 un.py service --help
```

### Testing Without Local Interpreter (Inception)

```bash
# Don't have Haskell installed? Test it via unsandbox
un -n semitrusted \
   -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
   -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
   clients/haskell/un.hs test/fib.py

# Test ALL languages via inception (if needed)
bash tests/inception-test-all.sh
```

---

## Common Failures & Fixes

### Python Tests Fail
```
❌ ModuleNotFoundError: No module named 'X'
✓ Fix: pip install -r clients/python/requirements.txt

❌ HMAC signature mismatch
✓ Fix: Verify authentication code in un.py matches other clients

❌ Import error when embedded
✓ Fix: Ensure no absolute imports, use relative imports for bundling
```

### Go Tests Fail
```
❌ go.mod not found
✓ Fix: cd clients/go && go mod init clients/go

❌ Module not found after changes
✓ Fix: go mod tidy

❌ Compilation error with crypto
✓ Fix: Ensure Go 1.16+ (crypto/hmac in stdlib)
```

### JavaScript Tests Fail
```
❌ Cannot find module
✓ Fix: npm install in clients/javascript

❌ Async/await not working
✓ Fix: Ensure Node.js version supports async/await (>7.6.0)
```

### Integration Fails (All Languages)
```
❌ 401 Unauthorized
✓ Fix: HMAC signature generation - compare with C reference (un.c)

❌ 503 API unavailable
✓ Fix: Check api.unsandbox.com is up and accessible

❌ Client hangs/timeout
✓ Fix: Check network access, proxy settings, firewall rules
```

---

## Rollout Schedule

| Phase | Goal | Timeline |
|---|---|---|
| Phase 1 (Now) | Setup clients/ structure, smart CI | This week |
| Phase 2 | Migrate top 10 languages to clients/ | 2 weeks |
| Phase 3 | Migrate remaining 32 languages | 4 weeks |
| Phase 4 | Deprecate root un.* files | 8 weeks |
| Phase 5 | Archive/document legacy implementations | Optional |

---

## Metrics & Coverage

**Success Criteria**:
- ✓ All test jobs pass
- ✓ Each language has unit tests
- ✓ Each language has integration tests
- ✓ Feature parity matrix 100% (all 42 features in all 42 languages)
- ✓ CI matrix correctly detects changes and skips irrelevant tests
- ✓ No false positives (unrelated changes don't trigger irrelevant tests)

**Tracking**:
- Build time by language (identify slow tests)
- Test coverage by client
- Feature parity matrix (visual dashboard)
- Inception test success rate (languages tested via unsandbox)
