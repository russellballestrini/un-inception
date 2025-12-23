# UN CLI Inception Test Suite

Comprehensive tests for all UN CLI implementations in the inception directory.

## Overview

Each test file validates three critical aspects:

1. **Unit Tests** - Extension detection logic (11 extensions)
2. **Integration Tests** - API connectivity (requires UNSANDBOX_API_KEY)
3. **Functional Tests** - End-to-end execution using fib.go

## Quick Start

### Prerequisites

1. Set your API key:
   ```bash
   export UNSANDBOX_API_KEY="your-key-here"
   ```

2. Build the UN CLI implementation you want to test (from parent directory):
   ```bash
   cd /home/fox/git/unsandbox.com/cli/inception

   # Go
   go build -o un_go un.go

   # Rust
   rustc un.rs -o un_rust

   # C
   gcc -o un_c un_inception.c -lcurl

   # C++
   g++ -o un_cpp un.cpp -lcurl

   # D
   dmd un.d -of=un_d

   # Zig
   zig build-exe un.zig -O ReleaseFast -femit-bin=un

   # Nim
   nim c -d:release un.nim

   # V
   v un.v -o un_v
   ```

## Running Tests

### Go Tests

```bash
cd /home/fox/git/unsandbox.com/cli/inception/tests
go build -o test_un_go test_un_go.go
./test_un_go
```

**Expected Output:**
```
UN CLI Go Implementation Test Suite
====================================

=== Test 1: Extension Detection ===
  PASS: script.py -> python
  PASS: app.js -> javascript
  PASS: main.go -> go
  ...
Extension Detection: 11 passed, 0 failed

=== Test 2: API Connection ===
  PASS: API connection successful
API Connection: passed

=== Test 3: Functional Test (fib.go) ===
  PASS: fib.go executed successfully
  Output: fib(10) = 55
Functional Test: passed

====================================
RESULT: ALL TESTS PASSED
```

### Rust Tests

**Note:** Requires dependencies. If using standalone compilation:

```bash
# Standalone (may fail on API test without reqwest/serde_json)
rustc test_un_rs.rs -o test_un_rs
./test_un_rs
```

For full functionality, create a Cargo.toml in tests directory:

```toml
[package]
name = "test_un_rs"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "test_un_rs"
path = "test_un_rs.rs"

[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
serde_json = "1.0"
```

Then run:
```bash
cargo build --release
./target/release/test_un_rs
```

### C Tests

```bash
gcc -o test_un_c test_un_c.c -lcurl
./test_un_c
```

### C++ Tests

```bash
g++ -o test_un_cpp test_un_cpp.cpp -lcurl
./test_un_cpp
```

### D Tests

```bash
dmd test_un_d.d -of=test_un_d
./test_un_d
```

Or with LDC2:
```bash
ldc2 test_un_d.d -of=test_un_d
./test_un_d
```

### Zig Tests

```bash
zig build-exe test_un_zig.zig -O ReleaseFast
./test_un_zig
```

### Nim Tests

```bash
nim c -d:release test_un_nim.nim
./test_un_nim
```

### V Tests

```bash
v test_un_v.v -o test_un_v
./test_un_v
```

## Test Behavior

### Without UNSANDBOX_API_KEY

Tests will skip API-dependent tests:

```
=== Test 2: API Connection ===
  SKIP: UNSANDBOX_API_KEY not set
API Connection: skipped

=== Test 3: Functional Test (fib.go) ===
  SKIP: UNSANDBOX_API_KEY not set
Functional Test: skipped
```

Exit code: **0** (skipped tests still pass)

### Without Binary Built

If the UN CLI binary doesn't exist:

```
=== Test 3: Functional Test (fib.go) ===
  SKIP: ../un_go binary not found (run: cd .. && go build -o un_go un.go)
Functional Test: skipped
```

Exit code: **0** (skipped tests still pass)

### Test Failures

Any actual test failure will exit with code **1**:

```
=== Test 1: Extension Detection ===
  FAIL: app.cpp -> got rust, expected cpp
Extension Detection: 10 passed, 1 failed

====================================
RESULT: SOME TESTS FAILED
```

Exit code: **1**

## Extension Detection Tests

All tests validate these 11 file extensions:

| Extension | Language   |
|-----------|------------|
| .py       | python     |
| .js       | javascript |
| .go       | go         |
| .rs       | rust       |
| .c        | c          |
| .cpp      | cpp        |
| .d        | d          |
| .zig      | zig        |
| .nim      | nim        |
| .v        | v          |
| .xyz      | (null)     |

## Test File: fib.go

The functional test uses `fib.go`:

```go
package main

import "fmt"

func fib(n int) int {
	if n <= 1 {
		return n
	}
	return fib(n-1) + fib(n-2)
}

func main() {
	result := fib(10)
	fmt.Printf("fib(10) = %d\n", result)
}
```

Expected output: `fib(10) = 55`

## Continuous Integration

To run all tests in CI:

```bash
#!/bin/bash
set -e

export UNSANDBOX_API_KEY="${UNSANDBOX_API_KEY}"

cd /home/fox/git/unsandbox.com/cli/inception/tests

# Build and test Go
echo "Testing Go..."
go build -o test_un_go test_un_go.go && ./test_un_go

# Build and test C
echo "Testing C..."
gcc -o test_un_c test_un_c.c -lcurl && ./test_un_c

# Build and test C++
echo "Testing C++..."
g++ -o test_un_cpp test_un_cpp.cpp -lcurl && ./test_un_cpp

# Build and test D
echo "Testing D..."
dmd test_un_d.d -of=test_un_d && ./test_un_d

# Build and test Zig
echo "Testing Zig..."
zig build-exe test_un_zig.zig -O ReleaseFast && ./test_un_zig

# Build and test Nim
echo "Testing Nim..."
nim c -d:release test_un_nim.nim && ./test_un_nim

# Build and test V
echo "Testing V..."
v test_un_v.v -o test_un_v && ./test_un_v

echo "All tests passed!"
```

## Debugging

### Enable Verbose Output

Most implementations print detailed information by default.

### Check API Response

To manually test the API:

```bash
curl -X POST https://api.unsandbox.com/execute \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $UNSANDBOX_API_KEY" \
  -d '{"language":"python","code":"print(\"Hello\")"}'
```

Expected response:
```json
{
  "stdout": "Hello\n",
  "stderr": "",
  "exit_code": 0
}
```

### Common Issues

1. **Missing libcurl**: Install with `apt install libcurl4-openssl-dev` (Ubuntu/Debian)
2. **Zig version**: Tests require Zig 0.11.0 or newer
3. **D compiler**: Install with `curl -fsS https://dlang.org/install.sh | bash -s dmd`
4. **Nim compiler**: Install with `curl https://nim-lang.org/choosenim/init.sh -sSf | sh`
5. **V compiler**: Install from https://github.com/vlang/v

## Test Coverage

Each test file covers:

- ✅ Extension detection for 10 supported languages
- ✅ Null extension handling
- ✅ HTTP POST to unsandbox API
- ✅ JSON request serialization
- ✅ JSON response parsing
- ✅ Authorization header handling
- ✅ Process execution and output capture
- ✅ Exit code propagation
- ✅ String matching for expected output

## Exit Codes

| Code | Meaning                     |
|------|-----------------------------|
| 0    | All tests passed or skipped |
| 1    | One or more tests failed    |

## Contributing

When adding new language support to UN CLI:

1. Create test file: `test_un_<lang>.<ext>`
2. Copy test structure from existing tests
3. Update this README with compilation instructions
4. Add to CI script if applicable
