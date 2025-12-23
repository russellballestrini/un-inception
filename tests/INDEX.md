# UN CLI Inception Test Suite - Index

## Quick Links

- **[TEST_README.md](TEST_README.md)** - Full documentation, compilation instructions, troubleshooting
- **[SUMMARY.md](SUMMARY.md)** - Overview of all test files and coverage
- **[run_compiled_tests.sh](run_compiled_tests.sh)** - Automated test runner for all languages

## Directory Structure

```
tests/
├── INDEX.md                      # This file - Quick navigation
├── TEST_README.md                # Full documentation
├── SUMMARY.md                    # Overview and summary
├── run_compiled_tests.sh         # Test runner script
│
├── fib.go                        # Test program (Fibonacci)
│
├── test_un_go.go                 # Go tests
├── test_un_rs.rs                 # Rust tests
├── test_un_c.c                   # C tests
├── test_un_cpp.cpp               # C++ tests
├── test_un_d.d                   # D tests
├── test_un_zig.zig               # Zig tests
├── test_un_nim.nim               # Nim tests
└── test_un_v.v                   # V tests
```

## Test Files

| Language | Test File | Lines | Binary Name | Compile Command |
|----------|-----------|-------|-------------|-----------------|
| Go | [test_un_go.go](test_un_go.go) | 209 | `test_un_go` | `go build -o test_un_go test_un_go.go` |
| Rust | [test_un_rs.rs](test_un_rs.rs) | 195 | `test_un_rs` | `rustc test_un_rs.rs -o test_un_rs` |
| C | [test_un_c.c](test_un_c.c) | 220 | `test_un_c` | `gcc -o test_un_c test_un_c.c -lcurl` |
| C++ | [test_un_cpp.cpp](test_un_cpp.cpp) | 210 | `test_un_cpp` | `g++ -o test_un_cpp test_un_cpp.cpp -lcurl` |
| D | [test_un_d.d](test_un_d.d) | 175 | `test_un_d` | `dmd test_un_d.d -of=test_un_d` |
| Zig | [test_un_zig.zig](test_un_zig.zig) | 235 | `test_un_zig` | `zig build-exe test_un_zig.zig -O ReleaseFast` |
| Nim | [test_un_nim.nim](test_un_nim.nim) | 130 | `test_un_nim` | `nim c -d:release test_un_nim.nim` |
| V | [test_un_v.v](test_un_v.v) | 145 | `test_un_v` | `v test_un_v.v -o test_un_v` |

## Test Coverage Matrix

| Test Type | Go | Rust | C | C++ | D | Zig | Nim | V |
|-----------|:--:|:----:|:-:|:---:|:-:|:---:|:---:|:-:|
| Extension Detection (11 tests) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| API Connection | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Functional Test (fib.go) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |

## Quick Start

### 1. Run All Tests

```bash
cd /home/fox/git/unsandbox.com/cli/inception/tests
./run_compiled_tests.sh
```

### 2. Run Single Test (Example: Go)

```bash
cd /home/fox/git/unsandbox.com/cli/inception/tests
go build -o test_un_go test_un_go.go
./test_un_go
```

### 3. With API Key

```bash
export UNSANDBOX_API_KEY="your-key-here"
./test_un_go
```

## What Each Test Does

### 1. Unit Tests - Extension Detection
Tests that the `detectLanguage()` function correctly maps file extensions to language names:
- `.py` → `python`
- `.js` → `javascript`
- `.go` → `go`
- `.rs` → `rust`
- `.c` → `c`
- `.cpp` → `cpp`
- `.d` → `d`
- `.zig` → `zig`
- `.nim` → `nim`
- `.v` → `v`
- `.xyz` → `null` (unknown)

### 2. Integration Tests - API Connection
Tests that the implementation can:
- Create valid JSON requests
- POST to `https://api.unsandbox.com/execute`
- Include proper authorization headers
- Parse JSON responses
- Extract stdout/stderr/exit_code

### 3. Functional Tests - End-to-End
Tests the complete workflow:
1. Read `fib.go` from disk
2. Detect language as "go"
3. Send code to unsandbox API
4. Receive and parse response
5. Verify output contains "fib(10) = 55"

## Test Output Example

```
UN CLI Go Implementation Test Suite
====================================

=== Test 1: Extension Detection ===
  PASS: script.py -> python
  PASS: app.js -> javascript
  PASS: main.go -> go
  PASS: program.rs -> rust
  PASS: code.c -> c
  PASS: app.cpp -> cpp
  PASS: prog.d -> d
  PASS: main.zig -> zig
  PASS: script.nim -> nim
  PASS: app.v -> v
  PASS: unknown.xyz ->
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

## Exit Codes

- **0** - All tests passed (or skipped gracefully)
- **1** - One or more tests failed

## Dependencies

### Required for All Tests
- The UN CLI implementation binary (e.g., `../un_go`)
- Compiler for the test language

### Required for API Tests
- `UNSANDBOX_API_KEY` environment variable
- Internet connection to `api.unsandbox.com`

### Language-Specific
- **C/C++**: libcurl (`apt install libcurl4-openssl-dev`)
- **Rust**: reqwest and serde_json (optional, for API tests)
- **D**: dmd or ldc2
- **Zig**: Zig 0.11.0+
- **Nim**: Nim compiler
- **V**: V compiler

## Files in This Directory

### Test Files (Executable)
All test files are self-contained and can be compiled and run independently.

### Support Files
- **fib.go** - Simple Fibonacci calculator used for functional testing
- **TEST_README.md** - Comprehensive documentation with examples
- **SUMMARY.md** - Quick reference and overview
- **INDEX.md** - This file
- **run_compiled_tests.sh** - Shell script to run all tests

## Contributing

When adding tests for new UN CLI implementations:
1. Follow the structure of existing test files
2. Include all three test types (unit, integration, functional)
3. Add compilation instructions in file header
4. Update this INDEX.md with the new test
5. Update run_compiled_tests.sh to include the new test

## Troubleshooting

See [TEST_README.md](TEST_README.md) for detailed troubleshooting information.

## License

Part of the unsandbox.com project.
