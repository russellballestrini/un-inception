# UN CLI Inception Tests

Comprehensive test suites for the UN CLI implementations in all 42+ languages.

## Quick Start - Master Test Runner

The easiest way to run tests for ALL implementations:

```bash
cd /home/fox/git/unsandbox.com/cli/inception/tests

# Run all tests (unit, integration, functional)
./run_all_tests.sh

# Run only unit tests (no API key required)
./run_all_tests.sh --unit

# Run only integration tests (requires API key)
./run_all_tests.sh --integration

# Run only functional tests (requires API key)
./run_all_tests.sh --functional

# Run multiple test types
./run_all_tests.sh --unit --integration
```

The master test runner:
- Tests all 42 language implementations automatically
- Handles missing interpreters gracefully (skips with warning)
- Provides color-coded summary table
- Shows timing and detailed pass/fail/skip counts
- Exits with code 0 only if ALL tests pass

## Test Files

### Master Test Runner
- `run_all_tests.sh` - Comprehensive test runner for ALL implementations (RECOMMENDED)

### Scripting Languages
- `test_un_sh.sh` - Bash UN CLI tests
- `test_un_tcl.tcl` - TCL UN CLI tests
- `test_un_raku.raku` - Raku UN CLI tests
- `test_un_py.py` - Python UN CLI tests
- `test_un_rb.rb` - Ruby UN CLI tests
- `test_un_pl.pl` - Perl UN CLI tests
- `test_un_lua.lua` - Lua UN CLI tests
- `test_un_php.php` - PHP UN CLI tests
- `test_un_js.js` - JavaScript (Node.js) UN CLI tests
- `test_un_ts.ts` - TypeScript (Node.js) UN CLI tests
- `test_un_deno.ts` - Deno TypeScript UN CLI tests
- `test_un_groovy.groovy` - Groovy UN CLI tests

### Functional Languages
- `test_un_hs.hs` - Haskell UN CLI tests
- `test_un_ml.ml` - OCaml UN CLI tests
- `test_un_clj.clj` - Clojure UN CLI tests
- `test_un_scm.scm` - Scheme (Guile) UN CLI tests
- `test_un_lisp.lisp` - Common Lisp (SBCL) UN CLI tests
- `test_un_erl.erl` - Erlang UN CLI tests
- `test_un_ex.exs` - Elixir UN CLI tests

### Systems Languages
- `test_un_c.c` - C UN CLI tests
- `test_un_cpp.cpp` - C++ UN CLI tests
- `test_un_go.go` - Go UN CLI tests
- `test_un_rs.rs` - Rust UN CLI tests
- `test_un_zig.zig` - Zig UN CLI tests
- `test_un_d.d` - D UN CLI tests
- `test_un_nim.nim` - Nim UN CLI tests
- `test_un_cr.cr` - Crystal UN CLI tests
- `test_un_v.v` - V UN CLI tests
- `test_un_m.sh` - Objective-C UN CLI tests (shell wrapper)

### JVM Languages
- `TestUn.java` - Java UN CLI tests
- `TestUn.cs` - C# UN CLI tests
- `test_un_kt.kt` - Kotlin UN CLI tests
- `test_un_fs.fs` - F# UN CLI tests

### Scientific/Specialized Languages
- `test_un_jl.jl` - Julia UN CLI tests
- `test_un_r.r` - R UN CLI tests
- `test_un_dart.dart` - Dart UN CLI tests
- `test_un_f90.f90` - Fortran UN CLI tests
- `test_un_cob.sh` - COBOL UN CLI tests (shell wrapper)
- `test_un_pro.pro` - Prolog UN CLI tests
- `test_un_forth.fth` - Forth UN CLI tests

## What Each Test Suite Covers

Each test file includes three types of tests:

1. **Unit Tests** - Extension detection logic
   - Tests that 10+ file extensions map to correct language identifiers
   - Ensures `.hs` → `"haskell"`, `.py` → `"python"`, etc.

2. **Integration Tests** - API connectivity
   - Creates a simple test file and runs it through the UN CLI
   - Verifies the CLI can reach `api.unsandbox.com` and execute code
   - Skipped if `UNSANDBOX_API_KEY` environment variable is not set

3. **Functional Tests** - End-to-end execution
   - Runs the corresponding `fib.*` file from `../test/`
   - Verifies output contains `"fib(10) = 55"`
   - Tests the full workflow: file reading → API call → output display
   - Skipped if `UNSANDBOX_API_KEY` environment variable is not set

## Prerequisites

### General
- Set `UNSANDBOX_API_KEY` environment variable to run integration/functional tests
- Run tests from `/home/fox/git/unsandbox.com/cli/inception/` directory

### Language-Specific Dependencies

**Haskell** (`test_un_hs.hs`):
```bash
# Install dependencies
cabal install --lib aeson http-conduit bytestring text

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
./tests/test_un_hs.hs
```

**OCaml** (`test_un_ml.ml`):
```bash
# Install dependencies
opam install cohttp-lwt-unix yojson

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
ocaml tests/test_un_ml.ml
```

**Clojure** (`test_un_clj.clj`):
```bash
# Install Clojure CLI tools
# Dependencies: clj-http, cheshire

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
clj -Sdeps '{:deps {clj-http/clj-http {:mvn/version "3.12.3"} cheshire/cheshire {:mvn/version "5.11.0"}}}' -M tests/test_un_clj.clj
```

**Scheme** (`test_un_scm.scm`):
```bash
# Install Guile and dependencies
sudo apt-get install guile-3.0 guile-json

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
./tests/test_un_scm.scm
```

**Common Lisp** (`test_un_lisp.lisp`):
```bash
# Install SBCL and Quicklisp
# In SBCL: (ql:quickload '(:dexador :jonathan))

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
sbcl --script tests/test_un_lisp.lisp
```

**Erlang** (`test_un_erl.erl`):
```bash
# Install Erlang/OTP (includes inets, ssl)

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
escript tests/test_un_erl.erl
```

**Elixir** (`test_un_ex.exs`):
```bash
# Elixir comes with standard library support

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
elixir tests/test_un_ex.exs
```

**Julia** (`test_un_jl.jl`):
```bash
# Install dependencies
julia -e 'using Pkg; Pkg.add("HTTP"); Pkg.add("JSON")'

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
julia tests/test_un_jl.jl
```

**R** (`test_un_r.r`):
```bash
# Install dependencies
R -e 'install.packages(c("httr", "jsonlite"), repos="https://cran.rstudio.com/")'

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
Rscript tests/test_un_r.r
```

**Crystal** (`test_un_cr.cr`):
```bash
# Crystal stdlib includes HTTP and JSON support

# Run tests (interpreted)
cd /home/fox/git/unsandbox.com/cli/inception/
crystal tests/test_un_cr.cr

# Or compile first for faster execution
crystal build tests/test_un_cr.cr -o test_un_cr
./test_un_cr
```

**Fortran** (`test_un_f90.f90`):
```bash
# Compile with gfortran
cd /home/fox/git/unsandbox.com/cli/inception/
gfortran -o test_un_f90 tests/test_un_f90.f90

# Run tests
./test_un_f90
rm test_un_f90
```

**COBOL** (`test_un_cob.sh`):
```bash
# Install GnuCOBOL
sudo apt-get install gnucobol  # Ubuntu/Debian
# or
sudo dnf install gnucobol      # Fedora

# Run tests (shell wrapper)
cd /home/fox/git/unsandbox.com/cli/inception/
bash tests/test_un_cob.sh
```

**Prolog** (`test_un_pro.pro`):
```bash
# Install SWI-Prolog
sudo apt-get install swi-prolog

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
swipl -g main -t halt tests/test_un_pro.pro
```

**Forth** (`test_un_forth.fth`):
```bash
# Install Gforth
sudo apt-get install gforth

# Run tests
cd /home/fox/git/unsandbox.com/cli/inception/
gforth tests/test_un_forth.fth
```

## Running All Tests

```bash
cd /home/fox/git/unsandbox.com/cli/inception/

# Export API key (required for integration/functional tests)
export UNSANDBOX_API_KEY="your_api_key_here"

# Run each test suite
echo "=== Haskell ==="
./tests/test_un_hs.hs
echo ""

echo "=== OCaml ==="
ocaml tests/test_un_ml.ml
echo ""

echo "=== Clojure ==="
clj -Sdeps '{:deps {clj-http/clj-http {:mvn/version "3.12.3"} cheshire/cheshire {:mvn/version "5.11.0"}}}' -M tests/test_un_clj.clj
echo ""

echo "=== Scheme ==="
./tests/test_un_scm.scm
echo ""

echo "=== Common Lisp ==="
sbcl --script tests/test_un_lisp.lisp
echo ""

echo "=== Erlang ==="
escript tests/test_un_erl.erl
echo ""

echo "=== Elixir ==="
elixir tests/test_un_ex.exs
echo ""

echo "=== Julia ==="
julia tests/test_un_jl.jl
echo ""

echo "=== R ==="
Rscript tests/test_un_r.r
echo ""

echo "=== Crystal ==="
crystal tests/test_un_cr.cr
echo ""

echo "=== Fortran ==="
gfortran -o test_un_f90 tests/test_un_f90.f90 && ./test_un_f90 && rm test_un_f90
echo ""

echo "=== COBOL ==="
bash tests/test_un_cob.sh
echo ""

echo "=== Prolog ==="
swipl -g main -t halt tests/test_un_pro.pro
echo ""

echo "=== Forth ==="
gforth tests/test_un_forth.fth
```

## Test Output

Each test suite produces color-coded output:

- **Green ✓ PASS** - Test passed successfully
- **Red ✗ FAIL** - Test failed with error message
- **Yellow ⚠ WARNING** - API key not set, some tests skipped

Example output:
```
=== Haskell UN CLI Test Suite ===

✓ PASS - Extension detection
✓ PASS - API integration
✓ PASS - Fibonacci end-to-end test

✓ All tests passed (3/3)
```

## Exit Codes

- `0` - All tests passed
- `1` - One or more tests failed

## Debugging Failed Tests

If a test fails:

1. Check that you're running from the correct directory (`/home/fox/git/unsandbox.com/cli/inception/`)
2. Verify `UNSANDBOX_API_KEY` is set correctly
3. Ensure the UN CLI implementation (`un.hs`, `un.ml`, etc.) is in the parent directory
4. Check that test files exist in `../test/` (e.g., `fib.hs`, `fib.ml`)
5. Review the error message - tests provide detailed failure information

## Implementation Notes

- Tests use the same extension-to-language mapping as the UN CLI implementations
- API tests create temporary files in `/tmp/`
- Tests verify both success (exit code 0) and expected output content
- Fibonacci tests specifically look for the string `"fib(10) = 55"` in output
- All tests are self-contained and can run independently
