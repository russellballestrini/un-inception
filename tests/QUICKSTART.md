# UN CLI Inception Tests - Quick Start Guide

## TL;DR - Run All Tests

```bash
cd /home/fox/git/unsandbox.com/cli/inception
export UNSANDBOX_API_KEY="your_api_key_here"
./tests/run_all_tests.sh
```

## Run Individual Tests

### Haskell
```bash
cd /home/fox/git/unsandbox.com/cli/inception
./tests/test_un_hs.hs
```

### OCaml
```bash
cd /home/fox/git/unsandbox.com/cli/inception
ocaml tests/test_un_ml.ml
```

### Clojure
```bash
cd /home/fox/git/unsandbox.com/cli/inception
clj -Sdeps '{:deps {clj-http/clj-http {:mvn/version "3.12.3"} cheshire/cheshire {:mvn/version "5.11.0"}}}' -M tests/test_un_clj.clj
```

### Scheme (Guile)
```bash
cd /home/fox/git/unsandbox.com/cli/inception
./tests/test_un_scm.scm
```

### Common Lisp (SBCL)
```bash
cd /home/fox/git/unsandbox.com/cli/inception
sbcl --script tests/test_un_lisp.lisp
```

### Erlang
```bash
cd /home/fox/git/unsandbox.com/cli/inception
escript tests/test_un_erl.erl
```

### Elixir
```bash
cd /home/fox/git/unsandbox.com/cli/inception
elixir tests/test_un_ex.exs
```

## What Gets Tested?

Each test suite validates:

1. **Extension Detection** (10+ mappings)
   - `.hs` → `haskell`
   - `.ml` → `ocaml`
   - `.py` → `python`
   - etc.

2. **API Integration**
   - Creates test file
   - Runs via UN CLI
   - Verifies output

3. **End-to-End Fibonacci**
   - Runs `../test/fib.*`
   - Checks for `"fib(10) = 55"`

## Output Example

```
=== Haskell UN CLI Test Suite ===

✓ PASS - Extension detection
✓ PASS - API integration
✓ PASS - Fibonacci end-to-end test

✓ All tests passed (3/3)
```

## Files Created

- `test_un_hs.hs` - Haskell tests (163 lines)
- `test_un_ml.ml` - OCaml tests (176 lines)
- `test_un_clj.clj` - Clojure tests (153 lines)
- `test_un_scm.scm` - Scheme tests (173 lines)
- `test_un_lisp.lisp` - Common Lisp tests (178 lines)
- `test_un_erl.erl` - Erlang tests (189 lines)
- `test_un_ex.exs` - Elixir tests (191 lines)
- `run_all_tests.sh` - Automated test runner
- `README.md` - Full documentation
- `TESTING_SUMMARY.md` - Test suite summary
- `QUICKSTART.md` - This file

## Exit Codes

- `0` = All tests passed
- `1` = One or more tests failed

## Without API Key

Tests run but skip integration/functional tests:

```
⚠ WARNING - UNSANDBOX_API_KEY not set, skipping API tests

✓ PASS - Extension detection
✓ PASS - API integration (skipped)
✓ PASS - Fibonacci end-to-end test (skipped)

✓ All tests passed (3/3)
```

## More Info

- Full documentation: `README.md`
- Test summary: `TESTING_SUMMARY.md`
