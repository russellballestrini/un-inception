# SDK Refactoring Checklist

Use this checklist when refactoring a UN SDK to ensure nothing is missed.

## Pre-Refactoring

- [ ] Read `tests/AGENT_REFACTORING_INSTRUCTIONS.md`
- [ ] Study Python SDK (`un.py`) as reference
- [ ] Study JavaScript SDK (`un.js`) as reference
- [ ] Create test file `tests/test_un_{language}.{ext}`
- [ ] Run baseline tests: `python3 tests/test_sdk_library.py --languages {lang}`

## Core Library Functions

### Execution Functions
- [ ] `execute(language, code, opts)` - Sync execution
- [ ] `executeAsync(language, code, opts)` - Async execution
- [ ] `run(file, opts)` - Run file sync
- [ ] `runAsync(file, opts)` - Run file async

### Job Management
- [ ] `wait(job_id, timeout)` - Poll for completion
- [ ] `getJob(job_id)` - Get job status
- [ ] `cancelJob(job_id)` - Cancel running job
- [ ] `listJobs(limit)` - List active jobs

### Snapshots (NEW!)
- [ ] `sessionSnapshot(session_id)` - Create session snapshot
- [ ] `serviceSnapshot(service_id)` - Create service snapshot
- [ ] `listSnapshots()` - List all snapshots
- [ ] `restoreSnapshot(snapshot_id)` - Restore from snapshot
- [ ] `deleteSnapshot(snapshot_id)` - Delete snapshot

### Utilities
- [ ] `languages(cache_ttl)` - Get supported languages
- [ ] `languageInfo(language)` - Get language details
- [ ] `detectLanguage(filename)` - Detect from extension
- [ ] `image(code, format)` - Generate images

## Credential System (4-tier)

- [ ] Load from function arguments (highest priority)
- [ ] Load from env vars: `UNSANDBOX_PUBLIC_KEY` + `UNSANDBOX_SECRET_KEY`
- [ ] Load from `~/.unsandbox/accounts.csv`
- [ ] Load from `./accounts.csv` (local directory)
- [ ] Raise error if no credentials found
- [ ] Test all 4 credential sources work

## HMAC Signature

- [ ] Implement `_sign_request(secret, timestamp, method, endpoint, body)`
- [ ] Generate signature: `HMAC-SHA256(secret, "{timestamp}:{method}:{endpoint}:{body}")`
- [ ] Signature format: 64 lowercase hexadecimal characters
- [ ] Include headers in requests:
  - [ ] `Authorization: Bearer {public_key}`
  - [ ] `X-Timestamp: {unix_timestamp}`
  - [ ] `X-Signature: {signature}`
  - [ ] `Content-Type: application/json`
- [ ] Test signature generation produces correct format

## Languages Cache

- [ ] Create cache directory: `~/.unsandbox/`
- [ ] Cache location: `~/.unsandbox/languages.json`
- [ ] Cache TTL: 3600 seconds (1 hour)
- [ ] Check cache exists AND is fresh before API call
- [ ] Only update cache on successful API response
- [ ] Create directory if needed before writing
- [ ] Test cache is used (don't call API twice within 1 hour)

## Error Handling

Define exception classes:
- [ ] `UnsandboxError` - Base exception
- [ ] `AuthenticationError` - Invalid credentials
- [ ] `ExecutionError` - Code execution failed
- [ ] `APIError` - API communication error
- [ ] `TimeoutError` - Job polling timeout

Implement proper error handling:
- [ ] Handle network errors gracefully
- [ ] Handle invalid credentials
- [ ] Handle API errors (4xx, 5xx)
- [ ] Handle malformed responses
- [ ] Handle job timeout scenarios

## Code Quality

### Docstrings (Language-Appropriate)
- [ ] Python: `""" """` triple quotes
- [ ] JavaScript: `/** */` JSDoc format with @param @returns
- [ ] Go: `//` line comments before functions
- [ ] Ruby: `#` line comments before methods
- [ ] PHP: `/** */` doc blocks
- [ ] Java: `/** */` Javadoc format
- [ ] C/C++: `// //` or `/* */` comments
- [ ] All public functions documented
- [ ] All parameters documented
- [ ] Return types documented
- [ ] Examples in docstrings

### Code Style
- [ ] Follow language conventions
- [ ] Use language-idiomatic patterns
- [ ] Consistent naming (snake_case, camelCase, PascalCase as appropriate)
- [ ] Proper error handling throughout
- [ ] No hardcoded credentials
- [ ] No debugging print statements
- [ ] No commented-out code

### License Header
- [ ] Full permacomputer public domain notice (35+ lines)
- [ ] Placed at very top of file
- [ ] Followed by blank line
- [ ] All license text preserved exactly

## CLI Functionality

- [ ] Original CLI behavior preserved
- [ ] Can execute code files: `un {file}`
- [ ] Can execute inline code: `un -s language 'code'`
- [ ] Proper usage/help output
- [ ] Correct exit codes
- [ ] Error messages to stderr
- [ ] Output to stdout
- [ ] Support for all original flags/options

### CLI Entry Point
- [ ] Python: `if __name__ == '__main__':`
- [ ] JavaScript: `if (require.main === module):`
- [ ] Go: Separate main() function
- [ ] Ruby: `if __FILE__ == $0:`
- [ ] Other languages: Language-appropriate

## Testing

### Unit Tests
- [ ] Create `tests/test_un_{language}.{ext}`
- [ ] Test credential loading (all 4 sources)
- [ ] Test HMAC signature generation
- [ ] Test function existence and signatures
- [ ] Test error handling for invalid inputs
- [ ] Tests pass: `python3 tests/test_sdk_library.py --languages {lang}`

### Manual Testing
- [ ] Can import/require library: `import un` or `require('./un')`
- [ ] Can call each function without errors
- [ ] Credentials load correctly
- [ ] HMAC signatures generate correctly
- [ ] Cache is created and used
- [ ] CLI still works: `un.py test.py` or `node un.js test.js`

### Integration Testing (if API key available)
- [ ] `execute()` works with real API
- [ ] `executeAsync()` works with real API
- [ ] `wait()` polls correctly
- [ ] Examples execute successfully

## Documentation

### Example Files
- [ ] Update `unsandbox.com/priv/static/docs/examples/{language}/execute.txt`
- [ ] Update `unsandbox.com/priv/static/docs/examples/{language}/execute_async.txt`
- [ ] Show library import/require
- [ ] Show credential setup (env vars)
- [ ] Show proper HMAC authentication
- [ ] Show error handling
- [ ] Code runs successfully

### Comments in Code
- [ ] Explain complex logic
- [ ] Document public API
- [ ] Note any language-specific workarounds
- [ ] Explain credential priority system
- [ ] Explain cache invalidation

## Git Workflow

- [ ] Create feature branch (optional): `git checkout -b refactor/{language}`
- [ ] Implement and test incrementally
- [ ] Commit with clear message explaining changes
- [ ] Include test file in commit
- [ ] Run all tests before committing
- [ ] Push to main (or PR if using branches)

### Commit Message Template
```
Refactor {Language} SDK: add library exports + HMAC auth + caching

- Implement execute, executeAsync, wait, getJob, cancelJob, listJobs
- Add 4-tier credential system (args > env > home > local)
- Add 1-hour cache for languages list
- Implement HMAC-SHA256 request signing
- Preserve original CLI functionality
- Language-specific docstrings
- Full test coverage
```

## Final Validation

Before declaring complete:

- [ ] `git status` is clean (all changes committed)
- [ ] Unit tests pass: `python3 tests/test_sdk_library.py --languages {lang}`
- [ ] Master test runner passes: `./tests/run_sdk_tests.sh --languages {lang}`
- [ ] CLI still works: `un.{ext} test.{ext}`
- [ ] Library imports work: `from un import execute` or `const un = require('./un')`
- [ ] Examples in docs can be copied and executed
- [ ] No syntax errors in any files
- [ ] No hardcoded credentials
- [ ] No debug code or print statements
- [ ] Git history is clean and meaningful

## Special Cases

### Compiled Languages (C, Go, Rust, etc.)
- [ ] Build works without errors
- [ ] No build artifacts committed
- [ ] Can be run after compilation
- [ ] Tests include compilation step
- [ ] Documentation mentions compilation requirement

### JVM Languages (Java, Kotlin, Scala, etc.)
- [ ] No .class or compiled files committed
- [ ] Build files present if needed
- [ ] Tests work with build system
- [ ] Documentation mentions build tool

### Functional Languages (Haskell, OCaml, etc.)
- [ ] Pure functions for calculations
- [ ] Monads for I/O and side effects
- [ ] Tests use language-specific patterns
- [ ] Documentation explains functional patterns

### Web Languages (PHP, JSP, etc.)
- [ ] Can run as CLI (not just web server)
- [ ] No web server dependency for CLI mode
- [ ] Tests work without web server

## Estimated Language Groupings

**Fast (< 2 hours)**
- Ruby, Perl, PHP, Lua, TCL, Shell/Bash, Deno

**Medium (2-4 hours)**
- Go, TypeScript, Crystal, Dart, Elixir, Scala, Kotlin

**Slower (4+ hours)**
- Rust (borrow checker learning curve), C++, Java, Fortran, COBOL

## Help & Resources

- **AGENT_REFACTORING_INSTRUCTIONS.md** - Detailed walkthrough
- **SDK_TESTING_GUIDE.md** - Testing patterns and examples
- **un.py** - Python reference implementation (canonical)
- **un.js** - JavaScript reference implementation
- **tests/test_sdk_library.py** - Python test framework
- **tests/run_sdk_tests.sh** - Master test runner
- **tests/test_un_{lang}.{ext}** - Language-specific test examples

## Success = Complete Testing Pass

Your SDK refactoring is done when:

```
✓ 8/8 passed (0 failed)
```

All unit tests pass, no failures, all functions tested.
