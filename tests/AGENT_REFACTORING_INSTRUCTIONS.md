# SDK Refactoring Instructions for Agents

This document explains how to refactor UN SDK implementations with proper testing and validation.

## Quick Start

### Step 1: Understand the Reference Implementations
Study these complete, working SDK implementations first:
- **Python**: `/home/fox/git/un-inception/un.py` (DONE - use as reference)
- **JavaScript**: `/home/fox/git/un-inception/un.js` (DONE - use as reference)
- **Go**: Check if exists; if incomplete, use Python/JS as pattern

### Step 2: Apply the Pattern to Your Language
Every refactored SDK needs:

1. **Library functions** (can be imported/required)
   ```
   execute(language, code, opts)
   executeAsync(language, code, opts)
   run(file, opts)
   runAsync(file, opts)
   wait(job_id)
   getJob(job_id)
   cancelJob(job_id)
   listJobs()
   languages()
   detectLanguage(filename)
   image(code, output_format)
   ```

2. **Credential system** (4-tier with fallback)
   ```
   Priority: args > env vars > ~/.unsandbox/accounts.csv > ./accounts.csv
   ```

3. **HMAC-SHA256 request signing**
   ```
   signature = HMAC(secret, "timestamp:METHOD:endpoint:body")
   ```

4. **1-hour cache** for `/languages` API endpoint
   ```
   ~/.unsandbox/languages.json with TTL check
   ```

5. **CLI functionality** (preserve original behavior)
   ```
   Command-line interface for executing code files
   ```

6. **Proper docstrings** (language-appropriate style)
   ```
   Python: """ """
   JavaScript: /** */
   Go: // line comments
   Ruby: # line comments
   PHP: /** */
   Etc.
   ```

7. **Complete license header** (permacomputer public domain)
   ```
   Must include full 35+ line license notice
   ```

### Step 3: Test Your Work

Use the provided test framework to validate:

```bash
# Run unit tests (no API key required)
cd /home/fox/git/un-inception
python3 tests/test_sdk_library.py --languages YOUR_LANGUAGE

# Or run the master test runner
./tests/run_sdk_tests.sh --languages YOUR_LANGUAGE
```

Expected output:
```
SDK LIBRARY TEST SUMMARY
========================================
✓ your_language     3/  4 passed (1 skipped)
========================================
Total: 3 passed, 0 failed, 1 skipped
```

### Step 4: Validate Examples Still Work

After refactoring, ensure the example files execute properly:

```bash
# Set credentials (if available)
export UNSANDBOX_API_KEY='your-test-key'

# Test CLI still works
python un.py examples/hello.py
node un.js examples/hello.js
ruby un.rb examples/hello.rb
# etc.

# Test library imports work
python3 -c "import un; result = un.execute('python', 'print(1)')"
node -e "const un = require('./un.js'); un.execute('javascript', 'console.log(1)')"
```

### Step 5: Update Documentation Examples

Once your SDK is refactored, update the example files:

```
/home/fox/git/unsandbox.com/priv/static/docs/examples/{language}/execute.txt
/home/fox/git/unsandbox.com/priv/static/docs/examples/{language}/execute_async.txt
```

Pattern to follow (from Python/JavaScript):
- Show SDK library import/require
- Demonstrate credential setup
- Show execute() call with proper options
- Show executeAsync() + wait() pattern
- Include error handling

### Step 6: Commit Your Work

```bash
git add un.{ext} tests/test_un_{lang}.{ext}
git commit -m "Refactor {Language} SDK: add library exports + HMAC auth + caching

- Implement execute, executeAsync, wait, getJob, cancelJob, listJobs
- Add 4-tier credential system (args > env > home > local)
- Add 1-hour cache for languages list in ~/.unsandbox/
- Implement HMAC-SHA256 request signing
- Preserve original CLI functionality
- Proper docstrings for {Language}
- Full test coverage with unit + integration tests"
```

## Key Implementation Details

### Credential Loading (4-tier system)

Priority order (stop at first match):

1. **Function arguments** - explicit parameters to execute()
   ```python
   execute('python', code, public_key='...', secret_key='...')
   ```

2. **Environment variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY='unsb-pk-xxxx'
   export UNSANDBOX_SECRET_KEY='unsb-sk-xxxx'
   ```

3. **Home directory** (default location)
   ```
   ~/.unsandbox/accounts.csv
   Format: public_key,secret_key (one per line)
   ```

4. **Local directory** (current working dir)
   ```
   ./accounts.csv
   Format: public_key,secret_key (one per line)
   ```

### HMAC Signature Format

Every API request must include:

```
Authorization: Bearer {public_key}
X-Timestamp: {unix_timestamp}
X-Signature: {hmac_sha256}
```

Where signature is:
```
HMAC-SHA256(
  secret_key,
  "{timestamp}:{METHOD}:{endpoint}:{json_body}"
)
```

Example with Python:
```python
import hmac
import hashlib
import json
import time

secret = 'unsb-sk-...'
timestamp = str(int(time.time()))
method = 'POST'
endpoint = '/execute'
body = json.dumps({'language': 'python', 'code': 'print(1)'})

message = f"{timestamp}:{method}:{endpoint}:{body}"
signature = hmac.new(
    secret.encode(),
    message.encode(),
    hashlib.sha256
).hexdigest()
```

### Languages Cache (1-hour TTL)

Implement caching to avoid repeated API calls:

1. **Cache location**: `~/.unsandbox/languages.json`
2. **TTL**: 3600 seconds (1 hour)
3. **Check logic**:
   - If file exists AND age < TTL → return cached
   - Otherwise → fetch from API AND update cache

Example with Python:
```python
from pathlib import Path
import json
import time

def languages(cache_ttl=3600):
    cache_path = Path.home() / '.unsandbox' / 'languages.json'

    # Check cache
    if cache_path.exists():
        age = time.time() - cache_path.stat().st_mtime
        if age < cache_ttl:
            return json.loads(cache_path.read_text())

    # Fetch from API
    result = api_request('GET', '/languages')
    langs = result['languages']

    # Update cache
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(json.dumps(langs))

    return langs
```

## Common Patterns

### Exception Hierarchy
Every SDK should define:
```python
class UnsandboxError(Exception): pass
class AuthenticationError(UnsandboxError): pass
class ExecutionError(UnsandboxError): pass
class APIError(UnsandboxError): pass
class TimeoutError(UnsandboxError): pass
```

### Exponential Backoff Polling
When waiting for async jobs:
```
Initial delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
Pattern: Increases generally but with variation to avoid thundering herd
Max polls: Usually around 120 (2+ hours for patience)
```

### CLI Entry Point
Preserve CLI functionality:
```python
# Python: at end of file
if __name__ == '__main__':
    import sys
    from pathlib import Path
    # Parse args, call execute/run, handle errors
```

```javascript
// JavaScript: at end of file
if (require.main === module) {
    cliMain().catch(e => {
        console.error(e.message);
        process.exit(1);
    });
}
```

```go
// Go: in main()
if len(os.Args) < 2 {
    // Show usage
}
// Parse args, call Execute/Run, handle errors
```

## Testing Checklist

Before committing, verify ALL of these:

- [ ] **Unit tests pass** - Library functions work
- [ ] **Library functions exist** - execute, executeAsync, wait, etc.
- [ ] **Credentials load** - All 4 sources (args, env, home, local)
- [ ] **HMAC signature** - Correct format and length (64 hex chars)
- [ ] **Cache works** - languages.json created and used
- [ ] **CLI still works** - Command-line execution preserved
- [ ] **Error handling** - Appropriate exceptions for errors
- [ ] **Docstrings** - Language-appropriate comment style
- [ ] **License header** - Full permacomputer notice at top
- [ ] **No hardcoded credentials** - All removed
- [ ] **Examples updated** - execute.txt and execute_async.txt
- [ ] **Git status clean** - All files committed

## Troubleshooting

### Problem: "Cannot import/require un module"
**Solution:**
1. Check file is in correct location: `/home/fox/git/un-inception/un.{ext}`
2. Check filename matches language (un.py, un.js, un.go, etc.)
3. Check for syntax errors: try running file directly

### Problem: "HMAC signature invalid"
**Solution:**
1. Verify message format: `"{timestamp}:{METHOD}:{endpoint}:{body}"`
2. Verify secret key encoding: bytes, not string
3. Verify hexdigest: should be 64 lowercase hex characters
4. Verify HMAC algorithm: must be SHA256, not SHA1 or other

### Problem: "Credentials not found"
**Solution:**
1. Check env vars: `echo $UNSANDBOX_PUBLIC_KEY`
2. Check files exist: `ls ~/.unsandbox/accounts.csv`
3. Check file format: one credential pair per line
4. Check loading order: function args, env, home, local

### Problem: "Cache not working"
**Solution:**
1. Check directory exists: `mkdir -p ~/.unsandbox/`
2. Check file created: `ls -la ~/.unsandbox/languages.json`
3. Check TTL logic: age < 3600 seconds
4. Check mtime: `stat ~/.unsandbox/languages.json`

## Examples of Refactored SDKs

To understand the pattern better, study:

1. **Python (un.py)** - Complete, well-commented implementation
   - Shows all library functions
   - Demonstrates credential system
   - Shows HMAC signing
   - Shows caching
   - Preserves CLI

2. **JavaScript (un.js)** - Node.js implementation
   - Shows async/await pattern
   - Shows Client class wrapper
   - Shows error handling
   - Shows function validation

3. **Go** (partially done) - if available
   - Shows goroutine patterns
   - Shows error handling
   - Shows HTTP client usage

## Questions or Blockers?

If you're stuck:

1. **Check the guide**: `/home/fox/git/un-inception/tests/SDK_TESTING_GUIDE.md`
2. **Review Python SDK**: `un.py` is the canonical reference
3. **Look at examples**: `priv/static/docs/examples/python/` and `javascript/`
4. **Run tests**: `python3 tests/test_sdk_library.py --languages YOUR_LANGUAGE`
5. **Check git history**: `git log --oneline un.py` to see refactoring commits

## Final Notes

- **Don't rush** - Quality over speed. Tests will catch errors.
- **Test frequently** - Run tests after each function you implement
- **Copy patterns** - Use Python/JavaScript as templates exactly
- **Preserve functionality** - Original CLI must still work
- **Document clearly** - Use language-appropriate docstrings
- **Commit atomically** - Each language in separate commit

Good luck! The test framework is there to help you validate your work. Use it!
