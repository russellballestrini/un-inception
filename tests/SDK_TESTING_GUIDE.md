# SDK Testing Guide for Refactoring

This guide explains how to test UN SDK library implementations during refactoring. It enables agents to validate their own work independently.

## Overview

Each SDK (un.py, un.js, un.go, etc.) must provide:

1. **Library functions** - The core API that can be imported/required by other code
2. **CLI functionality** - Command-line interface for direct code execution
3. **Tests** - Unit, integration, and functional tests to validate behavior

## Test Categories

### 1. Unit Tests (No API Key Required)
Focus on library function behavior without external API calls:

- **Credential loading** - Functions correctly parse credentials from env vars, files, and arguments
- **HMAC signature generation** - Signatures match the expected format (64 hex chars)
- **Cache checking** - Languages cache exists and can be read
- **Extension detection** - File extensions correctly map to language identifiers
- **Function signatures** - All expected functions exist and are callable
- **Error handling** - Invalid inputs produce appropriate errors

**Example Unit Test (Python):**
```python
def test_hmac_signature():
    """Test HMAC-SHA256 signature generation"""
    sig = un._sign_request(
        'test-secret-key',
        '1704067200',  # timestamp
        'POST',        # method
        '/execute',    # endpoint
        '{}'           # body
    )

    assert len(sig) == 64, f"Invalid signature length: {len(sig)}"
    assert all(c in '0123456789abcdef' for c in sig), "Invalid hex characters"
```

### 2. Integration Tests (Requires API Key)
Test actual API communication with real infrastructure:

- **execute()** - Synchronous code execution and result retrieval
- **executeAsync()** - Asynchronous job submission and status polling
- **wait()** - Job polling with exponential backoff
- **languages()** - Language list retrieval with caching
- **get_job()** - Job status retrieval
- **cancel_job()** - Job cancellation

**Example Integration Test (Python):**
```python
def test_execute_integration():
    """Test execute function with real API"""
    result = un.execute(
        'python',
        'print("hello world")',
        network_mode='zerotrust'
    )

    assert result['exit_code'] == 0
    assert 'hello world' in result['stdout']
```

### 3. Functional Tests (Requires API Key)
End-to-end workflow validation:

- **CLI execution** - Run code file through CLI interface
- **Output parsing** - Correctly display results to user
- **Error handling** - Handle API errors gracefully
- **Input/output files** - Process input files and write outputs

**Example Functional Test:**
```python
def test_cli_execution():
    """Test CLI end-to-end"""
    import subprocess

    result = subprocess.run(
        ['python', 'un.py', 'test_script.py'],
        capture_output=True,
        text=True
    )

    assert result.returncode == 0
    assert 'expected output' in result.stdout
```

## SDK Library Requirements

Every SDK must implement these functions:

### Core Execution Functions
```python
# Synchronous execution
result = execute(
    language: str,
    code: str,
    network_mode: str = 'zerotrust',
    ttl: int = 60,
    env: dict = None,
    files: list = None,
    input_file: str = None
) -> dict

# Asynchronous execution
job = executeAsync(
    language: str,
    code: str,
    network_mode: str = 'zerotrust',
    ttl: int = 60,
    env: dict = None,
    files: list = None
) -> dict

# Wait for job completion
result = wait(job_id: str, timeout: int = 3600) -> dict
```

### Job Management Functions
```python
# Get job status
job = get_job(job_id: str) -> dict

# Cancel job
cancel_job(job_id: str) -> dict

# List jobs
jobs = list_jobs(limit: int = 100) -> list[dict]
```

### Utility Functions
```python
# Get supported languages
languages = languages(cache_ttl: int = 3600) -> list[str]

# Get language info
info = language_info(language: str) -> dict
```

### Credential Functions
```python
# Load credentials from multiple sources
public_key, secret_key = _get_credentials(
    public_key: str = None,
    secret_key: str = None
) -> tuple[str, str]

# Load accounts.csv
accounts = _load_accounts_csv(path: str = None) -> list[tuple[str, str]]
```

### Signature Functions
```python
# Generate HMAC-SHA256 signature
signature = _sign_request(
    secret_key: str,
    timestamp: str,
    method: str,
    endpoint: str,
    body: str
) -> str
```

## Credential Priority System

SDKs must check credentials in this order:

1. **Function arguments** - Highest priority (explicit parameter)
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
3. **Home directory config** - `~/.unsandbox/accounts.csv`
4. **Local directory config** - `./accounts.csv` in current working directory
5. **Error** - Raise error if no credentials found

**Example (Python):**
```python
def _get_credentials(public_key=None, secret_key=None):
    """Load credentials with 4-tier fallback system"""

    # Tier 1: Function arguments
    if public_key and secret_key:
        return public_key, secret_key

    # Tier 2: Environment variables
    pk = os.environ.get('UNSANDBOX_PUBLIC_KEY')
    sk = os.environ.get('UNSANDBOX_SECRET_KEY')
    if pk and sk:
        return pk, sk

    # Tier 3: Home directory
    home_file = Path.home() / '.unsandbox' / 'accounts.csv'
    if home_file.exists():
        accounts = _load_accounts_csv(str(home_file))
        if accounts:
            return accounts[0]  # Use first account

    # Tier 4: Local directory
    local_file = Path('./accounts.csv')
    if local_file.exists():
        accounts = _load_accounts_csv(str(local_file))
        if accounts:
            return accounts[0]

    raise AuthenticationError("No credentials found")
```

## Caching Requirements

### Languages Cache
- **Location**: `~/.unsandbox/languages.json`
- **TTL**: 1 hour (3600 seconds)
- **Format**: JSON array of language strings
- **Update**: Only update if cache is older than TTL AND API call succeeds

**Example (Python):**
```python
def languages(cache_ttl=3600):
    """Get supported languages with 1-hour cache"""

    cache_path = Path.home() / '.unsandbox' / 'languages.json'

    # Check if cache exists and is fresh
    if cache_path.exists():
        age = time.time() - cache_path.stat().st_mtime
        if age < cache_ttl:
            return json.load(open(cache_path))

    # Fetch from API
    response = _make_request('GET', '/languages')
    langs = response['languages']

    # Update cache (create directory if needed)
    cache_path.parent.mkdir(parents=True, exist_ok=True)
    cache_path.write_text(json.dumps(langs))

    return langs
```

## HMAC Signature Format

All API requests must include proper HMAC signatures:

**Signature Input:**
```
{timestamp}:{method}:{endpoint}:{body}
```

**Example:**
```
1704067200:POST:/execute:{"language":"python","code":"print(1)"}
```

**Signature Calculation:**
```python
import hmac
import hashlib

signature = hmac.new(
    secret_key.encode(),
    message.encode(),
    hashlib.sha256
).hexdigest()
```

**Request Headers:**
```
Authorization: Bearer {public_key}
X-Timestamp: {timestamp}
X-Signature: {signature}
Content-Type: application/json
```

## Testing Workflow for Agents

### 1. Before Refactoring
```bash
# Create a checkpoint of the original SDK
cd /home/fox/git/un-inception
git stash

# Run baseline tests
./tests/run_sdk_tests.sh --unit
```

### 2. During Refactoring
```bash
# As you implement library functions, add tests
# Edit tests/test_sdk_library.py to add tests for your language

# Periodically validate unit tests pass
python3 tests/test_sdk_library.py --languages your_language
```

### 3. After Refactoring
```bash
# Run full test suite
./tests/run_sdk_tests.sh --unit

# If API key available, test integration
export UNSANDBOX_API_KEY='your-key'
./tests/run_sdk_tests.sh --integration

# Test CLI still works
python un.py examples/hello.py
node un.js examples/hello.js
go run un.go examples/hello.go
```

### 4. Validation Before Commit
```bash
# Run all tests
./tests/run_sdk_tests.sh --all

# Verify both library AND CLI work
node un.js -s javascript 'console.log("hello from cli")'
python un.py -s python 'print("hello from cli")'
go run un.go -s go 'fmt.Println("hello from cli")'

# Commit when all tests pass
git add un.py un.js un.go
git commit -m "Refactor SDK: add library exports + HMAC auth + caching"
```

## Common Testing Mistakes to Avoid

### ❌ Wrong: Only testing CLI functionality
```python
# BAD - doesn't test library functions
result = subprocess.run(['python', 'un.py', 'code.py'])
```

### ✅ Correct: Testing both library AND CLI
```python
# GOOD - test library functions exist and work
from un import execute, executeAsync

# AND test CLI
result = subprocess.run(['python', 'un.py', 'code.py'])
```

### ❌ Wrong: Ignoring credential loading
```python
# BAD - hardcoded credentials
def execute(language, code):
    pk = 'unsb-pk-xxxx'  # WRONG!
    sk = 'unsb-sk-xxxx'  # WRONG!
```

### ✅ Correct: Flexible credential loading
```python
# GOOD - respect credential sources
def execute(language, code, public_key=None, secret_key=None):
    pk, sk = _get_credentials(public_key, secret_key)
    # Now use pk, sk
```

### ❌ Wrong: Ignoring cache TTL
```python
# BAD - always calls API
def languages():
    return requests.get('/languages').json()
```

### ✅ Correct: Proper caching with TTL
```python
# GOOD - cache with 1-hour TTL
def languages(cache_ttl=3600):
    cache_file = Path.home() / '.unsandbox' / 'languages.json'

    if cache_file.exists():
        age = time.time() - cache_file.stat().st_mtime
        if age < cache_ttl:
            return json.load(open(cache_file))

    # Fetch, cache, return
```

## Adding Tests for a New Language

When refactoring a new SDK:

1. **Add unit tests** to `test_sdk_library.py`:
```python
def test_ruby_sdk(self):
    """Test Ruby SDK library functions"""
    # Import, test credentials, HMAC, etc.
```

2. **Add to test runner** `run_sdk_tests.sh`:
```bash
# Run Ruby SDK tests
echo -e "${BLUE}Running Ruby SDK tests...${RESET}"
if command -v ruby &> /dev/null; then
    ruby "$SCRIPT_DIR/test_un_ruby.rb" 2>&1 || true
else
    echo -e "${YELLOW}⚠ Ruby not found${RESET}"
fi
```

3. **Create test file** `tests/test_un_ruby.rb`:
```ruby
require_relative '../un.rb'

# Test HMAC signature
sig = Un._sign_request('test-sk', '1704067200', 'POST', '/execute', '{}')
if sig.length == 64 && sig.match?(/^[0-9a-f]+$/)
  puts "✓ PASS: HMAC-SHA256 signature generation"
else
  puts "✗ FAIL: HMAC-SHA256 signature generation"
end

# Test execute function exists
if Un.respond_to?(:execute)
  puts "✓ PASS: execute function exists"
else
  puts "✗ FAIL: execute function exists"
end
```

## Troubleshooting Tests

### Test: Import Error
```
Error: Cannot import un module
```

**Fix:** Ensure:
1. SDK file (un.py, un.js, etc.) is in parent directory
2. Correct extension (.py for Python, .js for JavaScript)
3. No syntax errors in SDK file

### Test: HMAC Signature Fails
```
FAIL: HMAC-SHA256 signature - invalid signature length
```

**Fix:** Check:
1. Using `hmac` library correctly
2. Using SHA256 hash algorithm
3. Converting result to hex string (64 chars)
4. Secret key is string/bytes, not encoded yet

### Test: API Integration Fails
```
FAIL: execute() function - status 401
```

**Fix:**
1. Check `UNSANDBOX_API_KEY` is set: `echo $UNSANDBOX_API_KEY`
2. Verify API key is valid and not expired
3. Check credentials are loaded correctly: `_get_credentials()`
4. Verify HMAC signature generation is working

### Test: Language Not Found
```
⚠ Python 3 not found
```

**Fix:** Install the required interpreter:
```bash
# Ubuntu/Debian
sudo apt-get install python3 nodejs golang-go ruby perl php

# macOS
brew install python node go ruby perl php
```

## Success Criteria

An SDK is ready for production when:

1. ✅ **All unit tests pass** - Library functions work correctly
2. ✅ **All integration tests pass** - API communication works
3. ✅ **All functional tests pass** - CLI still works
4. ✅ **Examples execute** - Python/JS examples work with new SDK
5. ✅ **Credentials load** - All 4 credential sources work
6. ✅ **Caching works** - Languages cache is created and used
7. ✅ **CLI still works** - Original CLI functionality preserved
8. ✅ **Documentation updated** - Examples reflect new SDK usage

## Example: Complete Ruby SDK Refactoring + Testing

### Step 1: Check Ruby is available
```bash
ruby --version
```

### Step 2: Refactor un.rb with library exports
```ruby
# un.rb - Ruby SDK with library exports
require 'net/http'
require 'json'
require 'openssl'
require 'base64'
require 'time'

module Un
  def self.execute(language, code, opts = {})
    # Implementation
  end

  def self.executeAsync(language, code, opts = {})
    # Implementation
  end

  private

  def self._sign_request(secret, timestamp, method, endpoint, body)
    message = "#{timestamp}:#{method}:#{endpoint}:#{body}"
    OpenSSL::HMAC.hexdigest('SHA256', secret, message)
  end
end

# CLI entry point
if __FILE__ == $0
  # Parse args and call Un.execute
end
```

### Step 3: Add unit tests to test_sdk_library.py
```python
def test_ruby_sdk(self):
    """Test Ruby SDK library functions"""
    language = 'ruby'

    try:
        # Create test script
        test_code = '''
require_relative '../un.rb'

sig = Un._sign_request('test-sk', '1704067200', 'POST', '/execute', '{}')
if sig.length == 64
  puts "✓ PASS: HMAC signature"
else
  puts "✗ FAIL: HMAC signature"
end
'''
        # Run test and parse output
    except Exception as e:
        self.results.add_result(language, 'unit', 'SDK import', False, str(e))
```

### Step 4: Create test_un_ruby.rb
```ruby
#!/usr/bin/env ruby
require_relative '../un.rb'

puts "Testing Ruby SDK..."

# Test 1: HMAC signature
sig = Un._sign_request('test-sk', '1704067200', 'POST', '/execute', '{}')
if sig && sig.length == 64 && sig.match?(/^[0-9a-f]+$/)
  puts "✓ PASS: HMAC-SHA256 signature"
else
  puts "✗ FAIL: HMAC-SHA256 signature"
end

# Test 2: execute function
if Un.respond_to?(:execute)
  puts "✓ PASS: execute function exists"
else
  puts "✗ FAIL: execute function exists"
end
```

### Step 5: Run tests
```bash
./tests/run_sdk_tests.sh --languages ruby

# Or manually
ruby tests/test_un_ruby.rb
```

### Step 6: Validate CLI still works
```bash
# Test CLI with inline code
ruby un.rb -s ruby 'puts "hello from ruby"'

# Test CLI with file
echo 'puts "hello from file"' > test.rb
ruby un.rb test.rb
```

### Step 7: Commit when all tests pass
```bash
git add un.rb tests/test_un_ruby.rb tests/test_sdk_library.py
git commit -m "Refactor Ruby SDK: add library exports + HMAC auth + caching

- Implement execute, executeAsync, wait, get_job, cancel_job, list_jobs
- Add 4-tier credential system (args > env > home > local)
- Add 1-hour cache for languages list
- Implement HMAC-SHA256 request signing
- Preserve original CLI functionality
- All unit tests passing"
```

## Questions?

If tests fail or you need clarification:
1. Check the error message - it usually indicates the problem
2. Review the example SDKs (un.py, un.js) for reference
3. Look at existing test files for patterns
4. Run individual tests to isolate issues

Good luck with your SDK refactoring!
