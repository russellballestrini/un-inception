# Unsandbox Python SDK (Sync) - Implementation Details

## Overview

The Unsandbox Python SDK (Synchronous) provides a complete, production-ready client library for executing code on unsandbox.com. The sync variant provides blocking/synchronous calls that wait for results.

## Architecture

### Core Module: `src/un.py`

The main implementation file containing all public APIs and supporting functions.

### Module Structure

```
src/
├── __init__.py          # Package exports and version
└── un.py                # Core implementation (712 lines)

tests/
├── __init__.py
├── test_credentials.py        # Credential resolution tests
├── test_language_detection.py # Language auto-detection tests
├── test_signatures.py         # HMAC-SHA256 signing tests
├── test_caching.py           # Languages cache tests
├── test_integration_mock.py   # Mocked API integration tests
└── test_real_world_scenarios.py # Real-world usage patterns
```

## Public API

### Execution Functions

1. **`execute_code(language, code, public_key=None, secret_key=None)`**
   - Executes code synchronously (blocks until completion)
   - Returns full result dict with stdout, stderr, exit_code, etc.
   - Internally uses polling with exponential backoff if job is pending/running

2. **`execute_async(language, code, public_key=None, secret_key=None)`**
   - Starts async execution (returns immediately with job_id)
   - Returns job ID string
   - Client can poll later with `get_job()` or `wait_for_job()`

### Job Management

3. **`get_job(job_id, public_key=None, secret_key=None)`**
   - Single poll for job status (no waiting)
   - Returns job result dict with current status
   - Used for manual polling

4. **`wait_for_job(job_id, public_key=None, secret_key=None)`**
   - Polls with exponential backoff until job completes
   - Blocking call that returns terminal status
   - Polling sequence: 300ms, 450ms, 700ms, 900ms, 650ms, 1600ms, 2000ms, ...

5. **`cancel_job(job_id, public_key=None, secret_key=None)`**
   - Cancels a running job
   - Returns confirmation dict

6. **`list_jobs(public_key=None, secret_key=None)`**
   - Lists all active jobs for authenticated account
   - Returns list of job dicts

### Language Support

7. **`get_languages(public_key=None, secret_key=None)`**
   - Returns list of supported languages
   - Results cached for 1 hour in `~/.unsandbox/languages.json`
   - API call only on cache miss

8. **`detect_language(filename)`**
   - Detects language from file extension
   - Maps 40+ file extensions to language identifiers
   - Returns None for unknown extensions
   - No authentication required (purely local)

### Snapshots

9. **`session_snapshot(session_id, public_key=None, secret_key=None, name=None, hot=False)`**
   - Creates snapshot of a session
   - Returns snapshot_id

10. **`service_snapshot(service_id, public_key=None, secret_key=None, name=None, hot=False)`**
    - Creates snapshot of a service
    - Returns snapshot_id

11. **`list_snapshots(public_key=None, secret_key=None)`**
    - Lists all snapshots for account
    - Returns list of snapshot dicts

12. **`restore_snapshot(snapshot_id, public_key=None, secret_key=None)`**
    - Restores a snapshot
    - Returns restoration result dict

13. **`delete_snapshot(snapshot_id, public_key=None, secret_key=None)`**
    - Deletes a snapshot permanently
    - Returns deletion confirmation

## Authentication System

### 4-Tier Credential Resolution

The SDK checks credentials in this priority order:

1. **Function Arguments** - Highest priority
   ```python
   execute_code("python", code, public_key="pk_...", secret_key="sk_...")
   ```

2. **Environment Variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY="pk_..."
   export UNSANDBOX_SECRET_KEY="sk_..."
   ```

3. **Config File** - `~/.unsandbox/accounts.csv`
   ```csv
   public_key_1,secret_key_1
   public_key_2,secret_key_2
   ```
   Select account with `UNSANDBOX_ACCOUNT=N` env var (0-based)

4. **Local Directory** - `./accounts.csv` (lowest priority)
   Same CSV format as config file

### HMAC-SHA256 Request Signing

Every API request is authenticated using HMAC-SHA256:

**Headers:**
- `Authorization: Bearer <public_key>` - Identifies account
- `X-Timestamp: <unix_seconds>` - Prevents replay attacks
- `X-Signature: <hmac_sha256>` - Proves secret + body integrity
- `Content-Type: application/json` - Declares content type

**Message Format:**
```
"timestamp:METHOD:path:body"
```

Example:
```
1234567890:POST:/execute:{"language":"python","code":"print(42)"}
```

The HMAC-SHA256 is computed over the entire message using the secret key.

## Caching

### Languages Cache

The `get_languages()` function caches results to reduce API calls:

- **Location**: `~/.unsandbox/languages.json`
- **TTL**: 3600 seconds (1 hour)
- **Format**: JSON with `languages` list and `timestamp`
- **Behavior**: Returns cached list if fresh, fetches from API otherwise

Cache file format:
```json
{
  "languages": ["python", "javascript", "go", ...],
  "timestamp": 1705337400
}
```

Cache expiration check uses file modification time, not stored timestamp.

## Language Detection

The `detect_language()` function maps file extensions to language names:

Supported extensions (40+):
- Python: `.py`
- JavaScript: `.js`
- TypeScript: `.ts`
- Go: `.go`
- Rust: `.rs`
- C: `.c`
- C++: `.cpp`, `.cc`, `.cxx`
- Java: `.java`
- Ruby: `.rb`
- PHP: `.php`
- Bash: `.sh`
- And many more...

Returns `None` for:
- Files without extensions
- Unknown extensions
- Empty filenames

## Error Handling

### Exception Types

1. **`CredentialsError`**
   - Raised when credentials cannot be found
   - Includes helpful message with resolution tiers

2. **`requests.RequestException`**
   - Network errors (connection failed, timeout, etc.)
   - Subclass: `requests.Timeout` for timeouts

3. **`ValueError`**
   - Invalid response format from API
   - JSON parsing failures

### Error Response Handling

API errors return appropriate HTTP status codes:
- `401 Unauthorized` - Invalid API key
- `429 Too Many Requests` - Rate limit exceeded
- `500 Internal Server Error` - Server error

All errors are converted to appropriate Python exceptions.

## Request Handling

### HTTP Methods

The SDK uses standard HTTP methods:
- **POST** - Create/execute (requests with body)
- **GET** - Retrieve/status (no body)
- **DELETE** - Cancel/delete (no body)

### Timeouts

All requests have a 120-second timeout to prevent hanging.

### Polling Strategy

Exponential backoff for job polling:

```
Poll 1: wait 300ms   → cumulative 300ms
Poll 2: wait 450ms   → cumulative 750ms
Poll 3: wait 700ms   → cumulative 1450ms
Poll 4: wait 900ms   → cumulative 2350ms
Poll 5: wait 650ms   → cumulative 3000ms
Poll 6: wait 1600ms  → cumulative 4600ms
Poll 7: wait 2000ms  → cumulative 6600ms
Poll 8+: wait 2000ms → cap at 2000ms per poll
```

This strategy balances:
- Fast response for quick-executing jobs
- Reduced API load for long-running jobs
- Maximum wait between polls capped at 2 seconds

## Testing

### Test Suite Structure

1. **Unit Tests**
   - `test_credentials.py` - Credential resolution (6 tests)
   - `test_language_detection.py` - Language detection (15 tests)
   - `test_signatures.py` - Request signing (10 tests)
   - `test_caching.py` - Language caching (9 tests)

2. **Integration Tests (Mocked)**
   - `test_integration_mock.py` - API integration with mocked responses (13 tests)
   - `test_real_world_scenarios.py` - Real-world usage patterns (11 tests)

Total: 64+ test cases covering all public APIs

### Running Tests

Without pytest installed (requires system Python):
```bash
python3 verify_sdk.py  # Verification script
```

With pytest:
```bash
pip install pytest
cd sync
pytest tests/ -v
pytest tests/ --cov=un  # With coverage
```

## File Structure

```
clients/python/sync/
├── src/
│   ├── __init__.py              # Package exports
│   └── un.py                    # Core implementation (712 lines)
├── tests/
│   ├── __init__.py
│   ├── test_credentials.py
│   ├── test_language_detection.py
│   ├── test_signatures.py
│   ├── test_caching.py
│   ├── test_integration_mock.py
│   └── test_real_world_scenarios.py
├── examples/
│   ├── hello_world.py           # Simple code example
│   ├── hello_world_client.py    # SDK client example
│   ├── fibonacci.py             # Recursive function example
│   ├── fibonacci_client.py      # SDK client example
│   ├── json_processing.py       # JSON example
│   ├── http_request.py          # HTTP request example
│   └── file_operations.py       # File I/O example
├── setup.py                     # Package configuration
├── README.md                    # API reference
├── USAGE.md                     # Usage guide
├── IMPLEMENTATION.md            # This file
├── LICENSE                      # Public domain license
├── MANIFEST.in                  # Distribution manifest
├── pytest.ini                   # Pytest configuration
└── verify_sdk.py                # Verification script
```

## Key Implementation Details

### Credential Manager

Function `_resolve_credentials()` implements 4-tier resolution:
- Returns tuple of (public_key, secret_key)
- Raises `CredentialsError` if not found
- Supports per-account selection via `UNSANDBOX_ACCOUNT` env var

### Request Signer

Function `_sign_request()` generates HMAC-SHA256 signatures:
- Message format: `"{timestamp}:{method}:{path}:{body}"`
- Body only included for POST requests
- Returns 64-character hex string

### HTTP Client

Function `_make_request()` handles all HTTP communication:
- Constructs URL from base + path
- Adds authentication headers
- Raises on HTTP errors
- Parses JSON response
- Timeout: 120 seconds for all requests

### Cache Manager

- `_get_languages_cache_path()` - Returns `~/.unsandbox/languages.json`
- `_load_languages_cache()` - Loads if fresh, returns None if expired/missing
- `_save_languages_cache()` - Saves with timestamp, catches all errors silently

### Language Mapping

Dictionary `_LANGUAGE_MAP` provides 40+ file extension → language mappings
- Case-insensitive (converted to lowercase)
- Handles multiple extensions for same language (e.g., .cc, .cxx → cpp)

## Dependencies

**Runtime:**
- `requests >= 2.25.0` - HTTP client

**Development (optional):**
- `pytest >= 6.0` - Testing framework
- `pytest-cov >= 2.0` - Coverage reporting
- `black >= 21.0` - Code formatter
- `flake8 >= 3.9` - Linter
- `mypy >= 0.900` - Type checker

**Built-in:**
- `hashlib` - HMAC-SHA256 signing
- `json` - JSON serialization
- `os` - Environment variables
- `time` - Timestamps and delays
- `pathlib` - File path handling
- `typing` - Type hints

## Performance Characteristics

### Synchronous Execution
- First execution: 5-7 seconds (container cold start)
- Subsequent: 1-2 seconds (warm pool)
- Language detection: < 1ms (local)
- Credential resolution: < 1ms (local)

### Async Execution
- Job start: < 100ms (immediate return)
- Polling overhead: ~50ms per poll
- First poll: 300ms wait
- Typical job poll: 450-2000ms between attempts

### Caching
- Cache hit: < 1ms (file I/O)
- Cache miss (API): 100-500ms (network)

## Public Domain License

This code is released into the PUBLIC DOMAIN with NO WARRANTY and NO LICENSE.

You are free to:
- Use for any purpose
- Modify and distribute
- Use commercially
- Use privately

There are no restrictions, warranties, or conditions attached to this code.

## Version Information

- **Version**: 1.0.0
- **Python**: 3.8+ (3.8, 3.9, 3.10, 3.11, 3.12 tested)
- **Status**: Production-ready
- **Last Updated**: 2024-01-15

## Known Limitations

1. **No streaming output** - Results are buffered until job completion
2. **No cancellation guarantee** - Cancelled jobs may still produce output
3. **No job history** - Only active jobs are listed
4. **Cache directory dependency** - Requires write access to `~/.unsandbox/`
5. **Synchronous polling only** - No websocket/SSE for real-time updates

## Future Enhancements

Potential additions (not implemented):
- Streaming output support
- Real-time job monitoring via websocket
- Job history API
- Custom timeout configuration
- Retry logic with exponential backoff
- Async/await support (use async SDK instead)
- Batch API calls
- Progress callbacks
