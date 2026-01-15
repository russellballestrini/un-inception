# Unsandbox Async Python SDK

Asynchronous Python SDK for [unsandbox.com](https://unsandbox.com) code execution service.

Execute code in 50+ programming languages with full async/await support in Python.

## Features

- **Fully Asynchronous**: Built on `aiohttp` for efficient concurrent I/O
- **50+ Languages**: Python, JavaScript, Go, Rust, Java, C/C++, and 44+ more
- **Flexible Execution**: Sync execution (blocks until completion) or async (fire-and-forget)
- **Job Management**: Poll, wait, cancel running jobs
- **Credential Management**: 4-tier credential resolution system
- **Request Signing**: HMAC-SHA256 authentication
- **Language Detection**: Automatic language detection from filenames
- **Caching**: Built-in language list caching
- **Concurrent Execution**: Execute multiple jobs concurrently with `asyncio.gather()`

## Installation

```bash
# Clone the repository
git clone https://github.com/unsandbox/un-inception
cd clients/python/async

# Install with development dependencies
pip install -e ".[dev]"

# Or install with just aiohttp
pip install -r requirements.txt
```

## Quick Start

### Basic Async Execution

```python
import asyncio
from un_async import execute_code

async def main():
    # Execute code and wait for completion
    result = await execute_code("python", 'print("Hello World")')
    print(result["stdout"])

asyncio.run(main())
```

### Fire-and-Forget with Polling

```python
import asyncio
from un_async import execute_async, wait_for_job

async def main():
    # Start execution (returns immediately)
    job_id = await execute_async("javascript", 'console.log("Job started")')
    print(f"Job ID: {job_id}")

    # Poll for completion
    result = await wait_for_job(job_id)
    print(f"Status: {result['status']}")
    print(f"Output: {result['stdout']}")

asyncio.run(main())
```

### Concurrent Execution

```python
import asyncio
from un_async import execute_code

async def main():
    # Run multiple executions concurrently
    results = await asyncio.gather(
        execute_code("python", "print('Python')"),
        execute_code("javascript", "console.log('JavaScript')"),
        execute_code("go", 'fmt.Println("Go")'),
    )

    for result in results:
        print(f"Language: {result['language']}, Output: {result['stdout']}")

asyncio.run(main())
```

## Credential Management (4-Tier Priority)

Credentials are resolved in the following order:

1. **Function Arguments** (highest priority)
   ```python
   result = await execute_code(
       "python",
       "print('hello')",
       public_key="your_public_key",
       secret_key="your_secret_key"
   )
   ```

2. **Environment Variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY="your_public_key"
   export UNSANDBOX_SECRET_KEY="your_secret_key"
   python script.py
   ```

3. **Config File** (`~/.unsandbox/accounts.csv`)
   ```
   public_key_1,secret_key_1
   public_key_2,secret_key_2
   # Select account with: export UNSANDBOX_ACCOUNT=1
   ```

4. **Local Directory** (`./accounts.csv`)
   Same format as config file

### Using Multiple Accounts

```bash
# List accounts in ~/.unsandbox/accounts.csv
# Use the second account (0-indexed)
export UNSANDBOX_ACCOUNT=1
python script.py
```

## API Reference

### Execution Functions

#### `execute_code(language, code, public_key=None, secret_key=None)`

Execute code synchronously and wait for completion.

**Args:**
- `language` (str): Programming language (e.g., "python", "javascript")
- `code` (str): Source code to execute
- `public_key` (str, optional): API public key
- `secret_key` (str, optional): API secret key

**Returns:** Dict with execution result

**Raises:** `CredentialsError`, `aiohttp.ClientError`

```python
result = await execute_code("python", "print(42)")
print(result["stdout"])  # "42\n"
print(result["exit_code"])  # 0
```

#### `execute_async(language, code, public_key=None, secret_key=None)`

Execute code asynchronously and return immediately with job ID.

**Args:** Same as `execute_code()`

**Returns:** Job ID (str)

```python
job_id = await execute_async("python", "print('starting')")
# Do other work while job runs...
result = await wait_for_job(job_id)
```

### Job Management Functions

#### `get_job(job_id, public_key=None, secret_key=None)`

Get current status of a job (single poll, no waiting).

**Args:**
- `job_id` (str): Job ID to check
- `public_key`, `secret_key` (optional)

**Returns:** Dict with job status

```python
status = await get_job(job_id)
print(status["status"])  # "running", "completed", "failed", etc.
```

#### `wait_for_job(job_id, public_key=None, secret_key=None)`

Wait for job completion with exponential backoff polling.

**Polling Delays (ms):** [300, 450, 700, 900, 650, 1600, 2000, ...]

**Args:** Same as `get_job()`

**Returns:** Dict with final job result

```python
result = await wait_for_job(job_id)
if result["status"] == "completed":
    print(result["stdout"])
```

#### `cancel_job(job_id, public_key=None, secret_key=None)`

Cancel a running job.

**Args:**
- `job_id` (str): Job ID to cancel
- `public_key`, `secret_key` (optional)

**Returns:** Dict with cancellation confirmation

```python
result = await cancel_job(job_id)
print(result["status"])  # "cancelled"
```

#### `list_jobs(public_key=None, secret_key=None)`

List all jobs for the authenticated account.

**Args:** `public_key`, `secret_key` (optional)

**Returns:** List of job dicts

```python
jobs = await list_jobs()
for job in jobs:
    print(f"Job {job['id']}: {job['status']}")
```

### Metadata Functions

#### `get_languages(public_key=None, secret_key=None)`

Get list of supported programming languages.

Results are cached for 1 hour in `~/.unsandbox/languages.json`.

**Args:** `public_key`, `secret_key` (optional)

**Returns:** List of language identifiers

```python
languages = await get_languages()
print(f"Supported languages: {', '.join(languages)}")
```

#### `detect_language(filename)`

Detect programming language from filename extension.

**Args:**
- `filename` (str): Filename to detect (e.g., "script.py")

**Returns:** Language identifier or None

```python
lang = detect_language("app.js")  # "javascript"
lang = detect_language("main.go")  # "go"
lang = detect_language("unknown")  # None
```

### Snapshot Functions

#### `session_snapshot(session_id, public_key=None, secret_key=None, name=None, hot=False)`

Create a snapshot of a session.

**Args:**
- `session_id` (str): Session ID to snapshot
- `name` (str, optional): Snapshot name
- `hot` (bool, optional): Hot snapshot (snapshot running session)

**Returns:** Snapshot ID (str)

#### `list_snapshots(public_key=None, secret_key=None)`

List all snapshots.

**Returns:** List of snapshot dicts

#### `restore_snapshot(snapshot_id, public_key=None, secret_key=None)`

Restore a snapshot.

**Args:**
- `snapshot_id` (str): Snapshot ID to restore

**Returns:** Dict with restored resource info

#### `delete_snapshot(snapshot_id, public_key=None, secret_key=None)`

Delete a snapshot.

**Args:**
- `snapshot_id` (str): Snapshot ID to delete

**Returns:** Dict with deletion confirmation

## Response Format

### Successful Execution

```python
{
    "job_id": "job_abc123",
    "status": "completed",
    "stdout": "output text\n",
    "stderr": "",
    "exit_code": 0,
    "language": "python",
    "duration_ms": 234
}
```

### Failed Execution

```python
{
    "job_id": "job_xyz789",
    "status": "failed",
    "stdout": "partial output",
    "stderr": "Error message\n",
    "exit_code": 1,
    "language": "python",
    "duration_ms": 567
}
```

### Job Statuses

- `pending` - Waiting to execute
- `running` - Currently executing
- `completed` - Finished successfully
- `failed` - Execution error
- `timeout` - Exceeded time limit
- `cancelled` - Cancelled by user

## Examples

See the `examples/` directory for complete working examples:

- `hello_world_async.py` - Basic async execution
- `fibonacci_async.py` - Concurrent fibonacci calculations
- `concurrent_execution.py` - Running multiple jobs concurrently
- `async_job_polling.py` - Fire-and-forget job management
- `sync_blocking_usage.py` - Using sync functions from async library

## Testing

Run the test suite:

```bash
# Install dev dependencies
pip install -e ".[dev]"

# Run all tests
pytest tests/

# Run with verbose output
pytest tests/ -v

# Run specific test file
pytest tests/test_language_detection.py

# Run with coverage
pytest tests/ --cov=un_async
```

### Test Files

- `test_credentials.py` - Credential resolution system
- `test_language_detection.py` - Language detection
- `test_async_operations.py` - Async API operations
- `test_hmac_signing.py` - HMAC request signing
- `conftest.py` - Shared fixtures

## Supported Languages

**50+ Languages** including:

**Interpreted:** Python, JavaScript, Ruby, PHP, Perl, Bash, Lua, R, Julia, Scheme, Tcl, Raku, Clojure, Groovy, Crystal, Dart, Elixir, Erlang, Haskell, OCaml, Common Lisp, Forth, Prolog, and more

**Compiled:** C, C++, Go, Rust, Java, Kotlin, C#, D, Nim, Zig, V, Pascal, Fortran, COBOL, Objective-C, and more

**Specialized:** TypeScript, F#, Odin

Use `detect_language()` for automatic detection or get full list with `await get_languages()`.

## Error Handling

```python
from un_async import CredentialsError
import aiohttp

try:
    result = await execute_code("python", "print('hello')")
except CredentialsError as e:
    print(f"Credentials error: {e}")
except aiohttp.ClientError as e:
    print(f"Network error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
```

## Performance Tips

1. **Use Concurrent Execution** for multiple independent jobs:
   ```python
   results = await asyncio.gather(
       execute_code("python", "..."),
       execute_code("go", "..."),
       execute_code("rust", "..."),
   )
   ```

2. **Use Exponential Backoff** with `wait_for_job()` instead of polling manually

3. **Cache Languages** - `get_languages()` caches results for 1 hour

4. **Reuse Session** - Create one `aiohttp.ClientSession` for multiple requests:
   ```python
   async with aiohttp.ClientSession() as session:
       # Reuse session for multiple operations
   ```

## Differences from Sync SDK

This async SDK provides the same API as the sync version but with async/await:

**Sync SDK:**
```python
from un import execute_code
result = execute_code("python", "print('hello')")
```

**Async SDK:**
```python
from un_async import execute_code
result = await execute_code("python", "print('hello')")
```

Key differences:
- All I/O functions are async (require `await`)
- Use `asyncio.run()` to execute from sync context
- Use `asyncio.gather()` for concurrent operations
- Built on `aiohttp` instead of `requests`
- Same credential system and HMAC signing

## License

Public Domain - NO LICENSE, NO WARRANTY

## Support

Visit [unsandbox.com](https://unsandbox.com) for API documentation and support.
