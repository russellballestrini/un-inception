# Python SDK Examples

This directory contains comprehensive examples for both synchronous and asynchronous usage of the unsandbox Python SDK.

## Setup

Before running examples, set your API credentials:

```bash
export UNSANDBOX_PUBLIC_KEY="your-public-key"
export UNSANDBOX_SECRET_KEY="your-secret-key"
```

Alternatively, save credentials to `~/.unsandbox/accounts.csv`:
```
public_key,secret_key
```

## Synchronous Examples (`sync/examples/`)

Synchronous examples use the standard `requests` library for blocking I/O operations.

### Basic Examples

#### hello_world.py
Simple code snippet that prints "Hello from unsandbox!"
```bash
# This is raw code to execute, not a SDK client example
python3 -c "print('Hello from unsandbox!')"
```

#### hello_world_client.py
SDK client example showing synchronous code execution.
```bash
python3 hello_world_client.py
# Expected output:
# Executing code synchronously...
# Result status: completed
# Output: Hello from unsandbox!
```

### Computational Examples

#### fibonacci.py
Simple recursive fibonacci implementation (raw code snippet).
```bash
# This is raw code, run via SDK
```

#### fibonacci_client.py
SDK client that executes fibonacci calculation in sandbox.
```bash
python3 fibonacci_client.py
# Expected output:
# Calculating fibonacci(10)...
# Result status: completed
# Output: fib(10) = 55
```

### Data Processing Examples

#### http_request.py
Demonstrates HTTP requests from sandboxed environment using requests library.
```bash
python3 http_request.py
# Expected output:
# Executing HTTP request in sandbox...
# === STDOUT ===
# Status Code: 200
# Response: {"origin": "..."}
```

Features:
- Uses `requests` library (pre-installed)
- Error handling for network failures
- JSON response parsing

#### json_processing.py
Shows JSON parsing and manipulation operations.
```bash
python3 json_processing.py
# Expected output:
# Executing JSON processing in sandbox...
# === STDOUT ===
# Original JSON: {"name": "Alice", "age": 30, ...}
# Parsed successfully!
# Name: Alice
# Age: 30
# Skills: Python, JavaScript
```

Features:
- JSON parsing with error handling
- Data manipulation and modification
- Re-serialization with formatting

#### file_operations.py
Demonstrates temporary file creation and manipulation.
```bash
python3 file_operations.py
# Expected output:
# Executing file operations in sandbox...
# === STDOUT ===
# File created at: /tmp/example.txt
# File exists: True
# File size: 84 bytes
# File contents:
# Line 1: Hello from the sandbox
# ...
```

Features:
- File writing with context managers
- File reading and parsing
- File system operations
- Proper error handling

## Asynchronous Examples (`async/examples/`)

Asynchronous examples use `asyncio` and `aiohttp` for concurrent operations.

### Basic Examples

#### hello_world_async.py
SDK client example showing asynchronous code execution.
```bash
python3 hello_world_async.py
# Expected output:
# Executing code asynchronously...
# Result status: completed
# Output: Hello from async unsandbox!
```

### Concurrent Computation

#### fibonacci_async.py
Runs multiple fibonacci calculations concurrently.
```bash
python3 fibonacci_async.py
# Expected output:
# Starting 3 concurrent fibonacci calculations...
# [fib-10] Result: fib(10) = 55
# [fib-15] Result: fib(15) = 610
# [fib-12] Result: fib(12) = 144
# All calculations completed!
```

Features:
- Multiple concurrent executions using `asyncio.gather()`
- Parallel CPU-bound operations
- Result aggregation

### Network Examples

#### concurrent_requests.py
Executes multiple HTTP requests concurrently.
```bash
python3 concurrent_requests.py
# Expected output:
# Starting 3 concurrent HTTP requests...
# [request-1] Status: 200, Response: {...}
# [request-2] Status: 200, Response: {...}
# [request-3] Status: 200, Response: {...}
# All requests completed successfully!
```

Features:
- Parallel network requests
- URL handling and error recovery
- Response processing
- Timeout handling

### Stream Processing

#### stream_processing.py
Demonstrates async generator patterns for data stream handling.
```bash
python3 stream_processing.py
# Expected output:
# Processing stream of data...
# [stream-task-1] Processed 10 items, sum: 45
# [stream-task-2] Processed 10 items, sum: 145
# [stream-task-3] Processed 10 items, sum: 245
# Stream processing completed!
```

Features:
- Async generator patterns
- Parallel stream processing
- Data aggregation

### Job Management Examples

#### async_job_polling.py
Shows how to manage async jobs with polling and cancellation.
```bash
python3 async_job_polling.py
# Expected output:
# 1. Starting async job...
#    Job ID: job_...
# 2. Checking job status...
#    Status: ...
# 3. Waiting for job completion...
#    Final status: completed
#    Output: Job result
# 4. Listing all jobs...
#    Total jobs: ...
```

Features:
- Fire-and-forget job submission
- Status polling with backoff
- Job cancellation
- Job listing

#### concurrent_execution.py
Runs multiple different code snippets concurrently across languages.
```bash
python3 concurrent_execution.py
# Expected output:
# Running 4 concurrent code executions...
# [python_hello] Result: Hello from Python
# [js_hello] Result: Hello from JavaScript
# [bash_hello] Result: Hello from Bash
# [python_math] Result: pi = 3.1416
# === Execution Summary ===
# python_hello: OK
# js_hello: OK
# bash_hello: OK
# python_math: OK
```

Features:
- Multi-language execution
- Concurrent task coordination
- Result aggregation

#### sync_blocking_usage.py
Shows how to use async SDK in both async and blocking contexts.
```bash
python3 sync_blocking_usage.py
# Expected output:
# === Async Approach ===
# Output: Hello from async
#
# === Sync Functions (in async context) ===
# Detected language for script.py: python
# Output: Executing python code
#
# === Mixed Sync/Async ===
# get_languages is available for fetching supported languages
# Output: Hello from mixed
```

Features:
- Async/await patterns
- Synchronous helper functions
- Mixed sync/async contexts
- Language detection

## Common Patterns

### Error Handling

All examples include proper error handling:

```python
try:
    result = execute_code("python", code, public_key, secret_key)
    if result.get("status") == "completed":
        print(result.get("stdout"))
    else:
        print(f"Failed: {result.get('error')}")
except CredentialsError as e:
    print(f"Credentials error: {e}")
except Exception as e:
    print(f"Error: {e}")
```

### Credential Resolution

Examples use the SDK's credential resolution system:

1. Environment variables (`UNSANDBOX_PUBLIC_KEY`, `UNSANDBOX_SECRET_KEY`)
2. `~/.unsandbox/accounts.csv`
3. `./accounts.csv`

### Concurrent Execution

For async examples, use `asyncio.gather()` for parallel operations:

```python
async def main():
    tasks = [
        execute_code("python", code1, pk, sk),
        execute_code("javascript", code2, pk, sk),
    ]
    results = await asyncio.gather(*tasks)
```

## Running Validation

Examples are designed to be validated by automated test scripts:

```bash
# Run all sync examples
for f in sync/examples/*_client.py; do
    python3 "$f" || echo "Failed: $f"
done

# Run all async examples
for f in async/examples/*.py; do
    python3 "$f" || echo "Failed: $f"
done
```

## Pre-installed Packages

All examples can use these pre-installed packages:

**Python:**
- requests (HTTP requests)
- json (data parsing)
- asyncio (async operations)
- aiohttp (async HTTP)
- numpy, scipy, pandas, matplotlib
- Beautiful Soup, requests, Pillow
- cryptography, pytest, and 20+ others

## Network Modes

Examples default to public endpoints, but note:

- **zerotrust**: No internet access (default)
- **semitrusted**: Internet via egress proxy (httpbin.org examples)

Request semitrusted mode when needed for your use case.

## Troubleshooting

### Module Import Errors

Ensure SDK path is set correctly:
```python
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))
```

### Timeout Errors

Increase timeout for long-running operations:
```python
result = wait_for_job(job_id, public_key, secret_key)  # Waits indefinitely with backoff
```

### Credentials Not Found

Set environment variables:
```bash
export UNSANDBOX_PUBLIC_KEY="key"
export UNSANDBOX_SECRET_KEY="secret"
```

Or create `~/.unsandbox/accounts.csv`:
```bash
mkdir -p ~/.unsandbox
echo "key,secret" > ~/.unsandbox/accounts.csv
chmod 600 ~/.unsandbox/accounts.csv
```

## Related Documentation

- [SDK API Reference](./README.md)
- [Architecture Overview](../../docs/ARCHITECTURE.md)
- [Language Support](../../docs/LANGUAGES.md)
