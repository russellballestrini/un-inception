# Async Python SDK Usage Guide

Complete guide to using the unsandbox async Python SDK with real-world examples.

## Table of Contents

1. [Installation](#installation)
2. [Basic Usage](#basic-usage)
3. [Authentication](#authentication)
4. [Execution Patterns](#execution-patterns)
5. [Advanced Examples](#advanced-examples)
6. [Error Handling](#error-handling)
7. [Performance Optimization](#performance-optimization)

## Installation

### Requirements

- Python 3.7+
- `aiohttp` (automatically installed)

### Setup

```bash
cd clients/python/async
pip install -e "."
```

Or for development with tests:

```bash
pip install -e ".[dev]"
```

## Basic Usage

### Simple Execution

Execute code and wait for completion:

```python
import asyncio
from un_async import execute_code

async def main():
    result = await execute_code(
        language="python",
        code='print("Hello, World!")'
    )

    print(f"Status: {result['status']}")
    print(f"Output: {result['stdout']}")

asyncio.run(main())
```

### Using Credentials

```python
import asyncio
from un_async import execute_code

async def main():
    # Credentials from environment variables
    # or use function arguments
    result = await execute_code(
        language="python",
        code='print("hello")',
        public_key="your_public_key",
        secret_key="your_secret_key"
    )
    print(result["stdout"])

asyncio.run(main())
```

## Authentication

### 4-Tier Credential System

Credentials are resolved in priority order:

#### 1. Function Arguments (Highest Priority)

```python
result = await execute_code(
    "python",
    "print('hello')",
    public_key="your_pk",
    secret_key="your_sk"
)
```

#### 2. Environment Variables

```bash
export UNSANDBOX_PUBLIC_KEY="your_pk"
export UNSANDBOX_SECRET_KEY="your_sk"
python script.py
```

#### 3. Config File (`~/.unsandbox/accounts.csv`)

```bash
# Create config
mkdir -p ~/.unsandbox
echo "your_pk,your_sk" > ~/.unsandbox/accounts.csv
```

#### 4. Local File (`./accounts.csv`)

```bash
echo "your_pk,your_sk" > ./accounts.csv
```

### Multiple Accounts

```bash
# In ~/.unsandbox/accounts.csv
account1_pk,account1_sk
account2_pk,account2_sk
account3_pk,account3_sk
```

```python
import os

# Use second account
os.environ["UNSANDBOX_ACCOUNT"] = "1"

result = await execute_code("python", "print('from account 2')")
```

## Execution Patterns

### Pattern 1: Simple Sync Execution

Wait for code to complete:

```python
import asyncio
from un_async import execute_code

async def run():
    result = await execute_code("python", """
        import random
        print(random.randint(1, 100))
    """)
    return result

# Run from sync context
result = asyncio.run(run())
print(f"Random number: {result['stdout'].strip()}")
```

### Pattern 2: Fire-and-Forget

Start job and retrieve later:

```python
import asyncio
from un_async import execute_async, get_job, wait_for_job

async def main():
    # Start the job
    job_id = await execute_async("python", "print('working...')")
    print(f"Started job: {job_id}")

    # Do other work
    await asyncio.sleep(1)

    # Check status
    status = await get_job(job_id)
    print(f"Job status: {status['status']}")

    # Wait for completion
    result = await wait_for_job(job_id)
    print(f"Result: {result['stdout']}")

asyncio.run(main())
```

### Pattern 3: Concurrent Execution

Run multiple jobs in parallel:

```python
import asyncio
from un_async import execute_code

async def main():
    # Execute 3 jobs concurrently
    languages = ["python", "javascript", "go"]
    codes = [
        "print('Python says hello')",
        "console.log('JS says hello')",
        "fmt.Println(\"Go says hello\")",
    ]

    tasks = [
        execute_code(lang, code)
        for lang, code in zip(languages, codes)
    ]

    results = await asyncio.gather(*tasks)

    for result in results:
        print(f"Output: {result['stdout']}")

asyncio.run(main())
```

### Pattern 4: Batch Processing

Process a list of code snippets:

```python
import asyncio
from un_async import execute_code

async def process_snippets(snippets):
    tasks = []
    for lang, code in snippets:
        task = execute_code(lang, code)
        tasks.append(task)

    results = await asyncio.gather(*tasks, return_exceptions=True)
    return results

async def main():
    snippets = [
        ("python", "print('1 + 1 =', 1 + 1)"),
        ("python", "print('2 * 3 =', 2 * 3)"),
        ("python", "print('10 / 2 =', 10 / 2)"),
    ]

    results = await process_snippets(snippets)

    for i, result in enumerate(results):
        if isinstance(result, Exception):
            print(f"Error in snippet {i}: {result}")
        else:
            print(f"Result {i}: {result['stdout'].strip()}")

asyncio.run(main())
```

### Pattern 5: Timeout Handling

```python
import asyncio
from un_async import execute_code

async def execute_with_timeout(language, code, timeout_sec=30):
    try:
        result = await asyncio.wait_for(
            execute_code(language, code),
            timeout=timeout_sec
        )
        return result
    except asyncio.TimeoutError:
        return {"error": "Execution timed out", "status": "timeout"}

async def main():
    # This will timeout if execution takes > 10 seconds
    result = await execute_with_timeout(
        "python",
        "import time; time.sleep(5); print('done')",
        timeout_sec=10
    )

    print(result)

asyncio.run(main())
```

## Advanced Examples

### Example 1: Streaming Job Status

Monitor a long-running job:

```python
import asyncio
from un_async import execute_async, get_job

async def main():
    print("Starting long-running job...")
    job_id = await execute_async("python", """
        import time
        for i in range(5):
            print(f"Step {i+1}/5")
            time.sleep(1)
        print("Done!")
    """)

    # Poll status every 2 seconds
    while True:
        status = await get_job(job_id)
        print(f"Status: {status['status']}")

        if status['status'] in ('completed', 'failed', 'timeout', 'cancelled'):
            print(f"Final output:\n{status['stdout']}")
            break

        await asyncio.sleep(2)

asyncio.run(main())
```

### Example 2: Retry Logic

```python
import asyncio
from un_async import execute_code
import aiohttp

async def execute_with_retry(language, code, max_retries=3):
    for attempt in range(max_retries):
        try:
            result = await execute_code(language, code)
            return result
        except aiohttp.ClientError as e:
            if attempt == max_retries - 1:
                raise
            wait_time = 2 ** attempt  # exponential backoff
            print(f"Attempt {attempt + 1} failed, retrying in {wait_time}s...")
            await asyncio.sleep(wait_time)

async def main():
    result = await execute_with_retry("python", "print('Success!')")
    print(result['stdout'])

asyncio.run(main())
```

### Example 3: Pipeline Processing

```python
import asyncio
from un_async import execute_code

async def main():
    # Stage 1: Generate data
    print("Stage 1: Generating data...")
    gen_result = await execute_code("python", """
        import json
        data = {"values": [1, 2, 3, 4, 5]}
        print(json.dumps(data))
    """)

    # Stage 2: Process data
    print("Stage 2: Processing data...")
    proc_result = await execute_code("python", """
        import json
        data = {"values": [1, 2, 3, 4, 5]}
        total = sum(data["values"])
        print(f"Total: {total}")
    """)

    # Stage 3: Analyze results
    print("Stage 3: Analysis complete")
    print(gen_result['stdout'])
    print(proc_result['stdout'])

asyncio.run(main())
```

### Example 4: Language Detection

```python
import asyncio
from un_async import detect_language, execute_code
import os

async def execute_file_async(filepath):
    """Execute any file based on its extension."""

    # Detect language
    language = detect_language(filepath)
    if not language:
        raise ValueError(f"Unknown language for file: {filepath}")

    # Read file
    with open(filepath, 'r') as f:
        code = f.read()

    # Execute
    result = await execute_code(language, code)
    return result

async def main():
    # Create test files
    with open('/tmp/test.py', 'w') as f:
        f.write("print('Hello from Python')")

    with open('/tmp/test.js', 'w') as f:
        f.write("console.log('Hello from JavaScript')")

    # Execute both concurrently
    results = await asyncio.gather(
        execute_file_async('/tmp/test.py'),
        execute_file_async('/tmp/test.js'),
    )

    for result in results:
        print(result['stdout'].strip())

asyncio.run(main())
```

## Error Handling

### Common Errors

```python
import asyncio
from un_async import (
    execute_code,
    CredentialsError,
    execute_async,
    get_job,
)
import aiohttp

async def main():
    # Error 1: Missing credentials
    try:
        result = await execute_code("python", "print('hello')")
    except CredentialsError as e:
        print(f"Credentials error: {e}")

    # Error 2: Network error
    try:
        result = await execute_code("python", "print('hello')")
    except aiohttp.ClientError as e:
        print(f"Network error: {e}")

    # Error 3: Execution error
    result = await execute_code("python", "raise Exception('oops')")
    if result['status'] == 'failed':
        print(f"Execution failed: {result['stderr']}")
        print(f"Exit code: {result['exit_code']}")

    # Error 4: Job not found
    try:
        result = await get_job("nonexistent_job_id")
    except Exception as e:
        print(f"Job lookup error: {e}")

asyncio.run(main())
```

### Exception Handling in Concurrent Tasks

```python
import asyncio
from un_async import execute_code

async def main():
    tasks = [
        execute_code("python", "print(1)"),
        execute_code("python", "raise Exception('boom')"),
        execute_code("python", "print(3)"),
    ]

    # Collect exceptions instead of failing
    results = await asyncio.gather(*tasks, return_exceptions=True)

    for i, result in enumerate(results):
        if isinstance(result, Exception):
            print(f"Task {i} failed: {result}")
        else:
            print(f"Task {i} output: {result['stdout'].strip()}")

asyncio.run(main())
```

## Performance Optimization

### 1. Connection Pooling

```python
import asyncio
import aiohttp
from un_async import execute_code

async def main():
    # Create a session to reuse TCP connections
    connector = aiohttp.TCPConnector(limit=10, limit_per_host=5)
    async with aiohttp.ClientSession(connector=connector) as session:
        # Use session for multiple operations
        tasks = [execute_code("python", f"print({i})") for i in range(10)]
        results = await asyncio.gather(*tasks)
```

### 2. Batch Similar Operations

```python
import asyncio
from un_async import execute_code

async def main():
    # Group similar operations for better parallelism
    python_tasks = [
        execute_code("python", f"print({i})")
        for i in range(5)
    ]

    js_tasks = [
        execute_code("javascript", f"console.log({i})")
        for i in range(5)
    ]

    # Run all concurrently
    results = await asyncio.gather(*python_tasks, *js_tasks)
```

### 3. Limit Concurrent Requests

```python
import asyncio
from un_async import execute_code

async def limited_gather(*tasks, limit=5):
    """Run tasks with concurrency limit."""
    semaphore = asyncio.Semaphore(limit)

    async def bounded_task(task):
        async with semaphore:
            return await task

    return await asyncio.gather(*[bounded_task(t) for t in tasks])

async def main():
    tasks = [
        execute_code("python", f"print({i})")
        for i in range(100)
    ]

    # Only run 5 concurrent requests
    results = await limited_gather(*tasks, limit=5)
```

### 4. Caching

```python
import asyncio
from un_async import get_languages

async def main():
    # First call fetches from API
    langs1 = await get_languages()
    print(f"First call: {len(langs1)} languages")

    # Second call uses cached result (~1 hour TTL)
    langs2 = await get_languages()
    print(f"Second call: {len(langs2)} languages (from cache)")
```

## Best Practices

1. **Always use async context managers** for aiohttp sessions
2. **Handle exceptions properly** with try/except or `return_exceptions=True`
3. **Use concurrent execution** for multiple independent operations
4. **Implement exponential backoff** for network errors
5. **Cache credentials** to avoid repeated file I/O
6. **Monitor job status** rather than hammering the API
7. **Set timeouts** on long-running operations
8. **Reuse connections** with session pooling

## Debugging

Enable debug logging:

```python
import logging
import asyncio
from un_async import execute_code

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)

async def main():
    result = await execute_code("python", "print('debug mode')")
    print(result)

asyncio.run(main())
```
