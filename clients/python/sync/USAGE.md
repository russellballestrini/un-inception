# Unsandbox Python SDK (Synchronous) - Usage Guide

## Overview

The Unsandbox Python SDK provides a synchronous (blocking) interface for executing code on unsandbox.com. Unlike the async SDK, the sync SDK makes blocking calls and waits for code execution to complete.

## Installation

### From Source

```bash
cd clients/python/sync
pip install -e .
```

### Development Setup

```bash
cd clients/python/sync
pip install -e ".[dev]"
```

## Quick Examples

### 1. Basic Synchronous Execution

Execute code and wait for completion:

```python
from un import execute_code

result = execute_code("python", "print('hello')")
print(result)
# {
#     'status': 'completed',
#     'stdout': 'hello\n',
#     'stderr': '',
#     'exit_code': 0,
#     'runtime_ms': 342
# }
```

### 2. Async Execution with Polling

Start execution and manually poll:

```python
from un import execute_async, wait_for_job

# Start execution (returns immediately)
job_id = execute_async("python", "print('working...')")
print(f"Job started: {job_id}")

# Later, poll for completion
result = wait_for_job(job_id)
print(result)
```

### 3. Check Job Status

Get current status without waiting:

```python
from un import get_job

job = get_job("job_123")
print(f"Status: {job['status']}")
if job['status'] == 'completed':
    print(f"Output: {job['stdout']}")
```

### 4. Cancel a Job

Stop a running job:

```python
from un import execute_async, cancel_job

job_id = execute_async("python", "import time; time.sleep(100)")
cancel_job(job_id)
```

### 5. List Active Jobs

Get all jobs for your account:

```python
from un import list_jobs

jobs = list_jobs()
for job in jobs:
    print(f"{job['job_id']}: {job['status']}")
```

### 6. Detect Language from Filename

Automatically determine language:

```python
from un import detect_language, execute_code

# Detect language
lang = detect_language("script.py")  # Returns "python"

# Use detected language
if lang:
    result = execute_code(lang, "print('hello')")
```

### 7. Get Supported Languages

List all available languages:

```python
from un import get_languages

languages = get_languages()
print(f"Supported languages: {', '.join(languages)}")
```

### 8. Snapshots (Save/Restore Sessions)

```python
from un import session_snapshot, list_snapshots, restore_snapshot

# Create a snapshot
snapshot_id = session_snapshot(
    "session_123",
    name="checkpoint_before_experiment"
)

# List all snapshots
snapshots = list_snapshots()
for snap in snapshots:
    print(f"{snap['id']}: {snap['name']}")

# Restore a snapshot
result = restore_snapshot(snapshot_id)
```

## Authentication

### 1. Function Arguments (Highest Priority)

```python
result = execute_code(
    "python",
    "print('hello')",
    public_key="your_public_key",
    secret_key="your_secret_key"
)
```

### 2. Environment Variables

```bash
export UNSANDBOX_PUBLIC_KEY="your_public_key"
export UNSANDBOX_SECRET_KEY="your_secret_key"
```

```python
from un import execute_code

# No args needed - uses environment variables
result = execute_code("python", "print('hello')")
```

### 3. Configuration File

Create `~/.unsandbox/accounts.csv`:

```csv
public_key_1,secret_key_1
public_key_2,secret_key_2
```

Then:

```python
from un import execute_code

# Uses first account (line 0)
result = execute_code("python", "print('hello')")

# To use second account:
import os
os.environ["UNSANDBOX_ACCOUNT"] = "1"
result = execute_code("python", "print('hello')")
```

### 4. Local accounts.csv (Lowest Priority)

Create `./accounts.csv` in your project directory:

```csv
public_key,secret_key
```

```python
from un import execute_code

result = execute_code("python", "print('hello')")
```

## Error Handling

```python
from un import execute_code, CredentialsError
import requests

try:
    result = execute_code("python", "print('hello')")
except CredentialsError:
    print("No credentials found - check environment or config files")
except requests.Timeout:
    print("Request timed out - API may be down")
except requests.RequestException as e:
    print(f"Network error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
```

## Checking Execution Results

All execution results have the same structure:

```python
result = execute_code("python", "print('hello')")

# Common fields:
result['status']       # 'completed', 'running', 'pending', 'failed', 'timeout', 'cancelled'
result['stdout']       # Standard output as string
result['stderr']       # Standard error as string
result['exit_code']    # Process exit code (0 = success)
result['runtime_ms']   # Execution time in milliseconds
result['job_id']       # Unique job identifier
```

## Advanced Usage

### Executing Large Programs

```python
with open("my_script.py", "r") as f:
    code = f.read()

result = execute_code("python", code)
```

### Language Detection for Multiple Files

```python
from un import detect_language, execute_code
import os

for filename in os.listdir("scripts"):
    lang = detect_language(filename)
    if lang:
        with open(f"scripts/{filename}") as f:
            code = f.read()
        result = execute_code(lang, code)
        print(f"{filename}: {result['status']}")
```

### Batch Execution

```python
from un import execute_async, get_job
import time

# Start multiple jobs
job_ids = []
for code in [code1, code2, code3]:
    job_id = execute_async("python", code)
    job_ids.append(job_id)

# Poll until all complete
results = {}
while job_ids:
    for job_id in list(job_ids):
        job = get_job(job_id)
        if job['status'] in ('completed', 'failed', 'timeout', 'cancelled'):
            results[job_id] = job
            job_ids.remove(job_id)

    if job_ids:
        time.sleep(1)  # Wait before next poll

# Process results
for job_id, result in results.items():
    print(f"{job_id}: {result['stdout']}")
```

### Polling with Custom Timeout

```python
from un import execute_async, get_job
import time

job_id = execute_async("python", "import time; time.sleep(5); print('done')")

# Poll with custom timeout
start = time.time()
timeout_sec = 30

while time.time() - start < timeout_sec:
    job = get_job(job_id)
    if job['status'] in ('completed', 'failed', 'timeout', 'cancelled'):
        print(f"Done: {job['stdout']}")
        break
    time.sleep(1)
else:
    print("Custom timeout reached")
```

## Performance Tips

1. **Reuse credentials**: Load credentials once, pass to multiple calls
2. **Use caching**: Languages list is cached for 1 hour
3. **Batch operations**: Start multiple jobs async, poll together
4. **Handle timeouts**: Always catch `requests.Timeout`

## Debugging

Enable verbose output:

```python
import logging

# Enable requests logging
logging.basicConfig(level=logging.DEBUG)
logging.getLogger("requests").setLevel(logging.DEBUG)
logging.getLogger("urllib3").setLevel(logging.DEBUG)

from un import execute_code

result = execute_code("python", "print('hello')")
```

## Testing Your Setup

```python
from un import get_languages, execute_code

# Test 1: Can we get credentials?
try:
    languages = get_languages()
    print(f"✓ Authentication works, {len(languages)} languages available")
except Exception as e:
    print(f"✗ Authentication failed: {e}")
    exit(1)

# Test 2: Can we execute code?
try:
    result = execute_code("python", "print('test')")
    if result['status'] == 'completed':
        print(f"✓ Code execution works: {result['stdout'].strip()}")
    else:
        print(f"✗ Code execution failed: {result['status']}")
except Exception as e:
    print(f"✗ Code execution error: {e}")
```

## Troubleshooting

### "No credentials found"

Ensure one of these is set:
- Function arguments: `execute_code(..., public_key="...", secret_key="...")`
- Environment: `export UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=...`
- Config: `~/.unsandbox/accounts.csv` or `./accounts.csv`

### "Connection timeout"

- Check internet connection
- Verify API is reachable: `curl https://api.unsandbox.com/cluster`
- Check for firewall/proxy issues

### "Authentication failed (401)"

- Verify public/secret keys are correct
- Ensure you're using the right account if multiple configured
- Check that account has API access enabled

### Slow execution

- First execution may be slow (container startup)
- Subsequent executions should be faster
- Use async execution for long-running jobs
- Check pool status: https://api.unsandbox.com/cluster

## See Also

- [API Reference](README.md)
- [Examples](examples/)
- [Tests](tests/)
- [Main Website](https://unsandbox.com)
