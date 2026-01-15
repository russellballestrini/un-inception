# Python SDK - Quick Start Guide

## Setup (30 seconds)

```bash
# Set your API credentials
export UNSANDBOX_PUBLIC_KEY="your-public-key"
export UNSANDBOX_SECRET_KEY="your-secret-key"
```

## Synchronous Usage

### Simple Execution
```bash
python3 sync/examples/hello_world_client.py
```

### Run Your Own Code
```python
from un import execute_code

result = execute_code("python", 'print("Hello")')
print(result.get("stdout"))  # Output: Hello
```

### Available Languages
Python, JavaScript, Go, Rust, Java, C, C++, Ruby, PHP, Bash, and 40+ more

### Examples by Category

| Category | File | What It Does |
|----------|------|--------------|
| **Basic** | hello_world_client.py | Simple print statement |
| **CPU** | fibonacci_client.py | Recursive computation |
| **Network** | http_request.py | HTTP requests |
| **Data** | json_processing.py | JSON parsing |
| **Files** | file_operations.py | Temp file I/O |

## Asynchronous Usage

### Simple Async Execution
```bash
python3 async/examples/hello_world_async.py
```

### Run Concurrent Tasks
```python
import asyncio
from un_async import execute_code

async def main():
    tasks = [
        execute_code("python", 'print(1)'),
        execute_code("python", 'print(2)'),
        execute_code("python", 'print(3)'),
    ]
    results = await asyncio.gather(*tasks)
    return results

asyncio.run(main())
```

### Async Examples by Category

| Category | File | What It Does |
|----------|------|--------------|
| **Basic** | hello_world_async.py | Async execution |
| **Concurrent CPU** | fibonacci_async.py | Parallel computation |
| **Concurrent Network** | concurrent_requests.py | Parallel HTTP |
| **Streams** | stream_processing.py | Async generators |
| **Jobs** | async_job_polling.py | Job management |
| **Multi-language** | concurrent_execution.py | Mixed language execution |
| **Hybrid** | sync_blocking_usage.py | Sync + async mixing |

## Common Tasks

### Task 1: Execute Python Code
```python
from un import execute_code

result = execute_code("python", """
numbers = [1, 2, 3, 4, 5]
print(f"Sum: {sum(numbers)}")
""")
print(result.get("stdout"))
```

### Task 2: Execute JavaScript Code
```python
from un import execute_code

result = execute_code("javascript", """
const nums = [1, 2, 3, 4, 5];
console.log(`Sum: ${nums.reduce((a, b) => a + b, 0)}`);
""")
print(result.get("stdout"))
```

### Task 3: Run Multiple Jobs Concurrently
```python
import asyncio
from un_async import execute_code

async def run_jobs():
    jobs = [
        execute_code("python", "print('Job 1')"),
        execute_code("javascript", "console.log('Job 2')"),
        execute_code("bash", "echo 'Job 3'"),
    ]
    return await asyncio.gather(*jobs)

asyncio.run(run_jobs())
```

### Task 4: Poll Job Status
```python
from un import execute_async, wait_for_job

# Start job
job_id = execute_async("python", "print('running')")

# Wait for completion
result = wait_for_job(job_id)
print(result.get("stdout"))
```

### Task 5: Make HTTP Request (from Sandbox)
```python
from un import execute_code

code = """
import requests
response = requests.get('https://httpbin.org/ip')
print(response.json())
"""
result = execute_code("python", code)
print(result.get("stdout"))
```

## Error Handling

```python
from un import execute_code, CredentialsError

try:
    result = execute_code("python", "print('hello')")

    if result.get("status") == "completed":
        print(f"Success: {result.get('stdout')}")
    elif result.get("status") == "failed":
        print(f"Failed: {result.get('error')}")
    elif result.get("status") == "timeout":
        print("Execution timed out")

except CredentialsError:
    print("Invalid credentials")
except Exception as e:
    print(f"Error: {e}")
```

## Credential Options

### Option 1: Environment Variables (Recommended)
```bash
export UNSANDBOX_PUBLIC_KEY="key"
export UNSANDBOX_SECRET_KEY="secret"
python3 script.py
```

### Option 2: Function Arguments
```python
from un import execute_code

result = execute_code(
    "python",
    "print('hello')",
    public_key="key",
    secret_key="secret"
)
```

### Option 3: Config File
Create `~/.unsandbox/accounts.csv`:
```csv
public_key,secret_key
```

## Validation

Check all examples work:
```bash
bash scripts/validate-examples.sh
```

With credentials (executes examples):
```bash
UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... \
bash scripts/validate-examples.sh --run
```

## File Structure

```
clients/python/
├── sync/
│   ├── src/un.py              (Sync SDK)
│   └── examples/              (7 sync examples)
├── async/
│   ├── src/un_async.py        (Async SDK)
│   └── examples/              (7 async examples)
├── EXAMPLES.md                (Full documentation)
└── scripts/validate-examples.sh (Validation)
```

## Performance Tips

### For Multiple Executions
- Use **async examples** for concurrency
- Use `asyncio.gather()` to run tasks in parallel
- Don't create new session for each request

### For Long-Running Tasks
- Use `execute_async()` + `wait_for_job()` pattern
- Poll periodically rather than spinning
- Timeout after reasonable time

### For API Key Limits
- Check rate limit headers in responses
- Implement backoff for retries
- Use concurrency limits from account tier

## Next Steps

1. **Review Examples**: Check `EXAMPLES.md` for detailed docs
2. **Run Validation**: `bash scripts/validate-examples.sh`
3. **Try Sync Examples**: Start with `hello_world_client.py`
4. **Try Async Examples**: Then try `hello_world_async.py`
5. **Build Your App**: Use patterns from examples

## Documentation

- **EXAMPLES.md** - Full guide with all examples explained
- **EXAMPLES_STRUCTURE.md** - Project structure overview
- **README.md** - SDK API reference
- **QUICK_START.md** - This file

## Support

For issues:
1. Check credentials are set correctly
2. Verify network connectivity
3. Review error messages
4. Check `EXAMPLES.md` for similar cases
5. Try running validation script

## Key Takeaways

- **Sync**: Use `from un import execute_code`
- **Async**: Use `from un_async import execute_code` with `await`
- **Concurrency**: Use `asyncio.gather(*tasks)`
- **Jobs**: Use `execute_async()` + `wait_for_job()`
- **Languages**: 50+ languages supported
- **Error Handling**: Always check `result.get("status")`

---

**Ready?** Run your first example:
```bash
python3 sync/examples/hello_world_client.py
```
