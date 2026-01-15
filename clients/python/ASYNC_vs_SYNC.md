# Async vs Sync Python SDK - Comparison Guide

Both sync and async SDKs provide the same functionality. Choose based on your use case.

## Quick Comparison

| Feature | Sync SDK | Async SDK |
|---------|----------|-----------|
| **HTTP Library** | `requests` | `aiohttp` |
| **I/O Model** | Blocking (threads) | Non-blocking (async/await) |
| **Concurrency** | Thread pools | Event loop |
| **Best For** | Scripts, simple apps | High-concurrency servers, async frameworks |
| **Import** | `from un import ...` | `from un_async import ...` |
| **Execution** | Direct calls | `await` calls in async context |
| **Location** | `clients/python/sync/` | `clients/python/async/` |

## Side-by-Side Examples

### Simple Execution

**Sync SDK:**
```python
from un import execute_code

result = execute_code("python", "print('hello')")
print(result["stdout"])
```

**Async SDK:**
```python
import asyncio
from un_async import execute_code

async def main():
    result = await execute_code("python", "print('hello')")
    print(result["stdout"])

asyncio.run(main())
```

### Concurrent Execution

**Sync SDK (using ThreadPoolExecutor):**
```python
from un import execute_code
from concurrent.futures import ThreadPoolExecutor

def execute_one(code):
    return execute_code("python", code)

with ThreadPoolExecutor(max_workers=5) as executor:
    results = list(executor.map(execute_one, [
        "print(1)",
        "print(2)",
        "print(3)",
    ]))

for result in results:
    print(result["stdout"])
```

**Async SDK (using asyncio):**
```python
import asyncio
from un_async import execute_code

async def main():
    results = await asyncio.gather(
        execute_code("python", "print(1)"),
        execute_code("python", "print(2)"),
        execute_code("python", "print(3)"),
    )

    for result in results:
        print(result["stdout"])

asyncio.run(main())
```

### Fire-and-Forget Job

**Sync SDK:**
```python
from un import execute_async, wait_for_job

job_id = execute_async("python", "print('started')")
# Do other work...
result = wait_for_job(job_id)
print(result["stdout"])
```

**Async SDK:**
```python
import asyncio
from un_async import execute_async, wait_for_job

async def main():
    job_id = await execute_async("python", "print('started')")
    # Do other work...
    result = await wait_for_job(job_id)
    print(result["stdout"])

asyncio.run(main())
```

### Error Handling

**Sync SDK:**
```python
from un import execute_code, CredentialsError
import requests

try:
    result = execute_code("python", "print('hello')")
except CredentialsError as e:
    print(f"Auth error: {e}")
except requests.RequestException as e:
    print(f"Network error: {e}")
```

**Async SDK:**
```python
import asyncio
from un_async import execute_code, CredentialsError
import aiohttp

async def main():
    try:
        result = await execute_code("python", "print('hello')")
    except CredentialsError as e:
        print(f"Auth error: {e}")
    except aiohttp.ClientError as e:
        print(f"Network error: {e}")

asyncio.run(main())
```

## When to Use Each

### Use Sync SDK When:

1. **Writing Simple Scripts**
   ```python
   # Simple one-off scripts work great with sync
   from un import execute_code
   result = execute_code("python", "print('done')")
   ```

2. **Working in Jupyter Notebooks**
   ```python
   from un import execute_code
   result = execute_code("python", code_cell)
   print(result["stdout"])
   ```

3. **Building Command-Line Tools**
   ```python
   #!/usr/bin/env python3
   from un import execute_code
   # CLI logic using sync SDK
   ```

4. **Prototyping**
   - Easier to understand and debug
   - No async/await syntax required

### Use Async SDK When:

1. **Building High-Concurrency Servers**
   ```python
   # FastAPI app with async SDK
   from fastapi import FastAPI
   from un_async import execute_code

   @app.post("/execute")
   async def run(request):
       result = await execute_code("python", request.code)
       return result
   ```

2. **Running Many Jobs Concurrently**
   ```python
   # Execute 1000+ jobs efficiently
   results = await asyncio.gather(*tasks)
   ```

3. **Long-Running Services**
   - Better resource utilization
   - No thread overhead
   - Scales to thousands of concurrent operations

4. **Async Web Frameworks**
   - FastAPI, Quart, aiohttp
   - Django async views
   - Any async/await codebase

## API Compatibility

Both SDKs have **identical APIs** - all functions exist in both versions:

```
✓ execute_code()
✓ execute_async()
✓ get_job()
✓ wait_for_job()
✓ cancel_job()
✓ list_jobs()
✓ get_languages()
✓ detect_language()
✓ session_snapshot()
✓ service_snapshot()
✓ list_snapshots()
✓ restore_snapshot()
✓ delete_snapshot()
```

The only difference is that async versions require `await`.

## Migration Guide

### From Sync to Async

1. **Add `async def` and `await` keywords:**
   ```python
   # Before
   result = execute_code("python", "print('hi')")

   # After
   result = await execute_code("python", "print('hi')")
   ```

2. **Wrap in async context:**
   ```python
   # Before
   result = execute_code("python", "print('hi')")

   # After
   import asyncio

   async def main():
       result = await execute_code("python", "print('hi')")
       return result

   result = asyncio.run(main())
   ```

3. **Replace concurrent.futures with asyncio:**
   ```python
   # Before
   from concurrent.futures import ThreadPoolExecutor
   with ThreadPoolExecutor(max_workers=5) as executor:
       results = executor.map(execute_code, codes)

   # After
   results = await asyncio.gather(
       *[execute_code(lang, code) for lang, code in zip(langs, codes)]
   )
   ```

### From Async to Sync

1. **Remove `async def` and `await` keywords:**
   ```python
   # Before
   async def main():
       result = await execute_code("python", "print('hi')")

   # After
   result = execute_code("python", "print('hi')")
   ```

2. **Remove asyncio.run wrapper:**
   ```python
   # Before
   result = asyncio.run(main())

   # After
   result = execute_code("python", "print('hi')")
   ```

3. **Use ThreadPoolExecutor instead of asyncio:**
   ```python
   # Before (async)
   results = await asyncio.gather(*tasks)

   # After (sync)
   from concurrent.futures import ThreadPoolExecutor
   with ThreadPoolExecutor() as executor:
       results = list(executor.map(execute_code, codes))
   ```

## Performance Considerations

### Sync SDK
- **Throughput:** Good for 1-100 concurrent jobs
- **Resource Usage:** One thread per connection
- **Overhead:** Thread context switching
- **Best:** Small to medium workloads

### Async SDK
- **Throughput:** Good for 100-10,000+ concurrent jobs
- **Resource Usage:** Single-threaded event loop
- **Overhead:** Minimal (event loop overhead only)
- **Best:** Large workloads, high concurrency

### Benchmark Example

```python
import asyncio
import time
from concurrent.futures import ThreadPoolExecutor

# Sync version
def sync_benchmark():
    from un import execute_code
    start = time.time()
    for i in range(100):
        execute_code("python", "print(1)")
    return time.time() - start

# Async version with concurrency
async def async_benchmark():
    from un_async import execute_code
    start = time.time()
    tasks = [execute_code("python", "print(1)") for i in range(100)]
    await asyncio.gather(*tasks)
    return time.time() - start

# Async version sequential
async def async_sequential():
    from un_async import execute_code
    start = time.time()
    for i in range(100):
        await execute_code("python", "print(1)")
    return time.time() - start
```

**Expected Results (100 jobs):**
- Sync sequential: ~50s (blocking)
- Async sequential: ~50s (same, just with await)
- Async concurrent: ~5-10s (10x faster due to concurrency)

## Shared Features

Both SDKs share:

1. **Credential System** (4-tier priority)
2. **HMAC-SHA256 Authentication**
3. **Language Detection**
4. **Language Caching**
5. **Error Classes** (CredentialsError, etc.)
6. **Response Format**
7. **Job Management**
8. **Snapshot Operations**

## Debugging

### Sync SDK
```python
import logging
logging.basicConfig(level=logging.DEBUG)

from un import execute_code
result = execute_code("python", "print('debug')")
```

### Async SDK
```python
import logging
import asyncio

logging.basicConfig(level=logging.DEBUG)

async def main():
    from un_async import execute_code
    result = await execute_code("python", "print('debug')")

asyncio.run(main())
```

## Choosing for Your Project

### Decision Tree

```
├─ Need to run many jobs concurrently?
│  ├─ Yes (100+) → Use ASYNC SDK
│  └─ No → Check next
├─ Building web service with async framework?
│  ├─ Yes (FastAPI, Quart, etc.) → Use ASYNC SDK
│  └─ No → Check next
├─ Building simple script or CLI?
│  ├─ Yes → Use SYNC SDK
│  └─ No → Check next
├─ Need to integrate into existing async codebase?
│  ├─ Yes → Use ASYNC SDK
│  └─ No → Use SYNC SDK
```

## Coexistence

You can use both SDKs in the same project:

```python
# In one module - sync operations
from un import execute_code
sync_result = execute_code("python", "print('sync')")

# In another module - async operations
from un_async import execute_code
async_result = await execute_code("python", "print('async')")
```

This allows gradual migration or hybrid approaches.

## Support

- **Sync SDK Docs:** `clients/python/sync/README.md`
- **Async SDK Docs:** `clients/python/async/README.md`
- **API Reference:** `clients/python/async/USAGE_GUIDE.md`
- **Examples:** See `examples/` in each folder
