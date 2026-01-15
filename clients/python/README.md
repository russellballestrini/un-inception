# Unsandbox Python SDKs

Official Python SDKs for [unsandbox.com](https://unsandbox.com) - Execute code in 50+ languages from Python.

Two implementations: synchronous and asynchronous.

## Quick Choice

| Need | SDK | Location | Import |
|------|-----|----------|--------|
| **Simple scripts & CLIs** | Sync | `sync/` | `from un import execute_code` |
| **High-concurrency web services** | Async | `async/` | `from un_async import execute_code` |
| **FastAPI, Quart, async frameworks** | Async | `async/` | `from un_async import execute_code` |
| **100+ concurrent jobs** | Async | `async/` | `from un_async import execute_code` |
| **Unsure** | Sync | `sync/` | `from un import execute_code` |

## Synchronous SDK (`sync/`)

Traditional blocking I/O with `requests` library.

### Quick Start

```python
from un import execute_code

result = execute_code("python", "print('Hello, World!')")
print(result["stdout"])
```

### When to Use
- Writing simple scripts or CLIs
- Working in Jupyter notebooks
- Building prototype applications
- Low concurrency requirements (< 10 concurrent jobs)
- You want the simplest API

### Features
- Works with Python 3.6+
- Single-threaded simple API
- Uses `requests` library
- Basic credential management
- Perfect for getting started

### Documentation
- `sync/README.md` - Quick reference
- Examples: `sync/examples/`

## Asynchronous SDK (`async/`)

High-performance async/await implementation with `aiohttp`.

### Quick Start

```python
import asyncio
from un_async import execute_code

async def main():
    result = await execute_code("python", "print('Hello, World!')")
    print(result["stdout"])

asyncio.run(main())
```

### Concurrent Execution

```python
import asyncio
from un_async import execute_code

async def main():
    results = await asyncio.gather(
        execute_code("python", "print(1)"),
        execute_code("javascript", "console.log(2)"),
        execute_code("go", "fmt.Println(3)"),
    )

asyncio.run(main())
```

### When to Use
- Building web services with async frameworks (FastAPI, Quart)
- High concurrency requirements (100+ concurrent jobs)
- Already using async/await in your codebase
- Want to maximize throughput
- Django async views or other async contexts
- Need efficient resource utilization

### Features
- Python 3.7+ required
- Full async/await support
- Efficient event loop based concurrency
- Uses `aiohttp` library
- Comprehensive test suite (200+ tests)
- 5 working examples
- Detailed documentation
- Production-ready error handling

### Documentation
- `async/README.md` - Quick reference
- `async/USAGE_GUIDE.md` - Comprehensive guide
- `async/examples/` - 5 working examples
- `ASYNC_vs_SYNC.md` - Detailed comparison

### Getting Started

```bash
cd async
pip install -e "."
python examples/hello_world_async.py
```

## Shared Features (Both SDKs)

### Supported Languages
**50+ languages** including:
- Interpreted: Python, JavaScript, Ruby, Bash, Perl, PHP, Lua, Julia, Scheme, Tcl, Raku, and more
- Compiled: C, C++, Go, Rust, Java, Kotlin, C#, D, Nim, Zig, V, Pascal, Fortran, COBOL, and more
- Functional: Haskell, OCaml, F#, Clojure, Scheme
- Specialized: TypeScript, Objective-C

### Credential Management (4-Tier Priority)
1. Function arguments
2. Environment variables
3. `~/.unsandbox/accounts.csv`
4. `./accounts.csv`

### Core API
```python
# Execution
execute_code(language, code, public_key=None, secret_key=None)
execute_async(language, code, public_key=None, secret_key=None)

# Job Management
get_job(job_id, public_key=None, secret_key=None)
wait_for_job(job_id, public_key=None, secret_key=None)
cancel_job(job_id, public_key=None, secret_key=None)
list_jobs(public_key=None, secret_key=None)

# Metadata
get_languages(public_key=None, secret_key=None)
detect_language(filename)

# Snapshots
session_snapshot(session_id, public_key=None, secret_key=None, name=None, hot=False)
service_snapshot(service_id, public_key=None, secret_key=None, name=None, hot=False)
list_snapshots(public_key=None, secret_key=None)
restore_snapshot(snapshot_id, public_key=None, secret_key=None)
delete_snapshot(snapshot_id, public_key=None, secret_key=None)
```

### Request Authentication
- HMAC-SHA256 signing
- Timestamp-based replay prevention
- Bearer token authentication

### Caching
- Language list cached for 1 hour
- Cache location: `~/.unsandbox/languages.json`

## Installation

### Sync SDK
```bash
cd sync
pip install -e .
```

### Async SDK
```bash
cd async
pip install -e .
```

### With Development Tools
```bash
cd async  # or sync
pip install -e ".[dev]"
```

## Examples

### Sync SDK
- `sync/examples/hello_world.py` - Basic execution
- `sync/examples/fibonacci.py` - Recursive functions

### Async SDK
- `async/examples/hello_world_async.py` - Basic async execution
- `async/examples/fibonacci_async.py` - Concurrent calculations
- `async/examples/concurrent_execution.py` - Multiple languages
- `async/examples/async_job_polling.py` - Fire-and-forget pattern
- `async/examples/sync_blocking_usage.py` - Mixed sync/async

## Testing

### Sync SDK
See `sync/README.md` for testing instructions.

### Async SDK
```bash
cd async
pip install -e ".[dev]"
make test-coverage
```

## Comparison

See `ASYNC_vs_SYNC.md` for detailed comparison including:
- Side-by-side code examples
- When to use each
- Performance benchmarks
- Migration guide

## Authentication Setup

### Using Environment Variables
```bash
export UNSANDBOX_PUBLIC_KEY="your_public_key"
export UNSANDBOX_SECRET_KEY="your_secret_key"
python script.py
```

### Using Config File
```bash
mkdir -p ~/.unsandbox
echo "public_key,secret_key" > ~/.unsandbox/accounts.csv
```

### Using Function Arguments
```python
result = await execute_code(
    "python",
    "print('hello')",
    public_key="your_pk",
    secret_key="your_sk"
)
```

## Response Format

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

## Error Handling

### Sync SDK
```python
from un import execute_code, CredentialsError
import requests

try:
    result = execute_code("python", "print('hello')")
except CredentialsError as e:
    print(f"Auth failed: {e}")
except requests.RequestException as e:
    print(f"Network error: {e}")
```

### Async SDK
```python
from un_async import execute_code, CredentialsError
import aiohttp

try:
    result = await execute_code("python", "print('hello')")
except CredentialsError as e:
    print(f"Auth failed: {e}")
except aiohttp.ClientError as e:
    print(f"Network error: {e}")
```

## Development

### Code Style
- PEP 8 compliant
- Type hints throughout
- Comprehensive docstrings

### Testing
- Unit tests for all major functions
- Async/await test patterns
- Mock-based API testing
- 95%+ coverage target

### Linting & Formatting
```bash
cd async
make lint      # Run flake8 and mypy
make format    # Format with black
```

## Performance

### Sync SDK
- Good for: 1-100 concurrent jobs
- Throughput: ~1-10 jobs/sec
- Resource: One thread per job
- Overhead: Thread context switching

### Async SDK
- Good for: 100-10,000+ concurrent jobs
- Throughput: ~10-100 jobs/sec
- Resource: Single event loop
- Overhead: Minimal (event loop only)

## Choosing Between SDKs

### Use Sync SDK if:
- Writing a simple script or CLI
- Working in Jupyter
- Don't need high concurrency
- Want the simplest API
- Running on older Python (3.6)

### Use Async SDK if:
- Building a web service
- Using async framework (FastAPI, Quart)
- Need 100+ concurrent jobs
- Already using async/await
- Want better resource utilization

## Coexistence

Both SDKs can be used in the same project:
```python
from un import execute_code as sync_execute
from un_async import execute_code as async_execute

# Use sync version for some operations
result1 = sync_execute("python", "code1")

# Use async version elsewhere
async def async_work():
    result2 = await async_execute("python", "code2")
```

## Documentation Structure

```
clients/python/
├── README.md                    # This file
├── ASYNC_vs_SYNC.md           # Comparison guide
├── IMPLEMENTATION_SUMMARY.md   # Async implementation details
├── sync/
│   ├── README.md              # Sync SDK quick start
│   ├── src/un.py              # Sync SDK implementation
│   └── examples/              # Sync examples
└── async/
    ├── README.md              # Async SDK quick start
    ├── USAGE_GUIDE.md         # Comprehensive async guide
    ├── src/un_async.py        # Async SDK implementation
    ├── examples/              # 5 async examples
    ├── tests/                 # 200+ test cases
    ├── setup.py               # Package config
    ├── requirements.txt       # Dependencies
    └── Makefile               # Development targets
```

## Support & Help

1. **Quick Start:** See README in `sync/` or `async/` folder
2. **Detailed Guide:** See `USAGE_GUIDE.md` in `async/` folder
3. **Comparison:** See `ASYNC_vs_SYNC.md`
4. **API Docs:** See `unsandbox.txt` in repository root
5. **Examples:** See `examples/` in `sync/` or `async/` folder

## License

Public Domain - NO LICENSE, NO WARRANTY

## Official Resources

- Website: https://unsandbox.com
- API Documentation: See `unsandbox.txt`
- Support: https://unsandbox.com/support
