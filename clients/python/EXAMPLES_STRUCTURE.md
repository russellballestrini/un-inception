# Python SDK Examples Structure

## Overview

This document describes the structure and organization of Python SDK examples for both synchronous and asynchronous code execution patterns.

## Directory Structure

```
clients/python/
├── EXAMPLES.md                          # Comprehensive examples guide
├── EXAMPLES_STRUCTURE.md               # This file - structural overview
├── scripts/
│   └── validate-examples.sh            # Validation script for all examples
├── sync/
│   ├── src/
│   │   └── un.py                       # Synchronous SDK
│   └── examples/
│       ├── hello_world.py              # Raw code snippet
│       ├── hello_world_client.py       # SDK wrapper example
│       ├── fibonacci.py                # Raw code snippet
│       ├── fibonacci_client.py         # SDK wrapper example
│       ├── http_request.py             # Network I/O example
│       ├── json_processing.py          # Data processing example
│       └── file_operations.py          # File I/O example
└── async/
    ├── src/
    │   └── un_async.py                 # Asynchronous SDK
    └── examples/
        ├── hello_world_async.py        # Basic async example
        ├── fibonacci_async.py          # Concurrent computation
        ├── concurrent_requests.py      # Parallel HTTP requests
        ├── stream_processing.py        # Async generator patterns
        ├── async_job_polling.py        # Job management
        ├── concurrent_execution.py     # Multi-language concurrency
        └── sync_blocking_usage.py      # Mixed sync/async patterns
```

## Synchronous Examples

### Location
`clients/python/sync/examples/`

### Types

#### Basic Examples (2 files)
- **hello_world.py**: Simple print statement (raw code to execute)
- **hello_world_client.py**: SDK wrapper that executes hello_world logic

#### Computational Examples (2 files)
- **fibonacci.py**: Recursive fibonacci (raw code)
- **fibonacci_client.py**: SDK wrapper executing fibonacci calculation

#### Data Processing Examples (3 files)
- **http_request.py**: HTTP requests using requests library
- **json_processing.py**: JSON parsing and manipulation
- **file_operations.py**: Temporary file creation and I/O

### Pattern
```python
# All sync examples follow this pattern:
from un import execute_code

def main():
    code = "..."  # Python code to execute
    result = execute_code("python", code, public_key, secret_key)
    print(result.get("stdout"))

if __name__ == "__main__":
    main()
```

## Asynchronous Examples

### Location
`clients/python/async/examples/`

### Types

#### Basic Examples (1 file)
- **hello_world_async.py**: Simple async execution with await

#### Concurrent Computation (1 file)
- **fibonacci_async.py**: Multiple concurrent fibonacci calculations

#### Network Examples (1 file)
- **concurrent_requests.py**: Parallel HTTP requests to different endpoints

#### Stream Processing (1 file)
- **stream_processing.py**: Async generator patterns for data streaming

#### Job Management (2 files)
- **async_job_polling.py**: Fire-and-forget job submission and polling
- **concurrent_execution.py**: Multi-language code execution

#### Hybrid Patterns (1 file)
- **sync_blocking_usage.py**: Mixing async and blocking function calls

### Pattern
```python
# All async examples follow this pattern:
import asyncio
from un_async import execute_code

async def main():
    code = "..."
    result = await execute_code("python", code, public_key, secret_key)
    print(result.get("stdout"))

if __name__ == "__main__":
    asyncio.run(main())
```

## File Coverage

### Sync Examples (7 files)
1. hello_world.py - Basic output
2. hello_world_client.py - SDK wrapper pattern
3. fibonacci.py - CPU computation
4. fibonacci_client.py - CPU computation via SDK
5. http_request.py - Network I/O
6. json_processing.py - Data transformation
7. file_operations.py - File I/O

**Coverage**: Basic I/O, CPU-bound, Network, Data structures, File systems

### Async Examples (7 files)
1. hello_world_async.py - Basic async pattern
2. fibonacci_async.py - Concurrent computation
3. concurrent_requests.py - Parallel network I/O
4. stream_processing.py - Async generators
5. async_job_polling.py - Job lifecycle management
6. concurrent_execution.py - Multi-language parallelism
7. sync_blocking_usage.py - Hybrid sync/async patterns

**Coverage**: Async patterns, Concurrency, Job management, Polyglot execution, Hybrid patterns

## Code Patterns Demonstrated

### 1. Basic Execution
- `hello_world_client.py` (sync)
- `hello_world_async.py` (async)

### 2. Error Handling
All examples include:
```python
try:
    result = execute_code(...)
except CredentialsError as e:
    # Handle missing credentials
except Exception as e:
    # Handle other errors
```

### 3. Credential Resolution
All examples show how to use environment variables:
```python
public_key = os.environ.get("UNSANDBOX_PUBLIC_KEY")
secret_key = os.environ.get("UNSANDBOX_SECRET_KEY")
```

### 4. Result Processing
All examples demonstrate:
```python
if result.get("status") == "completed":
    print(result.get("stdout"))
else:
    print(f"Error: {result.get('error')}")
```

### 5. Concurrency (Async only)
Examples show `asyncio.gather()` pattern:
```python
tasks = [
    execute_code("python", code1, pk, sk),
    execute_code("python", code2, pk, sk),
]
results = await asyncio.gather(*tasks)
```

### 6. Job Management (Async only)
Examples demonstrate:
```python
job_id = await execute_async(...)
status = await get_job(job_id)
result = await wait_for_job(job_id)
```

## Feature Coverage

### Sync Examples
- Execute code synchronously ✓
- Handle credentials ✓
- Process stdout/stderr ✓
- Error handling ✓
- Network requests ✓
- Data processing ✓
- File I/O ✓
- CPU-bound operations ✓

### Async Examples
- Execute code asynchronously ✓
- Concurrent execution ✓
- asyncio.gather() patterns ✓
- Job submission (fire-and-forget) ✓
- Job polling with backoff ✓
- Multiple languages ✓
- Hybrid sync/async contexts ✓
- Stream processing ✓

## Validation

### Script
`scripts/validate-examples.sh` - Comprehensive validation script

### Checks Performed
- File existence ✓
- Readable permissions ✓
- Python syntax validation ✓
- Content verification (contains expected patterns) ✓
- Optional: Execution tests (with credentials)

### Running Validation
```bash
# Basic validation (no credentials needed)
bash scripts/validate-examples.sh

# With execution tests (requires credentials)
UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... \
bash scripts/validate-examples.sh --run
```

## Expected Output Examples

### Sync Examples Output
```
Executing code synchronously...
Result status: completed
Output: Hello from unsandbox!
```

### Async Examples Output
```
Executing code asynchronously...
Result status: completed
Output: Hello from async unsandbox!
```

### Concurrent Examples Output
```
Starting 3 concurrent fibonacci calculations...
[fib-10] Result: fib(10) = 55
[fib-15] Result: fib(15) = 610
[fib-12] Result: fib(12) = 144
All calculations completed!
```

## Usage Examples

### Running Individual Examples

**Sync examples:**
```bash
export UNSANDBOX_PUBLIC_KEY="key"
export UNSANDBOX_SECRET_KEY="secret"
python3 sync/examples/hello_world_client.py
python3 sync/examples/http_request.py
```

**Async examples:**
```bash
export UNSANDBOX_PUBLIC_KEY="key"
export UNSANDBOX_SECRET_KEY="secret"
python3 async/examples/hello_world_async.py
python3 async/examples/fibonacci_async.py
```

### Batch Running

**All sync examples:**
```bash
for f in sync/examples/*_client.py; do
    echo "Running $f..."
    python3 "$f" || echo "Failed: $f"
done
```

**All async examples:**
```bash
for f in async/examples/*.py; do
    echo "Running $f..."
    python3 "$f" || echo "Failed: $f"
done
```

## Documentation

### Main Documentation
- `EXAMPLES.md` - Comprehensive guide with usage instructions
- `EXAMPLES_STRUCTURE.md` - This file, structural overview

### Docstrings
Each example file includes:
- Module docstring with description
- Usage instructions
- Expected output
- Feature highlights

## Requirements

### Python Version
- Python 3.7+

### Dependencies (Built-in)
- asyncio (async examples)
- os, sys (all examples)

### SDK Dependencies
- requests (sync SDK)
- aiohttp (async SDK)

### Pre-installed in Sandbox
- numpy, scipy, pandas
- matplotlib, seaborn, plotly
- requests, beautifulsoup4
- And 20+ other packages (see CLAUDE.md)

## Extensibility

### Adding New Sync Examples
1. Create `sync/examples/feature_name.py`
2. Import from `un` module
3. Follow established error handling pattern
4. Add docstring with expected output
5. Update `EXAMPLES.md`
6. Run validation: `bash scripts/validate-examples.sh`

### Adding New Async Examples
1. Create `async/examples/feature_name.py`
2. Import from `un_async` module
3. Use async/await syntax
4. Follow established error handling pattern
5. Add docstring with expected output
6. Update `EXAMPLES.md`
7. Run validation: `bash scripts/validate-examples.sh`

## Related Documentation

- `/home/fox/git/un-inception/CLAUDE.md` - Project instructions
- `/home/fox/git/un-inception/clients/python/README.md` - SDK documentation
- `/home/fox/git/un-inception/docs/` - Architecture and design docs

## Testing

All examples are designed to be:
- **Testable** - Deterministic output for validation
- **Runnable** - Complete with error handling
- **Self-documented** - Clear docstrings and comments
- **Extensible** - Can be used as templates for other examples

## Summary

**Total Examples**: 14 files
- **Sync Examples**: 7 files covering 4 categories
- **Async Examples**: 7 files covering 6 categories
- **Documentation**: 2 comprehensive guides
- **Validation**: 1 automated script

**Coverage**: All major use cases from simple I/O to complex concurrent operations with proper error handling and credential management.
