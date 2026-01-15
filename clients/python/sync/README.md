# Unsandbox Python SDK (Synchronous)

A synchronous Python client library for [unsandbox.com](https://unsandbox.com) - secure, multi-language code execution.

## Installation

```bash
pip install unsandbox
```

Or from source:

```bash
cd clients/python/sync
pip install -e .
```

## Quick Start

```python
from un import execute_code

# Execute Python code
result = execute_code("python", 'print("Hello from unsandbox!")')
print(result)
```

## Authentication

The SDK supports 4-tier credential resolution:

1. **Function arguments** - Pass directly to functions
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
3. **Config file** - `~/.unsandbox/accounts.csv` (line 0 by default)
4. **Local directory** - `./accounts.csv` (line 0 by default)

### Setting up credentials

Create `~/.unsandbox/accounts.csv`:

```csv
your_public_key,your_secret_key
another_public_key,another_secret_key
```

Or use environment variables:

```bash
export UNSANDBOX_PUBLIC_KEY="pk_xxxxx"
export UNSANDBOX_SECRET_KEY="sk_xxxxx"
```

## API Reference

### Synchronous Execution

Execute code and wait for completion:

```python
from un import execute_code

result = execute_code(
    language="python",
    code="print('hello')",
    public_key=None,      # Optional, uses credential resolution
    secret_key=None,      # Optional, uses credential resolution
)

print(result)
# Output: {
#     'status': 'completed',
#     'stdout': 'hello\n',
#     'stderr': '',
#     'exit_code': 0,
#     'runtime_ms': 342
# }
```

### Asynchronous Execution

Start execution and get a job ID:

```python
from un import execute_async, wait_for_job

# Start execution
job_id = execute_async("python", "print('hello')")

# Check status later
result = wait_for_job(job_id)
```

### Job Management

```python
from un import get_job, cancel_job, list_jobs

# Get single job status
job = get_job("job_123")

# List all active jobs
jobs = list_jobs()

# Cancel a job
cancel_job("job_123")
```

### Languages

```python
from un import get_languages, detect_language

# Get list of supported languages
languages = get_languages()
# Returns: ['python', 'javascript', 'go', 'rust', ...]

# Detect language from filename
lang = detect_language("script.py")  # Returns 'python'
```

### Snapshots

```python
from un import session_snapshot, list_snapshots, restore_snapshot, delete_snapshot

# Create a snapshot
snapshot_id = session_snapshot("session_123", name="checkpoint")

# List snapshots
snapshots = list_snapshots()

# Restore a snapshot
result = restore_snapshot(snapshot_id)

# Delete a snapshot
delete_snapshot(snapshot_id)
```

## Language Support

The SDK supports 50+ programming languages including:

- **Interpreted**: Python, JavaScript, Ruby, PHP, Perl, Bash, etc.
- **Compiled**: C, C++, Go, Rust, Java, etc.
- **Functional**: Haskell, OCaml, F#, Scheme, etc.
- **Other**: WASM, Prolog, Forth, etc.

See `get_languages()` for the complete list.

## Caching

The languages list is cached locally for 1 hour in `~/.unsandbox/languages.json`. This reduces API calls and improves startup performance.

To force a refresh, delete the cache file:

```bash
rm ~/.unsandbox/languages.json
```

## Error Handling

```python
from un import execute_code, CredentialsError
import requests

try:
    result = execute_code("python", "print('hello')")
except CredentialsError:
    print("No credentials found")
except requests.RequestException as e:
    print(f"Network error: {e}")
except ValueError as e:
    print(f"Invalid response: {e}")
```

## Examples

See the `examples/` directory for complete working examples:

- `hello_world.py` - Simple print example
- `fibonacci.py` - Recursive function example

Run an example:

```bash
cd examples
python hello_world.py
```

## Testing

Run the test suite:

```bash
pip install -e ".[dev]"
pytest tests/ -v
```

With coverage:

```bash
pytest tests/ --cov=un --cov-report=html
```

## Public Domain License

This code is released into the PUBLIC DOMAIN with NO WARRANTY and NO LICENSE.

You are free to:
- Use for any purpose
- Modify and distribute
- Use commercially
- Use privately

## Support

For issues or questions:
- GitHub Issues: https://github.com/unsandbox/un-inception/issues
- Website: https://unsandbox.com
