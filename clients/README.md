# Unsandbox SDK Clients

Multi-language SDK implementations for the Unsandbox API.

**Each SDK is BOTH a library AND a CLI tool** - see [CLI_SPEC.md](CLI_SPEC.md) for the full CLI specification.

```bash
# Library usage
python -c "from un import execute_code; print(execute_code('python', 'print(1)'))"

# CLI usage (identical across all 42+ languages)
python un.py script.py
python un.py -s bash 'echo hello'
python un.py session --tmux
python un.py service --list
```

## Directory Structure

```
clients/
├── python/
│   ├── sync/src/un.py          # Synchronous (requests)
│   └── async/src/un_async.py   # Asynchronous (aiohttp)
├── javascript/
│   ├── sync/src/un.js          # Synchronous (https)
│   └── async/src/un_async.js   # Asynchronous (fetch)
├── go/
│   ├── sync/src/un.go          # Synchronous (net/http)
│   └── async/src/un_async.go   # Asynchronous (goroutines)
├── java/
│   ├── sync/src/Un.java        # Synchronous (HttpURLConnection)
│   └── async/src/UnsandboxAsync.java  # Asynchronous (CompletableFuture)
├── ruby/
│   ├── sync/src/un.rb          # Synchronous (net/http)
│   └── async/src/un_async.rb   # Asynchronous (Future)
├── rust/
│   ├── sync/src/lib.rs         # Synchronous (reqwest blocking)
│   └── async/src/lib.rs        # Asynchronous (reqwest + tokio)
├── php/
│   ├── sync/src/un.php         # Synchronous (cURL)
│   └── async/src/UnsandboxAsync.php  # Asynchronous (Guzzle promises)
└── c/
    └── src/unsandbox.c         # Reference implementation
```

## API Functions (43 total)

All SDKs implement the complete Unsandbox API:

### Execute & Jobs (6 functions)
| Function | Description | Endpoint |
|----------|-------------|----------|
| `execute_code` | Execute code synchronously | POST /execute |
| `execute_async` | Execute code, return job ID | POST /execute (async mode) |
| `get_job` | Get job status | GET /jobs/{id} |
| `wait_for_job` | Poll until completion | GET /jobs/{id} (polling) |
| `cancel_job` | Cancel running job | DELETE /jobs/{id} |
| `list_jobs` | List all jobs | GET /jobs |

### Sessions (9 functions)
| Function | Description | Endpoint |
|----------|-------------|----------|
| `list_sessions` | List all sessions | GET /sessions |
| `get_session` | Get session details | GET /sessions/{id} |
| `create_session` | Create interactive session | POST /sessions |
| `delete_session` | Terminate session | DELETE /sessions/{id} |
| `freeze_session` | Pause session | POST /sessions/{id}/freeze |
| `unfreeze_session` | Resume session | POST /sessions/{id}/unfreeze |
| `boost_session` | Add vCPUs | POST /sessions/{id}/boost |
| `unboost_session` | Remove boost | POST /sessions/{id}/unboost |
| `shell_session` | Execute shell command | POST /sessions/{id}/shell |

### Services (16 functions)
| Function | Description | Endpoint |
|----------|-------------|----------|
| `list_services` | List all services | GET /services |
| `create_service` | Create persistent service | POST /services |
| `get_service` | Get service details | GET /services/{id} |
| `update_service` | Update/resize service | PATCH /services/{id} |
| `delete_service` | Destroy service | DELETE /services/{id} |
| `freeze_service` | Pause service | POST /services/{id}/freeze |
| `unfreeze_service` | Resume service | POST /services/{id}/unfreeze |
| `lock_service` | Prevent deletion | POST /services/{id}/lock |
| `unlock_service` | Allow deletion | POST /services/{id}/unlock |
| `get_service_logs` | Get bootstrap logs | GET /services/{id}/logs |
| `get_service_env` | Get env vault status | GET /services/{id}/env |
| `set_service_env` | Set environment vars | PUT /services/{id}/env |
| `delete_service_env` | Delete env vars | DELETE /services/{id}/env |
| `export_service_env` | Export decrypted env | POST /services/{id}/env/export |
| `redeploy_service` | Re-run bootstrap | POST /services/{id}/redeploy |
| `execute_in_service` | Run command in service | POST /services/{id}/execute |

### Snapshots (8 functions)
| Function | Description | Endpoint |
|----------|-------------|----------|
| `list_snapshots` | List all snapshots | GET /snapshots |
| `get_snapshot` | Get snapshot details | GET /snapshots/{id} |
| `delete_snapshot` | Delete snapshot | DELETE /snapshots/{id} |
| `session_snapshot` | Snapshot a session | POST /sessions/{id}/snapshot |
| `service_snapshot` | Snapshot a service | POST /services/{id}/snapshot |
| `restore_snapshot` | Restore from snapshot | POST /snapshots/{id}/restore |
| `lock_snapshot` | Prevent deletion | POST /snapshots/{id}/lock |
| `unlock_snapshot` | Allow deletion | POST /snapshots/{id}/unlock |
| `clone_snapshot` | Clone snapshot | POST /snapshots/{id}/clone |

### Utilities (4 functions)
| Function | Description | Endpoint |
|----------|-------------|----------|
| `get_languages` | Get supported languages | GET /languages (cached 1hr) |
| `detect_language` | Detect from filename | Local (no API call) |
| `validate_keys` | Validate API credentials | POST /keys/validate |
| `image` | Generate AI images | POST /image |

## Authentication

### 4-Tier Credential Resolution

All SDKs resolve credentials in this priority order:

1. **Function arguments** (highest priority)
   ```python
   result = execute_code("python", code, public_key="pk-xxx", secret_key="sk-xxx")
   ```

2. **Environment variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY="unsb-pk-xxxx"
   export UNSANDBOX_SECRET_KEY="unsb-sk-xxxx"
   ```

3. **Home directory config**
   ```
   ~/.unsandbox/accounts.csv
   Format: public_key,secret_key (one per line)
   ```

4. **Local directory config**
   ```
   ./accounts.csv
   Format: public_key,secret_key (one per line)
   ```

### HMAC-SHA256 Request Signing

Every API request includes:

```
Authorization: Bearer <public_key>
X-Timestamp: <unix_seconds>
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
```

Example signature message:
```
1704067200:POST:/execute:{"language":"python","code":"print(1)"}
```

## Caching

### Languages Cache

All SDKs cache the `/languages` response:
- **Location**: `~/.unsandbox/languages.json`
- **TTL**: 1 hour (3600 seconds)
- **Behavior**: Check cache freshness before API call

## Line Count Comparison

### Why un.c is 6,354 lines vs Python's 1,714 lines

The reference C implementation includes infrastructure that high-level languages get from standard libraries:

| Component | un.c (lines) | Python | Notes |
|-----------|--------------|--------|-------|
| **API Functions** | ~2,500 | ~1,500 | Similar complexity |
| **CLI main()** | 1,306 | 0 | SDK is library-only |
| **CLI help/usage** | 154 | 0 | No CLI in SDK |
| **HTTP client** | 574 | 0 | `requests` library |
| **JSON parsing** | ~200 | 0 | `json` module |
| **HMAC crypto** | ~100 | 0 | `hmac` module |
| **Memory management** | ~300 | 0 | Garbage collected |
| **String utilities** | ~200 | 0 | Built-in |
| **Total** | **6,354** | **1,714** | 3.7x difference |

### What Python Gets "For Free"

```python
import requests      # Replaces ~574 lines of curl code
import json          # Replaces ~200 lines of JSON parsing
import hmac          # Replaces ~100 lines of crypto
import hashlib       # Replaces SHA-256 implementation
```

### Feature Parity

Despite the line count difference, all SDKs implement:
- ✅ All 43 API functions
- ✅ 4-tier credential resolution
- ✅ HMAC-SHA256 request signing
- ✅ 1-hour languages caching
- ✅ Exponential backoff polling
- ✅ Proper error handling

The C implementation additionally includes:
- Full CLI with argument parsing
- Interactive shell support (WebSocket)
- Output formatting and display
- File input handling

## SDK Size Summary

| Language | Sync | Async | Total | Notes |
|----------|------|-------|-------|-------|
| Python | 1,714 | 1,701 | 3,415 | requests/aiohttp |
| JavaScript | 1,131 | 1,209 | 2,340 | https/fetch |
| Go | 1,080 | 1,642 | 2,722 | net/http + goroutines |
| Java | 1,856 | 1,849 | 3,705 | HttpURLConnection |
| Ruby | 1,264 | 1,543 | 2,807 | net/http + Future |
| Rust | 1,856 | 1,949 | 3,805 | reqwest + tokio |
| PHP | 1,403 | 1,346 | 2,749 | cURL + Guzzle |
| **Total** | | | **21,543** | |

Reference: `un.c` = 6,354 lines (includes CLI)

## Usage Examples

### Python (Sync)

```python
from un import execute_code, list_services, image

# Execute code
result = execute_code("python", 'print("Hello, World!")')
print(result["stdout"])

# List services
services = list_services()
for svc in services:
    print(f"{svc['name']}: {svc['status']}")

# Generate AI image
img = image("A sunset over mountains")
print(img["images"][0])
```

### Python (Async)

```python
import asyncio
from un_async import execute_code, list_services

async def main():
    result = await execute_code("python", 'print("Hello!")')
    print(result["stdout"])

asyncio.run(main())
```

### JavaScript (Sync)

```javascript
const un = require('./un');

const result = un.executeCode("javascript", 'console.log("Hello!")');
console.log(result.stdout);
```

### Go

```go
import "un"

func main() {
    creds, _ := un.ResolveCredentials("", "")
    result, _ := un.ExecuteCode(creds, "python", `print("Hello")`)
    fmt.Println(result.Stdout)
}
```

### Rust

```rust
use un::{execute_code, resolve_credentials};

fn main() -> Result<()> {
    let creds = resolve_credentials(None, None)?;
    let result = execute_code("python", r#"print("Hello")"#, &creds)?;
    println!("{}", result.output);
    Ok(())
}
```

## Error Handling

All SDKs define these error types:

| Error | Description |
|-------|-------------|
| `CredentialsError` | No credentials found or invalid |
| `APIError` | API returned error response |
| `TimeoutError` | Job polling exceeded timeout |
| `NetworkError` | Connection failed |

## Contributing

When adding new API endpoints:

1. Update all 14 SDK files (7 languages × 2 variants)
2. Follow existing patterns for authentication and error handling
3. Add proper documentation (docstrings/comments)
4. Test with the SDK test framework

## License

PUBLIC DOMAIN - No license, no warranty.

Part of the permacomputer project: https://permacomputer.com
