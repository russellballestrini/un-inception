# unsandbox Go SDK (Asynchronous)

PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

Asynchronous Go client for the unsandbox.com code execution API.

## Features

- **Channel-based async operations** - All API calls return channels for non-blocking execution
- **Goroutine-safe** - Safe for concurrent use from multiple goroutines
- **HMAC-SHA256 authentication** - Secure request signing
- **4-tier credential system** - Flexible credential resolution
- **Language detection** - Automatic language detection from file extensions
- **Job polling with exponential backoff** - Efficient polling for long-running jobs
- **Languages caching** - 1-hour cache for supported languages

## Installation

```bash
go get github.com/unsandbox/un-go-async
```

## Quick Start

```go
package main

import (
    "fmt"
    "log"

    un_async "github.com/unsandbox/un-go-async/src"
)

func main() {
    // Resolve credentials from environment or config files
    creds, err := un_async.ResolveCredentials("", "")
    if err != nil {
        log.Fatal(err)
    }

    // Execute code asynchronously (returns channel)
    resultChan := un_async.ExecuteCode(creds, "python", `print("Hello, World!")`)
    result := <-resultChan

    if result.Err != nil {
        log.Fatal(result.Err)
    }

    fmt.Println(result.Data["stdout"])
}
```

## Authentication

### 4-Tier Credential Resolution

Credentials are resolved in this priority order:

1. **Function arguments** - Pass `publicKey` and `secretKey` directly
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
3. **User config** - `~/.unsandbox/accounts.csv`
4. **Local config** - `./accounts.csv`

CSV format:
```csv
public_key,secret_key
```

Select account by index using `UNSANDBOX_ACCOUNT=N` environment variable (0-based).

### HMAC-SHA256 Request Signing

All requests are signed with HMAC-SHA256:

```
Authorization: Bearer <public_key>
X-Timestamp: <unix_seconds>
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
```

## API Reference

### Credential Resolution

```go
creds, err := un_async.ResolveCredentials(publicKey, secretKey string) (*Credentials, error)
```

### Code Execution

```go
// Execute and wait for completion (blocks via polling)
resultChan := un_async.ExecuteCode(creds, language, code string) <-chan ExecuteResult

// Submit async job (returns immediately with job ID)
jobChan := un_async.ExecuteAsync(creds, language, code string) <-chan JobIDResult

// Get job status (single poll)
jobChan := un_async.GetJob(creds, jobID string) <-chan JobResult

// Wait for job completion with timeout
waitChan := un_async.WaitForJob(creds, jobID string, timeout time.Duration) <-chan JobResult

// Cancel a running job
cancelChan := un_async.CancelJob(creds, jobID string) <-chan CancelResult

// List all jobs
listChan := un_async.ListJobs(creds) <-chan JobListResult
```

### Language Operations

```go
// Get supported languages (cached)
langChan := un_async.GetLanguages(creds) <-chan LanguagesResult

// Detect language from filename (synchronous, no I/O)
lang := un_async.DetectLanguage(filename string) string
```

### Snapshot Operations

```go
// Create session snapshot
snapChan := un_async.SessionSnapshot(creds, sessionID, name string, hot bool) <-chan SnapshotResult

// Create service snapshot
snapChan := un_async.ServiceSnapshot(creds, serviceID, name string) <-chan SnapshotResult

// List snapshots
listChan := un_async.ListSnapshots(creds) <-chan SnapshotListResult

// Restore snapshot
restoreChan := un_async.RestoreSnapshot(creds, snapshotID string) <-chan RestoreResult

// Delete snapshot
deleteChan := un_async.DeleteSnapshot(creds, snapshotID string) <-chan DeleteResult
```

## Result Types

All async functions return channels with typed results:

```go
type ExecuteResult struct {
    Data map[string]interface{}
    Err  error
}

type JobIDResult struct {
    JobID string
    Err   error
}

type JobResult struct {
    Data map[string]interface{}
    Err  error
}

type JobListResult struct {
    Jobs []map[string]interface{}
    Err  error
}

type LanguagesResult struct {
    Languages []string
    Err       error
}

type SnapshotResult struct {
    SnapshotID string
    Err        error
}
```

## Examples

### Concurrent Execution

```go
package main

import (
    "fmt"
    "sync"

    un_async "github.com/unsandbox/un-go-async/src"
)

func main() {
    creds, _ := un_async.ResolveCredentials("", "")

    languages := []string{"python", "javascript", "ruby"}
    var wg sync.WaitGroup

    for _, lang := range languages {
        wg.Add(1)
        go func(l string) {
            defer wg.Done()
            resultChan := un_async.ExecuteCode(creds, l, `print("Hello from " + "` + l + `")`)
            result := <-resultChan
            if result.Err == nil {
                fmt.Printf("[%s] %v\n", l, result.Data["stdout"])
            }
        }(lang)
    }

    wg.Wait()
}
```

### Fire-and-Forget with Polling

```go
// Submit job without waiting
jobChan := un_async.ExecuteAsync(creds, "python", longRunningCode)
jobResult := <-jobChan

// Do other work...

// Later, check on the job
waitChan := un_async.WaitForJob(creds, jobResult.JobID, 60*time.Second)
result := <-waitChan
```

## Testing

```bash
cd /path/to/clients/go/async
go test ./tests/...

# Verbose output
go test -v ./tests/...
```

## Polling Strategy

Job polling uses exponential backoff:

| Poll # | Delay (ms) | Cumulative (ms) |
|--------|-----------|-----------------|
| 1      | 300       | 300             |
| 2      | 450       | 750             |
| 3      | 700       | 1450            |
| 4      | 900       | 2350            |
| 5      | 650       | 3000            |
| 6      | 1600      | 4600            |
| 7+     | 2000      | 6600+           |

## Supported Languages

50+ runtimes including:

- **Interpreted**: Python, JavaScript, Ruby, PHP, Perl, Lua, R, Bash
- **Compiled**: Go, Rust, C, C++, Java, Kotlin, C#, F#
- **Functional**: Haskell, OCaml, Clojure, Elixir, Erlang
- **And more**: Julia, Nim, Zig, Crystal, Dart, TypeScript, etc.

Use `DetectLanguage()` for automatic detection from file extensions.

## Differences from Sync SDK

| Sync SDK | Async SDK |
|----------|-----------|
| `result, err := ExecuteCode(...)` | `resultChan := ExecuteCode(...); result := <-resultChan` |
| Blocks calling goroutine | Returns immediately with channel |
| Sequential execution | Easy parallel execution |
| Direct return values | Results wrapped in typed structs |

## License

PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
