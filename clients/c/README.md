# unsandbox.com C SDK

A complete, production-ready C SDK for the unsandbox.com code execution API.

## Features

- **Synchronous execution** (`unsandbox_execute`) - Get results immediately
- **Asynchronous execution** (`unsandbox_execute_async` + `unsandbox_wait_job`) - Fire and forget with polling
- **Job management** - Query, cancel, and list jobs
- **Language detection** - Automatically detect language from file extensions
- **Language support** - 50+ languages including Python, JavaScript, Go, Rust, etc.
- **Credential management** - 4-tier priority system (args > env > ~/.unsandbox/ > ./accounts.csv)
- **HMAC-SHA256 authentication** - Built-in OpenSSL integration
- **HTTP client** - libcurl-based requests with proper error handling
- **Memory safe** - Proper allocation and deallocation patterns
- **No heavy dependencies** - Only requires libcurl and OpenSSL (standard on Linux)

## Installation

### Dependencies

```bash
sudo apt install build-essential libcurl4-openssl-dev libssl-dev
```

### Building

```bash
# Build library and examples
cd clients/c
make

# Just build the library (src/unsandbox.c and src/unsandbox.h)
make lib

# Build examples (hello_world, fibonacci)
make examples

# Run tests
make test

# Clean
make clean
```

## Quick Start

### As a Library

Include the header and link with libcurl and libssl:

```c
#include "unsandbox.h"

int main(void) {
    // Execute code synchronously
    unsandbox_result_t *result = unsandbox_execute(
        "python",
        "print('Hello, World!')",
        NULL,  // uses env vars or ~/.unsandbox/accounts.csv
        NULL
    );

    if (result && result->success) {
        printf("Output: %s\n", result->stdout);
        unsandbox_free_result(result);
    } else {
        printf("Error: %s\n", unsandbox_last_error());
        return 1;
    }

    return 0;
}
```

Compile:

```bash
gcc -o myapp myapp.c src/unsandbox.c -Isrc -lcurl -lssl -lcrypto
```

### Examples

#### Hello World (Synchronous)

```c
#include "unsandbox.h"

int main(void) {
    // Execute immediately, wait for result
    unsandbox_result_t *result = unsandbox_execute(
        "python",
        "print('Hello')",
        NULL, NULL
    );

    if (result) {
        printf("%s", result->stdout);
        unsandbox_free_result(result);
    }
    return 0;
}
```

Run the example:

```bash
cd examples
gcc -o hello_world hello_world.c ../src/unsandbox.c -I../src -lcurl -lssl -lcrypto
./hello_world
```

#### Fibonacci (Asynchronous)

```c
#include "unsandbox.h"

int main(void) {
    // Submit job
    char *job_id = unsandbox_execute_async("python",
        "def fib(n): return n if n<2 else fib(n-1)+fib(n-2)\n"
        "print(fib(10))",
        NULL, NULL);

    if (job_id) {
        printf("Job: %s\n", job_id);

        // Wait for completion (with exponential backoff polling)
        unsandbox_result_t *result = unsandbox_wait_job(job_id, NULL, NULL);
        if (result) {
            printf("Result: %s\n", result->stdout);
            unsandbox_free_result(result);
        }
        free(job_id);
    }
    return 0;
}
```

Run the example:

```bash
cd examples
./fibonacci
```

## API Reference

### Core Functions

#### `unsandbox_execute(language, code, public_key, secret_key)`

Execute code synchronously and return result immediately.

```c
unsandbox_result_t *result = unsandbox_execute("python", "print(42)", NULL, NULL);

if (result->success) {
    printf("stdout: %s\n", result->stdout);
    printf("stderr: %s\n", result->stderr);
    printf("exit_code: %d\n", result->exit_code);
    unsandbox_free_result(result);
}
```

**Parameters:**
- `language` (const char *) - Language identifier (e.g., "python", "javascript")
- `code` (const char *) - Code to execute
- `public_key` (const char *) - API public key (NULL to use env/config)
- `secret_key` (const char *) - API secret key (NULL to use env/config)

**Returns:** `unsandbox_result_t *` - Result struct or NULL on error

**Result struct:**
```c
typedef struct {
    char *stdout;           // Program output
    char *stderr;           // Error output
    int exit_code;          // Process exit code
    char *language;         // Language used
    double execution_time;  // Time in seconds
    int success;            // 1 if successful, 0 if error
    char *error_message;    // Error description (if any)
} unsandbox_result_t;
```

#### `unsandbox_execute_async(language, code, public_key, secret_key)`

Submit code for async execution, returns immediately with job ID.

```c
char *job_id = unsandbox_execute_async("python", "print(42)", NULL, NULL);
if (job_id) {
    printf("Job ID: %s\n", job_id);
    free(job_id);
}
```

**Returns:** `char *` - Job ID string (must be freed) or NULL on error

#### `unsandbox_wait_job(job_id, public_key, secret_key)`

Wait for async job completion using exponential backoff polling.

```c
unsandbox_result_t *result = unsandbox_wait_job(job_id, NULL, NULL);
if (result) {
    // Process result
    unsandbox_free_result(result);
}
```

**Returns:** `unsandbox_result_t *` - Result when job completes

#### `unsandbox_get_job(job_id, public_key, secret_key)`

Get current job status without waiting.

```c
unsandbox_job_t *job = unsandbox_get_job(job_id, NULL, NULL);
if (job) {
    printf("Status: %s\n", job->status);  // "pending", "running", "completed"
    unsandbox_free_job(job);
}
```

#### `unsandbox_cancel_job(job_id, public_key, secret_key)`

Cancel a running job.

```c
int result = unsandbox_cancel_job(job_id, NULL, NULL);
if (result == 0) {
    printf("Job cancelled\n");
}
```

#### `unsandbox_list_jobs(public_key, secret_key)`

List all active jobs for the user.

```c
unsandbox_job_list_t *jobs = unsandbox_list_jobs(NULL, NULL);
if (jobs) {
    for (size_t i = 0; i < jobs->count; i++) {
        printf("%s: %s\n", jobs->jobs[i].id, jobs->jobs[i].status);
    }
    unsandbox_free_job_list(jobs);
}
```

### Language Functions

#### `unsandbox_detect_language(filename)`

Detect language from file extension.

```c
const char *lang = unsandbox_detect_language("script.py");  // Returns "python"
```

Supported extensions:
- Python: `.py`
- JavaScript: `.js`, TypeScript: `.ts`
- Go: `.go`, Rust: `.rs`, C: `.c`, C++: `.cpp`
- Ruby: `.rb`, PHP: `.php`, Bash: `.sh`
- And 40+ more languages...

#### `unsandbox_get_languages(public_key, secret_key)`

Get list of all supported languages from the API.

```c
unsandbox_languages_t *langs = unsandbox_get_languages(NULL, NULL);
if (langs) {
    for (size_t i = 0; i < langs->count; i++) {
        printf("%s\n", langs->languages[i]);
    }
    unsandbox_free_languages(langs);
}
```

### Credential Management

#### 4-Tier Priority System

Credentials are resolved in this order:

1. **Function arguments** - `unsandbox_execute("python", code, "key", "secret")`
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY`, `UNSANDBOX_SECRET_KEY`
3. **Home directory** - `~/.unsandbox/accounts.csv` (line 0, or `UNSANDBOX_ACCOUNT=N`)
4. **Current directory** - `./accounts.csv` (same format)

#### accounts.csv Format

```csv
public_key_1,secret_key_1
public_key_2,secret_key_2
public_key_3,secret_key_3
```

#### Usage Examples

```bash
# Option 1: Environment variables
export UNSANDBOX_PUBLIC_KEY="unsb-pk-..."
export UNSANDBOX_SECRET_KEY="unsb-sk-..."
./myapp

# Option 2: Config file
mkdir -p ~/.unsandbox
echo "unsb-pk-...,unsb-sk-..." > ~/.unsandbox/accounts.csv
chmod 600 ~/.unsandbox/accounts.csv
./myapp

# Option 3: Multiple accounts (select with UNSANDBOX_ACCOUNT)
echo "unsb-pk-1,unsb-sk-1" > ~/.unsandbox/accounts.csv
echo "unsb-pk-2,unsb-sk-2" >> ~/.unsandbox/accounts.csv
UNSANDBOX_ACCOUNT=1 ./myapp  # Uses account 2 (0-indexed)

# Option 4: Inline (least secure)
./myapp --public-key=unsb-pk-... --secret-key=unsb-sk-...
```

#### Manual Credential Resolution

```c
char *public_key = NULL, *secret_key = NULL;

if (unsandbox_resolve_credentials(&public_key, &secret_key, NULL, NULL) == 0) {
    // Use public_key and secret_key
    unsandbox_result_t *result = unsandbox_execute("python", code, 
                                                   public_key, secret_key);
    // Clean up
    free(public_key);
    free(secret_key);
    unsandbox_free_result(result);
}
```

### Memory Management

**Always clean up allocated memory:**

```c
void unsandbox_free_result(unsandbox_result_t *result);
void unsandbox_free_job(unsandbox_job_t *job);
void unsandbox_free_job_list(unsandbox_job_list_t *jobs);
void unsandbox_free_languages(unsandbox_languages_t *langs);
void unsandbox_free_quota(unsandbox_quota_t *quota);
```

### Utility Functions

#### `unsandbox_last_error()`

Get the last error message from a failed operation.

```c
unsandbox_result_t *result = unsandbox_execute("python", "code", NULL, NULL);
if (!result) {
    printf("Error: %s\n", unsandbox_last_error());
}
```

#### `unsandbox_health_check()`

Check if the API is available.

```c
int status = unsandbox_health_check();
// Returns: 1 = available, 0 = unavailable, -1 = error
if (status == 1) {
    printf("API is up\n");
}
```

#### `unsandbox_version()`

Get SDK version.

```c
printf("SDK version: %s\n", unsandbox_version());
```

## Supported Languages (50+)

**Interpreted:** Python, JavaScript, TypeScript, Ruby, PHP, Bash, Perl, Lua, R, Clojure, CommonLisp, Elixir, Erlang, Groovy, Idris2, Julia, Nim, Raku, Scheme, Tcl, Dart, Deno, Crystal, Kotlin

**Compiled:** C, C++, Go, Rust, Java, C#, F#, Haskell, OCaml, Cobol, D, Fortran, Odin, Pascal, V, Zig, Objective-C

**Other:** Prolog, Forth, WASM (C/C++/Rust/Zig/Go via Emscripten)

## Error Handling

All functions return NULL or negative values on error. Check `unsandbox_last_error()` for details:

```c
unsandbox_result_t *result = unsandbox_execute("python", "code", NULL, NULL);
if (!result) {
    fprintf(stderr, "Error: %s\n", unsandbox_last_error());
    return 1;
}

// Check execution errors
if (result->exit_code != 0) {
    fprintf(stderr, "Execution failed (exit %d): %s\n", 
            result->exit_code, result->stderr);
}

unsandbox_free_result(result);
```

## Authentication

Requests are authenticated using HMAC-SHA256:

```
Authorization: Bearer <public_key>
X-Timestamp: <unix_seconds>
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
```

The SDK handles this automatically.

## Testing

```bash
# Run all tests
make test

# Test specific functionality
make test-library

# Run examples
./examples/hello_world
./examples/fibonacci
```

## Performance

- **Synchronous execution:** 50-200ms (Python/Bash) to 5-30s (JVM languages)
- **Asynchronous execution:** ~50ms allocation + background execution
- **Polling backoff:** 300ms → 450ms → 700ms → 900ms → ... → 2000ms (capped)

## Limitations

- Max request body: 1MB per execution
- Max concurrent jobs per API key: Based on subscription tier
- Timeout: 30 seconds per request
- JSON parsing is simplified (use a real JSON library for complex responses)

## Troubleshooting

### "No credentials found"

Set credentials via environment, config file, or function arguments:

```bash
export UNSANDBOX_PUBLIC_KEY="your-key"
export UNSANDBOX_SECRET_KEY="your-secret"
./myapp
```

### "HTTP 401 Unauthorized"

Check your credentials are valid:
```bash
curl -H "Authorization: Bearer $UNSANDBOX_PUBLIC_KEY" https://api.unsandbox.com/health
```

### Compilation errors

Ensure dependencies are installed:
```bash
sudo apt install libcurl4-openssl-dev libssl-dev
```

Check include paths:
```bash
gcc -I/usr/include -c src/unsandbox.c
```

## License

PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

Use freely for any purpose.

## Examples Directory

- `hello_world.c` - Basic synchronous execution
- `fibonacci.c` - Asynchronous execution with polling

Compile examples:
```bash
cd examples
make -C .. examples  # Or:
gcc -o hello_world hello_world.c ../src/unsandbox.c -I../src -lcurl -lssl -lcrypto
./hello_world
```

## See Also

- Python SDK: `../python/sync/src/un.py`
- Go SDK: `../go/sync/src/un.go`
- API Documentation: `https://api.unsandbox.com/`
