# Unsandbox C SDK Examples

This directory contains practical examples demonstrating core functionality of the unsandbox C library.

## Examples Overview

### 1. hello_world.c - Simple Execute Example

A minimal example showing basic setup and execution patterns.

**What it demonstrates:**
- Loading credentials from environment variables
- Preparing code for execution
- Basic error handling
- Memory management patterns
- API structure overview

**Topics:**
- `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY` environment variables
- `execute_code()` function signature
- Return value handling (malloc'd strings)
- Safe memory cleanup with `free()`

**Compile:**
```bash
gcc -o hello_world hello_world.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
```

**Run:**
```bash
export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
./hello_world
```

### 2. fibonacci.c - Computation Example

Demonstrates executing computational code across multiple programming languages.

**What it demonstrates:**
- Same algorithm in Python, JavaScript, Go, and Rust
- Execution timing with `clock_gettime()`
- Multi-language support
- Timeout handling
- Error detection patterns

**Topics:**
- `execute_code()` for synchronous execution
- Language auto-detection from file extensions
- Measuring execution time
- Timeout detection and handling
- Runtime error vs output distinction

**Compile:**
```bash
gcc -o fibonacci fibonacci.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
```

**Run:**
```bash
export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
./fibonacci
```

**Code examples included:**
- Python recursive fibonacci
- JavaScript functional fibonacci
- Go compiled fibonacci
- Rust zero-cost abstraction fibonacci

### 3. error_handling.c - Proper Error Handling Patterns

Comprehensive error handling demonstration with categorized error types.

**What it demonstrates:**
- Error classification (auth, rate-limit, timeout, runtime, server)
- Error type detection from responses
- Retry strategies with exponential backoff
- Transient vs permanent errors
- Error logging patterns

**Topics:**
- `ErrorType` enumeration
- HTTP error codes (401, 429, 500)
- Response content parsing
- Automatic retry logic
- Timeout detection in output

**Compile:**
```bash
gcc -o error_handling error_handling.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
```

**Run:**
```bash
./error_handling
```

**Patterns demonstrated:**

1. **NULL check** - Detection of memory allocation failures
2. **HTTP error checking** - Parsing status codes from responses
3. **Retry logic** - Exponential backoff for rate limits
4. **Timeout detection** - Identifying killed processes
5. **Error logging** - Writing to log files for debugging

**Error types:**
- `ERROR_INVALID_CREDS` - 401 Unauthorized
- `ERROR_RATE_LIMITED` - 429 Too Many Requests
- `ERROR_TIMEOUT` - Execution exceeded time limit
- `ERROR_RUNTIME_ERROR` - Exception in user code
- `ERROR_SERVER_ERROR` - 500+ server errors
- `ERROR_NETWORK` - Connection failed
- `ERROR_OUT_OF_MEMORY` - malloc() failed

### 4. credentials.c - Credential Loading from 4 Sources

Shows how to load credentials with priority ordering.

**What it demonstrates:**
- Credential priority ordering
- All four credential sources
- Account selection and management
- Config file format
- Security best practices

**Topics:**
- CLI flags: `-p` (public key) and `-k` (secret key)
- Environment variables: `UNSANDBOX_PUBLIC_KEY`, `UNSANDBOX_SECRET_KEY`
- Config file: `~/.unsandbox/accounts.csv`
- Account selection: `--account N` and `UNSANDBOX_ACCOUNT=N`

**Compile:**
```bash
gcc -o credentials credentials.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
```

**Run:**
```bash
./credentials
```

**Credential sources (highest to lowest priority):**

1. **CLI Flags**
   ```bash
   un -p unsb-pk-xxxxx -k unsb-sk-xxxxx script.py
   ```

2. **Environment Variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
   export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
   ./un script.py
   ```

3. **Config File**
   ```bash
   ~/.unsandbox/accounts.csv
   # Format: public_key,secret_key (one per line)
   un --account 0 script.py  # Use first account
   ```

4. **Fallback (Error)**
   ```
   Error: API credentials required
   ```

**Setup config file:**
```bash
mkdir -p ~/.unsandbox
cat > ~/.unsandbox/accounts.csv << 'EOF'
unsb-pk-account1,unsb-sk-account1
unsb-pk-account2,unsb-sk-account2
EOF
chmod 600 ~/.unsandbox/accounts.csv
```

## Common Compilation Patterns

### Standard compile (requires un.c)
```bash
gcc -o example example.c -I../../.. \
  -lcurl -lwebsockets -lssl -lcrypto
```

### With optimization
```bash
gcc -O2 -o example example.c -I../../.. \
  -lcurl -lwebsockets -lssl -lcrypto
```

### With debugging symbols
```bash
gcc -g -o example example.c -I../../.. \
  -lcurl -lwebsockets -lssl -lcrypto
```

### With all warnings
```bash
gcc -Wall -Wextra -o example example.c -I../../.. \
  -lcurl -lwebsockets -lssl -lcrypto
```

## Dependencies

Required libraries:
```bash
apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev
```

## Key Patterns

### Pattern 1: Safe memory allocation

```c
char *result = execute_code(language, code, pk, sk);
if (!result) {
  fprintf(stderr, "Failed to allocate result\n");
  return 1;
}

// Use result...

free(result);  // Always free!
```

### Pattern 2: Error detection

```c
char *result = execute_code(language, code, pk, sk);
if (!result) {
  return 1;
}

if (strstr(result, "error") || strstr(result, "Error")) {
  fprintf(stderr, "Execution error: %s\n", result);
  free(result);
  return 1;
}

printf("Output: %s\n", result);
free(result);
```

### Pattern 3: Retry with backoff

```c
const int MAX_RETRIES = 3;
const int RETRY_DELAY_MS = 1000;

for (int attempt = 0; attempt < MAX_RETRIES; attempt++) {
  char *result = execute_code(language, code, pk, sk);
  if (result) {
    printf("Output: %s\n", result);
    free(result);
    return 0;
  }

  int backoff = RETRY_DELAY_MS * (1 << attempt);
  usleep(backoff * 1000);
}

fprintf(stderr, "Failed after %d attempts\n", MAX_RETRIES);
return 1;
```

### Pattern 4: Async execution

```c
// Start async execution
char *job_id = execute_async(language, code, pk, sk);
if (!job_id) {
  fprintf(stderr, "Failed to start async execution\n");
  return 1;
}

printf("Job ID: %s\n", job_id);

// Poll for completion
sleep(1);  // Wait before checking
char *result = wait_for_job(job_id, pk, sk);
if (result) {
  printf("Result: %s\n", result);
  free(result);
}

free(job_id);
```

### Pattern 5: Timeout handling

```c
struct timespec start, end;
clock_gettime(CLOCK_MONOTONIC, &start);

char *result = execute_code(language, code, pk, sk);

clock_gettime(CLOCK_MONOTONIC, &end);
long elapsed_ms = (end.tv_sec - start.tv_sec) * 1000 +
                  (end.tv_nsec - start.tv_nsec) / 1000000;

if (elapsed_ms > 300000) {  // 300 seconds
  fprintf(stderr, "Execution took %ld ms (likely timed out)\n", elapsed_ms);
}

if (result) {
  printf("Output: %s\n", result);
  free(result);
}
```

## API Reference

### Synchronous execution
```c
char *execute_code(
  const char *language,
  const char *code,
  const char *public_key,
  const char *secret_key
);
```

Returns: JSON response string (must be freed by caller) or NULL on failure

### Asynchronous execution
```c
char *execute_async(
  const char *language,
  const char *code,
  const char *public_key,
  const char *secret_key
);
```

Returns: Job ID string (must be freed) or NULL on failure

### Get job status
```c
char *get_job(
  const char *job_id,
  const char *public_key,
  const char *secret_key
);
```

Returns: JSON status (must be freed) or NULL on failure

### Wait for completion
```c
char *wait_for_job(
  const char *job_id,
  const char *public_key,
  const char *secret_key
);
```

Returns: Final result (must be freed) or NULL on failure

### Cancel job
```c
char *cancel_job(
  const char *job_id,
  const char *public_key,
  const char *secret_key
);
```

Returns: Cancellation response (must be freed) or NULL on failure

### List jobs
```c
char *list_jobs(
  const char *public_key,
  const char *secret_key
);
```

Returns: JSON job list (must be freed) or NULL on failure

### Get supported languages
```c
char *get_languages(
  const char *public_key,
  const char *secret_key
);
```

Returns: JSON language list (must be freed) or NULL on failure

### Language detection
```c
const char *detect_language(const char *filename);
```

Returns: Language name (e.g., "python", "javascript") or NULL

## Environment Variables

### Required for execution
- `UNSANDBOX_PUBLIC_KEY` - API public key (unsb-pk-xxxxx)
- `UNSANDBOX_SECRET_KEY` - API secret key (unsb-sk-xxxxx)

### Optional
- `UNSANDBOX_ACCOUNT` - Account index in config file (default: 0)

## Configuration File

Location: `~/.unsandbox/accounts.csv`

Format:
```
unsb-pk-account1,unsb-sk-account1
unsb-pk-account2,unsb-sk-account2
unsb-pk-account3,unsb-sk-account3
```

Permissions: Should be `600` (readable only by owner)

## Error Codes

| Code | HTTP Status | Meaning | Action |
|------|-----------|---------|--------|
| 401 | Unauthorized | Invalid credentials | Check API keys |
| 429 | Too Many Requests | Rate limited | Implement backoff |
| 500+ | Server Error | Server issue | Retry later |
| Timeout | N/A | Execution time exceeded | Optimize code |
| OOM | N/A | Out of memory | Reduce input size |

## Memory Management

**IMPORTANT**: All functions returning `char *` allocate memory with `malloc()`.
You must free this memory when done:

```c
char *result = execute_code(...);
if (result) {
  // Use result...
  free(result);  // MUST do this!
}
```

Failure to free will cause memory leaks in long-running applications.

## Testing

Compile all examples:
```bash
gcc -o hello_world hello_world.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
gcc -o fibonacci fibonacci.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
gcc -o error_handling error_handling.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
gcc -o credentials credentials.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
```

Run examples:
```bash
export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx

./hello_world
./fibonacci
./error_handling
./credentials
```

## Security Notes

1. **Never hardcode credentials** - Always use environment variables or config files
2. **Protect config files** - Use `chmod 600 ~/.unsandbox/accounts.csv`
3. **Rotate keys regularly** - Old keys should be revoked and removed
4. **Use separate credentials** - Dev, staging, and production should have different keys
5. **Don't log secrets** - Never print API keys to logs or stdout
6. **Clean shell history** - Remove entries with credentials: `history -c`

## Example Workflows

### Web server integration
```c
// In request handler
const char *language = "python";
const char *code = user_submitted_code;
const char *pk = getenv("UNSANDBOX_PUBLIC_KEY");
const char *sk = getenv("UNSANDBOX_SECRET_KEY");

char *result = execute_code(language, code, pk, sk);
if (result) {
  send_response(200, result);
  free(result);
} else {
  send_response(500, "Execution failed");
}
```

### Batch processing
```c
// Process many files
for (int i = 0; i < file_count; i++) {
  char *code = read_file(files[i]);
  char *result = execute_code("python", code, pk, sk);

  if (result) {
    printf("File %s: %s\n", files[i], result);
    free(result);
  } else {
    fprintf(stderr, "Failed: %s\n", files[i]);
  }

  free(code);
}
```

### Async job management
```c
// Start job
char *job_id = execute_async("python", code, pk, sk);
if (job_id) {
  // Store job_id in database
  // Later, check status periodically
  char *status = get_job(job_id, pk, sk);
  if (status) {
    printf("Status: %s\n", status);
    free(status);
  }
  free(job_id);
}
```

## Troubleshooting

### "API credentials required"
Set environment variables:
```bash
export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
```

### "401 Unauthorized"
Check that API keys are correct:
```bash
echo $UNSANDBOX_PUBLIC_KEY
echo $UNSANDBOX_SECRET_KEY
```

### "429 Rate limited"
Implement exponential backoff in retry logic (see error_handling.c)

### Memory leaks
Use valgrind to check:
```bash
valgrind --leak-check=full ./example
```

### Compilation errors
Ensure dependencies are installed:
```bash
apt install libcurl4-openssl-dev libwebsockets-dev libssl-dev
```

## Further Reading

- See `../Makefile` for compilation and testing infrastructure
- See `../tests/test_library.c` for unit test patterns
- See `../../un.c` for full library implementation
- Visit https://unsandbox.com for API documentation
