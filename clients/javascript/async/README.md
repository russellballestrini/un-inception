# Unsandbox Async JavaScript SDK

Asynchronous JavaScript SDK for [unsandbox.com](https://unsandbox.com) code execution service.

Execute code in 50+ programming languages with full async/await support in Node.js.

## Features

- **ES Modules**: Native ESM with async/await and native fetch
- **50+ Languages**: Python, JavaScript, Go, Rust, Java, C/C++, and 44+ more
- **Flexible Execution**: Sync execution (blocks until completion) or async (fire-and-forget)
- **Job Management**: Poll, wait, cancel running jobs
- **Credential Management**: 4-tier credential resolution system
- **Request Signing**: HMAC-SHA256 authentication
- **Language Detection**: Automatic language detection from filenames
- **Caching**: Built-in language list caching
- **Concurrent Execution**: Execute multiple jobs concurrently with `Promise.all()`

## Installation

```bash
# Clone the repository
git clone https://github.com/unsandbox/un-inception
cd clients/javascript/async

# Install dependencies (for testing)
npm install
```

## Quick Start

### Basic Async Execution

```javascript
import { executeCode } from './src/un_async.js';

// Execute code and wait for completion
const result = await executeCode('python', 'print("Hello World")');
console.log(result.stdout);
```

### Fire-and-Forget with Polling

```javascript
import { executeAsync, waitForJob } from './src/un_async.js';

// Start execution (returns immediately)
const jobId = await executeAsync('javascript', 'console.log("Job started")');
console.log(`Job ID: ${jobId}`);

// Poll for completion
const result = await waitForJob(jobId);
console.log(`Status: ${result.status}`);
console.log(`Output: ${result.stdout}`);
```

### Concurrent Execution

```javascript
import { executeCode } from './src/un_async.js';

// Run multiple executions concurrently
const results = await Promise.all([
  executeCode('python', "print('Python')"),
  executeCode('javascript', "console.log('JavaScript')"),
  executeCode('go', 'fmt.Println("Go")'),
]);

for (const result of results) {
  console.log(`Language: ${result.language}, Output: ${result.stdout}`);
}
```

## Credential Management (4-Tier Priority)

Credentials are resolved in the following order:

1. **Function Arguments** (highest priority)
   ```javascript
   const result = await executeCode(
     'python',
     "print('hello')",
     'your_public_key',
     'your_secret_key'
   );
   ```

2. **Environment Variables**
   ```bash
   export UNSANDBOX_PUBLIC_KEY="your_public_key"
   export UNSANDBOX_SECRET_KEY="your_secret_key"
   node script.js
   ```

3. **Config File** (`~/.unsandbox/accounts.csv`)
   ```
   public_key_1,secret_key_1
   public_key_2,secret_key_2
   # Select account with: export UNSANDBOX_ACCOUNT=1
   ```

4. **Local Directory** (`./accounts.csv`)
   Same format as config file

### Using Multiple Accounts

```bash
# List accounts in ~/.unsandbox/accounts.csv
# Use the second account (0-indexed)
export UNSANDBOX_ACCOUNT=1
node script.js
```

## API Reference

### Execution Functions

#### `executeCode(language, code, publicKey?, secretKey?)`

Execute code synchronously and wait for completion.

**Args:**
- `language` (string): Programming language (e.g., "python", "javascript")
- `code` (string): Source code to execute
- `publicKey` (string, optional): API public key
- `secretKey` (string, optional): API secret key

**Returns:** Promise<Object> with execution result

```javascript
const result = await executeCode('python', 'print(42)');
console.log(result.stdout);  // "42\n"
console.log(result.exit_code);  // 0
```

#### `executeAsync(language, code, publicKey?, secretKey?)`

Execute code asynchronously and return immediately with job ID.

**Args:** Same as `executeCode()`

**Returns:** Promise<string> (job ID)

```javascript
const jobId = await executeAsync('python', "print('starting')");
// Do other work while job runs...
const result = await waitForJob(jobId);
```

### Job Management Functions

#### `getJob(jobId, publicKey?, secretKey?)`

Get current status of a job (single poll, no waiting).

**Args:**
- `jobId` (string): Job ID to check
- `publicKey`, `secretKey` (optional)

**Returns:** Promise<Object> with job status

```javascript
const status = await getJob(jobId);
console.log(status.status);  // "running", "completed", "failed", etc.
```

#### `waitForJob(jobId, publicKey?, secretKey?, timeout?)`

Wait for job completion with exponential backoff polling.

**Polling Delays (ms):** [300, 450, 700, 900, 650, 1600, 2000, ...]

**Args:**
- `jobId` (string): Job ID to wait for
- `publicKey`, `secretKey` (optional)
- `timeout` (number, optional): Maximum wait time in seconds

**Returns:** Promise<Object> with final job result

**Throws:** TimeoutError if timeout is exceeded

```javascript
const result = await waitForJob(jobId);
if (result.status === 'completed') {
  console.log(result.stdout);
}
```

#### `cancelJob(jobId, publicKey?, secretKey?)`

Cancel a running job.

**Args:**
- `jobId` (string): Job ID to cancel
- `publicKey`, `secretKey` (optional)

**Returns:** Promise<Object> with cancellation confirmation

```javascript
const result = await cancelJob(jobId);
console.log(result.status);  // "cancelled"
```

#### `listJobs(publicKey?, secretKey?)`

List all jobs for the authenticated account.

**Args:** `publicKey`, `secretKey` (optional)

**Returns:** Promise<Array> of job objects

```javascript
const jobs = await listJobs();
for (const job of jobs) {
  console.log(`Job ${job.id}: ${job.status}`);
}
```

### Metadata Functions

#### `getLanguages(publicKey?, secretKey?)`

Get list of supported programming languages.

Results are cached for 1 hour in `~/.unsandbox/languages.json`.

**Args:** `publicKey`, `secretKey` (optional)

**Returns:** Promise<Array> of language identifiers

```javascript
const languages = await getLanguages();
console.log(`Supported languages: ${languages.join(', ')}`);
```

#### `detectLanguage(filename)`

Detect programming language from filename extension.

**Args:**
- `filename` (string): Filename to detect (e.g., "script.py")

**Returns:** Language identifier or null

```javascript
detectLanguage('app.js');    // "javascript"
detectLanguage('main.go');   // "go"
detectLanguage('unknown');   // null
```

### Snapshot Functions

#### `sessionSnapshot(sessionId, publicKey?, secretKey?, name?, ephemeral?)`

Create a snapshot of a session.

**Args:**
- `sessionId` (string): Session ID to snapshot
- `name` (string, optional): Snapshot name
- `ephemeral` (boolean, optional): If true, snapshot may be auto-deleted

**Returns:** Promise<string> (snapshot ID)

#### `serviceSnapshot(serviceId, publicKey?, secretKey?, name?)`

Create a snapshot of a service.

**Args:**
- `serviceId` (string): Service ID to snapshot
- `name` (string, optional): Snapshot name

**Returns:** Promise<string> (snapshot ID)

#### `listSnapshots(publicKey?, secretKey?)`

List all snapshots.

**Returns:** Promise<Array> of snapshot objects

#### `restoreSnapshot(snapshotId, publicKey?, secretKey?)`

Restore a snapshot.

**Args:**
- `snapshotId` (string): Snapshot ID to restore

**Returns:** Promise<Object> with restored resource info

#### `deleteSnapshot(snapshotId, publicKey?, secretKey?)`

Delete a snapshot.

**Args:**
- `snapshotId` (string): Snapshot ID to delete

**Returns:** Promise<Object> with deletion confirmation

## Response Format

### Successful Execution

```javascript
{
  job_id: "job_abc123",
  status: "completed",
  stdout: "output text\n",
  stderr: "",
  exit_code: 0,
  language: "python",
  duration_ms: 234
}
```

### Failed Execution

```javascript
{
  job_id: "job_xyz789",
  status: "failed",
  stdout: "partial output",
  stderr: "Error message\n",
  exit_code: 1,
  language: "python",
  duration_ms: 567
}
```

### Job Statuses

- `pending` - Waiting to execute
- `running` - Currently executing
- `completed` - Finished successfully
- `failed` - Execution error
- `timeout` - Exceeded time limit
- `cancelled` - Cancelled by user

## Examples

See the `examples/` directory for complete working examples:

- `hello_world.js` - Basic async execution
- `fibonacci.js` - Concurrent fibonacci calculations
- `concurrent_execution.js` - Running multiple jobs concurrently
- `async_job_polling.js` - Fire-and-forget job management
- `language_detection.js` - Automatic language detection

## Testing

Run the test suite:

```bash
# Install dev dependencies
npm install

# Run all tests
npm test

# Run with verbose output
npm test -- --verbose

# Run specific test file
npm test -- tests/language_detection.test.js

# Run with coverage
npm run test:coverage
```

### Test Files

- `hmac_signing.test.js` - HMAC request signing
- `language_detection.test.js` - Language detection
- `credentials.test.js` - Credential resolution system
- `async_operations.test.js` - Async API operations

## Supported Languages

**50+ Languages** including:

**Interpreted:** Python, JavaScript, Ruby, PHP, Perl, Bash, Lua, R, Julia, Scheme, Tcl, Raku, Clojure, Groovy, Crystal, Dart, Elixir, Erlang, Haskell, OCaml, Common Lisp, Forth, Prolog, and more

**Compiled:** C, C++, Go, Rust, Java, Kotlin, C#, D, Nim, Zig, V, Pascal, Fortran, COBOL, Objective-C, and more

**Specialized:** TypeScript, F#, Odin

Use `detectLanguage()` for automatic detection or get full list with `await getLanguages()`.

## Error Handling

```javascript
import { executeCode, CredentialsError, TimeoutError } from './src/un_async.js';

try {
  const result = await executeCode('python', "print('hello')");
} catch (e) {
  if (e instanceof CredentialsError) {
    console.log(`Credentials error: ${e.message}`);
  } else if (e instanceof TimeoutError) {
    console.log(`Timeout error: ${e.message}`);
  } else {
    console.log(`Unexpected error: ${e.message}`);
  }
}
```

## Performance Tips

1. **Use Concurrent Execution** for multiple independent jobs:
   ```javascript
   const results = await Promise.all([
     executeCode('python', '...'),
     executeCode('go', '...'),
     executeCode('rust', '...'),
   ]);
   ```

2. **Use Exponential Backoff** with `waitForJob()` instead of polling manually

3. **Cache Languages** - `getLanguages()` caches results for 1 hour

## Differences from Sync SDK

This async SDK uses ES Modules with native fetch, while the sync SDK uses CommonJS with https module:

**Sync SDK:**
```javascript
const { executeCode } = require('./un.js');
executeCode('python', "print('hello')").then(console.log);
```

**Async SDK:**
```javascript
import { executeCode } from './un_async.js';
const result = await executeCode('python', "print('hello')");
```

Key differences:
- ES Modules (`import`/`export`) instead of CommonJS (`require`)
- Uses native `fetch()` (Node.js 18+) instead of `https` module
- Same credential system and HMAC signing
- Same API functions with same signatures

## Requirements

- Node.js 18.0.0 or later (for native fetch support)

## License

Public Domain - NO LICENSE, NO WARRANTY

## Support

Visit [unsandbox.com](https://unsandbox.com) for API documentation and support.
