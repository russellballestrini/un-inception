# Unsandbox PHP SDK (Synchronous)

A synchronous PHP client library for [unsandbox.com](https://unsandbox.com) - secure, multi-language code execution.

## Installation

Using Composer:

```bash
composer require unsandbox/un
```

Or include directly:

```php
require_once 'path/to/src/un.php';
use Unsandbox\Unsandbox;
```

## Quick Start

```php
<?php
require_once 'vendor/autoload.php';

use Unsandbox\Unsandbox;

$client = new Unsandbox();

// Execute Python code
$result = $client->executeCode('python', 'print("Hello from unsandbox!")');
print_r($result);
```

## Authentication

The SDK supports 4-tier credential resolution:

1. **Method arguments** - Pass directly to methods
2. **Constructor arguments** - Set default credentials
3. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
4. **Config files** - `~/.unsandbox/accounts.csv` or `./accounts.csv`

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

Or pass to constructor:

```php
$client = new Unsandbox('pk_xxxxx', 'sk_xxxxx');
```

## API Reference

### Synchronous Execution

Execute code and wait for completion:

```php
$result = $client->executeCode(
    'python',           // language
    'print("hello")',   // code
    null,               // publicKey (optional)
    null                // secretKey (optional)
);

// Result:
// [
//     'status' => 'completed',
//     'stdout' => "hello\n",
//     'stderr' => '',
//     'exit_code' => 0,
//     'runtime_ms' => 342
// ]
```

### Asynchronous Execution

Start execution and get a job ID:

```php
// Start execution
$jobId = $client->executeAsync('python', 'print("hello")');

// Check status later
$result = $client->waitForJob($jobId);
```

### Job Management

```php
// Get single job status
$job = $client->getJob('job_123');

// List all active jobs
$jobs = $client->listJobs();

// Cancel a job
$client->cancelJob('job_123');
```

### Languages

```php
// Get list of supported languages (cached for 1 hour)
$languages = $client->getLanguages();
// Returns: ['python', 'javascript', 'go', 'rust', ...]

// Detect language from filename
$lang = Unsandbox::detectLanguage('script.py');  // Returns 'python'
```

### Snapshots

```php
// Create a session snapshot
$snapshotId = $client->sessionSnapshot('session_123', null, null, 'checkpoint');

// Create a service snapshot
$snapshotId = $client->serviceSnapshot('service_123', null, null, 'backup');

// List snapshots
$snapshots = $client->listSnapshots();

// Restore a snapshot
$result = $client->restoreSnapshot($snapshotId);

// Delete a snapshot
$client->deleteSnapshot($snapshotId);
```

## Language Support

The SDK supports 50+ programming languages including:

- **Interpreted**: Python, JavaScript, Ruby, PHP, Perl, Bash, Lua, etc.
- **Compiled**: C, C++, Go, Rust, Java, Kotlin, etc.
- **Functional**: Haskell, OCaml, F#, Scheme, Clojure, etc.
- **Other**: WASM, Prolog, Forth, etc.

See `getLanguages()` for the complete list.

## Caching

The languages list is cached locally for 1 hour in `~/.unsandbox/languages.json`. This reduces API calls and improves performance.

To force a refresh, delete the cache file:

```bash
rm ~/.unsandbox/languages.json
```

## Error Handling

```php
use Unsandbox\Unsandbox;
use Unsandbox\CredentialsException;
use Unsandbox\ApiException;

try {
    $client = new Unsandbox();
    $result = $client->executeCode('python', 'print("hello")');
} catch (CredentialsException $e) {
    echo "No credentials found: " . $e->getMessage();
} catch (ApiException $e) {
    echo "API error: " . $e->getMessage();
    echo "HTTP code: " . $e->getCode();
    $response = $e->getResponse();  // Full response array
}
```

## Examples

See the `examples/` directory for complete working examples:

- `hello_world.php` - Simple print example
- `fibonacci.php` - Recursive function example
- `hello_world_client.php` - Execute Python via SDK
- `fibonacci_client.php` - Execute JavaScript via SDK

Run an example:

```bash
php examples/hello_world_client.php
```

## Testing

Install dev dependencies and run tests:

```bash
composer install
composer test
```

Or run PHPUnit directly:

```bash
./vendor/bin/phpunit tests/
```

## Requirements

- PHP 7.4+
- ext-curl
- ext-json

## Request Authentication

All API requests are authenticated using HMAC-SHA256:

```
Authorization: Bearer <public_key>
X-Timestamp: <unix_seconds>
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
```

The signature is computed over the message format: `timestamp:METHOD:path:body`

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
