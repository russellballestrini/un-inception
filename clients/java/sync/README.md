# Unsandbox Java SDK (Synchronous)

A synchronous Java client library for [unsandbox.com](https://unsandbox.com) - secure, multi-language code execution.

## Installation

### Maven

```xml
<dependency>
    <groupId>com.unsandbox</groupId>
    <artifactId>un-sdk-sync</artifactId>
    <version>1.0.0</version>
</dependency>
```

### From Source

```bash
cd clients/java/sync
mvn install
```

## Quick Start

```java
import Un;
import java.util.Map;

// Execute Python code
Map<String, Object> result = Un.executeCode("python", "print('Hello from unsandbox!')", null, null);
System.out.println(result.get("stdout"));
```

## Authentication

The SDK supports 4-tier credential resolution:

1. **Method arguments** - Pass directly to methods
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

```java
import Un;
import java.util.Map;

Map<String, Object> result = Un.executeCode(
    "python",                  // language
    "print('hello')",          // code
    null,                      // publicKey (uses credential resolution)
    null                       // secretKey (uses credential resolution)
);

System.out.println(result.get("status"));    // "completed"
System.out.println(result.get("stdout"));    // "hello\n"
System.out.println(result.get("stderr"));    // ""
System.out.println(result.get("exit_code")); // 0
```

### Asynchronous Execution

Start execution and get a job ID:

```java
import Un;
import java.util.Map;

// Start execution
String jobId = Un.executeAsync("python", "print('hello')", null, null);

// Wait for completion with 60 second timeout
Map<String, Object> result = Un.waitForJob(jobId, null, null, 60000);
```

### Job Management

```java
import Un;
import java.util.Map;
import java.util.List;

// Get single job status
Map<String, Object> job = Un.getJob("job_123", null, null);

// List all jobs
List<Map<String, Object>> jobs = Un.listJobs(null, null);

// Cancel a job
Un.cancelJob("job_123", null, null);
```

### Languages

```java
import Un;
import java.util.List;

// Get list of supported languages
List<String> languages = Un.getLanguages(null, null);
// Returns: ["python", "javascript", "go", "rust", ...]

// Detect language from filename
String lang = Un.detectLanguage("script.py");  // Returns "python"
```

### Snapshots

```java
import Un;
import java.util.Map;
import java.util.List;

// Create a session snapshot
String snapshotId = Un.sessionSnapshot("session_123", null, null, "checkpoint", false);

// Create a service snapshot
String snapshotId = Un.serviceSnapshot("service_123", null, null, "backup");

// List snapshots
List<Map<String, Object>> snapshots = Un.listSnapshots(null, null);

// Restore a snapshot
Map<String, Object> result = Un.restoreSnapshot(snapshotId, null, null);

// Delete a snapshot
Un.deleteSnapshot(snapshotId, null, null);
```

## Language Support

The SDK supports 50+ programming languages including:

- **Interpreted**: Python, JavaScript, Ruby, PHP, Perl, Bash, etc.
- **Compiled**: C, C++, Go, Rust, Java, etc.
- **Functional**: Haskell, OCaml, F#, Scheme, etc.
- **Other**: WASM, Prolog, Forth, etc.

See `getLanguages()` for the complete list.

## Caching

The languages list is cached locally for 1 hour in `~/.unsandbox/languages.json`. This reduces API calls and improves startup performance.

To force a refresh, delete the cache file:

```bash
rm ~/.unsandbox/languages.json
```

## Error Handling

```java
import Un;
import java.util.Map;
import java.io.IOException;

try {
    Map<String, Object> result = Un.executeCode("python", "print('hello')", null, null);
} catch (Un.CredentialsException e) {
    System.err.println("No credentials found: " + e.getMessage());
} catch (Un.ApiException e) {
    System.err.println("API error: " + e.getMessage());
    System.err.println("Status code: " + e.getStatusCode());
    System.err.println("Response: " + e.getResponseBody());
} catch (IOException e) {
    System.err.println("Network error: " + e.getMessage());
}
```

## Examples

See the `examples/` directory for complete working examples:

- `HelloWorld.java` - Simple print example
- `Fibonacci.java` - Recursive function example
- `HelloWorldClient.java` - SDK client usage example
- `FibonacciClient.java` - CPU-bound computation example
- `HttpRequestClient.java` - HTTP request in sandbox example
- `AsyncJobClient.java` - Async execution with polling example

Compile and run an example:

```bash
cd examples
javac -cp ../src HelloWorldClient.java
export UNSANDBOX_PUBLIC_KEY="your-key"
export UNSANDBOX_SECRET_KEY="your-key"
java -cp .:../src HelloWorldClient
```

## Building

Build with Maven:

```bash
mvn clean package
```

Run tests:

```bash
mvn test
```

Create JAR with sources and Javadoc:

```bash
mvn package
```

## Requirements

- Java 17 or higher
- No external dependencies (uses standard library only)

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
