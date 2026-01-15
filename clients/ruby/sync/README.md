# Un - Ruby SDK for unsandbox.com

Synchronous Ruby client for the unsandbox.com secure code execution service.

## Installation

Add to your Gemfile:

```ruby
gem 'un', git: 'https://github.com/unsandbox/un-ruby'
```

Or copy `src/un.rb` directly into your project.

## Quick Start

```ruby
require 'un'

# Execute code synchronously
result = Un.execute_code('python', 'print("Hello, World!")')
puts result['stdout']  # => "Hello, World!\n"
```

## Authentication

The SDK uses a 4-tier credential resolution system:

1. **Method arguments** - Pass `public_key:` and `secret_key:` directly
2. **Environment variables** - `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY`
3. **User config file** - `~/.unsandbox/accounts.csv`
4. **Local config file** - `./accounts.csv`

### CSV Format

```csv
unsb-pk-xxxxx-xxxxx-xxxxx-xxxxx,unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx
```

Use `UNSANDBOX_ACCOUNT=N` to select a specific account (0-indexed).

### Request Signing

All requests are signed using HMAC-SHA256:

```
X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
X-Timestamp: <unix_seconds>
Authorization: Bearer <public_key>
```

## API Reference

### Execute Code

```ruby
# Synchronous execution (blocks until complete)
result = Un.execute_code('python', 'print(42)')
# => {"status"=>"completed", "stdout"=>"42\n", "stderr"=>"", "exit_code"=>0}

# Asynchronous execution (returns immediately)
job_id = Un.execute_async('python', 'import time; time.sleep(10); print("done")')
# => "job-abc123"

# Wait for async job
result = Un.wait_for_job(job_id, timeout: 60)
# => {"status"=>"completed", "stdout"=>"done\n", ...}
```

### Job Management

```ruby
# Get job status
job = Un.get_job(job_id)
# => {"job_id"=>"...", "status"=>"running", ...}

# List all jobs
jobs = Un.list_jobs
# => [{"job_id"=>"...", "status"=>"completed"}, ...]

# Cancel a running job
Un.cancel_job(job_id)
```

### Languages

```ruby
# Get supported languages (cached for 1 hour)
languages = Un.get_languages
# => ["python", "javascript", "go", "rust", ...]

# Detect language from filename
Un.detect_language('script.py')    # => "python"
Un.detect_language('app.js')       # => "javascript"
Un.detect_language('main.go')      # => "go"
```

### Snapshots

```ruby
# Create session snapshot
snapshot_id = Un.session_snapshot(session_id, name: 'my-backup')

# Create ephemeral session snapshot
snapshot_id = Un.session_snapshot(session_id, ephemeral: true)

# Create service snapshot
snapshot_id = Un.service_snapshot(service_id, name: 'prod-backup')

# List snapshots
snapshots = Un.list_snapshots

# Restore snapshot
result = Un.restore_snapshot(snapshot_id)

# Delete snapshot
Un.delete_snapshot(snapshot_id)
```

## Error Handling

```ruby
begin
  result = Un.execute_code('python', 'print("hello")')
rescue Un::CredentialsError => e
  # No credentials found
  puts e.message
rescue Un::APIError => e
  # API request failed
  puts "HTTP #{e.status_code}: #{e.message}"
  puts e.response_body
end
```

### Error Types

- `Un::CredentialsError` - No valid credentials found
- `Un::APIError` - API request failed (includes `status_code` and `response_body`)

### HTTP Status Codes

- `401` - Invalid or missing API key
- `429` - Rate limit or concurrency limit exceeded
- `500` - Server error

## Supported Languages

50+ runtimes including:

- **Interpreted**: Python, JavaScript, TypeScript, Ruby, PHP, Perl, Lua, R, Bash
- **Compiled**: C, C++, Go, Rust, Java, Kotlin, C#, F#
- **Functional**: Haskell, OCaml, Elixir, Erlang, Clojure, Scheme
- **Other**: Dart, Crystal, Nim, Zig, V, Julia, Fortran, COBOL

## Examples

See the `examples/` directory:

- `hello_world.rb` - Basic code execution
- `async_job.rb` - Async execution with polling
- `language_detection.rb` - Detect language from filename
- `snapshots.rb` - Snapshot operations

## Testing

```bash
bundle install
bundle exec rake test
```

## License

Public Domain - No License, No Warranty
