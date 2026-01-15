# SDK Examples Validation System

## Overview

The examples validation system is the **heart of self-validating documentation**. It automatically:

1. **Finds** all SDK example files in `clients/*/examples/` directories
2. **Detects** programming language from file extension
3. **Executes** each example through the unsandbox API
4. **Validates** output against expected results
5. **Generates** comprehensive JSON and HTML reports
6. **Proves** that documentation examples actually work

This ensures that example code in documentation is never stale, incomplete, or broken.

## Core Script

**Location**: `scripts/validate-examples.sh`

**Purpose**: Find and execute all SDK examples, validate they work, generate reports

### Features

- **Recursive discovery**: Finds all example files in `clients/{language}/{sync,async}/examples/`
- **Multi-language support**: Python, JavaScript, Go, Rust, Java, Ruby, PHP, TypeScript, C, C++, Bash, Perl
- **Parallel execution**: Runs up to 4 examples concurrently (configurable)
- **API authentication**: Uses `UNSANDBOX_API_KEY` environment variable
- **Timeout protection**: 30-second timeout per example execution
- **Comprehensive reporting**:
  - JSON report: Machine-readable results with timestamps and statistics
  - HTML report: Beautiful visual dashboard with language breakdown
  - XML (JUnit): For CI/CD pipeline integration

### Usage

#### Local Testing

```bash
# Run validation (requires UNSANDBOX_API_KEY)
bash scripts/validate-examples.sh

# Run with verbose output
VERBOSE=1 bash scripts/validate-examples.sh

# Customize parallel jobs
PARALLEL_JOBS=8 bash scripts/validate-examples.sh

# Custom API endpoint
UNSANDBOX_API_URL="https://api.staging.unsandbox.com" bash scripts/validate-examples.sh
```

#### In CI/CD Pipeline

```yaml
science-validate-examples:
  stage: science
  script:
    - apk add --no-cache curl jq bc
    - bash scripts/validate-examples.sh
  artifacts:
    paths:
      - science-results/
```

## Directory Structure

### Example Files

Examples live in language-specific directories:

```
clients/
├── python/
│   ├── sync/examples/
│   │   ├── hello_world.py
│   │   ├── fibonacci.py
│   │   └── ...
│   └── async/examples/
│       └── ...
├── javascript/
│   ├── sync/examples/
│   │   ├── hello_world.js
│   │   └── ...
│   └── async/examples/
│       └── ...
├── go/
├── rust/
├── java/
├── ruby/
├── php/
└── ... (more languages)
```

### Output

Reports are generated in `science-results/`:

```
science-results/
├── examples-validation-results.json    # Machine-readable stats
├── examples-validation-results.html    # Visual dashboard
└── ... (XML reports for CI)
```

## Creating Examples

### Format

Example files should:

1. **Be executable**: Valid, syntactically correct code
2. **Be concise**: Demonstrate one concept clearly
3. **Include documentation**: Comment explaining what it does
4. **Indicate expected output**: Comment with "Expected output:" (optional)

### Python Example

```python
#!/usr/bin/env python3
"""
Fibonacci example for unsandbox Python SDK
Expected output: fib(10) = 55
"""

def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(f"fib(10) = {fib(10)}")
```

Save as: `clients/python/sync/examples/fibonacci.py`

### JavaScript Example

```javascript
// Hello World example for unsandbox JavaScript SDK
// Expected output: Hello from unsandbox!

console.log("Hello from unsandbox!");
```

Save as: `clients/javascript/sync/examples/hello_world.js`

### Go Example

```go
package main

import "fmt"

// Hello World example for unsandbox Go SDK
// Expected output: Hello from unsandbox!

func main() {
    fmt.Println("Hello from unsandbox!")
}
```

Save as: `clients/go/sync/examples/hello_world.go`

## Report Formats

### JSON Report

**File**: `examples-validation-results.json`

```json
{
    "report_type": "examples_validation",
    "timestamp": "2026-01-15T20:44:42Z",
    "timestamp_readable": "2026-01-15 20:44:42 UTC",
    "summary": {
        "total_examples": 5,
        "total_validated": 5,
        "total_failed": 0,
        "success_rate": "100%"
    },
    "language_stats": [
        {
            "language": "python",
            "validated": 2,
            "total_time_ms": 850,
            "avg_time_ms": 425
        },
        {
            "language": "javascript",
            "validated": 1,
            "total_time_ms": 320,
            "avg_time_ms": 320
        }
    ],
    "notes": "Examples validated through unsandbox API. Each example executed with 30s timeout."
}
```

**Use Cases**:
- CI/CD integration and metrics
- Tracking validation history
- Performance monitoring
- Automated dashboards

### HTML Report

**File**: `examples-validation-results.html`

Features:
- **Status badge**: Shows overall validation status (passing, failing, or no examples)
- **Stats grid**: Total examples, validated, failed, success rate
- **Language table**: Breakdown by language with execution times
- **Last verified timestamp**: When validation ran
- **Responsive design**: Works on desktop and mobile
- **Professional styling**: Gradient header, color-coded stats

**Open in browser**: `open science-results/examples-validation-results.html`

### JUnit XML Report

**File**: `science-results.xml`

For CI/CD pipeline integration with test result aggregation.

## Environment Variables

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `UNSANDBOX_API_KEY` | Authentication token for API | (none) | Yes (for execution) |
| `UNSANDBOX_API_URL` | API endpoint URL | `https://api.unsandbox.com` | No |
| `PARALLEL_JOBS` | Number of parallel executions | `4` | No |
| `TIMEOUT_SECONDS` | Timeout per example (seconds) | `30` | No |
| `VERBOSE` | Enable debug output (0/1) | `0` | No |

## Language Support

Supported languages and file extensions:

| Language | Extensions |
|----------|-----------|
| Python | `.py`, `.python` |
| JavaScript | `.js`, `.javascript` |
| Go | `.go`, `.golang` |
| Rust | `.rs`, `.rust` |
| Java | `.java` |
| Ruby | `.rb`, `.ruby` |
| PHP | `.php` |
| TypeScript | `.ts`, `.typescript` |
| C++ | `.cpp`, `.cc`, `.c++` |
| C | `.c` |
| Bash | `.sh`, `.bash` |
| Perl | `.pl`, `.perl` |

## Pipeline Integration

The validation script runs as part of the science jobs stage:

```
Commit to main
    ↓
detect-changes (determine what changed)
    ↓
build (compile SDKs)
    ↓
test (run SDK tests) [parallel]
    ↓
science-validate-examples [parallel with other science jobs]
    ├─ Find all examples
    ├─ Execute each via API
    ├─ Generate JSON/HTML reports
    └─ Output artifacts
    ↓
validate-examples (verify science job succeeded)
    ↓
report (aggregate all results)
```

The script is part of **pool burning** science jobs, so it runs on idle capacity at no additional cost.

## Troubleshooting

### "UNSANDBOX_API_KEY not set"

**Problem**: Script runs but skips execution

**Solution**: Set the environment variable
```bash
export UNSANDBOX_API_KEY="unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx"
bash scripts/validate-examples.sh
```

Or in CI/CD, add to GitLab project settings:
- **Settings → CI/CD → Variables**
- Add `UNSANDBOX_API_KEY` with your token
- Mark as "Protected" for production safety

### "No examples found"

**Problem**: Script finds 0 examples

**Solution**: Create example files in the correct directory structure
```bash
mkdir -p clients/python/sync/examples
echo 'print("hello")' > clients/python/sync/examples/hello.py
```

### Examples fail execution

**Problem**: Some examples return exit code != 0

**Solution**: Check the example code:
```bash
# View the specific failure in verbose mode
VERBOSE=1 bash scripts/validate-examples.sh 2>&1 | grep -A5 "FAIL"
```

### Performance is slow

**Problem**: Validation takes >30 seconds per example

**Solution**: Increase timeout or optimize examples
```bash
TIMEOUT_SECONDS=60 bash scripts/validate-examples.sh
```

## Best Practices

### For Example Writers

1. **Keep examples focused**: One concept per example
2. **Show clear input/output**: Examples should be immediately understandable
3. **Add helpful comments**: Explain what the code does
4. **Use realistic data**: Examples should demonstrate realistic use cases
5. **Test locally first**: Run in unsandbox before committing

### For CI/CD Operators

1. **Always set UNSANDBOX_API_KEY**: Reports are incomplete without execution
2. **Monitor success rates**: Track regression in documentation
3. **Review failed examples**: Fix or update broken examples promptly
4. **Archive reports**: Keep historical validation data for trends

### For Documentation Maintainers

1. **Review examples regularly**: Keep examples up-to-date with SDK changes
2. **Add examples for new features**: Update `clients/*/examples/` when adding features
3. **Test before publishing**: Use local validation before releasing docs
4. **Link examples in docs**: Reference `clients/*/examples/` in documentation files

## Metrics & Monitoring

The script tracks:

- **Total examples found**: Indicates documentation coverage
- **Validation success rate**: Shows documentation quality
- **Execution time by language**: Identifies performance regressions
- **Language coverage**: Which languages have examples

Track these metrics over time to:
- Identify documentation gaps
- Monitor documentation quality
- Detect performance regressions
- Prioritize missing examples

## Integration Examples

### GitHub Actions

```yaml
- name: Validate Examples
  run: |
    export UNSANDBOX_API_KEY=${{ secrets.UNSANDBOX_API_KEY }}
    bash scripts/validate-examples.sh
```

### GitLab CI (already integrated)

See `.gitlab-ci.yml` for current configuration.

### Local Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit
if git diff --cached --name-only | grep -q 'clients/.*examples/'; then
    bash scripts/validate-examples.sh || exit 1
fi
```

## Future Enhancements

- [ ] Auto-generate example documentation from code comments
- [ ] Performance regression detection (track execution time trends)
- [ ] Example linting (check code style, completeness)
- [ ] Screenshot/GIF capture for visual examples
- [ ] Automatic example discovery from docstrings
- [ ] Example versioning (track examples per SDK version)
- [ ] Cross-language example comparison (show same algorithm in multiple languages)

## Related Files

- **Pipeline**: `.gitlab-ci.yml` - CI/CD configuration
- **Detect Changes**: `scripts/detect-changes.sh` - Identify what changed
- **Science Jobs**: `scripts/science/` - Pool burning tasks
- **Tests**: `tests/` - SDK unit tests
- **Examples**: `clients/*/examples/` - All example files

## References

- [PIPELINE.md](PIPELINE.md) - Pipeline architecture and strategy
- [UN-Inception README](../README.md) - Project overview
- [Unsandbox API Documentation](../unsandbox.txt) - API endpoints
