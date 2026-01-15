# SDK Testing Guidelines

This document defines the testing requirements for all un-inception SDK clients.

## Core Principle

**SDKs are LIBRARIES for embedding in other people's code.**

Every SDK must be:
1. **Importable** - Can be used as a library in other projects
2. **Testable** - Exports functions that tests can call directly
3. **Functional** - Actually works against the live API

## Three Testing Levels

### 1. Unit Tests (`test-library`)

**Purpose**: Test exported library functions in isolation.

**Requirements**:
- Test ACTUAL exported functions from the SDK
- NO mocking
- NO re-implementing functions locally
- Tests run without network access
- Tests run without API credentials

**What to test**:
- HMAC-SHA256 signature generation
- Request building (headers, body formatting)
- Response parsing (JSON → native types)
- Language detection from file extensions
- Error handling and edge cases
- Memory management (for C/C++/Rust)

**Example (C)**:
```c
#include "un.h"

void test_hmac_signature() {
    // Test the REAL exported function
    char *sig = unsandbox_hmac_sign("secret", "1234567890:POST:/execute:{}");
    assert(sig != NULL);
    assert(strlen(sig) == 64);  // hex-encoded SHA256
    free(sig);
}

void test_language_detection() {
    assert(strcmp(unsandbox_detect_language("test.py"), "python") == 0);
    assert(strcmp(unsandbox_detect_language("main.go"), "go") == 0);
    assert(unsandbox_detect_language("unknown.xyz") == NULL);
}
```

**Example (Python)**:
```python
from un import UnsandboxClient, hmac_sign, detect_language

def test_hmac_signature():
    sig = hmac_sign("secret", "1234567890:POST:/execute:{}")
    assert len(sig) == 64
    assert sig == "expected_hex_value"

def test_language_detection():
    assert detect_language("test.py") == "python"
    assert detect_language("main.go") == "go"
    assert detect_language("unknown.xyz") is None
```

### 2. Integration Tests (`test-integration`)

**Purpose**: Test that SDK components work together correctly.

**Requirements**:
- Test internal SDK consistency
- May use test doubles for HTTP layer
- Should NOT call live API
- Tests the full request/response cycle internally

**What to test**:
- Auth headers are generated correctly together
- Request body is properly formatted
- Response parsing handles all expected formats
- Error responses are properly converted to exceptions/errors
- Async operations work correctly (if applicable)

**Example (Python)**:
```python
def test_auth_headers_integration():
    client = UnsandboxClient(public_key="pk", secret_key="sk")
    headers = client._build_auth_headers("POST", "/execute", '{"code":"x"}')

    assert "Authorization" in headers
    assert "X-Timestamp" in headers
    assert "X-Signature" in headers
    assert headers["Authorization"] == "Bearer pk"

def test_request_building():
    client = UnsandboxClient(public_key="pk", secret_key="sk")
    req = client._build_execute_request("python", "print(1)")

    assert req["method"] == "POST"
    assert req["path"] == "/execute"
    assert "language" in req["body"]
    assert req["body"]["language"] == "python"
```

### 3. Functional Tests (`test-functional`)

**Purpose**: Test the SDK against the live API.

**Requirements**:
- Requires `UNSANDBOX_PUBLIC_KEY` and `UNSANDBOX_SECRET_KEY` environment variables
- Makes REAL API calls to api.unsandbox.com
- Tests the complete lifecycle: execute, sessions, services
- Should be skipped gracefully if credentials not available

**What to test**:
- Execute code in multiple languages
- Create/list/destroy sessions
- Create/list/destroy services
- Error handling for invalid requests
- Rate limiting behavior

**Example (Python)**:
```python
import os
import pytest

@pytest.fixture
def client():
    pk = os.environ.get("UNSANDBOX_PUBLIC_KEY")
    sk = os.environ.get("UNSANDBOX_SECRET_KEY")
    if not pk or not sk:
        pytest.skip("API credentials not set")
    return UnsandboxClient(public_key=pk, secret_key=sk)

def test_execute_python(client):
    result = client.execute("python", "print(42)")
    assert result.exit_code == 0
    assert "42" in result.stdout

def test_execute_invalid_language(client):
    with pytest.raises(UnsandboxError) as exc:
        client.execute("not_a_real_language", "code")
    assert "unsupported" in str(exc.value).lower()

def test_session_lifecycle(client):
    # Create
    session = client.session_create()
    assert session.id is not None

    # Execute in session
    result = client.session_execute(session.id, "echo hello")
    assert "hello" in result.stdout

    # Destroy
    client.session_destroy(session.id)
```

## Test Directory Structure

```
clients/{language}/
├── src/
│   ├── un.{ext}           # Main implementation
│   └── un.h               # Header (for C/C++)
├── tests/
│   ├── unit/
│   │   ├── test_hmac.{ext}
│   │   ├── test_language_detection.{ext}
│   │   └── test_request_building.{ext}
│   ├── integration/
│   │   ├── test_auth_flow.{ext}
│   │   └── test_response_parsing.{ext}
│   └── functional/
│       ├── test_execute.{ext}
│       ├── test_sessions.{ext}
│       └── test_services.{ext}
├── Makefile
└── README.md
```

## Makefile Requirements

Every client Makefile MUST implement these targets:

```makefile
.PHONY: test test-cli test-library test-integration test-functional clean

# Run all tests
test: test-cli test-library test-integration test-functional
	@echo "All tests complete"

# Test CLI mode - binary runs, --help works
test-cli:
	@echo "Testing CLI mode..."
	./un --help >/dev/null
	./un --version >/dev/null

# Test library mode - unit tests of exported functions
test-library:
	@echo "Testing library exports..."
	./run_unit_tests

# Test integration - SDK internal consistency
test-integration:
	@echo "Testing SDK integration..."
	./run_integration_tests

# Test functional - real API calls
test-functional:
	@echo "Testing against live API..."
	@if [ -z "$$UNSANDBOX_PUBLIC_KEY" ]; then \
		echo "  Skipped (no credentials)"; \
	else \
		./run_functional_tests; \
	fi
```

## Anti-Patterns (FORBIDDEN)

### 1. Re-implementing Functions Locally

```c
// ❌ WRONG - This tests a LOCAL copy, not the SDK
static void local_sha256_transform(...) { ... }
void test_sha256() {
    // Testing LOCAL function, not the one in un.c!
    local_sha256_transform(...);
}
```

### 2. Mocking Everything

```python
# ❌ WRONG - This doesn't test the real SDK behavior
@mock.patch('un.requests.post')
def test_execute(mock_post):
    mock_post.return_value = Mock(json=lambda: {"stdout": "42"})
    # This tests the mock, not the SDK!
```

### 3. Tests Without Assertions

```python
# ❌ WRONG - This doesn't actually verify anything
def test_execute():
    client = UnsandboxClient()
    client.execute("python", "print(1)")
    # No assertion! Test always passes!
```

### 4. Skipping Test Levels

```makefile
# ❌ WRONG - Missing test levels
test: test-functional  # Only functional tests? No unit/integration!
```

## Language-Specific Guidelines

### C/C++

- Use `#ifndef UNSANDBOX_LIBRARY` to guard `main()` for library builds
- Export functions without `static` keyword when built as library
- Tests link against the compiled library, not source directly
- Use assertion macros or a test framework (Unity, CUnit)

### Python

- Use pytest for all test levels
- Export functions at module level (not just class methods)
- Use `__all__` to define public API

### JavaScript/TypeScript

- Use Jest or Mocha for testing
- Export functions via `module.exports` or ES6 `export`
- Test both CommonJS and ESM imports if supporting both

### Go

- Use standard `testing` package
- Export functions with capital letters
- Tests in `*_test.go` files

### Rust

- Use `#[cfg(test)]` modules
- Export public API with `pub` keyword
- Use `cargo test` for all test levels

## CI Integration

Tests are run automatically on push via GitLab CI:

```yaml
test-{language}:
  stage: test
  script:
    - cd clients/{language}
    - make test-cli
    - make test-library
    - make test-integration
  rules:
    - changes:
        - clients/{language}/**/*

test-{language}-functional:
  stage: functional
  script:
    - cd clients/{language}
    - make test-functional
  rules:
    - changes:
        - clients/{language}/**/*
  variables:
    UNSANDBOX_PUBLIC_KEY: $CI_UNSANDBOX_PUBLIC_KEY
    UNSANDBOX_SECRET_KEY: $CI_UNSANDBOX_SECRET_KEY
```

## Summary

1. **Unit tests** - Test exported functions, no mocking
2. **Integration tests** - Test SDK internals work together
3. **Functional tests** - Test against live API
4. **All three levels are REQUIRED** for each SDK
5. **NO re-implementing functions locally** - test the REAL code
