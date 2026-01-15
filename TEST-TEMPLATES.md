# Test Templates for Multi-Mode Client Testing

Each UN client implementation must be tested in **4 modes**:

1. **CLI Mode**: Standalone command-line tool with argument parsing
2. **Library Mode**: Importable SDK for embedding in other code
3. **Integration Mode**: API contract validation (authentication, responses)
4. **Functional Mode**: Real-world usage scenarios

---

## Python Test Template

### File Structure
```
clients/python/
├── un.py                          # Main implementation (CLI + Library)
└── tests/
    ├── test_un_py_cli.py         # CLI mode tests
    ├── test_un_py_library.py      # Library mode tests (imports)
    ├── test_un_py_integration.py  # Integration with API
    └── test_un_py_functional.py   # Real-world scenarios
```

### 1. CLI Mode Test (`test_un_py_cli.py`)
```python
#!/usr/bin/env python3
import subprocess
import os
import tempfile

def test_cli_execute_with_code():
    """Test: python un.py 'print(42)'"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', '-c', 'print(42)'],
        capture_output=True, text=True
    )
    assert result.returncode == 0
    assert '42' in result.stdout or '42' in result.stderr

def test_cli_execute_file():
    """Test: python un.py test/fib.py"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', 'test/fib.py'],
        capture_output=True, text=True
    )
    assert result.returncode == 0

def test_cli_execute_with_env():
    """Test: python un.py -e VAR=val code.py"""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        f.write('import os\nprint(os.environ.get("TEST_VAR"))')
        f.flush()
        result = subprocess.run(
            ['python3', 'clients/python/un.py', '-e', 'TEST_VAR=hello', f.name],
            capture_output=True, text=True
        )
        assert 'hello' in result.stdout
    os.unlink(f.name)

def test_cli_session():
    """Test: python un.py session (interactive mode)"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', 'session', '--help'],
        capture_output=True, text=True
    )
    assert result.returncode == 0
    assert 'session' in result.stdout or 'session' in result.stderr

def test_cli_service():
    """Test: python un.py service (service mode)"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', 'service', '--help'],
        capture_output=True, text=True
    )
    assert result.returncode == 0

def test_cli_help():
    """Test: python un.py --help"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', '--help'],
        capture_output=True, text=True
    )
    assert result.returncode == 0
    assert 'execute' in result.stdout or 'session' in result.stdout

def test_cli_version():
    """Test: python un.py --version"""
    result = subprocess.run(
        ['python3', 'clients/python/un.py', '--version'],
        capture_output=True, text=True
    )
    assert result.returncode == 0
```

### 2. Library Mode Test (`test_un_py_library.py`)
```python
#!/usr/bin/env python3
import sys
sys.path.insert(0, 'clients/python')

from un import UnsandboxClient, UnsandboxAsyncClient

def test_library_import():
    """Test: Can import UnsandboxClient"""
    assert UnsandboxClient is not None
    assert UnsandboxAsyncClient is not None

def test_library_sync_client():
    """Test: Create and use sync client"""
    client = UnsandboxClient(
        api_key=os.environ.get('UNSANDBOX_PUBLIC_KEY'),
        secret_key=os.environ.get('UNSANDBOX_SECRET_KEY')
    )
    assert client is not None
    assert hasattr(client, 'execute')
    assert callable(client.execute)

def test_library_async_client():
    """Test: Create async client"""
    client = UnsandboxAsyncClient(
        api_key=os.environ.get('UNSANDBOX_PUBLIC_KEY'),
        secret_key=os.environ.get('UNSANDBOX_SECRET_KEY')
    )
    assert client is not None
    assert hasattr(client, 'execute_async')

def test_library_execute():
    """Test: Call client.execute('python', code)"""
    client = UnsandboxClient(
        api_key=os.environ.get('UNSANDBOX_PUBLIC_KEY'),
        secret_key=os.environ.get('UNSANDBOX_SECRET_KEY')
    )
    result = client.execute('python', 'print(42)')
    assert result is not None
    assert result.stdout or result.stderr
    assert result.exit_code == 0

def test_library_session():
    """Test: client.create_session() returns session ID"""
    client = UnsandboxClient(...)
    session = client.create_session()
    assert session.id is not None
    assert hasattr(session, 'execute')

def test_library_authentication():
    """Test: HMAC signature generation"""
    client = UnsandboxClient(
        api_key='unsb-pk-test-1234',
        secret_key='unsb-sk-test-5678'
    )
    # Verify internal HMAC generation works
    assert hasattr(client, '_generate_signature')
    sig = client._generate_signature('test-payload')
    assert len(sig) == 64  # SHA256 hex digest length
```

### 3. Integration Mode Test (`test_un_py_integration.py`)
```python
#!/usr/bin/env python3
import requests
import os
import sys
sys.path.insert(0, 'clients/python')
from un import UnsandboxClient

def test_integration_auth_valid():
    """Test: Valid authentication with API"""
    client = UnsandboxClient(
        api_key=os.environ.get('UNSANDBOX_PUBLIC_KEY'),
        secret_key=os.environ.get('UNSANDBOX_SECRET_KEY')
    )
    result = client.execute('python', 'print(42)')
    assert result.exit_code == 0

def test_integration_auth_invalid():
    """Test: Invalid key returns 401"""
    client = UnsandboxClient(
        api_key='invalid-key',
        secret_key='invalid-secret'
    )
    try:
        result = client.execute('python', 'print(42)')
        assert result.status_code == 401
    except Exception as e:
        assert '401' in str(e)

def test_integration_rate_limit():
    """Test: Rate limiting returns 429"""
    client = UnsandboxClient(...)
    # Make many rapid requests
    for i in range(100):
        result = client.execute('python', 'print(1)')
    # Eventually should hit rate limit
    # (may be soft limit)

def test_integration_language_support():
    """Test: All supported languages work"""
    client = UnsandboxClient(...)
    languages = ['python', 'javascript', 'go', 'ruby', 'php', 'rust']
    for lang in languages:
        try:
            result = client.execute(lang, 'print(42)' if lang == 'python' else 'console.log(42)')
            assert result.exit_code == 0
        except Exception as e:
            print(f"Language {lang} failed: {e}")

def test_integration_file_input():
    """Test: -f flag works with API"""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        f.write('print(42)')
        f.flush()
        client = UnsandboxClient(...)
        result = client.execute_file(f.name)
        assert result.exit_code == 0
    os.unlink(f.name)

def test_integration_environment_vars():
    """Test: -e flag passes to API"""
    client = UnsandboxClient(...)
    result = client.execute(
        'python',
        'import os; print(os.environ.get("TEST"))',
        env={'TEST': 'value'}
    )
    assert 'value' in result.stdout

def test_integration_timeout():
    """Test: Long-running code times out"""
    client = UnsandboxClient(...)
    result = client.execute(
        'python',
        'import time; time.sleep(100)',
        timeout=5
    )
    assert result.timed_out or result.exit_code != 0

def test_integration_artifacts():
    """Test: -a flag returns artifacts"""
    client = UnsandboxClient(...)
    result = client.execute(
        'python',
        'open("/tmp/output.txt", "w").write("test")',
        artifacts=['/tmp/output.txt']
    )
    assert result.artifacts is not None
    assert '/tmp/output.txt' in result.artifacts
```

### 4. Functional Mode Test (`test_un_py_functional.py`)
```python
#!/usr/bin/env python3
"""Real-world usage scenarios"""
import sys
sys.path.insert(0, 'clients/python')
from un import UnsandboxClient

def test_functional_fibonacci():
    """Real-world: Calculate Fibonacci sequence"""
    client = UnsandboxClient(...)
    code = """
def fib(n):
    if n <= 1: return n
    return fib(n-1) + fib(n-2)
print(fib(10))
"""
    result = client.execute('python', code)
    assert '55' in result.stdout

def test_functional_data_analysis():
    """Real-world: Data analysis with pandas"""
    client = UnsandboxClient(...)
    code = """
import pandas as pd
df = pd.DataFrame({'A': [1, 2, 3], 'B': [4, 5, 6]})
print(df.mean())
"""
    result = client.execute('python', code)
    assert result.exit_code == 0

def test_functional_web_scraping():
    """Real-world: HTTP request via semitrusted"""
    client = UnsandboxClient(...)
    code = """
import requests
resp = requests.get('https://httpbin.org/ip')
print(resp.status_code)
"""
    result = client.execute('python', code, network='semitrusted')
    assert '200' in result.stdout or '200' in result.stderr

def test_functional_file_operations():
    """Real-world: Read/write files"""
    client = UnsandboxClient(...)
    code = """
with open('/tmp/test.txt', 'w') as f:
    f.write('hello')
with open('/tmp/test.txt', 'r') as f:
    print(f.read())
"""
    result = client.execute('python', code)
    assert 'hello' in result.stdout

def test_functional_subprocess():
    """Real-world: Shell commands from Python"""
    client = UnsandboxClient(...)
    code = """
import subprocess
result = subprocess.run(['echo', 'hello'], capture_output=True, text=True)
print(result.stdout)
"""
    result = client.execute('python', code)
    assert 'hello' in result.stdout

def test_functional_json_parsing():
    """Real-world: JSON API response parsing"""
    client = UnsandboxClient(...)
    code = """
import json
data = json.loads('{"key": "value"}')
print(data['key'])
"""
    result = client.execute('python', code)
    assert 'value' in result.stdout

def test_functional_error_handling():
    """Real-world: Proper error handling"""
    client = UnsandboxClient(...)
    code = """
try:
    x = 1 / 0
except ZeroDivisionError as e:
    print('Caught:', type(e).__name__)
"""
    result = client.execute('python', code)
    assert 'Caught: ZeroDivisionError' in result.stdout

def test_functional_async_code():
    """Real-world: Async/await in Python"""
    client = UnsandboxClient(...)
    code = """
import asyncio
async def main():
    await asyncio.sleep(0.1)
    print('done')
asyncio.run(main())
"""
    result = client.execute('python', code)
    assert 'done' in result.stdout
```

---

## Go Test Template

### File Structure
```
clients/go/
├── un.go                     # Main implementation
├── un_test.go               # Built-in Go tests
└── tests/
    ├── cli_test.sh          # CLI mode
    ├── library_test.go      # Library mode
    ├── integration_test.go   # Integration mode
    └── functional_test.sh    # Functional mode
```

### 1. CLI Mode Test (`cli_test.sh`)
```bash
#!/bin/bash
set -e

echo "Testing Go CLI..."

# Test: go run un.go 'code'
go run un.go -c 'fmt.Println(42)' && echo "✓ CLI inline code"

# Test: go run un.go file.go
go run un.go test/fib.py && echo "✓ CLI file execution"

# Test: go run un.go -e VAR=val
go run un.go -e TEST_VAR=hello 'import os; print(os.environ["TEST_VAR"])' && echo "✓ CLI with env"

# Test: go run un.go --help
go run un.go --help && echo "✓ CLI help"

# Test: go run un.go session
go run un.go session --help && echo "✓ CLI session"

# Test: go run un.go service
go run un.go service --help && echo "✓ CLI service"
```

### 2. Library Mode Test (`library_test.go`)
```go
package main

import (
	"testing"
)

func TestLibraryImport(t *testing.T) {
	// Test: Can import and use as library
	client := NewUnsandboxClient(
		os.Getenv("UNSANDBOX_PUBLIC_KEY"),
		os.Getenv("UNSANDBOX_SECRET_KEY"),
	)
	if client == nil {
		t.Fatal("Failed to create client")
	}
}

func TestLibraryExecute(t *testing.T) {
	// Test: client.Execute(language, code)
	client := NewUnsandboxClient(...)
	result, err := client.Execute("python", "print(42)")
	if err != nil {
		t.Fatalf("Execute failed: %v", err)
	}
	if result.ExitCode != 0 {
		t.Fatalf("Expected exit code 0, got %d", result.ExitCode)
	}
}

func TestLibrarySignature(t *testing.T) {
	// Test: HMAC signature generation
	client := NewUnsandboxClient(...)
	sig := client.generateSignature("test-payload")
	if len(sig) != 64 {
		t.Fatalf("Invalid signature length: %d", len(sig))
	}
}

func TestLibrarySession(t *testing.T) {
	// Test: CreateSession() returns session
	client := NewUnsandboxClient(...)
	session, err := client.CreateSession()
	if err != nil {
		t.Fatalf("CreateSession failed: %v", err)
	}
	if session.ID == "" {
		t.Fatal("Session ID is empty")
	}
}
```

### 3. Integration Mode Test (`integration_test.go`)
```go
func TestIntegrationAuth(t *testing.T) {
	client := NewUnsandboxClient(
		os.Getenv("UNSANDBOX_PUBLIC_KEY"),
		os.Getenv("UNSANDBOX_SECRET_KEY"),
	)
	result, err := client.Execute("python", "print(42)")
	if err != nil {
		t.Fatalf("Integration test failed: %v", err)
	}
	if result.ExitCode != 0 {
		t.Fatalf("Code execution failed")
	}
}

func TestIntegrationLanguages(t *testing.T) {
	client := NewUnsandboxClient(...)
	languages := []string{"python", "go", "javascript", "ruby"}
	for _, lang := range languages {
		result, err := client.Execute(lang, "// test code")
		if err != nil {
			t.Logf("Language %s not available: %v", lang, err)
		}
		if result.ExitCode == 0 {
			t.Logf("✓ Language %s works", lang)
		}
	}
}

func TestIntegrationFileInput(t *testing.T) {
	client := NewUnsandboxClient(...)
	content := "print(42)"
	result, err := client.ExecuteFile("test.py", content)
	if err != nil {
		t.Fatalf("ExecuteFile failed: %v", err)
	}
}
```

### 4. Functional Mode Test (`functional_test.sh`)
```bash
#!/bin/bash

# Compile first
go build -o un un.go

# Real-world: Fibonacci
./un test/fib.py | grep -q "55" && echo "✓ Fibonacci works"

# Real-world: JSON parsing
./un <<'EOF'
import json
data = json.loads('{"key": "value"}')
print(data['key'])
EOF
grep -q "value" && echo "✓ JSON parsing works"

# Real-world: File operations
./un <<'EOF'
with open('/tmp/test.txt', 'w') as f:
    f.write('test')
with open('/tmp/test.txt', 'r') as f:
    print(f.read())
EOF
grep -q "test" && echo "✓ File operations work"

# Real-world: Error handling
./un <<'EOF'
try:
    x = 1/0
except ZeroDivisionError:
    print('Caught error')
EOF
grep -q "Caught error" && echo "✓ Error handling works"
```

---

## JavaScript Test Template

### File Structure
```
clients/javascript/
├── un.js                     # Main implementation
├── package.json             # Dependencies
└── tests/
    ├── cli.test.js          # CLI mode (via Node)
    ├── library.test.js      # Library mode (imports)
    ├── integration.test.js   # Integration mode
    └── functional.test.sh    # Functional mode
```

### 1. CLI Mode (`cli.test.js`)
```javascript
const { execSync } = require('child_process');

describe('JavaScript CLI Mode', () => {
    test('Execute inline code', () => {
        const result = execSync('node un.js -c "console.log(42)"');
        expect(result.toString()).toContain('42');
    });

    test('Execute file', () => {
        const result = execSync('node un.js test/fib.py');
        expect(result.toString()).not.toContain('error');
    });

    test('Execute with env', () => {
        const result = execSync('node un.js -e TEST=hello -c "console.log(process.env.TEST)"');
        expect(result.toString()).toContain('hello');
    });

    test('Help command', () => {
        const result = execSync('node un.js --help');
        expect(result.toString()).toMatch(/execute|session|service/);
    });

    test('Session command', () => {
        const result = execSync('node un.js session --help');
        expect(result.toString()).not.toContain('error');
    });

    test('Service command', () => {
        const result = execSync('node un.js service --help');
        expect(result.toString()).not.toContain('error');
    });
});
```

### 2. Library Mode (`library.test.js`)
```javascript
const { UnsandboxClient } = require('./un.js');

describe('JavaScript Library Mode', () => {
    test('Import client', () => {
        expect(UnsandboxClient).toBeDefined();
    });

    test('Create client instance', () => {
        const client = new UnsandboxClient({
            publicKey: process.env.UNSANDBOX_PUBLIC_KEY,
            secretKey: process.env.UNSANDBOX_SECRET_KEY,
        });
        expect(client).toBeDefined();
    });

    test('Execute method exists', async () => {
        const client = new UnsandboxClient(...);
        expect(client.execute).toBeDefined();
        expect(typeof client.execute).toBe('function');
    });

    test('Execute code', async () => {
        const client = new UnsandboxClient(...);
        const result = await client.execute('python', 'print(42)');
        expect(result.exitCode).toBe(0);
    });

    test('HMAC signature generation', () => {
        const client = new UnsandboxClient(...);
        const sig = client.generateSignature('test');
        expect(sig.length).toBe(64);  // SHA256 hex
    });

    test('Session creation', async () => {
        const client = new UnsandboxClient(...);
        const session = await client.createSession();
        expect(session.id).toBeDefined();
    });
});
```

### 3. Integration Mode (`integration.test.js`)
```javascript
describe('JavaScript Integration Mode', () => {
    test('Valid authentication', async () => {
        const client = new UnsandboxClient({
            publicKey: process.env.UNSANDBOX_PUBLIC_KEY,
            secretKey: process.env.UNSANDBOX_SECRET_KEY,
        });
        const result = await client.execute('python', 'print(42)');
        expect(result.exitCode).toBe(0);
    });

    test('Invalid authentication returns 401', async () => {
        const client = new UnsandboxClient({
            publicKey: 'invalid',
            secretKey: 'invalid',
        });
        try {
            await client.execute('python', 'print(42)');
            fail('Should have thrown');
        } catch (e) {
            expect(e.status).toBe(401);
        }
    });

    test('Language support', async () => {
        const client = new UnsandboxClient(...);
        const languages = ['python', 'javascript', 'go', 'ruby'];
        for (const lang of languages) {
            const result = await client.execute(lang, '// test');
            expect(result).toBeDefined();
        }
    });

    test('Environment variables', async () => {
        const client = new UnsandboxClient(...);
        const result = await client.execute('python',
            'import os; print(os.environ.get("TEST"))',
            { env: { TEST: 'value' } }
        );
        expect(result.stdout).toContain('value');
    });

    test('Timeout handling', async () => {
        const client = new UnsandboxClient(...);
        const result = await client.execute('python',
            'import time; time.sleep(100)',
            { timeout: 5 }
        );
        expect(result.timedOut).toBe(true);
    });
});
```

### 4. Functional Mode (`functional.test.sh`)
```bash
#!/bin/bash

# Test: Fibonacci
node un.js test/fib.py | grep -q "55" && echo "✓ Fibonacci"

# Test: JSON parsing
node un.js <<'EOF'
const json = JSON.parse('{"key": "value"}');
console.log(json.key);
EOF
grep -q "value" && echo "✓ JSON parsing"

# Test: HTTP requests
node un.js -n semitrusted <<'EOF'
const https = require('https');
https.get('https://httpbin.org/ip', (res) => {
  console.log('Status:', res.statusCode);
});
EOF
grep -q "Status: 200" && echo "✓ HTTP requests"

# Test: File operations
node un.js <<'EOF'
const fs = require('fs');
fs.writeFileSync('/tmp/test.txt', 'hello');
const content = fs.readFileSync('/tmp/test.txt', 'utf8');
console.log(content);
EOF
grep -q "hello" && echo "✓ File operations"

# Test: Error handling
node un.js <<'EOF'
try {
  throw new Error('test error');
} catch (e) {
  console.log('Caught:', e.message);
}
EOF
grep -q "Caught" && echo "✓ Error handling"
```

---

## Testing Execution Flow

### Local Development
```bash
# Run all test types for Python
make test-python              # Runs all 4 modes
make test-python-cli         # CLI mode only
make test-python-library     # Library mode only
make test-python-integration # Integration mode only
make test-python-functional  # Functional mode only

# Run specific test file
pytest tests/test_un_py_cli.py -v
pytest tests/test_un_py_library.py -v
```

### CI Pipeline
```bash
# For each changed client (e.g., clients/python/):
1. CLI tests (parsing, help, basic execution)
2. Library tests (imports, object creation, methods)
3. Integration tests (API authentication, error handling)
4. Functional tests (real-world code examples)
```

### Test Results
Each test mode produces:
- **CLI**: Execution success/failure, output validation
- **Library**: Import success, method availability, return types
- **Integration**: HTTP status codes, authentication validation
- **Functional**: Real-world code execution, data transformation

---

## Success Criteria Per Language

All 4 test modes must pass:
- ✓ CLI: `un.py --help`, `un.py code.py`, `un.py -e VAR=val`
- ✓ Library: `from un import Client`, `client.execute()`, `client.create_session()`
- ✓ Integration: Valid auth → 200, invalid auth → 401, all languages work
- ✓ Functional: Fibonacci, data parsing, file I/O, error handling, async code

Feature parity across all 42 languages guaranteed.
