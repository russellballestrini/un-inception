#!/usr/bin/env python3
"""
Test suite for UN CLI Python implementation (un.py)
Tests extension detection, API calls, and end-to-end functionality
"""

import os
import sys
import subprocess
import json

# Add parent directory to path to import un module
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# Import the un module functions
import un

# Test configuration
UN_SCRIPT = os.path.join(os.path.dirname(os.path.dirname(__file__)), 'un.py')
FIB_PY = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), 'test', 'fib.py')

class TestResults:
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.skipped = 0

    def pass_test(self, name):
        print(f"PASS: {name}")
        self.passed += 1

    def fail_test(self, name, error):
        print(f"FAIL: {name} - {error}")
        self.failed += 1

    def skip_test(self, name, reason):
        print(f"SKIP: {name} - {reason}")
        self.skipped += 1

results = TestResults()

# Test 1: Extension detection for Python
try:
    lang = un.detect_language('test.py')
    if lang == 'python':
        results.pass_test("Extension detection: .py -> python")
    else:
        results.fail_test("Extension detection: .py -> python", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .py -> python", str(e))

# Test 2: Extension detection for JavaScript
try:
    lang = un.detect_language('test.js')
    if lang == 'javascript':
        results.pass_test("Extension detection: .js -> javascript")
    else:
        results.fail_test("Extension detection: .js -> javascript", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .js -> javascript", str(e))

# Test 3: Extension detection for Ruby
try:
    lang = un.detect_language('test.rb')
    if lang == 'ruby':
        results.pass_test("Extension detection: .rb -> ruby")
    else:
        results.fail_test("Extension detection: .rb -> ruby", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .rb -> ruby", str(e))

# Test 4: Extension detection for Go
try:
    lang = un.detect_language('test.go')
    if lang == 'go':
        results.pass_test("Extension detection: .go -> go")
    else:
        results.fail_test("Extension detection: .go -> go", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .go -> go", str(e))

# Test 5: Extension detection for Rust
try:
    lang = un.detect_language('test.rs')
    if lang == 'rust':
        results.pass_test("Extension detection: .rs -> rust")
    else:
        results.fail_test("Extension detection: .rs -> rust", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .rs -> rust", str(e))

# Test 6: Extension detection for unknown extension
try:
    lang = un.detect_language('test.unknown', exit_on_error=False)
    if lang is None:
        results.pass_test("Extension detection: .unknown -> None")
    else:
        results.fail_test("Extension detection: .unknown -> None", f"Got {lang}")
except Exception as e:
    results.fail_test("Extension detection: .unknown -> None", str(e))

# Test 7: API call test (requires UNSANDBOX auth)
has_hmac = os.environ.get('UNSANDBOX_PUBLIC_KEY') and os.environ.get('UNSANDBOX_SECRET_KEY')
has_legacy = os.environ.get('UNSANDBOX_API_KEY')
if not (has_hmac or has_legacy):
    results.skip_test("API call test", "UNSANDBOX authentication not configured")
else:
    try:
        result = un.execute_code('python', 'print("Hello from API")')
        if 'stdout' in result and 'Hello from API' in result['stdout']:
            results.pass_test("API call test")
        else:
            results.fail_test("API call test", f"Unexpected result: {result}")
    except Exception as e:
        results.fail_test("API call test", str(e))

# Test 8: End-to-end test with fib.py
has_hmac = os.environ.get('UNSANDBOX_PUBLIC_KEY') and os.environ.get('UNSANDBOX_SECRET_KEY')
has_legacy = os.environ.get('UNSANDBOX_API_KEY')
if not (has_hmac or has_legacy):
    results.skip_test("End-to-end fib.py test", "UNSANDBOX authentication not configured")
elif not os.path.exists(FIB_PY):
    results.skip_test("End-to-end fib.py test", f"fib.py not found at {FIB_PY}")
else:
    try:
        result = subprocess.run(
            [sys.executable, UN_SCRIPT, FIB_PY],
            capture_output=True,
            text=True,
            timeout=30
        )

        # Check for expected output
        if 'fib(10) = 55' in result.stdout:
            results.pass_test("End-to-end fib.py test")
        else:
            results.fail_test("End-to-end fib.py test",
                            f"Expected 'fib(10) = 55' in output, got: {result.stdout[:200]}")
    except subprocess.TimeoutExpired:
        results.fail_test("End-to-end fib.py test", "Timeout (30s)")
    except Exception as e:
        results.fail_test("End-to-end fib.py test", str(e))

# Test 9: File reading test
try:
    # Create a temporary test file
    test_file = '/tmp/test_un_py_temp.txt'
    test_content = 'test content 123'
    with open(test_file, 'w') as f:
        f.write(test_content)

    content = un.read_file(test_file)
    os.unlink(test_file)

    if content == test_content:
        results.pass_test("File reading test")
    else:
        results.fail_test("File reading test", f"Expected '{test_content}', got '{content}'")
except Exception as e:
    results.fail_test("File reading test", str(e))

# Print summary
print("\n" + "="*50)
print(f"Test Summary:")
print(f"  PASSED:  {results.passed}")
print(f"  FAILED:  {results.failed}")
print(f"  SKIPPED: {results.skipped}")
print(f"  TOTAL:   {results.passed + results.failed + results.skipped}")
print("="*50)

# Exit with appropriate code
sys.exit(0 if results.failed == 0 else 1)
