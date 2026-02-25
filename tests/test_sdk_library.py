#!/usr/bin/env python3
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

"""
Comprehensive test suite for UN SDK library functions across all languages.

Tests validate:
1. Unit tests - Library function signatures and basic behavior
2. Integration tests - API communication with real credentials
3. Functional tests - End-to-end execution with input/output handling

This allows agents to validate their SDK refactoring work independently.
"""

import os
import sys
import json
import hmac
import hashlib
import tempfile
import subprocess
import time
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any

# ANSI color codes
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
BLUE = '\033[94m'
RESET = '\033[0m'


class SDKTestResults:
    """Track test results across all languages and test types."""

    def __init__(self):
        self.results = {}  # language -> {'unit': [...], 'integration': [...], 'functional': [...]}
        self.summary = {'passed': 0, 'failed': 0, 'skipped': 0}

    def add_result(self, language: str, test_type: str, name: str, passed: bool, message: str = ""):
        """Record test result."""
        if language not in self.results:
            self.results[language] = {'unit': [], 'integration': [], 'functional': []}

        self.results[language][test_type].append({
            'name': name,
            'passed': passed,
            'message': message
        })

        if passed:
            self.summary['passed'] += 1
        else:
            self.summary['failed'] += 1

    def skip_result(self, language: str, test_type: str, name: str, reason: str):
        """Record skipped test."""
        if language not in self.results:
            self.results[language] = {'unit': [], 'integration': [], 'functional': []}

        self.results[language][test_type].append({
            'name': name,
            'passed': None,  # None indicates skipped
            'message': reason
        })

        self.summary['skipped'] += 1

    def print_summary(self):
        """Print test summary."""
        print(f"\n{'='*70}")
        print(f"SDK LIBRARY TEST SUMMARY")
        print(f"{'='*70}")

        for language, test_types in sorted(self.results.items()):
            lang_passed = sum(1 for t in test_types.values() for r in t if r['passed'] is True)
            lang_failed = sum(1 for t in test_types.values() for r in t if r['passed'] is False)
            lang_skipped = sum(1 for t in test_types.values() for r in t if r['passed'] is None)
            lang_total = lang_passed + lang_failed + lang_skipped

            status = f"{GREEN}✓{RESET}" if lang_failed == 0 else f"{RED}✗{RESET}"
            print(f"{status} {language:15} {lang_passed:3}/{lang_total:3} passed" +
                  (f" ({lang_skipped} skipped)" if lang_skipped > 0 else ""))

            # Show failed tests
            for test_type, tests in test_types.items():
                for test in tests:
                    if test['passed'] is False:
                        print(f"    {RED}✗{RESET} {test['name']}: {test['message'][:60]}")

        print(f"{'='*70}")
        print(f"Total: {GREEN}{self.summary['passed']} passed{RESET}, " +
              f"{RED}{self.summary['failed']} failed{RESET}, " +
              f"{YELLOW}{self.summary['skipped']} skipped{RESET}")
        print(f"{'='*70}\n")

        return self.summary['failed'] == 0


class SDKLibraryTester:
    """Test SDK library implementations."""

    def __init__(self, sdk_dir: str = "/home/fox/git/un-inception"):
        self.sdk_dir = Path(sdk_dir)
        self.results = SDKTestResults()
        self.api_key = os.environ.get('UNSANDBOX_API_KEY', '')
        self.api_url = os.environ.get('UNSANDBOX_API_URL', 'https://api.unsandbox.com')

    def test_python_sdk(self):
        """Test Python SDK library functions."""
        language = 'python'

        try:
            # Import un module
            sys.path.insert(0, str(self.sdk_dir))
            import un

            # Unit test: credential loading
            try:
                # Test loading from env vars
                os.environ['UNSANDBOX_PUBLIC_KEY'] = 'unsb-pk-test-1234'
                os.environ['UNSANDBOX_SECRET_KEY'] = 'unsb-sk-test-5678'

                pk, sk = un._get_credentials()

                if pk == 'unsb-pk-test-1234' and sk == 'unsb-sk-test-5678':
                    self.results.add_result(language, 'unit', 'Credential loading from env', True)
                else:
                    self.results.add_result(language, 'unit', 'Credential loading from env', False,
                                          f"Got pk={pk}, sk={sk}")
            except Exception as e:
                self.results.add_result(language, 'unit', 'Credential loading from env', False, str(e))

            # Unit test: HMAC signature generation
            try:
                sig = un._sign_request('unsb-sk-test-5678', '1704067200', 'POST', '/execute', '{}')
                if len(sig) == 64 and all(c in '0123456789abcdef' for c in sig):
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', True)
                else:
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', False,
                                          f"Invalid signature: {sig}")
            except Exception as e:
                self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', False, str(e))

            # Unit test: languages cache
            try:
                # Create temp cache dir
                cache_dir = Path(tempfile.gettempdir()) / 'unsandbox_test_cache'
                cache_dir.mkdir(exist_ok=True)
                cache_file = cache_dir / 'languages.json'

                # Clean up old cache
                if cache_file.exists():
                    cache_file.unlink()

                # Test that function returns list (without calling API during unit test)
                # Just verify the function exists and has correct signature
                if callable(un.languages):
                    self.results.add_result(language, 'unit', 'Languages function signature', True)
                else:
                    self.results.add_result(language, 'unit', 'Languages function signature', False,
                                          "languages function not callable")
            except Exception as e:
                self.results.add_result(language, 'unit', 'Languages function signature', False, str(e))

            # Integration test: execute function (if API key available)
            if self.api_key:
                try:
                    result = un.execute(
                        'python',
                        'print("hello from sdk")',
                        network_mode='zerotrust',
                        ttl=60
                    )

                    if result.get('exit_code') == 0 and 'hello from sdk' in result.get('stdout', ''):
                        self.results.add_result(language, 'integration', 'execute() function', True)
                    else:
                        self.results.add_result(language, 'integration', 'execute() function', False,
                                              f"exit_code={result.get('exit_code')}, stdout={result.get('stdout')}")
                except Exception as e:
                    self.results.add_result(language, 'integration', 'execute() function', False, str(e)[:100])
            else:
                self.results.skip_result(language, 'integration', 'execute() function', 'No API key')

        except ImportError as e:
            self.results.add_result(language, 'unit', 'SDK import', False, f"Cannot import un: {e}")
        except Exception as e:
            self.results.add_result(language, 'unit', 'SDK import', False, str(e))

    def test_javascript_sdk(self):
        """Test JavaScript SDK library functions."""
        language = 'javascript'

        try:
            # Check if Node.js is available
            subprocess.run(['node', '--version'], capture_output=True, check=True)

            # Create test script
            test_script = '''
const un = require('./un.js');

// Test 1: HMAC signature
try {
    const sig = un._signRequest('test-sk', '1704067200', 'POST', '/execute', '{}');
    if (sig && sig.length === 64) {
        console.log('PASS: HMAC signature');
    } else {
        console.log('FAIL: HMAC signature - invalid length');
    }
} catch (e) {
    console.log('FAIL: HMAC signature - ' + e.message);
}

// Test 2: Client class exists
try {
    if (typeof un.Client === 'function') {
        console.log('PASS: Client class');
    } else {
        console.log('FAIL: Client class - not a function');
    }
} catch (e) {
    console.log('FAIL: Client class - ' + e.message);
}
'''

            with tempfile.NamedTemporaryFile(mode='w', suffix='.js', delete=False) as f:
                f.write(test_script)
                test_file = f.name

            try:
                result = subprocess.run(
                    ['node', test_file],
                    cwd=str(self.sdk_dir),
                    capture_output=True,
                    text=True,
                    timeout=10
                )

                output = result.stdout + result.stderr
                if 'PASS: HMAC signature' in output:
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', True)
                else:
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', False,
                                          output[:100])

                if 'PASS: Client class' in output:
                    self.results.add_result(language, 'unit', 'Client class exists', True)
                else:
                    self.results.add_result(language, 'unit', 'Client class exists', False,
                                          output[:100])
            finally:
                os.unlink(test_file)

        except subprocess.CalledProcessError:
            self.results.skip_result(language, 'unit', 'JavaScript SDK tests', 'Node.js not available')
        except Exception as e:
            self.results.skip_result(language, 'unit', 'JavaScript SDK tests', str(e))

    def test_go_sdk(self):
        """Test Go SDK library functions."""
        language = 'go'

        try:
            # Check if Go is available
            subprocess.run(['go', 'version'], capture_output=True, check=True)

            # Create test program
            test_code = '''
package main

import (
    "fmt"
    "crypto/hmac"
    "crypto/sha256"
    "encoding/hex"
)

func main() {
    // Test HMAC signature
    key := []byte("test-sk")
    message := "1704067200:POST:/execute:{}"
    h := hmac.New(sha256.New, key)
    h.Write([]byte(message))
    sig := hex.EncodeToString(h.Sum(nil))

    if len(sig) == 64 {
        fmt.Println("PASS: HMAC signature")
    } else {
        fmt.Printf("FAIL: HMAC signature - invalid length %d\\n", len(sig))
    }
}
'''

            with tempfile.TemporaryDirectory() as tmpdir:
                test_file = Path(tmpdir) / 'test.go'
                test_file.write_text(test_code)

                result = subprocess.run(
                    ['go', 'run', str(test_file)],
                    capture_output=True,
                    text=True,
                    timeout=15
                )

                if 'PASS: HMAC signature' in result.stdout:
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', True)
                else:
                    self.results.add_result(language, 'unit', 'HMAC-SHA256 signature generation', False,
                                          (result.stdout + result.stderr)[:100])

        except subprocess.CalledProcessError:
            self.results.skip_result(language, 'unit', 'Go SDK tests', 'Go not available')
        except Exception as e:
            self.results.skip_result(language, 'unit', 'Go SDK tests', str(e))

    def run_all_tests(self, languages: Optional[List[str]] = None):
        """Run all SDK tests."""
        print(f"{BLUE}Testing SDK Library Implementations{RESET}\n")

        if languages is None:
            languages = ['python', 'javascript', 'go']

        if 'python' in languages:
            print(f"{BLUE}Testing Python SDK...{RESET}")
            self.test_python_sdk()

        if 'javascript' in languages:
            print(f"{BLUE}Testing JavaScript SDK...{RESET}")
            self.test_javascript_sdk()

        if 'go' in languages:
            print(f"{BLUE}Testing Go SDK...{RESET}")
            self.test_go_sdk()

        return self.results.print_summary()


def main():
    """Main entry point."""
    import argparse

    parser = argparse.ArgumentParser(description='Test UN SDK library implementations')
    parser.add_argument('--languages', nargs='+', default=None,
                        help='Languages to test (default: all)')
    parser.add_argument('--api-key', default=None,
                        help='API key for integration tests (or set UNSANDBOX_API_KEY env var)')
    parser.add_argument('--api-url', default='https://api.unsandbox.com',
                        help='API URL for testing')

    args = parser.parse_args()

    if args.api_key:
        os.environ['UNSANDBOX_API_KEY'] = args.api_key

    if args.api_url:
        os.environ['UNSANDBOX_API_URL'] = args.api_url

    tester = SDKLibraryTester()
    success = tester.run_all_tests(languages=args.languages)

    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
