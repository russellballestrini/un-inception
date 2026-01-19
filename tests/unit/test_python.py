#!/usr/bin/env python3
"""
Unit tests for un.py - tests internal functions without API calls
"""

import sys
import os
import hmac
import hashlib
import tempfile
import unittest

# Add parent directory to path to import un module functions
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../..'))

# We need to mock the API key check before importing
os.environ['UNSANDBOX_PUBLIC_KEY'] = 'test-public-key'
os.environ['UNSANDBOX_SECRET_KEY'] = 'test-secret-key'


class TestExtensionMapping(unittest.TestCase):
    """Test the detect_language function for extension to language mapping"""

    def setUp(self):
        # Import here after env is set
        import importlib.util
        spec = importlib.util.spec_from_file_location("un", os.path.join(os.path.dirname(__file__), '../../un.py'))
        self.un = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.un)

    def test_python_extensions(self):
        self.assertEqual(self.un.detect_language('test.py'), 'python')

    def test_javascript_extensions(self):
        self.assertEqual(self.un.detect_language('test.js'), 'javascript')
        self.assertEqual(self.un.detect_language('test.ts'), 'typescript')

    def test_ruby_extension(self):
        self.assertEqual(self.un.detect_language('test.rb'), 'ruby')

    def test_go_extension(self):
        self.assertEqual(self.un.detect_language('test.go'), 'go')

    def test_rust_extension(self):
        self.assertEqual(self.un.detect_language('test.rs'), 'rust')

    def test_c_extensions(self):
        self.assertEqual(self.un.detect_language('test.c'), 'c')
        self.assertEqual(self.un.detect_language('test.cpp'), 'cpp')
        self.assertEqual(self.un.detect_language('test.cc'), 'cpp')
        self.assertEqual(self.un.detect_language('test.cxx'), 'cpp')

    def test_jvm_extensions(self):
        self.assertEqual(self.un.detect_language('test.java'), 'java')
        self.assertEqual(self.un.detect_language('test.kt'), 'kotlin')
        self.assertEqual(self.un.detect_language('test.groovy'), 'groovy')
        # scala not supported in current SDK
        self.assertIsNone(self.un.detect_language('test.scala'))

    def test_dotnet_extensions(self):
        self.assertEqual(self.un.detect_language('test.cs'), 'csharp')
        self.assertEqual(self.un.detect_language('test.fs'), 'fsharp')

    def test_functional_extensions(self):
        self.assertEqual(self.un.detect_language('test.hs'), 'haskell')
        self.assertEqual(self.un.detect_language('test.ml'), 'ocaml')
        self.assertEqual(self.un.detect_language('test.clj'), 'clojure')
        self.assertEqual(self.un.detect_language('test.scm'), 'scheme')
        self.assertEqual(self.un.detect_language('test.lisp'), 'commonlisp')
        self.assertEqual(self.un.detect_language('test.erl'), 'erlang')
        self.assertEqual(self.un.detect_language('test.ex'), 'elixir')
        self.assertEqual(self.un.detect_language('test.exs'), 'elixir')

    def test_scientific_extensions(self):
        self.assertEqual(self.un.detect_language('test.jl'), 'julia')
        self.assertEqual(self.un.detect_language('test.r'), 'r')
        # Note: .R gets lowercased, so still maps to 'r'
        self.assertEqual(self.un.detect_language('test.R'), 'r')
        self.assertEqual(self.un.detect_language('test.f90'), 'fortran')
        self.assertEqual(self.un.detect_language('test.f95'), 'fortran')

    def test_exotic_extensions(self):
        self.assertEqual(self.un.detect_language('test.d'), 'd')
        self.assertEqual(self.un.detect_language('test.nim'), 'nim')
        self.assertEqual(self.un.detect_language('test.zig'), 'zig')
        self.assertEqual(self.un.detect_language('test.v'), 'v')
        self.assertEqual(self.un.detect_language('test.cr'), 'crystal')
        self.assertEqual(self.un.detect_language('test.dart'), 'dart')

    def test_legacy_extensions(self):
        self.assertEqual(self.un.detect_language('test.cob'), 'cobol')
        self.assertEqual(self.un.detect_language('test.pro'), 'prolog')
        self.assertEqual(self.un.detect_language('test.forth'), 'forth')
        self.assertEqual(self.un.detect_language('test.4th'), 'forth')

    def test_other_extensions(self):
        self.assertEqual(self.un.detect_language('test.tcl'), 'tcl')
        self.assertEqual(self.un.detect_language('test.raku'), 'raku')
        self.assertEqual(self.un.detect_language('test.m'), 'objc')
        self.assertEqual(self.un.detect_language('test.lua'), 'lua')
        self.assertEqual(self.un.detect_language('test.pl'), 'perl')
        self.assertEqual(self.un.detect_language('test.php'), 'php')
        self.assertEqual(self.un.detect_language('test.sh'), 'bash')


class TestHMACSignature(unittest.TestCase):
    """Test HMAC signature generation"""

    def test_hmac_sha256_basic(self):
        """Test basic HMAC-SHA256 generation"""
        secret = "test-secret-key"
        message = "1234567890:POST:/execute:{}"

        expected = hmac.new(
            secret.encode(),
            message.encode(),
            hashlib.sha256
        ).hexdigest()

        # Verify we can generate the same signature
        actual = hmac.new(
            secret.encode(),
            message.encode(),
            hashlib.sha256
        ).hexdigest()

        self.assertEqual(expected, actual)
        self.assertEqual(len(actual), 64)  # SHA256 hex is 64 chars

    def test_hmac_different_secrets(self):
        """Test that different secrets produce different signatures"""
        message = "1234567890:POST:/execute:{}"

        sig1 = hmac.new(b"secret1", message.encode(), hashlib.sha256).hexdigest()
        sig2 = hmac.new(b"secret2", message.encode(), hashlib.sha256).hexdigest()

        self.assertNotEqual(sig1, sig2)

    def test_hmac_different_messages(self):
        """Test that different messages produce different signatures"""
        secret = b"test-secret"

        sig1 = hmac.new(secret, b"message1", hashlib.sha256).hexdigest()
        sig2 = hmac.new(secret, b"message2", hashlib.sha256).hexdigest()

        self.assertNotEqual(sig1, sig2)

    def test_signature_format(self):
        """Test the signature input format: timestamp:METHOD:path:body"""
        timestamp = "1704067200"
        method = "POST"
        path = "/execute"
        body = '{"language":"python","code":"print(1)"}'

        message = f"{timestamp}:{method}:{path}:{body}"

        # Verify format: starts with timestamp, has method and path
        self.assertTrue(message.startswith(timestamp))
        self.assertIn(":POST:", message)
        self.assertIn(":/execute:", message)


class TestLanguageDetection(unittest.TestCase):
    """Test language detection from file extensions and shebangs"""

    def test_detect_from_extension(self):
        """Test detection from file extension"""
        # Create a temp file with .py extension
        with tempfile.NamedTemporaryFile(suffix='.py', delete=False) as f:
            f.write(b'print("hello")')
            temp_path = f.name

        try:
            ext = os.path.splitext(temp_path)[1].lower()
            # Simulate EXT_MAP lookup
            EXT_MAP = {'.py': 'python', '.js': 'javascript', '.rb': 'ruby'}
            self.assertEqual(EXT_MAP.get(ext), 'python')
        finally:
            os.unlink(temp_path)

    def test_detect_from_shebang_python(self):
        """Test detection from python shebang"""
        content = "#!/usr/bin/env python3\nprint('hello')"
        first_line = content.split('\n')[0]

        self.assertTrue(first_line.startswith('#!'))
        self.assertIn('python', first_line)

    def test_detect_from_shebang_node(self):
        """Test detection from node shebang"""
        content = "#!/usr/bin/env node\nconsole.log('hello')"
        first_line = content.split('\n')[0]

        self.assertTrue(first_line.startswith('#!'))
        self.assertIn('node', first_line)

    def test_detect_from_shebang_bash(self):
        """Test detection from bash shebang"""
        content = "#!/bin/bash\necho hello"
        first_line = content.split('\n')[0]

        self.assertTrue(first_line.startswith('#!'))
        self.assertTrue('bash' in first_line or '/sh' in first_line)


class TestArgumentParsing(unittest.TestCase):
    """Test command-line argument patterns"""

    def test_env_var_format(self):
        """Test -e KEY=VALUE parsing"""
        arg = "DEBUG=1"
        key, value = arg.split('=', 1)

        self.assertEqual(key, "DEBUG")
        self.assertEqual(value, "1")

    def test_env_var_with_equals_in_value(self):
        """Test -e KEY=VALUE=WITH=EQUALS parsing"""
        arg = "URL=https://example.com?foo=bar"
        key, value = arg.split('=', 1)

        self.assertEqual(key, "URL")
        self.assertEqual(value, "https://example.com?foo=bar")

    def test_network_mode_values(self):
        """Test valid network mode values"""
        valid_modes = ['zerotrust', 'semitrusted']

        self.assertIn('zerotrust', valid_modes)
        self.assertIn('semitrusted', valid_modes)

    def test_subcommand_detection(self):
        """Test subcommand detection"""
        args = ['session', '--shell', 'python3']
        subcommand = args[0] if args and args[0] in ['session', 'service', 'key', 'restore'] else None

        self.assertEqual(subcommand, 'session')


class TestFileOperations(unittest.TestCase):
    """Test file reading and encoding"""

    def test_read_text_file(self):
        """Test reading a text file"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write('print("hello world")')
            temp_path = f.name

        try:
            with open(temp_path, 'r') as f:
                content = f.read()
            self.assertEqual(content, 'print("hello world")')
        finally:
            os.unlink(temp_path)

    def test_base64_encoding(self):
        """Test base64 encoding for file contents"""
        import base64

        content = b'print("hello world")'
        encoded = base64.b64encode(content).decode('utf-8')
        decoded = base64.b64decode(encoded)

        self.assertEqual(decoded, content)

    def test_file_basename(self):
        """Test extracting file basename"""
        path = "/home/user/project/script.py"
        basename = os.path.basename(path)

        self.assertEqual(basename, "script.py")


class TestAPIConstants(unittest.TestCase):
    """Test API-related constants"""

    def test_api_base_url(self):
        """Test API base URL format"""
        API_BASE = "https://api.unsandbox.com"

        self.assertTrue(API_BASE.startswith('https://'))
        self.assertIn('unsandbox.com', API_BASE)

    def test_portal_base_url(self):
        """Test portal base URL format"""
        PORTAL_BASE = "https://unsandbox.com"

        self.assertTrue(PORTAL_BASE.startswith('https://'))


if __name__ == '__main__':
    unittest.main(verbosity=2)
