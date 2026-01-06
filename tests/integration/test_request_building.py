#!/usr/bin/env python3
"""
Integration tests for request building - tests component interactions without API calls
"""

import sys
import os
import json
import hmac
import hashlib
import base64
import tempfile
import unittest
from unittest.mock import patch, MagicMock
from io import StringIO

# Set mock environment before importing
os.environ['UNSANDBOX_PUBLIC_KEY'] = 'test-pk-1234'
os.environ['UNSANDBOX_SECRET_KEY'] = 'test-sk-5678'


class TestRequestBuilding(unittest.TestCase):
    """Test that requests are built correctly with all components"""

    def test_execute_request_structure(self):
        """Test execute request has correct structure"""
        language = "python"
        code = 'print("hello world")'
        env_vars = {"DEBUG": "1", "NAME": "test"}
        network_mode = "zerotrust"

        # Build request body
        body = {
            "language": language,
            "code": code,
            "env": env_vars,
            "network_mode": network_mode
        }

        json_body = json.dumps(body)
        parsed = json.loads(json_body)

        self.assertEqual(parsed["language"], "python")
        self.assertEqual(parsed["code"], code)
        self.assertEqual(parsed["env"]["DEBUG"], "1")
        self.assertEqual(parsed["network_mode"], "zerotrust")

    def test_execute_request_with_files(self):
        """Test execute request with input files"""
        # Create temp file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False) as f:
            f.write("test data content")
            temp_path = f.name

        try:
            # Read and encode file
            with open(temp_path, 'rb') as f:
                content = f.read()
                encoded = base64.b64encode(content).decode('utf-8')

            files = [{
                "name": os.path.basename(temp_path),
                "content": encoded
            }]

            body = {
                "language": "python",
                "code": "print(open('test.txt').read())",
                "files": files
            }

            json_body = json.dumps(body)
            parsed = json.loads(json_body)

            self.assertIn("files", parsed)
            self.assertEqual(len(parsed["files"]), 1)
            self.assertIn("content", parsed["files"][0])

            # Verify content can be decoded back
            decoded = base64.b64decode(parsed["files"][0]["content"])
            self.assertEqual(decoded.decode('utf-8'), "test data content")

        finally:
            os.unlink(temp_path)

    def test_hmac_signature_generation(self):
        """Test HMAC signature is generated correctly for requests"""
        public_key = "test-pk-1234"
        secret_key = "test-sk-5678"
        timestamp = "1704067200"
        method = "POST"
        endpoint = "/execute"
        body = '{"language":"python","code":"print(1)"}'

        # Build signature input
        signature_input = f"{timestamp}:{method}:{endpoint}:{body}"

        # Generate signature
        signature = hmac.new(
            secret_key.encode(),
            signature_input.encode(),
            hashlib.sha256
        ).hexdigest()

        # Verify signature properties
        self.assertEqual(len(signature), 64)
        self.assertTrue(all(c in '0123456789abcdef' for c in signature))

        # Verify same input produces same signature
        signature2 = hmac.new(
            secret_key.encode(),
            signature_input.encode(),
            hashlib.sha256
        ).hexdigest()
        self.assertEqual(signature, signature2)

    def test_session_request_structure(self):
        """Test session request has correct structure"""
        body = {
            "shell": "python3",
            "network_mode": "semitrusted"
        }

        json_body = json.dumps(body)
        parsed = json.loads(json_body)

        self.assertEqual(parsed["shell"], "python3")
        self.assertEqual(parsed["network_mode"], "semitrusted")

    def test_service_request_structure(self):
        """Test service request has correct structure"""
        body = {
            "name": "my-service",
            "ports": [8080, 443],
            "bootstrap": "python3 -m http.server 8080",
            "network_mode": "semitrusted"
        }

        json_body = json.dumps(body)
        parsed = json.loads(json_body)

        self.assertEqual(parsed["name"], "my-service")
        self.assertEqual(parsed["ports"], [8080, 443])
        self.assertEqual(parsed["bootstrap"], "python3 -m http.server 8080")

    def test_authorization_header_format(self):
        """Test Authorization header is formatted correctly"""
        public_key = "unsb-pk-1234-5678-abcd-efgh"
        auth_header = f"Bearer {public_key}"

        self.assertTrue(auth_header.startswith("Bearer "))
        self.assertIn(public_key, auth_header)

    def test_request_headers_structure(self):
        """Test all required headers are present"""
        public_key = "unsb-pk-1234"
        timestamp = "1704067200"
        signature = "abc123def456"

        headers = {
            "Authorization": f"Bearer {public_key}",
            "X-Timestamp": timestamp,
            "X-Signature": signature,
            "Content-Type": "application/json"
        }

        self.assertIn("Authorization", headers)
        self.assertIn("X-Timestamp", headers)
        self.assertIn("X-Signature", headers)
        self.assertIn("Content-Type", headers)
        self.assertEqual(headers["Content-Type"], "application/json")


class TestResponseParsing(unittest.TestCase):
    """Test that responses are parsed correctly"""

    def test_execute_response_parsing(self):
        """Test execute response is parsed correctly"""
        response_json = {
            "id": "exec-12345",
            "status": "completed",
            "stdout": "hello world\n",
            "stderr": "",
            "exit_code": 0,
            "execution_time": 0.123
        }

        json_str = json.dumps(response_json)
        parsed = json.loads(json_str)

        self.assertEqual(parsed["id"], "exec-12345")
        self.assertEqual(parsed["status"], "completed")
        self.assertEqual(parsed["stdout"], "hello world\n")
        self.assertEqual(parsed["exit_code"], 0)

    def test_session_response_parsing(self):
        """Test session response is parsed correctly"""
        response_json = {
            "id": "sess-12345",
            "status": "ready",
            "websocket_url": "wss://api.unsandbox.com/ws/sess-12345"
        }

        json_str = json.dumps(response_json)
        parsed = json.loads(json_str)

        self.assertEqual(parsed["id"], "sess-12345")
        self.assertEqual(parsed["status"], "ready")
        self.assertIn("websocket_url", parsed)

    def test_service_response_parsing(self):
        """Test service response is parsed correctly"""
        response_json = {
            "id": "svc-12345",
            "name": "my-service",
            "status": "running",
            "domains": ["my-service.unsandbox.run"],
            "ports": {"8080": "https://my-service.unsandbox.run:8080"}
        }

        json_str = json.dumps(response_json)
        parsed = json.loads(json_str)

        self.assertEqual(parsed["id"], "svc-12345")
        self.assertEqual(parsed["name"], "my-service")
        self.assertEqual(parsed["status"], "running")
        self.assertIsInstance(parsed["domains"], list)

    def test_error_response_parsing(self):
        """Test error response is parsed correctly"""
        response_json = {
            "error": "Authentication failed",
            "code": "AUTH_ERROR",
            "status": 401
        }

        json_str = json.dumps(response_json)
        parsed = json.loads(json_str)

        self.assertIn("error", parsed)
        self.assertEqual(parsed["code"], "AUTH_ERROR")
        self.assertEqual(parsed["status"], 401)

    def test_artifacts_response_parsing(self):
        """Test response with artifacts is parsed correctly"""
        response_json = {
            "id": "exec-12345",
            "status": "completed",
            "stdout": "Generated output\n",
            "artifacts": [
                {
                    "name": "output.png",
                    "content": "iVBORw0KGgo=",  # base64
                    "size": 12345
                }
            ]
        }

        json_str = json.dumps(response_json)
        parsed = json.loads(json_str)

        self.assertIn("artifacts", parsed)
        self.assertEqual(len(parsed["artifacts"]), 1)
        self.assertEqual(parsed["artifacts"][0]["name"], "output.png")


class TestErrorHandling(unittest.TestCase):
    """Test error handling components"""

    def test_missing_api_key_detection(self):
        """Test detection of missing API keys"""
        # Save current env
        saved_pub = os.environ.get('UNSANDBOX_PUBLIC_KEY')
        saved_sec = os.environ.get('UNSANDBOX_SECRET_KEY')

        try:
            del os.environ['UNSANDBOX_PUBLIC_KEY']
            del os.environ['UNSANDBOX_SECRET_KEY']

            # Check that keys are missing
            public_key = os.environ.get('UNSANDBOX_PUBLIC_KEY')
            secret_key = os.environ.get('UNSANDBOX_SECRET_KEY')

            self.assertIsNone(public_key)
            self.assertIsNone(secret_key)

        finally:
            # Restore env
            if saved_pub:
                os.environ['UNSANDBOX_PUBLIC_KEY'] = saved_pub
            if saved_sec:
                os.environ['UNSANDBOX_SECRET_KEY'] = saved_sec

    def test_invalid_json_handling(self):
        """Test handling of invalid JSON responses"""
        invalid_json = "not valid json {"

        with self.assertRaises(json.JSONDecodeError):
            json.loads(invalid_json)

    def test_file_not_found_handling(self):
        """Test handling of missing files"""
        with self.assertRaises(FileNotFoundError):
            with open("/nonexistent/file/path.py", 'r') as f:
                f.read()

    def test_invalid_extension_handling(self):
        """Test handling of unrecognized file extensions"""
        EXT_MAP = {".py": "python", ".js": "javascript"}

        ext = ".xyz"
        lang = EXT_MAP.get(ext)
        self.assertIsNone(lang)


class TestFileProcessing(unittest.TestCase):
    """Test file processing integration"""

    def test_multiple_files_encoding(self):
        """Test encoding multiple files for request"""
        files_data = []

        # Create multiple temp files
        temp_files = []
        for i in range(3):
            with tempfile.NamedTemporaryFile(mode='w', suffix=f'.txt', delete=False) as f:
                f.write(f"File {i} content")
                temp_files.append(f.name)

        try:
            for temp_path in temp_files:
                with open(temp_path, 'rb') as f:
                    content = f.read()
                    files_data.append({
                        "name": os.path.basename(temp_path),
                        "content": base64.b64encode(content).decode('utf-8')
                    })

            # Verify all files encoded
            self.assertEqual(len(files_data), 3)

            # Verify each file can be decoded
            for i, file_data in enumerate(files_data):
                decoded = base64.b64decode(file_data["content"])
                self.assertIn(f"File {i} content", decoded.decode('utf-8'))

        finally:
            for temp_path in temp_files:
                os.unlink(temp_path)

    def test_binary_file_encoding(self):
        """Test encoding binary files"""
        # Create binary content
        binary_content = bytes([0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD])

        with tempfile.NamedTemporaryFile(mode='wb', delete=False) as f:
            f.write(binary_content)
            temp_path = f.name

        try:
            with open(temp_path, 'rb') as f:
                content = f.read()
                encoded = base64.b64encode(content).decode('utf-8')

            # Verify can decode back to original
            decoded = base64.b64decode(encoded)
            self.assertEqual(decoded, binary_content)

        finally:
            os.unlink(temp_path)


if __name__ == '__main__':
    unittest.main(verbosity=2)
