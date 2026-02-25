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
Tests for HMAC request signing
"""

import pytest
import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un_async import _sign_request


class TestHMACSignature:
    """Test HMAC-SHA256 request signing."""

    def test_sign_request_basic(self):
        """Test basic request signing."""
        signature = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        # Signature should be a 64-character hex string
        assert isinstance(signature, str)
        assert len(signature) == 64
        assert all(c in "0123456789abcdef" for c in signature)

    def test_sign_request_deterministic(self):
        """Test that signing is deterministic."""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        assert sig1 == sig2

    def test_sign_request_different_secrets(self):
        """Test that different secrets produce different signatures."""
        sig1 = _sign_request(
            secret_key="secret1",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        sig2 = _sign_request(
            secret_key="secret2",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        assert sig1 != sig2

    def test_sign_request_different_timestamps(self):
        """Test that different timestamps produce different signatures."""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567891,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        assert sig1 != sig2

    def test_sign_request_different_methods(self):
        """Test that different HTTP methods produce different signatures."""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/execute",
            body='{"code":"test"}',
        )

        assert sig1 != sig2

    def test_sign_request_different_paths(self):
        """Test that different paths produce different signatures."""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/jobs/123",
            body=None,
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/jobs/456",
            body=None,
        )

        assert sig1 != sig2

    def test_sign_request_empty_body(self):
        """Test signing with empty/no body."""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/languages",
            body=None,
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/languages",
            body="",
        )

        # Both should produce valid signatures
        assert isinstance(sig1, str) and len(sig1) == 64
        assert isinstance(sig2, str) and len(sig2) == 64

    def test_sign_request_special_characters_in_body(self):
        """Test signing with special characters in body."""
        body_with_special = '{"code":"print(\\"hello\\")"}'
        signature = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body=body_with_special,
        )

        assert isinstance(signature, str)
        assert len(signature) == 64

    def test_sign_request_unicode_in_secret(self):
        """Test signing with unicode characters in secret."""
        # Note: In production, secrets should be ASCII, but test unicode handling
        signature = _sign_request(
            secret_key="secret_with_unicode_αβγ",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"test"}',
        )

        assert isinstance(signature, str)
        assert len(signature) == 64

    def test_sign_request_message_format(self):
        """Verify the message format is correct: timestamp:METHOD:path:body"""
        # We can't directly inspect the message, but we can verify the signature
        # matches what we'd expect if we implement it ourselves
        import hmac
        import hashlib

        secret = "test_secret"
        timestamp = 1234567890
        method = "POST"
        path = "/execute"
        body = '{"test":"data"}'

        # Build expected message
        expected_message = f"{timestamp}:{method}:{path}:{body}"
        expected_signature = hmac.new(
            secret.encode(),
            expected_message.encode(),
            hashlib.sha256,
        ).hexdigest()

        # Compare with function output
        actual_signature = _sign_request(secret, timestamp, method, path, body)
        assert actual_signature == expected_signature
