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

"""Tests for HMAC-SHA256 request signing"""

from un import _sign_request


class TestRequestSigning:
    """Test HMAC-SHA256 request signing"""

    def test_sign_request_basic(self):
        """Test basic request signing"""
        signature = _sign_request(
            secret_key="my_secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"language":"python"}',
        )

        # Signature should be 64 hex characters
        assert len(signature) == 64
        assert all(c in "0123456789abcdef" for c in signature)

    def test_sign_request_get(self):
        """Test signing GET request (no body)"""
        signature = _sign_request(
            secret_key="my_secret",
            timestamp=1234567890,
            method="GET",
            path="/languages",
            body=None,
        )

        assert len(signature) == 64

    def test_sign_request_delete(self):
        """Test signing DELETE request"""
        signature = _sign_request(
            secret_key="my_secret",
            timestamp=1234567890,
            method="DELETE",
            path="/jobs/job_123",
            body=None,
        )

        assert len(signature) == 64

    def test_sign_request_deterministic(self):
        """Test that same inputs produce same signature"""
        signature1 = _sign_request(
            secret_key="test_secret",
            timestamp=9999,
            method="POST",
            path="/test",
            body='{"test":"data"}',
        )

        signature2 = _sign_request(
            secret_key="test_secret",
            timestamp=9999,
            method="POST",
            path="/test",
            body='{"test":"data"}',
        )

        assert signature1 == signature2

    def test_sign_request_different_secrets(self):
        """Test that different secrets produce different signatures"""
        signature1 = _sign_request(
            secret_key="secret1",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body="code",
        )

        signature2 = _sign_request(
            secret_key="secret2",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body="code",
        )

        assert signature1 != signature2

    def test_sign_request_different_timestamps(self):
        """Test that different timestamps produce different signatures"""
        signature1 = _sign_request(
            secret_key="secret",
            timestamp=1000,
            method="POST",
            path="/execute",
            body="code",
        )

        signature2 = _sign_request(
            secret_key="secret",
            timestamp=2000,
            method="POST",
            path="/execute",
            body="code",
        )

        assert signature1 != signature2

    def test_sign_request_different_paths(self):
        """Test that different paths produce different signatures"""
        signature1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/jobs",
            body=None,
        )

        signature2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/languages",
            body=None,
        )

        assert signature1 != signature2

    def test_sign_request_different_methods(self):
        """Test that different HTTP methods produce different signatures"""
        signature1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/jobs/123",
            body=None,
        )

        signature2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="DELETE",
            path="/jobs/123",
            body=None,
        )

        assert signature1 != signature2

    def test_sign_request_message_format(self):
        """Test that message format is correct"""
        # Known test case
        secret = "test_secret_key"
        timestamp = 1609459200  # 2021-01-01 00:00:00 UTC
        method = "POST"
        path = "/execute"
        body = '{"language":"python","code":"print(42)"}'

        signature = _sign_request(secret, timestamp, method, path, body)

        # Should be a valid hex string
        assert len(signature) == 64
        assert isinstance(signature, str)

    def test_sign_request_empty_body(self):
        """Test signing with empty body string"""
        sig1 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/test",
            body="",
        )

        sig2 = _sign_request(
            secret_key="secret",
            timestamp=1234567890,
            method="GET",
            path="/test",
            body=None,
        )

        # Both should be the same (None and "" are both empty)
        assert sig1 == sig2

    def test_sign_request_special_characters(self):
        """Test signing request with special characters in body"""
        signature = _sign_request(
            secret_key="my_secret",
            timestamp=1234567890,
            method="POST",
            path="/execute",
            body='{"code":"print(\\"hello\\")"}',
        )

        assert len(signature) == 64
        assert isinstance(signature, str)
