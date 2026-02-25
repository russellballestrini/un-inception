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
Tests for credential resolution system
"""

import pytest
import os
import tempfile
from pathlib import Path
import sys

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un_async import (
    CredentialsError,
    _resolve_credentials,
    _load_credentials_from_csv,
    _get_unsandbox_dir,
)


class TestCredentialResolution:
    """Test 4-tier credential resolution system."""

    def test_resolve_credentials_from_arguments(self):
        """Test Tier 1: Function arguments take highest priority."""
        pk, sk = _resolve_credentials("arg_pk", "arg_sk")
        assert pk == "arg_pk"
        assert sk == "arg_sk"

    def test_resolve_credentials_from_env(self, monkeypatch):
        """Test Tier 2: Environment variables."""
        monkeypatch.setenv("UNSANDBOX_PUBLIC_KEY", "env_pk")
        monkeypatch.setenv("UNSANDBOX_SECRET_KEY", "env_sk")
        # Clear any CSV-based credentials by not providing arguments
        pk, sk = _resolve_credentials(None, None)
        assert pk == "env_pk"
        assert sk == "env_sk"

    def test_credentials_error_no_sources(self, monkeypatch):
        """Test CredentialsError when no credentials found."""
        # Clear environment
        monkeypatch.delenv("UNSANDBOX_PUBLIC_KEY", raising=False)
        monkeypatch.delenv("UNSANDBOX_SECRET_KEY", raising=False)

        with pytest.raises(CredentialsError) as exc_info:
            _resolve_credentials(None, None)

        assert "No credentials found" in str(exc_info.value)

    def test_arguments_override_env(self, monkeypatch):
        """Test that function arguments override environment variables."""
        monkeypatch.setenv("UNSANDBOX_PUBLIC_KEY", "env_pk")
        monkeypatch.setenv("UNSANDBOX_SECRET_KEY", "env_sk")

        pk, sk = _resolve_credentials("arg_pk", "arg_sk")
        assert pk == "arg_pk"
        assert sk == "arg_sk"


class TestCSVCredentials:
    """Test CSV credential loading."""

    def test_load_csv_valid(self):
        """Test loading valid CSV file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            f.write("public_key_1,secret_key_1\n")
            f.write("public_key_2,secret_key_2\n")
            temp_path = f.name

        try:
            # Load first account
            creds = _load_credentials_from_csv(Path(temp_path), 0)
            assert creds == ("public_key_1", "secret_key_1")

            # Load second account
            creds = _load_credentials_from_csv(Path(temp_path), 1)
            assert creds == ("public_key_2", "secret_key_2")
        finally:
            os.unlink(temp_path)

    def test_load_csv_nonexistent(self):
        """Test loading from nonexistent file."""
        creds = _load_credentials_from_csv(Path("/nonexistent/path.csv"), 0)
        assert creds is None

    def test_load_csv_with_comments(self):
        """Test loading CSV with comment lines."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
            f.write("# This is a comment\n")
            f.write("public_key_1,secret_key_1\n")
            temp_path = f.name

        try:
            creds = _load_credentials_from_csv(Path(temp_path), 0)
            assert creds == ("public_key_1", "secret_key_1")
        finally:
            os.unlink(temp_path)

    def test_get_unsandbox_dir(self):
        """Test getting ~/.unsandbox directory."""
        unsandbox_dir = _get_unsandbox_dir()
        assert unsandbox_dir.exists()
        assert unsandbox_dir.name == ".unsandbox"
