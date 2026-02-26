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

"""Tests for credential resolution in unsandbox SDK"""

import os
import tempfile
from pathlib import Path
import pytest
from un import CredentialsError


def test_credentials_from_function_args():
    """Test that function arguments take priority"""
    from un import _resolve_credentials

    pk, sk = _resolve_credentials("func_pk", "func_sk")
    assert pk == "func_pk"
    assert sk == "func_sk"


def test_credentials_from_environment():
    """Test that environment variables are used when args are missing"""
    from un import _resolve_credentials

    # Save original values
    orig_pk = os.environ.get("UNSANDBOX_PUBLIC_KEY")
    orig_sk = os.environ.get("UNSANDBOX_SECRET_KEY")

    try:
        os.environ["UNSANDBOX_PUBLIC_KEY"] = "env_pk"
        os.environ["UNSANDBOX_SECRET_KEY"] = "env_sk"

        pk, sk = _resolve_credentials()
        assert pk == "env_pk"
        assert sk == "env_sk"
    finally:
        # Restore original values
        if orig_pk is not None:
            os.environ["UNSANDBOX_PUBLIC_KEY"] = orig_pk
        else:
            os.environ.pop("UNSANDBOX_PUBLIC_KEY", None)

        if orig_sk is not None:
            os.environ["UNSANDBOX_SECRET_KEY"] = orig_sk
        else:
            os.environ.pop("UNSANDBOX_SECRET_KEY", None)


def test_credentials_from_csv_file():
    """Test loading credentials from CSV file"""
    from un import _load_credentials_from_csv

    with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
        f.write("pk1,sk1\n")
        f.write("pk2,sk2\n")
        temp_path = f.name

    try:
        # Load first account
        pk, sk = _load_credentials_from_csv(Path(temp_path), 0)
        assert pk == "pk1"
        assert sk == "sk1"

        # Load second account
        pk, sk = _load_credentials_from_csv(Path(temp_path), 1)
        assert pk == "pk2"
        assert sk == "sk2"
    finally:
        os.unlink(temp_path)


def test_credentials_csv_with_comments():
    """Test that CSV loader ignores comments"""
    from un import _load_credentials_from_csv

    with tempfile.NamedTemporaryFile(mode="w", suffix=".csv", delete=False) as f:
        f.write("# Comment line\n")
        f.write("pk1,sk1\n")
        f.write("\n")  # Empty line
        f.write("pk2,sk2\n")
        temp_path = f.name

    try:
        pk, sk = _load_credentials_from_csv(Path(temp_path), 0)
        assert pk == "pk1"
        assert sk == "sk1"
    finally:
        os.unlink(temp_path)


def test_credentials_csv_nonexistent_file():
    """Test that nonexistent CSV returns None"""
    from un import _load_credentials_from_csv

    result = _load_credentials_from_csv(Path("/nonexistent/file.csv"))
    assert result is None


def test_credentials_missing_all(tmp_path, monkeypatch):
    """Test that CredentialsError is raised when no credentials available"""
    from un import _resolve_credentials
    import un

    monkeypatch.delenv("UNSANDBOX_PUBLIC_KEY", raising=False)
    monkeypatch.delenv("UNSANDBOX_SECRET_KEY", raising=False)
    # Point _get_unsandbox_dir to empty tmp dir so CSV lookup finds nothing
    monkeypatch.setattr(un, "_get_unsandbox_dir", lambda: tmp_path)
    # Also run from tmp_path so ./accounts.csv doesn't exist
    monkeypatch.chdir(tmp_path)

    with pytest.raises(CredentialsError):
        _resolve_credentials()
