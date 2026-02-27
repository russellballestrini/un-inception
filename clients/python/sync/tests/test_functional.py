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
UN Python SDK - Functional Tests

Tests library functions against real API.
Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY

Usage:
    cd clients/python/sync && PYTHONPATH=src pytest tests/test_functional.py -v
"""

import os
import sys
import pytest

# Import SDK functions
from un import (
    execute_code,
    get_languages,
    list_sessions,
    list_services,
    list_snapshots,
    list_images,
    validate_keys,
    health_check,
)


# Skip all tests if no credentials
pytestmark = pytest.mark.skipif(
    not os.environ.get("UNSANDBOX_PUBLIC_KEY") or not os.environ.get("UNSANDBOX_SECRET_KEY"),
    reason="UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required",
)


class TestFunctional:
    """Functional tests against real API (matches C SDK test_functional.c)"""

    def test_health_check(self):
        """API health check returns boolean"""
        result = health_check()
        assert isinstance(result, bool)

    def test_validate_keys(self):
        """API keys validate successfully"""
        info = validate_keys()
        assert isinstance(info, dict)
        assert "valid" in info
        assert info["valid"] is True

    def test_get_languages(self):
        """Languages endpoint returns list with python"""
        langs = get_languages()
        assert isinstance(langs, list)
        assert len(langs) > 0
        assert "python" in langs

    def test_execute(self):
        """Execute python code and verify stdout"""
        result = execute_code("python", "print('hello from Python SDK')")
        assert isinstance(result, dict)
        assert result.get("success") is True
        assert "hello from Python SDK" in result.get("stdout", "")
        assert result.get("exit_code") == 0

    def test_execute_error(self):
        """Execute code that exits with error"""
        result = execute_code("python", "import sys; sys.exit(1)")
        assert isinstance(result, dict)
        assert result.get("success") is False
        assert result.get("exit_code") == 1

    def test_session_list(self):
        """List sessions returns list"""
        sessions = list_sessions()
        assert isinstance(sessions, list)

    def test_session_lifecycle(self):
        """Create and destroy a session"""
        from un import create_session, delete_session

        session = create_session("python")
        assert isinstance(session, dict)
        assert "id" in session
        session_id = session["id"]

        destroyed = delete_session(session_id)
        assert destroyed is not None

    def test_service_list(self):
        """List services returns list"""
        services = list_services()
        assert isinstance(services, list)

    def test_snapshot_list(self):
        """List snapshots returns list"""
        snapshots = list_snapshots()
        assert isinstance(snapshots, list)

    def test_image_list(self):
        """List images returns list"""
        images = list_images()
        assert isinstance(images, list)
