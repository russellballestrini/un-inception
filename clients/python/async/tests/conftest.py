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
Pytest configuration for async tests
"""

import pytest
import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))


@pytest.fixture
def mock_api_key():
    """Provide a mock API key for testing."""
    return "test_public_key"


@pytest.fixture
def mock_secret_key():
    """Provide a mock secret key for testing."""
    return "test_secret_key"


@pytest.fixture
def mock_job_id():
    """Provide a mock job ID for testing."""
    return "job_12345_abcde"


@pytest.fixture
def mock_code():
    """Provide mock code for testing."""
    return 'print("Hello World")'


@pytest.fixture
def mock_result():
    """Provide a mock API result."""
    return {
        "job_id": "job_12345",
        "status": "completed",
        "stdout": "Hello World\n",
        "stderr": "",
        "exit_code": 0,
        "language": "python",
        "duration_ms": 120,
    }
