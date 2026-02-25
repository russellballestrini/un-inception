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
Tests for async operations (requires mocking or real API access)
"""

import pytest
import asyncio
import sys
import os
from unittest.mock import AsyncMock, patch, MagicMock

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

import un_async
from un_async import (
    execute_code,
    execute_async,
    get_job,
    wait_for_job,
    cancel_job,
    list_jobs,
)


@pytest.fixture
def mock_credentials(monkeypatch):
    """Provide mock credentials."""
    monkeypatch.setenv("UNSANDBOX_PUBLIC_KEY", "test_public_key")
    monkeypatch.setenv("UNSANDBOX_SECRET_KEY", "test_secret_key")


class TestAsyncOperations:
    """Test async API operations."""

    @pytest.mark.asyncio
    async def test_execute_code_completed(self, mock_credentials):
        """Test execute_code when job completes immediately."""
        mock_response = {
            "job_id": "job_123",
            "status": "completed",
            "stdout": "Hello World\n",
            "stderr": "",
            "exit_code": 0,
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            # First call for execute, then wait_for_job polls
            mock_req.side_effect = [
                mock_response,  # execute_code calls _make_request
            ]

            result = await execute_code("python", "print('Hello World')")

            assert result["status"] == "completed"
            assert "Hello World" in result["stdout"]
            assert result["exit_code"] == 0

    @pytest.mark.asyncio
    async def test_execute_async_returns_job_id(self, mock_credentials):
        """Test execute_async returns job_id immediately."""
        mock_response = {
            "job_id": "job_456",
            "status": "pending",
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.return_value = mock_response

            job_id = await execute_async("python", "print('test')")

            assert job_id == "job_456"
            mock_req.assert_called_once()

    @pytest.mark.asyncio
    async def test_get_job(self, mock_credentials):
        """Test getting job status."""
        mock_response = {
            "job_id": "job_789",
            "status": "running",
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.return_value = mock_response

            result = await get_job("job_789")

            assert result["job_id"] == "job_789"
            assert result["status"] == "running"

    @pytest.mark.asyncio
    async def test_wait_for_job_polling(self, mock_credentials):
        """Test wait_for_job polls until completion."""
        # Simulate polling: pending -> running -> completed
        mock_responses = [
            {"job_id": "job_100", "status": "pending"},
            {"job_id": "job_100", "status": "running"},
            {"job_id": "job_100", "status": "completed", "stdout": "done"},
        ]

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.side_effect = mock_responses

            with patch("asyncio.sleep", new_callable=AsyncMock):
                result = await wait_for_job("job_100")

            assert result["status"] == "completed"
            assert result["stdout"] == "done"
            assert mock_req.call_count == 3

    @pytest.mark.asyncio
    async def test_cancel_job(self, mock_credentials):
        """Test cancelling a job."""
        mock_response = {
            "job_id": "job_kill",
            "status": "cancelled",
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.return_value = mock_response

            result = await cancel_job("job_kill")

            assert result["status"] == "cancelled"

    @pytest.mark.asyncio
    async def test_list_jobs(self, mock_credentials):
        """Test listing all jobs."""
        mock_response = {
            "jobs": [
                {"id": "job_1", "status": "completed"},
                {"id": "job_2", "status": "running"},
            ]
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.return_value = mock_response

            jobs = await list_jobs()

            assert len(jobs) == 2
            assert jobs[0]["id"] == "job_1"
            assert jobs[1]["id"] == "job_2"


class TestAsyncConcurrency:
    """Test concurrent async operations."""

    @pytest.mark.asyncio
    async def test_concurrent_executions(self, mock_credentials):
        """Test running multiple executions concurrently."""
        mock_response = {
            "job_id": "job_concurrent",
            "status": "completed",
            "stdout": "result",
        }

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.return_value = mock_response

            # Run 3 concurrent executions
            results = await asyncio.gather(
                execute_code("python", "print(1)"),
                execute_code("javascript", "console.log(2)"),
                execute_code("bash", "echo 3"),
            )

            assert len(results) == 3
            assert all(r["status"] == "completed" for r in results)

    @pytest.mark.asyncio
    async def test_concurrent_with_different_statuses(self, mock_credentials):
        """Test concurrent operations with different final statuses."""
        responses = [
            {"job_id": "1", "status": "completed", "stdout": "ok"},
            {"job_id": "2", "status": "failed", "stderr": "error"},
            {"job_id": "3", "status": "timeout"},
        ]

        with patch("un_async._make_request", new_callable=AsyncMock) as mock_req:
            mock_req.side_effect = responses

            results = await asyncio.gather(
                execute_code("python", "print(1)"),
                execute_code("python", "raise Exception()"),
                execute_code("python", "while True: pass"),
            )

            assert results[0]["status"] == "completed"
            assert results[1]["status"] == "failed"
            assert results[2]["status"] == "timeout"


class TestAsyncSignature:
    """Test that functions have async signatures."""

    def test_functions_are_coroutines(self):
        """Verify key functions are async."""
        import inspect

        # These should be coroutine functions
        assert inspect.iscoroutinefunction(execute_code)
        assert inspect.iscoroutinefunction(execute_async)
        assert inspect.iscoroutinefunction(get_job)
        assert inspect.iscoroutinefunction(wait_for_job)
        assert inspect.iscoroutinefunction(cancel_job)
        assert inspect.iscoroutinefunction(list_jobs)

        # These should NOT be coroutine functions (sync helpers)
        assert not inspect.iscoroutinefunction(un_async.detect_language)
        assert not inspect.iscoroutinefunction(un_async._resolve_credentials)
