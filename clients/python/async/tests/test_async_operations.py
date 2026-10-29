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


class _FakeResponse:
    def __init__(self, payload, status=200):
        self._payload = payload
        self.status = status
        self.url = "http://test.invalid/"
        self.headers = {"Content-Type": "application/json"}

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc):
        return False

    async def text(self):
        import json

        return json.dumps(self._payload)


class _FakeSession:
    """Stands in for aiohttp.ClientSession; records every request it is asked to make."""

    calls = []

    def __init__(self, *args, **kwargs):
        pass

    async def __aenter__(self):
        return self

    async def __aexit__(self, *exc):
        return False

    def request(self, method, url, **kwargs):
        _FakeSession.calls.append((method, url, kwargs))
        return _FakeResponse({"success": True, "count": 2})


class TestServiceEnvRequest:
    """set_service_env must PUT /services/{id}/env with JSON {"env": {...}}"""

    @pytest.mark.asyncio
    async def test_set_service_env_puts_json_map(self):
        import hashlib
        import hmac
        import json

        _FakeSession.calls = []
        with patch.object(un_async.aiohttp, "ClientSession", _FakeSession), patch(
            "un_async.time.time", return_value=1700000000
        ):
            result = await un_async.set_service_env(
                "unsb-service-abc", {"API_KEY": "x", "DEBUG": "true"}, "pk_test", "sk_test"
            )

        assert result == {"success": True, "count": 2}
        assert len(_FakeSession.calls) == 1
        method, url, kwargs = _FakeSession.calls[0]
        assert method == "PUT"
        assert url == f"{un_async.API_BASE}/services/unsb-service-abc/env"
        assert kwargs["headers"]["Content-Type"] == "application/json"

        sent = kwargs["data"]
        assert json.loads(sent) == {"env": {"API_KEY": "x", "DEBUG": "true"}}

        # The signature covers exactly the bytes sent
        message = f"1700000000:PUT:/services/unsb-service-abc/env:{sent.decode('utf-8')}"
        expected = hmac.new(b"sk_test", message.encode(), hashlib.sha256).hexdigest()
        assert kwargs["headers"]["X-Signature"] == expected


class TestSignedBytesAreSentBytes:
    """_make_request must sign exactly the body it sends ({} and None included)"""

    @pytest.mark.asyncio
    @pytest.mark.parametrize("data,expected_body", [({}, b"{}"), (None, None), ({"a": 1}, b'{"a": 1}')])
    async def test_signature_verifies_against_sent_body(self, data, expected_body):
        import hashlib
        import hmac

        _FakeSession.calls = []
        with patch.object(un_async.aiohttp, "ClientSession", _FakeSession):
            await un_async._make_request("POST", "/services/unsb-service-abc/freeze", "pk_test", "sk_test", data)

        method, _url, kwargs = _FakeSession.calls[0]
        assert method == "POST"
        assert "json" not in kwargs
        sent = kwargs["data"]
        assert sent == expected_body
        ts = kwargs["headers"]["X-Timestamp"]
        message = f"{ts}:POST:/services/unsb-service-abc/freeze:{(sent or b'').decode('utf-8')}"
        expected = hmac.new(b"sk_test", message.encode(), hashlib.sha256).hexdigest()
        assert kwargs["headers"]["X-Signature"] == expected


class TestSnapshotRoutes:
    """Snapshots are made at POST /sessions/{id}/snapshot & /services/{id}/snapshot"""

    @pytest.mark.asyncio
    @pytest.mark.parametrize("kind", ["session", "service"])
    async def test_snapshot_posts_to_resource_route(self, kind):
        import json

        _FakeSession.calls = []
        make = un_async.session_snapshot if kind == "session" else un_async.service_snapshot
        with patch.object(un_async.aiohttp, "ClientSession", _FakeSession):
            await make("unsb-x-1", "pk_test", "sk_test", name="nightly", hot=True)

        method, url, kwargs = _FakeSession.calls[0]
        assert method == "POST"
        assert url == f"{un_async.API_BASE}/{kind}s/unsb-x-1/snapshot"
        assert json.loads(kwargs["data"]) == {"hot": True, "name": "nightly"}


class TestSyncAsyncParity:
    """Every public function of the sync client exists in the async client with the same parameters"""

    def test_same_public_functions_and_parameters(self):
        import inspect

        sync_dir = os.path.join(os.path.dirname(__file__), "..", "..", "sync", "src")
        sys.path.insert(0, sync_dir)
        try:
            import un
        finally:
            sys.path.remove(sync_dir)

        def public(mod):
            return {
                name: [p.name for p in inspect.signature(fn).parameters.values()]
                for name, fn in vars(mod).items()
                if inspect.isfunction(fn) and not name.startswith("_") and fn.__module__ == mod.__name__
            }

        assert public(un) == public(un_async)
