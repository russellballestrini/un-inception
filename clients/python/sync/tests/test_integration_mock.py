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

"""Integration tests with mocked API responses"""

from unittest.mock import patch, MagicMock
import json
import pytest
from un import (
    execute_code,
    execute_async,
    get_job,
    wait_for_job,
    cancel_job,
    list_jobs,
    get_languages,
)


class TestIntegrationMocked:
    """Integration tests using mocked HTTP responses"""

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_execute_code_success(self, mock_creds, mock_post):
        """Test successful synchronous code execution"""
        mock_creds.return_value = ("pk_test", "sk_test")

        # Mock the initial response
        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "job_123",
            "status": "completed",
            "stdout": "hello\n",
            "stderr": "",
            "exit_code": 0,
            "runtime_ms": 150,
        }
        mock_post.return_value = mock_response

        result = execute_code("python", "print('hello')")

        assert result["status"] == "completed"
        assert result["stdout"] == "hello\n"
        assert result["exit_code"] == 0

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_execute_async_returns_job_id(self, mock_creds, mock_post):
        """Test async execution returns job ID"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "job_456",
            "status": "pending",
        }
        mock_post.return_value = mock_response

        job_id = execute_async("javascript", "console.log('hello')")

        assert job_id == "job_456"

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_get_job(self, mock_creds, mock_get):
        """Test getting job status"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "job_789",
            "status": "running",
            "created_at": "2024-01-15T10:00:00Z",
        }
        mock_get.return_value = mock_response

        result = get_job("job_789")

        assert result["job_id"] == "job_789"
        assert result["status"] == "running"

    @patch("un.requests.get")
    @patch("un.time.sleep")
    @patch("un._resolve_credentials")
    def test_wait_for_job_completes(self, mock_creds, mock_sleep, mock_get):
        """Test waiting for job to complete"""
        mock_creds.return_value = ("pk_test", "sk_test")

        # First call: running, second call: completed
        mock_responses = [
            MagicMock(
                json=MagicMock(
                    return_value={
                        "job_id": "job_123",
                        "status": "running",
                    }
                )
            ),
            MagicMock(
                json=MagicMock(
                    return_value={
                        "job_id": "job_123",
                        "status": "completed",
                        "stdout": "result\n",
                        "exit_code": 0,
                    }
                )
            ),
        ]
        mock_get.side_effect = mock_responses

        result = wait_for_job("job_123")

        assert result["status"] == "completed"
        assert result["stdout"] == "result\n"

    @patch("un.requests.delete")
    @patch("un._resolve_credentials")
    def test_cancel_job(self, mock_creds, mock_delete):
        """Test cancelling a job"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "job_123",
            "status": "cancelled",
        }
        mock_delete.return_value = mock_response

        result = cancel_job("job_123")

        assert result["status"] == "cancelled"

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_list_jobs(self, mock_creds, mock_get):
        """Test listing jobs"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "jobs": [
                {"job_id": "job_1", "status": "completed"},
                {"job_id": "job_2", "status": "running"},
                {"job_id": "job_3", "status": "pending"},
            ]
        }
        mock_get.return_value = mock_response

        jobs = list_jobs()

        assert len(jobs) == 3
        assert jobs[0]["job_id"] == "job_1"
        assert jobs[1]["status"] == "running"

    @patch("un._load_languages_cache")
    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    @patch("un._save_languages_cache")
    def test_get_languages_uses_cache(
        self, mock_save_cache, mock_creds, mock_get, mock_load_cache
    ):
        """Test that get_languages uses cache when available"""
        mock_load_cache.return_value = ["python", "javascript", "go"]

        languages = get_languages()

        # Should use cache, not make API call
        assert languages == ["python", "javascript", "go"]
        mock_get.assert_not_called()

    @patch("un._load_languages_cache")
    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    @patch("un._save_languages_cache")
    def test_get_languages_fetches_from_api(
        self, mock_save_cache, mock_creds, mock_get, mock_load_cache
    ):
        """Test that get_languages fetches from API when cache misses"""
        mock_creds.return_value = ("pk_test", "sk_test")
        mock_load_cache.return_value = None  # Cache miss

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "languages": ["python", "javascript", "go", "rust"]
        }
        mock_get.return_value = mock_response

        languages = get_languages()

        assert len(languages) == 4
        assert "python" in languages
        mock_save_cache.assert_called_once_with(languages)

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_request_headers(self, mock_creds, mock_post):
        """Test that requests include proper authentication headers"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "job_123",
            "status": "completed",
        }
        mock_post.return_value = mock_response

        execute_async("python", "print(1)")

        # Verify headers were set
        call_kwargs = mock_post.call_args[1]
        headers = call_kwargs.get("headers", {})

        assert "Authorization" in headers
        assert headers["Authorization"].startswith("Bearer pk_test")
        assert "X-Timestamp" in headers
        assert "X-Signature" in headers
        assert "Content-Type" in headers
        assert headers["Content-Type"] == "application/json"

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_error_handling_http_error(self, mock_creds, mock_post):
        """Test error handling for HTTP errors"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.raise_for_status.side_effect = Exception("404 Not Found")
        mock_post.return_value = mock_response

        with pytest.raises(Exception):
            execute_async("python", "print(1)")

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_handle_network_timeout(self, mock_creds, mock_get):
        """Test handling of network timeouts"""
        import requests

        mock_creds.return_value = ("pk_test", "sk_test")
        mock_get.side_effect = requests.Timeout("Connection timeout")

        with pytest.raises(requests.Timeout):
            get_job("job_123")


class TestServiceEnvRequest:
    """set_service_env must PUT /services/{id}/env with JSON {"env": {...}}"""

    @patch("un.requests.post")
    @patch("un.requests.put")
    @patch("un.time.time", return_value=1700000000)
    def test_set_service_env_puts_json_map(self, _mock_time, mock_put, mock_post):
        import hashlib
        import hmac

        import un

        mock_response = MagicMock()
        mock_response.json.return_value = {"success": True, "count": 2}
        mock_put.return_value = mock_response

        result = un.set_service_env(
            "unsb-service-abc", {"API_KEY": "x", "DEBUG": "true"}, "pk_test", "sk_test"
        )

        assert result == {"success": True, "count": 2}
        mock_post.assert_not_called()
        mock_put.assert_called_once()
        url = mock_put.call_args[0][0]
        kwargs = mock_put.call_args[1]
        assert url == f"{un.API_BASE}/services/unsb-service-abc/env"
        assert kwargs["headers"]["Content-Type"] == "application/json"

        sent = kwargs["data"]
        assert json.loads(sent) == {"env": {"API_KEY": "x", "DEBUG": "true"}}

        # The signature covers exactly the bytes sent
        message = f"1700000000:PUT:/services/unsb-service-abc/env:{sent.decode('utf-8')}"
        expected = hmac.new(b"sk_test", message.encode(), hashlib.sha256).hexdigest()
        assert kwargs["headers"]["X-Signature"] == expected
        assert kwargs["headers"]["X-Timestamp"] == "1700000000"


class TestSignedBytesAreSentBytes:
    """_make_request must sign exactly the body it sends, for every method"""

    @staticmethod
    def _verify(kwargs, method, path):
        import hashlib
        import hmac

        sent = kwargs.get("data")
        sent_str = sent.decode("utf-8") if sent else ""
        ts = kwargs["headers"]["X-Timestamp"]
        message = f"{ts}:{method}:{path}:{sent_str}"
        expected = hmac.new(b"sk_test", message.encode(), hashlib.sha256).hexdigest()
        assert kwargs["headers"]["X-Signature"] == expected
        assert "json" not in kwargs
        return sent

    @pytest.mark.parametrize("method", ["POST", "PUT", "PATCH", "DELETE"])
    @pytest.mark.parametrize("data,expected_body", [({}, b"{}"), (None, None), ({"a": 1}, b'{"a": 1}')])
    def test_signature_verifies_against_sent_body(self, method, data, expected_body):
        import un

        with patch(f"un.requests.{method.lower()}") as mock_send:
            mock_send.return_value = MagicMock(**{"json.return_value": {"ok": True}})
            un._make_request(method, "/services/unsb-service-abc/freeze", "pk_test", "sk_test", data)

        kwargs = mock_send.call_args[1]
        sent = self._verify(kwargs, method, "/services/unsb-service-abc/freeze")
        assert sent == expected_body

    @patch("un.requests.post")
    def test_freeze_service_signs_empty_object(self, mock_post):
        """freeze_service passes {}: signed "{}" must equal sent "{}" (was 401)"""
        import un

        mock_post.return_value = MagicMock(**{"json.return_value": {"ok": True}})
        un.freeze_service("unsb-service-abc", "pk_test", "sk_test")

        kwargs = mock_post.call_args[1]
        assert self._verify(kwargs, "POST", "/services/unsb-service-abc/freeze") == b"{}"


class TestSnapshotRoutes:
    """Snapshots are made at POST /sessions/{id}/snapshot & /services/{id}/snapshot"""

    @pytest.mark.parametrize("kind", ["session", "service"])
    @patch("un.requests.post")
    def test_snapshot_posts_to_resource_route(self, mock_post, kind):
        import un

        mock_post.return_value = MagicMock(**{"json.return_value": {"id": "unsb-snapshot-1"}})
        make = un.session_snapshot if kind == "session" else un.service_snapshot
        snapshot_id = make("unsb-x-1", "pk_test", "sk_test", name="nightly", hot=True)

        assert snapshot_id == "unsb-snapshot-1"
        url = mock_post.call_args[0][0]
        assert url == f"{un.API_BASE}/{kind}s/unsb-x-1/snapshot"
        assert json.loads(mock_post.call_args[1]["data"]) == {"hot": True, "name": "nightly"}
