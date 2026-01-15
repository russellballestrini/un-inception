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
