"""Real-world scenario tests demonstrating SDK usage patterns"""

from unittest.mock import patch, MagicMock
import pytest
from un import (
    execute_code,
    execute_async,
    get_job,
    wait_for_job,
    cancel_job,
    list_jobs,
    get_languages,
    detect_language,
)


class TestRealWorldScenarios:
    """Test real-world usage patterns"""

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_fibonacci_calculation(self, mock_creds, mock_post):
        """Test calculating Fibonacci number"""
        mock_creds.return_value = ("pk_test", "sk_test")

        code = """
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

print(fib(10))
"""

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "fib_123",
            "status": "completed",
            "stdout": "55\n",
            "stderr": "",
            "exit_code": 0,
            "runtime_ms": 250,
        }
        mock_post.return_value = mock_response

        result = execute_code("python", code)

        assert result["status"] == "completed"
        assert "55" in result["stdout"]
        assert result["exit_code"] == 0

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_json_processing(self, mock_creds, mock_post):
        """Test JSON processing use case"""
        mock_creds.return_value = ("pk_test", "sk_test")

        code = """
import json

data = {"name": "Alice", "age": 30}
json_str = json.dumps(data)
print(json_str)
"""

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "json_123",
            "status": "completed",
            "stdout": '{"name": "Alice", "age": 30}\n',
            "stderr": "",
            "exit_code": 0,
            "runtime_ms": 180,
        }
        mock_post.return_value = mock_response

        result = execute_code("python", code)

        assert result["status"] == "completed"
        assert "Alice" in result["stdout"]

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_multiple_language_execution(self, mock_creds, mock_post):
        """Test executing code in different languages"""
        mock_creds.return_value = ("pk_test", "sk_test")

        test_cases = [
            ("python", "print('hello')", "hello"),
            ("javascript", "console.log('world')", "world"),
            ("go", 'fmt.Println("go")', "go"),
        ]

        for language, code, expected_output in test_cases:
            mock_response = MagicMock()
            mock_response.json.return_value = {
                "job_id": f"job_{language}",
                "status": "completed",
                "stdout": f"{expected_output}\n",
                "stderr": "",
                "exit_code": 0,
                "runtime_ms": 200,
            }
            mock_post.return_value = mock_response

            result = execute_code(language, code)

            assert result["status"] == "completed"
            assert expected_output in result["stdout"]

    @patch("un.requests.post")
    @patch("un.requests.get")
    @patch("un.time.sleep")
    @patch("un._resolve_credentials")
    def test_long_running_job_with_polling(self, mock_creds, mock_sleep, mock_get, mock_post):
        """Test handling long-running jobs with polling"""
        mock_creds.return_value = ("pk_test", "sk_test")

        # Start job
        start_response = MagicMock()
        start_response.json.return_value = {
            "job_id": "long_job_123",
            "status": "pending",
        }
        mock_post.return_value = start_response

        job_id = execute_async("python", "import time; time.sleep(10); print('done')")
        assert job_id == "long_job_123"

        # Poll for completion - simulate 3 polls before completion
        poll_responses = [
            MagicMock(json=MagicMock(return_value={"status": "pending"})),
            MagicMock(json=MagicMock(return_value={"status": "running"})),
            MagicMock(
                json=MagicMock(
                    return_value={
                        "status": "completed",
                        "stdout": "done\n",
                        "exit_code": 0,
                    }
                )
            ),
        ]
        mock_get.side_effect = poll_responses

        result = wait_for_job(job_id)

        assert result["status"] == "completed"
        assert result["stdout"] == "done\n"

    @patch("un.requests.post")
    @patch("un.requests.delete")
    @patch("un._resolve_credentials")
    def test_cancel_long_running_job(self, mock_creds, mock_delete, mock_post):
        """Test cancelling a job that takes too long"""
        mock_creds.return_value = ("pk_test", "sk_test")

        # Start job
        start_response = MagicMock()
        start_response.json.return_value = {
            "job_id": "cancel_me_123",
            "status": "pending",
        }
        mock_post.return_value = start_response

        job_id = execute_async("python", "while True: pass")

        # Cancel it
        cancel_response = MagicMock()
        cancel_response.json.return_value = {
            "job_id": job_id,
            "status": "cancelled",
        }
        mock_delete.return_value = cancel_response

        result = cancel_job(job_id)

        assert result["status"] == "cancelled"

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_error_handling_compilation_error(self, mock_creds, mock_get):
        """Test handling of compilation errors"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "error_123",
            "status": "failed",
            "stdout": "",
            "stderr": "SyntaxError: invalid syntax",
            "exit_code": 1,
            "runtime_ms": 100,
        }
        mock_get.return_value = mock_response

        result = get_job("error_123")

        assert result["status"] == "failed"
        assert result["exit_code"] == 1
        assert "SyntaxError" in result["stderr"]

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_error_handling_timeout(self, mock_creds, mock_get):
        """Test handling of execution timeouts"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "timeout_123",
            "status": "timeout",
            "stdout": "",
            "stderr": "Execution timeout: 30 seconds exceeded",
            "exit_code": 124,
            "runtime_ms": 30000,
        }
        mock_get.return_value = mock_response

        result = get_job("timeout_123")

        assert result["status"] == "timeout"
        assert result["exit_code"] == 124

    @patch("un.requests.post")
    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_batch_job_execution(self, mock_creds, mock_get, mock_post):
        """Test executing multiple jobs in batch"""
        mock_creds.return_value = ("pk_test", "sk_test")

        # Start 3 jobs
        job_ids = []
        for i in range(3):
            start_response = MagicMock()
            start_response.json.return_value = {
                "job_id": f"batch_job_{i}",
                "status": "pending",
            }
            mock_post.return_value = start_response
            job_id = execute_async("python", f"print({i})")
            job_ids.append(job_id)

        assert len(job_ids) == 3

        # Check all jobs
        for job_id in job_ids:
            get_response = MagicMock()
            get_response.json.return_value = {
                "job_id": job_id,
                "status": "completed",
                "stdout": "output\n",
                "exit_code": 0,
            }
            mock_get.return_value = get_response

            result = get_job(job_id)
            assert result["status"] == "completed"

    def test_language_auto_detection_workflow(self):
        """Test workflow using language auto-detection"""
        test_files = {
            "script.py": "python",
            "app.js": "javascript",
            "main.go": "go",
            "hello.rs": "rust",
            "unknown.xyz": None,
        }

        for filename, expected_lang in test_files.items():
            detected = detect_language(filename)
            assert detected == expected_lang

    @patch("un._load_languages_cache")
    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_get_available_languages(self, mock_creds, mock_get, mock_load_cache):
        """Test getting list of available languages"""
        mock_creds.return_value = ("pk_test", "sk_test")
        mock_load_cache.return_value = None

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "languages": [
                "python",
                "javascript",
                "go",
                "rust",
                "java",
                "cpp",
            ]
        }
        mock_get.return_value = mock_response

        languages = get_languages()

        assert len(languages) >= 6
        assert "python" in languages
        assert "javascript" in languages

    @patch("un.requests.get")
    @patch("un._resolve_credentials")
    def test_list_multiple_jobs(self, mock_creds, mock_get):
        """Test listing multiple jobs in different states"""
        mock_creds.return_value = ("pk_test", "sk_test")

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "jobs": [
                {"job_id": "job_1", "status": "completed", "runtime_ms": 150},
                {"job_id": "job_2", "status": "running", "runtime_ms": 5000},
                {"job_id": "job_3", "status": "pending", "runtime_ms": 0},
                {"job_id": "job_4", "status": "failed", "runtime_ms": 200},
            ]
        }
        mock_get.return_value = mock_response

        jobs = list_jobs()

        assert len(jobs) == 4
        assert jobs[0]["status"] == "completed"
        assert jobs[1]["status"] == "running"
        assert jobs[2]["status"] == "pending"
        assert jobs[3]["status"] == "failed"

    @patch("un.requests.post")
    @patch("un._resolve_credentials")
    def test_scientific_computation(self, mock_creds, mock_post):
        """Test scientific computation workflow"""
        mock_creds.return_value = ("pk_test", "sk_test")

        code = """
import math

# Calculate pi using Machin's formula
def compute_pi(iterations):
    result = 0.0
    for k in range(iterations):
        result += ((-1)**k) / (2*k + 1)
    return 4 * result

pi_approx = compute_pi(10000)
print(f"pi ≈ {pi_approx}")
"""

        mock_response = MagicMock()
        mock_response.json.return_value = {
            "job_id": "pi_123",
            "status": "completed",
            "stdout": "pi ≈ 3.1415\n",
            "stderr": "",
            "exit_code": 0,
            "runtime_ms": 500,
        }
        mock_post.return_value = mock_response

        result = execute_code("python", code)

        assert result["status"] == "completed"
        assert "pi" in result["stdout"].lower()
