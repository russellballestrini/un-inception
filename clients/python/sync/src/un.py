"""
PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

unsandbox.com Python SDK (Synchronous)

Library Usage:
    from un import (
        execute_code,
        execute_async,
        get_job,
        wait_for_job,
        cancel_job,
        list_jobs,
        get_languages,
        detect_language,
        session_snapshot,
        service_snapshot,
        list_snapshots,
        restore_snapshot,
        delete_snapshot,
    )

    # Execute code synchronously
    result = execute_code("python", 'print("hello")', public_key, secret_key)

    # Execute asynchronously
    job_id = execute_async("javascript", 'console.log("hello")', public_key, secret_key)

    # Wait for job completion with exponential backoff
    result = wait_for_job(job_id, public_key, secret_key)

    # List all jobs
    jobs = list_jobs(public_key, secret_key)

    # Get supported languages
    languages = get_languages(public_key, secret_key)

    # Detect language from filename
    lang = detect_language("script.py")  # Returns "python"

    # Snapshot operations (NEW)
    snapshot_id = session_snapshot(session_id, public_key, secret_key)
    snapshots = list_snapshots(public_key, secret_key)
    result = restore_snapshot(snapshot_id, public_key, secret_key)
    delete_snapshot(snapshot_id, public_key, secret_key)

Authentication Priority (4-tier):
    1. Function arguments (public_key, secret_key)
    2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
    4. Local directory (./accounts.csv, line 0 by default)

    Format: public_key,secret_key (one per line)
    Account selection: UNSANDBOX_ACCOUNT=N env var (0-based index)

Request Authentication (HMAC-SHA256):
    Authorization: Bearer <public_key>                  (identifies account)
    X-Timestamp: <unix_seconds>                         (replay prevention)
    X-Signature: HMAC-SHA256(secret_key, msg)           (proves secret + body integrity)

    Message format: "timestamp:METHOD:path:body"
    - timestamp: seconds since epoch
    - METHOD: GET, POST, DELETE, etc. (uppercase)
    - path: e.g., "/execute", "/jobs/123"
    - body: JSON payload (empty string for GET/DELETE)

Languages Cache:
    - Cached in ~/.unsandbox/languages.json
    - TTL: 1 hour
    - Updated on successful API calls
"""

import hashlib
import hmac
import json
import os
import time
import requests
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Dict, Any, List


API_BASE = "https://api.unsandbox.com"
POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000]
LANGUAGES_CACHE_TTL = 3600  # 1 hour


class CredentialsError(Exception):
    """Raised when credentials cannot be found or are invalid."""
    pass


def _get_unsandbox_dir() -> Path:
    """Get ~/.unsandbox directory path, creating if necessary."""
    home = Path.home()
    unsandbox_dir = home / ".unsandbox"
    unsandbox_dir.mkdir(exist_ok=True, mode=0o700)
    return unsandbox_dir


def _load_credentials_from_csv(csv_path: Path, account_index: int = 0) -> Optional[tuple[str, str]]:
    """Load credentials from CSV file (public_key,secret_key per line)."""
    if not csv_path.exists():
        return None

    try:
        with open(csv_path, "r") as f:
            for i, line in enumerate(f):
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                if i == account_index:
                    parts = line.split(",")
                    if len(parts) >= 2:
                        return (parts[0].strip(), parts[1].strip())
        return None
    except Exception:
        return None


def _resolve_credentials(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    account_index: Optional[int] = None,
) -> tuple[str, str]:
    """
    Resolve credentials from 4-tier priority system.

    Priority:
        1. Function arguments
        2. Environment variables
        3. ~/.unsandbox/accounts.csv
        4. ./accounts.csv
    """
    # Tier 1: Function arguments
    if public_key and secret_key:
        return (public_key, secret_key)

    # Tier 2: Environment variables
    env_pk = os.environ.get("UNSANDBOX_PUBLIC_KEY")
    env_sk = os.environ.get("UNSANDBOX_SECRET_KEY")
    if env_pk and env_sk:
        return (env_pk, env_sk)

    # Determine account index
    if account_index is None:
        account_index = int(os.environ.get("UNSANDBOX_ACCOUNT", "0"))

    # Tier 3: ~/.unsandbox/accounts.csv
    unsandbox_dir = _get_unsandbox_dir()
    creds = _load_credentials_from_csv(unsandbox_dir / "accounts.csv", account_index)
    if creds:
        return creds

    # Tier 4: ./accounts.csv
    creds = _load_credentials_from_csv(Path("accounts.csv"), account_index)
    if creds:
        return creds

    raise CredentialsError(
        "No credentials found. Please provide via:\n"
        "  1. Function arguments (public_key, secret_key)\n"
        "  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n"
        "  3. ~/.unsandbox/accounts.csv\n"
        "  4. ./accounts.csv"
    )


def _sign_request(
    secret_key: str,
    timestamp: int,
    method: str,
    path: str,
    body: Optional[str] = None,
) -> str:
    """
    Sign a request using HMAC-SHA256.

    Message format: "timestamp:METHOD:path:body"
    Returns: 64-character hex string
    """
    body_str = body or ""
    message = f"{timestamp}:{method}:{path}:{body_str}"
    signature = hmac.new(
        secret_key.encode(),
        message.encode(),
        hashlib.sha256,
    ).hexdigest()
    return signature


def _make_request(
    method: str,
    path: str,
    public_key: str,
    secret_key: str,
    data: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Make an authenticated HTTP request to the API.

    Raises requests.RequestException on network errors.
    Raises ValueError if response is not valid JSON.
    """
    url = f"{API_BASE}{path}"
    timestamp = int(time.time())
    body = json.dumps(data) if data else ""

    signature = _sign_request(secret_key, timestamp, method, path, body if data else None)

    headers = {
        "Authorization": f"Bearer {public_key}",
        "X-Timestamp": str(timestamp),
        "X-Signature": signature,
        "Content-Type": "application/json",
    }

    if method == "GET":
        response = requests.get(url, headers=headers, timeout=120)
    elif method == "POST":
        response = requests.post(url, headers=headers, json=data, timeout=120)
    elif method == "DELETE":
        response = requests.delete(url, headers=headers, timeout=120)
    else:
        raise ValueError(f"Unsupported HTTP method: {method}")

    response.raise_for_status()
    return response.json()


def _get_languages_cache_path() -> Path:
    """Get path to languages cache file."""
    return _get_unsandbox_dir() / "languages.json"


def _load_languages_cache() -> Optional[List[str]]:
    """Load languages from cache if valid (< 1 hour old)."""
    cache_path = _get_languages_cache_path()
    if not cache_path.exists():
        return None

    try:
        with open(cache_path, "r") as f:
            data = json.load(f)

        # Check if cache is fresh
        mtime = cache_path.stat().st_mtime
        age_seconds = time.time() - mtime
        if age_seconds < LANGUAGES_CACHE_TTL:
            return data.get("languages")
    except Exception:
        pass

    return None


def _save_languages_cache(languages: List[str]) -> None:
    """Save languages to cache."""
    try:
        cache_path = _get_languages_cache_path()
        with open(cache_path, "w") as f:
            json.dump({"languages": languages, "timestamp": int(time.time())}, f)
    except Exception:
        pass  # Cache failures are non-fatal


def execute_code(
    language: str,
    code: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Execute code synchronously (blocks until completion).

    Args:
        language: Programming language (e.g., "python", "javascript", "go")
        code: Source code to execute
        public_key: Optional API key (uses credentials resolution if not provided)
        secret_key: Optional API secret (uses credentials resolution if not provided)

    Returns:
        Response dict containing stdout, stderr, exit code, etc.

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = _make_request(
        "POST",
        "/execute",
        public_key,
        secret_key,
        {"language": language, "code": code},
    )

    # If we got a job_id, poll until completion
    job_id = response.get("job_id")
    status = response.get("status")

    if job_id and status in ("pending", "running"):
        return wait_for_job(job_id, public_key, secret_key)

    return response


def execute_async(
    language: str,
    code: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> str:
    """
    Execute code asynchronously (returns immediately with job_id).

    Args:
        language: Programming language (e.g., "python", "javascript")
        code: Source code to execute
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Job ID string

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = _make_request(
        "POST",
        "/execute",
        public_key,
        secret_key,
        {"language": language, "code": code},
    )
    return response.get("job_id")


def get_job(
    job_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Get current status/result of a job (single poll, no waiting).

    Args:
        job_id: Job ID from execute_async()
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Job response dict

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return _make_request("GET", f"/jobs/{job_id}", public_key, secret_key)


def wait_for_job(
    job_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Wait for job completion with exponential backoff polling.

    Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
    Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+

    Args:
        job_id: Job ID from execute_async()
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Final job result when status is terminal (completed, failed, timeout, cancelled)

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    poll_count = 0

    while True:
        # Sleep before polling
        delay_idx = min(poll_count, len(POLL_DELAYS_MS) - 1)
        time.sleep(POLL_DELAYS_MS[delay_idx] / 1000.0)
        poll_count += 1

        response = get_job(job_id, public_key, secret_key)
        status = response.get("status")

        if status in ("completed", "failed", "timeout", "cancelled"):
            return response

        # Still running, continue polling


def cancel_job(
    job_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Cancel a running job.

    Args:
        job_id: Job ID to cancel
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with cancellation confirmation

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return _make_request("DELETE", f"/jobs/{job_id}", public_key, secret_key)


def list_jobs(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """
    List all jobs for the authenticated account.

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        List of job dicts

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = _make_request("GET", "/jobs", public_key, secret_key)
    return response.get("jobs", [])


def get_languages(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> List[str]:
    """
    Get list of supported programming languages.

    Results are cached for 1 hour in ~/.unsandbox/languages.json

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        List of language identifiers (e.g., ["python", "javascript", "go", ...])

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    # Try cache first
    cached = _load_languages_cache()
    if cached:
        return cached

    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = _make_request("GET", "/languages", public_key, secret_key)
    languages = response.get("languages", [])

    # Cache the result
    _save_languages_cache(languages)
    return languages


# Language detection mapping (file extension -> language)
_LANGUAGE_MAP = {
    "py": "python",
    "js": "javascript",
    "ts": "typescript",
    "rb": "ruby",
    "php": "php",
    "pl": "perl",
    "sh": "bash",
    "r": "r",
    "R": "r",
    "lua": "lua",
    "go": "go",
    "rs": "rust",
    "c": "c",
    "cpp": "cpp",
    "cc": "cpp",
    "cxx": "cpp",
    "java": "java",
    "kt": "kotlin",
    "m": "objc",
    "cs": "csharp",
    "fs": "fsharp",
    "hs": "haskell",
    "ml": "ocaml",
    "clj": "clojure",
    "scm": "scheme",
    "ss": "scheme",
    "erl": "erlang",
    "ex": "elixir",
    "exs": "elixir",
    "jl": "julia",
    "d": "d",
    "nim": "nim",
    "zig": "zig",
    "v": "v",
    "cr": "crystal",
    "dart": "dart",
    "groovy": "groovy",
    "f90": "fortran",
    "f95": "fortran",
    "lisp": "commonlisp",
    "lsp": "commonlisp",
    "cob": "cobol",
    "tcl": "tcl",
    "raku": "raku",
    "pro": "prolog",
    "p": "prolog",
    "4th": "forth",
    "forth": "forth",
    "fth": "forth",
}


def detect_language(filename: str) -> Optional[str]:
    """
    Detect programming language from filename extension.

    Args:
        filename: Filename to detect language from (e.g., "script.py")

    Returns:
        Language identifier (e.g., "python") or None if unknown

    Examples:
        detect_language("hello.py")   # -> "python"
        detect_language("script.js")  # -> "javascript"
        detect_language("main.go")    # -> "go"
        detect_language("unknown")    # -> None
    """
    if not filename or "." not in filename:
        return None

    ext = filename.rsplit(".", 1)[-1].lower()
    return _LANGUAGE_MAP.get(ext)


def session_snapshot(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    name: Optional[str] = None,
    hot: bool = False,
) -> str:
    """
    Create a snapshot of a session (NEW).

    Args:
        session_id: Session ID to snapshot
        public_key: Optional API key
        secret_key: Optional API secret
        name: Optional snapshot name
        hot: If True, snapshot running session (hot snapshot)

    Returns:
        Snapshot ID

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data = {"session_id": session_id, "hot": hot}
    if name:
        data["name"] = name

    response = _make_request("POST", "/snapshots", public_key, secret_key, data)
    return response.get("snapshot_id")


def service_snapshot(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    name: Optional[str] = None,
    hot: bool = False,
) -> str:
    """
    Create a snapshot of a service (NEW).

    Args:
        service_id: Service ID to snapshot
        public_key: Optional API key
        secret_key: Optional API secret
        name: Optional snapshot name
        hot: If True, snapshot running service (hot snapshot)

    Returns:
        Snapshot ID

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data = {"service_id": service_id, "hot": hot}
    if name:
        data["name"] = name

    response = _make_request("POST", "/snapshots", public_key, secret_key, data)
    return response.get("snapshot_id")


def list_snapshots(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """
    List all snapshots (NEW).

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        List of snapshot dicts

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = _make_request("GET", "/snapshots", public_key, secret_key)
    return response.get("snapshots", [])


def restore_snapshot(
    snapshot_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Restore a snapshot (NEW).

    Args:
        snapshot_id: Snapshot ID to restore
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with restored resource info

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return _make_request("POST", f"/snapshots/{snapshot_id}/restore", public_key, secret_key, {})


def delete_snapshot(
    snapshot_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Delete a snapshot (NEW).

    Args:
        snapshot_id: Snapshot ID to delete
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with deletion confirmation

    Raises:
        requests.RequestException: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return _make_request("DELETE", f"/snapshots/{snapshot_id}", public_key, secret_key)
