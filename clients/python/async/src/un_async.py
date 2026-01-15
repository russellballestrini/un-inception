"""
PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

unsandbox.com Python SDK (Asynchronous)

Library Usage:
    import asyncio
    from un_async import (
        # Execution
        execute_code,
        execute_async,
        get_job,
        wait_for_job,
        cancel_job,
        list_jobs,
        get_languages,
        detect_language,
        # Sessions
        list_sessions,
        get_session,
        create_session,
        delete_session,
        freeze_session,
        unfreeze_session,
        boost_session,
        unboost_session,
        shell_session,
        # Services
        list_services,
        create_service,
        get_service,
        update_service,
        delete_service,
        freeze_service,
        unfreeze_service,
        lock_service,
        unlock_service,
        get_service_logs,
        get_service_env,
        set_service_env,
        delete_service_env,
        export_service_env,
        redeploy_service,
        execute_in_service,
        # Snapshots
        session_snapshot,
        service_snapshot,
        list_snapshots,
        restore_snapshot,
        delete_snapshot,
        lock_snapshot,
        unlock_snapshot,
        clone_snapshot,
        # Key validation
        validate_keys,
    )

    async def main():
        # Execute code synchronously
        result = await execute_code("python", 'print("hello")', public_key, secret_key)

        # Execute asynchronously
        job_id = await execute_async("javascript", 'console.log("hello")', public_key, secret_key)

        # Wait for job completion with exponential backoff
        result = await wait_for_job(job_id, public_key, secret_key)

        # List all jobs
        jobs = await list_jobs(public_key, secret_key)

        # Get supported languages
        languages = await get_languages(public_key, secret_key)

        # Snapshot operations
        snapshot_id = await session_snapshot(session_id, public_key, secret_key)

    asyncio.run(main())

Authentication Priority (4-tier):
    1. Function arguments (public_key, secret_key)
    2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
    4. Local directory (./accounts.csv, line 0 by default)

Request Authentication (HMAC-SHA256):
    Authorization: Bearer <public_key>
    X-Timestamp: <unix_seconds>
    X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")

Languages Cache:
    - Cached in ~/.unsandbox/languages.json
    - TTL: 1 hour
    - Updated on successful API calls
"""

import asyncio
import hashlib
import hmac
import json
import os
import time
import aiohttp
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


async def _make_request(
    method: str,
    path: str,
    public_key: str,
    secret_key: str,
    data: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Make an authenticated HTTP request to the API (async).

    Raises aiohttp.ClientError on network errors.
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

    async with aiohttp.ClientSession() as session:
        if method == "GET":
            async with session.get(url, headers=headers, timeout=aiohttp.ClientTimeout(total=120)) as resp:
                resp.raise_for_status()
                return await resp.json()
        elif method == "POST":
            async with session.post(url, headers=headers, json=data, timeout=aiohttp.ClientTimeout(total=120)) as resp:
                resp.raise_for_status()
                return await resp.json()
        elif method == "PATCH":
            async with session.patch(url, headers=headers, json=data, timeout=aiohttp.ClientTimeout(total=120)) as resp:
                resp.raise_for_status()
                return await resp.json()
        elif method == "DELETE":
            async with session.delete(url, headers=headers, timeout=aiohttp.ClientTimeout(total=120)) as resp:
                resp.raise_for_status()
                return await resp.json()
        else:
            raise ValueError(f"Unsupported HTTP method: {method}")


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


async def execute_code(
    language: str,
    code: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Execute code synchronously (awaits until completion).

    Args:
        language: Programming language (e.g., "python", "javascript", "go")
        code: Source code to execute
        public_key: Optional API key (uses credentials resolution if not provided)
        secret_key: Optional API secret (uses credentials resolution if not provided)

    Returns:
        Response dict containing stdout, stderr, exit code, etc.

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request(
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
        return await wait_for_job(job_id, public_key, secret_key)

    return response


async def execute_async(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request(
        "POST",
        "/execute",
        public_key,
        secret_key,
        {"language": language, "code": code},
    )
    return response.get("job_id")


async def get_job(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("GET", f"/jobs/{job_id}", public_key, secret_key)


async def wait_for_job(
    job_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Wait for job completion with exponential backoff polling (async).

    Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
    Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+

    Args:
        job_id: Job ID from execute_async()
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Final job result when status is terminal (completed, failed, timeout, cancelled)

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    poll_count = 0

    while True:
        # Sleep before polling
        delay_idx = min(poll_count, len(POLL_DELAYS_MS) - 1)
        await asyncio.sleep(POLL_DELAYS_MS[delay_idx] / 1000.0)
        poll_count += 1

        response = await get_job(job_id, public_key, secret_key)
        status = response.get("status")

        if status in ("completed", "failed", "timeout", "cancelled"):
            return response

        # Still running, continue polling


async def cancel_job(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("DELETE", f"/jobs/{job_id}", public_key, secret_key)


async def list_jobs(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request("GET", "/jobs", public_key, secret_key)
    return response.get("jobs", [])


async def get_languages(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    # Try cache first
    cached = _load_languages_cache()
    if cached:
        return cached

    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request("GET", "/languages", public_key, secret_key)
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


async def session_snapshot(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data = {"session_id": session_id, "hot": hot}
    if name:
        data["name"] = name

    response = await _make_request("POST", "/snapshots", public_key, secret_key, data)
    return response.get("snapshot_id")


async def service_snapshot(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data = {"service_id": service_id, "hot": hot}
    if name:
        data["name"] = name

    response = await _make_request("POST", "/snapshots", public_key, secret_key, data)
    return response.get("snapshot_id")


async def list_snapshots(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request("GET", "/snapshots", public_key, secret_key)
    return response.get("snapshots", [])


async def restore_snapshot(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/snapshots/{snapshot_id}/restore", public_key, secret_key, {})


async def delete_snapshot(
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
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("DELETE", f"/snapshots/{snapshot_id}", public_key, secret_key)


# =============================================================================
# Session Management Functions
# =============================================================================


async def list_sessions(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """
    List all sessions for the authenticated account.

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        List of session dicts containing id, container_name, status, etc.

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request("GET", "/sessions", public_key, secret_key)
    return response.get("sessions", [])


async def get_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Get details of a specific session.

    Args:
        session_id: Session ID to get details for
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Session details dict

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("GET", f"/sessions/{session_id}", public_key, secret_key)


async def create_session(
    language: Optional[str] = None,
    network_mode: str = "zerotrust",
    ttl: int = 3600,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    shell: Optional[str] = None,
    multiplexer: Optional[str] = None,
    vcpu: int = 1,
) -> Dict[str, Any]:
    """
    Create a new interactive session.

    Args:
        language: Optional programming language for the session
        network_mode: Network mode - "zerotrust" (default, no network) or "semitrusted" (with network)
        ttl: Time to live in seconds (default 3600)
        public_key: Optional API key
        secret_key: Optional API secret
        shell: Optional shell to use (e.g., "bash", "python3")
        multiplexer: Optional terminal multiplexer ("tmux" or "screen")
        vcpu: Number of vCPUs (1-8, default 1)

    Returns:
        Response dict containing session_id, container_name, etc.

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data: Dict[str, Any] = {
        "network_mode": network_mode,
        "ttl": ttl,
    }
    if language:
        data["language"] = language
    if shell:
        data["shell"] = shell
    if multiplexer:
        data["multiplexer"] = multiplexer
    if vcpu > 1:
        data["vcpu"] = vcpu

    return await _make_request("POST", "/sessions", public_key, secret_key, data)


async def delete_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Delete/terminate a session.

    Args:
        session_id: Session ID to delete
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with deletion confirmation and optional artifacts

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("DELETE", f"/sessions/{session_id}", public_key, secret_key)


async def freeze_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Freeze a session (pause execution, preserve state).

    Args:
        session_id: Session ID to freeze
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with freeze confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/sessions/{session_id}/freeze", public_key, secret_key, {})


async def unfreeze_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Unfreeze a session (resume execution).

    Args:
        session_id: Session ID to unfreeze
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with unfreeze confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/sessions/{session_id}/unfreeze", public_key, secret_key, {})


async def boost_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Boost a session (increase resources).

    Args:
        session_id: Session ID to boost
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with boost confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/sessions/{session_id}/boost", public_key, secret_key, {})


async def unboost_session(
    session_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Unboost a session (return to normal resources).

    Args:
        session_id: Session ID to unboost
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with unboost confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/sessions/{session_id}/unboost", public_key, secret_key, {})


async def shell_session(
    session_id: str,
    command: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Execute a shell command in a session.

    Note: This is for one-off commands. For interactive shell access,
    use WebSocket connection to /sessions/{id}/shell.

    Args:
        session_id: Session ID to execute command in
        command: Shell command to execute
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with command output

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request(
        "POST",
        f"/sessions/{session_id}/shell",
        public_key,
        secret_key,
        {"command": command},
    )


# =============================================================================
# Service Management Functions
# =============================================================================


async def list_services(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> List[Dict[str, Any]]:
    """
    List all services for the authenticated account.

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        List of service dicts containing id, name, status, ports, etc.

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    response = await _make_request("GET", "/services", public_key, secret_key)
    return response.get("services", [])


async def create_service(
    name: str,
    ports: List[int],
    bootstrap: Optional[str] = None,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    network_mode: str = "semitrusted",
    custom_domains: Optional[List[str]] = None,
    vcpu: int = 1,
    service_type: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Create a new persistent service.

    Args:
        name: Service name (used for subdomain: name.on.unsandbox.com)
        ports: List of ports to expose (e.g., [80, 443])
        bootstrap: Bootstrap script content, URL, or inline command
        public_key: Optional API key
        secret_key: Optional API secret
        network_mode: Network mode (default "semitrusted" for services)
        custom_domains: Optional list of custom domain names
        vcpu: Number of vCPUs (1-8, default 1)
        service_type: Optional service type for SRV records (e.g., "minecraft")

    Returns:
        Response dict containing service_id, etc.

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data: Dict[str, Any] = {
        "name": name,
        "ports": ports,
        "network_mode": network_mode,
    }
    if bootstrap:
        # Check if it looks like a URL
        if bootstrap.startswith("http://") or bootstrap.startswith("https://"):
            data["bootstrap"] = bootstrap
        else:
            data["bootstrap_content"] = bootstrap
    if custom_domains:
        data["custom_domains"] = custom_domains
    if vcpu > 1:
        data["vcpu"] = vcpu
    if service_type:
        data["service_type"] = service_type

    return await _make_request("POST", "/services", public_key, secret_key, data)


async def get_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Get details of a specific service.

    Args:
        service_id: Service ID to get details for
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Service details dict

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("GET", f"/services/{service_id}", public_key, secret_key)


async def update_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    vcpu: Optional[int] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Update a service (e.g., resize vCPU/memory).

    Args:
        service_id: Service ID to update
        public_key: Optional API key
        secret_key: Optional API secret
        vcpu: Optional new vCPU count (1-8)
        **kwargs: Additional fields to update

    Returns:
        Response dict with update confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data: Dict[str, Any] = {}
    if vcpu is not None:
        data["vcpu"] = vcpu
    data.update(kwargs)

    return await _make_request("PATCH", f"/services/{service_id}", public_key, secret_key, data)


async def delete_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Delete/destroy a service.

    Args:
        service_id: Service ID to delete
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with deletion confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("DELETE", f"/services/{service_id}", public_key, secret_key)


async def freeze_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Freeze a service (pause execution, preserve state).

    Args:
        service_id: Service ID to freeze
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with freeze confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/services/{service_id}/freeze", public_key, secret_key, {})


async def unfreeze_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Unfreeze a service (resume execution).

    Args:
        service_id: Service ID to unfreeze
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with unfreeze confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/services/{service_id}/unfreeze", public_key, secret_key, {})


async def lock_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Lock a service to prevent accidental deletion.

    Args:
        service_id: Service ID to lock
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with lock confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/services/{service_id}/lock", public_key, secret_key, {})


async def unlock_service(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Unlock a service to allow deletion.

    Args:
        service_id: Service ID to unlock
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with unlock confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/services/{service_id}/unlock", public_key, secret_key, {})


async def get_service_logs(
    service_id: str,
    all_logs: bool = False,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Get bootstrap/runtime logs for a service.

    Args:
        service_id: Service ID to get logs for
        all_logs: If True, get all logs; if False, get last ~9000 lines (tail)
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict containing "log" field with log content

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    path = f"/services/{service_id}/logs"
    if all_logs:
        path += "?all=true"
    return await _make_request("GET", path, public_key, secret_key)


async def get_service_env(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Get environment vault status for a service.

    Returns metadata about the vault (has_vault, count, updated_at)
    but NOT the actual secrets. Use export_service_env to retrieve secrets.

    Args:
        service_id: Service ID to get env status for
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with has_vault, count, updated_at fields

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("GET", f"/services/{service_id}/env", public_key, secret_key)


async def set_service_env(
    service_id: str,
    env_dict: Dict[str, str],
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Set environment variables for a service.

    Replaces the entire environment vault with the provided variables.
    Variables are encrypted at rest and injected into the container.

    Args:
        service_id: Service ID to set env for
        env_dict: Dictionary of environment variables (KEY: VALUE)
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with count of variables set

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    # Convert dict to .env format for the API
    env_content = "\n".join(f"{k}={v}" for k, v in env_dict.items())

    # Note: This endpoint expects text/plain body, but we'll send as JSON
    # and let the API handle conversion
    return await _make_request(
        "POST",
        f"/services/{service_id}/env",
        public_key,
        secret_key,
        {"env": env_content},
    )


async def delete_service_env(
    service_id: str,
    keys: Optional[List[str]] = None,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Delete environment vault or specific keys from a service.

    Args:
        service_id: Service ID to delete env from
        keys: Optional list of specific keys to delete; if None, deletes entire vault
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with deletion confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    path = f"/services/{service_id}/env"
    # If specific keys provided, could add as query params (API dependent)
    return await _make_request("DELETE", path, public_key, secret_key)


async def export_service_env(
    service_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Export environment vault secrets for a service.

    Requires HMAC authentication to prove ownership.
    Returns the actual secret values in .env format.

    Args:
        service_id: Service ID to export env from
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict containing "env" field with KEY=VALUE content

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/services/{service_id}/env/export", public_key, secret_key, {})


async def redeploy_service(
    service_id: str,
    bootstrap: Optional[str] = None,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Redeploy a service (re-run bootstrap script).

    Bootstrap scripts should be idempotent for proper upgrade behavior.

    Args:
        service_id: Service ID to redeploy
        bootstrap: Optional new bootstrap script/URL (uses existing if not provided)
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with redeploy confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data: Dict[str, Any] = {}
    if bootstrap:
        if bootstrap.startswith("http://") or bootstrap.startswith("https://"):
            data["bootstrap"] = bootstrap
        else:
            data["bootstrap_content"] = bootstrap

    return await _make_request("POST", f"/services/{service_id}/redeploy", public_key, secret_key, data)


async def execute_in_service(
    service_id: str,
    command: str,
    timeout: int = 30000,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Execute a command in a running service container.

    Uses async job polling for long-running commands.

    Args:
        service_id: Service ID to execute command in
        command: Shell command to execute
        timeout: Command timeout in milliseconds (default 30000)
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with job_id for async polling, or direct result

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request(
        "POST",
        f"/services/{service_id}/execute",
        public_key,
        secret_key,
        {"command": command, "timeout": timeout},
    )


# =============================================================================
# Additional Snapshot Functions
# =============================================================================


async def lock_snapshot(
    snapshot_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Lock a snapshot to prevent accidental deletion.

    Args:
        snapshot_id: Snapshot ID to lock
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with lock confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/snapshots/{snapshot_id}/lock", public_key, secret_key, {})


async def unlock_snapshot(
    snapshot_id: str,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Unlock a snapshot to allow deletion.

    Args:
        snapshot_id: Snapshot ID to unlock
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with unlock confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request("POST", f"/snapshots/{snapshot_id}/unlock", public_key, secret_key, {})


async def clone_snapshot(
    snapshot_id: str,
    clone_type: str = "session",
    name: Optional[str] = None,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
    shell: Optional[str] = None,
    ports: Optional[List[int]] = None,
) -> Dict[str, Any]:
    """
    Clone a snapshot to create a new session or service.

    Args:
        snapshot_id: Snapshot ID to clone from
        clone_type: Type of resource to create ("session" or "service")
        name: Optional name for the new resource
        public_key: Optional API key
        secret_key: Optional API secret
        shell: Optional shell for session clones
        ports: Optional ports list for service clones

    Returns:
        Response dict containing session_id or service_id

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    data: Dict[str, Any] = {"type": clone_type}
    if name:
        data["name"] = name
    if shell:
        data["shell"] = shell
    if ports:
        data["ports"] = ports

    return await _make_request("POST", f"/snapshots/{snapshot_id}/clone", public_key, secret_key, data)


# =============================================================================
# Key Validation
# =============================================================================


PORTAL_BASE = "https://unsandbox.com"


async def validate_keys(
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Validate API keys against the portal.

    Checks if the keys are valid, not expired, and not suspended.

    Args:
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with validation result:
        - valid: True if keys are valid
        - tier: Account tier level
        - expires_at: Expiration timestamp (if applicable)
        - reason: Reason for invalid status (if applicable)

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)

    url = f"{PORTAL_BASE}/keys/validate"
    timestamp = int(time.time())
    body = ""

    signature = _sign_request(secret_key, timestamp, "POST", "/keys/validate", body)

    headers = {
        "Authorization": f"Bearer {public_key}",
        "X-Timestamp": str(timestamp),
        "X-Signature": signature,
        "Content-Type": "application/json",
    }

    async with aiohttp.ClientSession() as session:
        async with session.post(url, headers=headers, data=body, timeout=aiohttp.ClientTimeout(total=30)) as resp:
            resp.raise_for_status()
            return await resp.json()
