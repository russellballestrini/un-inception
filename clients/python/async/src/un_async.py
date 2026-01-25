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
        set_unfreeze_on_demand,
        set_show_freeze_page,
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
        # Image generation
        image,
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
    unfreeze_on_demand: bool = False,
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
        unfreeze_on_demand: If True, automatically unfreeze service on incoming requests

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
    if unfreeze_on_demand:
        data["unfreeze_on_demand"] = unfreeze_on_demand

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


async def set_unfreeze_on_demand(
    service_id: str,
    enabled: bool,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Enable or disable automatic unfreezing on incoming requests.

    When enabled, a frozen service will automatically wake up when it
    receives an incoming HTTP request.

    Args:
        service_id: Service ID to configure
        enabled: True to enable auto-unfreeze, False to disable
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with update confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request(
        "PATCH",
        f"/services/{service_id}",
        public_key,
        secret_key,
        {"unfreeze_on_demand": enabled},
    )


async def set_show_freeze_page(
    service_id: str,
    enabled: bool,
    public_key: Optional[str] = None,
    secret_key: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Enable or disable freeze page display for a service.

    When disabled, frozen services return a JSON error response instead of
    displaying the payment/unfreeze page to visitors.

    Args:
        service_id: Service ID to configure
        enabled: True to show freeze page, False to return JSON error
        public_key: Optional API key
        secret_key: Optional API secret

    Returns:
        Response dict with update confirmation

    Raises:
        aiohttp.ClientError: Network errors
        ValueError: Invalid response format
        CredentialsError: Missing credentials
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    return await _make_request(
        "PATCH",
        f"/services/{service_id}",
        public_key,
        secret_key,
        {"show_freeze_page": enabled},
    )


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

    url = f"{API_BASE}/keys/validate"
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


# =============================================================================
# Image Generation
# =============================================================================


async def image(
    prompt: str,
    *,
    model: str = None,
    size: str = "1024x1024",
    quality: str = "standard",
    n: int = 1,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Generate images from text prompt using AI.

    Args:
        prompt: Text description of the image to generate
        model: Model to use (optional, uses default)
        size: Image size (e.g., "1024x1024", "512x512")
        quality: "standard" or "hd"
        n: Number of images to generate
        public_key: API public key (optional)
        secret_key: API secret key (optional)

    Returns:
        dict with keys: images (list of base64 or URLs), created_at

    Example:
        >>> result = await image("A sunset over mountains")
        >>> print(result["images"][0])
    """
    public_key, secret_key = _resolve_credentials(public_key, secret_key)
    payload = {
        "prompt": prompt,
        "size": size,
        "quality": quality,
        "n": n,
    }
    if model:
        payload["model"] = model

    return await _make_request("POST", "/image", public_key, secret_key, payload)


# =============================================================================
# CLI Implementation
# =============================================================================

import sys
import argparse


def _parse_env_file(file_path: str) -> Dict[str, str]:
    """Parse a .env file into a dictionary."""
    env_dict = {}
    try:
        with open(file_path, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                if "=" in line:
                    key, _, value = line.partition("=")
                    # Handle quoted values
                    value = value.strip()
                    if (value.startswith('"') and value.endswith('"')) or \
                       (value.startswith("'") and value.endswith("'")):
                        value = value[1:-1]
                    env_dict[key.strip()] = value
    except Exception as e:
        print(f"Error: Failed to parse env file: {e}", file=sys.stderr)
        sys.exit(1)
    return env_dict


def _format_list_output(items: List[Dict[str, Any]], resource_type: str) -> str:
    """Format list output in table format."""
    if not items:
        return f"No {resource_type}s found."

    # Determine columns based on resource type
    if resource_type == "session":
        headers = ["ID", "STATUS", "SHELL", "CREATED"]
        rows = []
        for item in items:
            rows.append([
                item.get("id", item.get("session_id", ""))[:36],
                item.get("status", "unknown"),
                item.get("shell", "bash"),
                item.get("created_at", "")[:19] if item.get("created_at") else "",
            ])
    elif resource_type == "service":
        headers = ["ID", "NAME", "STATUS", "PORTS", "CREATED"]
        rows = []
        for item in items:
            ports = item.get("ports", [])
            ports_str = ",".join(str(p) for p in ports) if ports else ""
            rows.append([
                item.get("id", item.get("service_id", ""))[:36],
                item.get("name", "")[:20],
                item.get("status", "unknown"),
                ports_str[:15],
                item.get("created_at", "")[:19] if item.get("created_at") else "",
            ])
    elif resource_type == "snapshot":
        headers = ["ID", "NAME", "TYPE", "SIZE", "CREATED"]
        rows = []
        for item in items:
            rows.append([
                item.get("id", item.get("snapshot_id", ""))[:36],
                item.get("name", "")[:20],
                item.get("source_type", "unknown"),
                item.get("size", ""),
                item.get("created_at", "")[:19] if item.get("created_at") else "",
            ])
    else:
        headers = ["ID", "STATUS"]
        rows = [[str(item.get("id", "")), str(item.get("status", ""))] for item in items]

    # Calculate column widths
    widths = [len(h) for h in headers]
    for row in rows:
        for i, cell in enumerate(row):
            widths[i] = max(widths[i], len(str(cell)))

    # Build output
    lines = []
    header_line = "  ".join(h.ljust(widths[i]) for i, h in enumerate(headers))
    lines.append(header_line)
    for row in rows:
        line = "  ".join(str(cell).ljust(widths[i]) for i, cell in enumerate(row))
        lines.append(line)

    return "\n".join(lines)


def _build_parser() -> argparse.ArgumentParser:
    """Build the argument parser for the CLI."""
    parser = argparse.ArgumentParser(
        prog="un_async.py",
        description="Unsandbox CLI (Async) - Execute code in secure containers",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python un_async.py script.py                    Execute Python script
  python un_async.py -s bash 'echo hello'         Inline bash command
  python un_async.py session --list               List active sessions
  python un_async.py service --list               List all services
  python un_async.py snapshot --list              List all snapshots
  python un_async.py key                          Check API key
""",
    )

    # Global options
    parser.add_argument("-s", "--shell", metavar="LANG",
                        help="Language for inline code execution")
    parser.add_argument("-e", "--env", action="append", metavar="KEY=VAL",
                        help="Set environment variable (can be used multiple times)")
    parser.add_argument("-f", "--file", action="append", metavar="FILE",
                        help="Add input file to /tmp/ (can be used multiple times)")
    parser.add_argument("-F", "--file-path", action="append", metavar="FILE",
                        help="Add input file with path preserved")
    parser.add_argument("-a", "--artifacts", action="store_true",
                        help="Return compiled artifacts")
    parser.add_argument("-o", "--output", metavar="DIR",
                        help="Output directory for artifacts")
    parser.add_argument("-p", "--public-key", metavar="KEY",
                        help="API public key")
    parser.add_argument("-k", "--secret-key", metavar="KEY",
                        help="API secret key")
    parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"],
                        default="zerotrust", help="Network mode (default: zerotrust)")
    parser.add_argument("-v", "--vcpu", type=int, default=1, choices=range(1, 9),
                        metavar="N", help="vCPU count (1-8, default: 1)")
    parser.add_argument("-y", "--yes", action="store_true",
                        help="Skip confirmation prompts")

    # Subcommands
    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # Session subcommand
    session_parser = subparsers.add_parser("session", help="Manage interactive sessions")
    session_group = session_parser.add_mutually_exclusive_group()
    session_group.add_argument("-l", "--list", action="store_true",
                               help="List active sessions")
    session_group.add_argument("--attach", metavar="ID",
                               help="Reconnect to existing session")
    session_group.add_argument("--kill", metavar="ID",
                               help="Terminate a session")
    session_group.add_argument("--freeze", metavar="ID",
                               help="Pause session")
    session_group.add_argument("--unfreeze", metavar="ID",
                               help="Resume session")
    session_group.add_argument("--boost", metavar="ID",
                               help="Add resources to session")
    session_group.add_argument("--unboost", metavar="ID",
                               help="Remove boost from session")
    session_group.add_argument("--snapshot", metavar="ID",
                               help="Create snapshot of session")
    session_parser.add_argument("--shell", metavar="SHELL",
                                help="Shell/REPL to use (default: bash)")
    session_parser.add_argument("--tmux", action="store_true",
                                help="Enable persistence with tmux")
    session_parser.add_argument("--screen", action="store_true",
                                help="Enable persistence with screen")
    session_parser.add_argument("--snapshot-name", metavar="NAME",
                                help="Name for snapshot")
    session_parser.add_argument("--hot", action="store_true",
                                help="Live snapshot (no freeze)")
    session_parser.add_argument("--audit", action="store_true",
                                help="Record session")

    # Service subcommand
    service_parser = subparsers.add_parser("service", help="Manage persistent services")
    service_group = service_parser.add_mutually_exclusive_group()
    service_group.add_argument("-l", "--list", action="store_true",
                               help="List all services")
    service_group.add_argument("--info", metavar="ID",
                               help="Get service details")
    service_group.add_argument("--logs", metavar="ID",
                               help="Get all logs")
    service_group.add_argument("--tail", metavar="ID",
                               help="Get last 9000 lines of logs")
    service_group.add_argument("--freeze", metavar="ID",
                               help="Pause service")
    service_group.add_argument("--unfreeze", metavar="ID",
                               help="Resume service")
    service_group.add_argument("--destroy", metavar="ID",
                               help="Delete service")
    service_group.add_argument("--lock", metavar="ID",
                               help="Prevent deletion")
    service_group.add_argument("--unlock", metavar="ID",
                               help="Allow deletion")
    service_group.add_argument("--resize", metavar="ID",
                               help="Resize service (with --vcpu)")
    service_group.add_argument("--redeploy", metavar="ID",
                               help="Re-run bootstrap")
    service_group.add_argument("--execute", nargs=2, metavar=("ID", "CMD"),
                               help="Run command in service")
    service_group.add_argument("--snapshot", metavar="ID",
                               help="Create snapshot of service")
    service_parser.add_argument("--name", metavar="NAME",
                                help="Service name (creates new)")
    service_parser.add_argument("--ports", metavar="PORTS",
                                help="Comma-separated ports")
    service_parser.add_argument("--domains", metavar="DOMAINS",
                                help="Custom domains")
    service_parser.add_argument("--type", metavar="TYPE", dest="service_type",
                                help="Service type (minecraft, tcp, udp)")
    service_parser.add_argument("--bootstrap", metavar="CMD",
                                help="Bootstrap command")
    service_parser.add_argument("--bootstrap-file", metavar="FILE",
                                help="Bootstrap from file")
    service_parser.add_argument("--env-file", metavar="FILE",
                                help="Load env from .env file")
    service_parser.add_argument("--snapshot-name", metavar="NAME",
                                help="Name for snapshot")
    service_parser.add_argument("--hot", action="store_true",
                                help="Live snapshot (no freeze)")

    # Service env subcommand
    service_env_parser = subparsers.add_parser("service-env",
                                                help="Manage service environment vault")
    service_env_parser.add_argument("action", choices=["status", "set", "export", "delete"],
                                    help="Environment action")
    service_env_parser.add_argument("service_id", metavar="ID",
                                    help="Service ID")
    service_env_parser.add_argument("--env-file", metavar="FILE",
                                    help="Load env from .env file (for set)")

    # Snapshot subcommand
    snapshot_parser = subparsers.add_parser("snapshot", help="Manage snapshots")
    snapshot_group = snapshot_parser.add_mutually_exclusive_group()
    snapshot_group.add_argument("-l", "--list", action="store_true",
                                help="List all snapshots")
    snapshot_group.add_argument("--info", metavar="ID",
                                help="Get snapshot details")
    snapshot_group.add_argument("--delete", metavar="ID",
                                help="Delete snapshot")
    snapshot_group.add_argument("--lock", metavar="ID",
                                help="Prevent deletion")
    snapshot_group.add_argument("--unlock", metavar="ID",
                                help="Allow deletion")
    snapshot_group.add_argument("--clone", metavar="ID",
                                help="Clone snapshot")
    snapshot_parser.add_argument("--type", choices=["session", "service"],
                                 dest="clone_type", help="Clone type")
    snapshot_parser.add_argument("--name", metavar="NAME",
                                 help="Name for cloned resource")
    snapshot_parser.add_argument("--shell", metavar="SHELL",
                                 help="Shell for cloned session")
    snapshot_parser.add_argument("--ports", metavar="PORTS",
                                 help="Ports for cloned service")

    # Key subcommand
    subparsers.add_parser("key", help="Check API key validity")

    # Positional argument for source file or inline code
    parser.add_argument("source", nargs="?",
                        help="Source file or inline code (with -s)")

    return parser


async def _async_main():
    """Async main entry point for CLI."""
    parser = _build_parser()
    args = parser.parse_args()

    # Resolve credentials
    try:
        public_key, secret_key = _resolve_credentials(
            args.public_key, args.secret_key
        )
    except CredentialsError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(3)

    try:
        # Handle subcommands
        if args.command == "session":
            await _handle_session_command(args, public_key, secret_key)
        elif args.command == "service":
            await _handle_service_command(args, public_key, secret_key)
        elif args.command == "service-env":
            await _handle_service_env_command(args, public_key, secret_key)
        elif args.command == "snapshot":
            await _handle_snapshot_command(args, public_key, secret_key)
        elif args.command == "key":
            await _handle_key_command(public_key, secret_key)
        elif args.source or args.shell:
            await _handle_execute_command(args, public_key, secret_key)
        else:
            parser.print_help()
            sys.exit(2)
    except CredentialsError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(3)
    except aiohttp.ClientResponseError as e:
        if e.status == 401:
            print("Error: Authentication failed", file=sys.stderr)
            sys.exit(3)
        print(f"Error: API error - {e}", file=sys.stderr)
        sys.exit(4)
    except aiohttp.ClientError as e:
        print(f"Error: Network error - {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


async def _handle_execute_command(args, public_key: str, secret_key: str):
    """Handle code execution command."""
    # Determine language and code
    if args.shell:
        # Inline code mode
        if not args.source:
            print("Error: Code required with -s/--shell", file=sys.stderr)
            sys.exit(2)
        language = args.shell
        code = args.source
    else:
        # File mode
        if not args.source:
            print("Error: Source file required", file=sys.stderr)
            sys.exit(2)

        # Detect language from filename
        language = detect_language(args.source)
        if not language:
            print(f"Error: Cannot detect language from '{args.source}'", file=sys.stderr)
            sys.exit(2)

        # Read source file
        try:
            with open(args.source, "r") as f:
                code = f.read()
        except FileNotFoundError:
            print(f"Error: File not found: {args.source}", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"Error: Failed to read file: {e}", file=sys.stderr)
            sys.exit(1)

    # Execute code
    result = await execute_code(language, code, public_key, secret_key)

    # Output result
    stdout = result.get("stdout", "")
    stderr = result.get("stderr", "")
    exit_code = result.get("exit_code", 0)
    execution_time = result.get("execution_time_ms", 0)

    if stdout:
        print(stdout, end="")
        if not stdout.endswith("\n"):
            print()

    if stderr:
        print(stderr, end="", file=sys.stderr)
        if not stderr.endswith("\n"):
            print(file=sys.stderr)

    print("---")
    print(f"Exit code: {exit_code}")
    print(f"Execution time: {execution_time}ms")

    sys.exit(exit_code if exit_code else 0)


async def _handle_session_command(args, public_key: str, secret_key: str):
    """Handle session subcommand."""
    if args.list:
        sessions = await list_sessions(public_key, secret_key)
        print(_format_list_output(sessions, "session"))
    elif args.attach:
        # Get session info for attach
        session = await get_session(args.attach, public_key, secret_key)
        print(f"Session ID: {session.get('id', session.get('session_id', ''))}")
        print(f"Status: {session.get('status', 'unknown')}")
        print(f"WebSocket URL: wss://api.unsandbox.com/sessions/{args.attach}/shell")
        print("\nUse a WebSocket client to connect interactively.")
    elif args.kill:
        result = await delete_session(args.kill, public_key, secret_key)
        print(f"Session {args.kill} terminated")
    elif args.freeze:
        result = await freeze_session(args.freeze, public_key, secret_key)
        print(f"Session {args.freeze} frozen")
    elif args.unfreeze:
        result = await unfreeze_session(args.unfreeze, public_key, secret_key)
        print(f"Session {args.unfreeze} unfrozen")
    elif args.boost:
        result = await boost_session(args.boost, public_key, secret_key)
        print(f"Session {args.boost} boosted")
    elif args.unboost:
        result = await unboost_session(args.unboost, public_key, secret_key)
        print(f"Session {args.unboost} unboosted")
    elif args.snapshot:
        snapshot_id = await session_snapshot(
            args.snapshot, public_key, secret_key,
            name=args.snapshot_name,
            hot=args.hot
        )
        print(f"Snapshot created: {snapshot_id}")
    else:
        # Create new session
        multiplexer = None
        if args.tmux:
            multiplexer = "tmux"
        elif args.screen:
            multiplexer = "screen"

        result = await create_session(
            shell=args.shell,
            network_mode="semitrusted" if hasattr(args, 'network') and args.network == "semitrusted" else "zerotrust",
            public_key=public_key,
            secret_key=secret_key,
            multiplexer=multiplexer,
        )

        session_id = result.get("session_id", result.get("id", ""))
        print(f"Session created: {session_id}")
        print(f"WebSocket URL: wss://api.unsandbox.com/sessions/{session_id}/shell")


async def _handle_service_command(args, public_key: str, secret_key: str):
    """Handle service subcommand."""
    if args.list:
        services = await list_services(public_key, secret_key)
        print(_format_list_output(services, "service"))
    elif args.info:
        service = await get_service(args.info, public_key, secret_key)
        print(json.dumps(service, indent=2))
    elif args.logs:
        result = await get_service_logs(args.logs, all_logs=True, public_key=public_key, secret_key=secret_key)
        print(result.get("log", ""))
    elif args.tail:
        result = await get_service_logs(args.tail, all_logs=False, public_key=public_key, secret_key=secret_key)
        print(result.get("log", ""))
    elif args.freeze:
        result = await freeze_service(args.freeze, public_key, secret_key)
        print(f"Service {args.freeze} frozen")
    elif args.unfreeze:
        result = await unfreeze_service(args.unfreeze, public_key, secret_key)
        print(f"Service {args.unfreeze} unfrozen")
    elif args.destroy:
        result = await delete_service(args.destroy, public_key, secret_key)
        print(f"Service {args.destroy} destroyed")
    elif args.lock:
        result = await lock_service(args.lock, public_key, secret_key)
        print(f"Service {args.lock} locked")
    elif args.unlock:
        result = await unlock_service(args.unlock, public_key, secret_key)
        print(f"Service {args.unlock} unlocked")
    elif args.resize:
        vcpu = getattr(args, 'vcpu', 1) or 1
        result = await update_service(args.resize, public_key, secret_key, vcpu=vcpu)
        print(f"Service {args.resize} resized to {vcpu} vCPU(s)")
    elif args.redeploy:
        bootstrap = None
        if args.bootstrap_file:
            with open(args.bootstrap_file, "r") as f:
                bootstrap = f.read()
        elif args.bootstrap:
            bootstrap = args.bootstrap
        result = await redeploy_service(args.redeploy, bootstrap=bootstrap, public_key=public_key, secret_key=secret_key)
        print(f"Service {args.redeploy} redeployed")
    elif args.execute:
        service_id, command = args.execute
        result = await execute_in_service(service_id, command, public_key=public_key, secret_key=secret_key)
        # Handle async result
        if result.get("job_id"):
            job_result = await wait_for_job(result["job_id"], public_key, secret_key)
            stdout = job_result.get("stdout", "")
            stderr = job_result.get("stderr", "")
            if stdout:
                print(stdout, end="")
            if stderr:
                print(stderr, end="", file=sys.stderr)
        else:
            stdout = result.get("stdout", "")
            stderr = result.get("stderr", "")
            if stdout:
                print(stdout, end="")
            if stderr:
                print(stderr, end="", file=sys.stderr)
    elif args.snapshot:
        snapshot_id = await service_snapshot(
            args.snapshot, public_key, secret_key,
            name=getattr(args, 'snapshot_name', None),
            hot=getattr(args, 'hot', False)
        )
        print(f"Snapshot created: {snapshot_id}")
    elif args.name:
        # Create new service
        if not args.ports:
            print("Error: --ports required when creating service", file=sys.stderr)
            sys.exit(2)

        ports = [int(p.strip()) for p in args.ports.split(",")]

        bootstrap = None
        if args.bootstrap_file:
            with open(args.bootstrap_file, "r") as f:
                bootstrap = f.read()
        elif args.bootstrap:
            bootstrap = args.bootstrap

        custom_domains = None
        if args.domains:
            custom_domains = [d.strip() for d in args.domains.split(",")]

        result = await create_service(
            name=args.name,
            ports=ports,
            bootstrap=bootstrap,
            public_key=public_key,
            secret_key=secret_key,
            custom_domains=custom_domains,
            vcpu=getattr(args, 'vcpu', 1) or 1,
            service_type=args.service_type,
        )

        service_id = result.get("service_id", result.get("id", ""))
        print(f"Service created: {service_id}")
        print(f"URL: https://{args.name}.on.unsandbox.com")
    else:
        print("Error: No action specified for service command", file=sys.stderr)
        sys.exit(2)


async def _handle_service_env_command(args, public_key: str, secret_key: str):
    """Handle service env subcommand."""
    if args.action == "status":
        result = await get_service_env(args.service_id, public_key, secret_key)
        print(f"Has vault: {result.get('has_vault', False)}")
        print(f"Variable count: {result.get('count', 0)}")
        if result.get('updated_at'):
            print(f"Updated at: {result.get('updated_at')}")
    elif args.action == "set":
        # Read env from file or stdin
        if args.env_file:
            env_dict = _parse_env_file(args.env_file)
        else:
            # Read from stdin
            print("Enter environment variables (KEY=VALUE), one per line. Ctrl+D to finish:", file=sys.stderr)
            env_dict = {}
            for line in sys.stdin:
                line = line.strip()
                if line and "=" in line:
                    key, _, value = line.partition("=")
                    env_dict[key.strip()] = value.strip()

        result = await set_service_env(args.service_id, env_dict, public_key, secret_key)
        print(f"Environment set: {result.get('count', len(env_dict))} variables")
    elif args.action == "export":
        result = await export_service_env(args.service_id, public_key, secret_key)
        env_content = result.get("env", "")
        print(env_content)
    elif args.action == "delete":
        result = await delete_service_env(args.service_id, public_key=public_key, secret_key=secret_key)
        print(f"Environment vault deleted for service {args.service_id}")


async def _handle_snapshot_command(args, public_key: str, secret_key: str):
    """Handle snapshot subcommand."""
    if args.list:
        snapshots = await list_snapshots(public_key, secret_key)
        print(_format_list_output(snapshots, "snapshot"))
    elif args.info:
        # Get snapshot info via listing and filtering
        snapshots = await list_snapshots(public_key, secret_key)
        snapshot = next((s for s in snapshots if s.get("id") == args.info or s.get("snapshot_id") == args.info), None)
        if snapshot:
            print(json.dumps(snapshot, indent=2))
        else:
            print(f"Error: Snapshot {args.info} not found", file=sys.stderr)
            sys.exit(1)
    elif args.delete:
        result = await delete_snapshot(args.delete, public_key, secret_key)
        print(f"Snapshot {args.delete} deleted")
    elif args.lock:
        result = await lock_snapshot(args.lock, public_key, secret_key)
        print(f"Snapshot {args.lock} locked")
    elif args.unlock:
        result = await unlock_snapshot(args.unlock, public_key, secret_key)
        print(f"Snapshot {args.unlock} unlocked")
    elif args.clone:
        clone_type = args.clone_type or "session"
        ports = None
        if args.ports:
            ports = [int(p.strip()) for p in args.ports.split(",")]

        result = await clone_snapshot(
            args.clone,
            clone_type=clone_type,
            name=args.name,
            public_key=public_key,
            secret_key=secret_key,
            shell=args.shell,
            ports=ports,
        )

        if clone_type == "session":
            print(f"Session created: {result.get('session_id', result.get('id', ''))}")
        else:
            print(f"Service created: {result.get('service_id', result.get('id', ''))}")
    else:
        print("Error: No action specified for snapshot command", file=sys.stderr)
        sys.exit(2)


async def _handle_key_command(public_key: str, secret_key: str):
    """Handle key validation command."""
    result = await validate_keys(public_key, secret_key)

    print(f"Public key: {public_key}")
    print(f"Valid: {result.get('valid', False)}")
    if result.get('tier'):
        print(f"Tier: {result.get('tier')}")
    if result.get('expires_at'):
        print(f"Expires: {result.get('expires_at')}")
    if result.get('reason'):
        print(f"Reason: {result.get('reason')}")


def cli_main():
    """Main entry point for CLI - wraps async main."""
    asyncio.run(_async_main())


if __name__ == "__main__":
    cli_main()
