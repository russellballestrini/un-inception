#!/usr/bin/env python3
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software
#
# unsandbox SDK for Python - Execute code in secure sandboxes
# https://unsandbox.com | https://api.unsandbox.com/openapi
#
# Library Usage:
#   import un
#   result = un.execute("python", 'print("Hello")')
#   job = un.execute_async("python", code)
#   result = un.wait(job["job_id"])
#
# CLI Usage:
#   python un.py script.py
#   python un.py -s python 'print("Hello")'
#   python un.py session --shell python3
#
# Authentication (in priority order):
#   1. Function arguments: execute(..., public_key="...", secret_key="...")
#   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
#   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

"""
unsandbox - Secure Code Execution SDK

Simple:
    >>> import un
    >>> result = un.execute("python", 'print("Hello World")')
    >>> print(result["stdout"])
    Hello World

Async:
    >>> job = un.execute_async("python", long_running_code)
    >>> result = un.wait(job["job_id"])

Auto-detect language:
    >>> result = un.run('#!/usr/bin/env python3\\nprint("detected!")')

Client class:
    >>> client = un.Client(public_key="unsb-pk-...", secret_key="unsb-sk-...")
    >>> result = client.execute("python", code)
"""

import sys
import os
import json
import base64
import hmac
import hashlib
import time
import urllib.request
import urllib.error
from pathlib import Path
from typing import Optional, Dict, List, Any, Union

__version__ = "2.0.0"
__all__ = [
    "execute", "execute_async", "run", "run_async",
    "get_job", "wait", "cancel_job", "list_jobs",
    "image", "languages",
    "session_snapshot", "service_snapshot", "list_snapshots", "restore_snapshot", "delete_snapshot",
    "Client",
]

# ============================================================================
# Configuration
# ============================================================================

API_BASE = "https://api.unsandbox.com"
PORTAL_BASE = "https://unsandbox.com"
DEFAULT_TIMEOUT = 300  # 5 minutes
DEFAULT_TTL = 60  # 1 minute execution limit

# Polling delays (ms) - exponential backoff matching un.c
POLL_DELAYS = [300, 450, 700, 900, 650, 1600, 2000]

# Extension to language mapping
EXT_MAP = {
    ".py": "python", ".js": "javascript", ".ts": "typescript",
    ".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
    ".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
    ".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
    ".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
    ".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
    ".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
    ".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
    ".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
    ".dart": "dart", ".groovy": "groovy", ".scala": "scala",
    ".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
    ".pro": "prolog", ".forth": "forth", ".4th": "forth",
    ".tcl": "tcl", ".raku": "raku", ".m": "objc", ".awk": "awk",
}

# ============================================================================
# Exceptions
# ============================================================================

class UnsandboxError(Exception):
    """Base exception for unsandbox errors"""
    pass

class AuthenticationError(UnsandboxError):
    """Authentication failed - invalid or missing credentials"""
    pass

class ExecutionError(UnsandboxError):
    """Code execution failed"""
    def __init__(self, message: str, exit_code: int = None, stderr: str = None):
        super().__init__(message)
        self.exit_code = exit_code
        self.stderr = stderr

class APIError(UnsandboxError):
    """API request failed"""
    def __init__(self, message: str, status_code: int = None, response: str = None):
        super().__init__(message)
        self.status_code = status_code
        self.response = response

class TimeoutError(UnsandboxError):
    """Execution timed out"""
    pass

# ============================================================================
# HMAC Authentication
# ============================================================================

def _sign_request(secret_key: str, timestamp: int, method: str, path: str, body: str = "") -> str:
    """
    Generate HMAC-SHA256 signature for API request.

    Signature = HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
    """
    message = f"{timestamp}:{method}:{path}:{body}"
    signature = hmac.new(
        secret_key.encode('utf-8'),
        message.encode('utf-8'),
        hashlib.sha256
    ).hexdigest()
    return signature

def _load_accounts_csv(filepath: Path, account_index: int = 0) -> tuple:
    """Load credentials from accounts.csv file. Returns (pk, sk) or None."""
    if not filepath.exists():
        return None
    try:
        lines = filepath.read_text().strip().split('\n')
        valid_accounts = []
        for line in lines:
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            if ',' in line:
                pk, sk = line.split(',', 1)
                if pk.startswith('unsb-pk-') and sk.startswith('unsb-sk-'):
                    valid_accounts.append((pk, sk))
        if valid_accounts and account_index < len(valid_accounts):
            return valid_accounts[account_index]
    except Exception:
        pass
    return None


def _get_credentials(public_key: str = None, secret_key: str = None, account_index: int = 0) -> tuple:
    """
    Get API credentials in priority order:
    1. Function arguments
    2. Environment variables
    3. ~/.unsandbox/accounts.csv
    4. ./accounts.csv (same directory as this SDK)

    Returns (public_key, secret_key) or raises AuthenticationError
    """
    # Priority 1: Function arguments
    if public_key and secret_key:
        return public_key, secret_key

    # Priority 2: Environment variables
    env_pk = os.environ.get("UNSANDBOX_PUBLIC_KEY")
    env_sk = os.environ.get("UNSANDBOX_SECRET_KEY")
    if env_pk and env_sk:
        return env_pk, env_sk

    # Priority 3: ~/.unsandbox/accounts.csv
    home_accounts = Path.home() / ".unsandbox" / "accounts.csv"
    result = _load_accounts_csv(home_accounts, account_index)
    if result:
        return result

    # Priority 4: ./accounts.csv (same directory as SDK)
    sdk_dir = Path(__file__).parent
    local_accounts = sdk_dir / "accounts.csv"
    result = _load_accounts_csv(local_accounts, account_index)
    if result:
        return result

    raise AuthenticationError(
        "No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, "
        "or create ~/.unsandbox/accounts.csv or ./accounts.csv, or pass credentials to function."
    )

# ============================================================================
# HTTP Client
# ============================================================================

def _api_request(
    endpoint: str,
    method: str = "GET",
    data: Dict = None,
    body_text: str = None,
    content_type: str = "application/json",
    public_key: str = None,
    secret_key: str = None,
    timeout: int = DEFAULT_TIMEOUT,
    _raise_for_status: bool = True
) -> Dict:
    """
    Make authenticated API request with HMAC signature.
    """
    pk, sk = _get_credentials(public_key, secret_key)

    url = f"{API_BASE}{endpoint}"

    # Prepare body
    if body_text is not None:
        body = body_text
    elif data is not None:
        body = json.dumps(data)
    else:
        body = ""

    # Generate signature
    timestamp = int(time.time())
    signature = _sign_request(sk, timestamp, method, endpoint, body)

    # Build headers
    headers = {
        "Authorization": f"Bearer {pk}",
        "X-Timestamp": str(timestamp),
        "X-Signature": signature,
        "Content-Type": content_type,
    }

    # Make request
    req = urllib.request.Request(url, method=method, headers=headers)
    if body:
        req.data = body.encode('utf-8')

    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            response_body = resp.read().decode('utf-8')
            return json.loads(response_body) if response_body else {}
    except urllib.error.HTTPError as e:
        error_body = e.read().decode('utf-8') if e.fp else str(e)

        if e.code == 401:
            if 'timestamp' in error_body.lower():
                raise AuthenticationError(
                    "Request timestamp expired. Your system clock may be out of sync. "
                    "Run: sudo ntpdate -s time.nist.gov"
                )
            raise AuthenticationError(f"Authentication failed: {error_body}")

        if e.code == 429:
            raise APIError(f"Rate limit exceeded: {error_body}", e.code, error_body)

        if _raise_for_status:
            raise APIError(f"HTTP {e.code}: {error_body}", e.code, error_body)

        try:
            return json.loads(error_body)
        except:
            return {"error": error_body, "status_code": e.code}

    except urllib.error.URLError as e:
        raise APIError(f"Connection failed: {e.reason}")

# ============================================================================
# Core Execution Functions
# ============================================================================

def execute(
    language: str,
    code: str,
    *,
    env: Dict[str, str] = None,
    input_files: List[Dict] = None,
    network_mode: str = "zerotrust",
    ttl: int = DEFAULT_TTL,
    vcpu: int = 1,
    return_artifact: bool = False,
    return_wasm_artifact: bool = False,
    public_key: str = None,
    secret_key: str = None,
    timeout: int = DEFAULT_TIMEOUT,
) -> Dict[str, Any]:
    """
    Execute code synchronously and return results.

    Args:
        language: Programming language (python, javascript, go, rust, etc.)
        code: Source code to execute
        env: Environment variables dict
        input_files: List of {"filename": "...", "content": "..."} or {"filename": "...", "content_base64": "..."}
        network_mode: "zerotrust" (no network) or "semitrusted" (internet access)
        ttl: Execution timeout in seconds (1-900, default 60)
        vcpu: Virtual CPUs (1-8, default 1)
        return_artifact: Return compiled binary
        return_wasm_artifact: Compile to WebAssembly
        public_key: API public key (optional if env vars set)
        secret_key: API secret key (optional if env vars set)
        timeout: HTTP request timeout in seconds

    Returns:
        dict with keys: success, stdout, stderr, exit_code, language, job_id,
                       total_time_ms, network_mode, artifacts (optional)

    Raises:
        AuthenticationError: Invalid or missing credentials
        ExecutionError: Code execution failed
        APIError: API request failed

    Example:
        >>> result = un.execute("python", 'print("Hello World")')
        >>> print(result["stdout"])
        Hello World
    """
    payload = {
        "language": language,
        "code": code,
        "network_mode": network_mode,
        "ttl": ttl,
        "vcpu": vcpu,
    }

    if env:
        payload["env"] = env

    if input_files:
        # Convert plain content to base64 if needed
        processed_files = []
        for f in input_files:
            if "content_base64" in f:
                processed_files.append(f)
            elif "content" in f:
                processed_files.append({
                    "filename": f["filename"],
                    "content_base64": base64.b64encode(f["content"].encode()).decode()
                })
            else:
                processed_files.append(f)
        payload["input_files"] = processed_files

    if return_artifact:
        payload["return_artifact"] = True
    if return_wasm_artifact:
        payload["return_wasm_artifact"] = True

    result = _api_request(
        "/execute",
        method="POST",
        data=payload,
        public_key=public_key,
        secret_key=secret_key,
        timeout=timeout,
    )

    return result


def execute_async(
    language: str,
    code: str,
    *,
    env: Dict[str, str] = None,
    input_files: List[Dict] = None,
    network_mode: str = "zerotrust",
    ttl: int = DEFAULT_TTL,
    vcpu: int = 1,
    return_artifact: bool = False,
    return_wasm_artifact: bool = False,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Execute code asynchronously. Returns immediately with job_id for polling.

    Args:
        Same as execute()

    Returns:
        dict with keys: job_id, status ("pending")

    Example:
        >>> job = un.execute_async("python", long_running_code)
        >>> print(f"Job submitted: {job['job_id']}")
        >>> result = un.wait(job["job_id"])
    """
    payload = {
        "language": language,
        "code": code,
        "network_mode": network_mode,
        "ttl": ttl,
        "vcpu": vcpu,
    }

    if env:
        payload["env"] = env

    if input_files:
        processed_files = []
        for f in input_files:
            if "content_base64" in f:
                processed_files.append(f)
            elif "content" in f:
                processed_files.append({
                    "filename": f["filename"],
                    "content_base64": base64.b64encode(f["content"].encode()).decode()
                })
            else:
                processed_files.append(f)
        payload["input_files"] = processed_files

    if return_artifact:
        payload["return_artifact"] = True
    if return_wasm_artifact:
        payload["return_wasm_artifact"] = True

    return _api_request(
        "/execute/async",
        method="POST",
        data=payload,
        public_key=public_key,
        secret_key=secret_key,
    )


def run(
    code: str,
    *,
    env: Dict[str, str] = None,
    network_mode: str = "zerotrust",
    ttl: int = DEFAULT_TTL,
    public_key: str = None,
    secret_key: str = None,
    timeout: int = DEFAULT_TIMEOUT,
) -> Dict[str, Any]:
    """
    Execute code with automatic language detection from shebang.

    Args:
        code: Source code with shebang (e.g., #!/usr/bin/env python3)
        env: Environment variables dict
        network_mode: "zerotrust" or "semitrusted"
        ttl: Execution timeout in seconds
        public_key: API public key
        secret_key: API secret key
        timeout: HTTP request timeout

    Returns:
        dict with keys: success, stdout, stderr, exit_code, detected_language, ...

    Example:
        >>> code = '''#!/usr/bin/env python3
        ... print("Auto-detected!")
        ... '''
        >>> result = un.run(code)
        >>> print(result["detected_language"])  # "python"
    """
    # Build query params
    params = [f"ttl={ttl}", f"network_mode={network_mode}"]
    if env:
        params.append(f"env={urllib.parse.quote(json.dumps(env))}")

    endpoint = "/run?" + "&".join(params)

    return _api_request(
        endpoint,
        method="POST",
        body_text=code,
        content_type="text/plain",
        public_key=public_key,
        secret_key=secret_key,
        timeout=timeout,
    )


def run_async(
    code: str,
    *,
    env: Dict[str, str] = None,
    network_mode: str = "zerotrust",
    ttl: int = DEFAULT_TTL,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Execute code asynchronously with automatic language detection.

    Returns:
        dict with keys: job_id, detected_language, status ("pending")
    """
    import urllib.parse

    params = [f"ttl={ttl}", f"network_mode={network_mode}"]
    if env:
        params.append(f"env={urllib.parse.quote(json.dumps(env))}")

    endpoint = "/run/async?" + "&".join(params)

    return _api_request(
        endpoint,
        method="POST",
        body_text=code,
        content_type="text/plain",
        public_key=public_key,
        secret_key=secret_key,
    )


# ============================================================================
# Job Management
# ============================================================================

def get_job(
    job_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Get job status and results.

    Args:
        job_id: Job ID from execute_async or run_async

    Returns:
        dict with keys: job_id, status, result (if completed), timestamps

        status values: pending, running, completed, failed, timeout, cancelled
    """
    return _api_request(
        f"/jobs/{job_id}",
        method="GET",
        public_key=public_key,
        secret_key=secret_key,
    )


def wait(
    job_id: str,
    *,
    max_polls: int = 100,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Wait for job completion with exponential backoff polling.

    Args:
        job_id: Job ID from execute_async or run_async
        max_polls: Maximum number of poll attempts (default 100)

    Returns:
        Final job result dict

    Raises:
        TimeoutError: Max polls exceeded
        ExecutionError: Job failed

    Example:
        >>> job = un.execute_async("python", code)
        >>> result = un.wait(job["job_id"])
        >>> print(result["stdout"])
    """
    terminal_states = {"completed", "failed", "timeout", "cancelled"}

    for i in range(max_polls):
        # Exponential backoff delay
        delay_idx = min(i, len(POLL_DELAYS) - 1)
        time.sleep(POLL_DELAYS[delay_idx] / 1000.0)

        result = get_job(job_id, public_key=public_key, secret_key=secret_key)
        status = result.get("status", "")

        if status in terminal_states:
            if status == "failed":
                raise ExecutionError(
                    f"Job failed: {result.get('error', 'Unknown error')}",
                    result.get("exit_code"),
                    result.get("stderr")
                )
            if status == "timeout":
                raise TimeoutError(f"Job timed out: {job_id}")
            return result

    raise TimeoutError(f"Max polls ({max_polls}) exceeded for job {job_id}")


def cancel_job(
    job_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Cancel a running job.

    Returns partial output and artifacts collected before cancellation.
    """
    return _api_request(
        f"/jobs/{job_id}",
        method="DELETE",
        public_key=public_key,
        secret_key=secret_key,
    )


def list_jobs(
    *,
    public_key: str = None,
    secret_key: str = None,
) -> List[Dict[str, Any]]:
    """
    List all active jobs for this API key.

    Returns:
        List of job summary dicts with keys: job_id, language, status, submitted_at
    """
    result = _api_request(
        "/jobs",
        method="GET",
        public_key=public_key,
        secret_key=secret_key,
    )
    return result.get("jobs", [])


# ============================================================================
# Image Generation
# ============================================================================

def image(
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
    Generate images from text prompt.

    Args:
        prompt: Text description of the image to generate
        model: Model to use (optional, uses default)
        size: Image size (e.g., "1024x1024", "512x512")
        quality: "standard" or "hd"
        n: Number of images to generate

    Returns:
        dict with keys: images (list of base64 or URLs), created_at

    Example:
        >>> result = un.image("A sunset over mountains")
        >>> print(result["images"][0])
    """
    payload = {
        "prompt": prompt,
        "size": size,
        "quality": quality,
        "n": n,
    }
    if model:
        payload["model"] = model

    return _api_request(
        "/image",
        method="POST",
        data=payload,
        public_key=public_key,
        secret_key=secret_key,
    )


# ============================================================================
# Snapshots (Save/Restore Session & Service State)
# ============================================================================

def session_snapshot(
    session_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Create a snapshot of a session's current state.

    Args:
        session_id: ID of the session to snapshot

    Returns:
        dict with snapshot_id, created_at, status

    Example:
        >>> snap = un.session_snapshot("sess-abc123")
        >>> print(snap["snapshot_id"])
    """
    return _api_request(
        f"/sessions/{session_id}/snapshot",
        method="POST",
        data={},
        public_key=public_key,
        secret_key=secret_key,
    )


def service_snapshot(
    service_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Create a snapshot of a service's current state.

    Args:
        service_id: ID of the service to snapshot

    Returns:
        dict with snapshot_id, created_at, status
    """
    return _api_request(
        f"/services/{service_id}/snapshot",
        method="POST",
        data={},
        public_key=public_key,
        secret_key=secret_key,
    )


def list_snapshots(
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    List all available snapshots.

    Returns:
        dict with snapshots (list), count
    """
    return _api_request(
        "/snapshots",
        method="GET",
        public_key=public_key,
        secret_key=secret_key,
    )


def restore_snapshot(
    snapshot_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Restore a session or service from a snapshot.

    Args:
        snapshot_id: ID of the snapshot to restore

    Returns:
        dict with restored_id (session or service ID), status
    """
    return _api_request(
        f"/snapshots/{snapshot_id}/restore",
        method="POST",
        data={},
        public_key=public_key,
        secret_key=secret_key,
    )


def delete_snapshot(
    snapshot_id: str,
    *,
    public_key: str = None,
    secret_key: str = None,
) -> Dict[str, Any]:
    """
    Delete a snapshot.

    Args:
        snapshot_id: ID of the snapshot to delete

    Returns:
        dict with status
    """
    return _api_request(
        f"/snapshots/{snapshot_id}",
        method="DELETE",
        public_key=public_key,
        secret_key=secret_key,
    )


# ============================================================================
# Utility Functions
# ============================================================================

def languages(
    *,
    public_key: str = None,
    secret_key: str = None,
    force_refresh: bool = False,
) -> Dict[str, Any]:
    """
    Get list of supported programming languages.

    Results are cached in ~/.unsandbox/languages.json for 1 hour.

    Args:
        force_refresh: Bypass cache and fetch fresh data

    Returns:
        dict with keys: languages (list), count, aliases (dict)
    """
    cache_path = Path.home() / ".unsandbox" / "languages.json"
    cache_max_age = 3600  # 1 hour in seconds

    # Check cache unless force refresh
    if not force_refresh and cache_path.exists():
        try:
            cache_mtime = cache_path.stat().st_mtime
            if time.time() - cache_mtime < cache_max_age:
                return json.loads(cache_path.read_text())
        except Exception:
            pass  # Cache read failed, fetch from API

    # Fetch from API
    result = _api_request(
        "/languages",
        method="GET",
        public_key=public_key,
        secret_key=secret_key,
    )

    # Save to cache
    try:
        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_text(json.dumps(result))
    except Exception:
        pass  # Cache write failed, continue anyway

    return result


def detect_language(filename: str) -> Optional[str]:
    """
    Detect programming language from file extension or shebang.

    Returns language name or None if undetected.
    """
    ext = os.path.splitext(filename)[1].lower()
    if ext in EXT_MAP:
        return EXT_MAP[ext]

    # Try shebang
    try:
        with open(filename, 'r') as f:
            first_line = f.readline()
            if first_line.startswith('#!'):
                if 'python' in first_line: return 'python'
                if 'node' in first_line: return 'javascript'
                if 'ruby' in first_line: return 'ruby'
                if 'perl' in first_line: return 'perl'
                if 'bash' in first_line or '/sh' in first_line: return 'bash'
                if 'lua' in first_line: return 'lua'
                if 'php' in first_line: return 'php'
    except:
        pass

    return None


# ============================================================================
# Client Class
# ============================================================================

class Client:
    """
    Unsandbox API client with stored credentials.

    Example:
        >>> client = un.Client(public_key="unsb-pk-...", secret_key="unsb-sk-...")
        >>> result = client.execute("python", 'print("Hello")')
        >>>
        >>> # Or load from environment/config automatically:
        >>> client = un.Client()
        >>> result = client.execute("python", code)
    """

    def __init__(
        self,
        public_key: str = None,
        secret_key: str = None,
        account_index: int = 0,
    ):
        """
        Initialize client with credentials.

        Args:
            public_key: API public key (unsb-pk-...)
            secret_key: API secret key (unsb-sk-...)
            account_index: Account index in ~/.unsandbox/accounts.csv (default 0)
        """
        self.public_key, self.secret_key = _get_credentials(
            public_key, secret_key, account_index
        )

    def execute(self, language: str, code: str, **kwargs) -> Dict[str, Any]:
        """Execute code synchronously. See module-level execute() for args."""
        return execute(
            language, code,
            public_key=self.public_key,
            secret_key=self.secret_key,
            **kwargs
        )

    def execute_async(self, language: str, code: str, **kwargs) -> Dict[str, Any]:
        """Execute code asynchronously. See module-level execute_async() for args."""
        return execute_async(
            language, code,
            public_key=self.public_key,
            secret_key=self.secret_key,
            **kwargs
        )

    def run(self, code: str, **kwargs) -> Dict[str, Any]:
        """Execute with auto-detect. See module-level run() for args."""
        return run(
            code,
            public_key=self.public_key,
            secret_key=self.secret_key,
            **kwargs
        )

    def run_async(self, code: str, **kwargs) -> Dict[str, Any]:
        """Execute async with auto-detect. See module-level run_async() for args."""
        return run_async(
            code,
            public_key=self.public_key,
            secret_key=self.secret_key,
            **kwargs
        )

    def get_job(self, job_id: str) -> Dict[str, Any]:
        """Get job status. See module-level get_job() for details."""
        return get_job(job_id, public_key=self.public_key, secret_key=self.secret_key)

    def wait(self, job_id: str, **kwargs) -> Dict[str, Any]:
        """Wait for job completion. See module-level wait() for details."""
        return wait(job_id, public_key=self.public_key, secret_key=self.secret_key, **kwargs)

    def cancel_job(self, job_id: str) -> Dict[str, Any]:
        """Cancel a job. See module-level cancel_job() for details."""
        return cancel_job(job_id, public_key=self.public_key, secret_key=self.secret_key)

    def list_jobs(self) -> List[Dict[str, Any]]:
        """List active jobs. See module-level list_jobs() for details."""
        return list_jobs(public_key=self.public_key, secret_key=self.secret_key)

    def image(self, prompt: str, **kwargs) -> Dict[str, Any]:
        """Generate image. See module-level image() for args."""
        return image(prompt, public_key=self.public_key, secret_key=self.secret_key, **kwargs)

    def languages(self, force_refresh: bool = False) -> Dict[str, Any]:
        """Get supported languages (cached for 1 hour)."""
        return languages(public_key=self.public_key, secret_key=self.secret_key, force_refresh=force_refresh)

    def session_snapshot(self, session_id: str) -> Dict[str, Any]:
        """Create snapshot of session. See module-level session_snapshot() for details."""
        return session_snapshot(session_id, public_key=self.public_key, secret_key=self.secret_key)

    def service_snapshot(self, service_id: str) -> Dict[str, Any]:
        """Create snapshot of service. See module-level service_snapshot() for details."""
        return service_snapshot(service_id, public_key=self.public_key, secret_key=self.secret_key)

    def list_snapshots(self) -> Dict[str, Any]:
        """List all snapshots. See module-level list_snapshots() for details."""
        return list_snapshots(public_key=self.public_key, secret_key=self.secret_key)

    def restore_snapshot(self, snapshot_id: str) -> Dict[str, Any]:
        """Restore from snapshot. See module-level restore_snapshot() for details."""
        return restore_snapshot(snapshot_id, public_key=self.public_key, secret_key=self.secret_key)

    def delete_snapshot(self, snapshot_id: str) -> Dict[str, Any]:
        """Delete snapshot. See module-level delete_snapshot() for details."""
        return delete_snapshot(snapshot_id, public_key=self.public_key, secret_key=self.secret_key)


# ============================================================================
# CLI Interface
# ============================================================================

# ANSI colors
BLUE = "\033[34m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
RESET = "\033[0m"


def _cli_main():
    """CLI entry point - matches un.c interface"""
    import argparse

    parser = argparse.ArgumentParser(
        description="unsandbox - Execute code in secure sandboxes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s script.py                    Execute Python script
  %(prog)s -s python 'print("Hello")'   Execute inline code
  %(prog)s -e DEBUG=1 script.py         With environment variable
  %(prog)s -f data.csv process.py       With input file
  %(prog)s -n semitrusted script.py     With network access
  %(prog)s session                      Interactive bash session
  %(prog)s session --shell python3      Python REPL
  %(prog)s service --name web --ports 80 --bootstrap "python -m http.server"
        """
    )

    parser.add_argument("source", nargs="?", help="Source file or inline code")
    parser.add_argument("-s", "--shell", dest="inline_lang", metavar="LANG",
                        help="Execute inline code with specified language")
    parser.add_argument("-e", "--env", action="append", metavar="KEY=VALUE",
                        help="Set environment variable")
    parser.add_argument("-f", "--file", action="append", dest="files", metavar="FILE",
                        help="Add input file")
    parser.add_argument("-n", "--network", choices=["zerotrust", "semitrusted"],
                        default="zerotrust", help="Network mode")
    parser.add_argument("-v", "--vcpu", type=int, default=1, choices=range(1, 9),
                        help="vCPU count (1-8)")
    parser.add_argument("--ttl", type=int, default=60, help="Timeout in seconds")
    parser.add_argument("-a", "--artifacts", action="store_true",
                        help="Return artifacts")
    parser.add_argument("-o", "--output", metavar="DIR", help="Output directory")
    parser.add_argument("-p", "--public-key", help="API public key")
    parser.add_argument("-k", "--secret-key", help="API secret key")
    parser.add_argument("--async", dest="async_mode", action="store_true",
                        help="Execute asynchronously")

    args = parser.parse_args()

    # Need source file or inline code
    if not args.source and not args.inline_lang:
        parser.print_help()
        sys.exit(1)

    try:
        # Determine language and code
        if args.inline_lang:
            language = args.inline_lang
            code = args.source or ""
        else:
            if not os.path.exists(args.source):
                # Treat as inline bash
                language = "bash"
                code = args.source
            else:
                language = detect_language(args.source)
                if not language:
                    print(f"{RED}Error: Cannot detect language for {args.source}{RESET}", file=sys.stderr)
                    sys.exit(1)
                with open(args.source, 'r') as f:
                    code = f.read()

        # Parse environment variables
        env = {}
        if args.env:
            for e in args.env:
                if '=' in e:
                    k, v = e.split('=', 1)
                    env[k] = v

        # Load input files
        input_files = []
        if args.files:
            for filepath in args.files:
                if not os.path.exists(filepath):
                    print(f"{RED}Error: File not found: {filepath}{RESET}", file=sys.stderr)
                    sys.exit(1)
                with open(filepath, 'rb') as f:
                    content = base64.b64encode(f.read()).decode()
                input_files.append({
                    "filename": os.path.basename(filepath),
                    "content_base64": content
                })

        # Execute
        if args.async_mode:
            result = execute_async(
                language, code,
                env=env or None,
                input_files=input_files or None,
                network_mode=args.network,
                ttl=args.ttl,
                vcpu=args.vcpu,
                return_artifact=args.artifacts,
                public_key=args.public_key,
                secret_key=args.secret_key,
            )
            print(f"{GREEN}Job submitted: {result.get('job_id')}{RESET}")
            print(f"Status: {result.get('status')}")
            print(f"\nPoll with: python un.py job {result.get('job_id')}")
        else:
            result = execute(
                language, code,
                env=env or None,
                input_files=input_files or None,
                network_mode=args.network,
                ttl=args.ttl,
                vcpu=args.vcpu,
                return_artifact=args.artifacts,
                public_key=args.public_key,
                secret_key=args.secret_key,
            )

            # Print output
            if result.get("stdout"):
                print(result["stdout"], end='')
            if result.get("stderr"):
                print(f"{RED}{result['stderr']}{RESET}", end='', file=sys.stderr)

            # Save artifacts
            if args.artifacts and result.get("artifacts"):
                out_dir = args.output or "."
                os.makedirs(out_dir, exist_ok=True)
                for artifact in result["artifacts"]:
                    filename = artifact.get("filename", "artifact")
                    content = base64.b64decode(artifact["content_base64"])
                    path = os.path.join(out_dir, filename)
                    with open(path, 'wb') as f:
                        f.write(content)
                    os.chmod(path, 0o755)
                    print(f"{GREEN}Saved: {path}{RESET}", file=sys.stderr)

            sys.exit(result.get("exit_code", 0))

    except AuthenticationError as e:
        print(f"{RED}Authentication error: {e}{RESET}", file=sys.stderr)
        sys.exit(1)
    except ExecutionError as e:
        print(f"{RED}Execution error: {e}{RESET}", file=sys.stderr)
        if e.stderr:
            print(f"{RED}{e.stderr}{RESET}", file=sys.stderr)
        sys.exit(e.exit_code or 1)
    except APIError as e:
        print(f"{RED}API error: {e}{RESET}", file=sys.stderr)
        sys.exit(1)
    except TimeoutError as e:
        print(f"{RED}Timeout: {e}{RESET}", file=sys.stderr)
        sys.exit(124)
    except KeyboardInterrupt:
        print(f"\n{YELLOW}Interrupted{RESET}", file=sys.stderr)
        sys.exit(130)


if __name__ == "__main__":
    _cli_main()
