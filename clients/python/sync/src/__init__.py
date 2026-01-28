"""
Unsandbox Python SDK (Synchronous)

This module provides a synchronous client for executing code on unsandbox.com

Example usage:
    from un import execute_code, get_languages

    result = execute_code("python", "print('hello')")
    print(result)
"""

from .un import (
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
    CredentialsError,
)

__version__ = "4.2.32"
__all__ = [
    "execute_code",
    "execute_async",
    "get_job",
    "wait_for_job",
    "cancel_job",
    "list_jobs",
    "get_languages",
    "detect_language",
    "session_snapshot",
    "service_snapshot",
    "list_snapshots",
    "restore_snapshot",
    "delete_snapshot",
    "CredentialsError",
]
