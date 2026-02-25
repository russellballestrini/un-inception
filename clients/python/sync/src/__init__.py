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

__version__ = "4.3.4"
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
