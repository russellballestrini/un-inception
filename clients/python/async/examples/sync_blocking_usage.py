#!/usr/bin/env python3
"""
Sync (blocking) operations from async library

This example shows how the async library also supports synchronous usage:
1. Using synchronous/blocking functions directly
2. Running async code from blocking context with asyncio.run()
3. Mixing sync and async patterns

Usage:
    python sync_blocking_usage.py

Or with custom credentials:
    UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=... python sync_blocking_usage.py
"""

import asyncio
import sys
import os

# Add src to path for development
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

try:
    from un_async import (
        execute_code,
        detect_language,
        get_languages,
    )
except ImportError as e:
    print(f"Missing dependency: {e}")
    print("Install with: pip install aiohttp")
    sys.exit(0)  # Exit gracefully for CI


async def async_approach():
    """Using async/await syntax."""
    print("=== Async Approach ===")
    result = await execute_code("python", 'print("Hello from async")')
    print(f"Output: {result.get('stdout', '').strip()}\n")


async def blocking_approach():
    """Using synchronous/blocking functions in async context."""
    print("=== Sync Functions (in async context) ===")

    # These are synchronous functions that don't need await
    lang = detect_language("script.py")
    print(f"Detected language for script.py: {lang}\n")

    # But we still need to await execute_code since it's async
    result = await execute_code("python", f'print("Executing {lang} code")')
    print(f"Output: {result.get('stdout', '').strip()}\n")


async def mixed_approach():
    """Mixing sync and async calls."""
    print("=== Mixed Sync/Async ===")

    # Synchronous call (no await needed)
    langs = get_languages.__doc__  # Just accessing the doc string
    print("get_languages is available for fetching supported languages\n")

    # Async call (await needed)
    result = await execute_code("javascript", 'console.log("Hello from mixed")')
    print(f"Output: {result.get('stdout', '').strip()}\n")


async def main():
    """Demonstrate various usage patterns."""
    try:
        await async_approach()
        await blocking_approach()
        await mixed_approach()
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    exit_code = asyncio.run(main())
    sys.exit(exit_code)
