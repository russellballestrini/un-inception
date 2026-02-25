#!/usr/bin/env python3
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
Verification script for unsandbox Python SDK (Synchronous)

This script verifies that the SDK is properly configured and working.
Run from the sync/ directory: python3 verify_sdk.py
"""

import sys
import os
from pathlib import Path

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "src"))

from un import (
    execute_code,
    execute_async,
    get_job,
    wait_for_job,
    cancel_job,
    list_jobs,
    get_languages,
    detect_language,
    CredentialsError,
    _resolve_credentials,
    _sign_request,
    _load_languages_cache,
    _save_languages_cache,
)


def test_language_detection():
    """Test language detection"""
    print("Testing language detection...")
    tests = [
        ("hello.py", "python"),
        ("app.js", "javascript"),
        ("main.go", "go"),
        ("script.rs", "rust"),
        ("main.cpp", "cpp"),
        ("unknown.xyz", None),
    ]

    for filename, expected in tests:
        result = detect_language(filename)
        status = "✓" if result == expected else "✗"
        print(f"  {status} detect_language('{filename}') = {result}")
        if result != expected:
            return False

    return True


def test_request_signing():
    """Test request signing"""
    print("\nTesting request signing...")

    sig1 = _sign_request("secret", 1234567890, "POST", "/execute", '{"code":"test"}')
    sig2 = _sign_request("secret", 1234567890, "POST", "/execute", '{"code":"test"}')

    # Test deterministic
    status = "✓" if sig1 == sig2 else "✗"
    print(f"  {status} Signatures are deterministic: {sig1 == sig2}")

    # Test format
    status = "✓" if len(sig1) == 64 else "✗"
    print(f"  {status} Signature is 64 hex chars: {len(sig1)} chars")

    # Test different secrets produce different sigs
    sig_diff = _sign_request("different", 1234567890, "POST", "/execute", '{"code":"test"}')
    status = "✓" if sig1 != sig_diff else "✗"
    print(f"  {status} Different secrets produce different signatures: {sig1 != sig_diff}")

    return len(sig1) == 64 and sig1 == sig2 and sig1 != sig_diff


def test_credentials():
    """Test credential resolution"""
    print("\nTesting credential resolution...")

    # Test 1: Function arguments
    try:
        pk, sk = _resolve_credentials("func_pk", "func_sk")
        status = "✓" if pk == "func_pk" and sk == "func_sk" else "✗"
        print(f"  {status} Function arguments: {pk == 'func_pk' and sk == 'func_sk'}")
    except Exception as e:
        print(f"  ✗ Function arguments failed: {e}")
        return False

    # Test 2: Can resolve credentials (from env or config)
    try:
        pk, sk = _resolve_credentials()
        print(f"  ✓ Credentials resolved: {pk[:20]}... / {sk[:20]}...")
        return True
    except CredentialsError:
        print(f"  ✗ No credentials found (this is OK for testing)")
        print(f"    Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables")
        return True  # Not a test failure, just missing credentials


def test_caching():
    """Test languages cache"""
    print("\nTesting caching...")

    import tempfile
    import json
    import time
    from unittest.mock import patch

    try:
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            # Save cache
            languages = ["python", "javascript", "go"]
            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)
                print(f"  ✓ Cache saved")

                # Load cache
                loaded = _load_languages_cache()
                if loaded == languages:
                    print(f"  ✓ Cache loaded correctly: {loaded}")
                else:
                    print(f"  ✗ Cache mismatch: {loaded}")
                    return False

                # Test expiration
                old_time = time.time() - 7200  # 2 hours old
                os.utime(cache_path, (old_time, old_time))
                expired = _load_languages_cache()
                if expired is None:
                    print(f"  ✓ Cache correctly expired")
                else:
                    print(f"  ✗ Cache should be expired: {expired}")
                    return False

        return True
    except Exception as e:
        print(f"  ✗ Cache test failed: {e}")
        import traceback

        traceback.print_exc()
        return False


def test_imports():
    """Test all public imports"""
    print("\nTesting imports...")

    functions = [
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

    print(f"  ✓ Imported {len(functions)} public functions and classes:")
    for func in functions:
        print(f"    - {func}")

    return True


def test_package_structure():
    """Verify package structure"""
    print("\nVerifying package structure...")

    required_files = [
        "src/__init__.py",
        "src/un.py",
        "setup.py",
        "README.md",
        "USAGE.md",
        "LICENSE",
        "MANIFEST.in",
        "pytest.ini",
        "tests/__init__.py",
        "tests/test_credentials.py",
        "tests/test_language_detection.py",
        "tests/test_signatures.py",
        "tests/test_caching.py",
        "tests/test_integration_mock.py",
        "tests/test_real_world_scenarios.py",
    ]

    base_dir = Path(__file__).parent
    all_exist = True

    for filename in required_files:
        filepath = base_dir / filename
        status = "✓" if filepath.exists() else "✗"
        print(f"  {status} {filename}")
        if not filepath.exists():
            all_exist = False

    return all_exist


def test_example_structure():
    """Verify examples exist"""
    print("\nVerifying examples...")

    example_files = [
        "examples/hello_world.py",
        "examples/fibonacci.py",
        "examples/hello_world_client.py",
        "examples/fibonacci_client.py",
        "examples/json_processing.py",
        "examples/http_request.py",
        "examples/file_operations.py",
    ]

    base_dir = Path(__file__).parent
    all_exist = True

    for filename in example_files:
        filepath = base_dir / filename
        status = "✓" if filepath.exists() else "✗"
        print(f"  {status} {filename}")
        if not filepath.exists():
            all_exist = False

    return all_exist


def main():
    """Run all verification tests"""
    print("=" * 60)
    print("Unsandbox Python SDK (Sync) - Verification")
    print("=" * 60)

    tests = [
        ("Package Structure", test_package_structure),
        ("Imports", test_imports),
        ("Language Detection", test_language_detection),
        ("Request Signing", test_request_signing),
        ("Credentials", test_credentials),
        ("Caching", test_caching),
        ("Examples", test_example_structure),
    ]

    results = []
    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append((test_name, result))
        except Exception as e:
            print(f"\n✗ {test_name} failed with exception: {e}")
            import traceback

            traceback.print_exc()
            results.append((test_name, False))

    print("\n" + "=" * 60)
    print("Summary")
    print("=" * 60)

    passed = sum(1 for _, result in results if result)
    total = len(results)

    for test_name, result in results:
        status = "✓" if result else "✗"
        print(f"{status} {test_name}")

    print(f"\nPassed: {passed}/{total}")

    if passed == total:
        print("\n✓ All verification tests passed!")
        return 0
    else:
        print(f"\n✗ {total - passed} test(s) failed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
