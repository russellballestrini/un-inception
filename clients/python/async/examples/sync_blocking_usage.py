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
Sync (blocking) operations demonstration - standalone version

This example shows language detection and demonstrates patterns
that would be used with the async library.

Usage:
    python sync_blocking_usage.py

Expected output:
    === Language Detection ===
    script.py -> python
    app.js -> javascript
    main.go -> go
    ...
    === Pattern Demo ===
    Sync functions work without await
    Async functions would need await in real usage
    Demo complete!
"""


def detect_language(filename):
    """Detect programming language from filename extension."""
    ext_map = {
        'py': 'python',
        'js': 'javascript',
        'ts': 'typescript',
        'go': 'go',
        'rs': 'rust',
        'java': 'java',
        'rb': 'ruby',
        'php': 'php',
        'c': 'c',
        'cpp': 'cpp',
        'cs': 'csharp',
        'sh': 'bash',
        'pl': 'perl',
        'lua': 'lua',
    }
    ext = filename.rsplit('.', 1)[-1].lower() if '.' in filename else ''
    return ext_map.get(ext)


def main():
    """Demonstrate sync/blocking patterns."""
    print("=== Language Detection ===")

    test_files = ['script.py', 'app.js', 'main.go', 'Cargo.rs', 'Main.java']
    for filename in test_files:
        lang = detect_language(filename)
        print(f"{filename} -> {lang}")

    print("\n=== Pattern Demo ===")
    print("Sync functions work without await")
    print("Async functions would need await in real usage")
    print("Demo complete!")

    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())
