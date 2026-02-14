#!/usr/bin/env python3
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
