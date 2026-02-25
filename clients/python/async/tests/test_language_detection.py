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
Tests for language detection
"""

import pytest
import sys
import os

# Add src to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from un_async import detect_language


class TestLanguageDetection:
    """Test language detection from filename."""

    def test_detect_python(self):
        """Test Python detection."""
        assert detect_language("script.py") == "python"
        assert detect_language("test.py") == "python"
        assert detect_language("/path/to/script.py") == "python"

    def test_detect_javascript(self):
        """Test JavaScript detection."""
        assert detect_language("app.js") == "javascript"
        assert detect_language("index.js") == "javascript"

    def test_detect_typescript(self):
        """Test TypeScript detection."""
        assert detect_language("app.ts") == "typescript"

    def test_detect_go(self):
        """Test Go detection."""
        assert detect_language("main.go") == "go"

    def test_detect_rust(self):
        """Test Rust detection."""
        assert detect_language("lib.rs") == "rust"

    def test_detect_c(self):
        """Test C detection."""
        assert detect_language("program.c") == "c"

    def test_detect_cpp(self):
        """Test C++ detection."""
        assert detect_language("app.cpp") == "cpp"
        assert detect_language("app.cc") == "cpp"
        assert detect_language("app.cxx") == "cpp"

    def test_detect_ruby(self):
        """Test Ruby detection."""
        assert detect_language("script.rb") == "ruby"

    def test_detect_java(self):
        """Test Java detection."""
        assert detect_language("Main.java") == "java"

    def test_detect_bash(self):
        """Test Bash detection."""
        assert detect_language("script.sh") == "bash"

    def test_detect_unknown(self):
        """Test unknown extension returns None."""
        assert detect_language("script.xyz") is None
        assert detect_language("no_extension") is None
        assert detect_language("") is None
        assert detect_language(None) is None

    def test_detect_case_insensitive(self):
        """Test that detection is case-insensitive for extensions."""
        assert detect_language("script.PY") == "python"
        assert detect_language("script.Py") == "python"
        assert detect_language("SCRIPT.JS") == "javascript"

    def test_detect_with_dots_in_path(self):
        """Test detection with dots in path."""
        assert detect_language("/path/to/my.code/script.py") == "python"
        assert detect_language("~/code.dir/app.js") == "javascript"

    def test_detect_all_supported_languages(self):
        """Test detection for all supported languages."""
        test_cases = {
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
            "java": "java",
            "kt": "kotlin",
            "cs": "csharp",
            "fs": "fsharp",
            "hs": "haskell",
            "ml": "ocaml",
            "clj": "clojure",
            "scm": "scheme",
            "erl": "erlang",
            "ex": "elixir",
            "jl": "julia",
            "d": "d",
            "nim": "nim",
            "zig": "zig",
            "v": "v",
            "cr": "crystal",
            "dart": "dart",
            "groovy": "groovy",
            "lisp": "commonlisp",
            "cob": "cobol",
            "tcl": "tcl",
            "raku": "raku",
            "pro": "prolog",
            "4th": "forth",
        }

        for ext, expected_lang in test_cases.items():
            filename = f"test.{ext}"
            assert detect_language(filename) == expected_lang, f"Failed for {filename}"
