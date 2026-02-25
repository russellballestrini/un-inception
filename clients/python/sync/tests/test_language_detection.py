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

"""Tests for language detection in unsandbox SDK"""

from un import detect_language


class TestLanguageDetection:
    """Test language detection from filenames"""

    def test_python_detection(self):
        """Test Python file detection"""
        assert detect_language("script.py") == "python"
        assert detect_language("main.py") == "python"
        assert detect_language("/path/to/script.py") == "python"

    def test_javascript_detection(self):
        """Test JavaScript file detection"""
        assert detect_language("app.js") == "javascript"
        assert detect_language("index.js") == "javascript"

    def test_typescript_detection(self):
        """Test TypeScript file detection"""
        assert detect_language("app.ts") == "typescript"

    def test_go_detection(self):
        """Test Go file detection"""
        assert detect_language("main.go") == "go"

    def test_rust_detection(self):
        """Test Rust file detection"""
        assert detect_language("main.rs") == "rust"

    def test_c_detection(self):
        """Test C file detection"""
        assert detect_language("main.c") == "c"

    def test_cpp_detection(self):
        """Test C++ file detection"""
        assert detect_language("main.cpp") == "cpp"
        assert detect_language("main.cc") == "cpp"
        assert detect_language("main.cxx") == "cpp"

    def test_java_detection(self):
        """Test Java file detection"""
        assert detect_language("Main.java") == "java"

    def test_ruby_detection(self):
        """Test Ruby file detection"""
        assert detect_language("script.rb") == "ruby"

    def test_php_detection(self):
        """Test PHP file detection"""
        assert detect_language("index.php") == "php"

    def test_bash_detection(self):
        """Test Bash file detection"""
        assert detect_language("script.sh") == "bash"

    def test_r_detection(self):
        """Test R file detection (both lowercase and uppercase)"""
        assert detect_language("script.r") == "r"
        assert detect_language("script.R") == "r"

    def test_perl_detection(self):
        """Test Perl file detection"""
        assert detect_language("script.pl") == "perl"

    def test_lua_detection(self):
        """Test Lua file detection"""
        assert detect_language("script.lua") == "lua"

    def test_unknown_extension(self):
        """Test that unknown extensions return None"""
        assert detect_language("script.unknown") is None
        assert detect_language("Makefile") is None

    def test_no_extension(self):
        """Test that files without extension return None"""
        assert detect_language("Makefile") is None
        assert detect_language("README") is None

    def test_empty_filename(self):
        """Test that empty filename returns None"""
        assert detect_language("") is None

    def test_dot_files(self):
        """Test dot files (like .gitignore) return None"""
        assert detect_language(".gitignore") is None

    def test_multiple_dots(self):
        """Test files with multiple dots (use last extension)"""
        assert detect_language("app.test.py") == "python"
        assert detect_language("archive.tar.gz") is None

    def test_case_insensitivity(self):
        """Test that detection is case-insensitive for extensions"""
        assert detect_language("script.PY") == "python"
        assert detect_language("script.Py") == "python"
        assert detect_language("script.JS") == "javascript"

    def test_common_languages(self):
        """Test detection for common languages"""
        test_cases = [
            ("helloworld.py", "python"),
            ("index.html", None),  # HTML not supported for execution
            ("style.css", None),   # CSS not supported
            ("app.ts", "typescript"),
            ("main.go", "go"),
            ("fibonacci.rs", "rust"),
        ]

        for filename, expected in test_cases:
            assert detect_language(filename) == expected, f"Failed for {filename}"
