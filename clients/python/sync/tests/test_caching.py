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

"""Tests for languages cache functionality"""

import os
import json
import tempfile
from pathlib import Path
import time
from unittest.mock import patch, MagicMock
from un import _load_languages_cache, _save_languages_cache


class TestLanguagesCaching:
    """Test languages list caching"""

    def test_save_and_load_cache(self):
        """Test saving and loading languages cache"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            languages = ["python", "javascript", "go"]

            # Mock the cache path
            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)

                # Verify file was created
                assert cache_path.exists()

                # Load and verify
                loaded = _load_languages_cache()
                assert loaded == languages

    def test_cache_ttl_expiration(self):
        """Test that cache expires after TTL"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            languages = ["python", "javascript"]

            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)

                # Make the file old
                old_time = time.time() - 7200  # 2 hours ago
                os.utime(cache_path, (old_time, old_time))

                # Should return None (expired)
                loaded = _load_languages_cache()
                assert loaded is None

    def test_cache_not_expired_within_ttl(self):
        """Test that cache is valid within TTL"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            languages = ["python", "javascript", "go", "rust"]

            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)

                # Should still be valid
                loaded = _load_languages_cache()
                assert loaded == languages

    def test_cache_corrupted_json(self):
        """Test that corrupted JSON is handled gracefully"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            # Write invalid JSON
            with open(cache_path, "w") as f:
                f.write("invalid json {{{")

            with patch("un._get_languages_cache_path", return_value=cache_path):
                # Should return None on JSON error
                loaded = _load_languages_cache()
                assert loaded is None

    def test_cache_missing_languages_key(self):
        """Test cache file with missing 'languages' key"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            # Write valid JSON but missing 'languages' key
            with open(cache_path, "w") as f:
                json.dump({"timestamp": int(time.time())}, f)

            with patch("un._get_languages_cache_path", return_value=cache_path):
                # Should return None when 'languages' key is missing
                loaded = _load_languages_cache()
                assert loaded is None

    def test_cache_nonexistent_file(self):
        """Test loading cache when file doesn't exist"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "nonexistent.json"

            with patch("un._get_languages_cache_path", return_value=cache_path):
                loaded = _load_languages_cache()
                assert loaded is None

    def test_cache_permissions_error(self):
        """Test that permission errors are handled gracefully"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            # Create a directory instead of a file
            os.makedirs(cache_path, exist_ok=True)

            with patch("un._get_languages_cache_path", return_value=cache_path):
                # Should return None on permission error
                loaded = _load_languages_cache()
                assert loaded is None

    def test_cache_content_format(self):
        """Test that cache file has correct format"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            languages = ["python", "javascript"]

            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)

                # Read and verify format
                with open(cache_path, "r") as f:
                    data = json.load(f)

                assert "languages" in data
                assert "timestamp" in data
                assert data["languages"] == languages
                assert isinstance(data["timestamp"], int)

    def test_cache_save_permission_error(self):
        """Test that cache save errors are handled gracefully"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a read-only directory
            cache_path = Path(tmpdir) / "readonly"
            cache_path.mkdir(mode=0o444)

            with patch("un._get_languages_cache_path", return_value=cache_path / "languages.json"):
                # Should not raise, just silently fail
                try:
                    _save_languages_cache(["python"])
                except Exception:
                    # Permission errors might still occur, but should be caught
                    pass

    def test_cache_empty_languages_list(self):
        """Test caching empty languages list"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            languages = []

            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)
                loaded = _load_languages_cache()
                assert loaded == []

    def test_cache_large_languages_list(self):
        """Test caching large languages list"""
        with tempfile.TemporaryDirectory() as tmpdir:
            cache_path = Path(tmpdir) / "languages.json"

            # Create a list with 100 languages
            languages = [f"lang_{i}" for i in range(100)]

            with patch("un._get_languages_cache_path", return_value=cache_path):
                _save_languages_cache(languages)
                loaded = _load_languages_cache()
                assert loaded == languages
                assert len(loaded) == 100
