#!/usr/bin/env elixir

# Tests for Un Elixir SDK
#
# Run with: elixir tests/test_library.exs
# Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables

Code.require_file("../src/un.ex", __DIR__)

defmodule UnTest do
  @moduledoc """
  Test suite for Un Elixir SDK library functions.
  """

  @blue "\e[34m"
  @red "\e[31m"
  @green "\e[32m"
  @yellow "\e[33m"
  @reset "\e[0m"

  def run_all do
    IO.puts("\n#{@blue}=== Un Elixir SDK Tests ===#@reset}\n")

    tests = [
      {"version", &test_version/0},
      {"detect_language", &test_detect_language/0},
      {"hmac_sign", &test_hmac_sign/0},
      {"hmac_sign_deterministic", &test_hmac_sign_deterministic/0},
      {"hmac_sign_different_secrets", &test_hmac_sign_different_secrets/0},
      {"get_languages", &test_get_languages/0}
    ]

    results = Enum.map(tests, fn {name, test_fn} ->
      try do
        test_fn.()
        IO.puts("#{@green}PASS#{@reset}: #{name}")
        :pass
      rescue
        e ->
          IO.puts("#{@red}FAIL#{@reset}: #{name} - #{inspect(e)}")
          :fail
      end
    end)

    passed = Enum.count(results, &(&1 == :pass))
    failed = Enum.count(results, &(&1 == :fail))
    total = length(results)

    IO.puts("\n#{@blue}Results: #{passed}/#{total} passed#{@reset}")
    if failed > 0 do
      IO.puts("#{@red}#{failed} test(s) failed#{@reset}")
      System.halt(1)
    else
      IO.puts("#{@green}All tests passed!#{@reset}")
    end
  end

  # ============================================================================
  # Unit Tests
  # ============================================================================

  def test_version do
    version = Un.version()
    assert is_binary(version), "version should be a string"
    assert String.match?(version, ~r/^\d+\.\d+\.\d+$/), "version should be semver format"
  end

  def test_detect_language do
    # Test common extensions
    assert Un.detect_language("script.py") == "python"
    assert Un.detect_language("app.js") == "javascript"
    assert Un.detect_language("main.go") == "go"
    assert Un.detect_language("main.rs") == "rust"
    assert Un.detect_language("main.c") == "c"
    assert Un.detect_language("main.cpp") == "cpp"
    assert Un.detect_language("Main.java") == "java"
    assert Un.detect_language("script.rb") == "ruby"
    assert Un.detect_language("script.sh") == "bash"
    assert Un.detect_language("script.lua") == "lua"
    assert Un.detect_language("script.pl") == "perl"
    assert Un.detect_language("index.php") == "php"
    assert Un.detect_language("main.hs") == "haskell"
    assert Un.detect_language("main.ml") == "ocaml"
    assert Un.detect_language("main.ex") == "elixir"
    assert Un.detect_language("main.erl") == "erlang"

    # Test with paths
    assert Un.detect_language("/path/to/script.py") == "python"

    # Test unknown extensions
    assert Un.detect_language("Makefile") == nil
    assert Un.detect_language("README") == nil
    assert Un.detect_language("script.unknown") == nil
  end

  def test_hmac_sign do
    signature = Un.hmac_sign("my_secret", "test message")
    assert is_binary(signature), "signature should be a string"
    assert String.length(signature) == 64, "signature should be 64 hex characters"
    assert String.match?(signature, ~r/^[0-9a-f]+$/), "signature should be lowercase hex"
  end

  def test_hmac_sign_deterministic do
    sig1 = Un.hmac_sign("test_secret", "same message")
    sig2 = Un.hmac_sign("test_secret", "same message")
    assert sig1 == sig2, "same inputs should produce same signature"
  end

  def test_hmac_sign_different_secrets do
    sig1 = Un.hmac_sign("secret1", "test message")
    sig2 = Un.hmac_sign("secret2", "test message")
    assert sig1 != sig2, "different secrets should produce different signatures"
  end

  def test_get_languages do
    languages = Un.get_languages()
    assert is_list(languages), "languages should be a list"
    assert length(languages) > 0, "languages list should not be empty"
    assert "python" in languages, "python should be in languages"
    assert "javascript" in languages, "javascript should be in languages"
  end

  # ============================================================================
  # Helpers
  # ============================================================================

  defp assert(true, _message), do: :ok
  defp assert(false, message), do: raise message
  defp assert(condition, message) when is_boolean(condition) do
    if condition, do: :ok, else: raise message
  end
end

# Run tests
UnTest.run_all()
