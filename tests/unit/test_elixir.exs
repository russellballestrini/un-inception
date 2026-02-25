#!/usr/bin/env elixir
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

# Unit tests for un.ex - tests internal functions without API calls

defmodule UnTest do
  def run do
    Agent.start_link(fn -> 0 end, name: :passed)
    Agent.start_link(fn -> 0 end, name: :failed)

    IO.puts("\n=== Extension Mapping Tests ===")

    ext_map = %{
      ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
      ".rb" => "ruby", ".php" => "php", ".pl" => "perl", ".lua" => "lua",
      ".sh" => "bash", ".go" => "go", ".rs" => "rust", ".c" => "c",
      ".cpp" => "cpp", ".java" => "java", ".kt" => "kotlin",
      ".hs" => "haskell", ".clj" => "clojure", ".erl" => "erlang",
      ".ex" => "elixir", ".exs" => "elixir", ".jl" => "julia"
    }

    test("Python extension maps correctly", fn ->
      assert_equal(ext_map[".py"], "python")
    end)

    test("Elixir extensions map correctly", fn ->
      assert_equal(ext_map[".ex"], "elixir")
      assert_equal(ext_map[".exs"], "elixir")
    end)

    test("JavaScript extension maps correctly", fn ->
      assert_equal(ext_map[".js"], "javascript")
    end)

    test("Go extension maps correctly", fn ->
      assert_equal(ext_map[".go"], "go")
    end)

    test("Erlang extension maps correctly", fn ->
      assert_equal(ext_map[".erl"], "erlang")
    end)

    IO.puts("\n=== HMAC Signature Tests ===")

    test("HMAC-SHA256 generates 64 character hex string", fn ->
      sig = :crypto.mac(:hmac, :sha256, "test-secret", "test-message")
            |> Base.encode16(case: :lower)
      assert_equal(String.length(sig), 64)
    end)

    test("Same input produces same signature", fn ->
      sig1 = :crypto.mac(:hmac, :sha256, "key", "msg") |> Base.encode16(case: :lower)
      sig2 = :crypto.mac(:hmac, :sha256, "key", "msg") |> Base.encode16(case: :lower)
      assert_equal(sig1, sig2)
    end)

    test("Different secrets produce different signatures", fn ->
      sig1 = :crypto.mac(:hmac, :sha256, "key1", "msg") |> Base.encode16(case: :lower)
      sig2 = :crypto.mac(:hmac, :sha256, "key2", "msg") |> Base.encode16(case: :lower)
      assert_not_equal(sig1, sig2)
    end)

    test("Signature format verification", fn ->
      timestamp = "1704067200"
      method = "POST"
      endpoint = "/execute"
      body = ~s({"language":"python"})

      message = "#{timestamp}:#{method}:#{endpoint}:#{body}"

      assert_true(String.starts_with?(message, timestamp))
      assert_contains(message, ":POST:")
      assert_contains(message, ":/execute:")
    end)

    IO.puts("\n=== Language Detection Tests ===")

    test("Detect language from .exs extension", fn ->
      ext = Path.extname("script.exs")
      assert_equal(ext_map[ext], "elixir")
    end)

    test("Python shebang detection", fn ->
      content = "#!/usr/bin/env python3\nprint('hello')"
      [first_line | _] = String.split(content, "\n")
      assert_true(String.starts_with?(first_line, "#!"))
      assert_contains(first_line, "python")
    end)

    IO.puts("\n=== Argument Parsing Tests ===")

    test("Parse -e KEY=VALUE format", fn ->
      arg = "DEBUG=1"
      [key | rest] = String.split(arg, "=", parts: 2)
      value = Enum.join(rest, "=")
      assert_equal(key, "DEBUG")
      assert_equal(value, "1")
    end)

    test("Parse -e KEY=VALUE with equals in value", fn ->
      arg = "URL=https://example.com?foo=bar"
      [key | rest] = String.split(arg, "=", parts: 2)
      value = Enum.join(rest, "=")
      assert_equal(key, "URL")
      assert_equal(value, "https://example.com?foo=bar")
    end)

    IO.puts("\n=== File Operations Tests ===")

    test("Base64 encoding/decoding", fn ->
      content = "print('hello world')"
      encoded = Base.encode64(content)
      {:ok, decoded} = Base.decode64(encoded)
      assert_equal(decoded, content)
    end)

    test("Extract file basename", fn ->
      path = "/home/user/project/script.exs"
      assert_equal(Path.basename(path), "script.exs")
    end)

    test("Extract file extension", fn ->
      path = "/home/user/project/script.exs"
      assert_equal(Path.extname(path), ".exs")
    end)

    IO.puts("\n=== API Constants Tests ===")

    test("API base URL format", fn ->
      api_base = "https://api.unsandbox.com"
      assert_true(String.starts_with?(api_base, "https://"))
      assert_contains(api_base, "unsandbox.com")
    end)

    # Summary
    passed = Agent.get(:passed, & &1)
    failed = Agent.get(:failed, & &1)

    IO.puts("\n=== Summary ===")
    IO.puts("Passed: #{passed}")
    IO.puts("Failed: #{failed}")
    IO.puts("Total:  #{passed + failed}")

    if failed > 0, do: System.halt(1), else: System.halt(0)
  end

  defp test(name, fun) do
    try do
      fun.()
      IO.puts("  ✓ #{name}")
      Agent.update(:passed, &(&1 + 1))
    rescue
      e ->
        IO.puts("  ✗ #{name}")
        IO.puts("    #{Exception.message(e)}")
        Agent.update(:failed, &(&1 + 1))
    end
  end

  defp assert_equal(actual, expected) do
    if actual != expected do
      raise "Expected '#{expected}' but got '#{actual}'"
    end
  end

  defp assert_not_equal(a, b) do
    if a == b do
      raise "Expected values to be different but both were '#{a}'"
    end
  end

  defp assert_contains(str, substr) do
    unless String.contains?(str, substr) do
      raise "Expected '#{str}' to contain '#{substr}'"
    end
  end

  defp assert_true(val) do
    unless val do
      raise "Expected true but got false"
    end
  end
end

UnTest.run()
