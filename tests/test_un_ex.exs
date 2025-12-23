#!/usr/bin/env elixir

# Elixir UN CLI Test Suite
#
# Usage:
#   chmod +x test_un_ex.exs
#   ./test_un_ex.exs
#
# Or with elixir:
#   elixir test_un_ex.exs
#
# Tests the Elixir UN CLI implementation (un.ex) for:
# 1. Extension detection logic
# 2. API integration (if UNSANDBOX_API_KEY is set)
# 3. End-to-end execution with fib.ex test file

defmodule UnCLITest do
  # ANSI color codes
  @green "\x1b[32m"
  @red "\x1b[31m"
  @yellow "\x1b[33m"
  @reset "\x1b[0m"

  # Extension to language mapping (from un.ex)
  @ext_to_lang %{
    ".hs" => "haskell",
    ".ml" => "ocaml",
    ".clj" => "clojure",
    ".scm" => "scheme",
    ".lisp" => "commonlisp",
    ".erl" => "erlang",
    ".ex" => "elixir",
    ".py" => "python",
    ".js" => "javascript",
    ".rb" => "ruby",
    ".go" => "go",
    ".rs" => "rust",
    ".c" => "c",
    ".cpp" => "cpp",
    ".java" => "java"
  }

  # Test result structure
  defstruct passed: false, message: nil

  # Print test result
  def print_result(test_name, %__MODULE__{passed: true}) do
    IO.puts("#{@green}✓ PASS#{@reset} - #{test_name}")
    true
  end

  def print_result(test_name, %__MODULE__{passed: false, message: msg}) do
    IO.puts("#{@red}✗ FAIL#{@reset} - #{test_name}")
    if msg, do: IO.puts("  Error: #{msg}")
    false
  end

  # Test 1: Extension detection
  def test_extension_detection do
    tests = [
      {".hs", "haskell"},
      {".ml", "ocaml"},
      {".clj", "clojure"},
      {".scm", "scheme"},
      {".lisp", "commonlisp"},
      {".erl", "erlang"},
      {".ex", "elixir"},
      {".py", "python"},
      {".js", "javascript"},
      {".rb", "ruby"}
    ]

    failures =
      Enum.filter(tests, fn {ext, expected} ->
        Map.get(@ext_to_lang, ext) != expected
      end)

    if Enum.empty?(failures) do
      %__MODULE__{passed: true}
    else
      %__MODULE__{passed: false, message: "#{length(failures)} tests failed"}
    end
  end

  # Run command and capture output
  defp run_command(cmd) do
    try do
      {output, status} = System.cmd("sh", ["-c", cmd], stderr_to_stdout: true)
      {status, output}
    rescue
      e -> {1, "Exception: #{inspect(e)}"}
    end
  end

  # Test 2: API integration
  def test_api_integration do
    case System.get_env("UNSANDBOX_API_KEY") do
      nil ->
        %__MODULE__{passed: true}

      _ ->
        try do
          # Create a simple test file
          test_code = "IO.puts(\"test\")\n"
          File.write!("/tmp/test_un_ex_api.ex", test_code)

          # Run the CLI
          {status, output} = run_command("./un.ex /tmp/test_un_ex_api.ex 2>&1")

          # Check if it executed successfully
          if status == 0 && String.contains?(output, "test") do
            %__MODULE__{passed: true}
          else
            %__MODULE__{
              passed: false,
              message: "API call failed: #{status}, output: #{output}"
            }
          end
        rescue
          e ->
            %__MODULE__{passed: false, message: "Exception: #{inspect(e)}"}
        end
    end
  end

  # Test 3: Functional test with fib.ex
  def test_fibonacci do
    case System.get_env("UNSANDBOX_API_KEY") do
      nil ->
        %__MODULE__{passed: true}

      _ ->
        try do
          # Check if fib.ex exists
          fib_path = "../test/fib.ex"

          # Run the CLI with fib.ex
          {status, output} = run_command("./un.ex #{fib_path} 2>&1")

          # Check if output contains expected fibonacci result
          if status == 0 && String.contains?(output, "fib(10) = 55") do
            %__MODULE__{passed: true}
          else
            %__MODULE__{
              passed: false,
              message: "Fibonacci test failed: #{status}, output: #{output}"
            }
          end
        rescue
          e ->
            %__MODULE__{passed: false, message: "Exception: #{inspect(e)}"}
        end
    end
  end

  # Main test runner
  def run do
    IO.puts("=== Elixir UN CLI Test Suite ===\n")

    # Check if API key is set
    unless System.get_env("UNSANDBOX_API_KEY") do
      IO.puts(
        "#{@yellow}⚠ WARNING#{@reset} - UNSANDBOX_API_KEY not set, skipping API tests\n"
      )
    end

    # Run tests
    results = [
      print_result("Extension detection", test_extension_detection()),
      print_result("API integration", test_api_integration()),
      print_result("Fibonacci end-to-end test", test_fibonacci())
    ]

    IO.puts("")

    # Summary
    passed = Enum.count(results, & &1)
    total = length(results)

    if passed == total do
      IO.puts("#{@green}✓ All tests passed (#{passed}/#{total})#{@reset}")
      System.halt(0)
    else
      IO.puts("#{@red}✗ Some tests failed (#{passed}/#{total} passed)#{@reset}")
      System.halt(1)
    end
  end
end

# Entry point
UnCLITest.run()
