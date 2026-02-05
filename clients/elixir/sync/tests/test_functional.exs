#!/usr/bin/env elixir

# Functional Tests for Un Elixir SDK
#
# Run with: elixir tests/test_functional.exs
# Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables
#
# These tests make real API calls to api.unsandbox.com

Code.require_file("../src/un.ex", __DIR__)

defmodule UnFunctionalTest do
  @moduledoc """
  Functional test suite for Un Elixir SDK.
  Tests real API calls to api.unsandbox.com.
  """

  @blue "\e[34m"
  @red "\e[31m"
  @green "\e[32m"
  @yellow "\e[33m"
  @reset "\e[0m"

  def run_all do
    IO.puts("\n#{@blue}=== Un Elixir SDK Functional Tests ===#@reset}\n")

    # Check for credentials
    unless System.get_env("UNSANDBOX_PUBLIC_KEY") && System.get_env("UNSANDBOX_SECRET_KEY") do
      IO.puts("#{@yellow}SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set#{@reset}")
      System.halt(0)
    end

    tests = [
      {"health_check", &test_health_check/0},
      {"validate_keys", &test_validate_keys/0},
      {"execute_python", &test_execute_python/0},
      {"execute_with_error", &test_execute_with_error/0},
      {"session_list", &test_session_list/0},
      {"service_list", &test_service_list/0},
      {"snapshot_list", &test_snapshot_list/0},
      {"image_list", &test_image_list/0}
    ]

    results = Enum.map(tests, fn {name, test_fn} ->
      IO.write("  Running #{name}... ")
      try do
        test_fn.()
        IO.puts("#{@green}PASS#{@reset}")
        :pass
      rescue
        e ->
          IO.puts("#{@red}FAIL#{@reset}")
          IO.puts("    #{inspect(e)}")
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
      IO.puts("#{@green}All functional tests passed!#{@reset}")
    end
  end

  # ============================================================================
  # Functional Tests
  # ============================================================================

  def test_health_check do
    result = Un.health_check()
    assert is_boolean(result), "health_check should return boolean"
    # Note: We don't require it to be true in case API is down
  end

  def test_validate_keys do
    key_info = Un.validate_keys()
    assert is_map(key_info), "validate_keys should return a map"
    assert Map.has_key?(key_info, :valid), "key_info should have :valid key"
    assert is_boolean(key_info.valid), ":valid should be boolean"
  end

  def test_execute_python do
    result = Un.execute("python", "print(6 * 7)")
    assert is_map(result), "execute should return a map"
    assert Map.has_key?(result, :success), "result should have :success key"
    assert Map.has_key?(result, :stdout), "result should have :stdout key"
    assert Map.has_key?(result, :exit_code), "result should have :exit_code key"

    # Check output
    assert result.success == true, "execution should succeed"
    assert String.contains?(result.stdout, "42"), "stdout should contain '42'"
    assert result.exit_code == 0, "exit_code should be 0"
  end

  def test_execute_with_error do
    result = Un.execute("python", "import sys; sys.exit(1)")
    assert is_map(result), "execute should return a map"
    assert result.success == false, "execution should fail"
    assert result.exit_code == 1, "exit_code should be 1"
  end

  def test_session_list do
    response = Un.session_list()
    assert is_binary(response), "session_list should return a string"
    # Response should be valid JSON (starts with [ or {)
    trimmed = String.trim(response)
    assert String.starts_with?(trimmed, "[") || String.starts_with?(trimmed, "{"),
           "response should be JSON"
  end

  def test_service_list do
    response = Un.service_list()
    assert is_binary(response), "service_list should return a string"
    trimmed = String.trim(response)
    assert String.starts_with?(trimmed, "[") || String.starts_with?(trimmed, "{"),
           "response should be JSON"
  end

  def test_snapshot_list do
    response = Un.snapshot_list()
    assert is_binary(response), "snapshot_list should return a string"
    trimmed = String.trim(response)
    assert String.starts_with?(trimmed, "[") || String.starts_with?(trimmed, "{"),
           "response should be JSON"
  end

  def test_image_list do
    response = Un.image_list()
    assert is_binary(response), "image_list should return a string"
    trimmed = String.trim(response)
    assert String.starts_with?(trimmed, "[") || String.starts_with?(trimmed, "{"),
           "response should be JSON"
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
UnFunctionalTest.run_all()
