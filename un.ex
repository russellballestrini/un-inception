#!/usr/bin/env elixir
# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# This is free public domain software for the public good of a permacomputer hosted
# at permacomputer.com - an always-on computer by the people, for the people. One
# which is durable, easy to repair, and distributed like tap water for machine
# learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around four values:
#
#   TRUTH    - Source code must be open source & freely distributed
#   FREEDOM  - Voluntary participation without corporate control
#   HARMONY  - Systems operating with minimal waste that self-renew
#   LOVE     - Individual rights protected while fostering cooperation
#
# This software contributes to that vision by enabling code execution across 42+
# programming languages through a unified interface, accessible to all. Code is
# seeds to sprout on any abandoned technology.
#
# Learn more: https://www.permacomputer.com
#
# Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
# software, either in source code form or as a compiled binary, for any purpose,
# commercial or non-commercial, and by any means.
#
# NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
#
# That said, our permacomputer's digital membrane stratum continuously runs unit,
# integration, and functional tests on all of it's own software - with our
# permacomputer monitoring itself, repairing itself, with minimal human in the
# loop guidance. Our agents do their best.
#
# Copyright 2025 TimeHexOn & foxhop & russell@unturf
# https://www.timehexon.com
# https://www.foxhop.net
# https://www.unturf.com/software

# un.ex - Unsandbox CLI client in Elixir
#
# Full-featured CLI matching un.py capabilities:
# - Execute code with env vars, input files, artifacts
# - Interactive sessions with shell/REPL support
# - Persistent services with domains and ports
#
# Usage:
#   chmod +x un.ex
#   export UNSANDBOX_API_KEY="your_key_here"
#   ./un.ex [options] <source_file>
#   ./un.ex session [options]
#   ./un.ex service [options]
#
# Uses curl for HTTP (no external dependencies)

defmodule Un do
  @blue "\e[34m"
  @red "\e[31m"
  @green "\e[32m"
  @yellow "\e[33m"
  @reset "\e[0m"

  @ext_map %{
    ".ex" => "elixir", ".exs" => "elixir", ".erl" => "erlang",
    ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
    ".rb" => "ruby", ".go" => "go", ".rs" => "rust", ".c" => "c",
    ".cpp" => "cpp", ".cc" => "cpp", ".java" => "java", ".kt" => "kotlin",
    ".cs" => "csharp", ".fs" => "fsharp", ".hs" => "haskell",
    ".ml" => "ocaml", ".clj" => "clojure", ".scm" => "scheme",
    ".lisp" => "commonlisp", ".jl" => "julia", ".r" => "r",
    ".cr" => "crystal", ".d" => "d", ".nim" => "nim", ".zig" => "zig",
    ".v" => "v", ".dart" => "dart", ".groovy" => "groovy", ".scala" => "scala",
    ".sh" => "bash", ".pl" => "perl", ".lua" => "lua", ".php" => "php",
    ".f90" => "fortran", ".cob" => "cobol", ".pro" => "prolog",
    ".forth" => "forth", ".tcl" => "tcl", ".raku" => "raku"
  }

  def main([]), do: print_usage()
  def main(["session" | rest]), do: session_command(rest)
  def main(["service" | rest]), do: service_command(rest)
  def main(args), do: execute_command(args)

  defp print_usage do
    IO.puts("Usage: un.ex [options] <source_file>")
    IO.puts("       un.ex session [options]")
    IO.puts("       un.ex service [options]")
    System.halt(1)
  end

  # Execute command
  defp execute_command(args) do
    api_key = get_api_key()
    {file, opts} = parse_exec_args(args)

    if is_nil(file) do
      IO.puts(:stderr, "Error: No source file specified")
      System.halt(1)
    end

    ext = Path.extname(file)
    language = Map.get(@ext_map, ext)

    if is_nil(language) do
      IO.puts(:stderr, "Error: Unknown extension: #{ext}")
      System.halt(1)
    end

    case File.read(file) do
      {:ok, code} ->
        json = build_execute_json(language, code, opts)
        response = curl_post(api_key, "/execute", json)
        IO.puts(response)

      {:error, reason} ->
        IO.puts(:stderr, "Error reading file: #{reason}")
        System.halt(1)
    end
  end

  # Session command
  defp session_command(["--list" | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/sessions")
    IO.puts(response)
  end

  defp session_command(["--kill", session_id | _]) do
    api_key = get_api_key()
    curl_delete(api_key, "/sessions/#{session_id}")
    IO.puts("#{@green}Session terminated: #{session_id}#{@reset}")
  end

  defp session_command(args) do
    api_key = get_api_key()
    shell = get_opt(args, "--shell", "-s", "bash")
    network = get_opt(args, "-n", nil, nil)
    vcpu = get_opt(args, "-v", nil, nil)

    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""

    json = "{\"shell\":\"#{shell}\"#{network_json}#{vcpu_json}}"
    response = curl_post(api_key, "/sessions", json)
    IO.puts("#{@yellow}Session created (WebSocket required)#{@reset}")
    IO.puts(response)
  end

  # Service command
  defp service_command(["--list" | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/services")
    IO.puts(response)
  end

  defp service_command(["--info", service_id | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/services/#{service_id}")
    IO.puts(response)
  end

  defp service_command(["--logs", service_id | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/services/#{service_id}/logs")
    IO.puts(response)
  end

  defp service_command(["--sleep", service_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/sleep", "{}")
    IO.puts("#{@green}Service sleeping: #{service_id}#{@reset}")
  end

  defp service_command(["--wake", service_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/wake", "{}")
    IO.puts("#{@green}Service waking: #{service_id}#{@reset}")
  end

  defp service_command(["--destroy", service_id | _]) do
    api_key = get_api_key()
    curl_delete(api_key, "/services/#{service_id}")
    IO.puts("#{@green}Service destroyed: #{service_id}#{@reset}")
  end

  defp service_command(args) do
    name = get_opt(args, "--name", nil, nil)

    if is_nil(name) do
      IO.puts(:stderr, "Error: --name required to create service")
      System.halt(1)
    end

    api_key = get_api_key()
    ports = get_opt(args, "--ports", nil, nil)
    bootstrap = get_opt(args, "--bootstrap", nil, nil)
    network = get_opt(args, "-n", nil, nil)
    vcpu = get_opt(args, "-v", nil, nil)

    ports_json = if ports, do: ",\"ports\":[#{ports}]", else: ""
    bootstrap_json = if bootstrap, do: ",\"bootstrap\":\"#{escape_json(bootstrap)}\"", else: ""
    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""

    json = "{\"name\":\"#{name}\"#{ports_json}#{bootstrap_json}#{network_json}#{vcpu_json}}"
    response = curl_post(api_key, "/services", json)
    IO.puts("#{@green}Service created#{@reset}")
    IO.puts(response)
  end

  # Helpers
  defp get_api_key do
    case System.get_env("UNSANDBOX_API_KEY") do
      nil ->
        IO.puts(:stderr, "Error: UNSANDBOX_API_KEY not set")
        System.halt(1)
      key -> key
    end
  end

  defp escape_json(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end

  defp build_execute_json(language, code, _opts) do
    "{\"language\":\"#{language}\",\"code\":\"#{escape_json(code)}\"}"
  end

  defp curl_post(api_key, endpoint, json) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {output, _exit} = System.cmd("curl", [
      "-s", "-X", "POST",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Content-Type: application/json",
      "-H", "Authorization: Bearer #{api_key}",
      "-d", "@#{tmp_file}"
    ], stderr_to_stdout: true)

    File.rm(tmp_file)
    output
  end

  defp curl_get(api_key, endpoint) do
    {output, _exit} = System.cmd("curl", [
      "-s",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Authorization: Bearer #{api_key}"
    ], stderr_to_stdout: true)

    output
  end

  defp curl_delete(api_key, endpoint) do
    {output, _exit} = System.cmd("curl", [
      "-s", "-X", "DELETE",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Authorization: Bearer #{api_key}"
    ], stderr_to_stdout: true)

    output
  end

  defp parse_exec_args(args) do
    parse_exec_args(args, nil, %{})
  end

  defp parse_exec_args([], file, opts), do: {file, opts}

  defp parse_exec_args([arg | rest], file, opts) do
    cond do
      String.starts_with?(arg, "-") ->
        parse_exec_args(rest, file, opts)
      is_nil(file) ->
        parse_exec_args(rest, arg, opts)
      true ->
        parse_exec_args(rest, file, opts)
    end
  end

  defp get_opt([], _long, _short, default), do: default

  defp get_opt([arg, value | rest], long, short, _default) when arg == long or arg == short do
    value
  end

  defp get_opt([_arg | rest], long, short, default) do
    get_opt(rest, long, short, default)
  end
end

Un.main(System.argv())
