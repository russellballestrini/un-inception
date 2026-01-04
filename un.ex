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
#   TRUTH    - First principles, math & science, open source code freely distributed
#   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE     - Be yourself without hurting others, cooperation through natural law
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

  @portal_base "https://unsandbox.com"

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
  def main(["key" | rest]), do: key_command(rest)
  def main(args), do: execute_command(args)

  defp print_usage do
    IO.puts("Usage: un.ex [options] <source_file>")
    IO.puts("       un.ex session [options]")
    IO.puts("       un.ex service [options]")
    IO.puts("       un.ex key [--extend]")
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
    validate_session_args(args)
    api_key = get_api_key()
    shell = get_opt(args, "--shell", "-s", "bash")
    network = get_opt(args, "-n", nil, nil)
    vcpu = get_opt(args, "-v", nil, nil)
    input_files = get_all_opts(args, "-f")

    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""
    input_files_json = build_input_files_json(input_files)

    json = "{\"shell\":\"#{shell}\"#{network_json}#{vcpu_json}#{input_files_json}}"
    response = curl_post(api_key, "/sessions", json)
    IO.puts("#{@yellow}Session created (WebSocket required)#{@reset}")
    IO.puts(response)
  end

  defp validate_session_args([]), do: :ok
  defp validate_session_args(["--shell", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["-s", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["-f", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["-n", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["-v", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args([arg | _]) do
    if String.starts_with?(arg, "-") do
      IO.puts(:stderr, "Unknown option: #{arg}")
      IO.puts(:stderr, "Usage: un.ex session [options]")
      System.halt(1)
    else
      validate_session_args([])
    end
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

  defp service_command(["--freeze", service_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/sleep", "{}")
    IO.puts("#{@green}Service sleeping: #{service_id}#{@reset}")
  end

  defp service_command(["--unfreeze", service_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/wake", "{}")
    IO.puts("#{@green}Service waking: #{service_id}#{@reset}")
  end

  defp service_command(["--destroy", service_id | _]) do
    api_key = get_api_key()
    curl_delete(api_key, "/services/#{service_id}")
    IO.puts("#{@green}Service destroyed: #{service_id}#{@reset}")
  end

  defp service_command(["--execute", service_id, "--command", command | _]) do
    api_key = get_api_key()
    json = "{\"command\":\"#{escape_json(command)}\"}"
    response = curl_post(api_key, "/services/#{service_id}/execute", json)

    case extract_json_value(response, "stdout") do
      nil -> :ok
      stdout -> IO.write("#{@blue}#{stdout}#{@reset}")
    end
  end

  defp service_command(["--dump-bootstrap", service_id, file | _]) do
    api_key = get_api_key()
    IO.puts(:stderr, "Fetching bootstrap script from #{service_id}...")
    json = "{\"command\":\"cat /tmp/bootstrap.sh\"}"
    response = curl_post(api_key, "/services/#{service_id}/execute", json)

    case extract_json_value(response, "stdout") do
      nil ->
        IO.puts(:stderr, "#{@red}Error: Failed to fetch bootstrap (service not running or no bootstrap file)#{@reset}")
        System.halt(1)
      script ->
        File.write!(file, script)
        System.cmd("chmod", ["755", file])
        IO.puts("Bootstrap saved to #{file}")
    end
  end

  defp service_command(["--dump-bootstrap", service_id | _]) do
    api_key = get_api_key()
    IO.puts(:stderr, "Fetching bootstrap script from #{service_id}...")
    json = "{\"command\":\"cat /tmp/bootstrap.sh\"}"
    response = curl_post(api_key, "/services/#{service_id}/execute", json)

    case extract_json_value(response, "stdout") do
      nil ->
        IO.puts(:stderr, "#{@red}Error: Failed to fetch bootstrap (service not running or no bootstrap file)#{@reset}")
        System.halt(1)
      script ->
        IO.write(script)
    end
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
    bootstrap_file = get_opt(args, "--bootstrap-file", nil, nil)
    network = get_opt(args, "-n", nil, nil)
    vcpu = get_opt(args, "-v", nil, nil)
    service_type = get_opt(args, "--type", nil, nil)
    input_files = get_all_opts(args, "-f")

    ports_json = if ports, do: ",\"ports\":[#{ports}]", else: ""
    bootstrap_json = if bootstrap, do: ",\"bootstrap\":\"#{escape_json(bootstrap)}\"", else: ""
    bootstrap_content_json = if bootstrap_file do
      case File.read(bootstrap_file) do
        {:ok, content} -> ",\"bootstrap_content\":\"#{escape_json(content)}\""
        {:error, _} ->
          IO.puts(:stderr, "#{@red}Error: Bootstrap file not found: #{bootstrap_file}#{@reset}")
          System.halt(1)
      end
    else
      ""
    end
    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""
    type_json = if service_type, do: ",\"service_type\":\"#{service_type}\"", else: ""
    input_files_json = build_input_files_json(input_files)

    json = "{\"name\":\"#{name}\"#{ports_json}#{bootstrap_json}#{bootstrap_content_json}#{network_json}#{vcpu_json}#{type_json}#{input_files_json}}"
    response = curl_post(api_key, "/services", json)
    IO.puts("#{@green}Service created#{@reset}")
    IO.puts(response)
  end

  # Key command
  defp key_command(args) do
    api_key = get_api_key()

    if "--extend" in args do
      validate_key(api_key, extend: true)
    else
      validate_key(api_key, extend: false)
    end
  end

  defp validate_key(api_key, extend: extend) do
    json = "{}"
    response = portal_curl_post(api_key, "/keys/validate", json)

    # Try to use Jason if available, otherwise fall back to manual parsing
    try do
      case Jason.decode(response) do
        {:ok, data} ->
          display_key_info(data, extend)

        {:error, _} ->
          # Fallback if Jason is not available, parse manually
          display_key_info_manual(response, extend)
      end
    rescue
      UndefinedFunctionError ->
        # If Jason module doesn't exist, use manual parsing
        display_key_info_manual(response, extend)
    end
  end

  defp display_key_info(data, extend) do
    status = Map.get(data, "status")
    public_key = Map.get(data, "public_key")
    tier = Map.get(data, "tier")
    expires_at = Map.get(data, "expires_at")
    time_remaining = Map.get(data, "time_remaining")
    rate_limit = Map.get(data, "rate_limit")
    burst = Map.get(data, "burst")
    concurrency = Map.get(data, "concurrency")

    case status do
      "valid" ->
        IO.puts("#{@green}Valid#{@reset}")
        IO.puts("Public Key: #{public_key}")
        IO.puts("Tier: #{tier}")
        IO.puts("Status: #{status}")
        IO.puts("Expires: #{expires_at}")
        if time_remaining, do: IO.puts("Time Remaining: #{time_remaining}")
        if rate_limit, do: IO.puts("Rate Limit: #{rate_limit}")
        if burst, do: IO.puts("Burst: #{burst}")
        if concurrency, do: IO.puts("Concurrency: #{concurrency}")

        if extend do
          open_browser("#{@portal_base}/keys/extend?pk=#{public_key}")
        end

      "expired" ->
        IO.puts("#{@red}Expired#{@reset}")
        IO.puts("Public Key: #{public_key}")
        IO.puts("Tier: #{tier}")
        IO.puts("Expired: #{expires_at}")
        IO.puts("#{@yellow}To renew: Visit #{@portal_base}/keys/extend#{@reset}")

        if extend do
          open_browser("#{@portal_base}/keys/extend?pk=#{public_key}")
        end

      "invalid" ->
        IO.puts("#{@red}Invalid#{@reset}")

      _ ->
        IO.puts("#{@red}Unknown status: #{status}#{@reset}")
    end
  end

  defp display_key_info_manual(response, extend) do
    # Simple manual parsing for JSON response
    status = extract_json_value(response, "status")
    public_key = extract_json_value(response, "public_key")
    tier = extract_json_value(response, "tier")
    expires_at = extract_json_value(response, "expires_at")
    time_remaining = extract_json_value(response, "time_remaining")
    rate_limit = extract_json_value(response, "rate_limit")
    burst = extract_json_value(response, "burst")
    concurrency = extract_json_value(response, "concurrency")

    case status do
      "valid" ->
        IO.puts("#{@green}Valid#{@reset}")
        IO.puts("Public Key: #{public_key}")
        IO.puts("Tier: #{tier}")
        IO.puts("Status: #{status}")
        IO.puts("Expires: #{expires_at}")
        if time_remaining, do: IO.puts("Time Remaining: #{time_remaining}")
        if rate_limit, do: IO.puts("Rate Limit: #{rate_limit}")
        if burst, do: IO.puts("Burst: #{burst}")
        if concurrency, do: IO.puts("Concurrency: #{concurrency}")

        if extend do
          open_browser("#{@portal_base}/keys/extend?pk=#{public_key}")
        end

      "expired" ->
        IO.puts("#{@red}Expired#{@reset}")
        IO.puts("Public Key: #{public_key}")
        IO.puts("Tier: #{tier}")
        IO.puts("Expired: #{expires_at}")
        IO.puts("#{@yellow}To renew: Visit #{@portal_base}/keys/extend#{@reset}")

        if extend do
          open_browser("#{@portal_base}/keys/extend?pk=#{public_key}")
        end

      "invalid" ->
        IO.puts("#{@red}Invalid#{@reset}")

      _ ->
        IO.puts("#{@red}Unknown status: #{status}#{@reset}")
        IO.puts(response)
    end
  end

  defp extract_json_value(json_str, key) do
    case Regex.run(~r/"#{key}"\s*:\s*"([^"]*)"/, json_str) do
      [_, value] -> value
      _ -> nil
    end
  end

  defp open_browser(url) do
    IO.puts("#{@blue}Opening browser: #{url}#{@reset}")

    case :os.type() do
      {:unix, :linux} ->
        System.cmd("xdg-open", [url], stderr_to_stdout: true)
      {:unix, :darwin} ->
        System.cmd("open", [url], stderr_to_stdout: true)
      {:win32, _} ->
        System.cmd("cmd", ["/c", "start", url], stderr_to_stdout: true)
      _ ->
        IO.puts("#{@yellow}Please open manually: #{url}#{@reset}")
    end
  end

  # Helpers
  defp get_api_keys do
    public_key = System.get_env("UNSANDBOX_PUBLIC_KEY")
    secret_key = System.get_env("UNSANDBOX_SECRET_KEY")

    # Fall back to UNSANDBOX_API_KEY for backwards compatibility
    api_key = System.get_env("UNSANDBOX_API_KEY")

    cond do
      public_key && secret_key ->
        {public_key, secret_key}
      api_key ->
        {api_key, nil}
      true ->
        IO.puts(:stderr, "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)")
        System.halt(1)
    end
  end

  defp get_api_key do
    {public_key, _} = get_api_keys()
    public_key
  end

  defp hmac_sha256(secret, message) do
    :crypto.mac(:hmac, :sha256, secret, message)
    |> Base.encode16(case: :lower)
  end

  defp make_signature(secret_key, timestamp, method, path, body) do
    message = "#{timestamp}:#{method}:#{path}:#{body}"
    hmac_sha256(secret_key, message)
  end

  defp escape_json(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end

  defp read_and_base64(filepath) do
    case File.read(filepath) do
      {:ok, content} -> Base.encode64(content)
      {:error, _} -> ""
    end
  end

  defp build_input_files_json([]), do: ""
  defp build_input_files_json(files) do
    file_jsons = files
      |> Enum.map(fn f ->
        b64 = read_and_base64(f)
        basename = Path.basename(f)
        "{\"filename\":\"#{escape_json(basename)}\",\"content\":\"#{b64}\"}"
      end)
      |> Enum.join(",")
    ",\"input_files\":[#{file_jsons}]"
  end

  defp build_execute_json(language, code, _opts) do
    "{\"language\":\"#{language}\",\"code\":\"#{escape_json(code)}\"}"
  end

  defp curl_post(api_key, endpoint, json) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "POST", endpoint, json)

    args = [
      "-s", "-X", "POST",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Content-Type: application/json"
    ] ++ headers ++ ["-d", "@#{tmp_file}"]

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp build_auth_headers(public_key, secret_key, method, path, body) do
    if secret_key do
      timestamp = System.system_time(:second) |> Integer.to_string()
      signature = make_signature(secret_key, timestamp, method, path, body)
      [
        "-H", "Authorization: Bearer #{public_key}",
        "-H", "X-Timestamp: #{timestamp}",
        "-H", "X-Signature: #{signature}"
      ]
    else
      # Backwards compatibility: use simple bearer token
      ["-H", "Authorization: Bearer #{public_key}"]
    end
  end

  defp portal_curl_post(api_key, endpoint, json) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "POST", endpoint, json)

    args = [
      "-s", "-X", "POST",
      "#{@portal_base}#{endpoint}",
      "-H", "Content-Type: application/json"
    ] ++ headers ++ ["-d", "@#{tmp_file}"]

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp curl_get(api_key, endpoint) do
    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "GET", endpoint, "")

    args = [
      "-s",
      "https://api.unsandbox.com#{endpoint}"
    ] ++ headers

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    check_clock_drift(output)
    output
  end

  defp curl_delete(api_key, endpoint) do
    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "DELETE", endpoint, "")

    args = [
      "-s", "-X", "DELETE",
      "https://api.unsandbox.com#{endpoint}"
    ] ++ headers

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    check_clock_drift(output)
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

  defp get_all_opts(args, flag), do: get_all_opts(args, flag, [])

  defp get_all_opts([], _flag, acc), do: Enum.reverse(acc)

  defp get_all_opts([arg, value | rest], flag, acc) when arg == flag do
    get_all_opts(rest, flag, [value | acc])
  end

  defp get_all_opts([_arg | rest], flag, acc) do
    get_all_opts(rest, flag, acc)
  end

  defp check_clock_drift(response) do
    response_lower = String.downcase(response)

    # Check if response contains "timestamp" and error indicators
    has_timestamp = String.contains?(response_lower, "timestamp")
    has_error = String.contains?(response_lower, "401") or
                String.contains?(response_lower, "expired") or
                String.contains?(response_lower, "invalid")

    if has_timestamp and has_error do
      IO.puts(:stderr, "#{@red}Error: Request timestamp expired (must be within 5 minutes of server time)#{@reset}")
      IO.puts(:stderr, "#{@yellow}Your computer's clock may have drifted.")
      IO.puts(:stderr, "Check your system time and sync with NTP if needed:")
      IO.puts(:stderr, "  Linux:   sudo ntpdate -s time.nist.gov")
      IO.puts(:stderr, "  macOS:   sudo sntp -sS time.apple.com")
      IO.puts(:stderr, "  Windows: w32tm /resync#{@reset}")
      System.halt(1)
    end
  end
end

Un.main(System.argv())
