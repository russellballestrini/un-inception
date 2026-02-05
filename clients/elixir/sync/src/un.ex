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
  @moduledoc """
  unsandbox.com Elixir SDK - Full API with execution, sessions, services, snapshots, and images.

  ## Library Usage

      # Execute code synchronously
      result = Un.execute("python", "print(42)")
      IO.puts(result.stdout)

      # List sessions
      sessions = Un.session_list()

      # Create a service
      service_id = Un.service_create("myapp", ports: "8080")

  ## Authentication

  Credentials are loaded in priority order:
  1. Function arguments (public_key, secret_key)
  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
  3. Config file (~/.unsandbox/accounts.csv)
  """

  @blue "\e[34m"
  @red "\e[31m"
  @green "\e[32m"
  @yellow "\e[33m"
  @reset "\e[0m"

  @api_base "https://api.unsandbox.com"
  @portal_base "https://unsandbox.com"
  @languages_cache_ttl 3600
  @version "4.2.0"

  # ============================================================================
  # Types
  # ============================================================================

  @type result :: %{
    success: boolean(),
    stdout: String.t(),
    stderr: String.t(),
    exit_code: integer(),
    job_id: String.t() | nil,
    language: String.t() | nil,
    execution_time: float() | nil
  }

  @type job :: %{
    id: String.t(),
    status: String.t(),
    language: String.t() | nil,
    created_at: integer() | nil,
    completed_at: integer() | nil
  }

  @type session :: %{
    id: String.t(),
    status: String.t(),
    container_name: String.t() | nil,
    network_mode: String.t() | nil,
    vcpu: integer() | nil,
    created_at: integer() | nil
  }

  @type service :: %{
    id: String.t(),
    name: String.t(),
    status: String.t(),
    ports: String.t() | nil,
    domains: String.t() | nil,
    vcpu: integer() | nil,
    locked: boolean(),
    unfreeze_on_demand: boolean(),
    created_at: integer() | nil
  }

  @type snapshot :: %{
    id: String.t(),
    name: String.t() | nil,
    type: String.t(),
    source_id: String.t(),
    hot: boolean(),
    locked: boolean(),
    created_at: integer() | nil,
    size_bytes: integer() | nil
  }

  @type image :: %{
    id: String.t(),
    name: String.t() | nil,
    description: String.t() | nil,
    visibility: String.t(),
    source_type: String.t(),
    source_id: String.t(),
    locked: boolean(),
    created_at: integer() | nil,
    size_bytes: integer() | nil
  }

  @type key_info :: %{
    valid: boolean(),
    tier: String.t() | nil,
    rate_limit_per_minute: integer() | nil,
    concurrency_limit: integer() | nil,
    expires_at: integer() | nil
  }

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

  # ============================================================================
  # Utility Functions
  # ============================================================================

  @doc """
  Return the SDK version.
  """
  @spec version() :: String.t()
  def version, do: @version

  @doc """
  Check API health.

  Returns true if API is healthy, false otherwise.
  """
  @spec health_check() :: boolean()
  def health_check do
    try do
      {output, 0} = System.cmd("curl", ["-s", "-o", "/dev/null", "-w", "%{http_code}", "#{@api_base}/health"])
      String.trim(output) == "200"
    rescue
      _ -> false
    end
  end

  @doc """
  Generate HMAC-SHA256 signature for a message.
  """
  @spec hmac_sign(String.t(), String.t()) :: String.t()
  def hmac_sign(secret_key, message) do
    hmac_sha256(secret_key, message)
  end

  @doc """
  Detect language from filename extension.
  """
  @spec detect_language(String.t()) :: String.t() | nil
  def detect_language(filename) do
    ext = Path.extname(filename) |> String.downcase()
    Map.get(@ext_map, ext)
  end

  # ============================================================================
  # Execution Functions (8)
  # ============================================================================

  @doc """
  Execute code synchronously.

  ## Options
    * `:network` - Network mode ("zerotrust" or "semitrusted")
    * `:vcpu` - Number of vCPUs (1-8)
    * `:ttl` - Time to live in seconds
    * `:env` - Environment variables as keyword list
    * `:input_files` - List of file paths to include
    * `:return_artifacts` - Return compiled artifacts
    * `:public_key` - API public key (optional)
    * `:secret_key` - API secret key (optional)

  ## Examples

      result = Un.execute("python", "print('Hello World')")
      IO.puts(result.stdout)

  """
  @spec execute(String.t(), String.t(), keyword()) :: result()
  def execute(language, code, opts \\ []) do
    json = build_execute_json_full(language, code, opts)
    response = api_post("/execute", json, opts)
    parse_result(response)
  end

  @doc """
  Execute code asynchronously, returning a job ID.
  """
  @spec execute_async(String.t(), String.t(), keyword()) :: String.t() | nil
  def execute_async(language, code, opts \\ []) do
    json = build_execute_json_full(language, code, opts)
    response = api_post("/execute/async", json, opts)
    extract_json_value(response, "job_id")
  end

  @doc """
  Wait for a job to complete and return the result.
  """
  @spec wait_job(String.t(), keyword()) :: result()
  def wait_job(job_id, opts \\ []) do
    poll_delays = [300, 450, 700, 900, 650, 1600, 2000]
    max_polls = Keyword.get(opts, :max_polls, 100)
    do_wait_job(job_id, poll_delays, 0, max_polls, opts)
  end

  defp do_wait_job(job_id, poll_delays, poll_count, max_polls, opts) when poll_count >= max_polls do
    %{success: false, stdout: "", stderr: "Max polls exceeded", exit_code: 1, job_id: job_id, language: nil, execution_time: nil}
  end

  defp do_wait_job(job_id, poll_delays, poll_count, max_polls, opts) do
    delay_idx = min(poll_count, length(poll_delays) - 1)
    delay = Enum.at(poll_delays, delay_idx)
    Process.sleep(delay)

    job = get_job(job_id, opts)
    case job.status do
      status when status in ["completed", "failed", "timeout", "cancelled"] ->
        response = api_get("/jobs/#{job_id}", opts)
        parse_result(response)
      _ ->
        do_wait_job(job_id, poll_delays, poll_count + 1, max_polls, opts)
    end
  end

  @doc """
  Get job status and details.
  """
  @spec get_job(String.t(), keyword()) :: job()
  def get_job(job_id, opts \\ []) do
    response = api_get("/jobs/#{job_id}", opts)
    %{
      id: job_id,
      status: extract_json_value(response, "status") || "unknown",
      language: extract_json_value(response, "language"),
      created_at: extract_json_int(response, "created_at"),
      completed_at: extract_json_int(response, "completed_at")
    }
  end

  @doc """
  Cancel a running job.
  """
  @spec cancel_job(String.t(), keyword()) :: boolean()
  def cancel_job(job_id, opts \\ []) do
    response = api_delete("/jobs/#{job_id}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  List all active jobs.
  """
  @spec list_jobs(keyword()) :: String.t()
  def list_jobs(opts \\ []) do
    api_get("/jobs", opts)
  end

  @doc """
  Get list of supported languages.
  """
  @spec get_languages(keyword()) :: [String.t()]
  def get_languages(opts \\ []) do
    case load_languages_cache() do
      nil ->
        response = api_get("/languages", opts)
        langs = extract_json_array(response, "languages")
        save_languages_cache(langs)
        langs
      cached ->
        cached
    end
  end

  # ============================================================================
  # Session Functions (9)
  # ============================================================================

  @doc """
  List all sessions.
  """
  @spec session_list(keyword()) :: String.t()
  def session_list(opts \\ []), do: api_get("/sessions", opts)

  @doc """
  Get session details.
  """
  @spec session_get(String.t(), keyword()) :: session()
  def session_get(session_id, opts \\ []) do
    response = api_get("/sessions/#{session_id}", opts)
    %{
      id: session_id,
      status: extract_json_value(response, "status") || "unknown",
      container_name: extract_json_value(response, "container_name"),
      network_mode: extract_json_value(response, "network_mode"),
      vcpu: extract_json_int(response, "vcpu"),
      created_at: extract_json_int(response, "created_at")
    }
  end

  @doc """
  Create a new session.

  ## Options
    * `:shell` - Shell to use (default "bash")
    * `:network` - Network mode
    * `:vcpu` - Number of vCPUs
    * `:input_files` - List of file paths
  """
  @spec session_create(keyword()) :: session()
  def session_create(opts \\ []) do
    shell = Keyword.get(opts, :shell, "bash")
    network = Keyword.get(opts, :network)
    vcpu = Keyword.get(opts, :vcpu)
    input_files = Keyword.get(opts, :input_files, [])

    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""
    input_files_json = build_input_files_json(input_files)

    json = "{\"shell\":\"#{shell}\"#{network_json}#{vcpu_json}#{input_files_json}}"
    response = api_post("/sessions", json, opts)

    %{
      id: extract_json_value(response, "id") || "",
      status: extract_json_value(response, "status") || "created",
      container_name: extract_json_value(response, "container_name"),
      network_mode: extract_json_value(response, "network_mode"),
      vcpu: extract_json_int(response, "vcpu"),
      created_at: extract_json_int(response, "created_at")
    }
  end

  @doc """
  Destroy a session.
  """
  @spec session_destroy(String.t(), keyword()) :: boolean()
  def session_destroy(session_id, opts \\ []) do
    response = api_delete("/sessions/#{session_id}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Freeze a session.
  """
  @spec session_freeze(String.t(), keyword()) :: boolean()
  def session_freeze(session_id, opts \\ []) do
    response = api_post("/sessions/#{session_id}/freeze", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unfreeze a session.
  """
  @spec session_unfreeze(String.t(), keyword()) :: boolean()
  def session_unfreeze(session_id, opts \\ []) do
    response = api_post("/sessions/#{session_id}/unfreeze", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Boost session resources (increase vCPU).
  """
  @spec session_boost(String.t(), integer(), keyword()) :: boolean()
  def session_boost(session_id, vcpu, opts \\ []) do
    json = "{\"vcpu\":#{vcpu}}"
    response = api_patch("/sessions/#{session_id}", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unboost session (reset to default resources).
  """
  @spec session_unboost(String.t(), keyword()) :: boolean()
  def session_unboost(session_id, opts \\ []) do
    json = "{\"vcpu\":1}"
    response = api_patch("/sessions/#{session_id}", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Execute a command in a session.
  """
  @spec session_execute(String.t(), String.t(), keyword()) :: result()
  def session_execute(session_id, command, opts \\ []) do
    json = "{\"command\":\"#{escape_json(command)}\"}"
    response = api_post("/sessions/#{session_id}/execute", json, opts)
    parse_result(response)
  end

  # ============================================================================
  # Service Functions (17)
  # ============================================================================

  @doc """
  List all services.
  """
  @spec service_list(keyword()) :: String.t()
  def service_list(opts \\ []), do: api_get("/services", opts)

  @doc """
  Get service details.
  """
  @spec service_get(String.t(), keyword()) :: service()
  def service_get(service_id, opts \\ []) do
    response = api_get("/services/#{service_id}", opts)
    %{
      id: service_id,
      name: extract_json_value(response, "name") || "",
      status: extract_json_value(response, "status") || "unknown",
      ports: extract_json_value(response, "ports"),
      domains: extract_json_value(response, "domains"),
      vcpu: extract_json_int(response, "vcpu"),
      locked: extract_json_value(response, "locked") == "true",
      unfreeze_on_demand: extract_json_value(response, "unfreeze_on_demand") == "true",
      created_at: extract_json_int(response, "created_at")
    }
  end

  @doc """
  Create a new service.

  ## Options
    * `:ports` - Ports to expose (e.g., "8080" or "80,443")
    * `:domains` - Custom domains
    * `:bootstrap` - Bootstrap script content
    * `:network` - Network mode
    * `:vcpu` - Number of vCPUs
    * `:input_files` - List of file paths
  """
  @spec service_create(String.t(), keyword()) :: String.t() | nil
  def service_create(name, opts \\ []) do
    ports = Keyword.get(opts, :ports)
    domains = Keyword.get(opts, :domains)
    bootstrap = Keyword.get(opts, :bootstrap)
    network = Keyword.get(opts, :network)
    vcpu = Keyword.get(opts, :vcpu)
    input_files = Keyword.get(opts, :input_files, [])

    ports_json = if ports, do: ",\"ports\":[#{ports}]", else: ""
    domains_json = if domains, do: ",\"domains\":\"#{escape_json(domains)}\"", else: ""
    bootstrap_json = if bootstrap, do: ",\"bootstrap\":\"#{escape_json(bootstrap)}\"", else: ""
    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""
    input_files_json = build_input_files_json(input_files)

    json = "{\"name\":\"#{escape_json(name)}\"#{ports_json}#{domains_json}#{bootstrap_json}#{network_json}#{vcpu_json}#{input_files_json}}"
    response = api_post("/services", json, opts)
    extract_json_value(response, "id")
  end

  @doc """
  Destroy a service.
  """
  @spec service_destroy(String.t(), keyword()) :: boolean()
  def service_destroy(service_id, opts \\ []) do
    response = api_delete("/services/#{service_id}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Freeze a service.
  """
  @spec service_freeze(String.t(), keyword()) :: boolean()
  def service_freeze(service_id, opts \\ []) do
    response = api_post("/services/#{service_id}/freeze", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unfreeze a service.
  """
  @spec service_unfreeze(String.t(), keyword()) :: boolean()
  def service_unfreeze(service_id, opts \\ []) do
    response = api_post("/services/#{service_id}/unfreeze", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Lock a service to prevent deletion.
  """
  @spec service_lock(String.t(), keyword()) :: boolean()
  def service_lock(service_id, opts \\ []) do
    response = api_post("/services/#{service_id}/lock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unlock a service.
  """
  @spec service_unlock(String.t(), keyword()) :: boolean()
  def service_unlock(service_id, opts \\ []) do
    response = api_post("/services/#{service_id}/unlock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Set unfreeze-on-demand for a service.
  """
  @spec service_set_unfreeze_on_demand(String.t(), boolean(), keyword()) :: boolean()
  def service_set_unfreeze_on_demand(service_id, enabled, opts \\ []) do
    json = "{\"unfreeze_on_demand\":#{enabled}}"
    response = api_patch("/services/#{service_id}", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Redeploy a service with optional new bootstrap.
  """
  @spec service_redeploy(String.t(), String.t() | nil, keyword()) :: boolean()
  def service_redeploy(service_id, bootstrap \\ nil, opts \\ []) do
    bootstrap_json = if bootstrap, do: "\"bootstrap\":\"#{escape_json(bootstrap)}\"", else: ""
    json = "{#{bootstrap_json}}"
    response = api_post("/services/#{service_id}/redeploy", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Get service bootstrap logs.
  """
  @spec service_logs(String.t(), keyword()) :: String.t()
  def service_logs(service_id, opts \\ []) do
    all_logs = Keyword.get(opts, :all_logs, false)
    endpoint = if all_logs, do: "/services/#{service_id}/logs?all=true", else: "/services/#{service_id}/logs"
    api_get(endpoint, opts)
  end

  @doc """
  Execute a command in a service.
  """
  @spec service_execute(String.t(), String.t(), keyword()) :: result()
  def service_execute(service_id, command, opts \\ []) do
    timeout_ms = Keyword.get(opts, :timeout_ms)
    timeout_json = if timeout_ms, do: ",\"timeout_ms\":#{timeout_ms}", else: ""
    json = "{\"command\":\"#{escape_json(command)}\"#{timeout_json}}"
    response = api_post("/services/#{service_id}/execute", json, opts)
    parse_result(response)
  end

  @doc """
  Get service environment vault.
  """
  @spec service_env_get(String.t(), keyword()) :: String.t()
  def service_env_get(service_id, opts \\ []) do
    api_get("/services/#{service_id}/env", opts)
  end

  @doc """
  Set service environment vault.
  """
  @spec service_env_set(String.t(), String.t(), keyword()) :: boolean()
  def service_env_set(service_id, env_content, opts \\ []) do
    api_put_text("/services/#{service_id}/env", env_content, opts)
  end

  @doc """
  Delete service environment vault.
  """
  @spec service_env_delete(String.t(), keyword()) :: boolean()
  def service_env_delete(service_id, opts \\ []) do
    response = api_delete("/services/#{service_id}/env", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Export service environment vault.
  """
  @spec service_env_export(String.t(), keyword()) :: String.t()
  def service_env_export(service_id, opts \\ []) do
    response = api_post("/services/#{service_id}/env/export", "{}", opts)
    extract_json_value(response, "content") || ""
  end

  @doc """
  Resize a service (change vCPU).
  """
  @spec service_resize(String.t(), integer(), keyword()) :: boolean()
  def service_resize(service_id, vcpu, opts \\ []) do
    json = "{\"vcpu\":#{vcpu}}"
    response = api_patch("/services/#{service_id}", json, opts)
    not String.contains?(response, "\"error\"")
  end

  # ============================================================================
  # Snapshot Functions (9)
  # ============================================================================

  @doc """
  List all snapshots.
  """
  @spec snapshot_list(keyword()) :: String.t()
  def snapshot_list(opts \\ []), do: api_get("/snapshots", opts)

  @doc """
  Get snapshot details.
  """
  @spec snapshot_get(String.t(), keyword()) :: snapshot()
  def snapshot_get(snapshot_id, opts \\ []) do
    response = api_get("/snapshots/#{snapshot_id}", opts)
    %{
      id: snapshot_id,
      name: extract_json_value(response, "name"),
      type: extract_json_value(response, "type") || "unknown",
      source_id: extract_json_value(response, "source_id") || "",
      hot: extract_json_value(response, "hot") == "true",
      locked: extract_json_value(response, "locked") == "true",
      created_at: extract_json_int(response, "created_at"),
      size_bytes: extract_json_int(response, "size_bytes")
    }
  end

  @doc """
  Create a snapshot of a session.
  """
  @spec snapshot_session(String.t(), keyword()) :: String.t() | nil
  def snapshot_session(session_id, opts \\ []) do
    name = Keyword.get(opts, :name)
    hot = Keyword.get(opts, :hot, false)
    name_json = if name, do: "\"name\":\"#{escape_json(name)}\",", else: ""
    json = "{#{name_json}\"hot\":#{hot}}"
    response = api_post("/sessions/#{session_id}/snapshot", json, opts)
    extract_json_value(response, "id")
  end

  @doc """
  Create a snapshot of a service.
  """
  @spec snapshot_service(String.t(), keyword()) :: String.t() | nil
  def snapshot_service(service_id, opts \\ []) do
    name = Keyword.get(opts, :name)
    hot = Keyword.get(opts, :hot, false)
    name_json = if name, do: "\"name\":\"#{escape_json(name)}\",", else: ""
    json = "{#{name_json}\"hot\":#{hot}}"
    response = api_post("/services/#{service_id}/snapshot", json, opts)
    extract_json_value(response, "id")
  end

  @doc """
  Restore from a snapshot.
  """
  @spec snapshot_restore(String.t(), keyword()) :: String.t() | nil
  def snapshot_restore(snapshot_id, opts \\ []) do
    response = api_post("/snapshots/#{snapshot_id}/restore", "{}", opts)
    extract_json_value(response, "id")
  end

  @doc """
  Delete a snapshot.
  """
  @spec snapshot_delete(String.t(), keyword()) :: boolean()
  def snapshot_delete(snapshot_id, opts \\ []) do
    response = api_delete("/snapshots/#{snapshot_id}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Lock a snapshot to prevent deletion.
  """
  @spec snapshot_lock(String.t(), keyword()) :: boolean()
  def snapshot_lock(snapshot_id, opts \\ []) do
    response = api_post("/snapshots/#{snapshot_id}/lock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unlock a snapshot.
  """
  @spec snapshot_unlock(String.t(), keyword()) :: boolean()
  def snapshot_unlock(snapshot_id, opts \\ []) do
    response = api_post("/snapshots/#{snapshot_id}/unlock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Clone a snapshot to create a new session or service.

  ## Options
    * `:type` - "session" or "service" (required)
    * `:name` - Name for cloned service
    * `:ports` - Ports for cloned service
    * `:shell` - Shell for cloned session
  """
  @spec snapshot_clone(String.t(), keyword()) :: String.t() | nil
  def snapshot_clone(snapshot_id, opts \\ []) do
    clone_type = Keyword.get(opts, :type)
    name = Keyword.get(opts, :name)
    ports = Keyword.get(opts, :ports)
    shell = Keyword.get(opts, :shell)

    type_json = "\"type\":\"#{clone_type}\""
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    ports_json = if ports, do: ",\"ports\":[#{ports}]", else: ""
    shell_json = if shell, do: ",\"shell\":\"#{shell}\"", else: ""
    json = "{#{type_json}#{name_json}#{ports_json}#{shell_json}}"
    response = api_post("/snapshots/#{snapshot_id}/clone", json, opts)
    extract_json_value(response, "id")
  end

  # ============================================================================
  # Image Functions (13)
  # ============================================================================

  @doc """
  List images.

  ## Options
    * `:filter` - "owned", "shared", "public", or nil for all
  """
  @spec image_list(keyword()) :: String.t()
  def image_list(opts \\ []) do
    filter = Keyword.get(opts, :filter)
    endpoint = if filter, do: "/images?filter=#{filter}", else: "/images"
    api_get(endpoint, opts)
  end

  @doc """
  Get image details.
  """
  @spec image_get(String.t(), keyword()) :: image()
  def image_get(image_id, opts \\ []) do
    response = api_get("/images/#{image_id}", opts)
    %{
      id: image_id,
      name: extract_json_value(response, "name"),
      description: extract_json_value(response, "description"),
      visibility: extract_json_value(response, "visibility") || "private",
      source_type: extract_json_value(response, "source_type") || "",
      source_id: extract_json_value(response, "source_id") || "",
      locked: extract_json_value(response, "locked") == "true",
      created_at: extract_json_int(response, "created_at"),
      size_bytes: extract_json_int(response, "size_bytes")
    }
  end

  @doc """
  Publish an image from a service or snapshot.

  ## Options
    * `:name` - Image name
    * `:description` - Image description
  """
  @spec image_publish(String.t(), String.t(), keyword()) :: String.t() | nil
  def image_publish(source_type, source_id, opts \\ []) do
    name = Keyword.get(opts, :name)
    description = Keyword.get(opts, :description)
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    desc_json = if description, do: ",\"description\":\"#{escape_json(description)}\"", else: ""
    json = "{\"source_type\":\"#{source_type}\",\"source_id\":\"#{source_id}\"#{name_json}#{desc_json}}"
    response = api_post("/images/publish", json, opts)
    extract_json_value(response, "id")
  end

  @doc """
  Delete an image.
  """
  @spec image_delete(String.t(), keyword()) :: boolean()
  def image_delete(image_id, opts \\ []) do
    response = api_delete("/images/#{image_id}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Lock an image to prevent deletion.
  """
  @spec image_lock(String.t(), keyword()) :: boolean()
  def image_lock(image_id, opts \\ []) do
    response = api_post("/images/#{image_id}/lock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Unlock an image.
  """
  @spec image_unlock(String.t(), keyword()) :: boolean()
  def image_unlock(image_id, opts \\ []) do
    response = api_post("/images/#{image_id}/unlock", "{}", opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Set image visibility.
  """
  @spec image_set_visibility(String.t(), String.t(), keyword()) :: boolean()
  def image_set_visibility(image_id, visibility, opts \\ []) do
    json = "{\"visibility\":\"#{visibility}\"}"
    response = api_post("/images/#{image_id}/visibility", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Grant access to an image for another API key.
  """
  @spec image_grant_access(String.t(), String.t(), keyword()) :: boolean()
  def image_grant_access(image_id, trusted_api_key, opts \\ []) do
    json = "{\"api_key\":\"#{trusted_api_key}\"}"
    response = api_post("/images/#{image_id}/access/grant", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Revoke access to an image from another API key.
  """
  @spec image_revoke_access(String.t(), String.t(), keyword()) :: boolean()
  def image_revoke_access(image_id, trusted_api_key, opts \\ []) do
    json = "{\"api_key\":\"#{trusted_api_key}\"}"
    response = api_post("/images/#{image_id}/access/revoke", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  List trusted API keys for an image.
  """
  @spec image_list_trusted(String.t(), keyword()) :: [String.t()]
  def image_list_trusted(image_id, opts \\ []) do
    response = api_get("/images/#{image_id}/access", opts)
    extract_json_array(response, "trusted_keys")
  end

  @doc """
  Transfer image ownership to another API key.
  """
  @spec image_transfer(String.t(), String.t(), keyword()) :: boolean()
  def image_transfer(image_id, to_api_key, opts \\ []) do
    json = "{\"to_api_key\":\"#{to_api_key}\"}"
    response = api_post("/images/#{image_id}/transfer", json, opts)
    not String.contains?(response, "\"error\"")
  end

  @doc """
  Spawn a new service from an image.

  ## Options
    * `:name` - Service name
    * `:ports` - Ports to expose
    * `:bootstrap` - Bootstrap command
    * `:network` - Network mode
  """
  @spec image_spawn(String.t(), keyword()) :: String.t() | nil
  def image_spawn(image_id, opts \\ []) do
    name = Keyword.get(opts, :name)
    ports = Keyword.get(opts, :ports)
    bootstrap = Keyword.get(opts, :bootstrap)
    network = Keyword.get(opts, :network)

    name_json = if name, do: "\"name\":\"#{escape_json(name)}\"", else: ""
    ports_json = if ports, do: "#{if name, do: ",", else: ""}\"ports\":[#{ports}]", else: ""
    bootstrap_json = if bootstrap, do: ",\"bootstrap\":\"#{escape_json(bootstrap)}\"", else: ""
    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    json = "{#{name_json}#{ports_json}#{bootstrap_json}#{network_json}}"
    response = api_post("/images/#{image_id}/spawn", json, opts)
    extract_json_value(response, "id")
  end

  @doc """
  Clone an image.

  ## Options
    * `:name` - Name for cloned image
    * `:description` - Description for cloned image
  """
  @spec image_clone(String.t(), keyword()) :: String.t() | nil
  def image_clone(image_id, opts \\ []) do
    name = Keyword.get(opts, :name)
    description = Keyword.get(opts, :description)
    name_json = if name, do: "\"name\":\"#{escape_json(name)}\"", else: ""
    desc_json = if description, do: "#{if name, do: ",", else: ""}\"description\":\"#{escape_json(description)}\"", else: ""
    json = "{#{name_json}#{desc_json}}"
    response = api_post("/images/#{image_id}/clone", json, opts)
    extract_json_value(response, "id")
  end

  # ============================================================================
  # PaaS Logs Functions (2)
  # ============================================================================

  @doc """
  Fetch batch logs from portal.

  ## Options
    * `:source` - "all", "api", "portal", "pool/cammy", "pool/ai"
    * `:lines` - Number of lines (1-10000)
    * `:since` - Time window ("1m", "5m", "1h", "1d")
    * `:grep` - Filter pattern
  """
  @spec logs_fetch(keyword()) :: String.t()
  def logs_fetch(opts \\ []) do
    source = Keyword.get(opts, :source, "all")
    lines = Keyword.get(opts, :lines, 100)
    since = Keyword.get(opts, :since, "1h")
    grep = Keyword.get(opts, :grep)

    grep_param = if grep, do: "&grep=#{URI.encode(grep)}", else: ""
    api_get("/logs?source=#{source}&lines=#{lines}&since=#{since}#{grep_param}", opts)
  end

  @doc """
  Stream logs via SSE. This is a blocking operation that calls the callback for each log line.
  Note: Full SSE streaming requires WebSocket support; this implementation provides basic fetch.
  """
  @spec logs_stream(keyword(), (String.t(), String.t() -> any())) :: :ok
  def logs_stream(opts \\ [], callback) do
    # For Elixir without external deps, we can't do true SSE streaming
    # Instead, we poll with a short interval
    source = Keyword.get(opts, :source, "all")
    grep = Keyword.get(opts, :grep)
    interval = Keyword.get(opts, :interval, 5000)

    grep_param = if grep, do: "&grep=#{URI.encode(grep)}", else: ""

    Stream.repeatedly(fn ->
      response = api_get("/logs?source=#{source}&lines=50&since=10s#{grep_param}", opts)
      callback.(source, response)
      Process.sleep(interval)
    end)
    |> Stream.run()

    :ok
  end

  # ============================================================================
  # Key Validation (1)
  # ============================================================================

  @doc """
  Validate API keys and get account information.
  """
  @spec validate_keys(keyword()) :: key_info()
  def validate_keys(opts \\ []) do
    response = portal_post("/keys/validate", "{}", opts)
    %{
      valid: extract_json_value(response, "status") == "valid",
      tier: extract_json_value(response, "tier"),
      rate_limit_per_minute: extract_json_int(response, "rate_per_minute"),
      concurrency_limit: extract_json_int(response, "concurrency"),
      expires_at: extract_json_int(response, "expires_at")
    }
  end

  # ============================================================================
  # Private API Functions
  # ============================================================================

  defp api_get(endpoint, opts) do
    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "GET", endpoint, "")
    args = ["-s", "#{@api_base}#{endpoint}"] ++ headers
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)
    check_clock_drift(output)
    output
  end

  defp api_post(endpoint, json, opts) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "POST", endpoint, json)

    args = ["-s", "-X", "POST", "#{@api_base}#{endpoint}", "-H", "Content-Type: application/json"] ++ headers ++ ["-d", "@#{tmp_file}"]
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp api_delete(endpoint, opts) do
    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "DELETE", endpoint, "")
    args = ["-s", "-X", "DELETE", "#{@api_base}#{endpoint}"] ++ headers
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)
    check_clock_drift(output)
    output
  end

  defp api_patch(endpoint, json, opts) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "PATCH", endpoint, json)

    args = ["-s", "-X", "PATCH", "#{@api_base}#{endpoint}", "-H", "Content-Type: application/json"] ++ headers ++ ["-d", "@#{tmp_file}"]
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp api_put_text(endpoint, body, opts) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.txt"
    File.write!(tmp_file, body)

    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "PUT", endpoint, body)

    args = ["-s", "-o", "/dev/null", "-w", "%{http_code}", "-X", "PUT", "#{@api_base}#{endpoint}", "-H", "Content-Type: text/plain"] ++ headers ++ ["-d", "@#{tmp_file}"]
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    status_code = String.trim(output) |> String.to_integer()
    status_code >= 200 and status_code < 300
  end

  defp portal_post(endpoint, json, opts) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys_from_opts(opts)
    headers = build_auth_headers(public_key, secret_key, "POST", endpoint, json)

    args = ["-s", "-X", "POST", "#{@portal_base}#{endpoint}", "-H", "Content-Type: application/json"] ++ headers ++ ["-d", "@#{tmp_file}"]
    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp get_api_keys_from_opts(opts) do
    public_key = Keyword.get(opts, :public_key)
    secret_key = Keyword.get(opts, :secret_key)

    if public_key && secret_key do
      {public_key, secret_key}
    else
      get_api_keys()
    end
  end

  defp build_execute_json_full(language, code, opts) do
    network = Keyword.get(opts, :network)
    vcpu = Keyword.get(opts, :vcpu)
    ttl = Keyword.get(opts, :ttl)
    env = Keyword.get(opts, :env, [])
    input_files = Keyword.get(opts, :input_files, [])
    return_artifacts = Keyword.get(opts, :return_artifacts, false)

    network_json = if network, do: ",\"network\":\"#{network}\"", else: ""
    vcpu_json = if vcpu, do: ",\"vcpu\":#{vcpu}", else: ""
    ttl_json = if ttl, do: ",\"ttl\":#{ttl}", else: ""
    env_json = if env != [], do: ",\"env\":{" <> Enum.map_join(env, ",", fn {k, v} -> "\"#{k}\":\"#{escape_json(v)}\"" end) <> "}", else: ""
    input_files_json = build_input_files_json(input_files)
    artifacts_json = if return_artifacts, do: ",\"return_artifacts\":true", else: ""

    "{\"language\":\"#{language}\",\"code\":\"#{escape_json(code)}\"#{network_json}#{vcpu_json}#{ttl_json}#{env_json}#{input_files_json}#{artifacts_json}}"
  end

  defp parse_result(response) do
    %{
      success: extract_json_int(response, "exit_code") == 0,
      stdout: extract_json_value(response, "stdout") || "",
      stderr: extract_json_value(response, "stderr") || "",
      exit_code: extract_json_int(response, "exit_code") || 0,
      job_id: extract_json_value(response, "job_id"),
      language: extract_json_value(response, "language"),
      execution_time: nil
    }
  end

  defp extract_json_int(json_str, key) do
    case Regex.run(~r/"#{key}"\s*:\s*(-?\d+)/, json_str) do
      [_, value] -> String.to_integer(value)
      _ -> nil
    end
  end

  # ============================================================================
  # CLI Entry Point
  # ============================================================================

  def main([]), do: print_usage()
  def main(["session" | rest]), do: session_command(rest)
  def main(["service" | rest]), do: service_command(rest)
  def main(["snapshot" | rest]), do: snapshot_command(rest)
  def main(["image" | rest]), do: image_command(rest)
  def main(["key" | rest]), do: key_command(rest)
  def main(["languages" | rest]), do: languages_command(rest)
  def main(args), do: execute_command(args)

  defp print_usage do
    IO.puts("Usage: un.ex [options] <source_file>")
    IO.puts("       un.ex session [options]")
    IO.puts("       un.ex service [options]")
    IO.puts("       un.ex service env <action> <service_id>")
    IO.puts("       un.ex snapshot [options]")
    IO.puts("       un.ex image [options]")
    IO.puts("       un.ex key [--extend]")
    IO.puts("       un.ex languages [--json]")
    IO.puts("")
    IO.puts("Service options: --name, --ports, --bootstrap, -e KEY=VALUE, --env-file FILE")
    IO.puts("                --set-unfreeze-on-demand ID true|false")
    IO.puts("Service env commands: status, set, export, delete")
    IO.puts("Image options: --list, --info ID, --delete ID, --lock ID, --unlock ID,")
    IO.puts("               --publish ID --source-type TYPE, --visibility ID MODE,")
    IO.puts("               --spawn ID, --clone ID, --name NAME, --ports PORTS")
    IO.puts("Languages options: --json (output as JSON array)")
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

  defp session_command(["--snapshot", session_id | rest]) do
    api_key = get_api_key()
    name = get_opt(rest, "--snapshot-name", nil, nil)
    hot = "--hot" in rest
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    hot_json = if hot, do: ",\"hot\":true", else: ""
    json = "{#{String.slice(name_json <> hot_json, 1..-1)}}"
    response = curl_post(api_key, "/sessions/#{session_id}/snapshot", json)
    IO.puts("#{@green}Snapshot created#{@reset}")
    IO.puts(response)
  end

  defp session_command(["--restore", snapshot_id | _rest]) do
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    api_key = get_api_key()
    response = curl_post(api_key, "/snapshots/#{snapshot_id}/restore", "{}")
    IO.puts("#{@green}Session restored from snapshot#{@reset}")
    IO.puts(response)
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
  defp validate_session_args(["--snapshot", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["--restore", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["--from", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["--snapshot-name", _ | rest]), do: validate_session_args(rest)
  defp validate_session_args(["--hot" | rest]), do: validate_session_args(rest)
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
    curl_post(api_key, "/services/#{service_id}/freeze", "{}")
    IO.puts("#{@green}Service frozen: #{service_id}#{@reset}")
  end

  defp service_command(["--unfreeze", service_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/unfreeze", "{}")
    IO.puts("#{@green}Service unfreezing: #{service_id}#{@reset}")
  end

  defp service_command(["--destroy", service_id | _]) do
    api_key = get_api_key()
    case curl_delete_with_sudo(api_key, "/services/#{service_id}") do
      {:ok, _, _} ->
        IO.puts("#{@green}Service destroyed: #{service_id}#{@reset}")
      {:ok, _} ->
        IO.puts("#{@green}Service destroyed: #{service_id}#{@reset}")
      {:error, :cancelled} ->
        System.halt(1)
      {:error, msg} ->
        IO.puts(:stderr, "#{@red}Error: #{msg}#{@reset}")
        System.halt(1)
    end
  end

  defp service_command(["--resize", service_id | rest]) do
    vcpu = get_opt(rest, "--vcpu", "-v", nil)

    if is_nil(vcpu) do
      IO.puts(:stderr, "#{@red}Error: --resize requires --vcpu N#{@reset}")
      System.halt(1)
    end

    vcpu_int = String.to_integer(vcpu)

    if vcpu_int < 1 or vcpu_int > 8 do
      IO.puts(:stderr, "#{@red}Error: --vcpu must be between 1 and 8#{@reset}")
      System.halt(1)
    end

    api_key = get_api_key()
    json = "{\"vcpu\":#{vcpu_int}}"
    curl_patch(api_key, "/services/#{service_id}", json)
    ram = vcpu_int * 2
    IO.puts("#{@green}Service resized to #{vcpu_int} vCPU, #{ram} GB RAM#{@reset}")
  end

  defp service_command(["--set-unfreeze-on-demand", service_id, enabled | _]) do
    api_key = get_api_key()
    enabled_bool = String.downcase(enabled) in ["true", "1", "yes", "on"]
    json = "{\"unfreeze_on_demand\":#{enabled_bool}}"
    curl_patch(api_key, "/services/#{service_id}", json)
    IO.puts("#{@green}Service unfreeze_on_demand set to #{enabled_bool}: #{service_id}#{@reset}")
  end

  defp service_command(["--snapshot", service_id | rest]) do
    api_key = get_api_key()
    name = get_opt(rest, "--snapshot-name", nil, nil)
    hot = "--hot" in rest
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    hot_json = if hot, do: ",\"hot\":true", else: ""
    json = "{#{String.slice(name_json <> hot_json, 1..-1)}}"
    response = curl_post(api_key, "/services/#{service_id}/snapshot", json)
    IO.puts("#{@green}Snapshot created#{@reset}")
    IO.puts(response)
  end

  defp service_command(["--restore", snapshot_id | _rest]) do
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    api_key = get_api_key()
    response = curl_post(api_key, "/snapshots/#{snapshot_id}/restore", "{}")
    IO.puts("#{@green}Service restored from snapshot#{@reset}")
    IO.puts(response)
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

  defp service_command(["env", "status", service_id | _]) do
    response = service_env_status(service_id)
    has_vault = extract_json_value(response, "has_vault") == "true"
    if has_vault do
      IO.puts("#{@green}Vault: configured#{@reset}")
      env_count = extract_json_value(response, "env_count")
      if env_count, do: IO.puts("Variables: #{env_count}")
      updated_at = extract_json_value(response, "updated_at")
      if updated_at, do: IO.puts("Updated: #{updated_at}")
    else
      IO.puts("#{@yellow}Vault: not configured#{@reset}")
    end
  end

  defp service_command(["env", "set", service_id | rest]) do
    envs = get_all_opts(rest, "-e")
    env_file = get_opt(rest, "--env-file", nil, nil)
    if Enum.empty?(envs) and is_nil(env_file) do
      IO.puts(:stderr, "#{@red}Error: service env set requires -e or --env-file#{@reset}")
      System.halt(1)
    end
    env_content = build_env_content(envs, env_file)
    if service_env_set(service_id, env_content) do
      IO.puts("#{@green}Vault updated for service #{service_id}#{@reset}")
    else
      IO.puts(:stderr, "#{@red}Error: Failed to update vault#{@reset}")
      System.halt(1)
    end
  end

  defp service_command(["env", "export", service_id | _]) do
    response = service_env_export(service_id)
    content = extract_json_value(response, "content")
    if content, do: IO.write(content)
  end

  defp service_command(["env", "delete", service_id | _]) do
    if service_env_delete(service_id) do
      IO.puts("#{@green}Vault deleted for service #{service_id}#{@reset}")
    else
      IO.puts(:stderr, "#{@red}Error: Failed to delete vault#{@reset}")
      System.halt(1)
    end
  end

  defp service_command(["env" | _]) do
    IO.puts(:stderr, "#{@red}Error: Usage: un.ex service env <status|set|export|delete> <service_id>#{@reset}")
    System.halt(1)
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
    envs = get_all_opts(args, "-e")
    env_file = get_opt(args, "--env-file", nil, nil)

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

    # Auto-set vault if env vars were provided
    service_id = extract_json_value(response, "id")
    if service_id and (not Enum.empty?(envs) or env_file) do
      env_content = build_env_content(envs, env_file)
      if String.length(env_content) > 0 do
        if service_env_set(service_id, env_content) do
          IO.puts("#{@green}Vault configured with environment variables#{@reset}")
        else
          IO.puts("#{@yellow}Warning: Failed to set vault#{@reset}")
        end
      end
    end
  end

  # Snapshot command
  defp snapshot_command(["--list" | _]) do
    snapshot_command(["-l"])
  end

  defp snapshot_command(["-l" | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/snapshots")
    IO.puts(response)
  end

  defp snapshot_command(["--info", snapshot_id | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/snapshots/#{snapshot_id}")
    IO.puts(response)
  end

  defp snapshot_command(["--delete", snapshot_id | _]) do
    api_key = get_api_key()
    case curl_delete_with_sudo(api_key, "/snapshots/#{snapshot_id}") do
      {:ok, _, _} ->
        IO.puts("#{@green}Snapshot deleted: #{snapshot_id}#{@reset}")
      {:ok, _} ->
        IO.puts("#{@green}Snapshot deleted: #{snapshot_id}#{@reset}")
      {:error, :cancelled} ->
        System.halt(1)
      {:error, msg} ->
        IO.puts(:stderr, "#{@red}Error: #{msg}#{@reset}")
        System.halt(1)
    end
  end

  defp snapshot_command(["--clone", snapshot_id | rest]) do
    api_key = get_api_key()
    clone_type = get_opt(rest, "--type", nil, nil)
    name = get_opt(rest, "--name", nil, nil)
    shell = get_opt(rest, "--shell", nil, nil)
    ports = get_opt(rest, "--ports", nil, nil)

    if !clone_type do
      IO.puts(:stderr, "#{@red}Error: --type required (session or service)#{@reset}")
      System.halt(1)
    end

    type_json = "\"type\":\"#{clone_type}\""
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    shell_json = if shell, do: ",\"shell\":\"#{shell}\"", else: ""
    ports_json = if ports, do: ",\"ports\":[#{ports}]", else: ""
    json = "{#{type_json}#{name_json}#{shell_json}#{ports_json}}"

    response = curl_post(api_key, "/snapshots/#{snapshot_id}/clone", json)
    IO.puts("#{@green}Created from snapshot#{@reset}")
    IO.puts(response)
  end

  defp snapshot_command(_) do
    IO.puts(:stderr, "Error: Use --list, --info ID, --delete ID, or --clone ID --type TYPE")
    System.halt(1)
  end

  # Image command
  defp image_command(["--list" | _]) do
    image_command(["-l"])
  end

  defp image_command(["-l" | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/images")
    IO.puts(response)
  end

  defp image_command(["--info", image_id | _]) do
    api_key = get_api_key()
    response = curl_get(api_key, "/images/#{image_id}")
    IO.puts(response)
  end

  defp image_command(["--delete", image_id | _]) do
    api_key = get_api_key()
    case curl_delete_with_sudo(api_key, "/images/#{image_id}") do
      {:ok, _, _} ->
        IO.puts("#{@green}Image deleted: #{image_id}#{@reset}")
      {:ok, _} ->
        IO.puts("#{@green}Image deleted: #{image_id}#{@reset}")
      {:error, :cancelled} ->
        System.halt(1)
      {:error, msg} ->
        IO.puts(:stderr, "#{@red}Error: #{msg}#{@reset}")
        System.halt(1)
    end
  end

  defp image_command(["--lock", image_id | _]) do
    api_key = get_api_key()
    curl_post(api_key, "/images/#{image_id}/lock", "{}")
    IO.puts("#{@green}Image locked: #{image_id}#{@reset}")
  end

  defp image_command(["--unlock", image_id | _]) do
    api_key = get_api_key()
    case curl_post_with_sudo(api_key, "/images/#{image_id}/unlock", "{}") do
      {:ok, _, _} ->
        IO.puts("#{@green}Image unlocked: #{image_id}#{@reset}")
      {:ok, _} ->
        IO.puts("#{@green}Image unlocked: #{image_id}#{@reset}")
      {:error, :cancelled} ->
        System.halt(1)
      {:error, msg} ->
        IO.puts(:stderr, "#{@red}Error: #{msg}#{@reset}")
        System.halt(1)
    end
  end

  defp image_command(["--publish", source_id | rest]) do
    source_type = get_opt(rest, "--source-type", nil, nil)
    if is_nil(source_type) do
      IO.puts(:stderr, "#{@red}Error: --source-type required (service or snapshot)#{@reset}")
      System.halt(1)
    end
    api_key = get_api_key()
    name = get_opt(rest, "--name", nil, nil)
    name_json = if name, do: ",\"name\":\"#{escape_json(name)}\"", else: ""
    json = "{\"source_type\":\"#{source_type}\",\"source_id\":\"#{source_id}\"#{name_json}}"
    response = curl_post(api_key, "/images/publish", json)
    IO.puts("#{@green}Image published#{@reset}")
    IO.puts(response)
  end

  defp image_command(["--visibility", image_id, mode | _]) do
    api_key = get_api_key()
    json = "{\"visibility\":\"#{mode}\"}"
    curl_post(api_key, "/images/#{image_id}/visibility", json)
    IO.puts("#{@green}Image visibility set to #{mode}#{@reset}")
  end

  defp image_command(["--spawn", image_id | rest]) do
    api_key = get_api_key()
    name = get_opt(rest, "--name", nil, nil)
    ports = get_opt(rest, "--ports", nil, nil)
    name_json = if name, do: "\"name\":\"#{escape_json(name)}\"", else: ""
    ports_json = if ports, do: "\"ports\":[#{ports}]", else: ""
    parts = [name_json, ports_json] |> Enum.filter(&(&1 != "")) |> Enum.join(",")
    json = "{#{parts}}"
    response = curl_post(api_key, "/images/#{image_id}/spawn", json)
    IO.puts("#{@green}Service spawned from image#{@reset}")
    IO.puts(response)
  end

  defp image_command(["--clone", image_id | rest]) do
    api_key = get_api_key()
    name = get_opt(rest, "--name", nil, nil)
    json = if name, do: "{\"name\":\"#{escape_json(name)}\"}", else: "{}"
    response = curl_post(api_key, "/images/#{image_id}/clone", json)
    IO.puts("#{@green}Image cloned#{@reset}")
    IO.puts(response)
  end

  defp image_command(_) do
    IO.puts(:stderr, "Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID")
    System.halt(1)
  end

  # Languages cache functions
  defp get_languages_cache_path do
    home = System.get_env("HOME") || "."
    Path.join([home, ".unsandbox", "languages.json"])
  end

  defp load_languages_cache do
    cache_path = get_languages_cache_path()

    case File.read(cache_path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            timestamp = Map.get(data, "timestamp", 0)
            now = System.system_time(:second)

            if now - timestamp < @languages_cache_ttl do
              Map.get(data, "languages", [])
            else
              nil
            end

          {:error, _} ->
            # Fallback to manual JSON parsing for environments without Jason
            timestamp = extract_json_number(content, "timestamp")
            now = System.system_time(:second)

            if timestamp != 0 and now - timestamp < @languages_cache_ttl do
              extract_json_array(content, "languages")
            else
              nil
            end
        end

      {:error, _} ->
        nil
    end
  rescue
    UndefinedFunctionError ->
      # Jason not available, use manual parsing
      cache_path = get_languages_cache_path()

      case File.read(cache_path) do
        {:ok, content} ->
          timestamp = extract_json_number(content, "timestamp")
          now = System.system_time(:second)

          if timestamp != 0 and now - timestamp < @languages_cache_ttl do
            extract_json_array(content, "languages")
          else
            nil
          end

        {:error, _} ->
          nil
      end
  end

  defp save_languages_cache(languages) do
    cache_path = get_languages_cache_path()
    cache_dir = Path.dirname(cache_path)

    # Ensure directory exists
    File.mkdir_p(cache_dir)

    timestamp = System.system_time(:second)
    languages_json = "[" <> Enum.map_join(languages, ",", &("\"#{&1}\"")) <> "]"
    json = "{\"languages\":#{languages_json},\"timestamp\":#{timestamp}}"

    File.write(cache_path, json)
  end

  defp extract_json_number(json_str, key) do
    case Regex.run(~r/"#{key}"\s*:\s*(\d+)/, json_str) do
      [_, value] -> String.to_integer(value)
      _ -> 0
    end
  end

  # Languages command
  defp languages_command(args) do
    json_output = "--json" in args

    # Try to load from cache first
    languages =
      case load_languages_cache() do
        nil ->
          # Cache miss or expired, fetch from API
          api_key = get_api_key()
          response = curl_get(api_key, "/languages")
          langs = extract_json_array(response, "languages")
          save_languages_cache(langs)
          langs

        cached_languages ->
          cached_languages
      end

    if json_output do
      # Output as JSON array
      json_str = "[" <> Enum.map_join(languages, ",", &("\"#{&1}\"")) <> "]"
      IO.puts(json_str)
    else
      # Output one language per line
      Enum.each(languages, &IO.puts/1)
    end
  end

  defp extract_json_array(json_str, key) do
    case Regex.run(~r/"#{key}"\s*:\s*\[([^\]]*)\]/, json_str) do
      [_, array_content] ->
        Regex.scan(~r/"([^"]*)"/, array_content)
        |> Enum.map(fn [_, val] -> val end)
      _ -> []
    end
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

  defp curl_patch(api_key, endpoint, json) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "PATCH", endpoint, json)

    args = [
      "-s", "-X", "PATCH",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Content-Type: application/json"
    ] ++ headers ++ ["-d", "@#{tmp_file}"]

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    check_clock_drift(output)
    output
  end

  defp curl_put_text(endpoint, body) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.txt"
    File.write!(tmp_file, body)

    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "PUT", endpoint, body)

    args = [
      "-s", "-o", "/dev/null", "-w", "%{http_code}",
      "-X", "PUT",
      "https://api.unsandbox.com#{endpoint}",
      "-H", "Content-Type: text/plain"
    ] ++ headers ++ ["-d", "@#{tmp_file}"]

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)
    status_code = String.trim(output) |> String.to_integer()
    status_code >= 200 and status_code < 300
  end

  @max_env_content_size 65536

  defp read_env_file(path) do
    case File.read(path) do
      {:ok, content} -> content
      {:error, _} ->
        IO.puts(:stderr, "#{@red}Error: Env file not found: #{path}#{@reset}")
        System.halt(1)
    end
  end

  defp build_env_content(envs, env_file) do
    file_lines = if env_file do
      content = read_env_file(env_file)
      content
      |> String.split("\n")
      |> Enum.map(&String.trim/1)
      |> Enum.filter(fn line ->
        String.length(line) > 0 and not String.starts_with?(line, "#")
      end)
    else
      []
    end
    (envs ++ file_lines) |> Enum.join("\n")
  end

  defp service_env_status(service_id) do
    api_key = get_api_key()
    curl_get(api_key, "/services/#{service_id}/env")
  end

  defp service_env_set(service_id, env_content) do
    if String.length(env_content) > @max_env_content_size do
      IO.puts(:stderr, "#{@red}Error: Env content exceeds maximum size of 64KB#{@reset}")
      false
    else
      curl_put_text("/services/#{service_id}/env", env_content)
    end
  end

  defp service_env_export(service_id) do
    api_key = get_api_key()
    curl_post(api_key, "/services/#{service_id}/env/export", "{}")
  end

  defp service_env_delete(service_id) do
    api_key = get_api_key()
    curl_delete(api_key, "/services/#{service_id}/env")
    true
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

  # Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
  defp handle_sudo_challenge(response, method, endpoint, body) do
    challenge_id = extract_json_value(response, "challenge_id")

    IO.puts(:stderr, "#{@yellow}Confirmation required. Check your email for a one-time code.#{@reset}")
    IO.write(:stderr, "Enter OTP: ")

    otp = IO.gets("") |> String.trim()

    if otp == "" do
      IO.puts(:stderr, "#{@red}Error: Operation cancelled#{@reset}")
      {:error, :cancelled}
    else
      # Retry the request with sudo headers
      {public_key, secret_key} = get_api_keys()
      body_str = body || ""
      headers = build_auth_headers(public_key, secret_key, method, endpoint, body_str)

      # Add sudo headers
      sudo_headers = ["-H", "X-Sudo-OTP: #{otp}"]
      sudo_headers = if challenge_id do
        sudo_headers ++ ["-H", "X-Sudo-Challenge: #{challenge_id}"]
      else
        sudo_headers
      end

      args = case method do
        "DELETE" ->
          ["-s", "-X", "DELETE", "https://api.unsandbox.com#{endpoint}"] ++ headers ++ sudo_headers
        "POST" ->
          tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
          File.write!(tmp_file, body_str)
          result = ["-s", "-X", "POST", "https://api.unsandbox.com#{endpoint}",
                   "-H", "Content-Type: application/json"] ++ headers ++ sudo_headers ++ ["-d", "@#{tmp_file}"]
          result
        _ ->
          ["-s", "https://api.unsandbox.com#{endpoint}"] ++ headers ++ sudo_headers
      end

      {output, exit_code} = System.cmd("curl", args, stderr_to_stdout: true)

      # Clean up temp file for POST requests
      if method == "POST" do
        tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
        File.rm(tmp_file)
      end

      if exit_code == 0 and not String.contains?(output, "\"error\"") do
        {:ok, output}
      else
        {:error, output}
      end
    end
  end

  # Curl with 428 handling for destructive operations
  defp curl_delete_with_sudo(api_key, endpoint) do
    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "DELETE", endpoint, "")

    args = ["-s", "-X", "DELETE", "-w", "\n%{http_code}",
            "https://api.unsandbox.com#{endpoint}"] ++ headers

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    # Split response body and status code
    lines = String.split(output, "\n")
    {body_lines, [status_code]} = Enum.split(lines, -1)
    body = Enum.join(body_lines, "\n")
    http_code = String.to_integer(String.trim(status_code))

    check_clock_drift(body)

    if http_code == 428 do
      handle_sudo_challenge(body, "DELETE", endpoint, nil)
    else
      {:ok, body, http_code}
    end
  end

  defp curl_post_with_sudo(api_key, endpoint, json) do
    tmp_file = "/tmp/un_ex_#{:rand.uniform(999999)}.json"
    File.write!(tmp_file, json)

    {public_key, secret_key} = get_api_keys()
    headers = build_auth_headers(public_key, secret_key, "POST", endpoint, json)

    args = ["-s", "-X", "POST", "-w", "\n%{http_code}",
            "https://api.unsandbox.com#{endpoint}",
            "-H", "Content-Type: application/json"] ++ headers ++ ["-d", "@#{tmp_file}"]

    {output, _exit} = System.cmd("curl", args, stderr_to_stdout: true)

    File.rm(tmp_file)

    # Split response body and status code
    lines = String.split(output, "\n")
    {body_lines, [status_code]} = Enum.split(lines, -1)
    body = Enum.join(body_lines, "\n")
    http_code = String.to_integer(String.trim(status_code))

    check_clock_drift(body)

    if http_code == 428 do
      handle_sudo_challenge(body, "POST", endpoint, json)
    else
      {:ok, body, http_code}
    end
  end
end

Un.main(System.argv())
