#!/usr/bin/env julia
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


#!/usr/bin/env julia

using HTTP
using JSON
using Base64
using ArgParse
using Printf
using SHA

# Extension to language mapping
const EXT_MAP = Dict(
    ".jl" => "julia", ".r" => "r", ".cr" => "crystal",
    ".f90" => "fortran", ".cob" => "cobol", ".pro" => "prolog",
    ".forth" => "forth", ".4th" => "forth", ".py" => "python",
    ".js" => "javascript", ".ts" => "typescript", ".rb" => "ruby",
    ".php" => "php", ".pl" => "perl", ".lua" => "lua", ".sh" => "bash",
    ".go" => "go", ".rs" => "rust", ".c" => "c", ".cpp" => "cpp",
    ".cc" => "cpp", ".cxx" => "cpp", ".java" => "java", ".kt" => "kotlin",
    ".cs" => "csharp", ".fs" => "fsharp", ".hs" => "haskell",
    ".ml" => "ocaml", ".clj" => "clojure", ".scm" => "scheme",
    ".lisp" => "commonlisp", ".erl" => "erlang", ".ex" => "elixir",
    ".exs" => "elixir", ".d" => "d", ".nim" => "nim", ".zig" => "zig",
    ".v" => "v", ".dart" => "dart", ".groovy" => "groovy",
    ".scala" => "scala", ".tcl" => "tcl", ".raku" => "raku", ".m" => "objc"
)

# ANSI color codes
const BLUE = "\033[34m"
const RED = "\033[31m"
const GREEN = "\033[32m"
const YELLOW = "\033[33m"
const RESET = "\033[0m"

const API_BASE = "https://api.unsandbox.com"
const PORTAL_BASE = "https://unsandbox.com"
const LANGUAGES_CACHE_TTL = 3600

function detect_language(filename::String)::String
    ext = lowercase(match(r"\.[^.]+$", filename).match)
    return get(EXT_MAP, ext, "unknown")
end

function get_api_keys(args_key=nothing)::Tuple{String,String}
    # Try new-style keys first
    public_key = something(args_key, get(ENV, "UNSANDBOX_PUBLIC_KEY", ""))
    secret_key = get(ENV, "UNSANDBOX_SECRET_KEY", "")

    # Fall back to old-style single key for backwards compatibility
    if isempty(public_key)
        old_key = get(ENV, "UNSANDBOX_API_KEY", "")
        if isempty(old_key)
            println(stderr, "$(RED)Error: UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set$(RESET)")
            exit(1)
        end
        # Old-style: use same key for both public and secret
        return (old_key, old_key)
    end

    if isempty(secret_key)
        println(stderr, "$(RED)Error: UNSANDBOX_SECRET_KEY not set$(RESET)")
        exit(1)
    end

    return (public_key, secret_key)
end

function hmac_sha256_hex(key::String, message::String)::String
    h = hmac_sha256(Vector{UInt8}(key), Vector{UInt8}(message))
    return bytes2hex(h)
end

function compute_signature(secret_key::String, timestamp::Int64, method::String, path::String, body::String)::String
    message = "$(timestamp):$(method):$(path):$(body)"
    return hmac_sha256_hex(secret_key, message)
end

function api_request(endpoint::String, public_key::String, secret_key::String; method="GET", data=nothing, sudo_otp=nothing, sudo_challenge=nothing)
    url = API_BASE * endpoint

    # Prepare body
    body = data !== nothing ? JSON.json(data) : ""

    # Generate timestamp and signature
    timestamp = Int64(floor(time()))
    signature = compute_signature(secret_key, timestamp, method, endpoint, body)

    headers = [
        "Authorization" => "Bearer $public_key",
        "X-Timestamp" => string(timestamp),
        "X-Signature" => signature,
        "Content-Type" => "application/json"
    ]

    # Add sudo headers if provided
    if sudo_otp !== nothing
        push!(headers, "X-Sudo-OTP" => sudo_otp)
    end
    if sudo_challenge !== nothing
        push!(headers, "X-Sudo-Challenge" => sudo_challenge)
    end

    try
        if method == "GET"
            response = HTTP.get(url, headers, readtimeout=300)
        elseif method == "POST"
            response = HTTP.post(url, headers, body, readtimeout=300)
        elseif method == "DELETE"
            response = HTTP.delete(url, headers, readtimeout=300)
        else
            error("Unsupported method: $method")
        end

        return JSON.parse(String(response.body))
    catch e
        if isa(e, HTTP.ExceptionRequest.StatusError)
            error_body = String(e.response.body)
            if e.status == 401 && occursin("timestamp", lowercase(error_body))
                println(stderr, "$(RED)Error: Request timestamp expired (must be within 5 minutes of server time)$(RESET)")
                println(stderr, "$(YELLOW)Your computer's clock may have drifted.$(RESET)")
                println(stderr, "Check your system time and sync with NTP if needed:")
                println(stderr, "  Linux:   sudo ntpdate -s time.nist.gov")
                println(stderr, "  macOS:   sudo sntp -sS time.apple.com")
                println(stderr, "  Windows: w32tm /resync")
            else
                println(stderr, "$(RED)Error: HTTP $(e.status) - $(error_body)$(RESET)")
            end
        else
            println(stderr, "$(RED)Error: Request failed: $e$(RESET)")
        end
        exit(1)
    end
end

# Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
function handle_sudo_challenge(endpoint::String, public_key::String, secret_key::String, method::String, data, response_body::String)
    # Extract challenge_id from response
    parsed = JSON.parse(response_body)
    challenge_id = get(parsed, "challenge_id", nothing)

    println(stderr, "$(YELLOW)Confirmation required. Check your email for a one-time code.$(RESET)")
    print(stderr, "Enter OTP: ")
    otp = readline()

    if isempty(strip(otp))
        println(stderr, "$(RED)Error: Operation cancelled$(RESET)")
        exit(1)
    end

    # Retry the request with sudo headers
    return api_request(endpoint, public_key, secret_key, method=method, data=data, sudo_otp=strip(otp), sudo_challenge=challenge_id)
end

# API request that handles 428 sudo challenges for destructive operations
function api_request_with_sudo(endpoint::String, public_key::String, secret_key::String; method="DELETE", data=nothing)
    url = API_BASE * endpoint

    # Prepare body
    body = data !== nothing ? JSON.json(data) : ""

    # Generate timestamp and signature
    timestamp = Int64(floor(time()))
    signature = compute_signature(secret_key, timestamp, method, endpoint, body)

    headers = [
        "Authorization" => "Bearer $public_key",
        "X-Timestamp" => string(timestamp),
        "X-Signature" => signature,
        "Content-Type" => "application/json"
    ]

    try
        if method == "GET"
            response = HTTP.get(url, headers, readtimeout=300, status_exception=false)
        elseif method == "POST"
            response = HTTP.post(url, headers, body, readtimeout=300, status_exception=false)
        elseif method == "DELETE"
            response = HTTP.delete(url, headers, readtimeout=300, status_exception=false)
        else
            error("Unsupported method: $method")
        end

        response_body = String(response.body)

        # Handle 428 - sudo OTP required
        if response.status == 428
            return handle_sudo_challenge(endpoint, public_key, secret_key, method, data, response_body)
        end

        # Handle other errors
        if response.status >= 400
            if response.status == 401 && occursin("timestamp", lowercase(response_body))
                println(stderr, "$(RED)Error: Request timestamp expired (must be within 5 minutes of server time)$(RESET)")
                println(stderr, "$(YELLOW)Your computer's clock may have drifted.$(RESET)")
                println(stderr, "Check your system time and sync with NTP if needed:")
                println(stderr, "  Linux:   sudo ntpdate -s time.nist.gov")
                println(stderr, "  macOS:   sudo sntp -sS time.apple.com")
                println(stderr, "  Windows: w32tm /resync")
            else
                println(stderr, "$(RED)Error: HTTP $(response.status) - $(response_body)$(RESET)")
            end
            exit(1)
        end

        return JSON.parse(response_body)
    catch e
        println(stderr, "$(RED)Error: Request failed: $e$(RESET)")
        exit(1)
    end
end

function api_request_patch(endpoint::String, public_key::String, secret_key::String; data=nothing)
    url = API_BASE * endpoint

    # Prepare body
    body = data !== nothing ? JSON.json(data) : ""

    # Generate timestamp and signature
    timestamp = Int64(floor(time()))
    signature = compute_signature(secret_key, timestamp, "PATCH", endpoint, body)

    headers = [
        "Authorization" => "Bearer $public_key",
        "X-Timestamp" => string(timestamp),
        "X-Signature" => signature,
        "Content-Type" => "application/json"
    ]

    try
        response = HTTP.request("PATCH", url, headers, body, readtimeout=300)
        return JSON.parse(String(response.body))
    catch e
        if isa(e, HTTP.ExceptionRequest.StatusError)
            error_body = String(e.response.body)
            if e.status == 401 && occursin("timestamp", lowercase(error_body))
                println(stderr, "$(RED)Error: Request timestamp expired (must be within 5 minutes of server time)$(RESET)")
                println(stderr, "$(YELLOW)Your computer's clock may have drifted.$(RESET)")
                println(stderr, "Check your system time and sync with NTP if needed:")
                println(stderr, "  Linux:   sudo ntpdate -s time.nist.gov")
                println(stderr, "  macOS:   sudo sntp -sS time.apple.com")
                println(stderr, "  Windows: w32tm /resync")
            else
                println(stderr, "$(RED)Error: HTTP $(e.status) - $(error_body)$(RESET)")
            end
        else
            println(stderr, "$(RED)Error: Request failed: $e$(RESET)")
        end
        exit(1)
    end
end

function api_request_text(endpoint::String, public_key::String, secret_key::String, body::String)::Bool
    url = API_BASE * endpoint
    timestamp = Int64(floor(time()))
    signature = compute_signature(secret_key, timestamp, "PUT", endpoint, body)

    headers = [
        "Authorization" => "Bearer $public_key",
        "X-Timestamp" => string(timestamp),
        "X-Signature" => signature,
        "Content-Type" => "text/plain"
    ]

    try
        response = HTTP.put(url, headers, body, readtimeout=300)
        return response.status >= 200 && response.status < 300
    catch e
        return false
    end
end

const MAX_ENV_CONTENT_SIZE = 65536

function read_env_file(path::String)::String
    if !isfile(path)
        println(stderr, "$(RED)Error: Env file not found: $path$(RESET)")
        exit(1)
    end
    return read(path, String)
end

function build_env_content(envs::Vector{String}, env_file::Union{String,Nothing})::String
    lines = copy(envs)
    if env_file !== nothing
        content = read_env_file(env_file)
        for line in split(content, '\n')
            trimmed = strip(line)
            if !isempty(trimmed) && !startswith(trimmed, "#")
                push!(lines, trimmed)
            end
        end
    end
    return join(lines, "\n")
end

function service_env_status(service_id::String, public_key::String, secret_key::String)
    return api_request("/services/$service_id/env", public_key, secret_key)
end

function service_env_set(service_id::String, env_content::String, public_key::String, secret_key::String)::Bool
    if length(env_content) > MAX_ENV_CONTENT_SIZE
        println(stderr, "$(RED)Error: Env content exceeds maximum size of 64KB$(RESET)")
        return false
    end
    return api_request_text("/services/$service_id/env", public_key, secret_key, env_content)
end

function service_env_export(service_id::String, public_key::String, secret_key::String)
    return api_request("/services/$service_id/env/export", public_key, secret_key, method="POST", data=Dict())
end

function service_env_delete(service_id::String, public_key::String, secret_key::String)::Bool
    try
        api_request("/services/$service_id/env", public_key, secret_key, method="DELETE")
        return true
    catch
        return false
    end
end

function cmd_service_env(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    action = get(args, "env-action", nothing)
    target = get(args, "env-target", nothing)

    if action == "status"
        if target === nothing
            println(stderr, "$(RED)Error: service env status requires service ID$(RESET)")
            exit(1)
        end
        result = service_env_status(target, public_key, secret_key)
        has_vault = get(result, "has_vault", false)
        if has_vault
            println("$(GREEN)Vault: configured$(RESET)")
            env_count = get(result, "env_count", nothing)
            if env_count !== nothing
                println("Variables: $env_count")
            end
            updated_at = get(result, "updated_at", nothing)
            if updated_at !== nothing
                println("Updated: $updated_at")
            end
        else
            println("$(YELLOW)Vault: not configured$(RESET)")
        end
    elseif action == "set"
        if target === nothing
            println(stderr, "$(RED)Error: service env set requires service ID$(RESET)")
            exit(1)
        end
        envs = something(args["vault-env"], String[])
        env_file = get(args, "env-file", nothing)
        if isempty(envs) && env_file === nothing
            println(stderr, "$(RED)Error: service env set requires -e or --env-file$(RESET)")
            exit(1)
        end
        env_content = build_env_content(envs, env_file)
        if service_env_set(target, env_content, public_key, secret_key)
            println("$(GREEN)Vault updated for service $target$(RESET)")
        else
            println(stderr, "$(RED)Error: Failed to update vault$(RESET)")
            exit(1)
        end
    elseif action == "export"
        if target === nothing
            println(stderr, "$(RED)Error: service env export requires service ID$(RESET)")
            exit(1)
        end
        result = service_env_export(target, public_key, secret_key)
        content = get(result, "content", nothing)
        if content !== nothing
            print(content)
        end
    elseif action == "delete"
        if target === nothing
            println(stderr, "$(RED)Error: service env delete requires service ID$(RESET)")
            exit(1)
        end
        if service_env_delete(target, public_key, secret_key)
            println("$(GREEN)Vault deleted for service $target$(RESET)")
        else
            println(stderr, "$(RED)Error: Failed to delete vault$(RESET)")
            exit(1)
        end
    else
        println(stderr, "$(RED)Error: Unknown env action: $action$(RESET)")
        println(stderr, "Usage: un.jl service env <status|set|export|delete> <service_id>")
        exit(1)
    end
end

function cmd_execute(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    filename = args["source_file"]
    if !isfile(filename)
        println(stderr, "$(RED)Error: File not found: $filename$(RESET)")
        exit(1)
    end

    language = detect_language(filename)
    if language == "unknown"
        println(stderr, "$(RED)Error: Cannot detect language for $filename$(RESET)")
        exit(1)
    end

    code = read(filename, String)

    # Build request payload
    payload = Dict("language" => language, "code" => code)

    # Add environment variables
    if args["env"] !== nothing
        env_vars = Dict{String,String}()
        for e in args["env"]
            if occursin('=', e)
                k, v = split(e, '=', limit=2)
                env_vars[k] = v
            end
        end
        if !isempty(env_vars)
            payload["env"] = env_vars
        end
    end

    # Add input files
    if args["files"] !== nothing
        input_files = []
        for filepath in args["files"]
            if !isfile(filepath)
                println(stderr, "$(RED)Error: Input file not found: $filepath$(RESET)")
                exit(1)
            end
            content = base64encode(read(filepath))
            push!(input_files, Dict(
                "filename" => basename(filepath),
                "content_base64" => content
            ))
        end
        if !isempty(input_files)
            payload["input_files"] = input_files
        end
    end

    # Add options
    if args["artifacts"]
        payload["return_artifacts"] = true
    end
    if args["network"] !== nothing
        payload["network"] = args["network"]
    end

    # Execute
    result = api_request("/execute", public_key, secret_key, method="POST", data=payload)

    # Print output
    if haskey(result, "stdout") && !isempty(result["stdout"])
        print(BLUE, result["stdout"], RESET)
    end
    if haskey(result, "stderr") && !isempty(result["stderr"])
        print(RED, result["stderr"], RESET)
    end

    # Save artifacts
    if args["artifacts"] && haskey(result, "artifacts")
        out_dir = something(args["output-dir"], ".")
        mkpath(out_dir)
        for artifact in result["artifacts"]
            filename = get(artifact, "filename", "artifact")
            content = base64decode(artifact["content_base64"])
            path = joinpath(out_dir, filename)
            write(path, content)
            chmod(path, 0o755)
            println(stderr, "$(GREEN)Saved: $path$(RESET)")
        end
    end

    exit_code = get(result, "exit_code", 0)
    exit(exit_code)
end

function cmd_session(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    if args["list"]
        result = api_request("/sessions", public_key, secret_key)
        sessions = get(result, "sessions", [])
        if isempty(sessions)
            println("No active sessions")
        else
            @printf("%-40s %-10s %-10s %s\n", "ID", "Shell", "Status", "Created")
            for s in sessions
                @printf("%-40s %-10s %-10s %s\n",
                    get(s, "id", "N/A"),
                    get(s, "shell", "N/A"),
                    get(s, "status", "N/A"),
                    get(s, "created_at", "N/A"))
            end
        end
        return
    end

    if args["kill"] !== nothing
        api_request("/sessions/$(args["kill"])", public_key, secret_key, method="DELETE")
        println("$(GREEN)Session terminated: $(args["kill"])$(RESET)")
        return
    end

    # Create new session
    payload = Dict("shell" => "bash")

    if args["network"] !== nothing
        payload["network"] = args["network"]
    end

    # Add input files
    if args["files"] !== nothing
        input_files = []
        for filepath in args["files"]
            if !isfile(filepath)
                println(stderr, "$(RED)Error: Input file not found: $filepath$(RESET)")
                exit(1)
            end
            content = base64encode(read(filepath))
            push!(input_files, Dict(
                "filename" => basename(filepath),
                "content_base64" => content
            ))
        end
        if !isempty(input_files)
            payload["input_files"] = input_files
        end
    end

    println("$(YELLOW)Creating session...$(RESET)")
    result = api_request("/sessions", public_key, secret_key, method="POST", data=payload)
    println("$(GREEN)Session created: $(get(result, "id", "N/A"))$(RESET)")
    println("$(YELLOW)(Interactive sessions require WebSocket - use un2 for full support)$(RESET)")
end

function cmd_service(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    # Handle env subcommand
    if get(args, "env-action", nothing) !== nothing
        cmd_service_env(args)
        return
    end

    if args["list"]
        result = api_request("/services", public_key, secret_key)
        services = get(result, "services", [])
        if isempty(services)
            println("No services")
        else
            @printf("%-20s %-15s %-10s %-15s %s\n", "ID", "Name", "Status", "Ports", "Domains")
            for s in services
                ports = join(get(s, "ports", []), ',')
                domains = join(get(s, "domains", []), ',')
                @printf("%-20s %-15s %-10s %-15s %s\n",
                    get(s, "id", "N/A"),
                    get(s, "name", "N/A"),
                    get(s, "status", "N/A"),
                    ports, domains)
            end
        end
        return
    end

    if args["info"] !== nothing
        result = api_request("/services/$(args["info"])", public_key, secret_key)
        println(JSON.json(result, 2))
        return
    end

    if args["logs"] !== nothing
        result = api_request("/services/$(args["logs"])/logs", public_key, secret_key)
        println(get(result, "logs", ""))
        return
    end

    if args["sleep"] !== nothing
        api_request("/services/$(args["sleep"])/freeze", public_key, secret_key, method="POST")
        println("$(GREEN)Service frozen: $(args["sleep"])$(RESET)")
        return
    end

    if args["wake"] !== nothing
        api_request("/services/$(args["wake"])/unfreeze", public_key, secret_key, method="POST")
        println("$(GREEN)Service unfreezing: $(args["wake"])$(RESET)")
        return
    end

    if args["destroy"] !== nothing
        api_request_with_sudo("/services/$(args["destroy"])", public_key, secret_key, method="DELETE")
        println("$(GREEN)Service destroyed: $(args["destroy"])$(RESET)")
        return
    end

    if args["resize"] !== nothing
        vcpu = args["vcpu"]
        if vcpu === nothing || vcpu <= 0
            println(stderr, "$(RED)Error: --resize requires --vcpu N (1-8)$(RESET)")
            exit(1)
        end
        api_request_patch("/services/$(args["resize"])", public_key, secret_key, data=Dict("vcpu" => vcpu))
        ram = vcpu * 2
        println("$(GREEN)Service resized to $(vcpu) vCPU, $(ram) GB RAM$(RESET)")
        return
    end

    if args["unfreeze-on-demand"] !== nothing
        enabled = args["unfreeze-on-demand-value"]
        if enabled === nothing
            println(stderr, "$(RED)Error: --unfreeze-on-demand requires true or false$(RESET)")
            exit(1)
        end
        api_request_patch("/services/$(args["unfreeze-on-demand"])", public_key, secret_key, data=Dict("unfreeze_on_demand" => enabled))
        println("$(GREEN)Service unfreeze_on_demand set to $(enabled): $(args["unfreeze-on-demand"])$(RESET)")
        return
    end

    if args["dump-bootstrap"] !== nothing
        println(stderr, "Fetching bootstrap script from $(args["dump-bootstrap"])...")
        payload = Dict("command" => "cat /tmp/bootstrap.sh")
        result = api_request("/services/$(args["dump-bootstrap"])/execute", public_key, secret_key, method="POST", data=payload)

        if haskey(result, "stdout") && !isempty(result["stdout"])
            bootstrap = result["stdout"]
            if args["dump-file"] !== nothing
                # Write to file
                try
                    write(args["dump-file"], bootstrap)
                    chmod(args["dump-file"], 0o755)
                    println("Bootstrap saved to $(args["dump-file"])")
                catch e
                    println(stderr, "$(RED)Error: Could not write to $(args["dump-file"]): $e$(RESET)")
                    exit(1)
                end
            else
                # Print to stdout
                print(bootstrap)
            end
        else
            println(stderr, "$(RED)Error: Failed to fetch bootstrap (service not running or no bootstrap file)$(RESET)")
            exit(1)
        end
        return
    end

    # Create new service
    if args["name"] !== nothing
        payload = Dict("name" => args["name"])

        if args["ports"] !== nothing
            ports = [parse(Int, strip(p)) for p in split(args["ports"], ',')]
            payload["ports"] = ports
        end

        if args["domains"] !== nothing
            domains = [strip(d) for d in split(args["domains"], ',')]
            payload["domains"] = domains
        end

        if args["type"] !== nothing
            payload["service_type"] = args["type"]
        end

        if args["bootstrap"] !== nothing
            payload["bootstrap"] = args["bootstrap"]
        end

        if args["bootstrap-file"] !== nothing
            bootstrap_file = args["bootstrap-file"]
            if isfile(bootstrap_file)
                payload["bootstrap_content"] = read(bootstrap_file, String)
            else
                println(stderr, "$(RED)Error: Bootstrap file not found: $bootstrap_file$(RESET)")
                exit(1)
            end
        end

        if args["network"] !== nothing
            payload["network"] = args["network"]
        end

        if args["vcpu"] !== nothing
            payload["vcpu"] = args["vcpu"]
        end

        # Add input files
        if args["files"] !== nothing
            input_files = []
            for filepath in args["files"]
                if !isfile(filepath)
                    println(stderr, "$(RED)Error: Input file not found: $filepath$(RESET)")
                    exit(1)
                end
                content = base64encode(read(filepath))
                push!(input_files, Dict(
                    "filename" => basename(filepath),
                    "content_base64" => content
                ))
            end
            if !isempty(input_files)
                payload["input_files"] = input_files
            end
        end

        result = api_request("/services", public_key, secret_key, method="POST", data=payload)
        service_id = get(result, "id", nothing)
        println("$(GREEN)Service created: $(something(service_id, "N/A"))$(RESET)")
        println("Name: $(get(result, "name", "N/A"))")
        if haskey(result, "url")
            println("URL: $(result["url"])")
        end

        # Auto-set vault if env vars were provided
        vault_envs = something(args["vault-env"], String[])
        vault_env_file = get(args, "env-file", nothing)
        if service_id !== nothing && (!isempty(vault_envs) || vault_env_file !== nothing)
            env_content = build_env_content(vault_envs, vault_env_file)
            if !isempty(env_content)
                if service_env_set(service_id, env_content, public_key, secret_key)
                    println("$(GREEN)Vault configured with environment variables$(RESET)")
                else
                    println("$(YELLOW)Warning: Failed to set vault$(RESET)")
                end
            end
        end
        return
    end

    println(stderr, "$(RED)Error: Use --name to create, or --list, --info, --logs, --freeze, --unfreeze, --destroy$(RESET)")
    exit(1)
end

function validate_key(api_key::String)
    url = PORTAL_BASE * "/keys/validate"
    headers = [
        "Authorization" => "Bearer $api_key",
        "Content-Type" => "application/json"
    ]

    try
        response = HTTP.post(url, headers, "{}", readtimeout=300)
        data = JSON.parse(String(response.body))

        # Check if valid
        if get(data, "valid", false)
            # Print valid key info
            println("$(GREEN)Valid$(RESET)\n")
            println(@sprintf("%-20s %s", "Public Key:", get(data, "public_key", "N/A")))
            println(@sprintf("%-20s %s", "Tier:", get(data, "tier", "N/A")))
            println(@sprintf("%-20s %s", "Status:", get(data, "status", "N/A")))
            println(@sprintf("%-20s %s", "Expires:", get(data, "valid_through_datetime", "N/A")))
            println(@sprintf("%-20s %s", "Time Remaining:", get(data, "valid_for_human", "N/A")))
            println(@sprintf("%-20s %s/min", "Rate Limit:", get(data, "rate_per_minute", "N/A")))
            println(@sprintf("%-20s %s", "Burst:", get(data, "burst", "N/A")))
            println(@sprintf("%-20s %s", "Concurrency:", get(data, "concurrency", "N/A")))
            return 0
        else
            # Handle invalid response
            reason = get(data, "reason", "unknown")
            if reason == "expired"
                println("$(RED)Expired$(RESET)\n")
                println(@sprintf("%-20s %s", "Public Key:", get(data, "public_key", "N/A")))
                println(@sprintf("%-20s %s", "Tier:", get(data, "tier", "N/A")))
                expired_at = get(data, "expired_at_datetime", "N/A")
                expired_ago = get(data, "expired_ago", "")
                if !isempty(expired_ago)
                    println(@sprintf("%-20s %s (%s)", "Expired:", expired_at, expired_ago))
                else
                    println(@sprintf("%-20s %s", "Expired:", expired_at))
                end
                renew_url = get(data, "renew_url", "https://unsandbox.com/pricing")
                println("\n$(YELLOW)To renew:$(RESET) Visit $renew_url")
            elseif reason == "invalid_key"
                println("$(RED)Invalid$(RESET): key not found")
            elseif reason == "suspended"
                println("$(RED)Suspended$(RESET): key has been suspended")
            else
                println("$(RED)Invalid$(RESET): $reason")
            end
            return 1
        end
    catch e
        if isa(e, HTTP.ExceptionRequest.StatusError)
            # Parse error response from body
            try
                data = JSON.parse(String(e.response.body))
                reason = get(data, "reason", "unknown")

                if reason == "expired"
                    println("$(RED)Expired$(RESET)\n")
                    println(@sprintf("%-20s %s", "Public Key:", get(data, "public_key", "N/A")))
                    println(@sprintf("%-20s %s", "Tier:", get(data, "tier", "N/A")))
                    expired_at = get(data, "expired_at_datetime", "N/A")
                    expired_ago = get(data, "expired_ago", "")
                    if !isempty(expired_ago)
                        println(@sprintf("%-20s %s (%s)", "Expired:", expired_at, expired_ago))
                    else
                        println(@sprintf("%-20s %s", "Expired:", expired_at))
                    end
                    renew_url = get(data, "renew_url", "https://unsandbox.com/pricing")
                    println("\n$(YELLOW)To renew:$(RESET) Visit $renew_url")
                elseif reason == "invalid_key"
                    println("$(RED)Invalid$(RESET): key not found")
                elseif reason == "suspended"
                    println("$(RED)Suspended$(RESET): key has been suspended")
                else
                    println("$(RED)Invalid$(RESET): $reason")
                end
            catch
                println(stderr, "$(RED)Error: HTTP $(e.status)$(RESET)")
            end
            return 1
        else
            println(stderr, "$(RED)Error: Request failed: $e$(RESET)")
            return 1
        end
    end
end

function get_languages_cache_path()::String
    home = get(ENV, "HOME", ".")
    return joinpath(home, ".unsandbox", "languages.json")
end

function load_languages_cache()::Union{Vector{String}, Nothing}
    cache_path = get_languages_cache_path()

    if !isfile(cache_path)
        return nothing
    end

    try
        content = read(cache_path, String)
        data = JSON.parse(content)
        timestamp = get(data, "timestamp", 0)
        now = Int64(floor(time()))

        if now - timestamp < LANGUAGES_CACHE_TTL
            langs = get(data, "languages", [])
            return [string(l) for l in langs]
        else
            return nothing
        end
    catch
        return nothing
    end
end

function save_languages_cache(languages::Vector)
    cache_path = get_languages_cache_path()
    cache_dir = dirname(cache_path)

    # Ensure directory exists
    mkpath(cache_dir)

    timestamp = Int64(floor(time()))
    data = Dict("languages" => languages, "timestamp" => timestamp)

    try
        open(cache_path, "w") do f
            write(f, JSON.json(data))
        end
    catch
        # Ignore write errors
    end
end

function cmd_languages(args)
    # Try to load from cache first
    langs = load_languages_cache()

    if langs === nothing
        # Cache miss or expired, fetch from API
        (public_key, secret_key) = get_api_keys(args["api-key"])
        result = api_request("/languages", public_key, secret_key)
        langs = get(result, "languages", [])
        save_languages_cache(langs)
    end

    if args["json"]
        println(JSON.json(langs))
    else
        for lang in langs
            println(lang)
        end
    end
end

function cmd_key(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])
    # For portal validation, we still use public_key as bearer token
    api_key = public_key

    # Handle --extend flag
    if args["extend"]
        # Validate key to get public key
        url = PORTAL_BASE * "/keys/validate"
        headers = [
            "Authorization" => "Bearer $api_key",
            "Content-Type" => "application/json"
        ]

        try
            response = HTTP.post(url, headers, "{}", readtimeout=300)
            data = JSON.parse(String(response.body))

            public_key = get(data, "public_key", nothing)
            if public_key === nothing
                println(stderr, "$(RED)Error: Invalid key or could not retrieve public key$(RESET)")
                exit(1)
            end

            # Build extend URL
            extend_url = "$(PORTAL_BASE)/keys/extend?pk=$(public_key)"

            println("Opening extension page in browser...")
            println("If browser doesn't open, visit: $extend_url")

            # Try to open browser
            if Sys.isapple()
                run(`open $extend_url`)
            elseif Sys.islinux()
                try
                    run(`xdg-open $extend_url`)
                catch
                    try
                        run(`sensible-browser $extend_url`)
                    catch
                        # Already printed the URL
                    end
                end
            elseif Sys.iswindows()
                run(`cmd /c start $extend_url`)
            end

            exit(0)
        catch e
            println(stderr, "$(RED)Error: Failed to validate key: $e$(RESET)")
            exit(1)
        end
    end

    # Default: validate and display key info
    exit(validate_key(api_key))
end

function main()
    s = ArgParseSettings(description="Unsandbox CLI - Execute code in secure sandboxes")

    @add_arg_table! s begin
        "source_file"
            help = "Source file to execute"
            required = false
        "--api-key", "-k"
            help = "API key (or set UNSANDBOX_API_KEY)"
        "--network", "-n"
            help = "Network mode"
            arg_type = String
            range_tester = x -> x in ["zerotrust", "semitrusted"]
        "--env", "-e"
            help = "Set environment variable (KEY=VALUE)"
            action = :append_arg
        "--files", "-f"
            help = "Add input file"
            action = :append_arg
        "--artifacts", "-a"
            help = "Return artifacts"
            action = :store_true
        "--output-dir", "-o"
            help = "Output directory for artifacts"
        "session"
            help = "Manage interactive sessions"
            action = :command
        "service"
            help = "Manage persistent services"
            action = :command
        "languages"
            help = "List available programming languages"
            action = :command
        "key"
            help = "Check API key validity and expiration"
            action = :command
        "image"
            help = "Manage images"
            action = :command
        "snapshot"
            help = "Manage snapshots"
            action = :command
    end

    @add_arg_table! s["snapshot"] begin
        "--list", "-l"
            help = "List all snapshots"
            action = :store_true
        "--info"
            help = "Get snapshot details"
        "--delete"
            help = "Delete a snapshot"
        "--lock"
            help = "Lock snapshot to prevent deletion"
        "--unlock"
            help = "Unlock snapshot"
        "--restore"
            help = "Restore from snapshot"
        "--clone"
            help = "Clone snapshot to session/service (requires --type)"
        "--type"
            help = "Clone type: session or service"
        "--name"
            help = "Name for cloned resource"
        "--shell"
            help = "Shell for cloned session"
        "--ports"
            help = "Comma-separated ports for cloned service"
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["session"] begin
        "--list", "-l"
            help = "List active sessions"
            action = :store_true
        "--kill"
            help = "Terminate session"
        "--files", "-f"
            help = "Add input file"
            action = :append_arg
        "--network", "-n"
            help = "Network mode"
            arg_type = String
            range_tester = x -> x in ["zerotrust", "semitrusted"]
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["service"] begin
        "--name"
            help = "Service name"
        "--ports"
            help = "Comma-separated ports"
        "--domains"
            help = "Comma-separated custom domains"
        "--type"
            help = "Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)"
        "--bootstrap"
            help = "Bootstrap command or URI"
        "--bootstrap-file"
            help = "Upload local file as bootstrap script"
        "--files", "-f"
            help = "Add input file"
            action = :append_arg
        "--vault-env", "-e"
            help = "Environment variable for vault (KEY=VALUE)"
            action = :append_arg
        "--env-file"
            help = "Load vault variables from file"
        "--network", "-n"
            help = "Network mode"
            arg_type = String
            range_tester = x -> x in ["zerotrust", "semitrusted"]
        "--vcpu", "-v"
            help = "vCPU count (1-8)"
            arg_type = Int
            range_tester = x -> x >= 1 && x <= 8
        "--list", "-l"
            help = "List services"
            action = :store_true
        "--info"
            help = "Get service details"
        "--logs"
            help = "Get all logs"
        "--freeze"
            help = "Freeze service"
        "--unfreeze"
            help = "Unfreeze service"
        "--destroy"
            help = "Destroy service"
        "--resize"
            help = "Resize service (requires --vcpu N)"
        "--unfreeze-on-demand"
            help = "Service ID to set unfreeze_on_demand for"
        "--unfreeze-on-demand-value"
            help = "Enable/disable unfreeze_on_demand (true or false)"
            arg_type = Bool
        "--dump-bootstrap"
            help = "Dump bootstrap script from service"
        "--dump-file"
            help = "File to save bootstrap (with --dump-bootstrap)"
        "--env-action"
            help = "Env action (status, set, export, delete)"
        "--env-target"
            help = "Service ID for env commands"
        "--api-key", "-k"
            help = "API key"
        "env"
            help = "Manage service environment vault"
            action = :command
    end

    @add_arg_table! s["service"]["env"] begin
        "action"
            help = "Env action: status, set, export, delete"
            required = true
        "service_id"
            help = "Service ID"
            required = false
        "-e"
            help = "Environment variable (KEY=VALUE)"
            action = :append_arg
            dest_name = "vault-env"
        "--env-file"
            help = "Load vault variables from file"
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["key"] begin
        "--extend"
            help = "Open browser to extend/renew key"
            action = :store_true
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["languages"] begin
        "--json"
            help = "Output as JSON array"
            action = :store_true
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["image"] begin
        "--list", "-l"
            help = "List all images"
            action = :store_true
        "--info"
            help = "Get image details"
        "--delete"
            help = "Delete an image"
        "--lock"
            help = "Lock image to prevent deletion"
        "--unlock"
            help = "Unlock image"
        "--publish"
            help = "Publish image from service/snapshot (requires --source-type)"
        "--source-type"
            help = "Source type: service or snapshot"
        "--visibility"
            help = "Image ID to set visibility for"
        "--visibility-mode"
            help = "Visibility mode: private, unlisted, or public"
        "--spawn"
            help = "Spawn new service from image"
        "--clone"
            help = "Clone an image"
        "--name"
            help = "Name for spawned service or cloned image"
        "--ports"
            help = "Comma-separated ports for spawned service"
        "--api-key", "-k"
            help = "API key"
    end

    args = parse_args(ARGS, s)

    if args["%COMMAND%"] == "session"
        cmd_session(args["session"])
    elseif args["%COMMAND%"] == "service"
        service_args = args["service"]
        # Check if env subcommand was used
        if get(service_args, "%COMMAND%", nothing) == "env"
            env_args = service_args["env"]
            # Copy env args to service args
            service_args["env-action"] = get(env_args, "action", nothing)
            service_args["env-target"] = get(env_args, "service_id", nothing)
            service_args["vault-env"] = get(env_args, "vault-env", nothing)
            service_args["env-file"] = get(env_args, "env-file", nothing)
            service_args["api-key"] = get(env_args, "api-key", nothing)
        end
        cmd_service(service_args)
    elseif args["%COMMAND%"] == "languages"
        cmd_languages(args["languages"])
    elseif args["%COMMAND%"] == "key"
        cmd_key(args["key"])
    elseif args["%COMMAND%"] == "image"
        cmd_image(args["image"])
    elseif args["%COMMAND%"] == "snapshot"
        cmd_snapshot(args["snapshot"])
    elseif args["source_file"] !== nothing
        cmd_execute(args)
    else
        println(stderr, "$(RED)Error: Provide source_file or use 'session'/'service'/'snapshot'/'languages'/'key'/'image' subcommand$(RESET)")
        exit(1)
    end
end

function cmd_image(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    if args["list"]
        result = api_request("/images", public_key, secret_key)
        println(JSON.json(result, 2))
        return
    end

    if args["info"] !== nothing
        result = api_request("/images/$(args["info"])", public_key, secret_key)
        println(JSON.json(result, 2))
        return
    end

    if args["delete"] !== nothing
        api_request_with_sudo("/images/$(args["delete"])", public_key, secret_key, method="DELETE")
        println("$(GREEN)Image deleted: $(args["delete"])$(RESET)")
        return
    end

    if args["lock"] !== nothing
        api_request("/images/$(args["lock"])/lock", public_key, secret_key, method="POST")
        println("$(GREEN)Image locked: $(args["lock"])$(RESET)")
        return
    end

    if args["unlock"] !== nothing
        api_request_with_sudo("/images/$(args["unlock"])/unlock", public_key, secret_key, method="POST", data=Dict())
        println("$(GREEN)Image unlocked: $(args["unlock"])$(RESET)")
        return
    end

    if args["publish"] !== nothing
        source_type = args["source-type"]
        if source_type === nothing
            println(stderr, "$(RED)Error: --source-type required (service or snapshot)$(RESET)")
            exit(1)
        end
        payload = Dict("source_type" => source_type, "source_id" => args["publish"])
        if args["name"] !== nothing
            payload["name"] = args["name"]
        end
        result = api_request("/images/publish", public_key, secret_key, method="POST", data=payload)
        println("$(GREEN)Image published$(RESET)")
        println(JSON.json(result, 2))
        return
    end

    if args["visibility"] !== nothing
        mode = args["visibility-mode"]
        if mode === nothing
            println(stderr, "$(RED)Error: --visibility requires MODE (private, unlisted, or public)$(RESET)")
            exit(1)
        end
        payload = Dict("visibility" => mode)
        api_request("/images/$(args["visibility"])/visibility", public_key, secret_key, method="POST", data=payload)
        println("$(GREEN)Image visibility set to $(mode): $(args["visibility"])$(RESET)")
        return
    end

    if args["spawn"] !== nothing
        payload = Dict()
        if args["name"] !== nothing
            payload["name"] = args["name"]
        end
        if args["ports"] !== nothing
            ports = [parse(Int, strip(p)) for p in split(args["ports"], ',')]
            payload["ports"] = ports
        end
        result = api_request("/images/$(args["spawn"])/spawn", public_key, secret_key, method="POST", data=payload)
        println("$(GREEN)Service spawned from image$(RESET)")
        println(JSON.json(result, 2))
        return
    end

    if args["clone"] !== nothing
        payload = Dict()
        if args["name"] !== nothing
            payload["name"] = args["name"]
        end
        result = api_request("/images/$(args["clone"])/clone", public_key, secret_key, method="POST", data=payload)
        println("$(GREEN)Image cloned$(RESET)")
        println(JSON.json(result, 2))
        return
    end

    println(stderr, "$(RED)Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID, --spawn ID, or --clone ID$(RESET)")
    exit(1)
end

function cmd_snapshot(args)
    (public_key, secret_key) = get_api_keys(args["api-key"])

    if args["list"]
        result = api_request("/snapshots", public_key, secret_key)
        snapshots = get(result, "snapshots", [])
        if isempty(snapshots)
            println("No snapshots found")
        else
            @printf("%-40s %-20s %-12s %-30s %s\n", "ID", "Name", "Type", "Source ID", "Size")
            for s in snapshots
                @printf("%-40s %-20s %-12s %-30s %s\n",
                    get(s, "id", "N/A"),
                    get(s, "name", "-"),
                    get(s, "source_type", "N/A"),
                    get(s, "source_id", "N/A"),
                    get(s, "size", "N/A"))
            end
        end
        return
    end

    if args["info"] !== nothing
        result = api_request("/snapshots/$(args["info"])", public_key, secret_key)
        println(JSON.json(result, 2))
        return
    end

    if args["delete"] !== nothing
        api_request_with_sudo("/snapshots/$(args["delete"])", public_key, secret_key, method="DELETE")
        println("$(GREEN)Snapshot deleted: $(args["delete"])$(RESET)")
        return
    end

    if args["lock"] !== nothing
        api_request("/snapshots/$(args["lock"])/lock", public_key, secret_key, method="POST")
        println("$(GREEN)Snapshot locked: $(args["lock"])$(RESET)")
        return
    end

    if args["unlock"] !== nothing
        api_request_with_sudo("/snapshots/$(args["unlock"])/unlock", public_key, secret_key, method="POST", data=Dict())
        println("$(GREEN)Snapshot unlocked: $(args["unlock"])$(RESET)")
        return
    end

    if args["restore"] !== nothing
        api_request("/snapshots/$(args["restore"])/restore", public_key, secret_key, method="POST", data=Dict())
        println("$(GREEN)Snapshot restored: $(args["restore"])$(RESET)")
        return
    end

    if args["clone"] !== nothing
        clone_type = args["type"]
        if clone_type === nothing
            println(stderr, "$(RED)Error: --type required for --clone (session or service)$(RESET)")
            exit(1)
        end
        payload = Dict("type" => clone_type)
        if args["name"] !== nothing
            payload["name"] = args["name"]
        end
        if args["shell"] !== nothing
            payload["shell"] = args["shell"]
        end
        if args["ports"] !== nothing
            ports = [parse(Int, strip(p)) for p in split(args["ports"], ',')]
            payload["ports"] = ports
        end
        result = api_request("/snapshots/$(args["clone"])/clone", public_key, secret_key, method="POST", data=payload)
        println("$(GREEN)Cloned from snapshot$(RESET)")
        println(JSON.json(result, 2))
        return
    end

    println(stderr, "$(RED)Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --restore ID, or --clone ID$(RESET)")
    exit(1)
end

# =============================================================================
# Library API Functions (for import/use as a module)
# =============================================================================

const VERSION = "1.0.0"

"""
    execute(language::String, code::String; kwargs...) -> Dict

Execute code synchronously and return the result.

# Arguments
- `language`: Programming language (e.g., "python", "javascript")
- `code`: Source code to execute

# Keyword Arguments
- `env::Dict{String,String}`: Environment variables
- `network::String`: Network mode ("zerotrust" or "semitrusted")
- `public_key::String`: API public key (optional)
- `secret_key::String`: API secret key (optional)

# Returns
Dict with `stdout`, `stderr`, `exit_code`, `success`

# Example
```julia
result = execute("python", "print('Hello, World!')")
println(result["stdout"])
```
"""
function execute(language::String, code::String;
                 env::Union{Dict{String,String}, Nothing}=nothing,
                 network::String="zerotrust",
                 public_key::Union{String,Nothing}=nothing,
                 secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("language" => language, "code" => code)
    if env !== nothing
        payload["env"] = env
    end
    if network != "zerotrust"
        payload["network"] = network
    end

    return api_request("/execute", pk, sk, method="POST", data=payload)
end

"""
    execute_async(language::String, code::String; kwargs...) -> String

Execute code asynchronously and return job ID.

# Returns
Job ID string for polling with `wait_job` or `get_job`.
"""
function execute_async(language::String, code::String;
                       env::Union{Dict{String,String}, Nothing}=nothing,
                       network::String="zerotrust",
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("language" => language, "code" => code, "async" => true)
    if env !== nothing
        payload["env"] = env
    end
    if network != "zerotrust"
        payload["network"] = network
    end

    result = api_request("/execute", pk, sk, method="POST", data=payload)
    return get(result, "job_id", "")
end

"""
    wait_job(job_id::String; kwargs...) -> Dict

Wait for an async job to complete and return results.
"""
function wait_job(job_id::String;
                  poll_interval::Int=1,
                  max_wait::Int=300,
                  public_key::Union{String,Nothing}=nothing,
                  secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    start_time = time()
    while true
        result = get_job(job_id, public_key=pk, secret_key=sk)
        status = get(result, "status", "")
        if status in ["completed", "failed", "timeout", "cancelled"]
            return result
        end
        if time() - start_time >= max_wait
            error("Job $job_id did not complete within $max_wait seconds")
        end
        sleep(poll_interval)
    end
end

"""
    get_job(job_id::String; kwargs...) -> Dict

Get the status of an async job.
"""
function get_job(job_id::String;
                 public_key::Union{String,Nothing}=nothing,
                 secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/jobs/$job_id", pk, sk)
end

"""
    cancel_job(job_id::String; kwargs...) -> Bool

Cancel a running job.
"""
function cancel_job(job_id::String;
                    public_key::Union{String,Nothing}=nothing,
                    secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/jobs/$job_id", pk, sk, method="DELETE")
    return true
end

"""
    list_jobs(; kwargs...) -> Vector{Dict}

List all jobs for the account.
"""
function list_jobs(;
                   public_key::Union{String,Nothing}=nothing,
                   secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/jobs", pk, sk)
    return get(result, "jobs", [])
end

"""
    get_languages(; kwargs...) -> Vector{String}

Get list of supported programming languages.
"""
function get_languages(;
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/languages", pk, sk)
    return get(result, "languages", [])
end

"""
    session_list(; kwargs...) -> Vector{Dict}

List all active sessions.
"""
function session_list(;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/sessions", pk, sk)
    return get(result, "sessions", [])
end

"""
    session_get(session_id::String; kwargs...) -> Dict

Get details of a session.
"""
function session_get(session_id::String;
                     public_key::Union{String,Nothing}=nothing,
                     secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/sessions/$session_id", pk, sk)
end

"""
    session_create(; kwargs...) -> Dict

Create a new interactive session.
"""
function session_create(;
                        shell::String="bash",
                        network::String="zerotrust",
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("shell" => shell)
    if network != "zerotrust"
        payload["network"] = network
    end

    return api_request("/sessions", pk, sk, method="POST", data=payload)
end

"""
    session_destroy(session_id::String; kwargs...) -> Bool

Destroy a session.
"""
function session_destroy(session_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/sessions/$session_id", pk, sk, method="DELETE")
    return true
end

"""
    session_freeze(session_id::String; kwargs...) -> Bool

Freeze (pause) a session.
"""
function session_freeze(session_id::String;
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/sessions/$session_id/freeze", pk, sk, method="POST")
    return true
end

"""
    session_unfreeze(session_id::String; kwargs...) -> Bool

Unfreeze (resume) a session.
"""
function session_unfreeze(session_id::String;
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/sessions/$session_id/unfreeze", pk, sk, method="POST")
    return true
end

"""
    session_boost(session_id::String, vcpu::Int; kwargs...) -> Bool

Boost session resources.
"""
function session_boost(session_id::String, vcpu::Int;
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_patch("/sessions/$session_id", pk, sk, data=Dict("vcpu" => vcpu))
    return true
end

"""
    session_unboost(session_id::String; kwargs...) -> Bool

Remove session boost.
"""
function session_unboost(session_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_patch("/sessions/$session_id", pk, sk, data=Dict("vcpu" => 1))
    return true
end

"""
    session_execute(session_id::String, command::String; kwargs...) -> Dict

Execute a command in a session.
"""
function session_execute(session_id::String, command::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/sessions/$session_id/execute", pk, sk, method="POST", data=Dict("command" => command))
end

"""
    service_list(; kwargs...) -> Vector{Dict}

List all services.
"""
function service_list(;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/services", pk, sk)
    return get(result, "services", [])
end

"""
    service_get(service_id::String; kwargs...) -> Dict

Get details of a service.
"""
function service_get(service_id::String;
                     public_key::Union{String,Nothing}=nothing,
                     secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/services/$service_id", pk, sk)
end

"""
    service_create(name::String; kwargs...) -> Dict

Create a new service.
"""
function service_create(name::String;
                        ports::Union{Vector{Int}, Nothing}=nothing,
                        domains::Union{Vector{String}, Nothing}=nothing,
                        bootstrap::Union{String, Nothing}=nothing,
                        network::String="semitrusted",
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("name" => name)
    if ports !== nothing
        payload["ports"] = ports
    end
    if domains !== nothing
        payload["domains"] = domains
    end
    if bootstrap !== nothing
        payload["bootstrap"] = bootstrap
    end
    if network != "semitrusted"
        payload["network"] = network
    end

    return api_request("/services", pk, sk, method="POST", data=payload)
end

"""
    service_destroy(service_id::String; kwargs...) -> Bool

Destroy a service.
"""
function service_destroy(service_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/services/$service_id", pk, sk, method="DELETE")
    return true
end

"""
    service_freeze(service_id::String; kwargs...) -> Bool

Freeze (pause) a service.
"""
function service_freeze(service_id::String;
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/services/$service_id/freeze", pk, sk, method="POST")
    return true
end

"""
    service_unfreeze(service_id::String; kwargs...) -> Bool

Unfreeze (resume) a service.
"""
function service_unfreeze(service_id::String;
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/services/$service_id/unfreeze", pk, sk, method="POST")
    return true
end

"""
    service_lock(service_id::String; kwargs...) -> Bool

Lock a service to prevent deletion.
"""
function service_lock(service_id::String;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/services/$service_id/lock", pk, sk, method="POST")
    return true
end

"""
    service_unlock(service_id::String; kwargs...) -> Bool

Unlock a service.
"""
function service_unlock(service_id::String;
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/services/$service_id/unlock", pk, sk, method="POST", data=Dict())
    return true
end

"""
    service_set_unfreeze_on_demand(service_id::String, enabled::Bool; kwargs...) -> Bool

Set unfreeze-on-demand for a service.
"""
function service_set_unfreeze_on_demand(service_id::String, enabled::Bool;
                                        public_key::Union{String,Nothing}=nothing,
                                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_patch("/services/$service_id", pk, sk, data=Dict("unfreeze_on_demand" => enabled))
    return true
end

"""
    service_redeploy(service_id::String; kwargs...) -> Bool

Redeploy a service (re-run bootstrap).
"""
function service_redeploy(service_id::String;
                          bootstrap::Union{String, Nothing}=nothing,
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = bootstrap !== nothing ? Dict("bootstrap" => bootstrap) : Dict()
    api_request("/services/$service_id/redeploy", pk, sk, method="POST", data=payload)
    return true
end

"""
    service_logs(service_id::String; kwargs...) -> String

Get service logs.
"""
function service_logs(service_id::String;
                      all_logs::Bool=false,
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    endpoint = all_logs ? "/services/$service_id/logs?all=true" : "/services/$service_id/logs"
    result = api_request(endpoint, pk, sk)
    return get(result, "logs", "")
end

"""
    service_execute(service_id::String, command::String; kwargs...) -> Dict

Execute a command in a service.
"""
function service_execute(service_id::String, command::String;
                         timeout_ms::Int=30000,
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/services/$service_id/execute", pk, sk, method="POST",
                       data=Dict("command" => command, "timeout_ms" => timeout_ms))
end

"""
    service_resize(service_id::String, vcpu::Int; kwargs...) -> Bool

Resize a service.
"""
function service_resize(service_id::String, vcpu::Int;
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_patch("/services/$service_id", pk, sk, data=Dict("vcpu" => vcpu))
    return true
end

"""
    service_env_get(service_id::String; kwargs...) -> String

Get service environment variables.
"""
function service_env_get(service_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/services/$service_id/env", pk, sk)
    return get(result, "env", "")
end

"""
    service_env_set(service_id::String, env_content::String; kwargs...) -> Bool

Set service environment variables.
"""
function service_env_set(service_id::String, env_content::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/services/$service_id/env", pk, sk, method="POST", data=Dict("env" => env_content))
    return true
end

"""
    service_env_delete(service_id::String; kwargs...) -> Bool

Delete service environment variables.
"""
function service_env_delete(service_id::String;
                            public_key::Union{String,Nothing}=nothing,
                            secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/services/$service_id/env", pk, sk, method="DELETE")
    return true
end

"""
    service_env_export(service_id::String; kwargs...) -> String

Export service environment variables as shell format.
"""
function service_env_export(service_id::String;
                            public_key::Union{String,Nothing}=nothing,
                            secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/services/$service_id/env/export", pk, sk)
    return get(result, "export", "")
end

"""
    snapshot_list(; kwargs...) -> Vector{Dict}

List all snapshots.
"""
function snapshot_list(;
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/snapshots", pk, sk)
    return get(result, "snapshots", [])
end

"""
    snapshot_get(snapshot_id::String; kwargs...) -> Dict

Get details of a snapshot.
"""
function snapshot_get(snapshot_id::String;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/snapshots/$snapshot_id", pk, sk)
end

"""
    snapshot_session(session_id::String; kwargs...) -> String

Create a snapshot from a session. Returns snapshot ID.
"""
function snapshot_session(session_id::String;
                          name::Union{String, Nothing}=nothing,
                          hot::Bool=false,
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict()
    if name !== nothing
        payload["name"] = name
    end
    if hot
        payload["hot"] = true
    end

    result = api_request("/sessions/$session_id/snapshot", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    snapshot_service(service_id::String; kwargs...) -> String

Create a snapshot from a service. Returns snapshot ID.
"""
function snapshot_service(service_id::String;
                          name::Union{String, Nothing}=nothing,
                          hot::Bool=false,
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict()
    if name !== nothing
        payload["name"] = name
    end
    if hot
        payload["hot"] = true
    end

    result = api_request("/services/$service_id/snapshot", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    snapshot_restore(snapshot_id::String; kwargs...) -> Bool

Restore from a snapshot.
"""
function snapshot_restore(snapshot_id::String;
                          public_key::Union{String,Nothing}=nothing,
                          secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/snapshots/$snapshot_id/restore", pk, sk, method="POST", data=Dict())
    return true
end

"""
    snapshot_delete(snapshot_id::String; kwargs...) -> Bool

Delete a snapshot.
"""
function snapshot_delete(snapshot_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/snapshots/$snapshot_id", pk, sk, method="DELETE")
    return true
end

"""
    snapshot_lock(snapshot_id::String; kwargs...) -> Bool

Lock a snapshot to prevent deletion.
"""
function snapshot_lock(snapshot_id::String;
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/snapshots/$snapshot_id/lock", pk, sk, method="POST")
    return true
end

"""
    snapshot_unlock(snapshot_id::String; kwargs...) -> Bool

Unlock a snapshot.
"""
function snapshot_unlock(snapshot_id::String;
                         public_key::Union{String,Nothing}=nothing,
                         secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/snapshots/$snapshot_id/unlock", pk, sk, method="POST", data=Dict())
    return true
end

"""
    snapshot_clone(snapshot_id::String, clone_type::String; kwargs...) -> String

Clone a snapshot to a new session or service. Returns the new resource ID.
"""
function snapshot_clone(snapshot_id::String, clone_type::String;
                        name::Union{String, Nothing}=nothing,
                        ports::Union{Vector{Int}, Nothing}=nothing,
                        shell::Union{String, Nothing}=nothing,
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("type" => clone_type)
    if name !== nothing
        payload["name"] = name
    end
    if ports !== nothing
        payload["ports"] = ports
    end
    if shell !== nothing
        payload["shell"] = shell
    end

    result = api_request("/snapshots/$snapshot_id/clone", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    image_list(; kwargs...) -> Vector{Dict}

List all images.
"""
function image_list(;
                    filter::Union{String, Nothing}=nothing,
                    public_key::Union{String,Nothing}=nothing,
                    secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    endpoint = filter !== nothing ? "/images?filter=$filter" : "/images"
    result = api_request(endpoint, pk, sk)
    return get(result, "images", [])
end

"""
    image_get(image_id::String; kwargs...) -> Dict

Get details of an image.
"""
function image_get(image_id::String;
                   public_key::Union{String,Nothing}=nothing,
                   secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    return api_request("/images/$image_id", pk, sk)
end

"""
    image_publish(source_type::String, source_id::String; kwargs...) -> String

Publish an image from a service or snapshot. Returns image ID.
"""
function image_publish(source_type::String, source_id::String;
                       name::Union{String, Nothing}=nothing,
                       description::Union{String, Nothing}=nothing,
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict("source_type" => source_type, "source_id" => source_id)
    if name !== nothing
        payload["name"] = name
    end
    if description !== nothing
        payload["description"] = description
    end

    result = api_request("/images/publish", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    image_delete(image_id::String; kwargs...) -> Bool

Delete an image.
"""
function image_delete(image_id::String;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/images/$image_id", pk, sk, method="DELETE")
    return true
end

"""
    image_lock(image_id::String; kwargs...) -> Bool

Lock an image to prevent deletion.
"""
function image_lock(image_id::String;
                    public_key::Union{String,Nothing}=nothing,
                    secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/images/$image_id/lock", pk, sk, method="POST")
    return true
end

"""
    image_unlock(image_id::String; kwargs...) -> Bool

Unlock an image.
"""
function image_unlock(image_id::String;
                      public_key::Union{String,Nothing}=nothing,
                      secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request_with_sudo("/images/$image_id/unlock", pk, sk, method="POST", data=Dict())
    return true
end

"""
    image_set_visibility(image_id::String, visibility::String; kwargs...) -> Bool

Set image visibility (private, unlisted, or public).
"""
function image_set_visibility(image_id::String, visibility::String;
                              public_key::Union{String,Nothing}=nothing,
                              secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/images/$image_id/visibility", pk, sk, method="POST", data=Dict("visibility" => visibility))
    return true
end

"""
    image_grant_access(image_id::String, trusted_api_key::String; kwargs...) -> Bool

Grant access to an image.
"""
function image_grant_access(image_id::String, trusted_api_key::String;
                            public_key::Union{String,Nothing}=nothing,
                            secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/images/$image_id/access", pk, sk, method="POST", data=Dict("trusted_api_key" => trusted_api_key))
    return true
end

"""
    image_revoke_access(image_id::String, trusted_api_key::String; kwargs...) -> Bool

Revoke access to an image.
"""
function image_revoke_access(image_id::String, trusted_api_key::String;
                             public_key::Union{String,Nothing}=nothing,
                             secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/images/$image_id/access/$trusted_api_key", pk, sk, method="DELETE")
    return true
end

"""
    image_list_trusted(image_id::String; kwargs...) -> Vector{String}

List trusted API keys for an image.
"""
function image_list_trusted(image_id::String;
                            public_key::Union{String,Nothing}=nothing,
                            secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    result = api_request("/images/$image_id/access", pk, sk)
    return get(result, "trusted_keys", [])
end

"""
    image_transfer(image_id::String, to_api_key::String; kwargs...) -> Bool

Transfer ownership of an image.
"""
function image_transfer(image_id::String, to_api_key::String;
                        public_key::Union{String,Nothing}=nothing,
                        secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    api_request("/images/$image_id/transfer", pk, sk, method="POST", data=Dict("to_api_key" => to_api_key))
    return true
end

"""
    image_spawn(image_id::String; kwargs...) -> String

Spawn a new service from an image. Returns service ID.
"""
function image_spawn(image_id::String;
                     name::Union{String, Nothing}=nothing,
                     ports::Union{Vector{Int}, Nothing}=nothing,
                     bootstrap::Union{String, Nothing}=nothing,
                     network::Union{String, Nothing}=nothing,
                     public_key::Union{String,Nothing}=nothing,
                     secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict()
    if name !== nothing
        payload["name"] = name
    end
    if ports !== nothing
        payload["ports"] = ports
    end
    if bootstrap !== nothing
        payload["bootstrap"] = bootstrap
    end
    if network !== nothing
        payload["network"] = network
    end

    result = api_request("/images/$image_id/spawn", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    image_clone(image_id::String; kwargs...) -> String

Clone an image. Returns new image ID.
"""
function image_clone(image_id::String;
                     name::Union{String, Nothing}=nothing,
                     description::Union{String, Nothing}=nothing,
                     public_key::Union{String,Nothing}=nothing,
                     secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    payload = Dict()
    if name !== nothing
        payload["name"] = name
    end
    if description !== nothing
        payload["description"] = description
    end

    result = api_request("/images/$image_id/clone", pk, sk, method="POST", data=payload)
    return get(result, "id", "")
end

"""
    logs_fetch(source::String; kwargs...) -> String

Fetch PaaS logs.
"""
function logs_fetch(source::String;
                    lines::Int=100,
                    since::String="1h",
                    grep::Union{String, Nothing}=nothing,
                    public_key::Union{String,Nothing}=nothing,
                    secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    endpoint = "/logs?source=$source&lines=$lines&since=$since"
    if grep !== nothing
        endpoint *= "&grep=$grep"
    end

    result = api_request(endpoint, pk, sk)
    return get(result, "logs", "")
end

"""
    validate_keys(; kwargs...) -> Dict

Validate API keys and return account info.
"""
function validate_keys(;
                       public_key::Union{String,Nothing}=nothing,
                       secret_key::Union{String,Nothing}=nothing)
    (pk, sk) = if public_key !== nothing && secret_key !== nothing
        (public_key, secret_key)
    else
        get_api_keys(nothing)
    end

    url = PORTAL_BASE * "/keys/validate"
    timestamp = Int64(floor(time()))
    body = "{}"
    signature = compute_signature(sk, timestamp, "POST", "/keys/validate", body)

    headers = [
        "Authorization" => "Bearer $pk",
        "X-Timestamp" => string(timestamp),
        "X-Signature" => signature,
        "Content-Type" => "application/json"
    ]

    try
        response = HTTP.post(url, headers, body, readtimeout=30)
        return JSON.parse(String(response.body))
    catch e
        return Dict("valid" => false, "error" => string(e))
    end
end

"""
    health_check() -> Bool

Check if the API is healthy.
"""
function health_check()
    try
        response = HTTP.get("$API_BASE/health", readtimeout=10)
        return response.status == 200
    catch
        return false
    end
end

"""
    version() -> String

Get SDK version.
"""
function version()
    return VERSION
end

# Thread-local error storage
const _last_error = Ref{String}("")

"""
    last_error() -> String

Get the last error message.
"""
function last_error()
    return _last_error[]
end

"""
    set_last_error(msg::String)

Set the last error message (internal use).
"""
function set_last_error(msg::String)
    _last_error[] = msg
end

"""
    hmac_sign(secret_key::String, message::String) -> String

Compute HMAC-SHA256 signature.
"""
function hmac_sign(secret_key::String, message::String)
    return bytes2hex(SHA.hmac_sha256(Vector{UInt8}(secret_key), Vector{UInt8}(message)))
end

main()
