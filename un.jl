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

function api_request(endpoint::String, public_key::String, secret_key::String; method="GET", data=nothing)
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
        api_request("/services/$(args["sleep"])/sleep", public_key, secret_key, method="POST")
        println("$(GREEN)Service sleeping: $(args["sleep"])$(RESET)")
        return
    end

    if args["wake"] !== nothing
        api_request("/services/$(args["wake"])/wake", public_key, secret_key, method="POST")
        println("$(GREEN)Service waking: $(args["wake"])$(RESET)")
        return
    end

    if args["destroy"] !== nothing
        api_request("/services/$(args["destroy"])", public_key, secret_key, method="DELETE")
        println("$(GREEN)Service destroyed: $(args["destroy"])$(RESET)")
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
        println("$(GREEN)Service created: $(get(result, "id", "N/A"))$(RESET)")
        println("Name: $(get(result, "name", "N/A"))")
        if haskey(result, "url")
            println("URL: $(result["url"])")
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
        "key"
            help = "Check API key validity and expiration"
            action = :command
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
        "--dump-bootstrap"
            help = "Dump bootstrap script from service"
        "--dump-file"
            help = "File to save bootstrap (with --dump-bootstrap)"
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

    args = parse_args(ARGS, s)

    if args["%COMMAND%"] == "session"
        cmd_session(args["session"])
    elseif args["%COMMAND%"] == "service"
        cmd_service(args["service"])
    elseif args["%COMMAND%"] == "key"
        cmd_key(args["key"])
    elseif args["source_file"] !== nothing
        cmd_execute(args)
    else
        println(stderr, "$(RED)Error: Provide source_file or use 'session'/'service'/'key' subcommand$(RESET)")
        exit(1)
    end
end

main()
