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

function detect_language(filename::String)::String
    ext = lowercase(match(r"\.[^.]+$", filename).match)
    return get(EXT_MAP, ext, "unknown")
end

function get_api_key(args_key=nothing)::String
    key = something(args_key, get(ENV, "UNSANDBOX_API_KEY", ""))
    if isempty(key)
        println(stderr, "$(RED)Error: UNSANDBOX_API_KEY not set$(RESET)")
        exit(1)
    end
    return key
end

function api_request(endpoint::String, api_key::String; method="GET", data=nothing)
    url = API_BASE * endpoint
    headers = [
        "Authorization" => "Bearer $api_key",
        "Content-Type" => "application/json"
    ]

    try
        if method == "GET"
            response = HTTP.get(url, headers, readtimeout=300)
        elseif method == "POST"
            body = data !== nothing ? JSON.json(data) : ""
            response = HTTP.post(url, headers, body, readtimeout=300)
        elseif method == "DELETE"
            response = HTTP.delete(url, headers, readtimeout=300)
        else
            error("Unsupported method: $method")
        end

        return JSON.parse(String(response.body))
    catch e
        if isa(e, HTTP.ExceptionRequest.StatusError)
            println(stderr, "$(RED)Error: HTTP $(e.status) - $(String(e.response.body))$(RESET)")
        else
            println(stderr, "$(RED)Error: Request failed: $e$(RESET)")
        end
        exit(1)
    end
end

function cmd_execute(args)
    api_key = get_api_key(args["api-key"])

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
    result = api_request("/execute", api_key, method="POST", data=payload)

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
    api_key = get_api_key(args["api-key"])

    if args["list"]
        result = api_request("/sessions", api_key)
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
        api_request("/sessions/$(args["kill"])", api_key, method="DELETE")
        println("$(GREEN)Session terminated: $(args["kill"])$(RESET)")
        return
    end

    println(stderr, "$(RED)Error: Use --list or --kill$(RESET)")
    exit(1)
end

function cmd_service(args)
    api_key = get_api_key(args["api-key"])

    if args["list"]
        result = api_request("/services", api_key)
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
        result = api_request("/services/$(args["info"])", api_key)
        println(JSON.json(result, 2))
        return
    end

    if args["logs"] !== nothing
        result = api_request("/services/$(args["logs"])/logs", api_key)
        println(get(result, "logs", ""))
        return
    end

    if args["sleep"] !== nothing
        api_request("/services/$(args["sleep"])/sleep", api_key, method="POST")
        println("$(GREEN)Service sleeping: $(args["sleep"])$(RESET)")
        return
    end

    if args["wake"] !== nothing
        api_request("/services/$(args["wake"])/wake", api_key, method="POST")
        println("$(GREEN)Service waking: $(args["wake"])$(RESET)")
        return
    end

    if args["destroy"] !== nothing
        api_request("/services/$(args["destroy"])", api_key, method="DELETE")
        println("$(GREEN)Service destroyed: $(args["destroy"])$(RESET)")
        return
    end

    println(stderr, "$(RED)Error: Use --list, --info, --logs, --sleep, --wake, or --destroy$(RESET)")
    exit(1)
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
    end

    @add_arg_table! s["session"] begin
        "--list", "-l"
            help = "List active sessions"
            action = :store_true
        "--kill"
            help = "Terminate session"
        "--api-key", "-k"
            help = "API key"
    end

    @add_arg_table! s["service"] begin
        "--list", "-l"
            help = "List services"
            action = :store_true
        "--info"
            help = "Get service details"
        "--logs"
            help = "Get all logs"
        "--sleep"
            help = "Freeze service"
        "--wake"
            help = "Unfreeze service"
        "--destroy"
            help = "Destroy service"
        "--api-key", "-k"
            help = "API key"
    end

    args = parse_args(ARGS, s)

    if args["%COMMAND%"] == "session"
        cmd_session(args["session"])
    elseif args["%COMMAND%"] == "service"
        cmd_service(args["service"])
    elseif args["source_file"] !== nothing
        cmd_execute(args)
    else
        println(stderr, "$(RED)Error: Provide source_file or use 'session'/'service' subcommand$(RESET)")
        exit(1)
    end
end

main()
