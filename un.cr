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


#!/usr/bin/env crystal

require "http/client"
require "json"
require "base64"
require "option_parser"

# Extension to language mapping
EXT_MAP = {
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
}

# ANSI color codes
BLUE = "\033[34m"
RED = "\033[31m"
GREEN = "\033[32m"
YELLOW = "\033[33m"
RESET = "\033[0m"

API_BASE = "https://api.unsandbox.com"

def detect_language(filename : String) : String
  ext = File.extname(filename).downcase
  EXT_MAP.fetch(ext, "unknown")
end

def get_api_key(args_key : String?) : String
  key = args_key || ENV["UNSANDBOX_API_KEY"]?
  if key.nil? || key.empty?
    STDERR.puts "#{RED}Error: UNSANDBOX_API_KEY not set#{RESET}"
    exit 1
  end
  key
end

def api_request(endpoint : String, api_key : String, method = "GET", data : JSON::Any? = nil)
  url = URI.parse(API_BASE + endpoint)
  headers = HTTP::Headers{
    "Content-Type" => "application/json",
    "Authorization" => "Bearer #{api_key}"
  }

  begin
    response = case method
    when "GET"
      HTTP::Client.get(url, headers: headers)
    when "POST"
      body = data ? data.to_json : ""
      HTTP::Client.post(url, headers: headers, body: body)
    when "DELETE"
      HTTP::Client.delete(url, headers: headers)
    else
      STDERR.puts "#{RED}Error: Unsupported method: #{method}#{RESET}"
      exit 1
    end

    JSON.parse(response.body)
  rescue ex
    STDERR.puts "#{RED}Error: Request failed: #{ex.message}#{RESET}"
    exit 1
  end
end

def cmd_execute(args)
  api_key = get_api_key(args[:api_key]?)

  filename = args[:source_file].as(String)
  unless File.exists?(filename)
    STDERR.puts "#{RED}Error: File not found: #{filename}#{RESET}"
    exit 1
  end

  language = detect_language(filename)
  if language == "unknown"
    STDERR.puts "#{RED}Error: Cannot detect language for #{filename}#{RESET}"
    exit 1
  end

  code = File.read(filename)

  # Build request payload
  payload = JSON.parse({language: language, code: code}.to_json)

  # Add environment variables
  if env_vars = args[:env]?.as?(Array(String))
    env_hash = {} of String => String
    env_vars.each do |e|
      if e.includes?('=')
        k, v = e.split('=', 2)
        env_hash[k] = v
      end
    end
    unless env_hash.empty?
      payload.as_h["env"] = JSON.parse(env_hash.to_json)
    end
  end

  # Add input files
  if files = args[:files]?.as?(Array(String))
    input_files = [] of JSON::Any
    files.each do |filepath|
      unless File.exists?(filepath)
        STDERR.puts "#{RED}Error: Input file not found: #{filepath}#{RESET}"
        exit 1
      end
      content = Base64.strict_encode(File.read(filepath))
      input_files << JSON.parse({
        filename: File.basename(filepath),
        content_base64: content
      }.to_json)
    end
    unless input_files.empty?
      payload.as_h["input_files"] = JSON.parse(input_files.to_json)
    end
  end

  # Add options
  if args[:artifacts]?.as?(Bool)
    payload.as_h["return_artifacts"] = JSON::Any.new(true)
  end
  if network = args[:network]?.as?(String)
    payload.as_h["network"] = JSON::Any.new(network)
  end

  # Execute
  result = api_request("/execute", api_key, method: "POST", data: payload)

  # Print output
  if stdout = result["stdout"]?.try(&.as_s?)
    print BLUE, stdout, RESET
  end
  if stderr = result["stderr"]?.try(&.as_s?)
    print RED, stderr, RESET
  end

  # Save artifacts
  if args[:artifacts]?.as?(Bool) && (artifacts = result["artifacts"]?.try(&.as_a?))
    out_dir = args[:output_dir]?.as?(String) || "."
    Dir.mkdir_p(out_dir)
    artifacts.each do |artifact|
      filename = artifact["filename"]?.try(&.as_s?) || "artifact"
      content = Base64.decode(artifact["content_base64"].as_s)
      path = File.join(out_dir, filename)
      File.write(path, content)
      File.chmod(path, 0o755)
      STDERR.puts "#{GREEN}Saved: #{path}#{RESET}"
    end
  end

  exit_code = result["exit_code"]?.try(&.as_i?) || 0
  exit exit_code
end

def cmd_session(args)
  api_key = get_api_key(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/sessions", api_key)
    sessions = result["sessions"]?.try(&.as_a?) || [] of JSON::Any
    if sessions.empty?
      puts "No active sessions"
    else
      printf "%-40s %-10s %-10s %s\n", "ID", "Shell", "Status", "Created"
      sessions.each do |s|
        printf "%-40s %-10s %-10s %s\n",
          s["id"]?.try(&.as_s?) || "N/A",
          s["shell"]?.try(&.as_s?) || "N/A",
          s["status"]?.try(&.as_s?) || "N/A",
          s["created_at"]?.try(&.as_s?) || "N/A"
      end
    end
    return
  end

  if kill_id = args[:kill]?.as?(String)
    api_request("/sessions/#{kill_id}", api_key, method: "DELETE")
    puts "#{GREEN}Session terminated: #{kill_id}#{RESET}"
    return
  end

  STDERR.puts "#{RED}Error: Use --list or --kill#{RESET}"
  exit 1
end

def cmd_service(args)
  api_key = get_api_key(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/services", api_key)
    services = result["services"]?.try(&.as_a?) || [] of JSON::Any
    if services.empty?
      puts "No services"
    else
      printf "%-20s %-15s %-10s %-15s %s\n", "ID", "Name", "Status", "Ports", "Domains"
      services.each do |s|
        ports = s["ports"]?.try(&.as_a?.map(&.as_i).join(',')) || ""
        domains = s["domains"]?.try(&.as_a?.map(&.as_s).join(',')) || ""
        printf "%-20s %-15s %-10s %-15s %s\n",
          s["id"]?.try(&.as_s?) || "N/A",
          s["name"]?.try(&.as_s?) || "N/A",
          s["status"]?.try(&.as_s?) || "N/A",
          ports, domains
      end
    end
    return
  end

  if info_id = args[:info]?.as?(String)
    result = api_request("/services/#{info_id}", api_key)
    puts result.to_pretty_json
    return
  end

  if logs_id = args[:logs]?.as?(String)
    result = api_request("/services/#{logs_id}/logs", api_key)
    puts result["logs"]?.try(&.as_s?) || ""
    return
  end

  if sleep_id = args[:sleep]?.as?(String)
    api_request("/services/#{sleep_id}/sleep", api_key, method: "POST")
    puts "#{GREEN}Service sleeping: #{sleep_id}#{RESET}"
    return
  end

  if wake_id = args[:wake]?.as?(String)
    api_request("/services/#{wake_id}/wake", api_key, method: "POST")
    puts "#{GREEN}Service waking: #{wake_id}#{RESET}"
    return
  end

  if destroy_id = args[:destroy]?.as?(String)
    api_request("/services/#{destroy_id}", api_key, method: "DELETE")
    puts "#{GREEN}Service destroyed: #{destroy_id}#{RESET}"
    return
  end

  # Create new service
  if name = args[:name]?.as?(String)
    payload = JSON.parse({name: name}.to_json)

    # Add ports
    if ports_str = args[:ports]?.as?(String)
      ports = ports_str.split(',').map(&.to_i)
      payload.as_h["ports"] = JSON.parse(ports.to_json)
    end

    # Add domains
    if domains_str = args[:domains]?.as?(String)
      domains = domains_str.split(',')
      payload.as_h["domains"] = JSON.parse(domains.to_json)
    end

    # Add service_type
    if service_type = args[:service_type]?.as?(String)
      payload.as_h["service_type"] = JSON::Any.new(service_type)
    end

    # Add bootstrap
    if bootstrap = args[:bootstrap]?.as?(String)
      # Check if bootstrap is a file
      if File.exists?(bootstrap)
        payload.as_h["bootstrap"] = JSON::Any.new(File.read(bootstrap))
      else
        payload.as_h["bootstrap"] = JSON::Any.new(bootstrap)
      end
    end

    # Add network
    if network = args[:network]?.as?(String)
      payload.as_h["network"] = JSON::Any.new(network)
    end

    # Create service
    result = api_request("/services", api_key, method: "POST", data: payload)
    puts "#{GREEN}Service created: #{result["id"]?.try(&.as_s?) || "N/A"}#{RESET}"
    puts "Name: #{result["name"]?.try(&.as_s?) || "N/A"}"
    if url = result["url"]?.try(&.as_s?)
      puts "URL: #{url}"
    end
    return
  end

  STDERR.puts "#{RED}Error: Use --list, --info, --logs, --sleep, --wake, --destroy, or --name to create#{RESET}"
  exit 1
end

def main
  args = {
    source_file: nil,
    api_key: nil,
    network: nil,
    env: [] of String,
    files: [] of String,
    artifacts: false,
    output_dir: nil,
    command: nil,
    list: false,
    kill: nil,
    info: nil,
    logs: nil,
    sleep: nil,
    wake: nil,
    destroy: nil,
    name: nil,
    ports: nil,
    domains: nil,
    service_type: nil,
    bootstrap: nil
  } of Symbol => (String | Array(String) | Bool | Nil)

  parser = OptionParser.new do |opts|
    opts.banner = "Usage: un.cr [options] <source_file>\n       un.cr session [options]\n       un.cr service [options]"

    opts.on("-k API_KEY", "--api-key=API_KEY", "API key") { |k| args[:api_key] = k }
    opts.on("-n NETWORK", "--network=NETWORK", "Network mode") { |n| args[:network] = n }
    opts.on("-e ENV", "--env=ENV", "Environment variable (KEY=VALUE)") { |e| args[:env].as(Array(String)) << e }
    opts.on("-f FILE", "--files=FILE", "Input file") { |f| args[:files].as(Array(String)) << f }
    opts.on("-a", "--artifacts", "Return artifacts") { args[:artifacts] = true }
    opts.on("-o DIR", "--output-dir=DIR", "Output directory") { |d| args[:output_dir] = d }
    opts.on("-l", "--list", "List items") { args[:list] = true }
    opts.on("--kill=ID", "Kill session") { |id| args[:kill] = id }
    opts.on("--info=ID", "Get service info") { |id| args[:info] = id }
    opts.on("--logs=ID", "Get service logs") { |id| args[:logs] = id }
    opts.on("--sleep=ID", "Sleep service") { |id| args[:sleep] = id }
    opts.on("--wake=ID", "Wake service") { |id| args[:wake] = id }
    opts.on("--destroy=ID", "Destroy service") { |id| args[:destroy] = id }
    opts.on("--name=NAME", "Service name") { |n| args[:name] = n }
    opts.on("--ports=PORTS", "Comma-separated ports") { |p| args[:ports] = p }
    opts.on("--domains=DOMAINS", "Comma-separated domains") { |d| args[:domains] = d }
    opts.on("--type=TYPE", "Service type for SRV records") { |t| args[:service_type] = t }
    opts.on("--bootstrap=CMD", "Bootstrap command/file") { |b| args[:bootstrap] = b }

    opts.unknown_args do |before, after|
      if before.size > 0
        case before[0]
        when "session"
          args[:command] = "session"
        when "service"
          args[:command] = "service"
        else
          args[:source_file] = before[0]
        end
      end
    end
  end

  parser.parse

  if args[:command] == "session"
    cmd_session(args)
  elsif args[:command] == "service"
    cmd_service(args)
  elsif args[:source_file]
    cmd_execute(args)
  else
    STDERR.puts parser
    exit 1
  end
end

main
