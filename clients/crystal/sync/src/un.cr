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
require "openssl/hmac"

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
PORTAL_BASE = "https://unsandbox.com"
MAX_ENV_CONTENT_SIZE = 65536
LANGUAGES_CACHE_TTL = 3600  # 1 hour in seconds

def detect_language(filename : String) : String
  ext = File.extname(filename).downcase
  EXT_MAP.fetch(ext, "unknown")
end

def get_languages_cache_path : String?
  home = ENV["HOME"]?
  return nil if home.nil? || home.empty?
  File.join(home, ".unsandbox", "languages.json")
end

def load_languages_cache : String?
  cache_path = get_languages_cache_path
  return nil if cache_path.nil? || !File.exists?(cache_path)

  begin
    content = File.read(cache_path)
    # Parse timestamp from JSON
    parsed = JSON.parse(content)
    cached_time = parsed["timestamp"]?.try(&.as_i64?)
    return nil if cached_time.nil?

    current_time = Time.utc.to_unix

    # Check if cache is still valid (within TTL)
    if current_time - cached_time < LANGUAGES_CACHE_TTL
      return content
    end
  rescue
    # Cache read failed, return nil to fetch fresh
  end

  nil
end

def save_languages_cache(response : JSON::Any)
  cache_path = get_languages_cache_path
  return if cache_path.nil?

  begin
    # Ensure directory exists
    cache_dir = File.dirname(cache_path)
    Dir.mkdir_p(cache_dir) unless Dir.exists?(cache_dir)

    # Extract languages array from response
    languages = response["languages"]?
    return if languages.nil?

    # Build cache JSON with timestamp
    timestamp = Time.utc.to_unix
    cache_data = {
      "languages" => languages,
      "timestamp" => timestamp
    }
    File.write(cache_path, cache_data.to_json)
  rescue
    # Cache write failed, ignore
  end
end

def get_api_keys(args_key : String?) : {String, String?}
  public_key = ENV["UNSANDBOX_PUBLIC_KEY"]?
  secret_key = ENV["UNSANDBOX_SECRET_KEY"]?

  # Fall back to UNSANDBOX_API_KEY for backwards compatibility
  if public_key.nil? || public_key.empty? || secret_key.nil? || secret_key.empty?
    legacy_key = args_key || ENV["UNSANDBOX_API_KEY"]?
    if legacy_key.nil? || legacy_key.empty?
      STDERR.puts "#{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set#{RESET}"
      exit 1
    end
    return {legacy_key, nil}
  end

  {public_key, secret_key}
end

def extract_challenge_id(response_body : String) : String?
  begin
    parsed = JSON.parse(response_body)
    parsed["challenge_id"]?.try(&.as_s?)
  rescue
    nil
  end
end

def handle_sudo_challenge(response_body : String, public_key : String, secret_key : String?, method : String, endpoint : String, body : String?) : JSON::Any
  challenge_id = extract_challenge_id(response_body)

  STDERR.puts "#{YELLOW}Confirmation required. Check your email for a one-time code.#{RESET}"
  STDERR.print "Enter OTP: "

  otp = STDIN.gets
  if otp.nil? || otp.strip.empty?
    STDERR.puts "#{RED}Error: Operation cancelled#{RESET}"
    exit 1
  end
  otp = otp.strip

  url = URI.parse(API_BASE + endpoint)
  headers = HTTP::Headers{
    "Content-Type" => "application/json"
  }

  request_body = body || ""

  # Add HMAC authentication headers
  if secret_key && !secret_key.empty?
    timestamp = Time.utc.to_unix.to_s
    message = "#{timestamp}:#{method}:#{endpoint}:#{request_body}"
    signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)

    headers["Authorization"] = "Bearer #{public_key}"
    headers["X-Timestamp"] = timestamp
    headers["X-Signature"] = signature
  else
    headers["Authorization"] = "Bearer #{public_key}"
  end

  # Add sudo headers
  headers["X-Sudo-OTP"] = otp
  if challenge_id
    headers["X-Sudo-Challenge"] = challenge_id
  end

  begin
    response = case method
    when "GET"
      HTTP::Client.get(url, headers: headers)
    when "POST"
      HTTP::Client.post(url, headers: headers, body: request_body)
    when "PATCH"
      HTTP::Client.patch(url, headers: headers, body: request_body)
    when "DELETE"
      HTTP::Client.delete(url, headers: headers)
    else
      STDERR.puts "#{RED}Error: Unsupported method: #{method}#{RESET}"
      exit 1
    end

    if response.status_code >= 200 && response.status_code < 300
      STDERR.puts "#{GREEN}Operation completed successfully#{RESET}"
      JSON.parse(response.body)
    else
      STDERR.puts "#{RED}Error: HTTP #{response.status_code}#{RESET}"
      begin
        error_json = JSON.parse(response.body)
        if error_msg = error_json["error"]?.try(&.as_s?)
          STDERR.puts error_msg
        else
          STDERR.puts response.body
        end
      rescue
        STDERR.puts response.body
      end
      exit 1
    end
  rescue ex
    STDERR.puts "#{RED}Error: #{ex.message}#{RESET}"
    exit 1
  end
end

def api_request_with_sudo(endpoint : String, public_key : String, secret_key : String?, method = "GET", data : JSON::Any? = nil) : {Int32, JSON::Any}
  url = URI.parse(API_BASE + endpoint)
  headers = HTTP::Headers{
    "Content-Type" => "application/json"
  }

  body = data ? data.to_json : ""

  # Add HMAC authentication headers if secret_key is provided
  if secret_key && !secret_key.empty?
    timestamp = Time.utc.to_unix.to_s
    message = "#{timestamp}:#{method}:#{endpoint}:#{body}"

    signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)

    headers["Authorization"] = "Bearer #{public_key}"
    headers["X-Timestamp"] = timestamp
    headers["X-Signature"] = signature
  else
    # Legacy API key authentication
    headers["Authorization"] = "Bearer #{public_key}"
  end

  begin
    response = case method
    when "GET"
      HTTP::Client.get(url, headers: headers)
    when "POST"
      HTTP::Client.post(url, headers: headers, body: body)
    when "PATCH"
      HTTP::Client.patch(url, headers: headers, body: body)
    when "DELETE"
      HTTP::Client.delete(url, headers: headers)
    else
      STDERR.puts "#{RED}Error: Unsupported method: #{method}#{RESET}"
      exit 1
    end

    {response.status_code, JSON.parse(response.body)}
  rescue ex
    error_msg = ex.message || ""
    if error_msg.downcase.includes?("timestamp")
      STDERR.puts "#{RED}Error: Request timestamp expired (must be within 5 minutes of server time)#{RESET}"
      STDERR.puts "#{YELLOW}Your computer's clock may have drifted.#{RESET}"
      STDERR.puts "Check your system time and sync with NTP if needed:"
      STDERR.puts "  Linux:   sudo ntpdate -s time.nist.gov"
      STDERR.puts "  macOS:   sudo sntp -sS time.apple.com"
      STDERR.puts "  Windows: w32tm /resync"
    else
      STDERR.puts "#{RED}Error: Request failed: #{ex.message}#{RESET}"
    end
    exit 1
  end
end

def api_request(endpoint : String, public_key : String, secret_key : String?, method = "GET", data : JSON::Any? = nil)
  url = URI.parse(API_BASE + endpoint)
  headers = HTTP::Headers{
    "Content-Type" => "application/json"
  }

  body = data ? data.to_json : ""

  # Add HMAC authentication headers if secret_key is provided
  if secret_key && !secret_key.empty?
    timestamp = Time.utc.to_unix.to_s
    message = "#{timestamp}:#{method}:#{endpoint}:#{body}"

    signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)

    headers["Authorization"] = "Bearer #{public_key}"
    headers["X-Timestamp"] = timestamp
    headers["X-Signature"] = signature
  else
    # Legacy API key authentication
    headers["Authorization"] = "Bearer #{public_key}"
  end

  begin
    response = case method
    when "GET"
      HTTP::Client.get(url, headers: headers)
    when "POST"
      HTTP::Client.post(url, headers: headers, body: body)
    when "PATCH"
      HTTP::Client.patch(url, headers: headers, body: body)
    when "DELETE"
      HTTP::Client.delete(url, headers: headers)
    else
      STDERR.puts "#{RED}Error: Unsupported method: #{method}#{RESET}"
      exit 1
    end

    JSON.parse(response.body)
  rescue ex
    error_msg = ex.message || ""
    if error_msg.downcase.includes?("timestamp") || (response && response.status_code == 401 && response.body.downcase.includes?("timestamp"))
      STDERR.puts "#{RED}Error: Request timestamp expired (must be within 5 minutes of server time)#{RESET}"
      STDERR.puts "#{YELLOW}Your computer's clock may have drifted.#{RESET}"
      STDERR.puts "Check your system time and sync with NTP if needed:"
      STDERR.puts "  Linux:   sudo ntpdate -s time.nist.gov"
      STDERR.puts "  macOS:   sudo sntp -sS time.apple.com"
      STDERR.puts "  Windows: w32tm /resync"
    else
      STDERR.puts "#{RED}Error: Request failed: #{ex.message}#{RESET}"
    end
    exit 1
  end
end

def api_request_text(endpoint : String, public_key : String, secret_key : String?, body : String) : Bool
  url = URI.parse(API_BASE + endpoint)
  headers = HTTP::Headers{
    "Content-Type" => "text/plain"
  }

  # Add HMAC authentication headers if secret_key is provided
  if secret_key && !secret_key.empty?
    timestamp = Time.utc.to_unix.to_s
    message = "#{timestamp}:PUT:#{endpoint}:#{body}"
    signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)
    headers["Authorization"] = "Bearer #{public_key}"
    headers["X-Timestamp"] = timestamp
    headers["X-Signature"] = signature
  else
    headers["Authorization"] = "Bearer #{public_key}"
  end

  begin
    response = HTTP::Client.put(url, headers: headers, body: body)
    return response.status_code >= 200 && response.status_code < 300
  rescue
    return false
  end
end

def read_env_file(path : String) : String
  unless File.exists?(path)
    STDERR.puts "#{RED}Error: Env file not found: #{path}#{RESET}"
    exit 1
  end
  File.read(path)
end

def build_env_content(envs : Array(String), env_file : String?) : String
  lines = envs.dup
  if env_file && !env_file.empty?
    content = read_env_file(env_file)
    content.split('\n').each do |line|
      trimmed = line.strip
      if !trimmed.empty? && !trimmed.starts_with?('#')
        lines << trimmed
      end
    end
  end
  lines.join('\n')
end

def cmd_service_env(args)
  public_key, secret_key = get_api_keys(args[:api_key]?.as?(String))

  action = args[:env_action]?.as?(String) || ""
  target = args[:env_target]?.as?(String) || ""

  case action
  when "status"
    if target.empty?
      STDERR.puts "#{RED}Error: service env status requires service ID#{RESET}"
      exit 1
    end
    result = api_request("/services/#{target}/env", public_key, secret_key)
    if result["has_vault"]?.try(&.as_bool?) == true
      puts "#{GREEN}Vault: configured#{RESET}"
      if env_count = result["env_count"]?
        puts "Variables: #{env_count}"
      end
      if updated_at = result["updated_at"]?.try(&.as_s?)
        puts "Updated: #{updated_at}"
      end
    else
      puts "#{YELLOW}Vault: not configured#{RESET}"
    end
  when "set"
    if target.empty?
      STDERR.puts "#{RED}Error: service env set requires service ID#{RESET}"
      exit 1
    end
    svc_envs = args[:svc_envs]?.as?(Array(String)) || [] of String
    svc_env_file = args[:svc_env_file]?.as?(String)
    if svc_envs.empty? && (svc_env_file.nil? || svc_env_file.empty?)
      STDERR.puts "#{RED}Error: service env set requires -e or --env-file#{RESET}"
      exit 1
    end
    env_content = build_env_content(svc_envs, svc_env_file)
    if env_content.size > MAX_ENV_CONTENT_SIZE
      STDERR.puts "#{RED}Error: Env content exceeds maximum size of 64KB#{RESET}"
      exit 1
    end
    if api_request_text("/services/#{target}/env", public_key, secret_key, env_content)
      puts "#{GREEN}Vault updated for service #{target}#{RESET}"
    else
      STDERR.puts "#{RED}Error: Failed to update vault#{RESET}"
      exit 1
    end
  when "export"
    if target.empty?
      STDERR.puts "#{RED}Error: service env export requires service ID#{RESET}"
      exit 1
    end
    result = api_request("/services/#{target}/env/export", public_key, secret_key, method: "POST", data: JSON.parse("{}"))
    if content = result["content"]?.try(&.as_s?)
      print content
    end
  when "delete"
    if target.empty?
      STDERR.puts "#{RED}Error: service env delete requires service ID#{RESET}"
      exit 1
    end
    api_request("/services/#{target}/env", public_key, secret_key, method: "DELETE")
    puts "#{GREEN}Vault deleted for service #{target}#{RESET}"
  else
    STDERR.puts "#{RED}Error: Unknown env action: #{action}#{RESET}"
    STDERR.puts "Usage: un.cr service env <status|set|export|delete> <service_id>"
    exit 1
  end
end

def cmd_execute(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

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
  result = api_request("/execute", public_key, secret_key, method: "POST", data: payload)

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
  public_key, secret_key = get_api_keys(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/sessions", public_key, secret_key)
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

  if info_id = args[:session_info]?.as?(String)
    result = api_request("/sessions/#{info_id}", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if kill_id = args[:kill]?.as?(String)
    api_request("/sessions/#{kill_id}", public_key, secret_key, method: "DELETE")
    puts "#{GREEN}Session terminated: #{kill_id}#{RESET}"
    return
  end

  if freeze_id = args[:session_freeze]?.as?(String)
    api_request("/sessions/#{freeze_id}/freeze", public_key, secret_key, method: "POST")
    puts "#{GREEN}Session frozen: #{freeze_id}#{RESET}"
    return
  end

  if unfreeze_id = args[:session_unfreeze]?.as?(String)
    api_request("/sessions/#{unfreeze_id}/unfreeze", public_key, secret_key, method: "POST")
    puts "#{GREEN}Session unfreezing: #{unfreeze_id}#{RESET}"
    return
  end

  if boost_id = args[:session_boost]?.as?(String)
    vcpu = args[:vcpu]?.as?(Int32) || 2
    payload = JSON.parse({vcpu: vcpu}.to_json)
    api_request("/sessions/#{boost_id}/boost", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Session boosted to #{vcpu} vCPU: #{boost_id}#{RESET}"
    return
  end

  if unboost_id = args[:session_unboost]?.as?(String)
    api_request("/sessions/#{unboost_id}/unboost", public_key, secret_key, method: "POST")
    puts "#{GREEN}Session unboosted: #{unboost_id}#{RESET}"
    return
  end

  if execute_id = args[:session_execute]?.as?(String)
    command = args[:command]?.as?(String) || ""
    payload = JSON.parse({command: command}.to_json)
    result = api_request("/sessions/#{execute_id}/execute", public_key, secret_key, method: "POST", data: payload)
    if stdout = result["stdout"]?.try(&.as_s?)
      print BLUE, stdout, RESET
    end
    if stderr = result["stderr"]?.try(&.as_s?)
      print RED, stderr, RESET
    end
    return
  end

  # Create new session
  payload = JSON.parse({shell: "bash"}.to_json)

  if network = args[:network]?.as?(String)
    payload.as_h["network"] = JSON::Any.new(network)
  end

  if shell = args[:shell]?.as?(String)
    payload.as_h["shell"] = JSON::Any.new(shell)
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

  puts "#{YELLOW}Creating session...#{RESET}"
  result = api_request("/sessions", public_key, secret_key, method: "POST", data: payload)
  puts "#{GREEN}Session created: #{result["id"]?.try(&.as_s?) || "N/A"}#{RESET}"
  puts "#{YELLOW}(Interactive sessions require WebSocket - use un2 for full support)#{RESET}"
end

def cmd_languages(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

  # Try to load from cache first
  cached_response = load_languages_cache
  result : JSON::Any

  if cached_response
    result = JSON.parse(cached_response)
  else
    # Fetch from API
    result = api_request("/languages", public_key, secret_key)

    # Save to cache
    save_languages_cache(result)
  end

  languages = result["languages"]?.try(&.as_a?) || [] of JSON::Any

  if args[:json]?.as?(Bool)
    # Output as JSON array of language names
    names = languages.map { |l| l["name"]?.try(&.as_s?) || "" }.reject(&.empty?)
    puts names.to_json
  else
    # Output one language per line
    languages.each do |lang|
      if name = lang["name"]?.try(&.as_s?)
        puts name
      end
    end
  end
end

def cmd_key(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

  # Validate key
  url = URI.parse(PORTAL_BASE + "/keys/validate")
  headers = HTTP::Headers{
    "Content-Type" => "application/json"
  }

  body = "{}"

  # Add HMAC authentication headers if secret_key is provided
  if secret_key && !secret_key.empty?
    timestamp = Time.utc.to_unix.to_s
    message = "#{timestamp}:POST:/keys/validate:#{body}"

    signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)

    headers["Authorization"] = "Bearer #{public_key}"
    headers["X-Timestamp"] = timestamp
    headers["X-Signature"] = signature
  else
    # Legacy API key authentication
    headers["Authorization"] = "Bearer #{public_key}"
  end

  begin
    response = HTTP::Client.post(url, headers: headers, body: body)
    result = JSON.parse(response.body)

    status = result["status"]?.try(&.as_s?) || "unknown"
    public_key = result["public_key"]?.try(&.as_s?) || "N/A"
    tier = result["tier"]?.try(&.as_s?) || "N/A"

    case status
    when "valid"
      puts "#{GREEN}Valid#{RESET}"
      puts "Public Key: #{public_key}"
      puts "Tier: #{tier}"
      if expires_at = result["expires_at"]?.try(&.as_s?)
        puts "Expires: #{expires_at}"
      end

      # Handle --extend flag
      if args[:extend]?.as?(Bool)
        extend_url = "#{PORTAL_BASE}/keys/extend?pk=#{public_key}"
        puts "\n#{BLUE}Opening browser to extend key...#{RESET}"
        # Try common browser commands
        ["xdg-open", "open", "firefox", "chromium", "google-chrome"].each do |browser|
          if system("which #{browser} > /dev/null 2>&1")
            system("#{browser} '#{extend_url}' > /dev/null 2>&1 &")
            break
          end
        end
        puts extend_url
      end

    when "expired"
      puts "#{RED}Expired#{RESET}"
      puts "Public Key: #{public_key}"
      puts "Tier: #{tier}"
      if expired_at = result["expires_at"]?.try(&.as_s?)
        puts "Expired: #{expired_at}"
      end
      puts "#{YELLOW}To renew: Visit #{PORTAL_BASE}/keys/extend#{RESET}"

      # Handle --extend flag for expired keys
      if args[:extend]?.as?(Bool)
        extend_url = "#{PORTAL_BASE}/keys/extend?pk=#{public_key}"
        puts "\n#{BLUE}Opening browser to renew key...#{RESET}"
        ["xdg-open", "open", "firefox", "chromium", "google-chrome"].each do |browser|
          if system("which #{browser} > /dev/null 2>&1")
            system("#{browser} '#{extend_url}' > /dev/null 2>&1 &")
            break
          end
        end
        puts extend_url
      end

    when "invalid"
      puts "#{RED}Invalid#{RESET}"
      STDERR.puts "#{RED}Error: API key is not valid#{RESET}"
      exit 1

    else
      puts "#{YELLOW}Unknown status: #{status}#{RESET}"
    end

  rescue ex
    STDERR.puts "#{RED}Error: Failed to validate key: #{ex.message}#{RESET}"
    exit 1
  end
end

def cmd_image(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/images", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if info_id = args[:image_info]?.as?(String)
    result = api_request("/images/#{info_id}", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if del_id = args[:image_delete]?.as?(String)
    status_code, response = api_request_with_sudo("/images/#{del_id}", public_key, secret_key, method: "DELETE")
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "DELETE", "/images/#{del_id}", nil)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Image deleted: #{del_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if lock_id = args[:image_lock]?.as?(String)
    payload = JSON.parse({}.to_json)
    api_request("/images/#{lock_id}/lock", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Image locked: #{lock_id}#{RESET}"
    return
  end

  if unlock_id = args[:image_unlock]?.as?(String)
    payload = JSON.parse({}.to_json)
    body = "{}"
    status_code, response = api_request_with_sudo("/images/#{unlock_id}/unlock", public_key, secret_key, method: "POST", data: payload)
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "POST", "/images/#{unlock_id}/unlock", body)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Image unlocked: #{unlock_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if publish_id = args[:image_publish]?.as?(String)
    source_type = args[:image_source_type]?.as?(String)
    if source_type.nil? || source_type.empty?
      STDERR.puts "#{RED}Error: --publish requires --source-type (service or snapshot)#{RESET}"
      exit 1
    end
    payload = JSON.parse({source_type: source_type, source_id: publish_id}.to_json)
    if name = args[:image_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    result = api_request("/images/publish", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Image published#{RESET}"
    puts result.to_pretty_json
    return
  end

  if visibility_id = args[:image_visibility_id]?.as?(String)
    visibility = args[:image_visibility]?.as?(String)
    if visibility.nil? || visibility.empty?
      STDERR.puts "#{RED}Error: --visibility requires visibility mode (private, unlisted, public)#{RESET}"
      exit 1
    end
    payload = JSON.parse({visibility: visibility}.to_json)
    api_request("/images/#{visibility_id}/visibility", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Image visibility set to: #{visibility}#{RESET}"
    return
  end

  if spawn_id = args[:image_spawn]?.as?(String)
    payload = JSON.parse({}.to_json)
    if name = args[:image_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    if ports_str = args[:image_ports]?.as?(String)
      ports = ports_str.split(',').map(&.to_i)
      payload.as_h["ports"] = JSON.parse(ports.to_json)
    end
    result = api_request("/images/#{spawn_id}/spawn", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Service spawned from image#{RESET}"
    puts result.to_pretty_json
    return
  end

  if clone_id = args[:image_clone]?.as?(String)
    payload = JSON.parse({}.to_json)
    if name = args[:image_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    result = api_request("/images/#{clone_id}/clone", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Image cloned#{RESET}"
    puts result.to_pretty_json
    return
  end

  if grant_id = args[:image_grant]?.as?(String)
    trusted_key = args[:image_trusted_key]?.as?(String)
    if trusted_key.nil? || trusted_key.empty?
      STDERR.puts "#{RED}Error: --grant requires --trusted-key#{RESET}"
      exit 1
    end
    payload = JSON.parse({trusted_api_key: trusted_key}.to_json)
    api_request("/images/#{grant_id}/grant", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Access granted to #{trusted_key}#{RESET}"
    return
  end

  if revoke_id = args[:image_revoke]?.as?(String)
    trusted_key = args[:image_trusted_key]?.as?(String)
    if trusted_key.nil? || trusted_key.empty?
      STDERR.puts "#{RED}Error: --revoke requires --trusted-key#{RESET}"
      exit 1
    end
    payload = JSON.parse({trusted_api_key: trusted_key}.to_json)
    api_request("/images/#{revoke_id}/revoke", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Access revoked from #{trusted_key}#{RESET}"
    return
  end

  if trusted_id = args[:image_trusted]?.as?(String)
    result = api_request("/images/#{trusted_id}/trusted", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if transfer_id = args[:image_transfer]?.as?(String)
    to_key = args[:image_to_key]?.as?(String)
    if to_key.nil? || to_key.empty?
      STDERR.puts "#{RED}Error: --transfer requires --to-key#{RESET}"
      exit 1
    end
    payload = JSON.parse({to_api_key: to_key}.to_json)
    status_code, response = api_request_with_sudo("/images/#{transfer_id}/transfer", public_key, secret_key, method: "POST", data: payload)
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "POST", "/images/#{transfer_id}/transfer", payload.to_json)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Image transferred to #{to_key}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  # Default: list images
  result = api_request("/images", public_key, secret_key)
  puts result.to_pretty_json
end

def cmd_snapshot(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/snapshots", public_key, secret_key)
    snapshots = result["snapshots"]?.try(&.as_a?) || [] of JSON::Any
    if snapshots.empty?
      puts "No snapshots"
    else
      printf "%-20s %-20s %-10s %-10s %-10s %s\n", "ID", "Name", "Type", "Hot", "Locked", "Created"
      snapshots.each do |s|
        printf "%-20s %-20s %-10s %-10s %-10s %s\n",
          s["id"]?.try(&.as_s?) || "N/A",
          s["name"]?.try(&.as_s?) || "N/A",
          s["type"]?.try(&.as_s?) || "N/A",
          s["hot"]?.try(&.as_bool?) ? "yes" : "no",
          s["locked"]?.try(&.as_bool?) ? "yes" : "no",
          s["created_at"]?.try(&.as_s?) || "N/A"
      end
    end
    return
  end

  if info_id = args[:snapshot_info]?.as?(String)
    result = api_request("/snapshots/#{info_id}", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if session_id = args[:snapshot_session]?.as?(String)
    payload = JSON.parse({}.to_json)
    if name = args[:snapshot_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    if args[:snapshot_hot]?.as?(Bool)
      payload.as_h["hot"] = JSON::Any.new(true)
    end
    result = api_request("/sessions/#{session_id}/snapshot", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Snapshot created#{RESET}"
    puts result.to_pretty_json
    return
  end

  if service_id = args[:snapshot_service]?.as?(String)
    payload = JSON.parse({}.to_json)
    if name = args[:snapshot_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    if args[:snapshot_hot]?.as?(Bool)
      payload.as_h["hot"] = JSON::Any.new(true)
    end
    result = api_request("/services/#{service_id}/snapshot", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Snapshot created#{RESET}"
    puts result.to_pretty_json
    return
  end

  if restore_id = args[:snapshot_restore]?.as?(String)
    payload = JSON.parse({}.to_json)
    result = api_request("/snapshots/#{restore_id}/restore", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Snapshot restored#{RESET}"
    puts result.to_pretty_json
    return
  end

  if del_id = args[:snapshot_delete]?.as?(String)
    status_code, response = api_request_with_sudo("/snapshots/#{del_id}", public_key, secret_key, method: "DELETE")
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "DELETE", "/snapshots/#{del_id}", nil)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Snapshot deleted: #{del_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if lock_id = args[:snapshot_lock]?.as?(String)
    payload = JSON.parse({}.to_json)
    api_request("/snapshots/#{lock_id}/lock", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Snapshot locked: #{lock_id}#{RESET}"
    return
  end

  if unlock_id = args[:snapshot_unlock]?.as?(String)
    payload = JSON.parse({}.to_json)
    body = "{}"
    status_code, response = api_request_with_sudo("/snapshots/#{unlock_id}/unlock", public_key, secret_key, method: "POST", data: payload)
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "POST", "/snapshots/#{unlock_id}/unlock", body)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Snapshot unlocked: #{unlock_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if clone_id = args[:snapshot_clone]?.as?(String)
    clone_type = args[:snapshot_clone_type]?.as?(String) || "session"
    payload = JSON.parse({clone_type: clone_type}.to_json)
    if name = args[:snapshot_name]?.as?(String)
      payload.as_h["name"] = JSON::Any.new(name)
    end
    if ports_str = args[:snapshot_ports]?.as?(String)
      ports = ports_str.split(',').map(&.to_i)
      payload.as_h["ports"] = JSON.parse(ports.to_json)
    end
    result = api_request("/snapshots/#{clone_id}/clone", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Snapshot cloned#{RESET}"
    puts result.to_pretty_json
    return
  end

  # Default: list snapshots
  result = api_request("/snapshots", public_key, secret_key)
  puts result.to_pretty_json
end

def cmd_logs(args)
  public_key, secret_key = get_api_keys(args[:api_key]?)

  source = args[:logs_source]?.as?(String) || "all"
  lines = args[:logs_lines]?.as?(Int32) || 100
  since = args[:logs_since]?.as?(String) || "1h"
  grep_pattern = args[:logs_grep]?.as?(String)

  endpoint = "/paas/logs?source=#{source}&lines=#{lines}&since=#{since}"
  if grep_pattern && !grep_pattern.empty?
    endpoint += "&grep=#{URI.encode_path(grep_pattern)}"
  end

  if args[:logs_follow]?.as?(Bool)
    # Streaming logs via SSE
    stream_endpoint = "/paas/logs/stream?source=#{source}"
    if grep_pattern && !grep_pattern.empty?
      stream_endpoint += "&grep=#{URI.encode_path(grep_pattern)}"
    end

    url = URI.parse(PORTAL_BASE + stream_endpoint)
    headers = HTTP::Headers{
      "Accept" => "text/event-stream"
    }

    # Add HMAC authentication headers
    if secret_key && !secret_key.empty?
      timestamp = Time.utc.to_unix.to_s
      message = "#{timestamp}:GET:#{stream_endpoint}:"
      signature = OpenSSL::HMAC.hexdigest(:sha256, secret_key, message)
      headers["Authorization"] = "Bearer #{public_key}"
      headers["X-Timestamp"] = timestamp
      headers["X-Signature"] = signature
    else
      headers["Authorization"] = "Bearer #{public_key}"
    end

    begin
      HTTP::Client.get(url, headers: headers) do |response|
        if response.status_code == 200
          response.body_io.each_line do |line|
            if line.starts_with?("data: ")
              data = line[6..]
              begin
                parsed = JSON.parse(data)
                src = parsed["source"]?.try(&.as_s?) || "unknown"
                msg = parsed["line"]?.try(&.as_s?) || data
                puts "[#{src}] #{msg}"
              rescue
                puts line
              end
            end
          end
        else
          STDERR.puts "#{RED}Error: HTTP #{response.status_code}#{RESET}"
          STDERR.puts response.body_io.gets_to_end
          exit 1
        end
      end
    rescue ex
      STDERR.puts "#{RED}Error: #{ex.message}#{RESET}"
      exit 1
    end
  else
    # Batch fetch
    result = api_request(endpoint, public_key, secret_key)
    if logs = result["logs"]?.try(&.as_a?)
      logs.each do |log|
        src = log["source"]?.try(&.as_s?) || "unknown"
        msg = log["line"]?.try(&.as_s?) || log.to_json
        ts = log["timestamp"]?.try(&.as_s?) || ""
        if ts.empty?
          puts "[#{src}] #{msg}"
        else
          puts "[#{ts}] [#{src}] #{msg}"
        end
      end
    else
      puts result.to_pretty_json
    end
  end
end

def cmd_health(args)
  begin
    url = URI.parse(API_BASE + "/health")
    response = HTTP::Client.get(url)
    if response.status_code == 200
      puts "#{GREEN}API is healthy#{RESET}"
      result = JSON.parse(response.body)
      puts result.to_pretty_json
    else
      puts "#{RED}API is unhealthy: HTTP #{response.status_code}#{RESET}"
      exit 1
    end
  rescue ex
    puts "#{RED}API is unreachable: #{ex.message}#{RESET}"
    exit 1
  end
end

def cmd_version(args)
  puts "un.cr version 1.0.0"
  puts "API: #{API_BASE}"
  puts "Portal: #{PORTAL_BASE}"
end

def cmd_service(args)
  # Handle env subcommand
  if env_action = args[:env_action]?.as?(String)
    if !env_action.empty?
      cmd_service_env(args)
      return
    end
  end

  public_key, secret_key = get_api_keys(args[:api_key]?)

  if args[:list]?.as?(Bool)
    result = api_request("/services", public_key, secret_key)
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
    result = api_request("/services/#{info_id}", public_key, secret_key)
    puts result.to_pretty_json
    return
  end

  if logs_id = args[:logs]?.as?(String)
    result = api_request("/services/#{logs_id}/logs", public_key, secret_key)
    puts result["logs"]?.try(&.as_s?) || ""
    return
  end

  if sleep_id = args[:sleep]?.as?(String)
    api_request("/services/#{sleep_id}/freeze", public_key, secret_key, method: "POST")
    puts "#{GREEN}Service frozen: #{sleep_id}#{RESET}"
    return
  end

  if wake_id = args[:wake]?.as?(String)
    api_request("/services/#{wake_id}/unfreeze", public_key, secret_key, method: "POST")
    puts "#{GREEN}Service unfreezing: #{wake_id}#{RESET}"
    return
  end

  if unfreeze_on_demand_id = args[:unfreeze_on_demand]?.as?(String)
    enabled = args[:unfreeze_on_demand_enabled]?.as?(Bool) || true
    payload = JSON.parse({unfreeze_on_demand: enabled}.to_json)
    api_request("/services/#{unfreeze_on_demand_id}", public_key, secret_key, method: "PATCH", data: payload)
    status = enabled ? "enabled" : "disabled"
    puts "#{GREEN}Unfreeze-on-demand #{status} for service: #{unfreeze_on_demand_id}#{RESET}"
    return
  end

  if destroy_id = args[:destroy]?.as?(String)
    status_code, response = api_request_with_sudo("/services/#{destroy_id}", public_key, secret_key, method: "DELETE")
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "DELETE", "/services/#{destroy_id}", nil)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Service destroyed: #{destroy_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if execute_id = args[:execute]?.as?(String)
    command = args[:command]?.as?(String) || ""
    payload = JSON.parse({command: command}.to_json)
    result = api_request("/services/#{execute_id}/execute", public_key, secret_key, method: "POST", data: payload)
    if stdout = result["stdout"]?.try(&.as_s?)
      print BLUE, stdout, RESET
    end
    if stderr = result["stderr"]?.try(&.as_s?)
      print RED, stderr, RESET
    end
    return
  end

  if dump_id = args[:dump_bootstrap]?.as?(String)
    STDERR.puts "Fetching bootstrap script from #{dump_id}..."
    payload = JSON.parse({command: "cat /tmp/bootstrap.sh"}.to_json)
    result = api_request("/services/#{dump_id}/execute", public_key, secret_key, method: "POST", data: payload)

    if bootstrap = result["stdout"]?.try(&.as_s?)
      if file_path = args[:dump_file]?.as?(String)
        File.write(file_path, bootstrap)
        File.chmod(file_path, 0o755)
        puts "Bootstrap saved to #{file_path}"
      else
        print bootstrap
      end
    else
      STDERR.puts "#{RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)#{RESET}"
      exit 1
    end
    return
  end

  if resize_id = args[:resize]?.as?(String)
    vcpu = args[:vcpu]?.as?(Int32)
    if vcpu.nil? || vcpu < 1 || vcpu > 8
      STDERR.puts "#{RED}Error: --resize requires -v N (1-8)#{RESET}"
      exit 1
    end
    payload = JSON.parse({vcpu: vcpu}.to_json)
    api_request("/services/#{resize_id}", public_key, secret_key, method: "PATCH", data: payload)
    ram = vcpu * 2
    puts "#{GREEN}Service resized to #{vcpu} vCPU, #{ram} GB RAM#{RESET}"
    return
  end

  if lock_id = args[:service_lock]?.as?(String)
    payload = JSON.parse({}.to_json)
    api_request("/services/#{lock_id}/lock", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Service locked: #{lock_id}#{RESET}"
    return
  end

  if unlock_id = args[:service_unlock]?.as?(String)
    payload = JSON.parse({}.to_json)
    body = "{}"
    status_code, response = api_request_with_sudo("/services/#{unlock_id}/unlock", public_key, secret_key, method: "POST", data: payload)
    if status_code == 428
      handle_sudo_challenge(response.to_json, public_key, secret_key, "POST", "/services/#{unlock_id}/unlock", body)
    elsif status_code >= 200 && status_code < 300
      puts "#{GREEN}Service unlocked: #{unlock_id}#{RESET}"
    else
      STDERR.puts "#{RED}Error: HTTP #{status_code}#{RESET}"
      STDERR.puts response.to_json
      exit 1
    end
    return
  end

  if redeploy_id = args[:redeploy]?.as?(String)
    payload = JSON.parse({}.to_json)
    if bootstrap = args[:bootstrap]?.as?(String)
      payload.as_h["bootstrap"] = JSON::Any.new(bootstrap)
    end
    if bootstrap_file = args[:bootstrap_file]?.as?(String)
      if File.exists?(bootstrap_file)
        payload.as_h["bootstrap_content"] = JSON::Any.new(File.read(bootstrap_file))
      else
        STDERR.puts "#{RED}Error: Bootstrap file not found: #{bootstrap_file}#{RESET}"
        exit 1
      end
    end
    result = api_request("/services/#{redeploy_id}/redeploy", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Service redeployed: #{redeploy_id}#{RESET}"
    puts result.to_pretty_json
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
      payload.as_h["bootstrap"] = JSON::Any.new(bootstrap)
    end

    # Add bootstrap_file
    if bootstrap_file = args[:bootstrap_file]?.as?(String)
      if File.exists?(bootstrap_file)
        payload.as_h["bootstrap_content"] = JSON::Any.new(File.read(bootstrap_file))
      else
        STDERR.puts "#{RED}Error: Bootstrap file not found: #{bootstrap_file}#{RESET}"
        exit 1
      end
    end

    # Add network
    if network = args[:network]?.as?(String)
      payload.as_h["network"] = JSON::Any.new(network)
    end

    # Add unfreeze_on_demand
    if args[:create_unfreeze_on_demand]?.as?(Bool)
      payload.as_h["unfreeze_on_demand"] = JSON::Any.new(true)
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

    # Create service
    result = api_request("/services", public_key, secret_key, method: "POST", data: payload)
    puts "#{GREEN}Service created: #{result["id"]?.try(&.as_s?) || "N/A"}#{RESET}"
    puts "Name: #{result["name"]?.try(&.as_s?) || "N/A"}"
    if url = result["url"]?.try(&.as_s?)
      puts "URL: #{url}"
    end

    # Auto-set vault if -e or --env-file provided
    svc_envs = args[:svc_envs]?.as?(Array(String)) || [] of String
    svc_env_file = args[:svc_env_file]?.as?(String)
    if !svc_envs.empty? || (svc_env_file && !svc_env_file.empty?)
      if service_id = result["id"]?.try(&.as_s?)
        env_content = build_env_content(svc_envs, svc_env_file)
        if api_request_text("/services/#{service_id}/env", public_key, secret_key, env_content)
          puts "#{GREEN}Vault configured for service #{service_id}#{RESET}"
        else
          STDERR.puts "#{YELLOW}Warning: Failed to set vault#{RESET}"
        end
      end
    end
    return
  end

  STDERR.puts "#{RED}Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, --resize, or --name to create#{RESET}"
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
    execute: nil,
    dump_bootstrap: nil,
    dump_file: nil,
    resize: nil,
    vcpu: nil,
    unfreeze_on_demand: nil,
    unfreeze_on_demand_enabled: true,
    create_unfreeze_on_demand: false,
    name: nil,
    ports: nil,
    domains: nil,
    service_type: nil,
    bootstrap: nil,
    bootstrap_file: nil,
    extend: false,
    svc_envs: [] of String,
    svc_env_file: nil,
    env_action: nil,
    env_target: nil,
    json: false,
    shell: nil,
    # Session options
    session_info: nil,
    session_freeze: nil,
    session_unfreeze: nil,
    session_boost: nil,
    session_unboost: nil,
    session_execute: nil,
    # Service options
    service_lock: nil,
    service_unlock: nil,
    redeploy: nil,
    # Image options
    image_info: nil,
    image_delete: nil,
    image_lock: nil,
    image_unlock: nil,
    image_publish: nil,
    image_source_type: nil,
    image_visibility_id: nil,
    image_visibility: nil,
    image_spawn: nil,
    image_clone: nil,
    image_name: nil,
    image_ports: nil,
    image_grant: nil,
    image_revoke: nil,
    image_trusted: nil,
    image_trusted_key: nil,
    image_transfer: nil,
    image_to_key: nil,
    # Snapshot options
    snapshot_info: nil,
    snapshot_session: nil,
    snapshot_service: nil,
    snapshot_restore: nil,
    snapshot_delete: nil,
    snapshot_lock: nil,
    snapshot_unlock: nil,
    snapshot_clone: nil,
    snapshot_clone_type: nil,
    snapshot_name: nil,
    snapshot_hot: false,
    snapshot_ports: nil,
    # Logs options
    logs_source: nil,
    logs_lines: nil,
    logs_since: nil,
    logs_grep: nil,
    logs_follow: false
  } of Symbol => (String | Array(String) | Bool | Int32 | Nil)

  parser = OptionParser.new do |opts|
    opts.banner = "Usage: un.cr [options] <source_file>\n       un.cr languages [--json]\n       un.cr session [options]\n       un.cr service [options]\n       un.cr service env <action> <service_id> [options]\n       un.cr snapshot [options]\n       un.cr image [options]\n       un.cr logs [options]\n       un.cr key [options]\n       un.cr health\n       un.cr version\n\nService env commands:\n  env status <id>     Show vault status\n  env set <id>        Set vault (-e KEY=VALUE or --env-file FILE)\n  env export <id>     Export vault contents\n  env delete <id>     Delete vault"

    opts.on("-k API_KEY", "--api-key=API_KEY", "API key") { |k| args[:api_key] = k }
    opts.on("-n NETWORK", "--network=NETWORK", "Network mode") { |n| args[:network] = n }
    opts.on("-e ENV", "--env=ENV", "Environment variable (KEY=VALUE)") { |e|
      args[:env].as(Array(String)) << e
      args[:svc_envs].as(Array(String)) << e
    }
    opts.on("-f FILE", "--files=FILE", "Input file") { |f| args[:files].as(Array(String)) << f }
    opts.on("-a", "--artifacts", "Return artifacts") { args[:artifacts] = true }
    opts.on("-o DIR", "--output-dir=DIR", "Output directory") { |d| args[:output_dir] = d }
    opts.on("-l", "--list", "List items") { args[:list] = true }
    opts.on("--kill=ID", "Kill session") { |id| args[:kill] = id }
    opts.on("--info=ID", "Get service/session info") { |id| args[:info] = id }
    opts.on("--logs=ID", "Get service logs") { |id| args[:logs] = id }
    opts.on("--freeze=ID", "Freeze service/session") { |id| args[:sleep] = id }
    opts.on("--unfreeze=ID", "Unfreeze service/session") { |id| args[:wake] = id }
    opts.on("--unfreeze-on-demand=ID", "Set unfreeze-on-demand for service") { |id| args[:unfreeze_on_demand] = id }
    opts.on("--unfreeze-on-demand-enabled=BOOL", "Enable/disable unfreeze-on-demand (default: true)") { |b| args[:unfreeze_on_demand_enabled] = b.downcase == "true" }
    opts.on("--with-unfreeze-on-demand", "Enable unfreeze-on-demand when creating service") { args[:create_unfreeze_on_demand] = true }
    opts.on("--destroy=ID", "Destroy service") { |id| args[:destroy] = id }
    opts.on("--execute=ID", "Execute command in service/session") { |id| args[:execute] = id }
    opts.on("--command=CMD", "Command to execute (with --execute)") { |cmd| args[:command] = cmd }
    opts.on("--dump-bootstrap=ID", "Dump bootstrap script") { |id| args[:dump_bootstrap] = id }
    opts.on("--dump-file=FILE", "File to save bootstrap (with --dump-bootstrap)") { |file| args[:dump_file] = file }
    opts.on("--resize=ID", "Resize service vCPU") { |id| args[:resize] = id }
    opts.on("-v VCPU", "--vcpu=VCPU", "vCPU count (1-8) for resize/boost") { |v| args[:vcpu] = v.to_i }
    opts.on("--name=NAME", "Service/snapshot name") { |n| args[:name] = n }
    opts.on("--ports=PORTS", "Comma-separated ports") { |p| args[:ports] = p }
    opts.on("--domains=DOMAINS", "Comma-separated domains") { |d| args[:domains] = d }
    opts.on("--type=TYPE", "Service type for SRV records") { |t| args[:service_type] = t }
    opts.on("--bootstrap=CMD", "Bootstrap command or URI") { |b| args[:bootstrap] = b }
    opts.on("--bootstrap-file=FILE", "Upload local file as bootstrap script") { |f| args[:bootstrap_file] = f }
    opts.on("--env-file=FILE", "Load env vars from file (for vault)") { |f| args[:svc_env_file] = f }
    opts.on("--extend", "Open browser to extend/renew key") { args[:extend] = true }
    opts.on("--json", "Output as JSON array (for languages command)") { args[:json] = true }
    opts.on("--shell=SHELL", "Shell for session (bash, python3, etc.)") { |s| args[:shell] = s }
    opts.on("--lock=ID", "Lock service/snapshot/image") { |id| args[:service_lock] = id }
    opts.on("--unlock=ID", "Unlock service/snapshot/image") { |id| args[:service_unlock] = id }
    opts.on("--redeploy=ID", "Redeploy service") { |id| args[:redeploy] = id }
    opts.on("--boost=ID", "Boost session vCPU") { |id| args[:session_boost] = id }
    opts.on("--unboost=ID", "Unboost session") { |id| args[:session_unboost] = id }
    # Snapshot options
    opts.on("--snapshot-session=ID", "Create snapshot from session") { |id| args[:snapshot_session] = id }
    opts.on("--snapshot-service=ID", "Create snapshot from service") { |id| args[:snapshot_service] = id }
    opts.on("--restore=ID", "Restore snapshot") { |id| args[:snapshot_restore] = id }
    opts.on("--delete=ID", "Delete snapshot") { |id| args[:snapshot_delete] = id }
    opts.on("--clone=ID", "Clone snapshot") { |id| args[:snapshot_clone] = id }
    opts.on("--clone-type=TYPE", "Clone type (session or service)") { |t| args[:snapshot_clone_type] = t }
    opts.on("--hot", "Create hot snapshot") { args[:snapshot_hot] = true }
    # Logs options
    opts.on("--source=SOURCE", "Log source (all, api, portal, pool/cammy, pool/ai)") { |s| args[:logs_source] = s }
    opts.on("--lines=N", "Number of log lines") { |n| args[:logs_lines] = n.to_i }
    opts.on("--since=TIME", "Time window (1m, 5m, 1h, 1d)") { |t| args[:logs_since] = t }
    opts.on("--grep=PATTERN", "Filter pattern") { |p| args[:logs_grep] = p }
    opts.on("--follow", "Follow log stream") { args[:logs_follow] = true }
    # Image access options
    opts.on("--grant=ID", "Grant image access") { |id| args[:image_grant] = id }
    opts.on("--revoke=ID", "Revoke image access") { |id| args[:image_revoke] = id }
    opts.on("--trusted=ID", "List trusted keys for image") { |id| args[:image_trusted] = id }
    opts.on("--trusted-key=KEY", "API key to grant/revoke access") { |k| args[:image_trusted_key] = k }
    opts.on("--transfer=ID", "Transfer image ownership") { |id| args[:image_transfer] = id }
    opts.on("--to-key=KEY", "Target API key for transfer") { |k| args[:image_to_key] = k }

    opts.unknown_args do |before, after|
      if before.size > 0
        case before[0]
        when "session"
          args[:command] = "session"
          # Parse session subcommand options
          i = 1
          while i < before.size
            case before[i]
            when "--list", "-l"
              args[:list] = true
              i += 1
            when "--info"
              if i + 1 < before.size
                args[:session_info] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--freeze"
              if i + 1 < before.size
                args[:session_freeze] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--unfreeze"
              if i + 1 < before.size
                args[:session_unfreeze] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--boost"
              if i + 1 < before.size
                args[:session_boost] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--unboost"
              if i + 1 < before.size
                args[:session_unboost] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--execute"
              if i + 1 < before.size
                args[:session_execute] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--command"
              if i + 1 < before.size
                args[:command] = before[i + 1]
                i += 2
              else
                i += 1
              end
            else
              i += 1
            end
          end
        when "service"
          args[:command] = "service"
          # Check for env subcommand
          if before.size > 1 && before[1] == "env"
            if before.size > 2
              args[:env_action] = before[2]
            end
            if before.size > 3 && !before[3].starts_with?("-")
              args[:env_target] = before[3]
            end
            # Parse remaining args for -e
            i = 4
            while i < before.size
              if before[i] == "-e" && i + 1 < before.size
                args[:svc_envs].as(Array(String)) << before[i + 1]
                i += 2
              else
                i += 1
              end
            end
          else
            # Parse service subcommand options
            i = 1
            while i < before.size
              case before[i]
              when "--lock"
                if i + 1 < before.size
                  args[:service_lock] = before[i + 1]
                  i += 2
                else
                  i += 1
                end
              when "--unlock"
                if i + 1 < before.size
                  args[:service_unlock] = before[i + 1]
                  i += 2
                else
                  i += 1
                end
              when "--redeploy"
                if i + 1 < before.size
                  args[:redeploy] = before[i + 1]
                  i += 2
                else
                  i += 1
                end
              else
                i += 1
              end
            end
          end
        when "key"
          args[:command] = "key"
        when "languages"
          args[:command] = "languages"
        when "snapshot"
          args[:command] = "snapshot"
          # Parse snapshot subcommand options
          i = 1
          while i < before.size
            case before[i]
            when "--list", "-l"
              args[:list] = true
              i += 1
            when "--info"
              if i + 1 < before.size
                args[:snapshot_info] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--session"
              if i + 1 < before.size
                args[:snapshot_session] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--service"
              if i + 1 < before.size
                args[:snapshot_service] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--restore"
              if i + 1 < before.size
                args[:snapshot_restore] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--delete"
              if i + 1 < before.size
                args[:snapshot_delete] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--lock"
              if i + 1 < before.size
                args[:snapshot_lock] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--unlock"
              if i + 1 < before.size
                args[:snapshot_unlock] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--clone"
              if i + 1 < before.size
                args[:snapshot_clone] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--clone-type"
              if i + 1 < before.size
                args[:snapshot_clone_type] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--name"
              if i + 1 < before.size
                args[:snapshot_name] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--hot"
              args[:snapshot_hot] = true
              i += 1
            when "--ports"
              if i + 1 < before.size
                args[:snapshot_ports] = before[i + 1]
                i += 2
              else
                i += 1
              end
            else
              i += 1
            end
          end
        when "logs"
          args[:command] = "logs"
          # Parse logs subcommand options
          i = 1
          while i < before.size
            case before[i]
            when "--source"
              if i + 1 < before.size
                args[:logs_source] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--lines"
              if i + 1 < before.size
                args[:logs_lines] = before[i + 1].to_i
                i += 2
              else
                i += 1
              end
            when "--since"
              if i + 1 < before.size
                args[:logs_since] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--grep"
              if i + 1 < before.size
                args[:logs_grep] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--follow", "-f"
              args[:logs_follow] = true
              i += 1
            else
              i += 1
            end
          end
        when "health"
          args[:command] = "health"
        when "version"
          args[:command] = "version"
        when "image"
          args[:command] = "image"
          # Parse image subcommand options
          i = 1
          while i < before.size
            case before[i]
            when "--list", "-l"
              args[:list] = true
              i += 1
            when "--info"
              if i + 1 < before.size
                args[:image_info] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--delete"
              if i + 1 < before.size
                args[:image_delete] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--lock"
              if i + 1 < before.size
                args[:image_lock] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--unlock"
              if i + 1 < before.size
                args[:image_unlock] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--publish"
              if i + 1 < before.size
                args[:image_publish] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--source-type"
              if i + 1 < before.size
                args[:image_source_type] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--visibility"
              if i + 2 < before.size
                args[:image_visibility_id] = before[i + 1]
                args[:image_visibility] = before[i + 2]
                i += 3
              else
                i += 1
              end
            when "--spawn"
              if i + 1 < before.size
                args[:image_spawn] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--clone"
              if i + 1 < before.size
                args[:image_clone] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--name"
              if i + 1 < before.size
                args[:image_name] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--ports"
              if i + 1 < before.size
                args[:image_ports] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--grant"
              if i + 1 < before.size
                args[:image_grant] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--revoke"
              if i + 1 < before.size
                args[:image_revoke] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--trusted"
              if i + 1 < before.size
                args[:image_trusted] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--trusted-key"
              if i + 1 < before.size
                args[:image_trusted_key] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--transfer"
              if i + 1 < before.size
                args[:image_transfer] = before[i + 1]
                i += 2
              else
                i += 1
              end
            when "--to-key"
              if i + 1 < before.size
                args[:image_to_key] = before[i + 1]
                i += 2
              else
                i += 1
              end
            else
              i += 1
            end
          end
        else
          if before[0].starts_with?("-")
            STDERR.puts "#{RED}Unknown option: #{before[0]}#{RESET}"
            exit 1
          else
            args[:source_file] = before[0]
          end
        end
      end
    end
  end

  parser.parse

  if args[:command] == "session"
    cmd_session(args)
  elsif args[:command] == "service"
    cmd_service(args)
  elsif args[:command] == "key"
    cmd_key(args)
  elsif args[:command] == "languages"
    cmd_languages(args)
  elsif args[:command] == "image"
    cmd_image(args)
  elsif args[:command] == "snapshot"
    cmd_snapshot(args)
  elsif args[:command] == "logs"
    cmd_logs(args)
  elsif args[:command] == "health"
    cmd_health(args)
  elsif args[:command] == "version"
    cmd_version(args)
  elsif args[:source_file]
    cmd_execute(args)
  else
    STDERR.puts parser
    exit 1
  end
end

main
