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

#!/usr/bin/env ruby
# un.rb - Unsandbox CLI Client (Ruby Implementation)
#
# Full-featured CLI matching un.c capabilities:
# - Execute code with env vars, input files, artifacts
# - Interactive sessions with shell/REPL support
# - Persistent services with domains and ports
#
# Usage:
#   un.rb [options] <source_file>
#   un.rb session [options]
#   un.rb service [options]
#
# Requires: UNSANDBOX_API_KEY environment variable

require 'json'
require 'net/http'
require 'uri'
require 'base64'
require 'fileutils'
require 'optparse'

API_BASE = 'https://api.unsandbox.com'
PORTAL_BASE = 'https://unsandbox.com'
BLUE = "\e[34m"
RED = "\e[31m"
GREEN = "\e[32m"
YELLOW = "\e[33m"
RESET = "\e[0m"

EXT_MAP = {
  '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript',
  '.rb' => 'ruby', '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua',
  '.sh' => 'bash', '.go' => 'go', '.rs' => 'rust', '.c' => 'c',
  '.cpp' => 'cpp', '.cc' => 'cpp', '.cxx' => 'cpp',
  '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.fs' => 'fsharp',
  '.hs' => 'haskell', '.ml' => 'ocaml', '.clj' => 'clojure', '.scm' => 'scheme',
  '.lisp' => 'commonlisp', '.erl' => 'erlang', '.ex' => 'elixir', '.exs' => 'elixir',
  '.jl' => 'julia', '.r' => 'r', '.R' => 'r', '.cr' => 'crystal',
  '.d' => 'd', '.nim' => 'nim', '.zig' => 'zig', '.v' => 'v',
  '.dart' => 'dart', '.groovy' => 'groovy', '.scala' => 'scala',
  '.f90' => 'fortran', '.f95' => 'fortran', '.cob' => 'cobol',
  '.pro' => 'prolog', '.forth' => 'forth', '.4th' => 'forth',
  '.tcl' => 'tcl', '.raku' => 'raku', '.m' => 'objc'
}.freeze

def get_api_keys(args_key = nil)
  public_key = ENV['UNSANDBOX_PUBLIC_KEY']
  secret_key = ENV['UNSANDBOX_SECRET_KEY']

  unless public_key && secret_key
    old_key = args_key || ENV['UNSANDBOX_API_KEY']
    if old_key
      public_key = old_key
      secret_key = old_key
    else
      warn "#{RED}Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set#{RESET}"
      warn "#{RED}       (or legacy UNSANDBOX_API_KEY for backwards compatibility)#{RESET}"
      exit 1
    end
  end

  { public_key: public_key, secret_key: secret_key }
end

def detect_language(filename)
  ext = File.extname(filename).downcase
  lang = EXT_MAP[ext]
  unless lang
    begin
      first_line = File.open(filename, &:readline)
      if first_line.start_with?('#!')
        return 'python' if first_line.include?('python')
        return 'javascript' if first_line.include?('node')
        return 'ruby' if first_line.include?('ruby')
        return 'perl' if first_line.include?('perl')
        return 'bash' if first_line.include?('bash') || first_line.include?('/sh')
        return 'lua' if first_line.include?('lua')
        return 'php' if first_line.include?('php')
      end
    rescue
    end
    warn "#{RED}Error: Cannot detect language for #{filename}#{RESET}"
    exit 1
  end
  lang
end

def api_request(endpoint, method: 'GET', data: nil, keys:)
  require 'openssl'

  uri = URI("#{API_BASE}#{endpoint}")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.read_timeout = 300

  timestamp = Time.now.to_i.to_s
  body = data ? JSON.generate(data) : ''
  message = "#{timestamp}:#{method}:#{uri.path}#{uri.query ? "?#{uri.query}" : ''}:#{body}"
  signature = OpenSSL::HMAC.hexdigest('SHA256', keys[:secret_key], message)

  request = case method
            when 'GET' then Net::HTTP::Get.new(uri)
            when 'POST' then Net::HTTP::Post.new(uri)
            when 'DELETE' then Net::HTTP::Delete.new(uri)
            when 'PATCH' then Net::HTTP::Patch.new(uri)
            else raise "Unknown method: #{method}"
            end

  request['Authorization'] = "Bearer #{keys[:public_key]}"
  request['X-Timestamp'] = timestamp
  request['X-Signature'] = signature
  request['Content-Type'] = 'application/json'
  request.body = body if data

  response = http.request(request)
  unless response.is_a?(Net::HTTPSuccess)
    if response.code == '401' && response.body.downcase.include?('timestamp')
      warn "#{RED}Error: Request timestamp expired (must be within 5 minutes of server time)#{RESET}"
      warn "#{YELLOW}Your computer's clock may have drifted.#{RESET}"
      warn "#{YELLOW}Check your system time and sync with NTP if needed:#{RESET}"
      warn "  Linux:   sudo ntpdate -s time.nist.gov"
      warn "  macOS:   sudo sntp -sS time.apple.com"
      warn "  Windows: w32tm /resync"
    else
      warn "#{RED}Error: HTTP #{response.code} - #{response.body}#{RESET}"
    end
    exit 1
  end

  JSON.parse(response.body)
rescue => e
  warn "#{RED}Error: #{e.message}#{RESET}"
  exit 1
end

def api_request_text(endpoint, method:, body:, keys:)
  require 'openssl'

  uri = URI("#{API_BASE}#{endpoint}")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.read_timeout = 300

  timestamp = Time.now.to_i.to_s
  message = "#{timestamp}:#{method}:#{uri.path}:#{body}"
  signature = OpenSSL::HMAC.hexdigest('SHA256', keys[:secret_key], message)

  request = case method
            when 'PUT' then Net::HTTP::Put.new(uri)
            else raise "Unknown method: #{method}"
            end

  request['Authorization'] = "Bearer #{keys[:public_key]}"
  request['X-Timestamp'] = timestamp
  request['X-Signature'] = signature
  request['Content-Type'] = 'text/plain'
  request.body = body

  response = http.request(request)
  unless response.is_a?(Net::HTTPSuccess)
    return { 'error' => "HTTP #{response.code} - #{response.body}" }
  end

  JSON.parse(response.body)
rescue => e
  { 'error' => e.message }
end

# ============================================================================
# Environment Secrets Vault Functions
# ============================================================================

MAX_ENV_CONTENT_SIZE = 64 * 1024 # 64KB max env vault size

def service_env_status(service_id, keys)
  result = api_request("/services/#{service_id}/env", keys: keys)
  has_vault = result['has_vault']

  if !has_vault
    puts "Vault exists: no"
    puts "Variable count: 0"
  else
    puts "Vault exists: yes"
    puts "Variable count: #{result['count'] || 0}"
    if result['updated_at']
      puts "Last updated: #{Time.at(result['updated_at']).strftime('%Y-%m-%d %H:%M:%S')}"
    end
  end
end

def service_env_set(service_id, env_content, keys)
  if env_content.nil? || env_content.empty?
    warn "#{RED}Error: No environment content provided#{RESET}"
    return false
  end

  if env_content.bytesize > MAX_ENV_CONTENT_SIZE
    warn "#{RED}Error: Environment content too large (max #{MAX_ENV_CONTENT_SIZE} bytes)#{RESET}"
    return false
  end

  result = api_request_text("/services/#{service_id}/env", method: 'PUT', body: env_content, keys: keys)

  if result['error']
    warn "#{RED}Error: #{result['error']}#{RESET}"
    return false
  end

  count = result['count'] || 0
  plural = count == 1 ? '' : 's'
  puts "#{GREEN}Environment vault updated: #{count} variable#{plural}#{RESET}"
  puts result['message'] if result['message']
  true
end

def service_env_export(service_id, keys)
  result = api_request("/services/#{service_id}/env/export", method: 'POST', data: {}, keys: keys)
  env_content = result['env']
  if env_content && !env_content.empty?
    print env_content
    puts unless env_content.end_with?("\n")
  end
end

def service_env_delete(service_id, keys)
  api_request("/services/#{service_id}/env", method: 'DELETE', keys: keys)
  puts "#{GREEN}Environment vault deleted#{RESET}"
end

def read_env_file(filepath)
  File.read(filepath)
rescue => e
  warn "#{RED}Error: Env file not found: #{filepath}#{RESET}"
  exit 1
end

def build_env_content(envs, env_file)
  parts = []

  # Read from env file first
  parts << read_env_file(env_file) if env_file && !env_file.empty?

  # Add -e flags
  envs.each do |e|
    parts << e if e.include?('=')
  end

  parts.join("\n")
end

def cmd_service_env(action, target, envs, env_file, keys)
  if action.nil? || action.empty?
    warn "#{RED}Error: env action required (status, set, export, delete)#{RESET}"
    exit 1
  end

  if target.nil? || target.empty?
    warn "#{RED}Error: Service ID required for env command#{RESET}"
    exit 1
  end

  case action
  when 'status'
    service_env_status(target, keys)
  when 'set'
    env_content = build_env_content(envs, env_file)
    if env_content.empty?
      warn "#{RED}Error: No env content provided. Use -e KEY=VAL or --env-file#{RESET}"
      exit 1
    end
    service_env_set(target, env_content, keys)
  when 'export'
    service_env_export(target, keys)
  when 'delete'
    service_env_delete(target, keys)
  else
    warn "#{RED}Error: Unknown env action '#{action}'. Use: status, set, export, delete#{RESET}"
    exit 1
  end
end

def cmd_execute(options)
  keys = get_api_keys(options[:api_key])

  # Check for inline mode: -s/--shell specified, or source_file doesn't exist
  if options[:exec_shell]
    # Inline mode with specified language
    code = options[:source_file]
    language = options[:exec_shell]
  elsif !File.exist?(options[:source_file])
    # File doesn't exist - treat as inline bash code
    code = options[:source_file]
    language = "bash"
  else
    # Normal file execution
    code = File.read(options[:source_file])
    language = detect_language(options[:source_file])
  end

  payload = { language: language, code: code }

  if options[:env] && !options[:env].empty?
    env_vars = {}
    options[:env].each do |e|
      k, v = e.split('=', 2)
      env_vars[k] = v if k && v
    end
    payload[:env] = env_vars unless env_vars.empty?
  end

  if options[:files] && !options[:files].empty?
    input_files = options[:files].map do |filepath|
      unless File.exist?(filepath)
        warn "#{RED}Error: Input file not found: #{filepath}#{RESET}"
        exit 1
      end
      {
        filename: File.basename(filepath),
        content_base64: Base64.strict_encode64(File.read(filepath, mode: 'rb'))
      }
    end
    payload[:input_files] = input_files
  end

  payload[:return_artifacts] = true if options[:artifacts]
  payload[:network] = options[:network] if options[:network]
  payload[:vcpu] = options[:vcpu] if options[:vcpu]

  result = api_request('/execute', method: 'POST', data: payload, keys: keys)

  print "#{BLUE}#{result['stdout']}#{RESET}" if result['stdout']
  $stderr.print "#{RED}#{result['stderr']}#{RESET}" if result['stderr']

  if options[:artifacts] && result['artifacts']
    out_dir = options[:output_dir] || '.'
    FileUtils.mkdir_p(out_dir) unless Dir.exist?(out_dir)
    result['artifacts'].each do |artifact|
      filename = artifact['filename'] || 'artifact'
      content = Base64.strict_decode64(artifact['content_base64'])
      filepath = File.join(out_dir, filename)
      File.write(filepath, content, mode: 'wb')
      File.chmod(0755, filepath)
      warn "#{GREEN}Saved: #{filepath}#{RESET}"
    end
  end

  exit(result['exit_code'] || 0)
end

def cmd_session(options)
  keys = get_api_keys(options[:api_key])

  if options[:list]
    result = api_request('/sessions', keys: keys)
    sessions = result['sessions'] || []
    if sessions.empty?
      puts 'No active sessions'
    else
      puts format('%-40s %-10s %-10s %s', 'ID', 'Shell', 'Status', 'Created')
      sessions.each do |s|
        puts format('%-40s %-10s %-10s %s',
                    s['id'] || 'N/A', s['shell'] || 'N/A',
                    s['status'] || 'N/A', s['created_at'] || 'N/A')
      end
    end
    return
  end

  if options[:kill]
    api_request("/sessions/#{options[:kill]}", method: 'DELETE', keys: keys)
    puts "#{GREEN}Session terminated: #{options[:kill]}#{RESET}"
    return
  end

  if options[:snapshot_session]
    payload = {}
    payload[:name] = options[:snapshot_name] if options[:snapshot_name]
    payload[:hot] = true if options[:hot]

    warn "#{YELLOW}Creating snapshot of session #{options[:snapshot_session]}...#{RESET}"
    result = api_request("/sessions/#{options[:snapshot_session]}/snapshot", method: 'POST', data: payload, keys: keys)
    puts "#{GREEN}Snapshot created successfully#{RESET}"
    puts "Snapshot ID: #{result['id'] || 'N/A'}"
    return
  end

  if options[:restore_session]
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    warn "#{YELLOW}Restoring from snapshot #{options[:restore_session]}...#{RESET}"
    result = api_request("/snapshots/#{options[:restore_session]}/restore", method: 'POST', keys: keys)
    puts "#{GREEN}Session restored from snapshot#{RESET}"
    puts "New session ID: #{result['session_id']}" if result['session_id']
    return
  end

  if options[:attach]
    puts "#{YELLOW}Attaching to session #{options[:attach]}...#{RESET}"
    puts "#{YELLOW}(Interactive sessions require WebSocket - use un2 for full support)#{RESET}"
    return
  end

  payload = { shell: options[:shell] || 'bash' }
  payload[:network] = options[:network] if options[:network]
  payload[:vcpu] = options[:vcpu] if options[:vcpu]
  payload[:persistence] = 'tmux' if options[:tmux]
  payload[:persistence] = 'screen' if options[:screen]
  payload[:audit] = true if options[:audit]

  # Add input files
  if options[:files] && !options[:files].empty?
    input_files = options[:files].map do |filepath|
      unless File.exist?(filepath)
        warn "#{RED}Error: Input file not found: #{filepath}#{RESET}"
        exit 1
      end
      {
        filename: File.basename(filepath),
        content_base64: Base64.strict_encode64(File.read(filepath, mode: 'rb'))
      }
    end
    payload[:input_files] = input_files
  end

  puts "#{YELLOW}Creating session...#{RESET}"
  result = api_request('/sessions', method: 'POST', data: payload, keys: keys)
  puts "#{GREEN}Session created: #{result['id'] || 'N/A'}#{RESET}"
  puts "#{YELLOW}(Interactive sessions require WebSocket - use un2 for full support)#{RESET}"
end

def validate_key(keys)
  require 'openssl'

  uri = URI("#{PORTAL_BASE}/keys/validate")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.read_timeout = 30

  timestamp = Time.now.to_i.to_s
  body = ''
  message = "#{timestamp}:POST:#{uri.path}:#{body}"
  signature = OpenSSL::HMAC.hexdigest('SHA256', keys[:secret_key], message)

  request = Net::HTTP::Post.new(uri)
  request['Authorization'] = "Bearer #{keys[:public_key]}"
  request['X-Timestamp'] = timestamp
  request['X-Signature'] = signature
  request['Content-Type'] = 'application/json'

  response = http.request(request)

  begin
    result = JSON.parse(response.body)
  rescue JSON::ParserError => e
    warn "#{RED}Error: Failed to parse response: #{e.message}#{RESET}"
    exit 1
  end

  if response.is_a?(Net::HTTPSuccess) && result['valid']
    puts "#{GREEN}Valid#{RESET}"
    puts "Public Key: #{result['public_key']}"
    puts "Tier: #{result['tier']}"
    puts "Status: #{result['status']}"
    puts "Expires: #{result['expires_at']}"
    puts "Time Remaining: #{result['time_remaining']}"
    puts "Rate Limit: #{result['rate_limit']}"
    puts "Burst: #{result['burst']}"
    puts "Concurrency: #{result['concurrency']}"
    result
  elsif result['expired']
    puts "#{RED}Expired#{RESET}"
    puts "Public Key: #{result['public_key']}"
    puts "Tier: #{result['tier']}"
    puts "Expired: #{result['expires_at']}"
    puts "#{YELLOW}To renew: Visit https://unsandbox.com/keys/extend#{RESET}"
    result
  else
    puts "#{RED}Invalid#{RESET}"
    puts "Error: #{result['error'] || result['reason'] || 'Unknown error'}"
    exit 1
  end
rescue => e
  warn "#{RED}Error: #{e.message}#{RESET}"
  exit 1
end

def open_browser(url)
  case RbConfig::CONFIG['host_os']
  when /mswin|mingw|cygwin/
    system("start #{url}")
  when /darwin/
    system("open #{url}")
  when /linux|bsd/
    system("xdg-open #{url}")
  else
    puts "#{YELLOW}Please open this URL in your browser:#{RESET}"
    puts url
  end
end

def cmd_key(options)
  keys = get_api_keys(options[:api_key])

  if options[:extend]
    result = validate_key(keys)
    public_key = result['public_key']
    if public_key
      url = "#{PORTAL_BASE}/keys/extend?pk=#{public_key}"
      puts "#{GREEN}Opening browser to extend key...#{RESET}"
      open_browser(url)
    else
      warn "#{RED}Error: Could not retrieve public key#{RESET}"
      exit 1
    end
  else
    validate_key(keys)
  end
end

def cmd_snapshot(options)
  keys = get_api_keys(options[:api_key])

  if options[:list]
    result = api_request('/snapshots', keys: keys)
    snapshots = result['snapshots'] || []
    if snapshots.empty?
      puts 'No snapshots found'
    else
      puts format('%-40s %-20s %-12s %-30s %s', 'ID', 'Name', 'Type', 'Source ID', 'Size')
      snapshots.each do |s|
        puts format('%-40s %-20s %-12s %-30s %s',
                    s['id'] || 'N/A', s['name'] || '-',
                    s['source_type'] || 'N/A', s['source_id'] || 'N/A',
                    s['size'] || 'N/A')
      end
    end
    return
  end

  if options[:info_snapshot]
    result = api_request("/snapshots/#{options[:info_snapshot]}", keys: keys)
    puts "#{BLUE}Snapshot Details#{RESET}\n"
    puts "Snapshot ID: #{result['id'] || 'N/A'}"
    puts "Name: #{result['name'] || '-'}"
    puts "Source Type: #{result['source_type'] || 'N/A'}"
    puts "Source ID: #{result['source_id'] || 'N/A'}"
    puts "Size: #{result['size'] || 'N/A'}"
    puts "Created: #{result['created_at'] || 'N/A'}"
    return
  end

  if options[:delete_snapshot]
    api_request("/snapshots/#{options[:delete_snapshot]}", method: 'DELETE', keys: keys)
    puts "#{GREEN}Snapshot deleted successfully#{RESET}"
    return
  end

  if options[:clone_snapshot]
    unless options[:clone_type]
      warn "#{RED}Error: --type required for --clone (session or service)#{RESET}"
      exit 1
    end
    unless ['session', 'service'].include?(options[:clone_type])
      warn "#{RED}Error: --type must be 'session' or 'service'#{RESET}"
      exit 1
    end

    payload = { type: options[:clone_type] }
    payload[:name] = options[:clone_name] if options[:clone_name]
    payload[:shell] = options[:clone_shell] if options[:clone_shell]
    payload[:ports] = options[:clone_ports].split(',').map(&:to_i) if options[:clone_ports]

    result = api_request("/snapshots/#{options[:clone_snapshot]}/clone", method: 'POST', data: payload, keys: keys)

    if options[:clone_type] == 'session'
      puts "#{GREEN}Session created from snapshot#{RESET}"
      puts "Session ID: #{result['id'] || 'N/A'}"
    else
      puts "#{GREEN}Service created from snapshot#{RESET}"
      puts "Service ID: #{result['id'] || 'N/A'}"
    end
    return
  end

  warn "#{RED}Error: Specify --list, --info ID, --delete ID, or --clone ID --type TYPE#{RESET}"
  exit 1
end

def cmd_service(options)
  keys = get_api_keys(options[:api_key])

  if options[:list]
    result = api_request('/services', keys: keys)
    services = result['services'] || []
    if services.empty?
      puts 'No services'
    else
      puts format('%-20s %-15s %-10s %-15s %s', 'ID', 'Name', 'Status', 'Ports', 'Domains')
      services.each do |s|
        ports = (s['ports'] || []).join(',')
        domains = (s['domains'] || []).join(',')
        puts format('%-20s %-15s %-10s %-15s %s',
                    s['id'] || 'N/A', s['name'] || 'N/A',
                    s['status'] || 'N/A', ports, domains)
      end
    end
    return
  end

  if options[:info]
    result = api_request("/services/#{options[:info]}", keys: keys)
    puts JSON.pretty_generate(result)
    return
  end

  if options[:logs]
    result = api_request("/services/#{options[:logs]}/logs", keys: keys)
    puts result['logs'] || ''
    return
  end

  if options[:tail]
    result = api_request("/services/#{options[:tail]}/logs?lines=9000", keys: keys)
    puts result['logs'] || ''
    return
  end

  if options[:sleep]
    api_request("/services/#{options[:sleep]}/freeze", method: 'POST', keys: keys)
    puts "#{GREEN}Service frozen: #{options[:sleep]}#{RESET}"
    return
  end

  if options[:wake]
    api_request("/services/#{options[:wake]}/unfreeze", method: 'POST', keys: keys)
    puts "#{GREEN}Service unfreezing: #{options[:wake]}#{RESET}"
    return
  end

  if options[:destroy]
    api_request("/services/#{options[:destroy]}", method: 'DELETE', keys: keys)
    puts "#{GREEN}Service destroyed: #{options[:destroy]}#{RESET}"
    return
  end

  if options[:resize]
    unless options[:vcpu]
      warn "#{RED}Error: --vcpu is required with --resize#{RESET}"
      exit 1
    end
    payload = { vcpu: options[:vcpu] }
    api_request("/services/#{options[:resize]}", method: 'PATCH', data: payload, keys: keys)
    ram = options[:vcpu] * 2
    puts "#{GREEN}Service resized to #{options[:vcpu]} vCPU, #{ram} GB RAM#{RESET}"
    return
  end

  if options[:snapshot_service]
    payload = {}
    payload[:name] = options[:snapshot_name] if options[:snapshot_name]
    payload[:hot] = true if options[:hot]

    warn "#{YELLOW}Creating snapshot of service #{options[:snapshot_service]}...#{RESET}"
    result = api_request("/services/#{options[:snapshot_service]}/snapshot", method: 'POST', data: payload, keys: keys)
    puts "#{GREEN}Snapshot created successfully#{RESET}"
    puts "Snapshot ID: #{result['id'] || 'N/A'}"
    return
  end

  if options[:restore_service]
    # --restore takes snapshot ID directly, calls /snapshots/:id/restore
    warn "#{YELLOW}Restoring from snapshot #{options[:restore_service]}...#{RESET}"
    result = api_request("/snapshots/#{options[:restore_service]}/restore", method: 'POST', keys: keys)
    puts "#{GREEN}Service restored from snapshot#{RESET}"
    puts "New service ID: #{result['service_id']}" if result['service_id']
    return
  end

  if options[:execute]
    payload = { command: options[:command] }
    result = api_request("/services/#{options[:execute]}/execute", method: 'POST', data: payload, keys: keys)
    print "#{BLUE}#{result['stdout']}#{RESET}" if result['stdout']
    $stderr.print "#{RED}#{result['stderr']}#{RESET}" if result['stderr']
    return
  end

  if options[:dump_bootstrap]
    warn "Fetching bootstrap script from #{options[:dump_bootstrap]}..."
    payload = { command: 'cat /tmp/bootstrap.sh' }
    result = api_request("/services/#{options[:dump_bootstrap]}/execute", method: 'POST', data: payload, keys: keys)

    if result['stdout']
      bootstrap = result['stdout']
      if options[:dump_file]
        # Write to file
        begin
          File.write(options[:dump_file], bootstrap)
          File.chmod(0755, options[:dump_file])
          puts "Bootstrap saved to #{options[:dump_file]}"
        rescue => e
          warn "#{RED}Error: Could not write to #{options[:dump_file]}: #{e.message}#{RESET}"
          exit 1
        end
      else
        # Print to stdout
        print bootstrap
      end
    else
      warn "#{RED}Error: Failed to fetch bootstrap (service not running or no bootstrap file)#{RESET}"
      exit 1
    end
    return
  end

  if options[:name]
    payload = { name: options[:name] }
    payload[:ports] = options[:ports].split(',').map(&:to_i) if options[:ports]
    payload[:domains] = options[:domains].split(',') if options[:domains]
    payload[:service_type] = options[:type] if options[:type]
    payload[:bootstrap] = options[:bootstrap] if options[:bootstrap]
    if options[:bootstrap_file]
      unless File.exist?(options[:bootstrap_file])
        warn "#{RED}Error: Bootstrap file not found: #{options[:bootstrap_file]}#{RESET}"
        exit 1
      end
      payload[:bootstrap_content] = File.read(options[:bootstrap_file])
    end
    # Add input files
    if options[:files] && !options[:files].empty?
      input_files = options[:files].map do |filepath|
        unless File.exist?(filepath)
          warn "#{RED}Error: Input file not found: #{filepath}#{RESET}"
          exit 1
        end
        {
          filename: File.basename(filepath),
          content_base64: Base64.strict_encode64(File.read(filepath, mode: 'rb'))
        }
      end
      payload[:input_files] = input_files
    end
    payload[:network] = options[:network] if options[:network]
    payload[:vcpu] = options[:vcpu] if options[:vcpu]

    result = api_request('/services', method: 'POST', data: payload, keys: keys)
    service_id = result['id']
    puts "#{GREEN}Service created: #{service_id || 'N/A'}#{RESET}"
    puts "Name: #{result['name'] || 'N/A'}"
    puts "URL: #{result['url']}" if result['url']

    # Auto-set vault if -e or --env-file provided
    env_content = build_env_content(options[:env] || [], options[:env_file])
    if !env_content.empty? && service_id
      service_env_set(service_id, env_content, keys)
    end
    return
  end

  warn "#{RED}Error: Specify --name to create a service, or use --list, --info, etc.#{RESET}"
  exit 1
end

def main
  options = {
    command: nil,
    source_file: nil,
    env: [],
    files: [],
    artifacts: false,
    output_dir: nil,
    network: nil,
    vcpu: nil,
    api_key: nil,
    shell: nil,
    list: false,
    attach: nil,
    kill: nil,
    snapshot_session: nil,
    snapshot_service: nil,
    restore_session: nil,
    restore_service: nil,
    from_snapshot: nil,
    snapshot_name: nil,
    hot: false,
    info_snapshot: nil,
    delete_snapshot: nil,
    clone_snapshot: nil,
    clone_type: nil,
    clone_name: nil,
    clone_shell: nil,
    clone_ports: nil,
    audit: false,
    tmux: false,
    screen: false,
    name: nil,
    ports: nil,
    domains: nil,
    type: nil,
    bootstrap: nil,
    info: nil,
    logs: nil,
    tail: nil,
    sleep: nil,
    wake: nil,
    destroy: nil,
    resize: nil,
    execute: nil,
    dump_bootstrap: nil,
    dump_file: nil,
    extend: false,
    bootstrap_file: nil,
    exec_shell: nil,
    env_file: nil,
    env_action: nil,
    env_target: nil
  }

  # Manual argument parsing
  i = 0
  while i < ARGV.length
    arg = ARGV[i]

    case arg
    when 'session', 'service', 'key', 'snapshot'
      options[:command] = arg
    when '-e'
      i += 1
      options[:env] << ARGV[i]
    when '-f'
      i += 1
      options[:files] << ARGV[i]
    when '-a'
      options[:artifacts] = true
    when '-o'
      i += 1
      options[:output_dir] = ARGV[i]
    when '-n'
      i += 1
      options[:network] = ARGV[i]
    when '-v'
      i += 1
      options[:vcpu] = ARGV[i].to_i
    when '-k'
      i += 1
      options[:api_key] = ARGV[i]
    when '-s', '--shell'
      i += 1
      # For session command, this is shell type. For execute, it's inline exec language.
      if options[:command] == 'session'
        options[:shell] = ARGV[i]
      else
        options[:exec_shell] = ARGV[i]
      end
    when '-l', '--list'
      options[:list] = true
    when '--attach'
      i += 1
      options[:attach] = ARGV[i]
    when '--kill'
      i += 1
      options[:kill] = ARGV[i]
    when '--audit'
      options[:audit] = true
    when '--tmux'
      options[:tmux] = true
    when '--screen'
      options[:screen] = true
    when '--name'
      i += 1
      options[:name] = ARGV[i]
    when '--ports'
      i += 1
      options[:ports] = ARGV[i]
    when '--domains'
      i += 1
      options[:domains] = ARGV[i]
    when '--type'
      i += 1
      options[:type] = ARGV[i]
    when '--bootstrap'
      i += 1
      options[:bootstrap] = ARGV[i]
    when '--bootstrap-file'
      i += 1
      options[:bootstrap_file] = ARGV[i]
    when '--env-file'
      i += 1
      options[:env_file] = ARGV[i]
    when 'env'
      # Handle "service env <action> <target>" subcommand
      if options[:command] == 'service'
        i += 1
        options[:env_action] = ARGV[i] if i < ARGV.length
        i += 1
        if i < ARGV.length && !ARGV[i].start_with?('-')
          options[:env_target] = ARGV[i]
        else
          i -= 1 # back up if next arg is a flag
        end
      end
    when '--info'
      i += 1
      options[:info] = ARGV[i]
    when '--logs'
      i += 1
      options[:logs] = ARGV[i]
    when '--tail'
      i += 1
      options[:tail] = ARGV[i]
    when '--freeze'
      i += 1
      options[:sleep] = ARGV[i]
    when '--unfreeze'
      i += 1
      options[:wake] = ARGV[i]
    when '--destroy'
      i += 1
      options[:destroy] = ARGV[i]
    when '--resize'
      i += 1
      options[:resize] = ARGV[i]
    when '--execute'
      i += 1
      options[:execute] = ARGV[i]
    when '--command'
      i += 1
      options[:command] = ARGV[i]
    when '--dump-bootstrap'
      i += 1
      options[:dump_bootstrap] = ARGV[i]
    when '--dump-file'
      i += 1
      options[:dump_file] = ARGV[i]
    when '--snapshot'
      i += 1
      if options[:command] == 'session'
        options[:snapshot_session] = ARGV[i]
      elsif options[:command] == 'service'
        options[:snapshot_service] = ARGV[i]
      end
    when '--restore'
      i += 1
      if options[:command] == 'session'
        options[:restore_session] = ARGV[i]
      elsif options[:command] == 'service'
        options[:restore_service] = ARGV[i]
      end
    when '--from'
      i += 1
      options[:from_snapshot] = ARGV[i]
    when '--snapshot-name'
      i += 1
      options[:snapshot_name] = ARGV[i]
    when '--hot'
      options[:hot] = true
    when '--info'
      i += 1
      if options[:command] == 'snapshot'
        options[:info_snapshot] = ARGV[i]
      else
        options[:info] = ARGV[i]
      end
    when '--delete'
      i += 1
      options[:delete_snapshot] = ARGV[i]
    when '--clone'
      i += 1
      options[:clone_snapshot] = ARGV[i]
    when '--type'
      i += 1
      if options[:clone_snapshot]
        options[:clone_type] = ARGV[i]
      else
        options[:type] = ARGV[i]
      end
    when '--shell'
      i += 1
      if options[:clone_snapshot]
        options[:clone_shell] = ARGV[i]
      else
        options[:shell] = ARGV[i]
      end
    when '--extend'
      options[:extend] = true
    else
      if arg.start_with?('-')
        warn "#{RED}Unknown option: #{arg}#{RESET}"
        exit 1
      else
        options[:source_file] = arg
      end
    end

    i += 1
  end

  case options[:command]
  when 'session'
    cmd_session(options)
  when 'service'
    # Check for "service env" subcommand
    if options[:env_action]
      keys = get_api_keys(options[:api_key])
      cmd_service_env(options[:env_action], options[:env_target], options[:env], options[:env_file], keys)
    else
      cmd_service(options)
    end
  when 'snapshot'
    cmd_snapshot(options)
  when 'key'
    cmd_key(options)
  else
    if options[:source_file]
      cmd_execute(options)
    else
      puts <<~HELP
        Unsandbox CLI - Execute code in secure sandboxes

        Usage:
          #{$PROGRAM_NAME} [options] <source_file>
          #{$PROGRAM_NAME} session [options]
          #{$PROGRAM_NAME} service [options]
          #{$PROGRAM_NAME} key [options]

        Execute options:
          -e KEY=VALUE      Environment variable (multiple allowed)
          -f FILE          Input file (multiple allowed)
          -a               Return artifacts
          -o DIR           Output directory for artifacts
          -n MODE          Network mode (zerotrust|semitrusted)
          -v N             vCPU count (1-8)
          -k KEY           API key

        Session options:
          -s, --shell NAME  Shell/REPL (default: bash)
          -l, --list       List sessions
          --attach ID      Attach to session
          --kill ID        Terminate session
          --audit          Record session
          --tmux           Enable tmux persistence
          --screen         Enable screen persistence

        Service options:
          --name NAME      Service name
          --ports PORTS    Comma-separated ports
          --domains DOMAINS Custom domains
          --type TYPE      Service type (minecraft|mumble|teamspeak|source|tcp|udp)
          --bootstrap CMD  Bootstrap command or URI
          --bootstrap-file FILE  Upload local file as bootstrap script
          -l, --list       List services
          --info ID        Get service details
          --logs ID        Get all logs
          --tail ID        Get last 9000 lines
          --freeze ID       Freeze service
          --unfreeze ID        Unfreeze service
          --destroy ID     Destroy service
          --resize ID      Resize service (requires -v)
          --execute ID     Execute command in service
          --command CMD    Command to execute (with --execute)
          --dump-bootstrap ID  Dump bootstrap script
          --dump-file FILE     File to save bootstrap (with --dump-bootstrap)

        Key options:
          -k KEY           API key (or use UNSANDBOX_API_KEY env var)
          --extend         Validate key and open browser to extend
      HELP
      exit 1
    end
  end
end

main if __FILE__ == $PROGRAM_NAME
