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

def get_api_key(args_key = nil)
  key = args_key || ENV['UNSANDBOX_API_KEY']
  unless key
    warn "#{RED}Error: UNSANDBOX_API_KEY not set#{RESET}"
    exit 1
  end
  key
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

def api_request(endpoint, method: 'GET', data: nil, api_key:)
  uri = URI("#{API_BASE}#{endpoint}")
  http = Net::HTTP.new(uri.host, uri.port)
  http.use_ssl = true
  http.read_timeout = 300

  request = case method
            when 'GET' then Net::HTTP::Get.new(uri)
            when 'POST' then Net::HTTP::Post.new(uri)
            when 'DELETE' then Net::HTTP::Delete.new(uri)
            else raise "Unknown method: #{method}"
            end

  request['Authorization'] = "Bearer #{api_key}"
  request['Content-Type'] = 'application/json'
  request.body = JSON.generate(data) if data

  response = http.request(request)
  unless response.is_a?(Net::HTTPSuccess)
    warn "#{RED}Error: HTTP #{response.code} - #{response.body}#{RESET}"
    exit 1
  end

  JSON.parse(response.body)
rescue => e
  warn "#{RED}Error: #{e.message}#{RESET}"
  exit 1
end

def cmd_execute(options)
  api_key = get_api_key(options[:api_key])

  unless File.exist?(options[:source_file])
    warn "#{RED}Error: File not found: #{options[:source_file]}#{RESET}"
    exit 1
  end

  code = File.read(options[:source_file])
  language = detect_language(options[:source_file])

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

  result = api_request('/execute', method: 'POST', data: payload, api_key: api_key)

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
  api_key = get_api_key(options[:api_key])

  if options[:list]
    result = api_request('/sessions', api_key: api_key)
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
    api_request("/sessions/#{options[:kill]}", method: 'DELETE', api_key: api_key)
    puts "#{GREEN}Session terminated: #{options[:kill]}#{RESET}"
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

  puts "#{YELLOW}Creating session...#{RESET}"
  result = api_request('/sessions', method: 'POST', data: payload, api_key: api_key)
  puts "#{GREEN}Session created: #{result['id'] || 'N/A'}#{RESET}"
  puts "#{YELLOW}(Interactive sessions require WebSocket - use un2 for full support)#{RESET}"
end

def cmd_service(options)
  api_key = get_api_key(options[:api_key])

  if options[:list]
    result = api_request('/services', api_key: api_key)
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
    result = api_request("/services/#{options[:info]}", api_key: api_key)
    puts JSON.pretty_generate(result)
    return
  end

  if options[:logs]
    result = api_request("/services/#{options[:logs]}/logs", api_key: api_key)
    puts result['logs'] || ''
    return
  end

  if options[:tail]
    result = api_request("/services/#{options[:tail]}/logs?lines=9000", api_key: api_key)
    puts result['logs'] || ''
    return
  end

  if options[:sleep]
    api_request("/services/#{options[:sleep]}/sleep", method: 'POST', api_key: api_key)
    puts "#{GREEN}Service sleeping: #{options[:sleep]}#{RESET}"
    return
  end

  if options[:wake]
    api_request("/services/#{options[:wake]}/wake", method: 'POST', api_key: api_key)
    puts "#{GREEN}Service waking: #{options[:wake]}#{RESET}"
    return
  end

  if options[:destroy]
    api_request("/services/#{options[:destroy]}", method: 'DELETE', api_key: api_key)
    puts "#{GREEN}Service destroyed: #{options[:destroy]}#{RESET}"
    return
  end

  if options[:execute]
    payload = { command: options[:command] }
    result = api_request("/services/#{options[:execute]}/execute", method: 'POST', data: payload, api_key: api_key)
    print "#{BLUE}#{result['stdout']}#{RESET}" if result['stdout']
    $stderr.print "#{RED}#{result['stderr']}#{RESET}" if result['stderr']
    return
  end

  if options[:name]
    payload = { name: options[:name] }
    payload[:ports] = options[:ports].split(',').map(&:to_i) if options[:ports]
    payload[:domains] = options[:domains].split(',') if options[:domains]
    if options[:bootstrap]
      payload[:bootstrap] = if File.exist?(options[:bootstrap])
                              File.read(options[:bootstrap])
                            else
                              options[:bootstrap]
                            end
    end
    payload[:network] = options[:network] if options[:network]
    payload[:vcpu] = options[:vcpu] if options[:vcpu]

    result = api_request('/services', method: 'POST', data: payload, api_key: api_key)
    puts "#{GREEN}Service created: #{result['id'] || 'N/A'}#{RESET}"
    puts "Name: #{result['name'] || 'N/A'}"
    puts "URL: #{result['url']}" if result['url']
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
    audit: false,
    tmux: false,
    screen: false,
    name: nil,
    ports: nil,
    domains: nil,
    bootstrap: nil,
    info: nil,
    logs: nil,
    tail: nil,
    sleep: nil,
    wake: nil,
    destroy: nil,
    execute: nil,
    command: nil
  }

  # Manual argument parsing
  i = 0
  while i < ARGV.length
    arg = ARGV[i]

    case arg
    when 'session', 'service'
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
      options[:shell] = ARGV[i]
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
    when '--bootstrap'
      i += 1
      options[:bootstrap] = ARGV[i]
    when '--info'
      i += 1
      options[:info] = ARGV[i]
    when '--logs'
      i += 1
      options[:logs] = ARGV[i]
    when '--tail'
      i += 1
      options[:tail] = ARGV[i]
    when '--sleep'
      i += 1
      options[:sleep] = ARGV[i]
    when '--wake'
      i += 1
      options[:wake] = ARGV[i]
    when '--destroy'
      i += 1
      options[:destroy] = ARGV[i]
    when '--execute'
      i += 1
      options[:execute] = ARGV[i]
    when '--command'
      i += 1
      options[:command] = ARGV[i]
    else
      options[:source_file] = arg unless arg.start_with?('-')
    end

    i += 1
  end

  case options[:command]
  when 'session'
    cmd_session(options)
  when 'service'
    cmd_service(options)
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
          --bootstrap CMD  Bootstrap command/file
          -l, --list       List services
          --info ID        Get service details
          --logs ID        Get all logs
          --tail ID        Get last 9000 lines
          --sleep ID       Freeze service
          --wake ID        Unfreeze service
          --destroy ID     Destroy service
          --execute ID     Execute command in service
          --command CMD    Command to execute (with --execute)
      HELP
      exit 1
    end
  end
end

main if __FILE__ == $PROGRAM_NAME
