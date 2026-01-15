#!/usr/bin/env ruby
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
#
# unsandbox SDK for Ruby - Execute code in secure sandboxes
# https://unsandbox.com | https://api.unsandbox.com/openapi
#
# Library Usage:
#   require_relative 'un.rb'
#   result = Un.execute("ruby", 'puts "Hello"')
#   job = Un.execute_async("ruby", code)
#   result = Un.wait(job["job_id"])
#
# CLI Usage:
#   ruby un.rb script.rb
#   ruby un.rb -s ruby 'puts "Hello"'
#
# Authentication (in priority order):
#   1. Function arguments: execute(..., public_key: "...", secret_key: "...")
#   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
#   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

require 'json'
require 'net/http'
require 'uri'
require 'base64'
require 'fileutils'
require 'optparse'
require 'openssl'
require 'time'

module Un
  VERSION = "2.0.0"

  API_BASE = 'https://api.unsandbox.com'
  PORTAL_BASE = 'https://unsandbox.com'

  # Exception classes
  class UnsandboxError < StandardError; end
  class AuthenticationError < UnsandboxError; end
  class ExecutionError < UnsandboxError; end
  class APIError < UnsandboxError; end
  class TimeoutError < UnsandboxError; end

  # ========================================================================
  # Credential System (4-tier)
  # ========================================================================

  def self._load_accounts_csv(path = nil)
    path = File.expand_path(path) if path
    path ||= File.expand_path('~/.unsandbox/accounts.csv')

    return [] unless File.exist?(path)

    accounts = []
    File.readlines(path).each do |line|
      line.strip!
      next if line.empty?
      pk, sk = line.split(',', 2)
      accounts << [pk.strip, sk.strip] if pk && sk
    end
    accounts
  end

  def self._get_credentials(public_key = nil, secret_key = nil)
    # Tier 1: Function arguments
    return [public_key, secret_key] if public_key && secret_key

    # Tier 2: Environment variables
    pk = ENV['UNSANDBOX_PUBLIC_KEY']
    sk = ENV['UNSANDBOX_SECRET_KEY']
    return [pk, sk] if pk && sk

    # Tier 3: Home directory
    accounts = _load_accounts_csv(File.expand_path('~/.unsandbox/accounts.csv'))
    return accounts[0] if accounts.any?

    # Tier 4: Local directory
    accounts = _load_accounts_csv('./accounts.csv')
    return accounts[0] if accounts.any?

    raise AuthenticationError, "No credentials found. Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY or create ~/.unsandbox/accounts.csv"
  end

  # ========================================================================
  # HMAC Signature
  # ========================================================================

  def self._sign_request(secret_key, timestamp, method, endpoint, body)
    message = "#{timestamp}:#{method}:#{endpoint}:#{body}"
    OpenSSL::HMAC.hexdigest('SHA256', secret_key, message)
  end

  # ========================================================================
  # API Communication
  # ========================================================================

  def self._api_request(method, endpoint, body = nil, public_key = nil, secret_key = nil)
    pk, sk = _get_credentials(public_key, secret_key)

    timestamp = Time.now.to_i.to_s
    url = "#{API_BASE}#{endpoint}"

    body_str = body ? JSON.generate(body) : '{}'
    signature = _sign_request(sk, timestamp, method, endpoint, body_str)

    uri = URI(url)
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    case method
    when 'GET'
      req = Net::HTTP::Get.new(uri)
    when 'POST'
      req = Net::HTTP::Post.new(uri)
    when 'DELETE'
      req = Net::HTTP::Delete.new(uri)
    else
      raise ArgumentError, "Unsupported method: #{method}"
    end

    req['Authorization'] = "Bearer #{pk}"
    req['X-Timestamp'] = timestamp
    req['X-Signature'] = signature
    req['Content-Type'] = 'application/json'

    req.body = body_str if body && method != 'GET'

    response = http.request(req)

    unless response.code.to_i == 200
      raise APIError, "API error (#{response.code}): #{response.body[0..100]}"
    end

    JSON.parse(response.body)
  rescue JSON::ParserError => e
    raise APIError, "Invalid API response: #{e.message}"
  end

  # ========================================================================
  # Languages Cache (1-hour TTL)
  # ========================================================================

  def self.languages(cache_ttl = 3600)
    cache_path = File.expand_path('~/.unsandbox/languages.json')

    # Check cache
    if File.exist?(cache_path)
      age = Time.now.to_i - File.stat(cache_path).mtime.to_i
      return JSON.parse(File.read(cache_path)) if age < cache_ttl
    end

    # Fetch from API
    result = _api_request('GET', '/languages')
    langs = result['languages'] || []

    # Update cache
    FileUtils.mkdir_p(File.dirname(cache_path))
    File.write(cache_path, JSON.generate(langs))

    langs
  end

  # ========================================================================
  # Core Execution Functions
  # ========================================================================

  def self.execute(language, code, opts = {})
    body = {
      'language' => language,
      'code' => code,
      'network_mode' => opts[:network_mode] || 'zerotrust',
      'ttl' => opts[:ttl] || 60
    }
    body['env'] = opts[:env] if opts[:env]

    _api_request('POST', '/execute', body, opts[:public_key], opts[:secret_key])
  end

  def self.execute_async(language, code, opts = {})
    body = {
      'language' => language,
      'code' => code,
      'network_mode' => opts[:network_mode] || 'zerotrust',
      'ttl' => opts[:ttl] || 300
    }
    body['env'] = opts[:env] if opts[:env]

    _api_request('POST', '/execute/async', body, opts[:public_key], opts[:secret_key])
  end

  def self.run(file_path, opts = {})
    code = File.read(file_path)
    execute(detect_language(file_path), code, opts)
  end

  def self.run_async(file_path, opts = {})
    code = File.read(file_path)
    execute_async(detect_language(file_path), code, opts)
  end

  # ========================================================================
  # Job Management
  # ========================================================================

  def self.get_job(job_id, opts = {})
    _api_request('GET', "/jobs/#{job_id}", nil, opts[:public_key], opts[:secret_key])
  end

  def self.wait(job_id, timeout = 3600, opts = {})
    start_time = Time.now
    delays = [300, 450, 700, 900, 650, 1600, 2000]
    max_polls = 120

    max_polls.times do |i|
      job = get_job(job_id, opts)
      status = job['status']

      if status == 'completed'
        return job
      elsif status == 'failed'
        raise ExecutionError, "Job failed: #{job['error']}"
      elsif status == 'cancelled'
        raise ExecutionError, "Job was cancelled"
      elsif status == 'timeout'
        raise TimeoutError, "Job timed out"
      end

      if Time.now - start_time > timeout
        raise TimeoutError, "Polling timeout after #{timeout}s"
      end

      delay_ms = delays[i] || 2000
      sleep(delay_ms / 1000.0)
    end

    raise TimeoutError, "Max polls exceeded for job #{job_id}"
  end

  def self.cancel_job(job_id, opts = {})
    _api_request('DELETE', "/jobs/#{job_id}", nil, opts[:public_key], opts[:secret_key])
  end

  def self.list_jobs(opts = {})
    result = _api_request('GET', '/jobs', nil, opts[:public_key], opts[:secret_key])
    result['jobs'] || []
  end

  # ========================================================================
  # Utilities
  # ========================================================================

  EXT_MAP = {
    'py' => 'python', 'rb' => 'ruby', 'js' => 'javascript', 'ts' => 'typescript',
    'go' => 'go', 'rs' => 'rust', 'java' => 'java', 'cs' => 'csharp',
    'cpp' => 'cpp', 'c' => 'c', 'h' => 'c', 'sh' => 'bash', 'pl' => 'perl',
    'php' => 'php', 'lua' => 'lua', 'rb' => 'ruby', 'jl' => 'julia',
    'r' => 'r', 'scala' => 'scala', 'kt' => 'kotlin', 'swift' => 'swift',
    'cr' => 'crystal', 'zig' => 'zig', 'nim' => 'nim', 'd' => 'd'
  }

  def self.detect_language(filename)
    ext = File.extname(filename).sub(/^\./, '')
    EXT_MAP[ext] || raise(ArgumentError, "Unknown file type: #{filename}")
  end

  def self.image(code, format = 'png', opts = {})
    body = { 'code' => code, 'format' => format }
    _api_request('POST', '/image', body, opts[:public_key], opts[:secret_key])
  end

  # ========================================================================
  # CLI
  # ========================================================================

  def self.cli_main
    case ARGV[0]
    when 'session'
      puts "session not yet supported"
      exit 1
    when 'service'
      puts "service not yet supported"
      exit 1
    else
      # Execute code file
      if ARGV.empty?
        puts "Usage: ruby un.rb <file> | ruby un.rb -s <language> '<code>'"
        exit 1
      end

      if ARGV[0] == '-s'
        language = ARGV[1]
        code = ARGV[2]
        result = execute(language, code)
      else
        file = ARGV[0]
        result = run(file)
      end

      puts result['stdout'] if result['stdout']
      STDERR.puts result['stderr'] if result['stderr']
      exit(result['exit_code'] || 0)
    end
  end
end

# Run CLI if called directly
if __FILE__ == $0
  begin
    Un.cli_main
  rescue Un::UnsandboxError => e
    STDERR.puts "Error: #{e.message}"
    exit 1
  rescue StandardError => e
    STDERR.puts "Error: #{e.message}"
    exit 1
  end
end
