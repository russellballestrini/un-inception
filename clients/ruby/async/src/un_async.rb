# frozen_string_literal: true

# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# unsandbox.com Ruby SDK (Asynchronous)
#
# Library Usage:
#     require_relative 'un_async'
#
#     # Execute code asynchronously with concurrent gem
#     result = UnAsync.execute_code("python", 'print("hello")').value
#
#     # Execute and get future for job_id
#     job_future = UnAsync.execute_async("javascript", 'console.log("hello")')
#     job_id = job_future.value
#
#     # Wait for job completion with exponential backoff
#     result = UnAsync.wait_for_job(job_id).value
#
#     # List all jobs
#     jobs = UnAsync.list_jobs.value
#
#     # Get supported languages (cached for 1 hour)
#     languages = UnAsync.get_languages.value
#
#     # Snapshot operations
#     snapshot_future = UnAsync.session_snapshot(session_id)
#     snapshot_id = snapshot_future.value
#
# Authentication Priority (4-tier):
#     1. Method arguments (public_key:, secret_key:)
#     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
#     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
#     4. Local directory (./accounts.csv, line 0 by default)
#
# Request Authentication (HMAC-SHA256):
#     Authorization: Bearer <public_key>
#     X-Timestamp: <unix_seconds>
#     X-Signature: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
#
# Languages Cache:
#     - Cached in ~/.unsandbox/languages.json
#     - TTL: 1 hour
#     - Updated on successful API calls
#
# Dependencies:
#     This async implementation uses Ruby's built-in Thread class for concurrency.
#     For production use, consider using the 'concurrent-ruby' gem for better
#     thread pool management and future/promise patterns.

require 'net/http'
require 'uri'
require 'json'
require 'openssl'
require 'fileutils'
require 'thread'

# Unsandbox Ruby SDK module (asynchronous)
# Returns Future objects that can be awaited with .value
module UnAsync
  # API base URL
  API_BASE = 'https://api.unsandbox.com'

  # Polling delays in milliseconds for exponential backoff
  POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000].freeze

  # Languages cache TTL in seconds (1 hour)
  LANGUAGES_CACHE_TTL = 3600

  # HTTP request timeout in seconds
  REQUEST_TIMEOUT = 120

  # Error raised when credentials cannot be found or are invalid
  class CredentialsError < StandardError; end

  # Error raised for API request failures
  class APIError < StandardError
    attr_reader :status_code, :response_body

    # @param message [String] Error message
    # @param status_code [Integer, nil] HTTP status code
    # @param response_body [String, nil] Response body
    def initialize(message, status_code: nil, response_body: nil)
      super(message)
      @status_code = status_code
      @response_body = response_body
    end
  end

  # Simple Future implementation for async operations
  # Wraps a thread and provides value/wait semantics
  class Future
    # @param block [Proc] Block to execute asynchronously
    def initialize(&block)
      @mutex = Mutex.new
      @condition = ConditionVariable.new
      @completed = false
      @value = nil
      @error = nil

      @thread = Thread.new do
        begin
          result = block.call
          @mutex.synchronize do
            @value = result
            @completed = true
            @condition.broadcast
          end
        rescue StandardError => e
          @mutex.synchronize do
            @error = e
            @completed = true
            @condition.broadcast
          end
        end
      end
    end

    # Wait for and return the result
    # @param timeout [Numeric, nil] Maximum time to wait in seconds
    # @return [Object] The result of the async operation
    # @raise [StandardError] If the async operation raised an error
    # @raise [Timeout::Error] If timeout is reached
    def value(timeout: nil)
      @mutex.synchronize do
        unless @completed
          if timeout
            deadline = Time.now + timeout
            until @completed
              remaining = deadline - Time.now
              raise Timeout::Error, 'Future timed out' if remaining <= 0

              @condition.wait(@mutex, remaining)
            end
          else
            @condition.wait(@mutex) until @completed
          end
        end

        raise @error if @error

        @value
      end
    end

    # Alias for value for compatibility
    alias wait value

    # Check if the future has completed
    # @return [Boolean] True if completed (success or error)
    def completed?
      @mutex.synchronize { @completed }
    end

    # Check if the future completed successfully
    # @return [Boolean] True if completed without error
    def success?
      @mutex.synchronize { @completed && @error.nil? }
    end

    # Check if the future completed with an error
    # @return [Boolean] True if completed with error
    def failed?
      @mutex.synchronize { @completed && !@error.nil? }
    end

    # Get the error if any
    # @return [StandardError, nil] The error or nil
    def error
      @mutex.synchronize { @error }
    end

    # Chain another operation to run after this one completes
    # @param block [Proc] Block that receives the result
    # @return [Future] New future for the chained operation
    def then(&block)
      Future.new { block.call(value) }
    end
  end

  class << self
    # Execute code asynchronously (returns Future, awaits until completion)
    #
    # @param language [String] Programming language (e.g., "python", "javascript", "go")
    # @param code [String] Source code to execute
    # @param public_key [String, nil] Optional API key (uses credentials resolution if not provided)
    # @param secret_key [String, nil] Optional API secret (uses credentials resolution if not provided)
    # @return [Future<Hash>] Future resolving to response hash containing stdout, stderr, exit code, etc.
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     future = UnAsync.execute_code("python", 'print("hello")')
    #     # Do other work...
    #     result = future.value
    #     puts result["stdout"]  # => "hello\n"
    def execute_code(language, code, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync(
          'POST',
          '/execute',
          pk,
          sk,
          { language: language, code: code }
        )

        # If we got a job_id, poll until completion
        job_id = response['job_id']
        status = response['status']

        if job_id && %w[pending running].include?(status)
          wait_for_job_sync(job_id, pk, sk)
        else
          response
        end
      end
    end

    # Execute code asynchronously (returns Future with job_id immediately)
    #
    # @param language [String] Programming language (e.g., "python", "javascript")
    # @param code [String] Source code to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<String>] Future resolving to job ID string
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     future = UnAsync.execute_async("python", 'import time; time.sleep(10); print("done")')
    #     job_id = future.value
    #     # Later...
    #     result = UnAsync.wait_for_job(job_id).value
    def execute_async(language, code, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync(
          'POST',
          '/execute',
          pk,
          sk,
          { language: language, code: code }
        )
        response['job_id']
      end
    end

    # Get current status/result of a job (single poll, no waiting)
    #
    # @param job_id [String] Job ID from execute_async
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to job response hash
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     job = UnAsync.get_job(job_id).value
    #     puts job["status"]  # => "running" or "completed"
    def get_job(job_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('GET', "/jobs/#{job_id}", pk, sk)
      end
    end

    # Wait for job completion with exponential backoff polling
    #
    # Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
    # Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+
    #
    # @param job_id [String] Job ID from execute_async
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param timeout [Integer] Maximum time to wait in seconds (default: 3600)
    # @return [Future<Hash>] Future resolving to final job result when status is terminal
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails or timeout reached (on .value)
    #
    # @example
    #     result = UnAsync.wait_for_job(job_id, timeout: 60).value
    #     puts result["stdout"]
    def wait_for_job(job_id, public_key: nil, secret_key: nil, timeout: 3600)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        wait_for_job_sync(job_id, pk, sk, timeout)
      end
    end

    # Cancel a running job
    #
    # @param job_id [String] Job ID to cancel
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with cancellation confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.cancel_job(job_id).value
    def cancel_job(job_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('DELETE', "/jobs/#{job_id}", pk, sk)
      end
    end

    # List all jobs for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Array<Hash>>] Future resolving to list of job hashes
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     jobs = UnAsync.list_jobs.value
    #     jobs.each { |job| puts "#{job['job_id']}: #{job['status']}" }
    def list_jobs(public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync('GET', '/jobs', pk, sk)
        response['jobs'] || []
      end
    end

    # Get list of supported programming languages
    #
    # Results are cached for 1 hour in ~/.unsandbox/languages.json
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Array<String>>] Future resolving to list of language identifiers
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     languages = UnAsync.get_languages.value
    #     puts languages.join(", ")
    def get_languages(public_key: nil, secret_key: nil)
      Future.new do
        # Try cache first (synchronous check)
        cached = load_languages_cache
        if cached
          cached
        else
          pk, sk = resolve_credentials(public_key, secret_key)
          response = make_request_sync('GET', '/languages', pk, sk)
          languages = response['languages'] || []

          # Cache the result
          save_languages_cache(languages)
          languages
        end
      end
    end

    # Detect programming language from filename extension
    # This is a synchronous operation (no I/O)
    #
    # @param filename [String] Filename to detect language from (e.g., "script.py")
    # @return [String, nil] Language identifier (e.g., "python") or nil if unknown
    #
    # @example
    #     UnAsync.detect_language("hello.py")   # => "python"
    #     UnAsync.detect_language("script.js")  # => "javascript"
    #     UnAsync.detect_language("main.go")    # => "go"
    #     UnAsync.detect_language("unknown")    # => nil
    def detect_language(filename)
      return nil if filename.nil? || !filename.include?('.')

      ext = filename.split('.').last&.downcase
      LANGUAGE_MAP[ext]
    end

    # Create a snapshot of a session
    #
    # @param session_id [String] Session ID to snapshot
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param name [String, nil] Optional snapshot name
    # @param ephemeral [Boolean] If true, create ephemeral snapshot (default: false)
    # @return [Future<String>] Future resolving to snapshot ID
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     snapshot_id = UnAsync.session_snapshot(session_id, name: "my-snapshot").value
    def session_snapshot(session_id, public_key: nil, secret_key: nil, name: nil, ephemeral: false)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = { session_id: session_id, hot: ephemeral }
        data[:name] = name if name

        response = make_request_sync('POST', '/snapshots', pk, sk, data)
        response['snapshot_id']
      end
    end

    # Create a snapshot of a service
    #
    # @param service_id [String] Service ID to snapshot
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param name [String, nil] Optional snapshot name
    # @return [Future<String>] Future resolving to snapshot ID
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     snapshot_id = UnAsync.service_snapshot(service_id, name: "production-backup").value
    def service_snapshot(service_id, public_key: nil, secret_key: nil, name: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = { service_id: service_id, hot: false }
        data[:name] = name if name

        response = make_request_sync('POST', '/snapshots', pk, sk, data)
        response['snapshot_id']
      end
    end

    # List all snapshots
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Array<Hash>>] Future resolving to list of snapshot hashes
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     snapshots = UnAsync.list_snapshots.value
    #     snapshots.each { |s| puts "#{s['snapshot_id']}: #{s['name']}" }
    def list_snapshots(public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync('GET', '/snapshots', pk, sk)
        response['snapshots'] || []
      end
    end

    # Restore a snapshot
    #
    # @param snapshot_id [String] Snapshot ID to restore
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with restored resource info
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.restore_snapshot(snapshot_id).value
    #     puts result["session_id"]  # or result["service_id"]
    def restore_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/snapshots/#{snapshot_id}/restore", pk, sk, {})
      end
    end

    # Delete a snapshot
    #
    # @param snapshot_id [String] Snapshot ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.delete_snapshot(snapshot_id).value
    def delete_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('DELETE', "/snapshots/#{snapshot_id}", pk, sk)
      end
    end

    # Execute multiple futures concurrently and wait for all to complete
    #
    # @param futures [Array<Future>] Array of futures to wait for
    # @param timeout [Numeric, nil] Maximum time to wait in seconds
    # @return [Array] Array of results in same order as input futures
    # @raise [StandardError] If any future raises an error
    #
    # @example
    #     futures = [
    #       UnAsync.execute_code("python", 'print(1)'),
    #       UnAsync.execute_code("python", 'print(2)'),
    #       UnAsync.execute_code("python", 'print(3)')
    #     ]
    #     results = UnAsync.all(futures)
    def all(futures, timeout: nil)
      futures.map { |f| f.value(timeout: timeout) }
    end

    # Execute multiple futures concurrently and return when first completes
    #
    # @param futures [Array<Future>] Array of futures
    # @param timeout [Numeric, nil] Maximum time to wait in seconds
    # @return [Object] Result of first completed future
    # @raise [StandardError] If the first completed future raised an error
    #
    # @example
    #     futures = [
    #       UnAsync.execute_code("python", 'import time; time.sleep(5); print("slow")'),
    #       UnAsync.execute_code("python", 'print("fast")')
    #     ]
    #     result = UnAsync.race(futures)  # Returns result of faster one
    def race(futures, timeout: nil)
      return nil if futures.empty?

      mutex = Mutex.new
      condition = ConditionVariable.new
      result = nil
      error = nil
      completed = false

      futures.each do |future|
        Thread.new do
          begin
            val = future.value
            mutex.synchronize do
              unless completed
                result = val
                completed = true
                condition.broadcast
              end
            end
          rescue StandardError => e
            mutex.synchronize do
              unless completed
                error = e
                completed = true
                condition.broadcast
              end
            end
          end
        end
      end

      mutex.synchronize do
        unless completed
          if timeout
            deadline = Time.now + timeout
            until completed
              remaining = deadline - Time.now
              raise Timeout::Error, 'Race timed out' if remaining <= 0

              condition.wait(mutex, remaining)
            end
          else
            condition.wait(mutex) until completed
          end
        end

        raise error if error

        result
      end
    end

    private

    # Language detection mapping (file extension -> language)
    LANGUAGE_MAP = {
      'py' => 'python',
      'js' => 'javascript',
      'ts' => 'typescript',
      'rb' => 'ruby',
      'php' => 'php',
      'pl' => 'perl',
      'sh' => 'bash',
      'r' => 'r',
      'lua' => 'lua',
      'go' => 'go',
      'rs' => 'rust',
      'c' => 'c',
      'cpp' => 'cpp',
      'cc' => 'cpp',
      'cxx' => 'cpp',
      'java' => 'java',
      'kt' => 'kotlin',
      'm' => 'objc',
      'cs' => 'csharp',
      'fs' => 'fsharp',
      'hs' => 'haskell',
      'ml' => 'ocaml',
      'clj' => 'clojure',
      'scm' => 'scheme',
      'ss' => 'scheme',
      'erl' => 'erlang',
      'ex' => 'elixir',
      'exs' => 'elixir',
      'jl' => 'julia',
      'd' => 'd',
      'nim' => 'nim',
      'zig' => 'zig',
      'v' => 'v',
      'cr' => 'crystal',
      'dart' => 'dart',
      'groovy' => 'groovy',
      'f90' => 'fortran',
      'f95' => 'fortran',
      'lisp' => 'commonlisp',
      'lsp' => 'commonlisp',
      'cob' => 'cobol',
      'tcl' => 'tcl',
      'raku' => 'raku',
      'pro' => 'prolog',
      'p' => 'prolog',
      '4th' => 'forth',
      'forth' => 'forth',
      'fth' => 'forth'
    }.freeze

    # Get ~/.unsandbox directory path, creating if necessary
    #
    # @return [String] Path to unsandbox config directory
    def unsandbox_dir
      dir = File.join(Dir.home, '.unsandbox')
      FileUtils.mkdir_p(dir, mode: 0o700) unless Dir.exist?(dir)
      dir
    end

    # Load credentials from CSV file (public_key,secret_key per line)
    #
    # @param csv_path [String] Path to CSV file
    # @param account_index [Integer] Account index (0-based)
    # @return [Array<String>, nil] [public_key, secret_key] or nil if not found
    def load_credentials_from_csv(csv_path, account_index = 0)
      return nil unless File.exist?(csv_path)

      current_index = 0
      File.foreach(csv_path) do |line|
        line = line.strip
        next if line.empty? || line.start_with?('#')

        if current_index == account_index
          parts = line.split(',')
          return [parts[0].strip, parts[1].strip] if parts.length >= 2
        end
        current_index += 1
      end

      nil
    rescue StandardError
      nil
    end

    # Resolve credentials from 4-tier priority system
    #
    # Priority:
    #   1. Method arguments
    #   2. Environment variables
    #   3. ~/.unsandbox/accounts.csv
    #   4. ./accounts.csv
    #
    # @param public_key [String, nil] Explicit public key
    # @param secret_key [String, nil] Explicit secret key
    # @param account_index [Integer, nil] Account index for CSV files
    # @return [Array<String>] [public_key, secret_key]
    # @raise [CredentialsError] If no credentials found
    def resolve_credentials(public_key = nil, secret_key = nil, account_index = nil)
      # Tier 1: Method arguments
      return [public_key, secret_key] if public_key && secret_key

      # Tier 2: Environment variables
      env_pk = ENV['UNSANDBOX_PUBLIC_KEY']
      env_sk = ENV['UNSANDBOX_SECRET_KEY']
      return [env_pk, env_sk] if env_pk && env_sk

      # Determine account index
      account_index ||= ENV.fetch('UNSANDBOX_ACCOUNT', '0').to_i

      # Tier 3: ~/.unsandbox/accounts.csv
      creds = load_credentials_from_csv(File.join(unsandbox_dir, 'accounts.csv'), account_index)
      return creds if creds

      # Tier 4: ./accounts.csv
      creds = load_credentials_from_csv('accounts.csv', account_index)
      return creds if creds

      raise CredentialsError, <<~MSG
        No credentials found. Please provide via:
          1. Method arguments (public_key:, secret_key:)
          2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
          3. ~/.unsandbox/accounts.csv
          4. ./accounts.csv
      MSG
    end

    # Sign a request using HMAC-SHA256
    #
    # Message format: "timestamp:METHOD:path:body"
    #
    # @param secret_key [String] Secret key for signing
    # @param timestamp [Integer] Unix timestamp
    # @param method [String] HTTP method (GET, POST, DELETE)
    # @param path [String] API path
    # @param body [String, nil] Request body (JSON string)
    # @return [String] 64-character lowercase hex signature
    def sign_request(secret_key, timestamp, method, path, body = nil)
      body_str = body || ''
      message = "#{timestamp}:#{method}:#{path}:#{body_str}"
      OpenSSL::HMAC.hexdigest('SHA256', secret_key, message)
    end

    # Make a synchronous authenticated HTTP request to the API
    # (Used internally by async methods running in threads)
    #
    # @param method [String] HTTP method (GET, POST, DELETE)
    # @param path [String] API path
    # @param public_key [String] API public key
    # @param secret_key [String] API secret key
    # @param data [Hash, nil] Request body data
    # @return [Hash] Parsed JSON response
    # @raise [APIError] If request fails
    def make_request_sync(method, path, public_key, secret_key, data = nil)
      uri = URI.parse("#{API_BASE}#{path}")
      timestamp = Time.now.to_i
      body = data ? JSON.generate(data) : ''

      signature = sign_request(secret_key, timestamp, method, path, data ? body : nil)

      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = true
      http.open_timeout = REQUEST_TIMEOUT
      http.read_timeout = REQUEST_TIMEOUT

      headers = {
        'Authorization' => "Bearer #{public_key}",
        'X-Timestamp' => timestamp.to_s,
        'X-Signature' => signature,
        'Content-Type' => 'application/json'
      }

      response = case method
                 when 'GET'
                   http.get(uri.request_uri, headers)
                 when 'POST'
                   http.post(uri.request_uri, body, headers)
                 when 'DELETE'
                   http.delete(uri.request_uri, headers)
                 else
                   raise APIError, "Unsupported HTTP method: #{method}"
                 end

      unless response.is_a?(Net::HTTPSuccess)
        raise APIError.new(
          "API request failed: #{response.code} #{response.message}",
          status_code: response.code.to_i,
          response_body: response.body
        )
      end

      JSON.parse(response.body)
    rescue JSON::ParserError => e
      raise APIError, "Invalid JSON response: #{e.message}"
    rescue Net::OpenTimeout, Net::ReadTimeout => e
      raise APIError, "Request timeout: #{e.message}"
    rescue StandardError => e
      raise APIError, "Request failed: #{e.message}" unless e.is_a?(APIError)

      raise
    end

    # Wait for job completion synchronously (used internally)
    #
    # @param job_id [String] Job ID
    # @param public_key [String] API public key
    # @param secret_key [String] API secret key
    # @param timeout [Integer] Maximum wait time in seconds
    # @return [Hash] Final job result
    # @raise [APIError] If timeout reached
    def wait_for_job_sync(job_id, public_key, secret_key, timeout = 3600)
      poll_count = 0
      start_time = Time.now

      loop do
        # Check timeout
        elapsed = Time.now - start_time
        raise APIError, "Job wait timeout after #{timeout} seconds" if elapsed > timeout

        # Sleep before polling
        delay_idx = [poll_count, POLL_DELAYS_MS.length - 1].min
        sleep(POLL_DELAYS_MS[delay_idx] / 1000.0)
        poll_count += 1

        response = make_request_sync('GET', "/jobs/#{job_id}", public_key, secret_key)
        status = response['status']

        return response if %w[completed failed timeout cancelled].include?(status)

        # Still running, continue polling
      end
    end

    # Get path to languages cache file
    #
    # @return [String] Path to languages.json
    def languages_cache_path
      File.join(unsandbox_dir, 'languages.json')
    end

    # Load languages from cache if valid (< 1 hour old)
    #
    # @return [Array<String>, nil] Cached languages or nil if cache invalid/missing
    def load_languages_cache
      cache_path = languages_cache_path
      return nil unless File.exist?(cache_path)

      # Check if cache is fresh
      age_seconds = Time.now - File.mtime(cache_path)
      return nil if age_seconds >= LANGUAGES_CACHE_TTL

      data = JSON.parse(File.read(cache_path))
      data['languages']
    rescue StandardError
      nil
    end

    # Save languages to cache
    #
    # @param languages [Array<String>] Languages to cache
    # @return [void]
    def save_languages_cache(languages)
      cache_path = languages_cache_path
      File.write(cache_path, JSON.generate({
                                             languages: languages,
                                             timestamp: Time.now.to_i
                                           }))
    rescue StandardError
      # Cache failures are non-fatal
      nil
    end
  end
end
