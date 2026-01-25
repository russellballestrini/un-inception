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
# CLI Usage:
#     ruby un_async.rb script.py                  # Execute Python script
#     ruby un_async.rb -s bash 'echo hello'       # Inline bash command
#     ruby un_async.rb session --list             # List sessions
#     ruby un_async.rb service --list             # List services
#     ruby un_async.rb snapshot --list            # List snapshots
#     ruby un_async.rb key                        # Check API key
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
require 'optparse'

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

    # Lock a snapshot to prevent deletion
    #
    # @param snapshot_id [String] Snapshot ID to lock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with lock confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.lock_snapshot(snapshot_id).value
    def lock_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/snapshots/#{snapshot_id}/lock", pk, sk, {})
      end
    end

    # Unlock a snapshot to allow deletion
    #
    # @param snapshot_id [String] Snapshot ID to unlock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with unlock confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.unlock_snapshot(snapshot_id).value
    def unlock_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/snapshots/#{snapshot_id}/unlock", pk, sk, {})
      end
    end

    # Clone a snapshot to create a new session or service
    #
    # @param snapshot_id [String] Snapshot ID to clone
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param name [String, nil] Optional name for the cloned resource
    # @param type [String] Type of resource to create ("session" or "service")
    # @param shell [String, nil] Optional shell for session clones
    # @param ports [Array<Integer>, nil] Optional ports for service clones
    # @return [Future<Hash>] Future resolving to response hash with cloned resource info
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example Clone to session
    #     result = UnAsync.clone_snapshot(snapshot_id, type: "session").value
    #     puts result["session_id"]
    def clone_snapshot(snapshot_id, public_key: nil, secret_key: nil, name: nil, type: 'session', shell: nil, ports: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = { type: type }
        data[:name] = name if name
        data[:shell] = shell if shell
        data[:ports] = ports if ports
        make_request_sync('POST', "/snapshots/#{snapshot_id}/clone", pk, sk, data)
      end
    end

    # ============================================================================
    # Session Functions
    # ============================================================================

    # List all sessions for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Array<Hash>>] Future resolving to list of session hashes
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     sessions = UnAsync.list_sessions.value
    #     sessions.each { |s| puts "#{s['id']}: #{s['status']}" }
    def list_sessions(public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync('GET', '/sessions', pk, sk)
        response['sessions'] || []
      end
    end

    # Get session details by ID
    #
    # @param session_id [String] Session ID to retrieve
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to session details hash
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     session = UnAsync.get_session(session_id).value
    #     puts session["status"]
    def get_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('GET', "/sessions/#{session_id}", pk, sk)
      end
    end

    # Create a new interactive session
    #
    # @param language [String] Shell or language for the session (e.g., "bash", "python3")
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param network_mode [String] Network mode ("zerotrust" or "semitrusted")
    # @param ttl [Integer] Time-to-live in seconds (default: 3600)
    # @param multiplexer [String, nil] Terminal multiplexer ("tmux" or "screen")
    # @param vcpu [Integer] Number of vCPUs (1-8)
    # @return [Future<Hash>] Future resolving to response hash with session_id and container_name
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.create_session("bash", network_mode: "semitrusted").value
    #     puts result["session_id"]
    def create_session(language, public_key: nil, secret_key: nil, network_mode: 'zerotrust', ttl: 3600, multiplexer: nil, vcpu: 1)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = {
          network_mode: network_mode,
          ttl: ttl
        }
        data[:shell] = language if language
        data[:multiplexer] = multiplexer if multiplexer
        data[:vcpu] = vcpu if vcpu > 1
        make_request_sync('POST', '/sessions', pk, sk, data)
      end
    end

    # Delete (terminate) a session
    #
    # @param session_id [String] Session ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.delete_session(session_id).value
    def delete_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('DELETE', "/sessions/#{session_id}", pk, sk)
      end
    end

    # Freeze a session (pause execution, reduce resource usage)
    #
    # @param session_id [String] Session ID to freeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with freeze confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.freeze_session(session_id).value
    def freeze_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/sessions/#{session_id}/freeze", pk, sk, {})
      end
    end

    # Unfreeze a session (resume execution)
    #
    # @param session_id [String] Session ID to unfreeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with unfreeze confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.unfreeze_session(session_id).value
    def unfreeze_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/sessions/#{session_id}/unfreeze", pk, sk, {})
      end
    end

    # Boost a session (increase vCPU allocation)
    #
    # @param session_id [String] Session ID to boost
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with boost confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.boost_session(session_id).value
    def boost_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/sessions/#{session_id}/boost", pk, sk, {})
      end
    end

    # Unboost a session (reduce vCPU allocation)
    #
    # @param session_id [String] Session ID to unboost
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with unboost confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.unboost_session(session_id).value
    def unboost_session(session_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/sessions/#{session_id}/unboost", pk, sk, {})
      end
    end

    # Execute a shell command in an existing session
    #
    # @param session_id [String] Session ID to execute command in
    # @param command [String] Command to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with command output
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.shell_session(session_id, "ls -la").value
    #     puts result["stdout"]
    def shell_session(session_id, command, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/sessions/#{session_id}/shell", pk, sk, { command: command })
      end
    end

    # ============================================================================
    # Service Functions
    # ============================================================================

    # List all services for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Array<Hash>>] Future resolving to list of service hashes
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     services = UnAsync.list_services.value
    #     services.each { |s| puts "#{s['id']}: #{s['name']} (#{s['state']})" }
    def list_services(public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        response = make_request_sync('GET', '/services', pk, sk)
        response['services'] || []
      end
    end

    # Create a new persistent service
    #
    # @param name [String] Service name
    # @param ports [Array<Integer>] Ports to expose
    # @param bootstrap [String] Bootstrap script content or URL
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param network_mode [String] Network mode ("zerotrust" or "semitrusted")
    # @param vcpu [Integer] Number of vCPUs (1-8)
    # @param custom_domains [Array<String>, nil] Custom domains for the service
    # @param service_type [String, nil] Service type for SRV records
    # @param unfreeze_on_demand [Boolean] If true, service will auto-wake on HTTP request (default: false)
    # @return [Future<Hash>] Future resolving to response hash with service_id
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.create_service("web", [80, 443], "apt install -y nginx && nginx").value
    #     puts result["service_id"]
    def create_service(name, ports, bootstrap, public_key: nil, secret_key: nil, network_mode: 'semitrusted', vcpu: 1, custom_domains: nil, service_type: nil, unfreeze_on_demand: false)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = {
          name: name,
          ports: ports,
          bootstrap: bootstrap,
          network_mode: network_mode
        }
        data[:vcpu] = vcpu if vcpu > 1
        data[:custom_domains] = custom_domains if custom_domains
        data[:service_type] = service_type if service_type
        data[:unfreeze_on_demand] = unfreeze_on_demand if unfreeze_on_demand
        make_request_sync('POST', '/services', pk, sk, data)
      end
    end

    # Get service details by ID
    #
    # @param service_id [String] Service ID to retrieve
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to service details hash
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     service = UnAsync.get_service(service_id).value
    #     puts service["status"]
    def get_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('GET', "/services/#{service_id}", pk, sk)
      end
    end

    # Update a service (e.g., resize vCPU)
    #
    # @param service_id [String] Service ID to update
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param vcpu [Integer, nil] New vCPU count (1-8)
    # @param name [String, nil] New service name
    # @return [Future<Hash>] Future resolving to response hash with update confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.update_service(service_id, vcpu: 4).value
    def update_service(service_id, public_key: nil, secret_key: nil, vcpu: nil, name: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = {}
        data[:vcpu] = vcpu if vcpu
        data[:name] = name if name
        make_request_sync('PATCH', "/services/#{service_id}", pk, sk, data)
      end
    end

    # Delete (destroy) a service
    #
    # @param service_id [String] Service ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.delete_service(service_id).value
    def delete_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('DELETE', "/services/#{service_id}", pk, sk)
      end
    end

    # Freeze a service (stop container, reduce resource usage)
    #
    # @param service_id [String] Service ID to freeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with freeze confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.freeze_service(service_id).value
    def freeze_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/services/#{service_id}/freeze", pk, sk, {})
      end
    end

    # Unfreeze a service (start container)
    #
    # @param service_id [String] Service ID to unfreeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with unfreeze confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.unfreeze_service(service_id).value
    def unfreeze_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/services/#{service_id}/unfreeze", pk, sk, {})
      end
    end

    # Lock a service to prevent deletion
    #
    # @param service_id [String] Service ID to lock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with lock confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.lock_service(service_id).value
    def lock_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/services/#{service_id}/lock", pk, sk, {})
      end
    end

    # Unlock a service to allow deletion
    #
    # @param service_id [String] Service ID to unlock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with unlock confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.unlock_service(service_id).value
    def unlock_service(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/services/#{service_id}/unlock", pk, sk, {})
      end
    end

    # Set unfreeze-on-demand for a service
    #
    # When enabled, a frozen service will automatically wake when it receives an HTTP request.
    #
    # @param service_id [String] Service ID to update
    # @param enabled [Boolean] Whether to enable unfreeze-on-demand
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with update confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.set_unfreeze_on_demand(service_id, true).value
    def set_unfreeze_on_demand(service_id, enabled, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('PATCH', "/services/#{service_id}", pk, sk, { unfreeze_on_demand: enabled })
      end
    end

    # Set show-freeze-page for a service
    #
    # When enabled, frozen services show a freeze page instead of 502 error.
    #
    # @param service_id [String] Service ID to update
    # @param enabled [Boolean] Whether to enable show-freeze-page
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with update confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.set_show_freeze_page(service_id, true).value
    def set_show_freeze_page(service_id, enabled, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('PATCH', "/services/#{service_id}", pk, sk, { show_freeze_page: enabled })
      end
    end

    # Get service logs (bootstrap output)
    #
    # @param service_id [String] Service ID to get logs for
    # @param all [Boolean] If true, get all logs; if false, get last 9000 lines
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with log content
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     logs = UnAsync.get_service_logs(service_id).value
    #     puts logs["log"]
    def get_service_logs(service_id, all: false, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        path = "/services/#{service_id}/logs"
        path += '?all=true' if all
        make_request_sync('GET', path, pk, sk)
      end
    end

    # Get service environment vault status
    #
    # @param service_id [String] Service ID to get env status for
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with vault status
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     status = UnAsync.get_service_env(service_id).value
    #     puts "Variables: #{status['count']}"
    def get_service_env(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('GET', "/services/#{service_id}/env", pk, sk)
      end
    end

    # Set service environment variables (replaces existing vault)
    #
    # @param service_id [String] Service ID to set env for
    # @param env [String] Environment content in .env format (KEY=VALUE per line)
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.set_service_env(service_id, "API_KEY=secret\nDEBUG=true").value
    def set_service_env(service_id, env, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_text_sync('PUT', "/services/#{service_id}/env", pk, sk, env)
      end
    end

    # Delete service environment vault
    #
    # @param service_id [String] Service ID to delete env for
    # @param keys [Array<String>, nil] Specific keys to delete (nil = delete entire vault)
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example Delete entire vault
    #     UnAsync.delete_service_env(service_id).value
    def delete_service_env(service_id, keys: nil, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        if keys
          make_request_sync('DELETE', "/services/#{service_id}/env", pk, sk, { keys: keys })
        else
          make_request_sync('DELETE', "/services/#{service_id}/env", pk, sk)
        end
      end
    end

    # Export service environment vault (returns .env format)
    #
    # @param service_id [String] Service ID to export env from
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with env content
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.export_service_env(service_id).value
    #     puts result["env"]
    def export_service_env(service_id, public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', "/services/#{service_id}/env/export", pk, sk, {})
      end
    end

    # Redeploy a service (re-run bootstrap script)
    #
    # @param service_id [String] Service ID to redeploy
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param bootstrap [String, nil] New bootstrap script (optional)
    # @return [Future<Hash>] Future resolving to response hash with redeploy confirmation
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     UnAsync.redeploy_service(service_id).value
    def redeploy_service(service_id, public_key: nil, secret_key: nil, bootstrap: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        data = {}
        data[:bootstrap] = bootstrap if bootstrap
        make_request_sync('POST', "/services/#{service_id}/redeploy", pk, sk, data)
      end
    end

    # Execute a command in a running service
    #
    # @param service_id [String] Service ID to execute command in
    # @param command [String] Command to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param timeout [Integer] Command timeout in milliseconds (default: 30000)
    # @return [Future<Hash>] Future resolving to response hash with command output
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.execute_in_service(service_id, "ls -la").value
    #     puts result["stdout"]
    def execute_in_service(service_id, command, public_key: nil, secret_key: nil, timeout: 30_000)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)

        # Start async execution
        response = make_request_sync('POST', "/services/#{service_id}/execute", pk, sk, {
                                       command: command,
                                       timeout: timeout
                                     })

        job_id = response['job_id']
        if job_id
          # Poll for completion
          wait_for_job_sync(job_id, pk, sk, (timeout / 1000) + 10)
        else
          response
        end
      end
    end

    # ============================================================================
    # Key Validation
    # ============================================================================

    # Validate API keys
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Future<Hash>] Future resolving to response hash with validation result
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails or keys invalid (on .value)
    #
    # @example
    #     result = UnAsync.validate_keys.value
    #     puts result["valid"]
    def validate_keys(public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        make_request_sync('POST', '/keys/validate', pk, sk, {})
      end
    end

    # Generate images from text prompt using AI.
    #
    # @param prompt [String] Text description of the image to generate
    # @param model [String, nil] Model to use (optional)
    # @param size [String] Image size (default: "1024x1024")
    # @param quality [String] "standard" or "hd" (default: "standard")
    # @param n [Integer] Number of images to generate (default: 1)
    # @param public_key [String, nil] API public key
    # @param secret_key [String, nil] API secret key
    # @return [Future<Hash>] Future resolving to result with :images array and :created_at
    # @raise [CredentialsError] If no credentials found (on .value)
    # @raise [APIError] If API request fails (on .value)
    #
    # @example
    #     result = UnAsync.image("A sunset over mountains").value
    #     puts result["images"]  # Array of image data/URLs
    def image(prompt, model: nil, size: '1024x1024', quality: 'standard', n: 1,
              public_key: nil, secret_key: nil)
      Future.new do
        pk, sk = resolve_credentials(public_key, secret_key)
        payload = {
          prompt: prompt,
          size: size,
          quality: quality,
          n: n
        }
        payload[:model] = model if model

        make_request_sync('POST', '/image', pk, sk, payload)
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
                 when 'PATCH'
                   http.patch(uri.request_uri, body, headers)
                 when 'PUT'
                   http.put(uri.request_uri, body, headers)
                 when 'DELETE'
                   req = Net::HTTP::Delete.new(uri.request_uri, headers)
                   req.body = body if data
                   http.request(req)
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

    # Make a synchronous authenticated HTTP request with text/plain content type
    #
    # @param method [String] HTTP method (PUT)
    # @param path [String] API path
    # @param public_key [String] API public key
    # @param secret_key [String] API secret key
    # @param body [String] Plain text request body
    # @return [Hash] Parsed JSON response
    # @raise [APIError] If request fails
    def make_request_text_sync(method, path, public_key, secret_key, body)
      uri = URI.parse("#{API_BASE}#{path}")
      timestamp = Time.now.to_i

      signature = sign_request(secret_key, timestamp, method, path, body)

      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = true
      http.open_timeout = REQUEST_TIMEOUT
      http.read_timeout = REQUEST_TIMEOUT

      headers = {
        'Authorization' => "Bearer #{public_key}",
        'X-Timestamp' => timestamp.to_s,
        'X-Signature' => signature,
        'Content-Type' => 'text/plain'
      }

      response = case method
                 when 'PUT'
                   http.put(uri.request_uri, body, headers)
                 else
                   raise APIError, "Unsupported HTTP method for text: #{method}"
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

  # ============================================================================
  # CLI Implementation
  # ============================================================================

  # Exit codes
  EXIT_SUCCESS = 0
  EXIT_ERROR = 1
  EXIT_INVALID_ARGS = 2
  EXIT_AUTH_ERROR = 3
  EXIT_API_ERROR = 4
  EXIT_TIMEOUT = 5

  class << self
    # Main CLI entry point
    def cli_main
      # Global options
      options = {
        shell: nil,
        env: [],
        files: [],
        file_paths: [],
        public_key: nil,
        secret_key: nil,
        network: 'zerotrust',
        vcpu: 1,
        yes: false,
        artifacts: false,
        output: nil
      }

      # Check for subcommands first
      if ARGV.empty?
        cli_show_help
        exit(EXIT_INVALID_ARGS)
      end

      case ARGV[0]
      when 'session'
        ARGV.shift
        cli_session(options)
      when 'service'
        ARGV.shift
        cli_service(options)
      when 'snapshot'
        ARGV.shift
        cli_snapshot(options)
      when 'key'
        ARGV.shift
        cli_key(options)
      when '-h', '--help', 'help'
        cli_show_help
        exit(EXIT_SUCCESS)
      else
        cli_execute(options)
      end
    rescue CredentialsError => e
      $stderr.puts "Error: #{e.message}"
      exit(EXIT_AUTH_ERROR)
    rescue APIError => e
      $stderr.puts "Error: #{e.message}"
      exit(EXIT_API_ERROR)
    rescue OptionParser::InvalidOption, OptionParser::MissingArgument => e
      $stderr.puts "Error: #{e.message}"
      exit(EXIT_INVALID_ARGS)
    rescue Interrupt
      $stderr.puts "\nInterrupted"
      exit(EXIT_ERROR)
    end

    private

    # Show main help
    def cli_show_help
      puts <<~HELP
        unsandbox.com Ruby SDK (Async) - Secure Code Execution

        Usage:
          ruby un_async.rb [options] <source_file>      Execute code file
          ruby un_async.rb [options] -s LANG 'code'     Execute inline code
          ruby un_async.rb session [options]            Manage sessions
          ruby un_async.rb service [options]            Manage services
          ruby un_async.rb snapshot [options]           Manage snapshots
          ruby un_async.rb key                          Check API key

        Global Options:
          -s, --shell LANG       Language for inline code
          -e, --env KEY=VAL      Set environment variable (can repeat)
          -f, --file FILE        Add input file to /tmp/
          -F, --file-path FILE   Add input file with path preserved
          -a, --artifacts        Return compiled artifacts
          -o, --output DIR       Output directory for artifacts
          -p, --public-key KEY   API public key
          -k, --secret-key KEY   API secret key
          -n, --network MODE     Network mode: zerotrust or semitrusted
          -v, --vcpu N           vCPU count (1-8)
          -y, --yes              Skip confirmation prompts
          -h, --help             Show this help

        Examples:
          ruby un_async.rb script.py
          ruby un_async.rb -s python 'print("hello")'
          ruby un_async.rb -n semitrusted crawler.py
          ruby un_async.rb session --list
          ruby un_async.rb service --name web --ports 80 --bootstrap "python -m http.server 80"
      HELP
    end

    # Parse global options from ARGV
    def parse_global_options(options)
      OptionParser.new do |opts|
        opts.on('-s', '--shell LANG', 'Language for inline code') do |v|
          options[:shell] = v
        end
        opts.on('-e', '--env KEY=VAL', 'Set environment variable') do |v|
          options[:env] << v
        end
        opts.on('-f', '--file FILE', 'Add input file to /tmp/') do |v|
          options[:files] << v
        end
        opts.on('-F', '--file-path FILE', 'Add input file with path preserved') do |v|
          options[:file_paths] << v
        end
        opts.on('-a', '--artifacts', 'Return compiled artifacts') do
          options[:artifacts] = true
        end
        opts.on('-o', '--output DIR', 'Output directory for artifacts') do |v|
          options[:output] = v
        end
        opts.on('-p', '--public-key KEY', 'API public key') do |v|
          options[:public_key] = v
        end
        opts.on('-k', '--secret-key KEY', 'API secret key') do |v|
          options[:secret_key] = v
        end
        opts.on('-n', '--network MODE', 'Network mode') do |v|
          options[:network] = v
        end
        opts.on('-v', '--vcpu N', Integer, 'vCPU count (1-8)') do |v|
          options[:vcpu] = v
        end
        opts.on('-y', '--yes', 'Skip confirmation prompts') do
          options[:yes] = true
        end
        opts.on('-h', '--help', 'Show help') do
          yield if block_given?
          exit(EXIT_SUCCESS)
        end
      end
    end

    # Execute code command
    def cli_execute(options)
      parser = parse_global_options(options) do
        cli_show_help
      end
      parser.parse!(ARGV)

      if ARGV.empty?
        $stderr.puts 'Error: No source file or code provided'
        exit(EXIT_INVALID_ARGS)
      end

      code = nil
      language = nil

      if options[:shell]
        # Inline code mode: -s LANG 'code'
        language = options[:shell]
        code = ARGV.join(' ')
      else
        # File mode: script.py
        filename = ARGV[0]
        unless File.exist?(filename)
          $stderr.puts "Error: File not found: #{filename}"
          exit(EXIT_INVALID_ARGS)
        end
        code = File.read(filename)
        language = detect_language(filename)
        unless language
          $stderr.puts "Error: Cannot detect language for: #{filename}"
          $stderr.puts 'Use -s/--shell to specify language'
          exit(EXIT_INVALID_ARGS)
        end
      end

      # Execute the code (async call, await result)
      result = execute_code(
        language,
        code,
        public_key: options[:public_key],
        secret_key: options[:secret_key]
      ).value

      # Output results
      cli_print_execute_result(result)
    end

    # Print execution result
    def cli_print_execute_result(result)
      puts result['stdout'] if result['stdout'] && !result['stdout'].empty?
      $stderr.puts result['stderr'] if result['stderr'] && !result['stderr'].empty?
      puts '---'
      puts "Exit code: #{result['exit_code'] || 0}"
      if result['execution_time_ms']
        puts "Execution time: #{result['execution_time_ms']}ms"
      end
    end

    # Session subcommand
    def cli_session(options)
      session_opts = {
        list: false,
        attach: nil,
        kill: nil,
        freeze: nil,
        unfreeze: nil,
        boost: nil,
        unboost: nil,
        snapshot: nil,
        snapshot_name: nil,
        hot: false,
        tmux: false,
        screen: false,
        shell: 'bash',
        audit: false
      }

      parser = parse_global_options(options) do
        cli_session_help
      end

      parser.on('-l', '--list', 'List active sessions') do
        session_opts[:list] = true
      end
      parser.on('--attach ID', 'Reconnect to existing session') do |v|
        session_opts[:attach] = v
      end
      parser.on('--kill ID', 'Terminate a session') do |v|
        session_opts[:kill] = v
      end
      parser.on('--freeze ID', 'Pause session') do |v|
        session_opts[:freeze] = v
      end
      parser.on('--unfreeze ID', 'Resume session') do |v|
        session_opts[:unfreeze] = v
      end
      parser.on('--boost ID', 'Add vCPUs/RAM') do |v|
        session_opts[:boost] = v
      end
      parser.on('--unboost ID', 'Remove boost') do |v|
        session_opts[:unboost] = v
      end
      parser.on('--snapshot ID', 'Create snapshot') do |v|
        session_opts[:snapshot] = v
      end
      parser.on('--snapshot-name NAME', 'Name for snapshot') do |v|
        session_opts[:snapshot_name] = v
      end
      parser.on('--hot', 'Live snapshot (no freeze)') do
        session_opts[:hot] = true
      end
      parser.on('--tmux', 'Enable persistence with tmux') do
        session_opts[:tmux] = true
      end
      parser.on('--screen', 'Enable persistence with screen') do
        session_opts[:screen] = true
      end
      parser.on('--shell SHELL', 'Shell/REPL to use') do |v|
        session_opts[:shell] = v
      end
      parser.on('--audit', 'Record session') do
        session_opts[:audit] = true
      end

      parser.parse!(ARGV)

      creds = { public_key: options[:public_key], secret_key: options[:secret_key] }

      if session_opts[:list]
        sessions = list_sessions(**creds).value
        cli_print_sessions_table(sessions)
      elsif session_opts[:kill]
        delete_session(session_opts[:kill], **creds).value
        puts "Session #{session_opts[:kill]} terminated"
      elsif session_opts[:freeze]
        freeze_session(session_opts[:freeze], **creds).value
        puts "Session #{session_opts[:freeze]} frozen"
      elsif session_opts[:unfreeze]
        unfreeze_session(session_opts[:unfreeze], **creds).value
        puts "Session #{session_opts[:unfreeze]} unfrozen"
      elsif session_opts[:boost]
        boost_session(session_opts[:boost], **creds).value
        puts "Session #{session_opts[:boost]} boosted"
      elsif session_opts[:unboost]
        unboost_session(session_opts[:unboost], **creds).value
        puts "Session #{session_opts[:unboost]} unboosted"
      elsif session_opts[:snapshot]
        snapshot_id = session_snapshot(
          session_opts[:snapshot],
          name: session_opts[:snapshot_name],
          ephemeral: session_opts[:hot],
          **creds
        ).value
        puts "Snapshot created: #{snapshot_id}"
      elsif session_opts[:attach]
        # Attach to existing session - show info
        session = get_session(session_opts[:attach], **creds).value
        puts "Session: #{session['id']}"
        puts "Status: #{session['status']}"
        puts "WebSocket URL: #{session['websocket_url']}" if session['websocket_url']
        puts "\nNote: Use a WebSocket client to connect interactively"
      else
        # Create new session
        multiplexer = nil
        multiplexer = 'tmux' if session_opts[:tmux]
        multiplexer = 'screen' if session_opts[:screen]

        result = create_session(
          session_opts[:shell],
          network_mode: options[:network],
          vcpu: options[:vcpu],
          multiplexer: multiplexer,
          **creds
        ).value
        puts "Session created: #{result['session_id']}"
        puts "Container: #{result['container_name']}" if result['container_name']
        puts "WebSocket URL: #{result['websocket_url']}" if result['websocket_url']
        puts "\nNote: Use a WebSocket client to connect interactively"
      end
    end

    # Print sessions table
    def cli_print_sessions_table(sessions)
      if sessions.empty?
        puts 'No active sessions'
        return
      end

      # Header
      puts format('%-38s %-20s %-10s %-20s', 'ID', 'NAME', 'STATUS', 'CREATED')
      sessions.each do |s|
        puts format('%-38s %-20s %-10s %-20s',
                    s['id'] || s['session_id'] || '-',
                    s['name'] || '-',
                    s['status'] || s['state'] || '-',
                    s['created_at'] || '-')
      end
    end

    # Session help
    def cli_session_help
      puts <<~HELP
        Session Management

        Usage:
          ruby un_async.rb session [options]

        Options:
          --shell SHELL          Shell/REPL to use (default: bash)
          -l, --list             List active sessions
          --attach ID            Reconnect to existing session
          --kill ID              Terminate a session
          --freeze ID            Pause session
          --unfreeze ID          Resume session
          --boost ID             Add vCPUs/RAM
          --unboost ID           Remove boost
          --tmux                 Enable persistence with tmux
          --screen               Enable persistence with screen
          --snapshot ID          Create snapshot
          --snapshot-name NAME   Name for snapshot
          --hot                  Live snapshot (no freeze)
          --audit                Record session

        Examples:
          ruby un_async.rb session                          # New bash session
          ruby un_async.rb session --shell python3          # Python REPL
          ruby un_async.rb session --tmux                   # Persistent session
          ruby un_async.rb session --list                   # List sessions
          ruby un_async.rb session --kill abc123            # Kill session
      HELP
    end

    # Service subcommand
    def cli_service(options)
      service_opts = {
        list: false,
        name: nil,
        ports: nil,
        domains: nil,
        type: nil,
        bootstrap: nil,
        bootstrap_file: nil,
        env_file: nil,
        info: nil,
        logs: nil,
        tail: nil,
        freeze: nil,
        unfreeze: nil,
        destroy: nil,
        lock: nil,
        unlock: nil,
        resize: nil,
        redeploy: nil,
        execute: nil,
        execute_cmd: nil,
        snapshot: nil,
        snapshot_name: nil
      }

      parser = parse_global_options(options) do
        cli_service_help
      end

      parser.on('-l', '--list', 'List all services') do
        service_opts[:list] = true
      end
      parser.on('--name NAME', 'Service name (creates new)') do |v|
        service_opts[:name] = v
      end
      parser.on('--ports PORTS', 'Comma-separated ports') do |v|
        service_opts[:ports] = v.split(',').map(&:to_i)
      end
      parser.on('--domains DOMAINS', 'Custom domains') do |v|
        service_opts[:domains] = v.split(',')
      end
      parser.on('--type TYPE', 'Service type (minecraft, tcp, udp)') do |v|
        service_opts[:type] = v
      end
      parser.on('--bootstrap CMD', 'Bootstrap command') do |v|
        service_opts[:bootstrap] = v
      end
      parser.on('--bootstrap-file FILE', 'Bootstrap from file') do |v|
        service_opts[:bootstrap_file] = v
      end
      parser.on('--env-file FILE', 'Load env from .env file') do |v|
        service_opts[:env_file] = v
      end
      parser.on('--info ID', 'Get service details') do |v|
        service_opts[:info] = v
      end
      parser.on('--logs ID', 'Get all logs') do |v|
        service_opts[:logs] = v
      end
      parser.on('--tail ID', 'Get last 9000 lines') do |v|
        service_opts[:tail] = v
      end
      parser.on('--freeze ID', 'Pause service') do |v|
        service_opts[:freeze] = v
      end
      parser.on('--unfreeze ID', 'Resume service') do |v|
        service_opts[:unfreeze] = v
      end
      parser.on('--destroy ID', 'Delete service') do |v|
        service_opts[:destroy] = v
      end
      parser.on('--lock ID', 'Prevent deletion') do |v|
        service_opts[:lock] = v
      end
      parser.on('--unlock ID', 'Allow deletion') do |v|
        service_opts[:unlock] = v
      end
      parser.on('--resize ID', 'Resize (with --vcpu)') do |v|
        service_opts[:resize] = v
      end
      parser.on('--redeploy ID', 'Re-run bootstrap') do |v|
        service_opts[:redeploy] = v
      end
      parser.on('--execute ID', 'Run command in service') do |v|
        service_opts[:execute] = v
      end
      parser.on('--snapshot ID', 'Create snapshot') do |v|
        service_opts[:snapshot] = v
      end
      parser.on('--snapshot-name NAME', 'Name for snapshot') do |v|
        service_opts[:snapshot_name] = v
      end

      parser.parse!(ARGV)

      # Check for env subcommand
      if ARGV[0] == 'env'
        ARGV.shift
        cli_service_env(options, service_opts)
        return
      end

      # Get command argument for execute
      service_opts[:execute_cmd] = ARGV.join(' ') if service_opts[:execute] && !ARGV.empty?

      creds = { public_key: options[:public_key], secret_key: options[:secret_key] }

      if service_opts[:list]
        services = list_services(**creds).value
        cli_print_services_table(services)
      elsif service_opts[:info]
        service = get_service(service_opts[:info], **creds).value
        cli_print_service_info(service)
      elsif service_opts[:logs]
        result = get_service_logs(service_opts[:logs], all: true, **creds).value
        puts result['log'] || result['logs'] || ''
      elsif service_opts[:tail]
        result = get_service_logs(service_opts[:tail], all: false, **creds).value
        puts result['log'] || result['logs'] || ''
      elsif service_opts[:freeze]
        freeze_service(service_opts[:freeze], **creds).value
        puts "Service #{service_opts[:freeze]} frozen"
      elsif service_opts[:unfreeze]
        unfreeze_service(service_opts[:unfreeze], **creds).value
        puts "Service #{service_opts[:unfreeze]} unfrozen"
      elsif service_opts[:destroy]
        delete_service(service_opts[:destroy], **creds).value
        puts "Service #{service_opts[:destroy]} destroyed"
      elsif service_opts[:lock]
        lock_service(service_opts[:lock], **creds).value
        puts "Service #{service_opts[:lock]} locked"
      elsif service_opts[:unlock]
        unlock_service(service_opts[:unlock], **creds).value
        puts "Service #{service_opts[:unlock]} unlocked"
      elsif service_opts[:resize]
        update_service(service_opts[:resize], vcpu: options[:vcpu], **creds).value
        puts "Service #{service_opts[:resize]} resized to #{options[:vcpu]} vCPUs"
      elsif service_opts[:redeploy]
        bootstrap = nil
        if service_opts[:bootstrap_file]
          bootstrap = File.read(service_opts[:bootstrap_file])
        elsif service_opts[:bootstrap]
          bootstrap = service_opts[:bootstrap]
        end
        redeploy_service(service_opts[:redeploy], bootstrap: bootstrap, **creds).value
        puts "Service #{service_opts[:redeploy]} redeployed"
      elsif service_opts[:execute]
        cmd = service_opts[:execute_cmd]
        if cmd.nil? || cmd.empty?
          $stderr.puts 'Error: No command provided for --execute'
          exit(EXIT_INVALID_ARGS)
        end
        result = execute_in_service(service_opts[:execute], cmd, **creds).value
        cli_print_execute_result(result)
      elsif service_opts[:snapshot]
        snapshot_id = service_snapshot(
          service_opts[:snapshot],
          name: service_opts[:snapshot_name],
          **creds
        ).value
        puts "Snapshot created: #{snapshot_id}"
      elsif service_opts[:name]
        # Create new service
        bootstrap = service_opts[:bootstrap]
        if service_opts[:bootstrap_file]
          bootstrap = File.read(service_opts[:bootstrap_file])
        end

        unless bootstrap
          $stderr.puts 'Error: --bootstrap or --bootstrap-file required'
          exit(EXIT_INVALID_ARGS)
        end
        unless service_opts[:ports]
          $stderr.puts 'Error: --ports required'
          exit(EXIT_INVALID_ARGS)
        end

        result = create_service(
          service_opts[:name],
          service_opts[:ports],
          bootstrap,
          network_mode: options[:network],
          vcpu: options[:vcpu],
          custom_domains: service_opts[:domains],
          service_type: service_opts[:type],
          **creds
        ).value
        puts "Service created: #{result['service_id']}"
        puts "URL: #{result['url']}" if result['url']
      else
        cli_service_help
        exit(EXIT_INVALID_ARGS)
      end
    end

    # Print services table
    def cli_print_services_table(services)
      if services.empty?
        puts 'No services'
        return
      end

      puts format('%-38s %-20s %-10s %-20s', 'ID', 'NAME', 'STATUS', 'CREATED')
      services.each do |s|
        puts format('%-38s %-20s %-10s %-20s',
                    s['id'] || s['service_id'] || '-',
                    s['name'] || '-',
                    s['status'] || s['state'] || '-',
                    s['created_at'] || '-')
      end
    end

    # Print service info
    def cli_print_service_info(service)
      puts "ID: #{service['id'] || service['service_id']}"
      puts "Name: #{service['name']}"
      puts "Status: #{service['status'] || service['state']}"
      puts "URL: #{service['url']}" if service['url']
      puts "Ports: #{service['ports']&.join(', ')}" if service['ports']
      puts "vCPU: #{service['vcpu']}" if service['vcpu']
      puts "Network: #{service['network_mode']}" if service['network_mode']
      puts "Created: #{service['created_at']}" if service['created_at']
      puts "Locked: #{service['locked']}" if service.key?('locked')
    end

    # Service help
    def cli_service_help
      puts <<~HELP
        Service Management

        Usage:
          ruby un_async.rb service [options]
          ruby un_async.rb service env <command> <id>

        Options:
          --name NAME            Service name (creates new)
          --ports PORTS          Comma-separated ports
          --domains DOMAINS      Custom domains
          --type TYPE            Service type (minecraft, tcp, udp)
          --bootstrap CMD        Bootstrap command
          --bootstrap-file FILE  Bootstrap from file
          --env-file FILE        Load env from .env file
          -l, --list             List all services
          --info ID              Get service details
          --logs ID              Get all logs
          --tail ID              Get last 9000 lines
          --freeze ID            Pause service
          --unfreeze ID          Resume service
          --destroy ID           Delete service
          --lock ID              Prevent deletion
          --unlock ID            Allow deletion
          --resize ID            Resize (with --vcpu)
          --redeploy ID          Re-run bootstrap
          --execute ID 'cmd'     Run command in service
          --snapshot ID          Create snapshot

        Env Subcommands:
          ruby un_async.rb service env status ID    Show vault status
          ruby un_async.rb service env set ID       Set from --env-file or stdin
          ruby un_async.rb service env export ID    Export to stdout
          ruby un_async.rb service env delete ID    Delete vault

        Examples:
          ruby un_async.rb service --name web --ports 80 --bootstrap "python -m http.server 80"
          ruby un_async.rb service --list
          ruby un_async.rb service --logs abc123
          ruby un_async.rb service --execute abc123 'ls -la'
          ruby un_async.rb service env status abc123
      HELP
    end

    # Service env subcommand
    def cli_service_env(options, service_opts)
      if ARGV.empty?
        $stderr.puts 'Error: env subcommand requires: status, set, export, or delete'
        exit(EXIT_INVALID_ARGS)
      end

      cmd = ARGV.shift
      service_id = ARGV.shift

      unless service_id
        $stderr.puts 'Error: Service ID required'
        exit(EXIT_INVALID_ARGS)
      end

      creds = { public_key: options[:public_key], secret_key: options[:secret_key] }

      case cmd
      when 'status'
        result = get_service_env(service_id, **creds).value
        puts "Has vault: #{result['has_vault'] || false}"
        puts "Variables: #{result['count'] || 0}"
        puts "Updated: #{result['updated_at']}" if result['updated_at']
      when 'set'
        env_content = nil
        if service_opts[:env_file]
          env_content = File.read(service_opts[:env_file])
        elsif !$stdin.tty?
          env_content = $stdin.read
        else
          $stderr.puts 'Error: Provide --env-file or pipe content to stdin'
          exit(EXIT_INVALID_ARGS)
        end
        set_service_env(service_id, env_content, **creds).value
        puts "Environment vault updated for #{service_id}"
      when 'export'
        result = export_service_env(service_id, **creds).value
        puts result['env'] || ''
      when 'delete'
        delete_service_env(service_id, **creds).value
        puts "Environment vault deleted for #{service_id}"
      else
        $stderr.puts "Error: Unknown env command: #{cmd}"
        exit(EXIT_INVALID_ARGS)
      end
    end

    # Snapshot subcommand
    def cli_snapshot(options)
      snapshot_opts = {
        list: false,
        info: nil,
        delete: nil,
        lock: nil,
        unlock: nil,
        clone: nil,
        clone_type: 'session',
        clone_name: nil,
        clone_shell: nil,
        clone_ports: nil
      }

      parser = parse_global_options(options) do
        cli_snapshot_help
      end

      parser.on('-l', '--list', 'List all snapshots') do
        snapshot_opts[:list] = true
      end
      parser.on('--info ID', 'Get snapshot details') do |v|
        snapshot_opts[:info] = v
      end
      parser.on('--delete ID', 'Delete snapshot') do |v|
        snapshot_opts[:delete] = v
      end
      parser.on('--lock ID', 'Prevent deletion') do |v|
        snapshot_opts[:lock] = v
      end
      parser.on('--unlock ID', 'Allow deletion') do |v|
        snapshot_opts[:unlock] = v
      end
      parser.on('--clone ID', 'Clone snapshot') do |v|
        snapshot_opts[:clone] = v
      end
      parser.on('--type TYPE', 'Clone type: session or service') do |v|
        snapshot_opts[:clone_type] = v
      end
      parser.on('--name NAME', 'Name for cloned resource') do |v|
        snapshot_opts[:clone_name] = v
      end
      parser.on('--shell SHELL', 'Shell for cloned session') do |v|
        snapshot_opts[:clone_shell] = v
      end
      parser.on('--ports PORTS', 'Ports for cloned service') do |v|
        snapshot_opts[:clone_ports] = v.split(',').map(&:to_i)
      end

      parser.parse!(ARGV)

      creds = { public_key: options[:public_key], secret_key: options[:secret_key] }

      if snapshot_opts[:list]
        snapshots = list_snapshots(**creds).value
        cli_print_snapshots_table(snapshots)
      elsif snapshot_opts[:info]
        # Get snapshot details via restore endpoint or list
        snapshots = list_snapshots(**creds).value
        snapshot = snapshots.find { |s| s['snapshot_id'] == snapshot_opts[:info] || s['id'] == snapshot_opts[:info] }
        if snapshot
          cli_print_snapshot_info(snapshot)
        else
          $stderr.puts "Error: Snapshot not found: #{snapshot_opts[:info]}"
          exit(EXIT_ERROR)
        end
      elsif snapshot_opts[:delete]
        delete_snapshot(snapshot_opts[:delete], **creds).value
        puts "Snapshot #{snapshot_opts[:delete]} deleted"
      elsif snapshot_opts[:lock]
        lock_snapshot(snapshot_opts[:lock], **creds).value
        puts "Snapshot #{snapshot_opts[:lock]} locked"
      elsif snapshot_opts[:unlock]
        unlock_snapshot(snapshot_opts[:unlock], **creds).value
        puts "Snapshot #{snapshot_opts[:unlock]} unlocked"
      elsif snapshot_opts[:clone]
        result = clone_snapshot(
          snapshot_opts[:clone],
          type: snapshot_opts[:clone_type],
          name: snapshot_opts[:clone_name],
          shell: snapshot_opts[:clone_shell],
          ports: snapshot_opts[:clone_ports],
          **creds
        ).value
        if result['session_id']
          puts "Session created: #{result['session_id']}"
        elsif result['service_id']
          puts "Service created: #{result['service_id']}"
        else
          puts 'Clone completed'
          puts JSON.pretty_generate(result)
        end
      else
        cli_snapshot_help
        exit(EXIT_INVALID_ARGS)
      end
    end

    # Print snapshots table
    def cli_print_snapshots_table(snapshots)
      if snapshots.empty?
        puts 'No snapshots'
        return
      end

      puts format('%-38s %-20s %-10s %-20s', 'ID', 'NAME', 'TYPE', 'CREATED')
      snapshots.each do |s|
        puts format('%-38s %-20s %-10s %-20s',
                    s['snapshot_id'] || s['id'] || '-',
                    s['name'] || '-',
                    s['type'] || s['source_type'] || '-',
                    s['created_at'] || '-')
      end
    end

    # Print snapshot info
    def cli_print_snapshot_info(snapshot)
      puts "ID: #{snapshot['snapshot_id'] || snapshot['id']}"
      puts "Name: #{snapshot['name']}" if snapshot['name']
      puts "Type: #{snapshot['type'] || snapshot['source_type']}"
      puts "Source ID: #{snapshot['source_id']}" if snapshot['source_id']
      puts "Size: #{snapshot['size']}" if snapshot['size']
      puts "Locked: #{snapshot['locked']}" if snapshot.key?('locked')
      puts "Created: #{snapshot['created_at']}" if snapshot['created_at']
    end

    # Snapshot help
    def cli_snapshot_help
      puts <<~HELP
        Snapshot Management

        Usage:
          ruby un_async.rb snapshot [options]

        Options:
          -l, --list             List all snapshots
          --info ID              Get snapshot details
          --delete ID            Delete snapshot
          --lock ID              Prevent deletion
          --unlock ID            Allow deletion
          --clone ID             Clone snapshot
          --type TYPE            Clone type: session or service
          --name NAME            Name for cloned resource
          --shell SHELL          Shell for cloned session
          --ports PORTS          Ports for cloned service

        Examples:
          ruby un_async.rb snapshot --list
          ruby un_async.rb snapshot --clone abc123 --type service --name myapp --ports 80
      HELP
    end

    # Key command
    def cli_key(options)
      parser = parse_global_options(options) do
        puts 'Usage: ruby un_async.rb key [-p PUBLIC_KEY] [-k SECRET_KEY]'
        puts
        puts 'Check API key validity and show account info'
      end
      parser.parse!(ARGV)

      creds = { public_key: options[:public_key], secret_key: options[:secret_key] }

      begin
        result = validate_keys(**creds).value
        puts "Valid: #{result['valid']}"
        puts "Account: #{result['account'] || result['account_id']}" if result['account'] || result['account_id']
        puts "Email: #{result['email']}" if result['email']
        puts "Plan: #{result['plan']}" if result['plan']
        puts "Credits: #{result['credits']}" if result['credits']
      rescue APIError => e
        if e.status_code == 401 || e.status_code == 403
          puts 'Valid: false'
          puts "Error: #{e.message}"
          exit(EXIT_AUTH_ERROR)
        end
        raise
      end
    end
  end
end

# CLI entry point
if __FILE__ == $0
  UnAsync.cli_main
end
