# frozen_string_literal: true

# PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
#
# unsandbox.com Ruby SDK (Synchronous)
#
# Library Usage:
#     require_relative 'un'
#
#     # Execute code synchronously
#     result = Un.execute_code("python", 'print("hello")')
#
#     # Execute asynchronously and get job_id
#     job_id = Un.execute_async("javascript", 'console.log("hello")')
#
#     # Wait for job completion with exponential backoff
#     result = Un.wait_for_job(job_id)
#
#     # List all jobs
#     jobs = Un.list_jobs
#
#     # Get supported languages (cached for 1 hour)
#     languages = Un.get_languages
#
#     # Snapshot operations
#     snapshot_id = Un.session_snapshot(session_id)
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

require 'net/http'
require 'uri'
require 'json'
require 'openssl'
require 'fileutils'

# Unsandbox Ruby SDK module (synchronous)
module Un
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

  class << self
    # Execute code synchronously (blocks until completion)
    #
    # @param language [String] Programming language (e.g., "python", "javascript", "go")
    # @param code [String] Source code to execute
    # @param public_key [String, nil] Optional API key (uses credentials resolution if not provided)
    # @param secret_key [String, nil] Optional API secret (uses credentials resolution if not provided)
    # @return [Hash] Response hash containing stdout, stderr, exit code, etc.
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.execute_code("python", 'print("hello")')
    #     puts result["stdout"]  # => "hello\n"
    def execute_code(language, code, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request(
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
        return wait_for_job(job_id, public_key: pk, secret_key: sk)
      end

      response
    end

    # Execute code asynchronously (returns immediately with job_id)
    #
    # @param language [String] Programming language (e.g., "python", "javascript")
    # @param code [String] Source code to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [String] Job ID string
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     job_id = Un.execute_async("python", 'import time; time.sleep(10); print("done")')
    #     # Later...
    #     result = Un.wait_for_job(job_id)
    def execute_async(language, code, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request(
        'POST',
        '/execute',
        pk,
        sk,
        { language: language, code: code }
      )
      response['job_id']
    end

    # Get current status/result of a job (single poll, no waiting)
    #
    # @param job_id [String] Job ID from execute_async
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Job response hash
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     job = Un.get_job(job_id)
    #     puts job["status"]  # => "running" or "completed"
    def get_job(job_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('GET', "/jobs/#{job_id}", pk, sk)
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
    # @return [Hash] Final job result when status is terminal (completed, failed, timeout, cancelled)
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails or timeout reached
    #
    # @example
    #     result = Un.wait_for_job(job_id, timeout: 60)
    #     puts result["stdout"]
    def wait_for_job(job_id, public_key: nil, secret_key: nil, timeout: 3600)
      pk, sk = resolve_credentials(public_key, secret_key)
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

        response = get_job(job_id, public_key: pk, secret_key: sk)
        status = response['status']

        return response if %w[completed failed timeout cancelled].include?(status)

        # Still running, continue polling
      end
    end

    # Cancel a running job
    #
    # @param job_id [String] Job ID to cancel
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with cancellation confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.cancel_job(job_id)
    def cancel_job(job_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('DELETE', "/jobs/#{job_id}", pk, sk)
    end

    # List all jobs for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Array<Hash>] List of job hashes
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     jobs = Un.list_jobs
    #     jobs.each { |job| puts "#{job['job_id']}: #{job['status']}" }
    def list_jobs(public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request('GET', '/jobs', pk, sk)
      response['jobs'] || []
    end

    # Get list of supported programming languages
    #
    # Results are cached for 1 hour in ~/.unsandbox/languages.json
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Array<String>] List of language identifiers (e.g., ["python", "javascript", "go", ...])
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     languages = Un.get_languages
    #     puts languages.join(", ")
    def get_languages(public_key: nil, secret_key: nil)
      # Try cache first
      cached = load_languages_cache
      return cached if cached

      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request('GET', '/languages', pk, sk)
      languages = response['languages'] || []

      # Cache the result
      save_languages_cache(languages)
      languages
    end

    # Detect programming language from filename extension
    #
    # @param filename [String] Filename to detect language from (e.g., "script.py")
    # @return [String, nil] Language identifier (e.g., "python") or nil if unknown
    #
    # @example
    #     Un.detect_language("hello.py")   # => "python"
    #     Un.detect_language("script.js")  # => "javascript"
    #     Un.detect_language("main.go")    # => "go"
    #     Un.detect_language("unknown")    # => nil
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
    # @return [String] Snapshot ID
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     snapshot_id = Un.session_snapshot(session_id, name: "my-snapshot")
    def session_snapshot(session_id, public_key: nil, secret_key: nil, name: nil, ephemeral: false)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = { session_id: session_id, hot: ephemeral }
      data[:name] = name if name

      response = make_request('POST', '/snapshots', pk, sk, data)
      response['snapshot_id']
    end

    # Create a snapshot of a service
    #
    # @param service_id [String] Service ID to snapshot
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param name [String, nil] Optional snapshot name
    # @return [String] Snapshot ID
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     snapshot_id = Un.service_snapshot(service_id, name: "production-backup")
    def service_snapshot(service_id, public_key: nil, secret_key: nil, name: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = { service_id: service_id, hot: false }
      data[:name] = name if name

      response = make_request('POST', '/snapshots', pk, sk, data)
      response['snapshot_id']
    end

    # List all snapshots
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Array<Hash>] List of snapshot hashes
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     snapshots = Un.list_snapshots
    #     snapshots.each { |s| puts "#{s['snapshot_id']}: #{s['name']}" }
    def list_snapshots(public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request('GET', '/snapshots', pk, sk)
      response['snapshots'] || []
    end

    # Restore a snapshot
    #
    # @param snapshot_id [String] Snapshot ID to restore
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with restored resource info
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.restore_snapshot(snapshot_id)
    #     puts result["session_id"]  # or result["service_id"]
    def restore_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/snapshots/#{snapshot_id}/restore", pk, sk, {})
    end

    # Delete a snapshot
    #
    # @param snapshot_id [String] Snapshot ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.delete_snapshot(snapshot_id)
    def delete_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('DELETE', "/snapshots/#{snapshot_id}", pk, sk)
    end

    # Lock a snapshot to prevent deletion
    #
    # @param snapshot_id [String] Snapshot ID to lock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with lock confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.lock_snapshot(snapshot_id)
    def lock_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/snapshots/#{snapshot_id}/lock", pk, sk, {})
    end

    # Unlock a snapshot to allow deletion
    #
    # @param snapshot_id [String] Snapshot ID to unlock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with unlock confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.unlock_snapshot(snapshot_id)
    def unlock_snapshot(snapshot_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/snapshots/#{snapshot_id}/unlock", pk, sk, {})
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
    # @return [Hash] Response hash with cloned resource info (session_id or service_id)
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example Clone to session
    #     result = Un.clone_snapshot(snapshot_id, type: "session")
    #     puts result["session_id"]
    #
    # @example Clone to service
    #     result = Un.clone_snapshot(snapshot_id, type: "service", ports: [80, 443])
    #     puts result["service_id"]
    def clone_snapshot(snapshot_id, public_key: nil, secret_key: nil, name: nil, type: 'session', shell: nil, ports: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = { type: type }
      data[:name] = name if name
      data[:shell] = shell if shell
      data[:ports] = ports if ports
      make_request('POST', "/snapshots/#{snapshot_id}/clone", pk, sk, data)
    end

    # ============================================================================
    # Session Functions
    # ============================================================================

    # List all sessions for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Array<Hash>] List of session hashes
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     sessions = Un.list_sessions
    #     sessions.each { |s| puts "#{s['id']}: #{s['status']}" }
    def list_sessions(public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request('GET', '/sessions', pk, sk)
      response['sessions'] || []
    end

    # Get session details by ID
    #
    # @param session_id [String] Session ID to retrieve
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Session details hash
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     session = Un.get_session(session_id)
    #     puts session["status"]
    def get_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('GET', "/sessions/#{session_id}", pk, sk)
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
    # @return [Hash] Response hash with session_id and container_name
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.create_session("bash", network_mode: "semitrusted")
    #     puts result["session_id"]
    def create_session(language, public_key: nil, secret_key: nil, network_mode: 'zerotrust', ttl: 3600, multiplexer: nil, vcpu: 1)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = {
        network_mode: network_mode,
        ttl: ttl
      }
      data[:shell] = language if language
      data[:multiplexer] = multiplexer if multiplexer
      data[:vcpu] = vcpu if vcpu > 1
      make_request('POST', '/sessions', pk, sk, data)
    end

    # Delete (terminate) a session
    #
    # @param session_id [String] Session ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.delete_session(session_id)
    def delete_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('DELETE', "/sessions/#{session_id}", pk, sk)
    end

    # Freeze a session (pause execution, reduce resource usage)
    #
    # @param session_id [String] Session ID to freeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with freeze confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.freeze_session(session_id)
    def freeze_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/sessions/#{session_id}/freeze", pk, sk, {})
    end

    # Unfreeze a session (resume execution)
    #
    # @param session_id [String] Session ID to unfreeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with unfreeze confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.unfreeze_session(session_id)
    def unfreeze_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/sessions/#{session_id}/unfreeze", pk, sk, {})
    end

    # Boost a session (increase vCPU allocation)
    #
    # @param session_id [String] Session ID to boost
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with boost confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.boost_session(session_id)
    def boost_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/sessions/#{session_id}/boost", pk, sk, {})
    end

    # Unboost a session (reduce vCPU allocation)
    #
    # @param session_id [String] Session ID to unboost
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with unboost confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.unboost_session(session_id)
    def unboost_session(session_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/sessions/#{session_id}/unboost", pk, sk, {})
    end

    # Execute a shell command in an existing session
    # Note: This is for non-interactive command execution, not for WebSocket shell access
    #
    # @param session_id [String] Session ID to execute command in
    # @param command [String] Command to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with command output
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.shell_session(session_id, "ls -la")
    #     puts result["stdout"]
    def shell_session(session_id, command, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/sessions/#{session_id}/shell", pk, sk, { command: command })
    end

    # ============================================================================
    # Service Functions
    # ============================================================================

    # List all services for the authenticated account
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Array<Hash>] List of service hashes
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     services = Un.list_services
    #     services.each { |s| puts "#{s['id']}: #{s['name']} (#{s['state']})" }
    def list_services(public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      response = make_request('GET', '/services', pk, sk)
      response['services'] || []
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
    # @param service_type [String, nil] Service type for SRV records (e.g., "minecraft")
    # @return [Hash] Response hash with service_id
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.create_service("web", [80, 443], "apt install -y nginx && nginx")
    #     puts result["service_id"]
    def create_service(name, ports, bootstrap, public_key: nil, secret_key: nil, network_mode: 'semitrusted', vcpu: 1, custom_domains: nil, service_type: nil)
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
      make_request('POST', '/services', pk, sk, data)
    end

    # Get service details by ID
    #
    # @param service_id [String] Service ID to retrieve
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Service details hash
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     service = Un.get_service(service_id)
    #     puts service["status"]
    def get_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('GET', "/services/#{service_id}", pk, sk)
    end

    # Update a service (e.g., resize vCPU)
    #
    # @param service_id [String] Service ID to update
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param vcpu [Integer, nil] New vCPU count (1-8)
    # @param name [String, nil] New service name
    # @return [Hash] Response hash with update confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.update_service(service_id, vcpu: 4)
    def update_service(service_id, public_key: nil, secret_key: nil, vcpu: nil, name: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = {}
      data[:vcpu] = vcpu if vcpu
      data[:name] = name if name
      make_request('PATCH', "/services/#{service_id}", pk, sk, data)
    end

    # Delete (destroy) a service
    #
    # @param service_id [String] Service ID to delete
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.delete_service(service_id)
    def delete_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('DELETE', "/services/#{service_id}", pk, sk)
    end

    # Freeze a service (stop container, reduce resource usage)
    #
    # @param service_id [String] Service ID to freeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with freeze confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.freeze_service(service_id)
    def freeze_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/services/#{service_id}/freeze", pk, sk, {})
    end

    # Unfreeze a service (start container)
    #
    # @param service_id [String] Service ID to unfreeze
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with unfreeze confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.unfreeze_service(service_id)
    def unfreeze_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/services/#{service_id}/unfreeze", pk, sk, {})
    end

    # Lock a service to prevent deletion
    #
    # @param service_id [String] Service ID to lock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with lock confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.lock_service(service_id)
    def lock_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/services/#{service_id}/lock", pk, sk, {})
    end

    # Unlock a service to allow deletion
    #
    # @param service_id [String] Service ID to unlock
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with unlock confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.unlock_service(service_id)
    def unlock_service(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/services/#{service_id}/unlock", pk, sk, {})
    end

    # Get service logs (bootstrap output)
    #
    # @param service_id [String] Service ID to get logs for
    # @param all [Boolean] If true, get all logs; if false, get last 9000 lines (default: false)
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with log content
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     logs = Un.get_service_logs(service_id)
    #     puts logs["log"]
    def get_service_logs(service_id, all: false, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      path = "/services/#{service_id}/logs"
      path += '?all=true' if all
      make_request('GET', path, pk, sk)
    end

    # Get service environment vault status
    #
    # @param service_id [String] Service ID to get env status for
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with vault status (has_vault, count, updated_at)
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     status = Un.get_service_env(service_id)
    #     puts "Variables: #{status['count']}"
    def get_service_env(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('GET', "/services/#{service_id}/env", pk, sk)
    end

    # Set service environment variables (replaces existing vault)
    #
    # @param service_id [String] Service ID to set env for
    # @param env [String] Environment content in .env format (KEY=VALUE per line)
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.set_service_env(service_id, "API_KEY=secret\nDEBUG=true")
    def set_service_env(service_id, env, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      # This endpoint uses text/plain content type and PUT method
      make_request_text('PUT', "/services/#{service_id}/env", pk, sk, env)
    end

    # Delete service environment vault
    #
    # @param service_id [String] Service ID to delete env for
    # @param keys [Array<String>, nil] Specific keys to delete (nil = delete entire vault)
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with deletion confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example Delete entire vault
    #     Un.delete_service_env(service_id)
    #
    # @example Delete specific keys (if API supports it)
    #     Un.delete_service_env(service_id, keys: ["API_KEY", "DEBUG"])
    def delete_service_env(service_id, keys: nil, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      if keys
        make_request('DELETE', "/services/#{service_id}/env", pk, sk, { keys: keys })
      else
        make_request('DELETE', "/services/#{service_id}/env", pk, sk)
      end
    end

    # Export service environment vault (returns .env format)
    #
    # @param service_id [String] Service ID to export env from
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with env content
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.export_service_env(service_id)
    #     puts result["env"]  # API_KEY=secret\nDEBUG=true
    def export_service_env(service_id, public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      make_request('POST', "/services/#{service_id}/env/export", pk, sk, {})
    end

    # Redeploy a service (re-run bootstrap script)
    #
    # @param service_id [String] Service ID to redeploy
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param bootstrap [String, nil] New bootstrap script (optional)
    # @return [Hash] Response hash with redeploy confirmation
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     Un.redeploy_service(service_id)
    def redeploy_service(service_id, public_key: nil, secret_key: nil, bootstrap: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      data = {}
      data[:bootstrap] = bootstrap if bootstrap
      make_request('POST', "/services/#{service_id}/redeploy", pk, sk, data)
    end

    # Execute a command in a running service
    #
    # @param service_id [String] Service ID to execute command in
    # @param command [String] Command to execute
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @param timeout [Integer] Command timeout in milliseconds (default: 30000)
    # @return [Hash] Response hash with command output (stdout, stderr, exit_code)
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails
    #
    # @example
    #     result = Un.execute_in_service(service_id, "ls -la")
    #     puts result["stdout"]
    def execute_in_service(service_id, command, public_key: nil, secret_key: nil, timeout: 30_000)
      pk, sk = resolve_credentials(public_key, secret_key)

      # Start async execution
      response = make_request('POST', "/services/#{service_id}/execute", pk, sk, {
                                command: command,
                                timeout: timeout
                              })

      job_id = response['job_id']
      return response unless job_id

      # Poll for completion
      wait_for_job(job_id, public_key: pk, secret_key: sk, timeout: (timeout / 1000) + 10)
    end

    # ============================================================================
    # Key Validation
    # ============================================================================

    # Validate API keys
    #
    # @param public_key [String, nil] Optional API key
    # @param secret_key [String, nil] Optional API secret
    # @return [Hash] Response hash with validation result and account info
    # @raise [CredentialsError] If no credentials found
    # @raise [APIError] If API request fails or keys invalid
    #
    # @example
    #     result = Un.validate_keys
    #     puts result["valid"]  # true or false
    def validate_keys(public_key: nil, secret_key: nil)
      pk, sk = resolve_credentials(public_key, secret_key)
      # Note: This endpoint is on the portal, not API, but we use same auth
      make_request('POST', '/keys/validate', pk, sk, {})
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

    # Make an authenticated HTTP request to the API
    #
    # @param method [String] HTTP method (GET, POST, DELETE)
    # @param path [String] API path
    # @param public_key [String] API public key
    # @param secret_key [String] API secret key
    # @param data [Hash, nil] Request body data
    # @return [Hash] Parsed JSON response
    # @raise [APIError] If request fails
    def make_request(method, path, public_key, secret_key, data = nil)
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

    # Make an authenticated HTTP request with text/plain content type
    #
    # @param method [String] HTTP method (PUT)
    # @param path [String] API path
    # @param public_key [String] API public key
    # @param secret_key [String] API secret key
    # @param body [String] Plain text request body
    # @return [Hash] Parsed JSON response
    # @raise [APIError] If request fails
    def make_request_text(method, path, public_key, secret_key, body)
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
