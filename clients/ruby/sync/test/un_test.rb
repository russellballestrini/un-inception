# frozen_string_literal: true

require_relative 'test_helper'
require 'json'
require 'openssl'

class UnTest < Minitest::Test
  API_BASE = 'https://api.unsandbox.com'

  def setup
    WebMock.reset!
    # Clear environment variables
    ENV.delete('UNSANDBOX_PUBLIC_KEY')
    ENV.delete('UNSANDBOX_SECRET_KEY')
    ENV.delete('UNSANDBOX_ACCOUNT')
  end

  # =========================================================================
  # Credential Resolution Tests
  # =========================================================================

  def test_credentials_from_method_arguments
    stub_execute_request(status: 'completed', stdout: 'hello')

    result = Un.execute_code(
      'python',
      'print("hello")',
      public_key: 'pk-test',
      secret_key: 'sk-test'
    )

    assert_equal 'completed', result['status']
    assert_equal 'hello', result['stdout']
  end

  def test_credentials_from_environment
    ENV['UNSANDBOX_PUBLIC_KEY'] = 'env-pk'
    ENV['UNSANDBOX_SECRET_KEY'] = 'env-sk'

    stub_execute_request(status: 'completed', stdout: 'env-test')

    result = Un.execute_code('python', 'print("env")')

    assert_equal 'completed', result['status']
    assert_requested :post, "#{API_BASE}/execute" do |req|
      req.headers['Authorization'] == 'Bearer env-pk'
    end
  end

  def test_credentials_error_when_none_found
    error = assert_raises(Un::CredentialsError) do
      Un.execute_code('python', 'print("test")')
    end

    assert_match(/No credentials found/, error.message)
  end

  # =========================================================================
  # HMAC-SHA256 Signature Tests
  # =========================================================================

  def test_signature_format
    ENV['UNSANDBOX_PUBLIC_KEY'] = 'pk-sig'
    ENV['UNSANDBOX_SECRET_KEY'] = 'sk-sig-secret'

    captured_signature = nil
    captured_timestamp = nil

    stub_request(:post, "#{API_BASE}/execute")
      .with { |req|
        captured_signature = req.headers['X-Signature']
        captured_timestamp = req.headers['X-Timestamp']
        true
      }
      .to_return(
        status: 200,
        body: JSON.generate(status: 'completed', stdout: 'test'),
        headers: { 'Content-Type' => 'application/json' }
      )

    Un.execute_code('python', 'print("test")')

    # Verify signature is 64-char hex
    assert_match(/\A[0-9a-f]{64}\z/, captured_signature)

    # Verify timestamp is numeric
    assert_match(/\A\d+\z/, captured_timestamp)

    # Verify signature is correct HMAC-SHA256
    body = JSON.generate(language: 'python', code: 'print("test")')
    message = "#{captured_timestamp}:POST:/execute:#{body}"
    expected_sig = OpenSSL::HMAC.hexdigest('SHA256', 'sk-sig-secret', message)
    assert_equal expected_sig, captured_signature
  end

  def test_signature_with_empty_body_for_get
    ENV['UNSANDBOX_PUBLIC_KEY'] = 'pk-get'
    ENV['UNSANDBOX_SECRET_KEY'] = 'sk-get-secret'

    captured_signature = nil
    captured_timestamp = nil

    stub_request(:get, "#{API_BASE}/languages")
      .with { |req|
        captured_signature = req.headers['X-Signature']
        captured_timestamp = req.headers['X-Timestamp']
        true
      }
      .to_return(
        status: 200,
        body: JSON.generate(languages: %w[python javascript]),
        headers: { 'Content-Type' => 'application/json' }
      )

    # Clear language cache to force API call
    cache_path = File.join(Dir.home, '.unsandbox', 'languages.json')
    File.delete(cache_path) if File.exist?(cache_path)

    Un.get_languages

    # Verify signature for GET (empty body)
    message = "#{captured_timestamp}:GET:/languages:"
    expected_sig = OpenSSL::HMAC.hexdigest('SHA256', 'sk-get-secret', message)
    assert_equal expected_sig, captured_signature
  end

  # =========================================================================
  # Execute Code Tests
  # =========================================================================

  def test_execute_code_sync_returns_immediately_on_completed
    stub_execute_request(status: 'completed', stdout: 'hello world')

    result = Un.execute_code(
      'python',
      'print("hello world")',
      public_key: 'pk',
      secret_key: 'sk'
    )

    assert_equal 'completed', result['status']
    assert_equal 'hello world', result['stdout']
  end

  def test_execute_code_polls_when_pending
    # First request returns pending with job_id
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 200,
        body: JSON.generate(status: 'pending', job_id: 'job-123'),
        headers: { 'Content-Type' => 'application/json' }
      )

    # Subsequent polls return completed
    stub_request(:get, "#{API_BASE}/jobs/job-123")
      .to_return(
        status: 200,
        body: JSON.generate(status: 'completed', stdout: 'polled result'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.execute_code('python', 'print("test")', public_key: 'pk', secret_key: 'sk')

    assert_equal 'completed', result['status']
    assert_equal 'polled result', result['stdout']
  end

  # =========================================================================
  # Execute Async Tests
  # =========================================================================

  def test_execute_async_returns_job_id
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 200,
        body: JSON.generate(job_id: 'async-job-456', status: 'pending'),
        headers: { 'Content-Type' => 'application/json' }
      )

    job_id = Un.execute_async('javascript', 'console.log("async")', public_key: 'pk', secret_key: 'sk')

    assert_equal 'async-job-456', job_id
  end

  # =========================================================================
  # Job Management Tests
  # =========================================================================

  def test_get_job
    stub_request(:get, "#{API_BASE}/jobs/job-789")
      .to_return(
        status: 200,
        body: JSON.generate(job_id: 'job-789', status: 'running'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.get_job('job-789', public_key: 'pk', secret_key: 'sk')

    assert_equal 'job-789', result['job_id']
    assert_equal 'running', result['status']
  end

  def test_list_jobs
    stub_request(:get, "#{API_BASE}/jobs")
      .to_return(
        status: 200,
        body: JSON.generate(jobs: [
                              { job_id: 'j1', status: 'completed' },
                              { job_id: 'j2', status: 'running' }
                            ]),
        headers: { 'Content-Type' => 'application/json' }
      )

    jobs = Un.list_jobs(public_key: 'pk', secret_key: 'sk')

    assert_equal 2, jobs.length
    assert_equal 'j1', jobs[0]['job_id']
  end

  def test_cancel_job
    stub_request(:delete, "#{API_BASE}/jobs/cancel-me")
      .to_return(
        status: 200,
        body: JSON.generate(status: 'cancelled'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.cancel_job('cancel-me', public_key: 'pk', secret_key: 'sk')

    assert_equal 'cancelled', result['status']
  end

  # =========================================================================
  # Wait for Job Tests
  # =========================================================================

  def test_wait_for_job_returns_on_completed
    stub_request(:get, "#{API_BASE}/jobs/wait-job")
      .to_return(
        status: 200,
        body: JSON.generate(status: 'completed', stdout: 'done'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.wait_for_job('wait-job', public_key: 'pk', secret_key: 'sk')

    assert_equal 'completed', result['status']
    assert_equal 'done', result['stdout']
  end

  def test_wait_for_job_returns_on_failed
    stub_request(:get, "#{API_BASE}/jobs/fail-job")
      .to_return(
        status: 200,
        body: JSON.generate(status: 'failed', error: 'execution error'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.wait_for_job('fail-job', public_key: 'pk', secret_key: 'sk')

    assert_equal 'failed', result['status']
  end

  def test_wait_for_job_polls_until_done
    call_count = 0
    stub_request(:get, "#{API_BASE}/jobs/poll-job")
      .to_return do |_request|
        call_count += 1
        if call_count < 3
          {
            status: 200,
            body: JSON.generate(status: 'running'),
            headers: { 'Content-Type' => 'application/json' }
          }
        else
          {
            status: 200,
            body: JSON.generate(status: 'completed', stdout: 'finally'),
            headers: { 'Content-Type' => 'application/json' }
          }
        end
      end

    result = Un.wait_for_job('poll-job', public_key: 'pk', secret_key: 'sk')

    assert_equal 'completed', result['status']
    assert_equal 'finally', result['stdout']
    assert_equal 3, call_count
  end

  # =========================================================================
  # Language Detection Tests
  # =========================================================================

  def test_detect_language_python
    assert_equal 'python', Un.detect_language('script.py')
  end

  def test_detect_language_javascript
    assert_equal 'javascript', Un.detect_language('app.js')
  end

  def test_detect_language_typescript
    assert_equal 'typescript', Un.detect_language('index.ts')
  end

  def test_detect_language_ruby
    assert_equal 'ruby', Un.detect_language('file.rb')
  end

  def test_detect_language_go
    assert_equal 'go', Un.detect_language('main.go')
  end

  def test_detect_language_rust
    assert_equal 'rust', Un.detect_language('lib.rs')
  end

  def test_detect_language_cpp_variants
    assert_equal 'cpp', Un.detect_language('file.cpp')
    assert_equal 'cpp', Un.detect_language('file.cc')
    assert_equal 'cpp', Un.detect_language('file.cxx')
  end

  def test_detect_language_unknown
    assert_nil Un.detect_language('unknown.xyz')
    assert_nil Un.detect_language('noextension')
    assert_nil Un.detect_language(nil)
  end

  # =========================================================================
  # Languages API Tests
  # =========================================================================

  def test_get_languages
    # Clear cache first
    cache_path = File.join(Dir.home, '.unsandbox', 'languages.json')
    File.delete(cache_path) if File.exist?(cache_path)

    stub_request(:get, "#{API_BASE}/languages")
      .to_return(
        status: 200,
        body: JSON.generate(languages: %w[python javascript ruby go]),
        headers: { 'Content-Type' => 'application/json' }
      )

    languages = Un.get_languages(public_key: 'pk', secret_key: 'sk')

    assert_includes languages, 'python'
    assert_includes languages, 'javascript'
    assert_equal 4, languages.length

    # Clean up
    File.delete(cache_path) if File.exist?(cache_path)
  end

  # =========================================================================
  # Snapshot Tests
  # =========================================================================

  def test_session_snapshot
    stub_request(:post, "#{API_BASE}/snapshots")
      .to_return(
        status: 200,
        body: JSON.generate(snapshot_id: 'snap-session-123'),
        headers: { 'Content-Type' => 'application/json' }
      )

    snapshot_id = Un.session_snapshot('sess-456', public_key: 'pk', secret_key: 'sk')

    assert_equal 'snap-session-123', snapshot_id
    assert_requested :post, "#{API_BASE}/snapshots" do |req|
      body = JSON.parse(req.body)
      body['session_id'] == 'sess-456' && body['hot'] == false
    end
  end

  def test_session_snapshot_ephemeral
    stub_request(:post, "#{API_BASE}/snapshots")
      .to_return(
        status: 200,
        body: JSON.generate(snapshot_id: 'snap-ephemeral'),
        headers: { 'Content-Type' => 'application/json' }
      )

    Un.session_snapshot('sess-789', public_key: 'pk', secret_key: 'sk', ephemeral: true)

    assert_requested :post, "#{API_BASE}/snapshots" do |req|
      body = JSON.parse(req.body)
      body['hot'] == true
    end
  end

  def test_service_snapshot
    stub_request(:post, "#{API_BASE}/snapshots")
      .to_return(
        status: 200,
        body: JSON.generate(snapshot_id: 'snap-service-456'),
        headers: { 'Content-Type' => 'application/json' }
      )

    snapshot_id = Un.service_snapshot('svc-123', public_key: 'pk', secret_key: 'sk', name: 'backup')

    assert_equal 'snap-service-456', snapshot_id
    assert_requested :post, "#{API_BASE}/snapshots" do |req|
      body = JSON.parse(req.body)
      body['service_id'] == 'svc-123' && body['name'] == 'backup'
    end
  end

  def test_list_snapshots
    stub_request(:get, "#{API_BASE}/snapshots")
      .to_return(
        status: 200,
        body: JSON.generate(snapshots: [
                              { snapshot_id: 's1', name: 'first' },
                              { snapshot_id: 's2', name: 'second' }
                            ]),
        headers: { 'Content-Type' => 'application/json' }
      )

    snapshots = Un.list_snapshots(public_key: 'pk', secret_key: 'sk')

    assert_equal 2, snapshots.length
  end

  def test_restore_snapshot
    stub_request(:post, "#{API_BASE}/snapshots/snap-restore/restore")
      .to_return(
        status: 200,
        body: JSON.generate(session_id: 'new-session'),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.restore_snapshot('snap-restore', public_key: 'pk', secret_key: 'sk')

    assert_equal 'new-session', result['session_id']
  end

  def test_delete_snapshot
    stub_request(:delete, "#{API_BASE}/snapshots/snap-delete")
      .to_return(
        status: 200,
        body: JSON.generate(deleted: true),
        headers: { 'Content-Type' => 'application/json' }
      )

    result = Un.delete_snapshot('snap-delete', public_key: 'pk', secret_key: 'sk')

    assert result['deleted']
  end

  # =========================================================================
  # Error Handling Tests
  # =========================================================================

  def test_api_error_on_401
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 401,
        body: JSON.generate(error: 'unauthorized'),
        headers: { 'Content-Type' => 'application/json' }
      )

    error = assert_raises(Un::APIError) do
      Un.execute_code('python', 'print()', public_key: 'bad', secret_key: 'creds')
    end

    assert_equal 401, error.status_code
    assert_match(/401/, error.message)
  end

  def test_api_error_on_429
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 429,
        body: JSON.generate(error: 'rate_limit_exceeded'),
        headers: { 'Content-Type' => 'application/json' }
      )

    error = assert_raises(Un::APIError) do
      Un.execute_code('python', 'print()', public_key: 'pk', secret_key: 'sk')
    end

    assert_equal 429, error.status_code
  end

  def test_api_error_on_500
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 500,
        body: 'Internal Server Error',
        headers: { 'Content-Type' => 'text/plain' }
      )

    error = assert_raises(Un::APIError) do
      Un.execute_code('python', 'print()', public_key: 'pk', secret_key: 'sk')
    end

    assert_equal 500, error.status_code
  end

  private

  def stub_execute_request(status:, stdout: '', stderr: '', exit_code: 0)
    stub_request(:post, "#{API_BASE}/execute")
      .to_return(
        status: 200,
        body: JSON.generate(
          status: status,
          stdout: stdout,
          stderr: stderr,
          exit_code: exit_code
        ),
        headers: { 'Content-Type' => 'application/json' }
      )
  end
end
