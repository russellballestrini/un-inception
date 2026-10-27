# frozen_string_literal: true

# Target OS option (tickets #123, #124): "os" is sent only when given,
# normalized to lowercase, and invalid values are rejected before any request.

require_relative 'test_helper'
require 'json'

class OsTest < Minitest::Test
  KEYS = { public_key: 'pk-test', secret_key: 'sk-test' }.freeze

  def setup
    WebMock.reset!
    stub_request(:post, "#{Un::API_BASE}/execute")
      .to_return(status: 200, body: { status: 'completed', stdout: '' }.to_json,
                 headers: { 'Content-Type' => 'application/json' })
    stub_request(:post, "#{Un::API_BASE}/sessions")
      .to_return(status: 200, body: { session_id: 's-1' }.to_json,
                 headers: { 'Content-Type' => 'application/json' })
    stub_request(:post, "#{Un::API_BASE}/services")
      .to_return(status: 200, body: { service_id: 'v-1' }.to_json,
                 headers: { 'Content-Type' => 'application/json' })
  end

  def assert_os(path, expected)
    assert_requested(:post, "#{Un::API_BASE}#{path}", times: 1) do |req|
      body = JSON.parse(req.body)
      expected.nil? ? !body.key?('os') : body['os'] == expected
    end
  end

  def test_normalize_os
    assert_equal 'linux', Un.normalize_os('linux')
    assert_equal 'freebsd', Un.normalize_os('FreeBSD')
    assert_raises(ArgumentError) { Un.normalize_os('openbsd') }
    assert_raises(ArgumentError) { Un.normalize_os('') }
  end

  def test_execute_with_os
    Un.execute_code('bash', 'echo hi', os: 'FreeBSD', **KEYS)
    assert_os('/execute', 'freebsd')
  end

  def test_execute_without_os
    Un.execute_code('bash', 'echo hi', **KEYS)
    assert_os('/execute', nil)
  end

  def test_execute_async_with_os
    Un.execute_async('bash', 'echo hi', os: 'linux', **KEYS)
    assert_os('/execute', 'linux')
  end

  def test_session_with_and_without_os
    Un.create_session('bash', os: 'freebsd', **KEYS)
    assert_os('/sessions', 'freebsd')
    WebMock::RequestRegistry.instance.reset!
    Un.create_session('bash', **KEYS)
    assert_os('/sessions', nil)
  end

  def test_service_with_and_without_os
    Un.create_service('ostest', [80], 'true', os: 'freebsd', **KEYS)
    assert_os('/services', 'freebsd')
    WebMock::RequestRegistry.instance.reset!
    Un.create_service('ostest', [80], 'true', **KEYS)
    assert_os('/services', nil)
  end

  def test_invalid_os_sends_nothing
    assert_raises(ArgumentError) { Un.execute_code('bash', 'echo hi', os: 'openbsd', **KEYS) }
    assert_raises(ArgumentError) { Un.execute_async('bash', 'echo hi', os: '', **KEYS) }
    assert_raises(ArgumentError) { Un.create_session('bash', os: 'windows', **KEYS) }
    assert_raises(ArgumentError) { Un.create_service('ostest', [80], 'true', os: 'openbsd', **KEYS) }
    assert_not_requested :any, /.*/
  end
end
