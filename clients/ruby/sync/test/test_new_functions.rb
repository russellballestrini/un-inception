# frozen_string_literal: true

require_relative 'test_helper'

class TestNewFunctions < Minitest::Test
  # =========================================================================
  # Utility Functions Tests
  # =========================================================================

  def test_version_returns_string
    v = Un.version
    assert_instance_of String, v
    refute_empty v
  end

  def test_version_is_semantic_format
    v = Un.version
    parts = v.split('.')
    assert parts.length >= 2, "Version should be semantic: #{v}"
  end

  def test_hmac_sign_returns_64_hex_chars
    signature = Un.hmac_sign('secret_key', 'message_to_sign')
    assert_instance_of String, signature
    assert_equal 64, signature.length
    assert_match(/\A[0-9a-f]+\z/, signature)
  end

  def test_hmac_sign_is_deterministic
    sig1 = Un.hmac_sign('secret', 'message')
    sig2 = Un.hmac_sign('secret', 'message')
    assert_equal sig1, sig2
  end

  def test_hmac_sign_different_secrets_different_signatures
    sig1 = Un.hmac_sign('secret1', 'message')
    sig2 = Un.hmac_sign('secret2', 'message')
    refute_equal sig1, sig2
  end

  def test_hmac_sign_different_messages_different_signatures
    sig1 = Un.hmac_sign('secret', 'message1')
    sig2 = Un.hmac_sign('secret', 'message2')
    refute_equal sig1, sig2
  end

  def test_hmac_sign_matches_openssl
    secret = 'test_secret'
    message = '1234567890:POST:/execute:'
    signature = Un.hmac_sign(secret, message)
    expected = OpenSSL::HMAC.hexdigest('SHA256', secret, message)
    assert_equal expected, signature
  end

  def test_last_error_returns_nil_or_string
    error = Un.last_error
    assert error.nil? || error.is_a?(String)
  end

  # =========================================================================
  # Function Exports Tests
  # =========================================================================

  def test_execution_functions_defined
    # 8 execution functions
    assert Un.respond_to?(:execute_code)
    assert Un.respond_to?(:execute_async)
    assert Un.respond_to?(:get_job)
    assert Un.respond_to?(:wait_for_job)
    assert Un.respond_to?(:cancel_job)
    assert Un.respond_to?(:list_jobs)
    assert Un.respond_to?(:get_languages)
    assert Un.respond_to?(:detect_language)
  end

  def test_session_functions_defined
    # 9 session functions
    assert Un.respond_to?(:list_sessions)
    assert Un.respond_to?(:get_session)
    assert Un.respond_to?(:create_session)
    assert Un.respond_to?(:delete_session)
    assert Un.respond_to?(:freeze_session)
    assert Un.respond_to?(:unfreeze_session)
    assert Un.respond_to?(:boost_session)
    assert Un.respond_to?(:unboost_session)
    assert Un.respond_to?(:shell_session)
  end

  def test_service_functions_defined
    # 17 service functions
    assert Un.respond_to?(:list_services)
    assert Un.respond_to?(:create_service)
    assert Un.respond_to?(:get_service)
    assert Un.respond_to?(:update_service)
    assert Un.respond_to?(:delete_service)
    assert Un.respond_to?(:freeze_service)
    assert Un.respond_to?(:unfreeze_service)
    assert Un.respond_to?(:lock_service)
    assert Un.respond_to?(:unlock_service)
    assert Un.respond_to?(:set_unfreeze_on_demand)
    assert Un.respond_to?(:get_service_logs)
    assert Un.respond_to?(:get_service_env)
    assert Un.respond_to?(:set_service_env)
    assert Un.respond_to?(:delete_service_env)
    assert Un.respond_to?(:export_service_env)
    assert Un.respond_to?(:redeploy_service)
    assert Un.respond_to?(:execute_in_service)
    assert Un.respond_to?(:resize_service), 'resize_service should be defined (NEW)'
  end

  def test_snapshot_functions_defined
    # 9 snapshot functions
    assert Un.respond_to?(:session_snapshot)
    assert Un.respond_to?(:service_snapshot)
    assert Un.respond_to?(:list_snapshots)
    assert Un.respond_to?(:get_snapshot), 'get_snapshot should be defined (NEW)'
    assert Un.respond_to?(:restore_snapshot)
    assert Un.respond_to?(:delete_snapshot)
    assert Un.respond_to?(:lock_snapshot)
    assert Un.respond_to?(:unlock_snapshot)
    assert Un.respond_to?(:clone_snapshot)
  end

  def test_image_functions_defined
    # 13 image functions
    assert Un.respond_to?(:image_publish)
    assert Un.respond_to?(:list_images)
    assert Un.respond_to?(:get_image)
    assert Un.respond_to?(:delete_image)
    assert Un.respond_to?(:lock_image)
    assert Un.respond_to?(:unlock_image)
    assert Un.respond_to?(:set_image_visibility)
    assert Un.respond_to?(:grant_image_access)
    assert Un.respond_to?(:revoke_image_access)
    assert Un.respond_to?(:list_image_trusted)
    assert Un.respond_to?(:transfer_image)
    assert Un.respond_to?(:spawn_from_image)
    assert Un.respond_to?(:clone_image)
  end

  def test_logs_functions_defined
    # 2 PaaS logs functions (NEW)
    assert Un.respond_to?(:logs_fetch), 'logs_fetch should be defined (NEW)'
    assert Un.respond_to?(:logs_stream), 'logs_stream should be defined (NEW)'
  end

  def test_utility_functions_defined
    assert Un.respond_to?(:validate_keys)
    assert Un.respond_to?(:version), 'version should be defined (NEW)'
    assert Un.respond_to?(:health_check), 'health_check should be defined (NEW)'
    assert Un.respond_to?(:last_error), 'last_error should be defined (NEW)'
    assert Un.respond_to?(:hmac_sign), 'hmac_sign should be defined (NEW)'
  end

  # =========================================================================
  # Language Detection Tests
  # =========================================================================

  def test_detect_language_python
    assert_equal 'python', Un.detect_language('test.py')
  end

  def test_detect_language_javascript
    assert_equal 'javascript', Un.detect_language('test.js')
  end

  def test_detect_language_typescript
    assert_equal 'typescript', Un.detect_language('test.ts')
  end

  def test_detect_language_ruby
    assert_equal 'ruby', Un.detect_language('test.rb')
  end

  def test_detect_language_go
    assert_equal 'go', Un.detect_language('test.go')
  end

  def test_detect_language_rust
    assert_equal 'rust', Un.detect_language('test.rs')
  end

  def test_detect_language_unknown_returns_nil
    assert_nil Un.detect_language('test.unknown')
  end
end

# Functional tests require API credentials
class TestFunctionalAPI < Minitest::Test
  def setup
    skip 'UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required' unless credentials_available?
    WebMock.allow_net_connect!
  end

  def teardown
    WebMock.disable_net_connect!
  end

  def test_health_check
    result = Un.health_check
    assert [true, false].include?(result)
  end

  def test_validate_keys
    result = Un.validate_keys
    assert result.is_a?(Hash)
  end

  def test_get_languages
    languages = Un.get_languages
    assert languages.is_a?(Array)
    assert languages.include?('python')
  end

  def test_list_sessions
    sessions = Un.list_sessions
    assert sessions.is_a?(Array)
  end

  def test_list_services
    services = Un.list_services
    assert services.is_a?(Array)
  end

  def test_list_snapshots
    snapshots = Un.list_snapshots
    assert snapshots.is_a?(Array)
  end

  def test_list_images
    images = Un.list_images
    assert images.is_a?(Array)
  end

  def test_execute_code
    result = Un.execute_code('python', 'print("hello")')
    assert result.is_a?(Hash)
    assert %w[completed pending].include?(result['status'])
  end

  private

  def credentials_available?
    ENV['UNSANDBOX_PUBLIC_KEY'] && ENV['UNSANDBOX_SECRET_KEY']
  end
end
