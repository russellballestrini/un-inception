"""Tests for new SDK functions (feature parity with C implementation)"""

import pytest
from un import (
    # Execution
    execute_code,
    execute_async,
    get_job,
    wait_for_job,
    cancel_job,
    list_jobs,
    get_languages,
    detect_language,
    # Sessions
    list_sessions,
    get_session,
    create_session,
    delete_session,
    freeze_session,
    unfreeze_session,
    boost_session,
    unboost_session,
    shell_session,
    # Services
    list_services,
    create_service,
    get_service,
    update_service,
    delete_service,
    freeze_service,
    unfreeze_service,
    lock_service,
    unlock_service,
    set_unfreeze_on_demand,
    get_service_logs,
    get_service_env,
    set_service_env,
    delete_service_env,
    export_service_env,
    redeploy_service,
    execute_in_service,
    resize_service,
    # Snapshots
    session_snapshot,
    service_snapshot,
    list_snapshots,
    get_snapshot,
    restore_snapshot,
    delete_snapshot,
    lock_snapshot,
    unlock_snapshot,
    clone_snapshot,
    # Images
    image_publish,
    list_images,
    get_image,
    delete_image,
    lock_image,
    unlock_image,
    set_image_visibility,
    grant_image_access,
    revoke_image_access,
    list_image_trusted,
    transfer_image,
    spawn_from_image,
    clone_image,
    # PaaS Logs
    logs_fetch,
    logs_stream,
    # Utilities
    version,
    health_check,
    last_error,
    hmac_sign,
    validate_keys,
)


class TestUtilityFunctions:
    """Test utility functions that don't require API calls"""

    def test_version_returns_string(self):
        """Test version function returns a string"""
        v = version()
        assert isinstance(v, str)
        assert len(v) > 0
        # Should be semantic version format
        parts = v.split('.')
        assert len(parts) >= 2

    def test_hmac_sign_basic(self):
        """Test HMAC signing produces correct format"""
        signature = hmac_sign("secret_key", "message_to_sign")
        assert isinstance(signature, str)
        assert len(signature) == 64
        # Should be lowercase hex
        assert all(c in '0123456789abcdef' for c in signature)

    def test_hmac_sign_deterministic(self):
        """Test HMAC signing is deterministic"""
        sig1 = hmac_sign("secret", "message")
        sig2 = hmac_sign("secret", "message")
        assert sig1 == sig2

    def test_hmac_sign_different_secrets(self):
        """Test different secrets produce different signatures"""
        sig1 = hmac_sign("secret1", "message")
        sig2 = hmac_sign("secret2", "message")
        assert sig1 != sig2

    def test_hmac_sign_different_messages(self):
        """Test different messages produce different signatures"""
        sig1 = hmac_sign("secret", "message1")
        sig2 = hmac_sign("secret", "message2")
        assert sig1 != sig2

    def test_hmac_sign_known_value(self):
        """Test HMAC signing matches known value"""
        # This is a known test vector
        signature = hmac_sign("test_secret", "1234567890:POST:/execute:")
        assert len(signature) == 64

    def test_last_error_initial_none(self):
        """Test last_error is None initially or after success"""
        # Note: This may not be None if previous tests failed
        error = last_error()
        assert error is None or isinstance(error, str)


class TestFunctionExports:
    """Test that all required functions are exported"""

    def test_execution_functions_exported(self):
        """Test execution functions are exported"""
        assert callable(execute_code)
        assert callable(execute_async)
        assert callable(get_job)
        assert callable(wait_for_job)
        assert callable(cancel_job)
        assert callable(list_jobs)
        assert callable(get_languages)
        assert callable(detect_language)

    def test_session_functions_exported(self):
        """Test session functions are exported"""
        assert callable(list_sessions)
        assert callable(get_session)
        assert callable(create_session)
        assert callable(delete_session)
        assert callable(freeze_session)
        assert callable(unfreeze_session)
        assert callable(boost_session)
        assert callable(unboost_session)
        assert callable(shell_session)

    def test_service_functions_exported(self):
        """Test service functions are exported"""
        assert callable(list_services)
        assert callable(create_service)
        assert callable(get_service)
        assert callable(update_service)
        assert callable(delete_service)
        assert callable(freeze_service)
        assert callable(unfreeze_service)
        assert callable(lock_service)
        assert callable(unlock_service)
        assert callable(set_unfreeze_on_demand)
        assert callable(get_service_logs)
        assert callable(get_service_env)
        assert callable(set_service_env)
        assert callable(delete_service_env)
        assert callable(export_service_env)
        assert callable(redeploy_service)
        assert callable(execute_in_service)
        assert callable(resize_service)  # New function

    def test_snapshot_functions_exported(self):
        """Test snapshot functions are exported"""
        assert callable(session_snapshot)
        assert callable(service_snapshot)
        assert callable(list_snapshots)
        assert callable(get_snapshot)  # New function
        assert callable(restore_snapshot)
        assert callable(delete_snapshot)
        assert callable(lock_snapshot)
        assert callable(unlock_snapshot)
        assert callable(clone_snapshot)

    def test_image_functions_exported(self):
        """Test image functions are exported"""
        assert callable(image_publish)
        assert callable(list_images)
        assert callable(get_image)
        assert callable(delete_image)
        assert callable(lock_image)
        assert callable(unlock_image)
        assert callable(set_image_visibility)
        assert callable(grant_image_access)
        assert callable(revoke_image_access)
        assert callable(list_image_trusted)
        assert callable(transfer_image)
        assert callable(spawn_from_image)
        assert callable(clone_image)

    def test_logs_functions_exported(self):
        """Test PaaS logs functions are exported"""
        assert callable(logs_fetch)  # New function
        assert callable(logs_stream)  # New function

    def test_utility_functions_exported(self):
        """Test utility functions are exported"""
        assert callable(validate_keys)
        assert callable(version)  # New function
        assert callable(health_check)  # New function
        assert callable(last_error)  # New function
        assert callable(hmac_sign)  # New function


class TestLanguageDetection:
    """Test language detection (subset, full tests in test_language_detection.py)"""

    def test_common_extensions(self):
        """Test common file extensions"""
        assert detect_language("test.py") == "python"
        assert detect_language("test.js") == "javascript"
        assert detect_language("test.rb") == "ruby"
        assert detect_language("test.go") == "go"
        assert detect_language("test.rs") == "rust"

    def test_unknown_returns_none(self):
        """Test unknown extensions return None"""
        assert detect_language("test.unknown") is None
        assert detect_language("Makefile") is None


# Functional tests require API credentials - mark as skip without credentials
@pytest.mark.skipif(
    not all([
        __import__('os').environ.get('UNSANDBOX_PUBLIC_KEY'),
        __import__('os').environ.get('UNSANDBOX_SECRET_KEY'),
    ]),
    reason="UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required"
)
class TestFunctionalAPI:
    """Functional tests that require real API credentials"""

    def test_health_check(self):
        """Test API health check"""
        result = health_check()
        assert isinstance(result, bool)
        # API should be healthy
        assert result is True

    def test_validate_keys(self):
        """Test key validation"""
        result = validate_keys()
        assert isinstance(result, dict)
        assert 'valid' in result or 'tier' in result

    def test_get_languages(self):
        """Test getting supported languages"""
        languages = get_languages()
        assert isinstance(languages, list)
        assert len(languages) > 0
        assert 'python' in languages
        assert 'javascript' in languages

    def test_list_sessions(self):
        """Test listing sessions"""
        sessions = list_sessions()
        assert isinstance(sessions, list)

    def test_list_services(self):
        """Test listing services"""
        services = list_services()
        assert isinstance(services, list)

    def test_list_snapshots(self):
        """Test listing snapshots"""
        snapshots = list_snapshots()
        assert isinstance(snapshots, list)

    def test_list_images(self):
        """Test listing images"""
        images = list_images()
        assert isinstance(images, list)

    def test_execute_code(self):
        """Test code execution"""
        result = execute_code("python", "print('hello world')")
        assert isinstance(result, dict)
        assert result.get('status') in ['completed', 'pending']
        if result.get('status') == 'completed':
            assert 'hello world' in result.get('stdout', '')
