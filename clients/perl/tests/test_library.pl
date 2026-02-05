#!/usr/bin/env perl
# Unit Tests for un.pl Library Functions
#
# Tests the ACTUAL exported functions from Un package.
# NO local re-implementations. NO mocking.
#
# Run: perl tests/test_library.pl

use strict;
use warnings;
use Test::More;
use FindBin qw($Bin);
use lib "$Bin/../sync/src";

# Load the Un module
require "$Bin/../sync/src/un.pl";

# Test counters
my $tests_passed = 0;
my $tests_failed = 0;

# ============================================================================
# Test: Un::version()
# ============================================================================

subtest 'Un::version()' => sub {
    my $version = Un::version();
    ok(defined $version, 'version() returns defined value');
    ok(length($version) > 0, 'version() returns non-empty string');
    like($version, qr/^\d+\.\d+\.\d+$/, 'version() matches X.Y.Z format');
    diag("Version: $version");
};

# ============================================================================
# Test: Un::detect_language()
# ============================================================================

subtest 'Un::detect_language()' => sub {
    my @tests = (
        ['test.py', 'python'],
        ['app.js', 'javascript'],
        ['main.go', 'go'],
        ['script.rb', 'ruby'],
        ['lib.rs', 'rust'],
        ['main.c', 'c'],
        ['app.cpp', 'cpp'],
        ['Main.java', 'java'],
        ['index.php', 'php'],
        ['script.pl', 'perl'],
        ['init.lua', 'lua'],
        ['run.sh', 'bash'],
        ['main.ts', 'typescript'],
        ['app.kt', 'kotlin'],
        ['lib.ex', 'elixir'],
        ['main.hs', 'haskell'],
    );

    for my $test (@tests) {
        my ($file, $expected) = @$test;
        my $result = Un::detect_language($file);
        is($result, $expected, "detect_language('$file') -> '$expected'");
    }

    # Test NULL handling
    my $null_result = Un::detect_language(undef);
    ok(!defined $null_result, 'detect_language(undef) returns undef');

    # Test unknown extension
    my $unknown = Un::detect_language('file.xyz123');
    ok(!defined $unknown, 'detect_language(unknown ext) returns undef');

    # Test no extension
    my $noext = Un::detect_language('Makefile');
    ok(!defined $noext, 'detect_language(no ext) returns undef');
};

# ============================================================================
# Test: Un::hmac_sign()
# ============================================================================

subtest 'Un::hmac_sign()' => sub {
    # Test basic signature generation
    my $sig = Un::hmac_sign('secret_key', '1234567890:POST:/execute:{}');
    ok(defined $sig, 'hmac_sign() returns defined value');
    is(length($sig), 64, 'hmac_sign() returns 64-char hex string');

    # Verify hex characters
    like($sig, qr/^[0-9a-fA-F]+$/, 'hmac_sign() returns valid hex');

    # Test deterministic output
    my $sig1 = Un::hmac_sign('key', 'message');
    my $sig2 = Un::hmac_sign('key', 'message');
    is($sig1, $sig2, 'hmac_sign() is deterministic');

    # Test different keys produce different signatures
    my $sig_a = Un::hmac_sign('key_a', 'message');
    my $sig_b = Un::hmac_sign('key_b', 'message');
    isnt($sig_a, $sig_b, 'Different keys produce different signatures');

    # Test different messages produce different signatures
    my $sig_m1 = Un::hmac_sign('key', 'message1');
    my $sig_m2 = Un::hmac_sign('key', 'message2');
    isnt($sig_m1, $sig_m2, 'Different messages produce different signatures');

    # Test NULL handling
    my $null_key = Un::hmac_sign(undef, 'message');
    ok(!defined $null_key, 'hmac_sign(undef, msg) returns undef');

    my $null_msg = Un::hmac_sign('key', undef);
    ok(!defined $null_msg, 'hmac_sign(key, undef) returns undef');

    # Test known HMAC value
    my $known_sig = Un::hmac_sign('key', 'message');
    like($known_sig, qr/^6e9ef29b75fffc5b7abae527d58fdadb/,
         'HMAC-SHA256("key", "message") matches expected prefix');
};

# ============================================================================
# Test: Un::last_error()
# ============================================================================

subtest 'Un::last_error()' => sub {
    # Initially should be empty or previous error
    my $error = Un::last_error();
    ok(defined $error, 'last_error() returns defined value');

    # Set an error and check
    Un::set_error('test error');
    is(Un::last_error(), 'test error', 'last_error() returns set error');
};

# ============================================================================
# Test: Memory stress test
# ============================================================================

subtest 'Memory stress test' => sub {
    # Stress test HMAC allocation
    for my $i (0..999) {
        my $sig = Un::hmac_sign('key', 'message');
    }
    pass('1000 HMAC calls without crash');

    # Stress test language detection
    for my $i (0..999) {
        Un::detect_language('test.py');
    }
    pass('1000 detect_language calls without crash');

    # Stress test version
    for my $i (0..999) {
        Un::version();
    }
    pass('1000 version calls without crash');
};

# ============================================================================
# Test: Function existence
# ============================================================================

subtest 'Library function existence' => sub {
    # Execution functions (8)
    can_ok('Un', 'execute');
    can_ok('Un', 'execute_async');
    can_ok('Un', 'wait_job');
    can_ok('Un', 'get_job');
    can_ok('Un', 'cancel_job');
    can_ok('Un', 'list_jobs');
    can_ok('Un', 'get_languages');
    can_ok('Un', 'detect_language');

    # Session functions (9)
    can_ok('Un', 'session_list');
    can_ok('Un', 'session_get');
    can_ok('Un', 'session_create');
    can_ok('Un', 'session_destroy');
    can_ok('Un', 'session_freeze');
    can_ok('Un', 'session_unfreeze');
    can_ok('Un', 'session_boost');
    can_ok('Un', 'session_unboost');
    can_ok('Un', 'session_execute');

    # Service functions (17)
    can_ok('Un', 'service_list');
    can_ok('Un', 'service_get');
    can_ok('Un', 'service_create');
    can_ok('Un', 'service_destroy');
    can_ok('Un', 'service_freeze');
    can_ok('Un', 'service_unfreeze');
    can_ok('Un', 'service_lock');
    can_ok('Un', 'service_unlock');
    can_ok('Un', 'service_set_unfreeze_on_demand');
    can_ok('Un', 'service_redeploy');
    can_ok('Un', 'service_logs');
    can_ok('Un', 'service_execute');
    can_ok('Un', 'service_env_get');
    can_ok('Un', 'service_env_set');
    can_ok('Un', 'service_env_delete');
    can_ok('Un', 'service_env_export');
    can_ok('Un', 'service_resize');

    # Snapshot functions (9)
    can_ok('Un', 'snapshot_list');
    can_ok('Un', 'snapshot_get');
    can_ok('Un', 'snapshot_session');
    can_ok('Un', 'snapshot_service');
    can_ok('Un', 'snapshot_restore');
    can_ok('Un', 'snapshot_delete');
    can_ok('Un', 'snapshot_lock');
    can_ok('Un', 'snapshot_unlock');
    can_ok('Un', 'snapshot_clone');

    # Image functions (13)
    can_ok('Un', 'image_list');
    can_ok('Un', 'image_get');
    can_ok('Un', 'image_publish');
    can_ok('Un', 'image_delete');
    can_ok('Un', 'image_lock');
    can_ok('Un', 'image_unlock');
    can_ok('Un', 'image_set_visibility');
    can_ok('Un', 'image_grant_access');
    can_ok('Un', 'image_revoke_access');
    can_ok('Un', 'image_list_trusted');
    can_ok('Un', 'image_transfer');
    can_ok('Un', 'image_spawn');
    can_ok('Un', 'image_clone');

    # PaaS Logs (2)
    can_ok('Un', 'logs_fetch');
    can_ok('Un', 'logs_stream');

    # Utilities
    can_ok('Un', 'validate_keys');
    can_ok('Un', 'hmac_sign');
    can_ok('Un', 'health_check');
    can_ok('Un', 'version');
    can_ok('Un', 'last_error');
};

done_testing();
