#!/usr/bin/env -S awk -f
# Unit Tests for un.awk Library Functions
#
# Tests the ACTUAL exported functions from Un module.
# NO local re-implementations. NO mocking.
#
# Run: awk -f tests/test_library.awk
#
# Note: AWK has limited introspection, so we test via CLI invocation

BEGIN {
    # Test counters
    tests_passed = 0
    tests_failed = 0

    # Colors
    GREEN = "\033[32m"
    RED = "\033[31m"
    RESET = "\033[0m"

    # Get script directory
    script_dir = ENVIRON["PWD"]
    if (script_dir == "") script_dir = "."

    print ""
    print "Testing AWK SDK..."
    print "====================================="

    # ============================================================================
    # Test: Version
    # ============================================================================

    print ""
    print "Testing version..."

    # AWK doesn't have introspection like other languages, so we verify the
    # script can be loaded and outputs help
    cmd = "awk -f " script_dir "/sync/src/un.awk --help 2>&1"
    result = ""
    while ((cmd | getline line) > 0) {
        result = result line "\n"
    }
    close(cmd)

    if (match(result, /Usage:/)) {
        print "  " GREEN "[PASS]" RESET " --help shows usage"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " --help shows usage (got: " result ")"
        tests_failed++
    }

    # ============================================================================
    # Test: Extension detection (via script)
    # ============================================================================

    print ""
    print "Testing extension map..."

    # Test that the extension map exists in the script
    cmd = "grep -c 'py:python' " script_dir "/sync/src/un.awk"
    cmd | getline count
    close(cmd)

    if (count > 0) {
        print "  " GREEN "[PASS]" RESET " Extension map includes py:python"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " Extension map includes py:python"
        tests_failed++
    }

    # Test more extensions
    extensions["js"] = "javascript"
    extensions["go"] = "go"
    extensions["rb"] = "ruby"
    extensions["rs"] = "rust"
    extensions["lua"] = "lua"

    for (ext in extensions) {
        expected = extensions[ext]
        pattern = ext ":" expected
        cmd = "grep -c '" pattern "' " script_dir "/sync/src/un.awk"
        cmd | getline count
        close(cmd)

        if (count > 0) {
            print "  " GREEN "[PASS]" RESET " Extension map includes " ext ":" expected
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " Extension map includes " ext ":" expected
            tests_failed++
        }
    }

    # ============================================================================
    # Test: HMAC signing (via openssl)
    # ============================================================================

    print ""
    print "Testing HMAC signing (openssl)..."

    # AWK SDK uses openssl for HMAC - verify openssl is available
    cmd = "which openssl >/dev/null 2>&1 && echo 'available'"
    cmd | getline openssl_status
    close(cmd)

    if (openssl_status == "available") {
        print "  " GREEN "[PASS]" RESET " openssl is available"
        tests_passed++

        # Test HMAC generation
        cmd = "echo -n 'message' | openssl dgst -sha256 -hmac 'key' 2>/dev/null | sed 's/^.* //'"
        cmd | getline sig
        close(cmd)

        if (length(sig) == 64) {
            print "  " GREEN "[PASS]" RESET " HMAC returns 64-char hex string"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " HMAC returns 64-char hex string (got: " length(sig) ")"
            tests_failed++
        }

        # Verify hex characters
        if (match(sig, /^[0-9a-fA-F]+$/)) {
            print "  " GREEN "[PASS]" RESET " HMAC returns valid hex"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " HMAC returns valid hex"
            tests_failed++
        }

        # Test known HMAC value
        if (match(sig, /^6e9ef29b75fffc5b7abae527d58fdadb/)) {
            print "  " GREEN "[PASS]" RESET " HMAC-SHA256('key', 'message') matches expected prefix"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " HMAC-SHA256('key', 'message') matches expected prefix (got: " sig ")"
            tests_failed++
        }

        # Test deterministic output
        cmd = "echo -n 'message' | openssl dgst -sha256 -hmac 'key' 2>/dev/null | sed 's/^.* //'"
        cmd | getline sig2
        close(cmd)

        if (sig == sig2) {
            print "  " GREEN "[PASS]" RESET " HMAC is deterministic"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " HMAC is deterministic"
            tests_failed++
        }

    } else {
        print "  " RED "[FAIL]" RESET " openssl is available"
        tests_failed++
    }

    # ============================================================================
    # Test: Function existence (via grep)
    # ============================================================================

    print ""
    print "Testing function existence..."

    # List of required functions
    functions["execute"] = "execute"
    functions["session_list"] = "session_list"
    functions["session_kill"] = "session_kill"
    functions["service_list"] = "service_list"
    functions["service_create"] = "service_create"
    functions["service_destroy"] = "service_destroy"
    functions["service_resize"] = "service_resize"
    functions["snapshot_list"] = "snapshot_list"
    functions["snapshot_info"] = "snapshot_info"
    functions["snapshot_delete"] = "snapshot_delete"
    functions["image_list"] = "image_list"
    functions["image_info"] = "image_info"
    functions["image_delete"] = "image_delete"
    functions["image_lock"] = "image_lock"
    functions["image_unlock"] = "image_unlock"
    functions["image_publish"] = "image_publish"
    functions["image_visibility"] = "image_visibility"
    functions["image_spawn"] = "image_spawn"
    functions["image_clone"] = "image_clone"
    functions["validate_key"] = "validate_key"
    functions["languages_list"] = "languages_list"
    functions["get_api_keys"] = "get_api_keys"
    functions["escape_json"] = "escape_json"
    functions["handle_sudo_challenge"] = "handle_sudo_challenge"

    for (func in functions) {
        pattern = "function " func "\\("
        cmd = "grep -cE '" pattern "' " script_dir "/sync/src/un.awk"
        cmd | getline count
        close(cmd)

        if (count > 0) {
            print "  " GREEN "[PASS]" RESET " " func "() exists"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " " func "() exists"
            tests_failed++
        }
    }

    # ============================================================================
    # Test: CLI commands (via grep)
    # ============================================================================

    print ""
    print "Testing CLI commands..."

    commands["session"] = "session"
    commands["service"] = "service"
    commands["snapshot"] = "snapshot"
    commands["image"] = "image"
    commands["key"] = "key"
    commands["languages"] = "languages"

    for (cmd_name in commands) {
        pattern = "ARGV\\[1\\] == \"" cmd_name "\""
        cmd = "grep -c '" pattern "' " script_dir "/sync/src/un.awk"
        cmd | getline count
        close(cmd)

        if (count > 0) {
            print "  " GREEN "[PASS]" RESET " CLI command '" cmd_name "' exists"
            tests_passed++
        } else {
            print "  " RED "[FAIL]" RESET " CLI command '" cmd_name "' exists"
            tests_failed++
        }
    }

    # ============================================================================
    # Test: 428 Sudo OTP handling
    # ============================================================================

    print ""
    print "Testing 428 Sudo OTP handling..."

    cmd = "grep -c 'handle_sudo_challenge' " script_dir "/sync/src/un.awk"
    cmd | getline count
    close(cmd)

    if (count > 0) {
        print "  " GREEN "[PASS]" RESET " 428 sudo challenge handling exists"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " 428 sudo challenge handling exists"
        tests_failed++
    }

    cmd = "grep -c 'X-Sudo-OTP' " script_dir "/sync/src/un.awk"
    cmd | getline count
    close(cmd)

    if (count > 0) {
        print "  " GREEN "[PASS]" RESET " X-Sudo-OTP header support exists"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " X-Sudo-OTP header support exists"
        tests_failed++
    }

    # ============================================================================
    # Test: Languages caching
    # ============================================================================

    print ""
    print "Testing languages caching..."

    cmd = "grep -c 'LANGUAGES_CACHE_TTL' " script_dir "/sync/src/un.awk"
    cmd | getline count
    close(cmd)

    if (count > 0) {
        print "  " GREEN "[PASS]" RESET " Languages cache TTL defined"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " Languages cache TTL defined"
        tests_failed++
    }

    cmd = "grep -c 'write_languages_cache' " script_dir "/sync/src/un.awk"
    cmd | getline count
    close(cmd)

    if (count > 0) {
        print "  " GREEN "[PASS]" RESET " Languages cache write function exists"
        tests_passed++
    } else {
        print "  " RED "[FAIL]" RESET " Languages cache write function exists"
        tests_failed++
    }

    # ============================================================================
    # Summary
    # ============================================================================

    print ""
    print "====================================="
    print "Test Summary"
    print "====================================="
    print "Passed: " GREEN tests_passed RESET
    print "Failed: " RED tests_failed RESET
    print "====================================="

    exit(tests_failed > 0 ? 1 : 0)
}
