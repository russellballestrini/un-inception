#!/usr/bin/env tclsh
# Unit Tests for un.tcl Library Functions
#
# Tests the ACTUAL exported functions from Un module.
# NO local re-implementations. NO mocking.
#
# Run: tclsh tests/test_library.tcl

# Adjust package path to find the module
set script_dir [file dirname [info script]]
source [file join $script_dir "../sync/src/un.tcl"]

# Test counters
set tests_passed 0
set tests_failed 0

proc PASS {msg} {
    global tests_passed
    puts "  \033\[32m\[PASS\]\033\[0m $msg"
    incr tests_passed
}

proc FAIL {msg} {
    global tests_failed
    puts "  \033\[31m\[FAIL\]\033\[0m $msg"
    incr tests_failed
}

proc assert_equal {actual expected msg} {
    if {$actual eq $expected} {
        PASS $msg
    } else {
        FAIL "$msg (expected: $expected, got: $actual)"
    }
}

proc assert_not_empty {value msg} {
    if {$value ne ""} {
        PASS $msg
    } else {
        FAIL "$msg (expected non-empty)"
    }
}

proc assert_match {value pattern msg} {
    if {[regexp $pattern $value]} {
        PASS $msg
    } else {
        FAIL "$msg (value: $value does not match pattern: $pattern)"
    }
}

# ============================================================================
# Test: Un::version()
# ============================================================================

puts "\nTesting Un::version()..."

set ver [Un::version]
assert_not_empty $ver "version() returns non-empty string"
assert_match $ver {^\d+\.\d+\.\d+$} "version() matches X.Y.Z format"
puts "    Version: $ver"

# ============================================================================
# Test: Un::detect_language()
# ============================================================================

puts "\nTesting Un::detect_language()..."

set tests {
    {"test.py" "python"}
    {"app.js" "javascript"}
    {"main.go" "go"}
    {"script.rb" "ruby"}
    {"lib.rs" "rust"}
    {"main.c" "c"}
    {"app.cpp" "cpp"}
    {"Main.java" "java"}
    {"index.php" "php"}
    {"script.pl" "perl"}
    {"init.lua" "lua"}
    {"run.sh" "bash"}
    {"main.ts" "typescript"}
    {"app.kt" "kotlin"}
    {"lib.ex" "elixir"}
    {"main.hs" "haskell"}
}

foreach test $tests {
    set file [lindex $test 0]
    set expected [lindex $test 1]
    set result [Un::detect_language $file]
    assert_equal $result $expected "detect_language('$file') -> '$expected'"
}

# Test unknown extension
set unknown [Un::detect_language "file.xyz123"]
assert_equal $unknown "" "detect_language(unknown ext) returns empty"

# Test no extension
set noext [Un::detect_language "Makefile"]
assert_equal $noext "" "detect_language(no ext) returns empty"

# ============================================================================
# Test: Un::hmac_sign()
# ============================================================================

puts "\nTesting Un::hmac_sign()..."

# Test basic signature generation
set sig [Un::hmac_sign "secret_key" "1234567890:POST:/execute:{}"]
assert_not_empty $sig "hmac_sign() returns non-nil"
assert_equal [string length $sig] 64 "hmac_sign() returns 64-char hex string"

# Verify hex characters
assert_match $sig {^[0-9a-fA-F]+$} "hmac_sign() returns valid hex"

# Test deterministic output
set sig1 [Un::hmac_sign "key" "message"]
set sig2 [Un::hmac_sign "key" "message"]
assert_equal $sig1 $sig2 "hmac_sign() is deterministic"

# Test different keys produce different signatures
set sig_a [Un::hmac_sign "key_a" "message"]
set sig_b [Un::hmac_sign "key_b" "message"]
if {$sig_a ne $sig_b} {
    PASS "Different keys produce different signatures"
} else {
    FAIL "Different keys produce different signatures"
}

# Test different messages produce different signatures
set sig_m1 [Un::hmac_sign "key" "message1"]
set sig_m2 [Un::hmac_sign "key" "message2"]
if {$sig_m1 ne $sig_m2} {
    PASS "Different messages produce different signatures"
} else {
    FAIL "Different messages produce different signatures"
}

# Test known HMAC value
set known_sig [Un::hmac_sign "key" "message"]
if {[string range $known_sig 0 31] eq "6e9ef29b75fffc5b7abae527d58fdadb"} {
    PASS "HMAC-SHA256('key', 'message') matches expected prefix"
} else {
    FAIL "HMAC-SHA256('key', 'message') matches expected prefix (got: $known_sig)"
}

# ============================================================================
# Test: Un::last_error()
# ============================================================================

puts "\nTesting Un::last_error()..."

Un::set_error "test error"
set err [Un::last_error]
assert_equal $err "test error" "last_error() returns set error"

# ============================================================================
# Test: Memory stress test
# ============================================================================

puts "\nTesting Memory Management..."

# Stress test HMAC allocation
for {set i 0} {$i < 1000} {incr i} {
    Un::hmac_sign "key" "message"
}
PASS "1000 HMAC calls without crash"

# Stress test language detection
for {set i 0} {$i < 1000} {incr i} {
    Un::detect_language "test.py"
}
PASS "1000 detect_language calls without crash"

# Stress test version
for {set i 0} {$i < 1000} {incr i} {
    Un::version
}
PASS "1000 version calls without crash"

# ============================================================================
# Test: Function existence
# ============================================================================

puts "\nTesting Library function existence..."

set functions {
    execute execute_async wait_job get_job
    cancel_job list_jobs get_languages detect_language

    session_list session_get session_create session_destroy
    session_freeze session_unfreeze session_boost session_unboost
    session_execute

    service_list service_get service_create service_destroy
    service_freeze service_unfreeze service_lock service_unlock
    service_set_unfreeze_on_demand service_redeploy service_logs
    service_execute service_env_get service_env_set
    service_env_delete service_env_export service_resize

    snapshot_list snapshot_get snapshot_session snapshot_service
    snapshot_restore snapshot_delete snapshot_lock snapshot_unlock
    snapshot_clone

    image_list image_get image_publish image_delete
    image_lock image_unlock image_set_visibility
    image_grant_access image_revoke_access image_list_trusted
    image_transfer image_spawn image_clone

    logs_fetch logs_stream

    validate_keys hmac_sign health_check version last_error
}

foreach func $functions {
    if {[llength [info procs Un::$func]] > 0 || [llength [info commands Un::$func]] > 0} {
        PASS "Un::$func() exists"
    } else {
        FAIL "Un::$func() exists"
    }
}

# ============================================================================
# Summary
# ============================================================================

puts "\n====================================="
puts "Test Summary"
puts "====================================="
puts "Passed: \033\[32m$tests_passed\033\[0m"
puts "Failed: \033\[31m$tests_failed\033\[0m"
puts "====================================="

exit [expr {$tests_failed > 0 ? 1 : 0}]
