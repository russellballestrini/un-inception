#!/usr/bin/env lua
-- Unit Tests for un.lua Library Functions
--
-- Tests the ACTUAL exported functions from Un module.
-- NO local re-implementations. NO mocking.
--
-- Run: lua tests/test_library.lua

-- Adjust package path to find the module
local script_dir = arg[0]:match("(.*/)") or "./"
package.path = script_dir .. "../sync/src/?.lua;" .. package.path

local Un = require("un")

-- Test counters
local tests_passed = 0
local tests_failed = 0

local function PASS(msg)
    print("  \027[32m[PASS]\027[0m " .. msg)
    tests_passed = tests_passed + 1
end

local function FAIL(msg)
    print("  \027[31m[FAIL]\027[0m " .. msg)
    tests_failed = tests_failed + 1
end

local function assert_equal(actual, expected, msg)
    if actual == expected then
        PASS(msg)
    else
        FAIL(msg .. " (expected: " .. tostring(expected) .. ", got: " .. tostring(actual) .. ")")
    end
end

local function assert_true(condition, msg)
    if condition then
        PASS(msg)
    else
        FAIL(msg)
    end
end

local function assert_nil(value, msg)
    if value == nil then
        PASS(msg)
    else
        FAIL(msg .. " (expected nil, got: " .. tostring(value) .. ")")
    end
end

local function assert_not_nil(value, msg)
    if value ~= nil then
        PASS(msg)
    else
        FAIL(msg .. " (expected non-nil)")
    end
end

-- ============================================================================
-- Test: Un.version()
-- ============================================================================

print("\nTesting Un.version()...")

local version = Un.version()
assert_not_nil(version, "version() returns non-nil")
assert_true(#version > 0, "version() returns non-empty string")
assert_true(version:match("^%d+%.%d+%.%d+$") ~= nil, "version() matches X.Y.Z format")
print("    Version: " .. version)

-- ============================================================================
-- Test: Un.detect_language()
-- ============================================================================

print("\nTesting Un.detect_language()...")

local tests = {
    {"test.py", "python"},
    {"app.js", "javascript"},
    {"main.go", "go"},
    {"script.rb", "ruby"},
    {"lib.rs", "rust"},
    {"main.c", "c"},
    {"app.cpp", "cpp"},
    {"Main.java", "java"},
    {"index.php", "php"},
    {"script.pl", "perl"},
    {"init.lua", "lua"},
    {"run.sh", "bash"},
    {"main.ts", "typescript"},
    {"app.kt", "kotlin"},
    {"lib.ex", "elixir"},
    {"main.hs", "haskell"},
}

for _, test in ipairs(tests) do
    local file, expected = test[1], test[2]
    local result = Un.detect_language(file)
    assert_equal(result, expected, "detect_language('" .. file .. "') -> '" .. expected .. "'")
end

-- Test nil handling
local null_result = Un.detect_language(nil)
assert_nil(null_result, "detect_language(nil) returns nil")

-- Test unknown extension
local unknown = Un.detect_language("file.xyz123")
assert_nil(unknown, "detect_language(unknown ext) returns nil")

-- Test no extension
local noext = Un.detect_language("Makefile")
assert_nil(noext, "detect_language(no ext) returns nil")

-- ============================================================================
-- Test: Un.hmac_sign()
-- ============================================================================

print("\nTesting Un.hmac_sign()...")

-- Test basic signature generation
local sig = Un.hmac_sign("secret_key", "1234567890:POST:/execute:{}")
assert_not_nil(sig, "hmac_sign() returns non-nil")
assert_equal(#sig, 64, "hmac_sign() returns 64-char hex string")

-- Verify hex characters
assert_true(sig:match("^[0-9a-fA-F]+$") ~= nil, "hmac_sign() returns valid hex")

-- Test deterministic output
local sig1 = Un.hmac_sign("key", "message")
local sig2 = Un.hmac_sign("key", "message")
assert_equal(sig1, sig2, "hmac_sign() is deterministic")

-- Test different keys produce different signatures
local sig_a = Un.hmac_sign("key_a", "message")
local sig_b = Un.hmac_sign("key_b", "message")
assert_true(sig_a ~= sig_b, "Different keys produce different signatures")

-- Test different messages produce different signatures
local sig_m1 = Un.hmac_sign("key", "message1")
local sig_m2 = Un.hmac_sign("key", "message2")
assert_true(sig_m1 ~= sig_m2, "Different messages produce different signatures")

-- Test nil handling
local null_key = Un.hmac_sign(nil, "message")
assert_nil(null_key, "hmac_sign(nil, msg) returns nil")

local null_msg = Un.hmac_sign("key", nil)
assert_nil(null_msg, "hmac_sign(key, nil) returns nil")

-- Test known HMAC value
local known_sig = Un.hmac_sign("key", "message")
assert_true(known_sig:sub(1, 32) == "6e9ef29b75fffc5b7abae527d58fdadb",
    "HMAC-SHA256('key', 'message') matches expected prefix")

-- ============================================================================
-- Test: Un.last_error()
-- ============================================================================

print("\nTesting Un.last_error()...")

local error_msg = Un.last_error()
assert_not_nil(error_msg, "last_error() returns non-nil")

Un.set_error("test error")
assert_equal(Un.last_error(), "test error", "last_error() returns set error")

-- ============================================================================
-- Test: Memory stress test
-- ============================================================================

print("\nTesting Memory Management...")

-- Stress test HMAC allocation
for i = 0, 999 do
    Un.hmac_sign("key", "message")
end
PASS("1000 HMAC calls without crash")

-- Stress test language detection
for i = 0, 999 do
    Un.detect_language("test.py")
end
PASS("1000 detect_language calls without crash")

-- Stress test version
for i = 0, 999 do
    Un.version()
end
PASS("1000 version calls without crash")

-- ============================================================================
-- Test: Function existence
-- ============================================================================

print("\nTesting Library function existence...")

local functions = {
    -- Execution functions (8)
    "execute", "execute_async", "wait_job", "get_job",
    "cancel_job", "list_jobs", "get_languages", "detect_language",

    -- Session functions (9)
    "session_list", "session_get", "session_create", "session_destroy",
    "session_freeze", "session_unfreeze", "session_boost", "session_unboost",
    "session_execute",

    -- Service functions (17)
    "service_list", "service_get", "service_create", "service_destroy",
    "service_freeze", "service_unfreeze", "service_lock", "service_unlock",
    "service_set_unfreeze_on_demand", "service_redeploy", "service_logs",
    "service_execute", "service_env_get", "service_env_set",
    "service_env_delete", "service_env_export", "service_resize",

    -- Snapshot functions (9)
    "snapshot_list", "snapshot_get", "snapshot_session", "snapshot_service",
    "snapshot_restore", "snapshot_delete", "snapshot_lock", "snapshot_unlock",
    "snapshot_clone",

    -- Image functions (13)
    "image_list", "image_get", "image_publish", "image_delete",
    "image_lock", "image_unlock", "image_set_visibility",
    "image_grant_access", "image_revoke_access", "image_list_trusted",
    "image_transfer", "image_spawn", "image_clone",

    -- PaaS Logs (2)
    "logs_fetch", "logs_stream",

    -- Utilities
    "validate_keys", "hmac_sign", "health_check", "version", "last_error"
}

for _, func_name in ipairs(functions) do
    assert_true(type(Un[func_name]) == "function", "Un." .. func_name .. " exists")
end

-- ============================================================================
-- Summary
-- ============================================================================

print("\n=====================================")
print("Test Summary")
print("=====================================")
print("Passed: \027[32m" .. tests_passed .. "\027[0m")
print("Failed: \027[31m" .. tests_failed .. "\027[0m")
print("=====================================")

os.exit(tests_failed > 0 and 1 or 0)
