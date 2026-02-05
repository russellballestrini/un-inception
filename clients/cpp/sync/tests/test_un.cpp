// Tests for the C++ unsandbox SDK
// Compile: g++ -std=c++17 -o test_un test_un.cpp -I../src
// Run: ./test_un

#include <iostream>
#include <cassert>
#include <cstring>
#include <cstdlib>

// Include the SDK source directly for testing
// In production, you'd link against the compiled library
#include "../src/un.cpp"

using namespace std;

int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) void test_##name()
#define RUN_TEST(name) do { \
    cout << "Running " << #name << "..." << endl; \
    try { \
        test_##name(); \
        cout << "  PASS" << endl; \
        tests_passed++; \
    } catch (const exception& e) { \
        cout << "  FAIL: " << e.what() << endl; \
        tests_failed++; \
    } catch (...) { \
        cout << "  FAIL: Unknown exception" << endl; \
        tests_failed++; \
    } \
} while(0)

#define ASSERT(cond) do { \
    if (!(cond)) { \
        throw runtime_error("Assertion failed: " #cond); \
    } \
} while(0)

#define ASSERT_EQ(a, b) do { \
    if ((a) != (b)) { \
        throw runtime_error("Assertion failed: " #a " == " #b); \
    } \
} while(0)

#define ASSERT_NE(a, b) do { \
    if ((a) == (b)) { \
        throw runtime_error("Assertion failed: " #a " != " #b); \
    } \
} while(0)

// ============================================================================
// Unit Tests - Test exported library functions
// ============================================================================

TEST(detect_language) {
    ASSERT_EQ(detect_language("script.py"), "python");
    ASSERT_EQ(detect_language("script.js"), "javascript");
    ASSERT_EQ(detect_language("script.ts"), "typescript");
    ASSERT_EQ(detect_language("script.go"), "go");
    ASSERT_EQ(detect_language("script.rs"), "rust");
    ASSERT_EQ(detect_language("script.c"), "c");
    ASSERT_EQ(detect_language("script.cpp"), "cpp");
    ASSERT_EQ(detect_language("script.d"), "d");
    ASSERT_EQ(detect_language("script.zig"), "zig");
    ASSERT_EQ(detect_language("script.sh"), "bash");
    ASSERT_EQ(detect_language("script.lua"), "lua");
    ASSERT_EQ(detect_language("script.php"), "php");
    ASSERT_EQ(detect_language("script.unknown"), "");
    ASSERT_EQ(detect_language("script"), "");
}

TEST(hmac_sign) {
    string secret_key = "test-secret";
    string message = "test-message";

    string result = hmac_sign(secret_key, message);

    // Should return a 64-character hex string
    ASSERT_EQ(result.length(), 64u);

    // Should be deterministic
    string result2 = hmac_sign(secret_key, message);
    ASSERT_EQ(result, result2);

    // Different inputs should produce different outputs
    string result3 = hmac_sign(secret_key, "different-message");
    ASSERT_NE(result, result3);
}

TEST(version) {
    string v = version();
    ASSERT(!v.empty());
    // Should be in semver format (at least "0.0.0")
    ASSERT(v.length() >= 5);
}

TEST(last_error) {
    // Set an error
    set_last_error("test error message");

    // Retrieve it
    string err = last_error();
    ASSERT_EQ(err, "test error message");

    // Clear it
    set_last_error("");
    err = last_error();
    ASSERT(err.empty());
}

TEST(escape_json) {
    ASSERT_EQ(escape_json("hello"), "hello");
    ASSERT_EQ(escape_json("hello\"world"), "hello\\\"world");
    ASSERT_EQ(escape_json("line1\nline2"), "line1\\nline2");
    ASSERT_EQ(escape_json("tab\there"), "tab\\there");
    ASSERT_EQ(escape_json("back\\slash"), "back\\\\slash");
}

TEST(base64_encode) {
    ASSERT_EQ(base64_encode(""), "");
    ASSERT_EQ(base64_encode("f"), "Zg==");
    ASSERT_EQ(base64_encode("fo"), "Zm8=");
    ASSERT_EQ(base64_encode("foo"), "Zm9v");
    ASSERT_EQ(base64_encode("foob"), "Zm9vYg==");
    ASSERT_EQ(base64_encode("fooba"), "Zm9vYmE=");
    ASSERT_EQ(base64_encode("foobar"), "Zm9vYmFy");
}

// ============================================================================
// Integration Tests - Test SDK internal consistency
// ============================================================================

TEST(compute_hmac) {
    string key = "test-key";
    string msg = "test-message";

    string sig1 = compute_hmac(key, msg);
    string sig2 = compute_hmac(key, msg);

    // Should be deterministic
    ASSERT_EQ(sig1, sig2);

    // Should produce different results for different inputs
    string sig3 = compute_hmac(key, "different");
    ASSERT_NE(sig1, sig3);
}

TEST(build_auth_headers) {
    string pk = "unsb-pk-test-test-test-test";
    string sk = "unsb-sk-test1-test2-test3-test4";

    string headers = build_auth_headers("POST", "/execute", "{}", pk, sk);

    // Should contain auth header
    ASSERT(headers.find("Authorization: Bearer " + pk) != string::npos);
    // Should contain timestamp header
    ASSERT(headers.find("X-Timestamp:") != string::npos);
    // Should contain signature header
    ASSERT(headers.find("X-Signature:") != string::npos);
}

// ============================================================================
// Functional Tests - Test against real API (requires credentials)
// ============================================================================

bool has_credentials() {
    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");
    return pk != nullptr && sk != nullptr && strlen(pk) > 0 && strlen(sk) > 0;
}

TEST(health_check_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    bool healthy = health_check();
    // Just verify it doesn't crash
    cout << "  Health check result: " << (healthy ? "healthy" : "unhealthy") << endl;
}

TEST(get_languages_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = get_languages(pk, sk);
    ASSERT(!result.empty());
    // Should contain python
    ASSERT(result.find("python") != string::npos);
}

TEST(validate_keys_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = validate_keys(pk, sk);
    ASSERT(!result.empty());
}

TEST(execute_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = execute("python", "print('hello from cpp test')", pk, sk);
    ASSERT(!result.empty());
    // Should contain output
    ASSERT(result.find("stdout") != string::npos || result.find("output") != string::npos);
}

TEST(session_list_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = session_list(pk, sk);
    ASSERT(!result.empty());
}

TEST(service_list_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = service_list(pk, sk);
    ASSERT(!result.empty());
}

TEST(snapshot_list_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = snapshot_list(pk, sk);
    ASSERT(!result.empty());
}

TEST(image_list_functional) {
    if (!has_credentials()) {
        cout << "  SKIP (no credentials)" << endl;
        return;
    }

    const char* pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char* sk = getenv("UNSANDBOX_SECRET_KEY");

    string result = image_list("", pk, sk);
    ASSERT(!result.empty());
}

int main() {
    cout << "===== C++ SDK Tests =====" << endl << endl;

    // Unit tests
    cout << "--- Unit Tests ---" << endl;
    RUN_TEST(detect_language);
    RUN_TEST(hmac_sign);
    RUN_TEST(version);
    RUN_TEST(last_error);
    RUN_TEST(escape_json);
    RUN_TEST(base64_encode);

    cout << endl << "--- Integration Tests ---" << endl;
    RUN_TEST(compute_hmac);
    RUN_TEST(build_auth_headers);

    cout << endl << "--- Functional Tests ---" << endl;
    RUN_TEST(health_check_functional);
    RUN_TEST(get_languages_functional);
    RUN_TEST(validate_keys_functional);
    RUN_TEST(execute_functional);
    RUN_TEST(session_list_functional);
    RUN_TEST(service_list_functional);
    RUN_TEST(snapshot_list_functional);
    RUN_TEST(image_list_functional);

    cout << endl << "===== Results =====" << endl;
    cout << "Passed: " << tests_passed << endl;
    cout << "Failed: " << tests_failed << endl;

    return tests_failed > 0 ? 1 : 0;
}
