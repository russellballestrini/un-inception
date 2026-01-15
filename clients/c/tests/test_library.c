/*
 * Unit Tests for un.c Library Functions
 *
 * Tests the ACTUAL exported functions from un.c via un.h.
 * NO local re-implementations. NO mocking.
 *
 * Compile: make test-library
 * Run: ./tests/test_library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "un.h"

/* Test counters */
static int tests_passed = 0;
static int tests_failed = 0;

#define PASS(msg) do { printf("  \033[32m✓\033[0m %s\n", msg); tests_passed++; } while(0)
#define FAIL(msg) do { printf("  \033[31m✗\033[0m %s\n", msg); tests_failed++; } while(0)

/* ============================================================================
 * Test: unsandbox_version()
 * ============================================================================ */

void test_version(void) {
    printf("\nTesting unsandbox_version()...\n");

    const char *version = unsandbox_version();

    if (version != NULL) {
        PASS("unsandbox_version() returns non-NULL");
    } else {
        FAIL("unsandbox_version() returned NULL");
        return;
    }

    if (strlen(version) > 0) {
        PASS("unsandbox_version() returns non-empty string");
        printf("    Version: %s\n", version);
    } else {
        FAIL("unsandbox_version() returned empty string");
    }
}

/* ============================================================================
 * Test: unsandbox_detect_language()
 * ============================================================================ */

void test_detect_language(void) {
    printf("\nTesting unsandbox_detect_language()...\n");

    struct { const char *file; const char *expected; } tests[] = {
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
        {NULL, NULL}
    };

    for (int i = 0; tests[i].file; i++) {
        const char *lang = unsandbox_detect_language(tests[i].file);
        if (lang && strcmp(lang, tests[i].expected) == 0) {
            char msg[100];
            snprintf(msg, sizeof(msg), "detect_language('%s') -> '%s'", tests[i].file, tests[i].expected);
            PASS(msg);
        } else {
            char msg[100];
            snprintf(msg, sizeof(msg), "detect_language('%s') expected '%s', got '%s'",
                    tests[i].file, tests[i].expected, lang ? lang : "NULL");
            FAIL(msg);
        }
    }

    /* Test NULL handling */
    const char *null_result = unsandbox_detect_language(NULL);
    if (null_result == NULL) {
        PASS("detect_language(NULL) returns NULL");
    } else {
        FAIL("detect_language(NULL) should return NULL");
    }

    /* Test unknown extension */
    const char *unknown = unsandbox_detect_language("file.xyz123");
    if (unknown == NULL) {
        PASS("detect_language('file.xyz123') returns NULL for unknown");
    } else {
        char msg[100];
        snprintf(msg, sizeof(msg), "detect_language('file.xyz123') expected NULL, got '%s'", unknown);
        FAIL(msg);
    }

    /* Test no extension */
    const char *noext = unsandbox_detect_language("Makefile");
    if (noext == NULL) {
        PASS("detect_language('Makefile') returns NULL (no extension)");
    } else {
        char msg[100];
        snprintf(msg, sizeof(msg), "detect_language('Makefile') expected NULL, got '%s'", noext);
        FAIL(msg);
    }
}

/* ============================================================================
 * Test: unsandbox_hmac_sign()
 * ============================================================================ */

void test_hmac_sign(void) {
    printf("\nTesting unsandbox_hmac_sign()...\n");

    /* Test basic signature generation */
    char *sig = unsandbox_hmac_sign("secret_key", "1234567890:POST:/execute:{}");

    if (sig != NULL) {
        PASS("unsandbox_hmac_sign() returns non-NULL");
    } else {
        FAIL("unsandbox_hmac_sign() returned NULL");
        return;
    }

    if (strlen(sig) == 64) {
        PASS("unsandbox_hmac_sign() returns 64-char hex string");
    } else {
        char msg[100];
        snprintf(msg, sizeof(msg), "unsandbox_hmac_sign() returned %zu chars, expected 64", strlen(sig));
        FAIL(msg);
    }

    /* Verify it's all hex characters */
    int all_hex = 1;
    for (int i = 0; i < 64 && sig[i]; i++) {
        char c = sig[i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
            all_hex = 0;
            break;
        }
    }
    if (all_hex) {
        PASS("unsandbox_hmac_sign() returns valid hex characters");
    } else {
        FAIL("unsandbox_hmac_sign() returned non-hex characters");
    }

    free(sig);

    /* Test deterministic output - same input should produce same output */
    char *sig1 = unsandbox_hmac_sign("key", "message");
    char *sig2 = unsandbox_hmac_sign("key", "message");

    if (sig1 && sig2 && strcmp(sig1, sig2) == 0) {
        PASS("unsandbox_hmac_sign() is deterministic");
    } else {
        FAIL("unsandbox_hmac_sign() not deterministic");
    }

    free(sig1);
    free(sig2);

    /* Test different keys produce different signatures */
    char *sig_a = unsandbox_hmac_sign("key_a", "message");
    char *sig_b = unsandbox_hmac_sign("key_b", "message");

    if (sig_a && sig_b && strcmp(sig_a, sig_b) != 0) {
        PASS("Different keys produce different signatures");
    } else {
        FAIL("Different keys should produce different signatures");
    }

    free(sig_a);
    free(sig_b);

    /* Test different messages produce different signatures */
    char *sig_m1 = unsandbox_hmac_sign("key", "message1");
    char *sig_m2 = unsandbox_hmac_sign("key", "message2");

    if (sig_m1 && sig_m2 && strcmp(sig_m1, sig_m2) != 0) {
        PASS("Different messages produce different signatures");
    } else {
        FAIL("Different messages should produce different signatures");
    }

    free(sig_m1);
    free(sig_m2);

    /* Test NULL handling */
    char *null_key = unsandbox_hmac_sign(NULL, "message");
    if (null_key == NULL) {
        PASS("unsandbox_hmac_sign(NULL, msg) returns NULL");
    } else {
        FAIL("unsandbox_hmac_sign(NULL, msg) should return NULL");
        free(null_key);
    }

    char *null_msg = unsandbox_hmac_sign("key", NULL);
    if (null_msg == NULL) {
        PASS("unsandbox_hmac_sign(key, NULL) returns NULL");
    } else {
        FAIL("unsandbox_hmac_sign(key, NULL) should return NULL");
        free(null_msg);
    }

    /* Test known HMAC value (RFC 4231 test vector) */
    /* HMAC-SHA256("key", "message") should be a specific value */
    char *known_sig = unsandbox_hmac_sign("key", "message");
    if (known_sig) {
        /* Expected: 6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a */
        if (strncmp(known_sig, "6e9ef29b75fffc5b7abae527d58fdadb", 32) == 0) {
            PASS("HMAC-SHA256('key', 'message') matches expected value");
        } else {
            printf("    Got: %s\n", known_sig);
            printf("    Expected prefix: 6e9ef29b75fffc5b7abae527d58fdadb\n");
            FAIL("HMAC-SHA256 value mismatch");
        }
        free(known_sig);
    }
}

/* ============================================================================
 * Test: Memory stress test
 * ============================================================================ */

void test_memory(void) {
    printf("\nTesting Memory Management...\n");

    /* Stress test HMAC allocation */
    for (int i = 0; i < 1000; i++) {
        char *sig = unsandbox_hmac_sign("key", "message");
        if (sig) free(sig);
    }
    PASS("1000 HMAC allocations without crash");

    /* Stress test language detection */
    for (int i = 0; i < 1000; i++) {
        unsandbox_detect_language("test.py");
    }
    PASS("1000 detect_language calls without crash");

    /* Stress test version */
    for (int i = 0; i < 1000; i++) {
        unsandbox_version();
    }
    PASS("1000 version calls without crash");
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(void) {
    printf("=====================================\n");
    printf("UN C SDK - Unit Tests\n");
    printf("Testing ACTUAL exported functions\n");
    printf("=====================================\n");

    test_version();
    test_detect_language();
    test_hmac_sign();
    test_memory();

    printf("\n=====================================\n");
    printf("Test Summary\n");
    printf("=====================================\n");
    printf("Passed: \033[32m%d\033[0m\n", tests_passed);
    printf("Failed: \033[31m%d\033[0m\n", tests_failed);
    printf("=====================================\n");

    return tests_failed > 0 ? 1 : 0;
}
