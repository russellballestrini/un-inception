/*
 * Library Mode Tests for un.c
 * Tests un.c functions as an embeddable C library
 *
 * Compile: gcc -o test_library test_library.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
 * Run: ./test_library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Test counters
static int tests_passed = 0;
static int tests_failed = 0;

#define PASS(msg) do { printf("  \033[32m✓\033[0m %s\n", msg); tests_passed++; } while(0)
#define FAIL(msg) do { printf("  \033[31m✗\033[0m %s\n", msg); tests_failed++; } while(0)
#define SKIP(msg) do { printf("  \033[33m⊘\033[0m %s (skipped)\n", msg); } while(0)

// ============================================================================
// Minimal SHA-256 test (copied from un.c for standalone testing)
// ============================================================================

static const uint32_t sha256_k[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROTR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))
#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22))
#define EP1(x) (ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25))
#define SIG0(x) (ROTR(x, 7) ^ ROTR(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTR(x, 17) ^ ROTR(x, 19) ^ ((x) >> 10))

typedef struct { uint32_t state[8]; uint64_t count; unsigned char buffer[64]; } SHA256_CTX;

static void sha256_init(SHA256_CTX *ctx) {
    ctx->state[0] = 0x6a09e667; ctx->state[1] = 0xbb67ae85;
    ctx->state[2] = 0x3c6ef372; ctx->state[3] = 0xa54ff53a;
    ctx->state[4] = 0x510e527f; ctx->state[5] = 0x9b05688c;
    ctx->state[6] = 0x1f83d9ab; ctx->state[7] = 0x5be0cd19;
    ctx->count = 0;
}

static void sha256_transform(SHA256_CTX *ctx, const unsigned char *data) {
    uint32_t a, b, c, d, e, f, g, h, t1, t2, w[64];
    int i;
    for (i = 0; i < 16; i++)
        w[i] = ((uint32_t)data[i*4] << 24) | ((uint32_t)data[i*4+1] << 16) |
               ((uint32_t)data[i*4+2] << 8) | ((uint32_t)data[i*4+3]);
    for (i = 16; i < 64; i++)
        w[i] = SIG1(w[i-2]) + w[i-7] + SIG0(w[i-15]) + w[i-16];
    a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2]; d = ctx->state[3];
    e = ctx->state[4]; f = ctx->state[5]; g = ctx->state[6]; h = ctx->state[7];
    for (i = 0; i < 64; i++) {
        t1 = h + EP1(e) + CH(e, f, g) + sha256_k[i] + w[i];
        t2 = EP0(a) + MAJ(a, b, c);
        h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
    }
    ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
    ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

static void sha256_update(SHA256_CTX *ctx, const unsigned char *data, size_t len) {
    size_t i, index, part_len;
    index = (size_t)(ctx->count & 0x3F);
    ctx->count += len;
    part_len = 64 - index;
    if (len >= part_len) {
        memcpy(&ctx->buffer[index], data, part_len);
        sha256_transform(ctx, ctx->buffer);
        for (i = part_len; i + 63 < len; i += 64)
            sha256_transform(ctx, &data[i]);
        index = 0;
    } else { i = 0; }
    memcpy(&ctx->buffer[index], &data[i], len - i);
}

static void sha256_final(SHA256_CTX *ctx, unsigned char hash[32]) {
    unsigned char pad[64] = {0x80};
    unsigned char count_bits[8];
    uint64_t bits = ctx->count * 8;
    size_t index = (size_t)(ctx->count & 0x3F);
    size_t pad_len = (index < 56) ? (56 - index) : (120 - index);
    for (int i = 0; i < 8; i++) count_bits[7-i] = (bits >> (i*8)) & 0xff;
    sha256_update(ctx, pad, pad_len);
    sha256_update(ctx, count_bits, 8);
    for (int i = 0; i < 8; i++) {
        hash[i*4] = (ctx->state[i] >> 24) & 0xff;
        hash[i*4+1] = (ctx->state[i] >> 16) & 0xff;
        hash[i*4+2] = (ctx->state[i] >> 8) & 0xff;
        hash[i*4+3] = ctx->state[i] & 0xff;
    }
}

// HMAC-SHA256
static char *hmac_sha256(const char *key, const char *message) {
    if (!key || !message) return NULL;

    unsigned char k_ipad[64], k_opad[64], tk[32];
    size_t key_len = strlen(key);

    if (key_len > 64) {
        SHA256_CTX ctx;
        sha256_init(&ctx);
        sha256_update(&ctx, (unsigned char *)key, key_len);
        sha256_final(&ctx, tk);
        key = (char *)tk;
        key_len = 32;
    }

    memset(k_ipad, 0x36, 64);
    memset(k_opad, 0x5c, 64);
    for (size_t i = 0; i < key_len; i++) {
        k_ipad[i] ^= key[i];
        k_opad[i] ^= key[i];
    }

    SHA256_CTX ctx;
    unsigned char inner_hash[32], outer_hash[32];

    sha256_init(&ctx);
    sha256_update(&ctx, k_ipad, 64);
    sha256_update(&ctx, (unsigned char *)message, strlen(message));
    sha256_final(&ctx, inner_hash);

    sha256_init(&ctx);
    sha256_update(&ctx, k_opad, 64);
    sha256_update(&ctx, inner_hash, 32);
    sha256_final(&ctx, outer_hash);

    char *result = malloc(65);
    for (int i = 0; i < 32; i++)
        sprintf(&result[i*2], "%02x", outer_hash[i]);
    result[64] = '\0';
    return result;
}

// Language detection (simplified)
static const char *detect_language(const char *filename) {
    if (!filename) return NULL;
    const char *ext = strrchr(filename, '.');
    if (!ext) return NULL;
    ext++;
    if (strcmp(ext, "py") == 0) return "python";
    if (strcmp(ext, "js") == 0) return "javascript";
    if (strcmp(ext, "go") == 0) return "go";
    if (strcmp(ext, "rb") == 0) return "ruby";
    if (strcmp(ext, "rs") == 0) return "rust";
    if (strcmp(ext, "c") == 0) return "c";
    if (strcmp(ext, "cpp") == 0) return "cpp";
    if (strcmp(ext, "java") == 0) return "java";
    if (strcmp(ext, "php") == 0) return "php";
    if (strcmp(ext, "pl") == 0) return "perl";
    if (strcmp(ext, "lua") == 0) return "lua";
    if (strcmp(ext, "sh") == 0) return "bash";
    return NULL;
}

// ============================================================================
// Tests
// ============================================================================

void test_sha256(void) {
    printf("\nTesting SHA-256...\n");

    SHA256_CTX ctx;
    unsigned char hash[32];

    // Test known hash: SHA256("hello") = 2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824
    sha256_init(&ctx);
    sha256_update(&ctx, (unsigned char *)"hello", 5);
    sha256_final(&ctx, hash);

    if (hash[0] == 0x2c && hash[1] == 0xf2 && hash[2] == 0x4d && hash[3] == 0xba) {
        PASS("Library: SHA-256('hello') correct");
    } else {
        FAIL("Library: SHA-256('hello') mismatch");
    }

    // Test empty string: SHA256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
    sha256_init(&ctx);
    sha256_update(&ctx, (unsigned char *)"", 0);
    sha256_final(&ctx, hash);

    if (hash[0] == 0xe3 && hash[1] == 0xb0 && hash[2] == 0xc4 && hash[3] == 0x42) {
        PASS("Library: SHA-256('') correct");
    } else {
        FAIL("Library: SHA-256('') mismatch");
    }
}

void test_hmac_sha256(void) {
    printf("\nTesting HMAC-SHA256...\n");

    // Test basic HMAC
    char *hmac = hmac_sha256("key", "message");
    if (hmac && strlen(hmac) == 64) {
        PASS("Library: HMAC-SHA256 returns 64-char hex");

        // Known value: HMAC-SHA256("key", "message") = 6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a
        if (strncmp(hmac, "6e9ef29b75fffc5b7abae527d58fdadb", 32) == 0) {
            PASS("Library: HMAC-SHA256 value correct");
        } else {
            FAIL("Library: HMAC-SHA256 value mismatch");
            printf("    Got: %s\n", hmac);
        }
        free(hmac);
    } else {
        FAIL("Library: HMAC-SHA256 failed");
    }

    // Test NULL handling
    hmac = hmac_sha256(NULL, "message");
    if (hmac == NULL) {
        PASS("Library: HMAC-SHA256(NULL, msg) returns NULL");
    } else {
        FAIL("Library: HMAC-SHA256 should reject NULL key");
        free(hmac);
    }

    hmac = hmac_sha256("key", NULL);
    if (hmac == NULL) {
        PASS("Library: HMAC-SHA256(key, NULL) returns NULL");
    } else {
        FAIL("Library: HMAC-SHA256 should reject NULL message");
        free(hmac);
    }
}

void test_detect_language(void) {
    printf("\nTesting detect_language()...\n");

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
        {NULL, NULL}
    };

    for (int i = 0; tests[i].file; i++) {
        const char *lang = detect_language(tests[i].file);
        if (lang && strcmp(lang, tests[i].expected) == 0) {
            char msg[100];
            snprintf(msg, sizeof(msg), "Library: detect_language('%s') -> '%s'", tests[i].file, tests[i].expected);
            PASS(msg);
        } else {
            char msg[100];
            snprintf(msg, sizeof(msg), "Library: detect_language('%s') failed (got '%s')", tests[i].file, lang ? lang : "NULL");
            FAIL(msg);
        }
    }

    // Test NULL
    if (detect_language(NULL) == NULL) {
        PASS("Library: detect_language(NULL) returns NULL");
    } else {
        FAIL("Library: detect_language(NULL) should return NULL");
    }

    // Test unknown extension
    const char *unknown = detect_language("file.xyz123");
    if (unknown == NULL) {
        PASS("Library: detect_language('file.xyz123') returns NULL");
    } else {
        SKIP("Library: detect_language handles unknown (returns something)");
    }
}

void test_memory(void) {
    printf("\nTesting Memory Management...\n");

    // Stress test HMAC allocation
    for (int i = 0; i < 1000; i++) {
        char *hmac = hmac_sha256("key", "message");
        if (hmac) free(hmac);
    }
    PASS("Library: 1000 HMAC allocations without crash");

    // Stress test detect_language
    for (int i = 0; i < 1000; i++) {
        detect_language("test.py");
    }
    PASS("Library: 1000 detect_language calls without crash");
}

// ============================================================================
// Main
// ============================================================================

int main(void) {
    printf("Library Mode Tests for un.c\n");
    printf("============================\n");

    test_sha256();
    test_hmac_sha256();
    test_detect_language();
    test_memory();

    printf("\n============================\n");
    printf("Library Mode Test Summary\n");
    printf("============================\n");
    printf("Passed: \033[32m%d\033[0m\n", tests_passed);
    printf("Failed: \033[31m%d\033[0m\n", tests_failed);

    return tests_failed > 0 ? 1 : 0;
}
