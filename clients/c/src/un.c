/*
 * un - unsandbox.com CLI
 *
 * Authentication priority (highest to lowest, per POSIX convention):
 *   1. CLI flags: -p (public key) + -k (secret key)
 *   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
 *   3. Config file: ~/.unsandbox/accounts.csv (format: public_key,secret_key per line)
 *      - Use --account N to select account by index (0-based, default: 0)
 *      - Or set UNSANDBOX_ACCOUNT=N environment variable
 *
 * Request authentication:
 *   Authorization: Bearer <public_key>                        <- identifies account
 *   X-Timestamp: <unix_seconds>                               <- replay prevention
 *   X-Signature: HMAC-SHA256(secret_key, ts:method:path:body) <- proves secret + body integrity
 *
 * The secret key is NEVER transmitted. Server decrypts stored secret to verify HMAC.
 * Timestamp must be within ±5 minutes of server time (prevents replay attacks).
 * Body is included in signature to prevent tampering (empty string for GET/DELETE).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <curl/curl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <poll.h>
#include <libwebsockets.h>
#include <pwd.h>

#ifdef UNSANDBOX_LIBRARY
#include "un.h"
#endif

#define API_URL "https://api.unsandbox.com/execute"
#define API_BASE "https://api.unsandbox.com"
#define PORTAL_BASE "https://unsandbox.com"
#define MAX_FILE_SIZE (100 * 1024 * 1024) // 100MB max single file
#define MAX_INPUT_FILES 1000
#define MAX_TOTAL_INPUT_SIZE (4096L * 1024 * 1024) // 4GB total across all input files
#define LARGE_UPLOAD_WARN_SIZE (1024L * 1024 * 1024) // Warn if total > 1GB
#define MAX_ENV_VARS 256  // LXC limit is typically higher
#define MAX_ENV_CONTENT_SIZE (64 * 1024)  // 64KB max env vault size
#define LANGUAGES_CACHE_TTL 3600  // 1 hour cache for languages list

// ============================================================================
// SHA-256 Implementation (for HMAC-SHA256)
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

#define SHA256_ROTR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))
#define SHA256_CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define SHA256_MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define SHA256_EP0(x) (SHA256_ROTR(x, 2) ^ SHA256_ROTR(x, 13) ^ SHA256_ROTR(x, 22))
#define SHA256_EP1(x) (SHA256_ROTR(x, 6) ^ SHA256_ROTR(x, 11) ^ SHA256_ROTR(x, 25))
#define SHA256_SIG0(x) (SHA256_ROTR(x, 7) ^ SHA256_ROTR(x, 18) ^ ((x) >> 3))
#define SHA256_SIG1(x) (SHA256_ROTR(x, 17) ^ SHA256_ROTR(x, 19) ^ ((x) >> 10))

typedef struct {
    uint32_t state[8];
    uint64_t count;
    unsigned char buffer[64];
} UN_SHA256_CTX;

static void sha256_init(UN_SHA256_CTX *ctx) {
    ctx->state[0] = 0x6a09e667;
    ctx->state[1] = 0xbb67ae85;
    ctx->state[2] = 0x3c6ef372;
    ctx->state[3] = 0xa54ff53a;
    ctx->state[4] = 0x510e527f;
    ctx->state[5] = 0x9b05688c;
    ctx->state[6] = 0x1f83d9ab;
    ctx->state[7] = 0x5be0cd19;
    ctx->count = 0;
}

static void sha256_transform(UN_SHA256_CTX *ctx, const unsigned char *data) {
    uint32_t a, b, c, d, e, f, g, h, t1, t2, w[64];
    int i;

    for (i = 0; i < 16; i++) {
        w[i] = ((uint32_t)data[i * 4] << 24) | ((uint32_t)data[i * 4 + 1] << 16) |
               ((uint32_t)data[i * 4 + 2] << 8) | ((uint32_t)data[i * 4 + 3]);
    }
    for (i = 16; i < 64; i++) {
        w[i] = SHA256_SIG1(w[i - 2]) + w[i - 7] + SHA256_SIG0(w[i - 15]) + w[i - 16];
    }

    a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2]; d = ctx->state[3];
    e = ctx->state[4]; f = ctx->state[5]; g = ctx->state[6]; h = ctx->state[7];

    for (i = 0; i < 64; i++) {
        t1 = h + SHA256_EP1(e) + SHA256_CH(e, f, g) + sha256_k[i] + w[i];
        t2 = SHA256_EP0(a) + SHA256_MAJ(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
    ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

static void sha256_update(UN_SHA256_CTX *ctx, const unsigned char *data, size_t len) {
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
    } else {
        i = 0;
    }
    memcpy(&ctx->buffer[index], &data[i], len - i);
}

static void sha256_final(UN_SHA256_CTX *ctx, unsigned char hash[32]) {
    unsigned char pad[64];
    unsigned char count_bits[8];
    size_t index, pad_len;
    uint64_t bits = ctx->count * 8;
    int i;

    for (i = 0; i < 8; i++) {
        count_bits[i] = (unsigned char)(bits >> (56 - i * 8));
    }

    index = (size_t)(ctx->count & 0x3F);
    pad_len = (index < 56) ? (56 - index) : (120 - index);
    memset(pad, 0, pad_len);
    pad[0] = 0x80;
    sha256_update(ctx, pad, pad_len);
    sha256_update(ctx, count_bits, 8);

    for (i = 0; i < 8; i++) {
        hash[i * 4] = (unsigned char)(ctx->state[i] >> 24);
        hash[i * 4 + 1] = (unsigned char)(ctx->state[i] >> 16);
        hash[i * 4 + 2] = (unsigned char)(ctx->state[i] >> 8);
        hash[i * 4 + 3] = (unsigned char)(ctx->state[i]);
    }
}

// Compute raw SHA-256 hash (32 bytes)
static void sha256_raw(const unsigned char *data, size_t len, unsigned char hash[32]) {
    UN_SHA256_CTX ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, data, len);
    sha256_final(&ctx, hash);
}

// ============================================================================
// HMAC-SHA256 Implementation
// ============================================================================

#define HMAC_SHA256_BLOCK_SIZE 64
#define HMAC_SHA256_HASH_SIZE 32

// Compute HMAC-SHA256 and return as lowercase hex string (64 chars + null)
static char* hmac_sha256_hex(const char *key, size_t key_len, const char *data, size_t data_len) {
    unsigned char k_ipad[HMAC_SHA256_BLOCK_SIZE];
    unsigned char k_opad[HMAC_SHA256_BLOCK_SIZE];
    unsigned char tk[HMAC_SHA256_HASH_SIZE];
    unsigned char inner_hash[HMAC_SHA256_HASH_SIZE];
    unsigned char final_hash[HMAC_SHA256_HASH_SIZE];
    size_t i;

    // If key is longer than block size, hash it first
    if (key_len > HMAC_SHA256_BLOCK_SIZE) {
        sha256_raw((const unsigned char *)key, key_len, tk);
        key = (const char *)tk;
        key_len = HMAC_SHA256_HASH_SIZE;
    }

    // XOR key with ipad and opad values
    memset(k_ipad, 0x36, HMAC_SHA256_BLOCK_SIZE);
    memset(k_opad, 0x5c, HMAC_SHA256_BLOCK_SIZE);
    for (i = 0; i < key_len; i++) {
        k_ipad[i] ^= (unsigned char)key[i];
        k_opad[i] ^= (unsigned char)key[i];
    }

    // Inner hash: SHA256(k_ipad || data)
    UN_SHA256_CTX ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, k_ipad, HMAC_SHA256_BLOCK_SIZE);
    sha256_update(&ctx, (const unsigned char *)data, data_len);
    sha256_final(&ctx, inner_hash);

    // Outer hash: SHA256(k_opad || inner_hash)
    sha256_init(&ctx);
    sha256_update(&ctx, k_opad, HMAC_SHA256_BLOCK_SIZE);
    sha256_update(&ctx, inner_hash, HMAC_SHA256_HASH_SIZE);
    sha256_final(&ctx, final_hash);

    // Convert to hex
    char *hex = malloc(65);
    if (!hex) return NULL;
    for (i = 0; i < HMAC_SHA256_HASH_SIZE; i++) {
        sprintf(hex + i * 2, "%02x", final_hash[i]);
    }
    hex[64] = '\0';
    return hex;
}

// Sign a request: HMAC-SHA256(secret_key, timestamp:method:path:body)
// Returns signature as hex string (caller must free)
// body can be NULL for bodyless requests (GET, DELETE)
static char* sign_request(const char *secret_key, long timestamp, const char *method, const char *path, const char *body) {
    // Build message: "timestamp:method:path:body"
    // Body is included raw to prevent tampering (empty string if NULL)
    char ts_str[32];
    snprintf(ts_str, sizeof(ts_str), "%ld", timestamp);

    const char *body_str = body ? body : "";
    size_t msg_len = strlen(ts_str) + 1 + strlen(method) + 1 + strlen(path) + 1 + strlen(body_str);
    char *message = malloc(msg_len + 1);
    if (!message) return NULL;

    snprintf(message, msg_len + 1, "%s:%s:%s:%s", ts_str, method, path, body_str);

    char *signature = hmac_sha256_hex(secret_key, strlen(secret_key), message, strlen(message));
    free(message);
    return signature;
}

// ============================================================================
// Account Credentials Management (~/.unsandbox/accounts.csv)
// ============================================================================

typedef struct {
    char *public_key;   // unsb-pk-xxxx-xxxx-xxxx-xxxx - used as bearer token to identify account
    char *secret_key;   // unsb-sk-xxxxx-xxxxx-xxxxx-xxxxx - used only for HMAC signing, never transmitted
} UnsandboxCredentials;

// Get path to ~/.unsandbox/accounts.csv
static char* get_accounts_csv_path(void) {
    const char *home = getenv("HOME");
    if (!home) {
        struct passwd *pw = getpwuid(getuid());
        if (pw) home = pw->pw_dir;
    }
    if (!home) return NULL;

    char *path = malloc(strlen(home) + 32);
    if (!path) return NULL;
    sprintf(path, "%s/.unsandbox/accounts.csv", home);
    return path;
}

// Ensure ~/.unsandbox directory exists
__attribute__((unused))
static void ensure_unsandbox_dir(void) {
    const char *home = getenv("HOME");
    if (!home) {
        struct passwd *pw = getpwuid(getuid());
        if (pw) home = pw->pw_dir;
    }
    if (!home) return;

    char dir[512];
    snprintf(dir, sizeof(dir), "%s/.unsandbox", home);
    mkdir(dir, 0700);
}

// Load account from ~/.unsandbox/accounts.csv by index (0-based)
// Format: public_key,secret_key (one per line)
// index -1 means use first valid account
static UnsandboxCredentials* load_credentials_from_csv(int account_index) {
    char *path = get_accounts_csv_path();
    if (!path) return NULL;

    FILE *f = fopen(path, "r");
    free(path);
    if (!f) return NULL;

    char line[1024];
    UnsandboxCredentials *creds = NULL;
    int current_index = 0;

    while (fgets(line, sizeof(line), f)) {
        // Skip empty lines and comments
        if (line[0] == '\n' || line[0] == '#') continue;

        // Remove newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') line[len - 1] = '\0';

        // Parse CSV: public_key,secret_key
        char *comma = strchr(line, ',');
        if (!comma) continue;

        *comma = '\0';
        char *pk = line;
        char *sk = comma + 1;

        // Validate key prefixes
        if (strncmp(pk, "unsb-pk-", 8) != 0) continue;
        if (strncmp(sk, "unsb-sk-", 8) != 0) continue;

        // Check if this is the account we want
        if (account_index >= 0 && current_index != account_index) {
            current_index++;
            continue;
        }

        creds = malloc(sizeof(UnsandboxCredentials));
        if (!creds) break;

        creds->public_key = strdup(pk);
        creds->secret_key = strdup(sk);

        if (!creds->public_key || !creds->secret_key) {
            free(creds->public_key);
            free(creds->secret_key);
            free(creds);
            creds = NULL;
        }
        break;
    }

    fclose(f);
    return creds;
}

// Count total accounts in CSV
__attribute__((unused))
static int count_accounts_in_csv(void) {
    char *path = get_accounts_csv_path();
    if (!path) return 0;

    FILE *f = fopen(path, "r");
    free(path);
    if (!f) return 0;

    char line[1024];
    int count = 0;

    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n' || line[0] == '#') continue;
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') line[len - 1] = '\0';
        char *comma = strchr(line, ',');
        if (!comma) continue;
        *comma = '\0';
        if (strncmp(line, "unsb-pk-", 8) == 0 && strncmp(comma + 1, "unsb-sk-", 8) == 0) {
            count++;
        }
    }

    fclose(f);
    return count;
}

// Get credentials using POSIX priority: CLI flags > env vars > CSV file
// cli_pk/cli_sk: from -p/-k flags (can be NULL)
// account_index: from --account flag (-1 means use env var or default to 0)
static UnsandboxCredentials* get_credentials(const char *cli_pk, const char *cli_sk, int account_index) {
    // Priority 1: CLI flags (-p and -k) - highest priority per POSIX convention
    if (cli_pk && cli_sk && strlen(cli_pk) > 0 && strlen(cli_sk) > 0) {
        UnsandboxCredentials *creds = malloc(sizeof(UnsandboxCredentials));
        if (!creds) return NULL;

        creds->public_key = strdup(cli_pk);
        creds->secret_key = strdup(cli_sk);

        if (!creds->public_key || !creds->secret_key) {
            free(creds->public_key);
            free(creds->secret_key);
            free(creds);
            return NULL;
        }
        return creds;
    }

    // Priority 2: Environment variables (keys)
    const char *env_pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *env_sk = getenv("UNSANDBOX_SECRET_KEY");

    if (env_pk && env_sk && strlen(env_pk) > 0 && strlen(env_sk) > 0) {
        UnsandboxCredentials *creds = malloc(sizeof(UnsandboxCredentials));
        if (!creds) return NULL;

        creds->public_key = strdup(env_pk);
        creds->secret_key = strdup(env_sk);

        if (!creds->public_key || !creds->secret_key) {
            free(creds->public_key);
            free(creds->secret_key);
            free(creds);
            return NULL;
        }
        return creds;
    }

    // Priority 3: Config file (~/.unsandbox/accounts.csv)
    // Use account_index from --account flag, or UNSANDBOX_ACCOUNT env var, or default to 0
    int csv_index = account_index;
    if (csv_index < 0) {
        const char *env_account = getenv("UNSANDBOX_ACCOUNT");
        if (env_account && strlen(env_account) > 0) {
            csv_index = atoi(env_account);
        } else {
            csv_index = 0;
        }
    }
    return load_credentials_from_csv(csv_index);
}

static void free_credentials(UnsandboxCredentials *creds) {
    if (!creds) return;
    free(creds->public_key);
    free(creds->secret_key);
    free(creds);
}

// ============================================================================
// End Credentials Management
// ============================================================================

// ============================================================================
// HMAC Auth Headers Helper
// ============================================================================

// Add HMAC authentication headers to a curl_slist
// Returns a new slist with auth headers appended (caller must free with curl_slist_free_all)
// method: "GET", "POST", etc.
// path: e.g., "/execute", "/services", "/sessions"
//
// Adds these headers:
//   Authorization: Bearer <public_key>                                 (identifies account)
//   X-Timestamp: <unix_timestamp>                                      (replay prevention)
//   X-Signature: <HMAC-SHA256(secret_key, timestamp:method:path:body)> (proves secret + body integrity)
// body can be NULL for bodyless requests (GET, DELETE)
static struct curl_slist* add_hmac_auth_headers(struct curl_slist *headers,
                                                  const UnsandboxCredentials *creds,
                                                  const char *method,
                                                  const char *path,
                                                  const char *body) {
    if (!creds || !creds->public_key || !creds->secret_key) return headers;

    long timestamp = (long)time(NULL);
    char *signature = sign_request(creds->secret_key, timestamp, method, path, body);
    if (!signature) return headers;

    char auth_header[256];
    char ts_header[64];
    char sig_header[128];

    // Public key identifies the account (server looks up by key)
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", creds->public_key);
    // Timestamp prevents replay attacks (server checks ±5 minutes)
    snprintf(ts_header, sizeof(ts_header), "X-Timestamp: %ld", timestamp);
    // Signature proves possession of secret key and body integrity
    snprintf(sig_header, sizeof(sig_header), "X-Signature: %s", signature);

    headers = curl_slist_append(headers, auth_header);
    headers = curl_slist_append(headers, ts_header);
    headers = curl_slist_append(headers, sig_header);

    free(signature);
    return headers;
}

// ============================================================================
// End HMAC Auth Headers Helper
// ============================================================================

// Polling delays (milliseconds) - matches opencompletion.com cadence
// Cumulative: 300ms, 750ms, 1450ms, 2350ms, 3000ms, 4600ms, 6600ms+
static const int POLL_DELAYS[] = {300, 450, 700, 900, 650, 1600, 2000};
#define POLL_DELAYS_COUNT 7

// Response buffer structure
struct ResponseBuffer {
    char *data;
    size_t size;
};

// Input file structure
struct InputFile {
    char *filename;
    char *content_base64;
};

// Environment variable structure
struct EnvVar {
    char *key;
    char *value;
};

// Write callback for libcurl
static size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    struct ResponseBuffer *mem = (struct ResponseBuffer *)userp;

    char *ptr = realloc(mem->data, mem->size + realsize + 1);
    if (!ptr) {
        fprintf(stderr, "Error: out of memory\n");
        return 0;
    }

    mem->data = ptr;
    memcpy(&(mem->data[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->data[mem->size] = 0;

    return realsize;
}

// Base64 encoding table
static const char base64_table[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// Base64 encode
char* base64_encode(const unsigned char *data, size_t input_length, size_t *output_length) {
    *output_length = 4 * ((input_length + 2) / 3);
    char *encoded = malloc(*output_length + 1);
    if (!encoded) return NULL;

    size_t i, j;
    for (i = 0, j = 0; i < input_length;) {
        uint32_t octet_a = i < input_length ? data[i++] : 0;
        uint32_t octet_b = i < input_length ? data[i++] : 0;
        uint32_t octet_c = i < input_length ? data[i++] : 0;
        uint32_t triple = (octet_a << 16) + (octet_b << 8) + octet_c;

        encoded[j++] = base64_table[(triple >> 18) & 0x3F];
        encoded[j++] = base64_table[(triple >> 12) & 0x3F];
        encoded[j++] = base64_table[(triple >> 6) & 0x3F];
        encoded[j++] = base64_table[triple & 0x3F];
    }

    // Add padding
    int mod = input_length % 3;
    if (mod > 0) {
        encoded[*output_length - 1] = '=';
        if (mod == 1) encoded[*output_length - 2] = '=';
    }

    encoded[*output_length] = '\0';
    return encoded;
}

// Base64 decode
unsigned char* base64_decode(const char *data, size_t input_length, size_t *output_length) {
    if (input_length % 4 != 0) return NULL;

    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '=') (*output_length)--;
    if (data[input_length - 2] == '=') (*output_length)--;

    unsigned char *decoded = malloc(*output_length + 1);
    if (!decoded) return NULL;

    int decoding_table[256];
    for (int i = 0; i < 256; i++) decoding_table[i] = -1;
    for (int i = 0; i < 64; i++) decoding_table[(unsigned char)base64_table[i]] = i;

    size_t i, j;
    for (i = 0, j = 0; i < input_length;) {
        uint32_t sextet_a = data[i] == '=' ? 0 : decoding_table[(unsigned char)data[i]]; i++;
        uint32_t sextet_b = data[i] == '=' ? 0 : decoding_table[(unsigned char)data[i]]; i++;
        uint32_t sextet_c = data[i] == '=' ? 0 : decoding_table[(unsigned char)data[i]]; i++;
        uint32_t sextet_d = data[i] == '=' ? 0 : decoding_table[(unsigned char)data[i]]; i++;

        uint32_t triple = (sextet_a << 18) + (sextet_b << 12) + (sextet_c << 6) + sextet_d;

        if (j < *output_length) decoded[j++] = (triple >> 16) & 0xFF;
        if (j < *output_length) decoded[j++] = (triple >> 8) & 0xFF;
        if (j < *output_length) decoded[j++] = triple & 0xFF;
    }

    decoded[*output_length] = '\0';
    return decoded;
}

// Detect language from shebang line
const char* detect_language_from_shebang(const char *code) {
    if (code[0] != '#' || code[1] != '!') return NULL;

    if (strstr(code, "/python") || strstr(code, "/python3") || strstr(code, "/python2")) return "python";
    if (strstr(code, "/node") || strstr(code, "/nodejs")) return "javascript";
    if (strstr(code, "/ruby")) return "ruby";
    if (strstr(code, "/perl")) return "perl";
    if (strstr(code, "/php")) return "php";
    if (strstr(code, "/bash") || strstr(code, "/sh")) return "bash";
    if (strstr(code, "/lua")) return "lua";
    if (strstr(code, "/tclsh") || strstr(code, "/wish")) return "tcl";
    if (strstr(code, "/raku") || strstr(code, "/perl6")) return "raku";
    if (strstr(code, "/julia")) return "julia";
    if (strstr(code, "/Rscript")) return "r";
    if (strstr(code, "/groovy")) return "groovy";
    if (strstr(code, "/scala")) return "scala";
    if (strstr(code, "/swift")) return "swift";
    if (strstr(code, "/racket")) return "racket";
    if (strstr(code, "/scheme") || strstr(code, "/guile")) return "scheme";
    if (strstr(code, "/clisp") || strstr(code, "/sbcl")) return "commonlisp";
    if (strstr(code, "/ocaml")) return "ocaml";
    if (strstr(code, "/elixir")) return "elixir";

    return NULL;
}

// Detect language from file extension
const char* detect_language_from_extension(const char *filename) {
    const char *ext = strrchr(filename, '.');
    if (!ext) return NULL;
    ext++;

    if (strcmp(ext, "py") == 0) return "python";
    if (strcmp(ext, "js") == 0) return "javascript";
    if (strcmp(ext, "ts") == 0) return "typescript";
    if (strcmp(ext, "rb") == 0) return "ruby";
    if (strcmp(ext, "php") == 0) return "php";
    if (strcmp(ext, "pl") == 0) return "perl";
    if (strcmp(ext, "sh") == 0) return "bash";
    if (strcmp(ext, "r") == 0 || strcmp(ext, "R") == 0) return "r";
    if (strcmp(ext, "lua") == 0) return "lua";
    if (strcmp(ext, "go") == 0) return "go";
    if (strcmp(ext, "rs") == 0) return "rust";
    if (strcmp(ext, "c") == 0) return "c";
    if (strcmp(ext, "cpp") == 0 || strcmp(ext, "cc") == 0 || strcmp(ext, "cxx") == 0) return "cpp";
    if (strcmp(ext, "java") == 0) return "java";
    if (strcmp(ext, "kt") == 0) return "kotlin";
    if (strcmp(ext, "m") == 0) return "objc";
    if (strcmp(ext, "cs") == 0) return "csharp";
    if (strcmp(ext, "fs") == 0) return "fsharp";
    if (strcmp(ext, "hs") == 0) return "haskell";
    if (strcmp(ext, "ml") == 0) return "ocaml";
    if (strcmp(ext, "clj") == 0) return "clojure";
    if (strcmp(ext, "scm") == 0 || strcmp(ext, "ss") == 0) return "scheme";
    if (strcmp(ext, "erl") == 0) return "erlang";
    if (strcmp(ext, "ex") == 0 || strcmp(ext, "exs") == 0) return "elixir";
    if (strcmp(ext, "jl") == 0) return "julia";
    if (strcmp(ext, "d") == 0) return "d";
    if (strcmp(ext, "nim") == 0) return "nim";
    if (strcmp(ext, "zig") == 0) return "zig";
    if (strcmp(ext, "v") == 0) return "v";
    if (strcmp(ext, "cr") == 0) return "crystal";
    if (strcmp(ext, "dart") == 0) return "dart";
    if (strcmp(ext, "groovy") == 0) return "groovy";
    if (strcmp(ext, "f90") == 0 || strcmp(ext, "f95") == 0) return "fortran";
    if (strcmp(ext, "lisp") == 0 || strcmp(ext, "lsp") == 0) return "commonlisp";
    if (strcmp(ext, "cob") == 0) return "cobol";
    if (strcmp(ext, "tcl") == 0) return "tcl";
    if (strcmp(ext, "raku") == 0) return "raku";
    if (strcmp(ext, "pro") == 0 || strcmp(ext, "p") == 0) return "prolog";
    if (strcmp(ext, "4th") == 0 || strcmp(ext, "forth") == 0 || strcmp(ext, "fth") == 0) return "forth";

    return NULL;
}

// Read file contents
char* read_file(const char *filename, size_t *size) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "Error: cannot open file '%s'\n", filename);
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (fsize > MAX_FILE_SIZE) {
        fprintf(stderr, "Error: file too large (max %d bytes)\n", MAX_FILE_SIZE);
        fclose(f);
        return NULL;
    }

    char *content = malloc(fsize + 1);
    if (!content) {
        fprintf(stderr, "Error: out of memory\n");
        fclose(f);
        return NULL;
    }

    size_t read_size = fread(content, 1, fsize, f);
    content[read_size] = 0;
    *size = read_size;

    fclose(f);
    return content;
}

// Escape JSON string
char* escape_json_string(const char *str) {
    size_t len = strlen(str);
    char *escaped = malloc(len * 6 + 1); // Worst case: \uXXXX for each char
    if (!escaped) return NULL;

    char *out = escaped;
    for (size_t i = 0; i < len; i++) {
        switch (str[i]) {
            case '"':  *out++ = '\\'; *out++ = '"'; break;
            case '\\': *out++ = '\\'; *out++ = '\\'; break;
            case '\b': *out++ = '\\'; *out++ = 'b'; break;
            case '\f': *out++ = '\\'; *out++ = 'f'; break;
            case '\n': *out++ = '\\'; *out++ = 'n'; break;
            case '\r': *out++ = '\\'; *out++ = 'r'; break;
            case '\t': *out++ = '\\'; *out++ = 't'; break;
            default:
                if ((unsigned char)str[i] < 32) {
                    sprintf(out, "\\u%04x", (unsigned char)str[i]);
                    out += 6;
                } else {
                    *out++ = str[i];
                }
                break;
        }
    }
    *out = 0;
    return escaped;
}

// Extract JSON string value (simple parser)
char* extract_json_string(const char *json, const char *key) {
    char search[256];
    snprintf(search, sizeof(search), "\"%s\":\"", key);
    const char *start = strstr(json, search);
    if (!start) return NULL;

    start += strlen(search);
    const char *end = start;

    while (*end && !(*end == '"' && *(end - 1) != '\\')) {
        end++;
    }

    size_t len = end - start;
    char *result = malloc(len + 1);
    if (!result) return NULL;

    // Unescape while copying
    char *out = result;
    for (const char *p = start; p < end; p++) {
        if (*p == '\\' && p + 1 < end) {
            p++;
            switch (*p) {
                case 'n': *out++ = '\n'; break;
                case 't': *out++ = '\t'; break;
                case 'r': *out++ = '\r'; break;
                case '\\': *out++ = '\\'; break;
                case '"': *out++ = '"'; break;
                default: *out++ = *p; break;
            }
        } else {
            *out++ = *p;
        }
    }
    *out = 0;
    return result;
}

// Extract JSON number value (returns -1 if not found or null)
long long extract_json_number(const char *json, const char *key) {
    char search[256];
    snprintf(search, sizeof(search), "\"%s\":", key);
    const char *start = strstr(json, search);
    if (!start) return -1;

    start += strlen(search);
    // Skip whitespace
    while (*start == ' ' || *start == '\t') start++;

    // Check for null
    if (strncmp(start, "null", 4) == 0) return -1;

    return atoll(start);
}

// ============================================================================
// JSON Array Helpers (for Library API)
// ============================================================================

// Count objects in a JSON array (for malloc sizing)
// Returns number of top-level objects in array, or 0 if not found/empty
__attribute__((unused))
static int count_json_array_objects(const char *json, const char *array_key) {
    char search[256];
    snprintf(search, sizeof(search), "\"%s\":[", array_key);
    const char *start = strstr(json, search);
    if (!start) return 0;

    start += strlen(search);
    int count = 0, depth = 0;
    for (const char *p = start; *p && !(*p == ']' && depth == 0); p++) {
        if (*p == '{') {
            if (depth == 0) count++;
            depth++;
        } else if (*p == '}') {
            depth--;
        } else if (*p == '"') {
            // Skip string contents (may contain { } characters)
            p++;
            while (*p && !(*p == '"' && *(p-1) != '\\')) p++;
        }
    }
    return count;
}

// Skip past current JSON object, return pointer to position after closing }
// If pos doesn't point to '{', returns pos unchanged
__attribute__((unused))
static const char* skip_json_object(const char *pos) {
    if (!pos || *pos != '{') return pos;
    int depth = 1;
    pos++;
    while (*pos && depth > 0) {
        if (*pos == '{') {
            depth++;
        } else if (*pos == '}') {
            depth--;
        } else if (*pos == '"') {
            // Skip string contents
            pos++;
            while (*pos && !(*pos == '"' && *(pos-1) != '\\')) pos++;
        }
        pos++;
    }
    return pos;
}

// ============================================================================
// Thread-Local Error Storage (for Library API)
// ============================================================================

static __thread char unsandbox_error_buffer[512] = {0};

__attribute__((unused))
static void set_last_error(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsnprintf(unsandbox_error_buffer, sizeof(unsandbox_error_buffer), fmt, args);
    va_end(args);
}

// Format bytes to human readable (e.g., 1234567 -> "1.2M")
void format_bytes(long long bytes, char *buf, size_t bufsize) {
    if (bytes < 0) {
        snprintf(buf, bufsize, "-");
    } else if (bytes < 1024) {
        snprintf(buf, bufsize, "%lldB", bytes);
    } else if (bytes < 1024 * 1024) {
        snprintf(buf, bufsize, "%.1fK", bytes / 1024.0);
    } else if (bytes < 1024LL * 1024 * 1024) {
        snprintf(buf, bufsize, "%.1fM", bytes / (1024.0 * 1024));
    } else {
        snprintf(buf, bufsize, "%.1fG", bytes / (1024.0 * 1024 * 1024));
    }
}

// Get basename without extension
char* get_basename_no_ext(const char *path) {
    const char *base = strrchr(path, '/');
    base = base ? base + 1 : path;

    char *result = strdup(base);
    char *dot = strrchr(result, '.');
    if (dot) *dot = '\0';
    return result;
}

// Parse and handle response
void parse_and_print_response(const char *json_response, int save_artifacts, const char *artifact_dir, const char *source_file) {
    // Print stdout in blue
    char *out = extract_json_string(json_response, "stdout");
    if (out && strlen(out) > 0) {
        printf("\033[34m%s\033[0m", out);
        free(out);
    }

    // Print stderr in red
    char *err = extract_json_string(json_response, "stderr");
    if (err && strlen(err) > 0) {
        fprintf(stderr, "\033[31m%s\033[0m", err);
        free(err);
    }

    // Print API error in bold red
    char *error = extract_json_string(json_response, "error");
    if (error && strlen(error) > 0) {
        fprintf(stderr, "\033[1;31mError: %s\033[0m\n", error);
        free(error);
    }

    // Handle artifacts - API returns "artifacts":[{"filename":"...", "content_base64":"..."}]
    if (save_artifacts) {
        const char *artifacts_start = strstr(json_response, "\"artifacts\":[");
        if (artifacts_start) {
            const char *pos = artifacts_start + 13; // Skip "artifacts":[

            // Process each artifact in array
            while ((pos = strchr(pos, '{')) != NULL) {
                char *artifact_data = extract_json_string(pos, "content_base64");
                char *artifact_filename = extract_json_string(pos, "filename");

                if (artifact_data) {
                    size_t decoded_len;
                    unsigned char *decoded = base64_decode(artifact_data, strlen(artifact_data), &decoded_len);

                    if (decoded) {
                        char output_path[512];
                        const char *fname = artifact_filename ? artifact_filename : "a.out";

                        // For executables, use source filename instead of API filename
                        char *source_basename = NULL;
                        int is_elf = decoded_len >= 4 && decoded[0] == 0x7F &&
                                     decoded[1] == 'E' && decoded[2] == 'L' && decoded[3] == 'F';
                        int is_pe = decoded_len >= 2 && decoded[0] == 'M' && decoded[1] == 'Z';
                        int is_macho = decoded_len >= 4 &&
                                       ((decoded[0] == 0xCF && decoded[1] == 0xFA) ||
                                        (decoded[0] == 0xFE && decoded[1] == 0xED));

                        if ((is_elf || is_pe || is_macho) && source_file) {
                            source_basename = get_basename_no_ext(source_file);
                            fname = source_basename;
                        }

                        if (artifact_dir) {
                            snprintf(output_path, sizeof(output_path), "%s/%s", artifact_dir, fname);
                        } else {
                            snprintf(output_path, sizeof(output_path), "%s", fname);
                        }

                        FILE *f = fopen(output_path, "wb");
                        if (f) {
                            fwrite(decoded, 1, decoded_len, f);
                            fclose(f);

                            // Check magic bytes to determine if executable
                            int is_executable = 0;
                            int is_data = 0;
                            if (decoded_len >= 4) {
                                // Executable formats
                                if (decoded[0] == 0x7F && decoded[1] == 'E' &&
                                    decoded[2] == 'L' && decoded[3] == 'F') {
                                    is_executable = 1;  // ELF
                                } else if (decoded[0] == 'M' && decoded[1] == 'Z') {
                                    is_executable = 1;  // PE/Windows
                                } else if ((decoded[0] == 0xCF && decoded[1] == 0xFA) ||
                                           (decoded[0] == 0xFE && decoded[1] == 0xED)) {
                                    is_executable = 1;  // Mach-O
                                } else if (decoded[0] == '#' && decoded[1] == '!') {
                                    is_executable = 1;  // Shebang script
                                }
                                // Data formats - don't make executable
                                else if (decoded[0] == 0x89 && decoded[1] == 'P' &&
                                         decoded[2] == 'N' && decoded[3] == 'G') {
                                    is_data = 1;  // PNG
                                } else if (decoded[0] == 0xFF && decoded[1] == 0xD8) {
                                    is_data = 1;  // JPEG
                                } else if (decoded[0] == 'G' && decoded[1] == 'I' &&
                                           decoded[2] == 'F' && decoded[3] == '8') {
                                    is_data = 1;  // GIF
                                } else if (decoded[0] == '%' && decoded[1] == 'P' &&
                                           decoded[2] == 'D' && decoded[3] == 'F') {
                                    is_data = 1;  // PDF
                                } else if (decoded[0] == 'P' && decoded[1] == 'K' &&
                                           decoded[2] == 0x03 && decoded[3] == 0x04) {
                                    is_data = 1;  // ZIP
                                } else if (decoded[0] == 0x1F && decoded[1] == 0x8B) {
                                    is_data = 1;  // GZIP
                                } else if (decoded[0] == '{' || decoded[0] == '[') {
                                    is_data = 1;  // JSON
                                } else if (decoded[0] == '<') {
                                    is_data = 1;  // XML/HTML
                                }
                            }

                            // Fall back to extension if magic bytes inconclusive
                            if (!is_executable && !is_data) {
                                const char *ext = strrchr(fname, '.');
                                if (ext) {
                                    is_data = (
                                        // Images
                                        strcmp(ext, ".png") == 0 || strcmp(ext, ".jpg") == 0 ||
                                        strcmp(ext, ".jpeg") == 0 || strcmp(ext, ".gif") == 0 ||
                                        strcmp(ext, ".svg") == 0 || strcmp(ext, ".webp") == 0 ||
                                        strcmp(ext, ".bmp") == 0 || strcmp(ext, ".ico") == 0 ||
                                        strcmp(ext, ".tiff") == 0 || strcmp(ext, ".tif") == 0 ||
                                        strcmp(ext, ".psd") == 0 || strcmp(ext, ".ai") == 0 ||
                                        strcmp(ext, ".eps") == 0 || strcmp(ext, ".raw") == 0 ||
                                        // Documents
                                        strcmp(ext, ".pdf") == 0 || strcmp(ext, ".doc") == 0 ||
                                        strcmp(ext, ".docx") == 0 || strcmp(ext, ".xls") == 0 ||
                                        strcmp(ext, ".xlsx") == 0 || strcmp(ext, ".ppt") == 0 ||
                                        strcmp(ext, ".pptx") == 0 || strcmp(ext, ".odt") == 0 ||
                                        strcmp(ext, ".ods") == 0 || strcmp(ext, ".odp") == 0 ||
                                        strcmp(ext, ".rtf") == 0 || strcmp(ext, ".tex") == 0 ||
                                        // Text/Config
                                        strcmp(ext, ".txt") == 0 || strcmp(ext, ".md") == 0 ||
                                        strcmp(ext, ".rst") == 0 || strcmp(ext, ".log") == 0 ||
                                        strcmp(ext, ".json") == 0 || strcmp(ext, ".xml") == 0 ||
                                        strcmp(ext, ".yaml") == 0 || strcmp(ext, ".yml") == 0 ||
                                        strcmp(ext, ".toml") == 0 || strcmp(ext, ".ini") == 0 ||
                                        strcmp(ext, ".conf") == 0 || strcmp(ext, ".cfg") == 0 ||
                                        strcmp(ext, ".config") == 0 || strcmp(ext, ".env") == 0 ||
                                        strcmp(ext, ".properties") == 0 || strcmp(ext, ".plist") == 0 ||
                                        // Data
                                        strcmp(ext, ".csv") == 0 || strcmp(ext, ".tsv") == 0 ||
                                        strcmp(ext, ".sql") == 0 || strcmp(ext, ".db") == 0 ||
                                        strcmp(ext, ".sqlite") == 0 || strcmp(ext, ".parquet") == 0 ||
                                        strcmp(ext, ".avro") == 0 || strcmp(ext, ".npy") == 0 ||
                                        strcmp(ext, ".npz") == 0 || strcmp(ext, ".pkl") == 0 ||
                                        strcmp(ext, ".pickle") == 0 || strcmp(ext, ".h5") == 0 ||
                                        strcmp(ext, ".hdf5") == 0 ||
                                        // Web
                                        strcmp(ext, ".html") == 0 || strcmp(ext, ".htm") == 0 ||
                                        strcmp(ext, ".css") == 0 || strcmp(ext, ".scss") == 0 ||
                                        strcmp(ext, ".sass") == 0 || strcmp(ext, ".less") == 0 ||
                                        strcmp(ext, ".woff") == 0 || strcmp(ext, ".woff2") == 0 ||
                                        strcmp(ext, ".ttf") == 0 || strcmp(ext, ".otf") == 0 ||
                                        strcmp(ext, ".eot") == 0 ||
                                        // Archives
                                        strcmp(ext, ".zip") == 0 || strcmp(ext, ".tar") == 0 ||
                                        strcmp(ext, ".gz") == 0 || strcmp(ext, ".tgz") == 0 ||
                                        strcmp(ext, ".bz2") == 0 || strcmp(ext, ".xz") == 0 ||
                                        strcmp(ext, ".7z") == 0 || strcmp(ext, ".rar") == 0 ||
                                        strcmp(ext, ".zst") == 0 ||
                                        // Audio
                                        strcmp(ext, ".mp3") == 0 || strcmp(ext, ".wav") == 0 ||
                                        strcmp(ext, ".flac") == 0 || strcmp(ext, ".aac") == 0 ||
                                        strcmp(ext, ".ogg") == 0 || strcmp(ext, ".m4a") == 0 ||
                                        strcmp(ext, ".wma") == 0 || strcmp(ext, ".aiff") == 0 ||
                                        // Video
                                        strcmp(ext, ".mp4") == 0 || strcmp(ext, ".mkv") == 0 ||
                                        strcmp(ext, ".avi") == 0 || strcmp(ext, ".mov") == 0 ||
                                        strcmp(ext, ".wmv") == 0 || strcmp(ext, ".flv") == 0 ||
                                        strcmp(ext, ".webm") == 0 || strcmp(ext, ".m4v") == 0 ||
                                        // 3D/CAD
                                        strcmp(ext, ".obj") == 0 || strcmp(ext, ".stl") == 0 ||
                                        strcmp(ext, ".fbx") == 0 || strcmp(ext, ".gltf") == 0 ||
                                        strcmp(ext, ".glb") == 0 ||
                                        // Misc
                                        strcmp(ext, ".lock") == 0 || strcmp(ext, ".sum") == 0 ||
                                        strcmp(ext, ".map") == 0 || strcmp(ext, ".wasm") == 0
                                    );
                                }
                            }

                            if (is_executable || !is_data) {
                                chmod(output_path, 0755);
                            }
                            fprintf(stderr, "\033[32mArtifact saved: %s (%zu bytes)\033[0m\n", output_path, decoded_len);
                        }
                        if (source_basename) free(source_basename);
                        free(decoded);
                    }
                    free(artifact_data);
                }
                if (artifact_filename) free(artifact_filename);

                // Move to next object
                pos++;
                const char *next_obj = strchr(pos, '{');
                const char *end_arr = strchr(pos, ']');
                if (!next_obj || (end_arr && end_arr < next_obj)) break;
                pos = next_obj;
            }
        }
    }
}

// Get basename from path
const char* get_basename(const char *path) {
    const char *base = strrchr(path, '/');
    return base ? base + 1 : path;
}

// Poll job status with exponential backoff
// Returns the final response JSON (caller must free), or NULL on error
static char* poll_job_status(const UnsandboxCredentials *creds, const char *job_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    char url[512];
    char path[256];
    snprintf(path, sizeof(path), "/jobs/%s", job_id);
    snprintf(url, sizeof(url), "%s%s", API_BASE, path);

    int poll_count = 0;
    char *final_response = NULL;

    while (1) {
        // Sleep before polling (except first iteration handled by caller)
        int delay_idx = poll_count < POLL_DELAYS_COUNT ? poll_count : POLL_DELAYS_COUNT - 1;
        usleep(POLL_DELAYS[delay_idx] * 1000);
        poll_count++;

        struct ResponseBuffer response = {0};
        response.data = malloc(1);
        response.size = 0;

        // Regenerate auth headers each poll to keep timestamp fresh
        struct curl_slist *headers = NULL;
        headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

        CURLcode res = curl_easy_perform(curl);
        curl_slist_free_all(headers);

        if (res != CURLE_OK) {
            fprintf(stderr, "Error polling job: %s\n", curl_easy_strerror(res));
            free(response.data);
            break;
        }

        long http_code = 0;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

        if (http_code == 404) {
            fprintf(stderr, "Error: job not found\n");
            free(response.data);
            break;
        }

        if (http_code != 200) {
            fprintf(stderr, "Error: HTTP %ld while polling job\n", http_code);
            free(response.data);
            break;
        }

        // Check status field
        char *status = extract_json_string(response.data, "status");
        if (!status) {
            // No status field - might be final result format
            final_response = response.data;
            break;
        }

        int is_terminal = (strcmp(status, "completed") == 0 ||
                          strcmp(status, "failed") == 0 ||
                          strcmp(status, "timeout") == 0 ||
                          strcmp(status, "cancelled") == 0);
        free(status);

        if (is_terminal) {
            final_response = response.data;
            break;
        }

        // Still running - continue polling
        free(response.data);
    }

    curl_easy_cleanup(curl);
    return final_response;
}

// ============================================================================
// Interactive Shell Support
// ============================================================================

static struct termios orig_termios;
static int shell_running = 0;
static struct lws *shell_wsi = NULL;

// Terminal size
static void get_terminal_size(int *cols, int *rows) {
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0) {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
    } else {
        *cols = 80;
        *rows = 24;
    }
}

// Raw terminal mode
static void enable_raw_mode(void) {
    tcgetattr(STDIN_FILENO, &orig_termios);
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
    raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

static void disable_raw_mode(void) {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

// Signal handler for terminal resize
static void handle_sigwinch(int sig) {
    (void)sig;
    // Flag set - resize sent in main loop
}

// Signal handler for interrupt
static void handle_sigint(int sig) {
    (void)sig;
    shell_running = 0;
}

// WebSocket shell state
struct shell_state {
    char *session_id;
    int connected;
    unsigned char *send_buf;
    size_t send_len;
    int need_resize;
    int detached;  // 1 if session was detached (can reconnect), 0 if ended
    int need_initial_enter;  // 1 to send Enter after connect (tmux repaint fix)
};

// WebSocket callback for shell
static int shell_ws_callback(struct lws *wsi, enum lws_callback_reasons reason,
                              void *user, void *in, size_t len) {
    struct shell_state *state = (struct shell_state *)user;

    switch (reason) {
    case LWS_CALLBACK_CLIENT_ESTABLISHED:
        state->connected = 1;
        state->need_resize = 1;  // Send initial resize
        lws_callback_on_writable(wsi);
        break;

    case LWS_CALLBACK_CLIENT_RECEIVE:
        if (in && len > 0) {
            // Check if it's a binary frame (raw stdout) or text frame (JSON control)
            if (lws_frame_is_binary(wsi)) {
                // Binary frame = raw shell output, write directly
                if (write(STDOUT_FILENO, in, len) < 0) { /* ignore */ }
            } else {
                // Text frame = JSON control message (exit, error, detached, etc.)
                char *json = (char *)in;
                if (strstr(json, "\"type\":\"exit\"")) {
                    shell_running = 0;
                    state->detached = 0;  // Session ended
                } else if (strstr(json, "\"type\":\"detached\"")) {
                    shell_running = 0;
                    state->detached = 1;  // Detached, can reconnect
                }
            }
        }
        break;

    case LWS_CALLBACK_CLIENT_WRITEABLE:
        if (!state->connected) break;

        // Send resize if needed
        if (state->need_resize) {
            int cols, rows;
            get_terminal_size(&cols, &rows);
            char msg[128];
            int mlen = snprintf(msg, sizeof(msg),
                "{\"type\":\"resize\",\"cols\":%d,\"rows\":%d}", cols, rows);
            unsigned char buf[LWS_PRE + 128];
            memcpy(&buf[LWS_PRE], msg, mlen);
            lws_write(wsi, &buf[LWS_PRE], mlen, LWS_WRITE_TEXT);
            state->need_resize = 0;
            lws_callback_on_writable(wsi);
            break;
        }

        // Send initial Enter to force tmux repaint (fixes blank screen on connect)
        if (state->need_initial_enter) {
            unsigned char buf[LWS_PRE + 1];
            buf[LWS_PRE] = '\n';
            lws_write(wsi, &buf[LWS_PRE], 1, LWS_WRITE_BINARY);
            state->need_initial_enter = 0;
            break;
        }

        // Send stdin data as binary frame (fast path, no JSON encoding)
        if (state->send_buf && state->send_len > 0) {
            unsigned char buf[LWS_PRE + 256];
            memcpy(&buf[LWS_PRE], state->send_buf, state->send_len);
            lws_write(wsi, &buf[LWS_PRE], state->send_len, LWS_WRITE_BINARY);

            free(state->send_buf);
            state->send_buf = NULL;
            state->send_len = 0;
        }
        break;

    case LWS_CALLBACK_CLIENT_CONNECTION_ERROR:
        fprintf(stderr, "\r\nConnection error: %s\r\n", in ? (char *)in : "unknown");
        shell_running = 0;
        break;

    case LWS_CALLBACK_CLIENT_CLOSED:
        shell_running = 0;
        break;

    default:
        break;
    }
    return 0;
}

static const struct lws_protocols shell_protocols[] = {
    {"unsandbox-shell", shell_ws_callback, sizeof(struct shell_state), 4096, 0, NULL, 0},
    {NULL, NULL, 0, 0, 0, NULL, 0}
};

// Session info returned from create_session
struct SessionInfo {
    char *session_id;
    char *container_name;
};

// Find session ID by container name (for reconnect by container name)
static char* find_session_by_container(const UnsandboxCredentials *creds, const char *container_name) {
    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions", API_BASE);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/sessions", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK || !response.data) {
        free(response.data);
        return NULL;
    }

    // Search for matching container name in sessions array
    const char *sessions_start = strstr(response.data, "\"sessions\":[");
    if (sessions_start) {
        const char *pos = sessions_start + 12;

        while ((pos = strchr(pos, '{')) != NULL) {
            char *container = extract_json_string(pos, "container_name");
            char *session_id = extract_json_string(pos, "id");

            if (container && strcmp(container, container_name) == 0) {
                free(container);
                free(response.data);
                return session_id;  // Found it!
            }

            if (container) free(container);
            if (session_id) free(session_id);

            pos++;
            const char *next_obj = strchr(pos, '{');
            const char *end_arr = strchr(pos, ']');
            if (!next_obj || (end_arr && end_arr < next_obj)) break;
            pos = next_obj;
        }
    }

    free(response.data);
    return NULL;
}

// Kill a session by ID or container name
static int kill_session(const UnsandboxCredentials *creds, const char *session_id_or_container) {
    char *session_id = NULL;

    // Check if it looks like a container name or session ID
    // Container names: unsb-vm-*, exec-*, sandbox-* (legacy)
    if (strncmp(session_id_or_container, "unsb-vm-", 8) == 0 ||
        strncmp(session_id_or_container, "exec-", 5) == 0 ||
        strncmp(session_id_or_container, "sandbox-", 8) == 0) {
        // It's a container name - look up the session ID
        fprintf(stderr, "Looking up session for %s...", session_id_or_container);
        fflush(stderr);
        session_id = find_session_by_container(creds, session_id_or_container);
        if (!session_id) {
            fprintf(stderr, " not found\nError: No active session for container '%s'\n", session_id_or_container);
            return 1;
        }
        fprintf(stderr, " found\n");
    } else {
        session_id = strdup(session_id_or_container);
    }

    fprintf(stderr, "Terminating session %s...", session_id);
    fflush(stderr);

    CURL *curl = curl_easy_init();
    if (!curl) {
        free(session_id);
        return 1;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(url, sizeof(url), "%s/sessions/%s", API_BASE, session_id);
    snprintf(path, sizeof(path), "/sessions/%s", session_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " failed\nError: %s\n", curl_easy_strerror(res));
        free(response.data);
        free(session_id);
        return 1;
    }

    if (http_code == 200) {
        fprintf(stderr, " done\n");
        fprintf(stderr, "\033[32mSession terminated successfully\033[0m\n");
    } else if (http_code == 404) {
        fprintf(stderr, " not found\nError: Session not found or already terminated\n");
        free(response.data);
        free(session_id);
        return 1;
    } else {
        fprintf(stderr, " failed\nError: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        free(session_id);
        return 1;
    }

    free(response.data);
    free(session_id);
    return 0;
}

// Freeze a session
static int freeze_session(const UnsandboxCredentials *creds, const char *session_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(url, sizeof(url), "%s/sessions/%s/freeze", API_BASE, session_id);
    snprintf(path, sizeof(path), "/sessions/%s/freeze", session_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Session not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 400) {
        // Parse error message from response
        fprintf(stderr, "Error: %s\n", response.data);
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    fprintf(stderr, "\033[32mSession frozen successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Unfreeze a session
static int unfreeze_session(const UnsandboxCredentials *creds, const char *session_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(url, sizeof(url), "%s/sessions/%s/unfreeze", API_BASE, session_id);
    snprintf(path, sizeof(path), "/sessions/%s/unfreeze", session_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Session not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 429) {
        // Concurrency limit reached
        fprintf(stderr, "Error: Concurrency limit reached - cannot unfreeze session\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mSession woken successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Boost a session's resources (increase vCPU, memory is derived: vcpu * 2048MB)
static int boost_session(const UnsandboxCredentials *creds, const char *session_id, int vcpu) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(url, sizeof(url), "%s/sessions/%s/boost", API_BASE, session_id);
    snprintf(path, sizeof(path), "/sessions/%s/boost", session_id);

    char post_data[256];
    snprintf(post_data, sizeof(post_data), "{\"vcpu\":%d}", vcpu);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, post_data);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_data);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Session not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 429) {
        fprintf(stderr, "Error: Not enough concurrency slots to boost (boost consumes additional slots)\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    int memory_mb = vcpu * 2048;
    printf("\033[32mSession boosted to %d vCPU, %d MB RAM\033[0m\n", vcpu, memory_mb);
    free(response.data);
    return 0;
}

// Remove boost from a session (return to base resources)
static int unboost_session(const UnsandboxCredentials *creds, const char *session_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(url, sizeof(url), "%s/sessions/%s/unboost", API_BASE, session_id);
    snprintf(path, sizeof(path), "/sessions/%s/unboost", session_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Session not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mSession unboosted, returning to base resources\033[0m\n");
    free(response.data);
    return 0;
}

// List active sessions
static int list_sessions(const UnsandboxCredentials *creds) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions", API_BASE);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/sessions", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse and display sessions
    // Format: {"sessions":[...], "count": N}
    if (!response.data || !strstr(response.data, "\"sessions\"")) {
        fprintf(stderr, "Error: Invalid response\n");
        free(response.data);
        return 1;
    }

    // Extract count
    const char *count_str = strstr(response.data, "\"count\":");
    int count = 0;
    if (count_str) {
        count = atoi(count_str + 8);
    }

    if (count == 0) {
        printf("No active sessions\n");
        free(response.data);
        return 0;
    }

    printf("Active sessions: %d\n\n", count);
    printf("%-40s %-20s %-10s %-8s %-10s\n", "SESSION ID", "CONTAINER", "SHELL", "TTL", "STATUS");
    printf("%-40s %-20s %-10s %-8s %-10s\n", "----------------------------------------",
           "--------------------", "----------", "--------", "----------");

    // Parse sessions array - simple parser for [{...}, {...}]
    const char *sessions_start = strstr(response.data, "\"sessions\":[");
    if (sessions_start) {
        const char *pos = sessions_start + 12;

        while ((pos = strchr(pos, '{')) != NULL) {
            char *session_id = extract_json_string(pos, "id");
            char *container = extract_json_string(pos, "container_name");
            char *shell = extract_json_string(pos, "shell");
            char *status = extract_json_string(pos, "status");

            // Extract remaining_ttl (numeric)
            int remaining_ttl = 0;
            const char *ttl_str = strstr(pos, "\"remaining_ttl\":");
            if (ttl_str) {
                remaining_ttl = atoi(ttl_str + 16);
            }

            // Format TTL as human-readable
            char ttl_fmt[16];
            if (remaining_ttl >= 3600) {
                snprintf(ttl_fmt, sizeof(ttl_fmt), "%dh%dm", remaining_ttl / 3600, (remaining_ttl % 3600) / 60);
            } else if (remaining_ttl >= 60) {
                snprintf(ttl_fmt, sizeof(ttl_fmt), "%dm%ds", remaining_ttl / 60, remaining_ttl % 60);
            } else {
                snprintf(ttl_fmt, sizeof(ttl_fmt), "%ds", remaining_ttl);
            }

            printf("%-40s %-20s %-10s %-8s %-10s\n",
                   session_id ? session_id : "-",
                   container ? container : "-",
                   shell ? shell : "bash",
                   ttl_fmt,
                   status ? status : "-");

            if (session_id) free(session_id);
            if (container) free(container);
            if (shell) free(shell);
            if (status) free(status);

            // Move to next object
            pos++;
            const char *next_obj = strchr(pos, '{');
            const char *end_arr = strchr(pos, ']');
            if (!next_obj || (end_arr && end_arr < next_obj)) break;
            pos = next_obj;
        }
    }

    free(response.data);
    return 0;
}

// Get path to languages cache file (~/.unsandbox/languages.json)
static char* get_languages_cache_path(void) {
    const char *home = getenv("HOME");
    if (!home) {
        struct passwd *pw = getpwuid(getuid());
        if (pw) home = pw->pw_dir;
    }
    if (!home) return NULL;

    char *path = malloc(strlen(home) + 32);
    if (!path) return NULL;
    sprintf(path, "%s/.unsandbox/languages.json", home);
    return path;
}

// Load languages from cache if valid (< 1 hour old)
// Returns JSON string with languages array, or NULL if cache invalid/missing
static char* load_languages_cache(void) {
    char *cache_path = get_languages_cache_path();
    if (!cache_path) return NULL;

    struct stat st;
    if (stat(cache_path, &st) != 0) {
        free(cache_path);
        return NULL;
    }

    // Check if cache is fresh (< 1 hour old)
    time_t now = time(NULL);
    if (now - st.st_mtime >= LANGUAGES_CACHE_TTL) {
        free(cache_path);
        return NULL;
    }

    FILE *f = fopen(cache_path, "r");
    free(cache_path);
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size <= 0 || size > 1024 * 1024) {  // Sanity check: max 1MB
        fclose(f);
        return NULL;
    }

    char *data = malloc(size + 1);
    if (!data) {
        fclose(f);
        return NULL;
    }

    size_t read = fread(data, 1, size, f);
    fclose(f);
    data[read] = '\0';

    // Verify it contains languages array
    if (!strstr(data, "\"languages\"")) {
        free(data);
        return NULL;
    }

    return data;
}

// Save languages to cache
static void save_languages_cache(const char *json_response) {
    if (!json_response) return;

    char *cache_path = get_languages_cache_path();
    if (!cache_path) return;

    // Ensure ~/.unsandbox directory exists
    char *dir = strdup(cache_path);
    if (dir) {
        char *last_slash = strrchr(dir, '/');
        if (last_slash) {
            *last_slash = '\0';
            mkdir(dir, 0700);
        }
        free(dir);
    }

    FILE *f = fopen(cache_path, "w");
    free(cache_path);
    if (!f) return;

    // Extract just the languages array if present, wrap in standard format
    const char *langs_start = strstr(json_response, "\"languages\":[");
    if (langs_start) {
        const char *arr_start = strchr(langs_start, '[');
        const char *arr_end = strchr(arr_start, ']');
        if (arr_start && arr_end) {
            fprintf(f, "{\"languages\":%.*s,\"timestamp\":%ld}",
                    (int)(arr_end - arr_start + 1), arr_start, (long)time(NULL));
        }
    } else {
        // Try to find raw array
        const char *arr_start = strchr(json_response, '[');
        const char *arr_end = arr_start ? strchr(arr_start, ']') : NULL;
        if (arr_start && arr_end) {
            fprintf(f, "{\"languages\":%.*s,\"timestamp\":%ld}",
                    (int)(arr_end - arr_start + 1), arr_start, (long)time(NULL));
        }
    }

    fclose(f);
}

// List supported languages (CLI helper)
static int list_languages_cli(const UnsandboxCredentials *creds, int json_output) {
    // Try cache first
    char *cached = load_languages_cache();
    if (cached) {
        const char *langs_start = strstr(cached, "\"languages\":[");
        if (!langs_start) langs_start = strchr(cached, '[');

        if (langs_start) {
            const char *arr_start = strchr(langs_start, '[');
            if (arr_start) {
                if (json_output) {
                    const char *arr_end = strchr(arr_start, ']');
                    if (arr_end) {
                        printf("%.*s\n", (int)(arr_end - arr_start + 1), arr_start);
                    }
                } else {
                    const char *p = arr_start + 1;
                    while (*p && *p != ']') {
                        if (*p == '"') {
                            p++;
                            const char *end = strchr(p, '"');
                            if (end) {
                                printf("%.*s\n", (int)(end - p), p);
                                p = end + 1;
                            }
                        } else {
                            p++;
                        }
                    }
                }
                free(cached);
                return 0;
            }
        }
        free(cached);
    }

    // Cache miss or invalid - fetch from API
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/languages", API_BASE);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/languages", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK || http_code != 200) {
        fprintf(stderr, "Error: Failed to fetch languages (HTTP %ld)\n", http_code);
        free(response.data);
        return 1;
    }

    // Find the languages array in JSON: "languages":["lang1","lang2",...] or just ["lang1",...]
    const char *langs_start = strstr(response.data, "\"languages\":[");
    if (!langs_start) {
        langs_start = strchr(response.data, '[');
    }

    if (!langs_start) {
        fprintf(stderr, "Error: Invalid response format\n");
        free(response.data);
        return 1;
    }

    const char *arr_start = strchr(langs_start, '[');
    if (!arr_start) {
        fprintf(stderr, "Error: Invalid response format\n");
        free(response.data);
        return 1;
    }

    if (json_output) {
        // Output as JSON array - find matching ]
        const char *arr_end = strchr(arr_start, ']');
        if (arr_end) {
            int len = arr_end - arr_start + 1;
            printf("%.*s\n", len, arr_start);
        }
    } else {
        // Output one language per line
        const char *p = arr_start + 1;
        while (*p && *p != ']') {
            if (*p == '"') {
                p++;
                const char *end = strchr(p, '"');
                if (end) {
                    printf("%.*s\n", (int)(end - p), p);
                    p = end + 1;
                }
            } else {
                p++;
            }
        }
    }

    // Save to cache for future use
    save_languages_cache(response.data);

    free(response.data);
    return 0;
}

// Create a session via HTTP API
// multiplexer: NULL for no multiplexer (default), "screen", or "tmux"
// input_files: array of files to include (written to /tmp/ in container)
// input_file_count: number of input files
static struct SessionInfo create_session(const UnsandboxCredentials *creds, const char *network_mode, int audit, const char *shell, const char *multiplexer, int vcpu, struct InputFile *input_files, int input_file_count) {
    struct SessionInfo info = {NULL, NULL};
    CURL *curl = curl_easy_init();
    if (!curl) return info;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    if (audit) {
        snprintf(url, sizeof(url), "%s/sessions?audit=1", API_BASE);
    } else {
        snprintf(url, sizeof(url), "%s/sessions", API_BASE);
    }

    // Calculate required payload size
    size_t payload_size = 512;  // Base size for JSON structure
    for (int i = 0; i < input_file_count; i++) {
        payload_size += strlen(input_files[i].content_base64) + 256;
    }

    // Build payload with optional shell and multiplexer
    char *payload = malloc(payload_size);
    if (!payload) {
        curl_easy_cleanup(curl);
        free(response.data);
        return info;
    }
    char *p = payload;
    p += sprintf(p, "{\"network_mode\":\"%s\",\"ttl\":3600",
                 network_mode ? network_mode : "zerotrust");
    if (shell && strlen(shell) > 0) {
        p += sprintf(p, ",\"shell\":\"%s\"", shell);
    }
    if (multiplexer && strlen(multiplexer) > 0) {
        p += sprintf(p, ",\"multiplexer\":\"%s\"", multiplexer);
    }
    if (vcpu > 1) {
        p += sprintf(p, ",\"vcpu\":%d", vcpu);
    }
    // Add input files
    if (input_file_count > 0) {
        p += sprintf(p, ",\"input_files\":[");
        for (int i = 0; i < input_file_count; i++) {
            if (i > 0) *p++ = ',';
            char *esc_filename = escape_json_string(input_files[i].filename);
            p += sprintf(p, "{\"filename\":\"%s\",\"content\":\"%s\"}",
                        esc_filename, input_files[i].content_base64);
            free(esc_filename);
        }
        p += sprintf(p, "]");
    }
    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/sessions", payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    // Get HTTP status code before cleanup
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " curl error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return info;
    }

    // Extract session_id and container_name from response
    info.session_id = extract_json_string(response.data, "session_id");
    info.container_name = extract_json_string(response.data, "container_name");
    if (!info.session_id && response.data) {
        // Parse error response for user-friendly message
        char *error = extract_json_string(response.data, "error");
        char *message = extract_json_string(response.data, "message");

        if (http_code == 429 && error) {
            if (strcmp(error, "concurrency_limit_reached") == 0) {
                // Parse active/limit for helpful context
                const char *active_str = strstr(response.data, "\"active_executions\":");
                const char *limit_str = strstr(response.data, "\"concurrency_limit\":");
                int active = active_str ? atoi(active_str + 20) : 0;
                int limit = limit_str ? atoi(limit_str + 20) : 1;

                fprintf(stderr, "\n\n");
                fprintf(stderr, "  \033[1;33mSession limit reached\033[0m\n\n");
                fprintf(stderr, "  You have %d of %d concurrent session%s in use.\n",
                        active, limit, limit == 1 ? "" : "s");
                fprintf(stderr, "\n");
                fprintf(stderr, "  To continue, either:\n");
                fprintf(stderr, "    1. Wait for a running session to finish\n");
                fprintf(stderr, "    2. Run '\033[36mun session --list\033[0m' to see active sessions\n");
                fprintf(stderr, "    3. Run '\033[36mun session --attach <id>\033[0m' to reconnect\n");
                fprintf(stderr, "\n");
            } else if (strcmp(error, "rate_limit_exceeded") == 0) {
                fprintf(stderr, "\n\n");
                fprintf(stderr, "  \033[1;33mRate limit exceeded\033[0m\n\n");
                if (message) {
                    fprintf(stderr, "  %s\n", message);
                } else {
                    fprintf(stderr, "  Too many requests. Please wait a moment and try again.\n");
                }
                fprintf(stderr, "\n");
            } else {
                fprintf(stderr, "\n  \033[1;31mError:\033[0m %s\n", message ? message : error);
            }
        } else if (http_code == 401) {
            fprintf(stderr, "\n\n");
            fprintf(stderr, "  \033[1;31mAuthentication failed\033[0m\n\n");
            // Check if it's a timestamp issue
            if (message && (strstr(message, "timestamp") || strstr(message, "Timestamp"))) {
                fprintf(stderr, "  Request timestamp expired (must be within 5 minutes of server time).\n\n");
                fprintf(stderr, "  \033[1;33mYour computer's clock may have drifted.\033[0m\n");
                fprintf(stderr, "  Check your system time and sync with NTP if needed:\n");
                fprintf(stderr, "    Linux:   sudo ntpdate -s time.nist.gov\n");
                fprintf(stderr, "    macOS:   sudo sntp -sS time.apple.com\n");
                fprintf(stderr, "    Windows: w32tm /resync\n");
            } else {
                fprintf(stderr, "  Your API key is invalid or expired.\n");
                fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY to valid keys.\n");
            }
            fprintf(stderr, "\n");
        } else if (error || message) {
            fprintf(stderr, "\n  \033[1;31mError:\033[0m %s\n", message ? message : error);
        } else {
            fprintf(stderr, " HTTP %ld: %s\n", http_code, response.data);
        }

        if (error) free(error);
        if (message) free(message);
    }
    free(payload);
    free(response.data);
    return info;
}

// Terminate a session and optionally save artifacts
// save_artifacts: 1 to save, 0 to discard
// artifact_dir: directory to save artifacts (NULL for current dir)
// container_name: used to postfix artifact filenames (e.g., bash_history-sandbox-abc123)
static void terminate_session(const UnsandboxCredentials *creds, const char *session_id, int save_artifacts, const char *artifact_dir, int audit_history, const char *container_name) {
    CURL *curl = curl_easy_init();
    if (!curl) return;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    snprintf(path, sizeof(path), "/sessions/%s", session_id);
    if (audit_history) {
        // Request audit - server will copy bash history to /tmp/artifacts before termination
        snprintf(url, sizeof(url), "%s%s?audit=1", API_BASE, path);
    } else {
        snprintf(url, sizeof(url), "%s%s", API_BASE, path);
    }

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res == CURLE_OK && response.data && save_artifacts) {
        // Parse artifacts from response with container name for postfixing filenames
        const char *artifacts_start = strstr(response.data, "\"artifacts\":[");
        if (artifacts_start) {
            const char *pos = artifacts_start + 13; // Skip "artifacts":[

            // Process each artifact in array
            while ((pos = strchr(pos, '{')) != NULL) {
                char *artifact_data = extract_json_string(pos, "content_base64");
                char *artifact_filename = extract_json_string(pos, "filename");

                if (artifact_data) {
                    size_t decoded_len;
                    unsigned char *decoded = base64_decode(artifact_data, strlen(artifact_data), &decoded_len);

                    if (decoded) {
                        char output_path[512];
                        const char *fname = artifact_filename ? artifact_filename : "artifact";

                        // Postfix filename with container name if available
                        // e.g., bash_history -> bash_history-sandbox-abc123
                        char postfixed_name[256];
                        if (container_name) {
                            // Find extension if any
                            const char *ext = strrchr(fname, '.');
                            if (ext) {
                                // Has extension: file.ext -> file-container.ext
                                size_t base_len = ext - fname;
                                snprintf(postfixed_name, sizeof(postfixed_name), "%.*s-%s%s",
                                         (int)base_len, fname, container_name, ext);
                            } else {
                                // No extension: file -> file-container
                                snprintf(postfixed_name, sizeof(postfixed_name), "%s-%s", fname, container_name);
                            }
                            fname = postfixed_name;
                        }

                        if (artifact_dir) {
                            snprintf(output_path, sizeof(output_path), "%s/%s", artifact_dir, fname);
                        } else {
                            snprintf(output_path, sizeof(output_path), "%s", fname);
                        }

                        FILE *f = fopen(output_path, "wb");
                        if (f) {
                            fwrite(decoded, 1, decoded_len, f);
                            fclose(f);
                            fprintf(stderr, "\033[32mArtifact saved: %s (%zu bytes)\033[0m\n", output_path, decoded_len);
                        }
                        free(decoded);
                    }
                    free(artifact_data);
                }
                if (artifact_filename) free(artifact_filename);

                // Move to next object
                pos++;
                const char *next_obj = strchr(pos, '{');
                const char *end_arr = strchr(pos, ']');
                if (!next_obj || (end_arr && end_arr < next_obj)) break;
                pos = next_obj;
            }
        }
    }

    free(response.data);
}

// Reconnect to existing session (shared WebSocket setup with shell_command)
static int reconnect_session(const UnsandboxCredentials *creds, const char *session_id_or_container, int save_artifacts, const char *artifact_dir, int audit_history);

// Main shell command
// multiplexer: NULL for no multiplexer (default), "screen", or "tmux"
// input_files: array of files to include in session (written to /tmp/ in container)
// input_file_count: number of input files
static int shell_command(const UnsandboxCredentials *creds, const char *network_mode, int save_artifacts, const char *artifact_dir, int audit_history, const char *shell, const char *multiplexer, int vcpu, struct InputFile *input_files, int input_file_count) {
    // Disable stdout buffering for real-time output
    setvbuf(stdout, NULL, _IONBF, 0);

    // Create session (pass audit flag to enable script recording on server)
    fprintf(stderr, "Connecting to unsandbox...");
    fflush(stderr);
    struct SessionInfo session = create_session(creds, network_mode, audit_history, shell, multiplexer, vcpu, input_files, input_file_count);
    if (!session.session_id) {
        // Detailed error already printed by create_session
        return 1;
    }
    char *session_id = session.session_id;
    char *container_name = session.container_name;
    fprintf(stderr, " done\n");

    // Set up signal handlers
    signal(SIGWINCH, handle_sigwinch);
    signal(SIGINT, handle_sigint);

    // Enable raw terminal mode
    enable_raw_mode();
    atexit(disable_raw_mode);

    // Disable libwebsockets logging (too noisy)
    lws_set_log_level(0, NULL);

    // Create WebSocket context
    struct lws_context_creation_info info;
    memset(&info, 0, sizeof(info));
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.protocols = shell_protocols;
    info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;

    struct lws_context *context = lws_create_context(&info);
    if (!context) {
        fprintf(stderr, "\r\nError: Failed to create WebSocket context\r\n");
        free(session_id);
        return 1;
    }

    // Connect to WebSocket
    struct lws_client_connect_info ccinfo;
    memset(&ccinfo, 0, sizeof(ccinfo));
    ccinfo.context = context;
    ccinfo.address = "api.unsandbox.com";
    ccinfo.port = 443;

    char path[256];
    snprintf(path, sizeof(path), "/sessions/%s/shell", session_id);
    ccinfo.path = path;

    ccinfo.host = ccinfo.address;
    ccinfo.origin = ccinfo.address;
    ccinfo.protocol = shell_protocols[0].name;
    // LCCSCF_IP_LOW_LATENCY sets TCP_NODELAY to disable Nagle's algorithm
    ccinfo.ssl_connection = LCCSCF_USE_SSL | LCCSCF_IP_LOW_LATENCY;

    shell_wsi = lws_client_connect_via_info(&ccinfo);
    if (!shell_wsi) {
        fprintf(stderr, "\r\nError: Failed to connect to WebSocket\r\n");
        lws_context_destroy(context);
        free(session_id);
        return 1;
    }

    // Get user data from wsi
    struct shell_state *state = (struct shell_state *)lws_wsi_user(shell_wsi);
    state->session_id = session_id;
    // Send initial Enter to force tmux/screen repaint (fixes blank screen on connect)
    if (multiplexer && strlen(multiplexer) > 0) {
        state->need_initial_enter = 1;
    }

    shell_running = 1;

    // Main event loop - poll ONLY on stdin, service websocket separately
    while (shell_running) {
        // Poll stdin with very short timeout (1ms)
        struct pollfd pfd;
        pfd.fd = STDIN_FILENO;
        pfd.events = POLLIN;

        if (poll(&pfd, 1, 1) > 0 && (pfd.revents & POLLIN)) {
            char buf[256];
            ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
            if (n > 0 && state->connected) {
                state->send_buf = malloc(n);
                memcpy(state->send_buf, buf, n);
                state->send_len = n;
                lws_callback_on_writable(shell_wsi);
                // Wake up lws_service if it's sleeping
                lws_cancel_service(context);
            }
        }

        // Service websocket - use NEGATIVE timeout for true non-blocking (lws 3.2+)
        lws_service(context, -1);
    }

    // Cleanup
    disable_raw_mode();
    int was_detached = state->detached;
    lws_context_destroy(context);

    if (was_detached) {
        fprintf(stderr, "\r\n\033[32mSession detached.\033[0m Reconnect with: un session --attach %s\r\n", container_name);
        // Don't terminate - session is still running in multiplexer
    } else {
        fprintf(stderr, "\r\nSession ended.\r\n");
        // Terminate session and collect artifacts (postfix with container name)
        terminate_session(creds, session_id, save_artifacts, artifact_dir, audit_history, container_name);
    }

    // Hint for replaying audit logs (only when session ended, not detached)
    if (audit_history && !was_detached) {
        fprintf(stderr, "\033[33mTip: Replay session with: zcat session.log*.gz | less -R\033[0m\n");
    }
    free(session_id);
    if (container_name) free(container_name);

    return 0;
}

// Reconnect to an existing session by ID or container name
static int reconnect_session(const UnsandboxCredentials *creds, const char *session_id_or_container, int save_artifacts, const char *artifact_dir, int audit_history) {
    // Disable stdout buffering for real-time output
    setvbuf(stdout, NULL, _IONBF, 0);

    char *session_id = NULL;
    char *container_name = NULL;

    // Check if it looks like a container name or session ID
    // Container names: unsb-vm-*, exec-*, sandbox-* (legacy)
    if (strncmp(session_id_or_container, "unsb-vm-", 8) == 0 ||
        strncmp(session_id_or_container, "exec-", 5) == 0 ||
        strncmp(session_id_or_container, "sandbox-", 8) == 0) {
        // It's a container name - look up the session ID
        fprintf(stderr, "Looking up session for %s...", session_id_or_container);
        fflush(stderr);
        session_id = find_session_by_container(creds, session_id_or_container);
        if (!session_id) {
            fprintf(stderr, " not found\nError: No active session for container '%s'\n", session_id_or_container);
            return 1;
        }
        container_name = strdup(session_id_or_container);
        fprintf(stderr, " found\n");
    } else {
        // Assume it's a session ID
        session_id = strdup(session_id_or_container);
    }

    fprintf(stderr, "Reconnecting to session %s...", session_id);
    fflush(stderr);

    // Set up signal handlers
    signal(SIGWINCH, handle_sigwinch);
    signal(SIGINT, handle_sigint);

    // Enable raw terminal mode
    enable_raw_mode();
    atexit(disable_raw_mode);

    // Disable libwebsockets logging
    lws_set_log_level(0, NULL);

    // Create WebSocket context
    struct lws_context_creation_info info;
    memset(&info, 0, sizeof(info));
    info.port = CONTEXT_PORT_NO_LISTEN;
    info.protocols = shell_protocols;
    info.options = LWS_SERVER_OPTION_DO_SSL_GLOBAL_INIT;

    struct lws_context *context = lws_create_context(&info);
    if (!context) {
        fprintf(stderr, " failed\nError: Failed to create WebSocket context\n");
        free(session_id);
        if (container_name) free(container_name);
        return 1;
    }

    // Connect to WebSocket
    struct lws_client_connect_info ccinfo;
    memset(&ccinfo, 0, sizeof(ccinfo));
    ccinfo.context = context;
    ccinfo.address = "api.unsandbox.com";
    ccinfo.port = 443;

    char path[256];
    snprintf(path, sizeof(path), "/sessions/%s/shell", session_id);
    ccinfo.path = path;

    ccinfo.host = ccinfo.address;
    ccinfo.origin = ccinfo.address;
    ccinfo.protocol = shell_protocols[0].name;
    ccinfo.ssl_connection = LCCSCF_USE_SSL | LCCSCF_IP_LOW_LATENCY;

    shell_wsi = lws_client_connect_via_info(&ccinfo);
    if (!shell_wsi) {
        fprintf(stderr, " failed\nError: Failed to connect (session may have expired)\n");
        lws_context_destroy(context);
        free(session_id);
        if (container_name) free(container_name);
        return 1;
    }

    fprintf(stderr, " done\n");

    // Get user data from wsi
    struct shell_state *state = (struct shell_state *)lws_wsi_user(shell_wsi);
    state->session_id = session_id;
    // Always send initial Enter on reconnect (fixes tmux/screen blank screen)
    state->need_initial_enter = 1;

    shell_running = 1;

    // Main event loop
    while (shell_running) {
        struct pollfd pfd;
        pfd.fd = STDIN_FILENO;
        pfd.events = POLLIN;

        if (poll(&pfd, 1, 1) > 0 && (pfd.revents & POLLIN)) {
            char buf[256];
            ssize_t n = read(STDIN_FILENO, buf, sizeof(buf));
            if (n > 0 && state->connected) {
                state->send_buf = malloc(n);
                memcpy(state->send_buf, buf, n);
                state->send_len = n;
                lws_callback_on_writable(shell_wsi);
                lws_cancel_service(context);
            }
        }

        lws_service(context, -1);
    }

    // Cleanup
    disable_raw_mode();
    lws_context_destroy(context);

    fprintf(stderr, "\r\nSession ended.\r\n");

    // Note: We don't terminate the session on reconnect disconnect
    // The session stays alive for future reconnects
    // Only collect artifacts if explicitly requested
    if (save_artifacts || audit_history) {
        terminate_session(creds, session_id, save_artifacts, artifact_dir, audit_history, container_name);
        if (audit_history) {
            fprintf(stderr, "\033[33mTip: Replay session with: zcat session.log*.gz | less -R\033[0m\n");
        }
    } else {
        fprintf(stderr, "\033[33mSession still active. Reconnect with: un session --attach %s\033[0m\n",
                container_name ? container_name : session_id);
    }

    free(session_id);
    if (container_name) free(container_name);

    return 0;
}

// ============================================================================
// End Interactive Shell Support
// ============================================================================

// ============================================================================
// Service Management Support
// ============================================================================

// Get bootstrap logs for a service
// mode: 0 = tail (last 9000 lines), 1 = all logs
static char* get_service_logs(const UnsandboxCredentials *creds, const char *service_id, int all_logs) {
    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    char path[256];
    if (all_logs) {
        snprintf(url, sizeof(url), "%s/services/%s/logs?all=true", API_BASE, service_id);
        snprintf(path, sizeof(path), "/services/%s/logs?all=true", service_id);
    } else {
        snprintf(url, sizeof(url), "%s/services/%s/logs", API_BASE, service_id);
        snprintf(path, sizeof(path), "/services/%s/logs", service_id);
    }

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    // Extract log from response
    char *log = extract_json_string(response.data, "log");
    free(response.data);
    return log;
}

// Create a service via HTTP API
// bootstrap_content: if provided, sent as bootstrap_content (file contents)
// bootstrap: if bootstrap_content is NULL and this starts with http, sent as bootstrap URL
// service_type: optional type for SRV-enabled services (minecraft, mumble, teamspeak, etc.)
// input_files: array of files to include (written to /tmp/ in container)
// input_file_count: number of input files
// golden_image: optional LXD image alias to use instead of default (for testing)
static char* create_service(const UnsandboxCredentials *creds, const char *name, const char *ports, const char *domains, const char *bootstrap, const char *bootstrap_content, const char *network_mode, int vcpu, const char *service_type, struct InputFile *input_files, int input_file_count, const char *golden_image, int unfreeze_on_demand) {
    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/services", API_BASE);

    // Calculate required payload size (bootstrap_content can be large)
    size_t payload_size = 1024;  // Base size for JSON structure
    if (bootstrap_content) {
        payload_size += strlen(bootstrap_content) * 2 + 100;  // Escaped content + field name
    } else if (bootstrap) {
        payload_size += strlen(bootstrap) * 2 + 100;
    }
    if (domains) {
        payload_size += strlen(domains) * 2 + 100;  // Escaped domains + JSON overhead
    }
    // Add space for input files
    for (int i = 0; i < input_file_count; i++) {
        payload_size += strlen(input_files[i].content_base64) + 256;
    }

    // Build payload
    char *payload = malloc(payload_size);
    if (!payload) {
        curl_easy_cleanup(curl);
        return NULL;
    }
    char *p = payload;
    p += sprintf(p, "{");

    if (name && strlen(name) > 0) {
        char *esc_name = escape_json_string(name);
        p += sprintf(p, "\"name\":\"%s\"", esc_name);
        free(esc_name);
    }

    if (ports && strlen(ports) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"ports\":[");
        // Parse comma-separated ports
        char *ports_copy = strdup(ports);
        char *port_token = strtok(ports_copy, ",");
        int first = 1;
        while (port_token) {
            if (!first) p += sprintf(p, ",");
            p += sprintf(p, "%d", atoi(port_token));
            first = 0;
            port_token = strtok(NULL, ",");
        }
        free(ports_copy);
        p += sprintf(p, "]");
    }

    // Add custom_domains as JSON array of strings
    if (domains && strlen(domains) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"custom_domains\":[");
        // Parse comma-separated domains
        char *domains_copy = strdup(domains);
        char *domain_token = strtok(domains_copy, ",");
        int first = 1;
        while (domain_token) {
            if (!first) p += sprintf(p, ",");
            // Trim whitespace from domain
            while (*domain_token == ' ') domain_token++;
            char *end = domain_token + strlen(domain_token) - 1;
            while (end > domain_token && *end == ' ') *end-- = '\0';
            char *esc_domain = escape_json_string(domain_token);
            p += sprintf(p, "\"%s\"", esc_domain);
            free(esc_domain);
            first = 0;
            domain_token = strtok(NULL, ",");
        }
        free(domains_copy);
        p += sprintf(p, "]");
    }

    // Prefer bootstrap_content (file contents), fall back to bootstrap (URL/command)
    if (bootstrap_content && strlen(bootstrap_content) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        char *esc_content = escape_json_string(bootstrap_content);
        p += sprintf(p, "\"bootstrap_content\":\"%s\"", esc_content);
        free(esc_content);
    } else if (bootstrap && strlen(bootstrap) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        char *esc_bootstrap = escape_json_string(bootstrap);
        p += sprintf(p, "\"bootstrap\":\"%s\"", esc_bootstrap);
        free(esc_bootstrap);
    }

    if (network_mode && strlen(network_mode) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"network_mode\":\"%s\"", network_mode);
    }

    if (vcpu > 1) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"vcpu\":%d", vcpu);
    }

    // Service type for SRV-enabled services (minecraft, mumble, etc.)
    if (service_type && strlen(service_type) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        char *esc_type = escape_json_string(service_type);
        p += sprintf(p, "\"service_type\":\"%s\"", esc_type);
        free(esc_type);
    }

    // Golden image override (for testing with different base images)
    if (golden_image && strlen(golden_image) > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        char *esc_image = escape_json_string(golden_image);
        p += sprintf(p, "\"golden_image\":\"%s\"", esc_image);
        free(esc_image);
    }

    // Add input files
    if (input_file_count > 0) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"input_files\":[");
        for (int i = 0; i < input_file_count; i++) {
            if (i > 0) *p++ = ',';
            char *esc_filename = escape_json_string(input_files[i].filename);
            p += sprintf(p, "{\"filename\":\"%s\",\"content\":\"%s\"}",
                        esc_filename, input_files[i].content_base64);
            free(esc_filename);
        }
        p += sprintf(p, "]");
    }

    // Unfreeze on demand (auto-wake on HTTP request)
    if (unfreeze_on_demand) {
        if (p > payload + 1) p += sprintf(p, ",");
        p += sprintf(p, "\"unfreeze_on_demand\":true");
    }

    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/services", payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(payload);  // Done with payload after curl_easy_perform

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return NULL;
    }

    if (http_code != 200 && http_code != 201) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return NULL;
    }

    // Extract service ID from response (try both "service_id" and "id" for compatibility)
    char *service_id = extract_json_string(response.data, "service_id");
    if (!service_id) {
        service_id = extract_json_string(response.data, "id");
    }
    free(response.data);
    return service_id;
}

// List all services
static int list_services(const UnsandboxCredentials *creds) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/services", API_BASE);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/services", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse and display services
    if (!response.data || !strstr(response.data, "\"services\"")) {
        fprintf(stderr, "Error: Invalid response\n");
        free(response.data);
        return 1;
    }

    // Extract count
    const char *count_str = strstr(response.data, "\"count\":");
    int count = 0;
    if (count_str) {
        count = atoi(count_str + 8);
    }

    if (count == 0) {
        printf("No services found\n");
        free(response.data);
        return 0;
    }

    printf("Services: %d\n\n", count);
    printf("%-40s %-20s %-10s %-8s %-15s\n", "SERVICE ID", "NAME", "STATUS", "DISK", "PORTS");
    printf("%-40s %-20s %-10s %-8s %-15s\n", "----------------------------------------",
           "--------------------", "----------", "--------", "---------------");

    // Parse services array
    const char *services_start = strstr(response.data, "\"services\":[");
    if (services_start) {
        const char *pos = services_start + 12;

        while ((pos = strchr(pos, '{')) != NULL) {
            char *service_id = extract_json_string(pos, "id");
            char *name = extract_json_string(pos, "name");
            char *status = extract_json_string(pos, "state");

            // Extract disk_used (bytes as number)
            long long disk_used = extract_json_number(pos, "disk_used");
            char disk_str[16];
            format_bytes(disk_used, disk_str, sizeof(disk_str));

            // Extract ports array
            char ports_str[128] = "-";
            const char *ports_start = strstr(pos, "\"ports\":[");
            if (ports_start) {
                const char *ports_end = strchr(ports_start + 9, ']');
                if (ports_end) {
                    size_t len = ports_end - (ports_start + 9);
                    if (len < sizeof(ports_str) - 1) {
                        strncpy(ports_str, ports_start + 9, len);
                        ports_str[len] = '\0';
                    }
                }
            }

            printf("%-40s %-20s %-10s %-8s %-15s\n",
                   service_id ? service_id : "-",
                   name ? name : "-",
                   status ? status : "-",
                   disk_str,
                   ports_str);

            if (service_id) free(service_id);
            if (name) free(name);
            if (status) free(status);

            // Move to next object by skipping past current object's closing }
            // Need to properly match braces to handle nested objects like port_mappings
            int brace_depth = 1;
            pos++;  // move past opening {
            while (*pos && brace_depth > 0) {
                if (*pos == '{') brace_depth++;
                else if (*pos == '}') brace_depth--;
                else if (*pos == '"') {
                    // Skip strings (may contain { or })
                    pos++;
                    while (*pos && !(*pos == '"' && *(pos-1) != '\\')) pos++;
                }
                pos++;
            }
            // pos now points just past the closing } of current object
            const char *next_obj = strchr(pos, '{');
            const char *end_arr = strchr(pos, ']');
            if (!next_obj || (end_arr && end_arr < next_obj)) break;
            pos = next_obj;
        }
    }

    free(response.data);
    return 0;
}

// Get service info
static int get_service_info(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Extract and display service details
    char *id = extract_json_string(response.data, "id");
    char *name = extract_json_string(response.data, "name");
    char *status = extract_json_string(response.data, "status");
    char *network_mode = extract_json_string(response.data, "network_mode");
    int locked = (int)extract_json_number(response.data, "locked");
    int unfreeze_on_demand = (int)extract_json_number(response.data, "unfreeze_on_demand");

    printf("Service Information:\n");
    printf("  ID:              %s\n", id ? id : "-");
    printf("  Name:            %s\n", name ? name : "-");
    printf("  Status:          %s\n", status ? status : "-");
    printf("  Network Mode:    %s\n", network_mode ? network_mode : "-");
    printf("  Locked:          %s\n", locked ? "yes" : "no");
    printf("  Auto-Unfreeze:   %s\n", unfreeze_on_demand ? "yes" : "no");

    // Extract ports array
    const char *ports_start = strstr(response.data, "\"ports\":[");
    if (ports_start) {
        printf("  Ports:           ");
        const char *pos = ports_start + 9;
        const char *ports_end = strchr(pos, ']');
        if (ports_end) {
            char ports_buf[256];
            size_t len = ports_end - pos;
            if (len < sizeof(ports_buf)) {
                strncpy(ports_buf, pos, len);
                ports_buf[len] = '\0';
                printf("%s\n", ports_buf);
            }
        }
    }

    if (id) free(id);
    if (name) free(name);
    if (status) free(status);
    if (network_mode) free(network_mode);
    free(response.data);
    return 0;
}

// Freeze a service
static int freeze_service(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/freeze", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/freeze", service_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mService frozen successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Unfreeze a service
static int unfreeze_service(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/unfreeze", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/unfreeze", service_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mService unfrozen successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Destroy a service
static int destroy_service(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mService destroyed successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Lock a service to prevent deletion
static int lock_service(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/lock", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/lock", service_id);

    const char *body = "{}";
    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mService locked successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Unlock a service to allow deletion
static int unlock_service(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/unlock", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/unlock", service_id);

    const char *body = "{}";
    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mService unlocked successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Set unfreeze_on_demand for a service
static int set_unfreeze_on_demand(const UnsandboxCredentials *creds, const char *service_id, int enabled) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    char body[128];
    snprintf(body, sizeof(body), "{\"unfreeze_on_demand\":%s}", enabled ? "true" : "false");

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "PATCH", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PATCH");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mAuto-unfreeze %s\033[0m\n", enabled ? "enabled" : "disabled");
    free(response.data);
    return 0;
}

// Set show_freeze_page for a service (controls whether frozen services show payment page or JSON error)
static int set_show_freeze_page(const UnsandboxCredentials *creds, const char *service_id, int enabled) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    char body[128];
    snprintf(body, sizeof(body), "{\"show_freeze_page\":%s}", enabled ? "true" : "false");

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "PATCH", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PATCH");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mFreeze page %s\033[0m\n", enabled ? "enabled" : "disabled");
    free(response.data);
    return 0;
}

// Resize a service (change vCPU/memory live)
static int resize_service(const UnsandboxCredentials *creds, const char *service_id, int vcpu) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    char body[64];
    snprintf(body, sizeof(body), "{\"vcpu\":%d}", vcpu);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "PATCH", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PATCH");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 429) {
        fprintf(stderr, "Error: Cannot resize - would exceed tier concurrency limit\n");
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    if (http_code == 400) {
        fprintf(stderr, "Error: Invalid vcpu value (must be 1-8)\n");
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse response to show details
    printf("\033[32mService resized to %d vCPU, %dGB RAM\033[0m\n", vcpu, vcpu * 2);
    if (response.data) {
        printf("Details: %s\n", response.data);
    }
    free(response.data);
    return 0;
}

// ============================================================================
// Environment Secrets Vault Functions
// ============================================================================

// Get environment vault status for a service
// Returns: 0 on success, 1 on error
static int service_env_status(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/env", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/env", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse response: {"has_vault": true, "updated_at": 123456, "count": 3}
    // Check for has_vault field
    const char *has_vault_str = strstr(response.data, "\"has_vault\":");
    int has_vault = 0;
    if (has_vault_str) {
        has_vault_str += 12;  // Skip "has_vault":
        while (*has_vault_str == ' ') has_vault_str++;
        has_vault = (strncmp(has_vault_str, "true", 4) == 0);
    }

    if (!has_vault) {
        printf("Vault exists: no\n");
        printf("Variable count: 0\n");
    } else {
        printf("Vault exists: yes\n");

        // Extract count
        long long count = extract_json_number(response.data, "count");
        if (count >= 0) {
            printf("Variable count: %lld\n", count);
        }

        // Extract updated_at
        long long updated_at = extract_json_number(response.data, "updated_at");
        if (updated_at > 0) {
            time_t ts = (time_t)updated_at;
            struct tm *tm_info = localtime(&ts);
            char time_buf[64];
            strftime(time_buf, sizeof(time_buf), "%Y-%m-%d %H:%M:%S", tm_info);
            printf("Last updated: %s\n", time_buf);
        }
    }

    free(response.data);
    return 0;
}

// Set environment vault for a service (PUT /services/:id/env)
// env_content: .env format string (KEY=VALUE\nKEY2=VALUE2\n...)
// Returns: 0 on success, 1 on error
static int service_env_set(const UnsandboxCredentials *creds, const char *service_id, const char *env_content) {
    if (!env_content || strlen(env_content) == 0) {
        fprintf(stderr, "Error: No environment content provided\n");
        return 1;
    }

    if (strlen(env_content) > MAX_ENV_CONTENT_SIZE) {
        fprintf(stderr, "Error: Environment content too large (max %d bytes)\n", MAX_ENV_CONTENT_SIZE);
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/env", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/env", service_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: text/plain");
    headers = add_hmac_auth_headers(headers, creds, "PUT", path, env_content);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PUT");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, env_content);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Extract count from response
    long long count = extract_json_number(response.data, "count");
    if (count >= 0) {
        printf("\033[32mEnvironment vault updated: %lld variable%s\033[0m\n",
               count, count == 1 ? "" : "s");
    } else {
        printf("\033[32mEnvironment vault updated\033[0m\n");
    }

    // Print note about taking effect
    char *message = extract_json_string(response.data, "message");
    if (message) {
        printf("%s\n", message);
        free(message);
    }

    free(response.data);
    return 0;
}

// Export environment vault for a service (POST /services/:id/env/export)
// HMAC auth proves ownership - returns .env format string
// Returns: 0 on success, 1 on error
static int service_env_export(const UnsandboxCredentials *creds, const char *service_id) {
    // HMAC auth proves ownership - no additional confirmation needed
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/env/export", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/env/export", service_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, 0L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found or no vault exists\n");
        free(response.data);
        return 1;
    }

    if (http_code == 401 || http_code == 403) {
        fprintf(stderr, "Error: Not authorized\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Extract env content from response
    char *env_content = extract_json_string(response.data, "env");
    if (env_content) {
        printf("%s", env_content);
        // Ensure trailing newline
        if (strlen(env_content) > 0 && env_content[strlen(env_content) - 1] != '\n') {
            printf("\n");
        }
        free(env_content);
    }

    free(response.data);
    return 0;
}

// Delete environment vault for a service (DELETE /services/:id/env)
// Returns: 0 on success, 1 on error
static int service_env_delete(const UnsandboxCredentials *creds, const char *service_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/env", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/env", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found or no vault exists\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mEnvironment vault deleted\033[0m\n");

    // Print note about taking effect
    char *message = extract_json_string(response.data, "message");
    if (message) {
        printf("%s\n", message);
        free(message);
    }

    free(response.data);
    return 0;
}

// Read .env file contents
// Returns: allocated string with file contents, or NULL on error
static char* read_env_file(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        fprintf(stderr, "Error: Cannot open env file '%s'\n", filename);
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (fsize > MAX_ENV_CONTENT_SIZE) {
        fprintf(stderr, "Error: Env file too large (max %d bytes)\n", MAX_ENV_CONTENT_SIZE);
        fclose(f);
        return NULL;
    }

    char *content = malloc(fsize + 1);
    if (!content) {
        fprintf(stderr, "Error: Out of memory\n");
        fclose(f);
        return NULL;
    }

    size_t read_size = fread(content, 1, fsize, f);
    content[read_size] = '\0';
    fclose(f);

    return content;
}

// Read env content from stdin until EOF
// Returns: allocated string with content, or NULL on error
static char* read_env_stdin(void) {
    size_t capacity = 4096;
    size_t size = 0;
    char *content = malloc(capacity);
    if (!content) return NULL;

    char buf[1024];
    while (fgets(buf, sizeof(buf), stdin)) {
        size_t len = strlen(buf);
        if (size + len + 1 > capacity) {
            capacity *= 2;
            if (capacity > MAX_ENV_CONTENT_SIZE) {
                fprintf(stderr, "Error: Input too large (max %d bytes)\n", MAX_ENV_CONTENT_SIZE);
                free(content);
                return NULL;
            }
            char *new_content = realloc(content, capacity);
            if (!new_content) {
                free(content);
                return NULL;
            }
            content = new_content;
        }
        memcpy(content + size, buf, len);
        size += len;
    }
    content[size] = '\0';
    return content;
}

// ============================================================================
// End Environment Secrets Vault Functions
// ============================================================================

// Redeploy a service (re-run bootstrap script)
// Bootstrap scripts should be idempotent for proper upgrade behavior
static int redeploy_service(const UnsandboxCredentials *creds, const char *service_id, const char *bootstrap) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/redeploy", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/redeploy", service_id);

    // Check if bootstrap is a file or URL
    char *bootstrap_content = NULL;
    const char *bootstrap_url = NULL;

    if (bootstrap && strlen(bootstrap) > 0) {
        if (strncmp(bootstrap, "http://", 7) == 0 ||
            strncmp(bootstrap, "https://", 8) == 0) {
            bootstrap_url = bootstrap;
        } else {
            // Try to read as file
            struct stat st;
            if (stat(bootstrap, &st) == 0 && S_ISREG(st.st_mode)) {
                size_t fsize;
                bootstrap_content = read_file(bootstrap, &fsize);
                if (!bootstrap_content) {
                    fprintf(stderr, "Error reading bootstrap file: %s\n", bootstrap);
                    curl_easy_cleanup(curl);
                    free(response.data);
                    return 1;
                }
                printf("Read bootstrap script (%zu bytes) from %s\n", fsize, bootstrap);
            } else {
                // Treat as inline command
                bootstrap_url = bootstrap;
            }
        }
    }

    // Calculate required payload size
    size_t payload_size = 256;  // Base size
    if (bootstrap_content) {
        payload_size += strlen(bootstrap_content) * 2 + 100;
    } else if (bootstrap_url) {
        payload_size += strlen(bootstrap_url) * 2 + 100;
    }

    // Build JSON payload manually (matching create_service pattern)
    char *payload = malloc(payload_size);
    if (!payload) {
        if (bootstrap_content) free(bootstrap_content);
        curl_easy_cleanup(curl);
        free(response.data);
        return 1;
    }
    char *p = payload;
    p += sprintf(p, "{");

    if (bootstrap_content) {
        char *esc_content = escape_json_string(bootstrap_content);
        p += sprintf(p, "\"bootstrap_content\":\"%s\"", esc_content);
        free(esc_content);
        free(bootstrap_content);
    } else if (bootstrap_url) {
        char *esc_url = escape_json_string(bootstrap_url);
        p += sprintf(p, "\"bootstrap\":\"%s\"", esc_url);
        free(esc_url);
    }

    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    printf("Redeploying service '%s'...\n", service_id);
    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(payload);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 400) {
        fprintf(stderr, "Error: No bootstrap script provided. Use --bootstrap option.\n");
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mRedeploy initiated successfully\033[0m\n");
    printf("Note: Bootstrap scripts should be idempotent for proper upgrade behavior.\n");
    printf("Use 'un service --logs %s' to check progress.\n", service_id);
    free(response.data);
    return 0;
}

// Execute a command in a running service container
// Uses async job polling for long-running commands
static int execute_service(const UnsandboxCredentials *creds, const char *service_id, const char *command, int timeout_ms) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/execute", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/execute", service_id);

    // Build JSON payload
    char *esc_command = escape_json_string(command);
    if (!esc_command) {
        curl_easy_cleanup(curl);
        free(response.data);
        return 1;
    }

    char payload[8192];
    snprintf(payload, sizeof(payload), "{\"command\":\"%s\",\"timeout\":%d}", esc_command, timeout_ms);
    free(esc_command);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code == 409) {
        fprintf(stderr, "Error: Service is not running. Unfreeze it first with --unfreeze\n");
        free(response.data);
        return 1;
    }

    if (http_code != 202) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Extract job_id from response
    char *job_id = extract_json_string(response.data, "job_id");
    free(response.data);

    if (!job_id) {
        fprintf(stderr, "Error: No job_id in response\n");
        return 1;
    }

    // Poll for job completion
    char job_url[512];
    snprintf(job_url, sizeof(job_url), "%s/jobs/%s", API_BASE, job_id);

    char job_path[256];
    snprintf(job_path, sizeof(job_path), "/jobs/%s", job_id);

    int poll_count = 0;
    int max_polls = (timeout_ms / 1000) + 10;  // timeout + 10 extra seconds

    while (poll_count < max_polls) {
        usleep(500000);  // 500ms between polls
        poll_count++;

        curl = curl_easy_init();
        if (!curl) {
            free(job_id);
            return 1;
        }

        struct ResponseBuffer job_response = {0};
        job_response.data = malloc(1);
        job_response.size = 0;

        headers = NULL;
        headers = add_hmac_auth_headers(headers, creds, "GET", job_path, NULL);

        curl_easy_setopt(curl, CURLOPT_URL, job_url);
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &job_response);

        res = curl_easy_perform(curl);
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);

        if (res != CURLE_OK || http_code != 200) {
            free(job_response.data);
            continue;
        }

        // Check job status
        char *status = extract_json_string(job_response.data, "status");
        if (status && strcmp(status, "completed") == 0) {
            // Job completed - print result using same format as code execution
            parse_and_print_response(job_response.data, 0, NULL, NULL);
            free(status);
            free(job_response.data);
            free(job_id);
            return 0;
        }

        if (status && (strcmp(status, "failed") == 0 || strcmp(status, "cancelled") == 0)) {
            char *error = extract_json_string(job_response.data, "error");
            fprintf(stderr, "Error: Job %s: %s\n", status, error ? error : "unknown");
            if (error) free(error);
            free(status);
            free(job_response.data);
            free(job_id);
            return 1;
        }

        if (status) free(status);
        free(job_response.data);
    }

    fprintf(stderr, "Error: Command timed out after %d seconds\n", timeout_ms / 1000);
    free(job_id);
    return 1;
}

// Execute a command in a service and capture output (returns malloc'd string or NULL)
static char* execute_service_capture(const UnsandboxCredentials *creds, const char *service_id, const char *command, int timeout_ms) {
    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[256];
    char url[512];
    snprintf(path, sizeof(path), "/services/%s/execute", service_id);
    snprintf(url, sizeof(url), "%s%s", API_BASE, path);

    char *esc_command = escape_json_string(command);
    if (!esc_command) {
        curl_easy_cleanup(curl);
        free(response.data);
        return NULL;
    }

    char payload[8192];
    snprintf(payload, sizeof(payload), "{\"command\":\"%s\",\"timeout\":%d}", esc_command, timeout_ms);
    free(esc_command);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK || http_code != 202) {
        if (http_code == 409) {
            fprintf(stderr, "\033[34mError: Instance is not running\n\033[0m");
        }
        free(response.data);
        return NULL;
    }

    char *job_id = extract_json_string(response.data, "job_id");
    free(response.data);

    if (!job_id) return NULL;

    char job_path[256];
    char job_url[512];
    snprintf(job_path, sizeof(job_path), "/jobs/%s", job_id);
    snprintf(job_url, sizeof(job_url), "%s%s", API_BASE, job_path);

    int poll_count = 0;
    int max_polls = (timeout_ms / 1000) + 10;

    while (poll_count < max_polls) {
        usleep(500000);
        poll_count++;

        curl = curl_easy_init();
        if (!curl) {
            free(job_id);
            return NULL;
        }

        struct ResponseBuffer job_response = {0};
        job_response.data = malloc(1);
        job_response.size = 0;

        // Regenerate auth headers each poll to keep timestamp fresh
        headers = NULL;
        headers = add_hmac_auth_headers(headers, creds, "GET", job_path, NULL);

        curl_easy_setopt(curl, CURLOPT_URL, job_url);
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &job_response);

        res = curl_easy_perform(curl);
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);

        if (res != CURLE_OK || http_code != 200) {
            free(job_response.data);
            continue;
        }

        char *status = extract_json_string(job_response.data, "status");
        if (status && strcmp(status, "completed") == 0) {
            // Extract stdout from result
            char *output = extract_json_string(job_response.data, "stdout");
            free(status);
            free(job_response.data);
            free(job_id);
            return output;  // Caller must free
        }

        if (status && (strcmp(status, "failed") == 0 || strcmp(status, "cancelled") == 0)) {
            free(status);
            free(job_response.data);
            free(job_id);
            return NULL;
        }

        if (status) free(status);
        free(job_response.data);
    }

    free(job_id);
    return NULL;
}

// ============================================================================
// Snapshot Management Support
// ============================================================================

// List all snapshots
static int list_snapshots(const UnsandboxCredentials *creds) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/snapshots", API_BASE);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/snapshots", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 403) {
        fprintf(stderr, "Error: Snapshots not available for free tier\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse and display snapshots
    if (!response.data || !strstr(response.data, "\"snapshots\"")) {
        fprintf(stderr, "Error: Invalid response\n");
        free(response.data);
        return 1;
    }

    // Extract count
    const char *count_str = strstr(response.data, "\"count\":");
    int count = 0;
    if (count_str) {
        count = atoi(count_str + 8);
    }

    if (count == 0) {
        printf("No snapshots found\n");
        free(response.data);
        return 0;
    }

    printf("Snapshots: %d\n\n", count);
    printf("%-40s %-20s %-12s %-30s %-8s\n", "SNAPSHOT ID", "NAME", "SOURCE TYPE", "SOURCE ID", "SIZE");
    printf("%-40s %-20s %-12s %-30s %-8s\n", "----------------------------------------",
           "--------------------", "------------", "------------------------------", "--------");

    // Parse snapshots array
    const char *snapshots_start = strstr(response.data, "\"snapshots\":[");
    if (snapshots_start) {
        const char *pos = snapshots_start + 13;

        while ((pos = strchr(pos, '{')) != NULL) {
            char *snapshot_id = extract_json_string(pos, "id");
            char *name = extract_json_string(pos, "name");
            char *source_type = extract_json_string(pos, "source_type");
            char *source_id = extract_json_string(pos, "source_id");
            long long size_bytes = extract_json_number(pos, "size_bytes");

            char size_str[16];
            format_bytes(size_bytes, size_str, sizeof(size_str));

            printf("%-40s %-20s %-12s %-30s %-8s\n",
                   snapshot_id ? snapshot_id : "-",
                   name ? name : "-",
                   source_type ? source_type : "-",
                   source_id ? source_id : "-",
                   size_str);

            if (snapshot_id) free(snapshot_id);
            if (name) free(name);
            if (source_type) free(source_type);
            if (source_id) free(source_id);

            // Move to next object
            int brace_depth = 1;
            pos++;
            while (*pos && brace_depth > 0) {
                if (*pos == '{') brace_depth++;
                else if (*pos == '}') brace_depth--;
                else if (*pos == '"') {
                    pos++;
                    while (*pos && !(*pos == '"' && *(pos-1) != '\\')) pos++;
                }
                pos++;
            }
            const char *next_obj = strchr(pos, '{');
            const char *end_arr = strchr(pos, ']');
            if (!next_obj || (end_arr && end_arr < next_obj)) break;
            pos = next_obj;
        }
    }

    free(response.data);
    return 0;
}

// Get snapshot info
static int get_snapshot_info(const UnsandboxCredentials *creds, const char *snapshot_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s", snapshot_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    // Parse and display snapshot info
    char *id = extract_json_string(response.data, "id");
    char *name = extract_json_string(response.data, "name");
    char *source_type = extract_json_string(response.data, "source_type");
    char *source_id = extract_json_string(response.data, "source_id");
    char *container_name = extract_json_string(response.data, "container_name");
    char *status = extract_json_string(response.data, "status");
    long long size_bytes = extract_json_number(response.data, "size_bytes");
    long long created_at = extract_json_number(response.data, "created_at");

    char size_str[16];
    format_bytes(size_bytes, size_str, sizeof(size_str));

    printf("\033[1mSnapshot Details\033[0m\n\n");
    printf("%-20s %s\n", "Snapshot ID:", id ? id : "-");
    printf("%-20s %s\n", "Name:", name ? name : "-");
    printf("%-20s %s\n", "Source Type:", source_type ? source_type : "-");
    printf("%-20s %s\n", "Source ID:", source_id ? source_id : "-");
    printf("%-20s %s\n", "Container:", container_name ? container_name : "-");
    printf("%-20s %s\n", "Size:", size_str);
    printf("%-20s %s\n", "Status:", status ? status : "-");

    if (created_at > 0) {
        time_t t = (time_t)created_at;
        struct tm *tm_info = localtime(&t);
        char time_buf[64];
        strftime(time_buf, sizeof(time_buf), "%Y-%m-%d %H:%M:%S", tm_info);
        printf("%-20s %s\n", "Created:", time_buf);
    }

    if (id) free(id);
    if (name) free(name);
    if (source_type) free(source_type);
    if (source_id) free(source_id);
    if (container_name) free(container_name);
    if (status) free(status);

    free(response.data);
    return 0;
}

// Delete a snapshot
static int delete_snapshot(const UnsandboxCredentials *creds, const char *snapshot_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s", snapshot_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mSnapshot deleted successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Lock a snapshot to prevent deletion
static int lock_snapshot(const UnsandboxCredentials *creds, const char *snapshot_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s/lock", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s/lock", snapshot_id);

    const char *body = "{}";
    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mSnapshot locked successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Unlock a snapshot to allow deletion
static int unlock_snapshot(const UnsandboxCredentials *creds, const char *snapshot_id) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s/unlock", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s/unlock", snapshot_id);

    const char *body = "{}";
    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);
    headers = curl_slist_append(headers, "Content-Type: application/json");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    printf("\033[32mSnapshot unlocked successfully\033[0m\n");
    free(response.data);
    return 0;
}

// Create snapshot of a session
static int create_session_snapshot(const UnsandboxCredentials *creds, const char *session_id, const char *name, int hot) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/sessions/%s/snapshot", API_BASE, session_id);

    char path[256];
    snprintf(path, sizeof(path), "/sessions/%s/snapshot", session_id);

    char payload[1024];
    if (name && strlen(name) > 0) {
        char *esc_name = escape_json_string(name);
        snprintf(payload, sizeof(payload), "{\"name\":\"%s\",\"hot\":%s}", esc_name, hot ? "true" : "false");
        free(esc_name);
    } else {
        snprintf(payload, sizeof(payload), "{\"hot\":%s}", hot ? "true" : "false");
    }

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    fprintf(stderr, "Creating snapshot of session %s...", session_id);
    fflush(stderr);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 403) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Snapshots not available for free tier\n");
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Session not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200 && http_code != 201) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    fprintf(stderr, " done\n");

    char *snapshot_id = extract_json_string(response.data, "id");
    if (snapshot_id) {
        printf("\033[32mSnapshot created successfully\033[0m\n");
        printf("Snapshot ID: %s\n", snapshot_id);
        free(snapshot_id);
    }

    free(response.data);
    return 0;
}

// Create snapshot of a service
static int create_service_snapshot(const UnsandboxCredentials *creds, const char *service_id, const char *name, int hot) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/services/%s/snapshot", API_BASE, service_id);

    char path[256];
    snprintf(path, sizeof(path), "/services/%s/snapshot", service_id);

    char payload[1024];
    if (name && strlen(name) > 0) {
        char *esc_name = escape_json_string(name);
        snprintf(payload, sizeof(payload), "{\"name\":\"%s\",\"hot\":%s}", esc_name, hot ? "true" : "false");
        free(esc_name);
    } else {
        snprintf(payload, sizeof(payload), "{\"hot\":%s}", hot ? "true" : "false");
    }

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    fprintf(stderr, "Creating snapshot of service %s...", service_id);
    fflush(stderr);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 403) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Snapshots not available for free tier\n");
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Service not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200 && http_code != 201) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    fprintf(stderr, " done\n");

    char *snapshot_id = extract_json_string(response.data, "id");
    if (snapshot_id) {
        printf("\033[32mSnapshot created successfully\033[0m\n");
        printf("Snapshot ID: %s\n", snapshot_id);
        free(snapshot_id);
    }

    free(response.data);
    return 0;
}

// Restore session from snapshot
static int restore_from_snapshot(const UnsandboxCredentials *creds, const char *snapshot_id, const char *type) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s/restore", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s/restore", snapshot_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    fprintf(stderr, "Restoring %s from snapshot %s...", type, snapshot_id);
    fflush(stderr);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    fprintf(stderr, " done\n");
    printf("\033[32m%s restored from snapshot\033[0m\n", type);

    free(response.data);
    return 0;
}

// Clone from snapshot to create new session or service
static int clone_snapshot(const UnsandboxCredentials *creds, const char *snapshot_id, const char *clone_type,
                         const char *name, const char *shell, const char *ports) {
    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[512];
    snprintf(url, sizeof(url), "%s/snapshots/%s/clone", API_BASE, snapshot_id);

    char path[256];
    snprintf(path, sizeof(path), "/snapshots/%s/clone", snapshot_id);

    // Build payload
    char payload[2048];
    char *p = payload;
    p += sprintf(p, "{\"type\":\"%s\"", clone_type);

    if (name && strlen(name) > 0) {
        char *esc = escape_json_string(name);
        p += sprintf(p, ",\"name\":\"%s\"", esc);
        free(esc);
    }

    if (shell && strlen(shell) > 0) {
        char *esc = escape_json_string(shell);
        p += sprintf(p, ",\"shell\":\"%s\"", esc);
        free(esc);
    }

    if (ports && strlen(ports) > 0) {
        // Parse comma-separated ports into array
        p += sprintf(p, ",\"ports\":[");
        char *ports_copy = strdup(ports);
        char *tok = strtok(ports_copy, ",");
        int first = 1;
        while (tok) {
            if (!first) p += sprintf(p, ",");
            p += sprintf(p, "%d", atoi(tok));
            first = 0;
            tok = strtok(NULL, ",");
        }
        free(ports_copy);
        p += sprintf(p, "]");
    }

    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    fprintf(stderr, "Cloning snapshot %s to create new %s...", snapshot_id, clone_type);
    fflush(stderr);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code == 403) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Snapshots not available for free tier\n");
        free(response.data);
        return 1;
    }

    if (http_code == 404) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: Snapshot not found\n");
        free(response.data);
        return 1;
    }

    if (http_code != 200 && http_code != 201) {
        fprintf(stderr, " failed\n");
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) fprintf(stderr, "%s\n", response.data);
        free(response.data);
        return 1;
    }

    fprintf(stderr, " done\n");

    if (strcmp(clone_type, "session") == 0) {
        char *session_id = extract_json_string(response.data, "session_id");
        if (session_id) {
            printf("\033[32mSession created from snapshot\033[0m\n");
            printf("Session ID: %s\n", session_id);
            free(session_id);
        }
    } else {
        char *service_id = extract_json_string(response.data, "service_id");
        if (service_id) {
            printf("\033[32mService created from snapshot\033[0m\n");
            printf("Service ID: %s\n", service_id);
            free(service_id);
        }
    }

    free(response.data);
    return 0;
}

// ============================================================================
// End Snapshot Management Support
// ============================================================================

// ============================================================================
// End Service Management Support
// ============================================================================

// ============================================================================
// LXD Container Images API
// ============================================================================

// Publish an image from a service or snapshot
static char* image_publish(const UnsandboxCredentials *creds, const char *source_type,
                           const char *source_id, const char *name, const char *description) {
    if (!creds || !creds->public_key || !creds->secret_key || !source_type || !source_id) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/images", API_BASE);

    // Build JSON body
    char body[2048];
    char *p = body;
    p += sprintf(p, "{\"source_type\":\"%s\",\"source_id\":\"%s\"", source_type, source_id);
    if (name && strlen(name) > 0) {
        char *esc = escape_json_string(name);
        p += sprintf(p, ",\"name\":\"%s\"", esc);
        free(esc);
    }
    if (description && strlen(description) > 0) {
        char *esc = escape_json_string(description);
        p += sprintf(p, ",\"description\":\"%s\"", esc);
        free(esc);
    }
    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/images", body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// List images (filter_type can be NULL, "owned", "shared", or "public")
static char* list_images(const UnsandboxCredentials *creds, const char *filter_type) {
    if (!creds || !creds->public_key || !creds->secret_key) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    if (filter_type && strlen(filter_type) > 0) {
        snprintf(url, sizeof(url), "%s/images/%s", API_BASE, filter_type);
        snprintf(path, sizeof(path), "/images/%s", filter_type);
    } else {
        snprintf(url, sizeof(url), "%s/images", API_BASE);
        snprintf(path, sizeof(path), "/images");
    }

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// Get image details
static char* get_image(const UnsandboxCredentials *creds, const char *image_id) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s", image_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// Delete an image
static int delete_image(const UnsandboxCredentials *creds, const char *image_id) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s", image_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Lock an image
static int lock_image(const UnsandboxCredentials *creds, const char *image_id) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/lock", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/lock", image_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Unlock an image
static int unlock_image(const UnsandboxCredentials *creds, const char *image_id) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/unlock", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/unlock", image_id);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, "{}");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "{}");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Set image visibility (private, unlisted, public)
static int set_image_visibility(const UnsandboxCredentials *creds, const char *image_id, const char *visibility) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id || !visibility) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/visibility", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/visibility", image_id);

    char body[128];
    snprintf(body, sizeof(body), "{\"visibility\":\"%s\"}", visibility);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Grant image access to another API key
__attribute__((unused))
static int grant_image_access(const UnsandboxCredentials *creds, const char *image_id, const char *trusted_api_key) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id || !trusted_api_key) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/grant", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/grant", image_id);

    char body[256];
    snprintf(body, sizeof(body), "{\"trusted_api_key\":\"%s\"}", trusted_api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Revoke image access from another API key
__attribute__((unused))
static int revoke_image_access(const UnsandboxCredentials *creds, const char *image_id, const char *trusted_api_key) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id || !trusted_api_key) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/revoke", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/revoke", image_id);

    char body[256];
    snprintf(body, sizeof(body), "{\"trusted_api_key\":\"%s\"}", trusted_api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// List API keys with access to an image
__attribute__((unused))
static char* list_image_trusted(const UnsandboxCredentials *creds, const char *image_id) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/trusted", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/trusted", image_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// Transfer image ownership to another API key
__attribute__((unused))
static int transfer_image(const UnsandboxCredentials *creds, const char *image_id, const char *to_api_key) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id || !to_api_key) {
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/transfer", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/transfer", image_id);

    char body[256];
    snprintf(body, sizeof(body), "{\"to_api_key\":\"%s\"}", to_api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    free(response.data);
    return (res == CURLE_OK) ? 0 : 1;
}

// Spawn a new service from an image
static char* spawn_from_image(const UnsandboxCredentials *creds, const char *image_id,
                              const char *name, const char *ports, const char *bootstrap,
                              const char *network_mode) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/spawn", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/spawn", image_id);

    // Build JSON body
    char body[4096];
    char *p = body;
    p += sprintf(p, "{");
    int need_comma = 0;

    if (name && strlen(name) > 0) {
        char *esc = escape_json_string(name);
        p += sprintf(p, "\"name\":\"%s\"", esc);
        free(esc);
        need_comma = 1;
    }
    if (ports && strlen(ports) > 0) {
        if (need_comma) p += sprintf(p, ",");
        p += sprintf(p, "\"ports\":\"%s\"", ports);
        need_comma = 1;
    }
    if (bootstrap && strlen(bootstrap) > 0) {
        if (need_comma) p += sprintf(p, ",");
        char *esc = escape_json_string(bootstrap);
        p += sprintf(p, "\"bootstrap\":\"%s\"", esc);
        free(esc);
        need_comma = 1;
    }
    if (network_mode && strlen(network_mode) > 0) {
        if (need_comma) p += sprintf(p, ",");
        p += sprintf(p, "\"network_mode\":\"%s\"", network_mode);
    }
    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// Clone an image to create a copy owned by the current user
static char* clone_image(const UnsandboxCredentials *creds, const char *image_id,
                         const char *name, const char *description) {
    if (!creds || !creds->public_key || !creds->secret_key || !image_id) {
        return NULL;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return NULL;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    char path[128];
    snprintf(url, sizeof(url), "%s/images/%s/clone", API_BASE, image_id);
    snprintf(path, sizeof(path), "/images/%s/clone", image_id);

    // Build JSON body
    char body[2048];
    char *p = body;
    p += sprintf(p, "{");
    int need_comma = 0;

    if (name && strlen(name) > 0) {
        char *esc = escape_json_string(name);
        p += sprintf(p, "\"name\":\"%s\"", esc);
        free(esc);
        need_comma = 1;
    }
    if (description && strlen(description) > 0) {
        if (need_comma) p += sprintf(p, ",");
        char *esc = escape_json_string(description);
        p += sprintf(p, "\"description\":\"%s\"", esc);
        free(esc);
    }
    p += sprintf(p, "}");

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, body);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);
    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        free(response.data);
        return NULL;
    }

    return response.data;
}

// ============================================================================
// End LXD Container Images API
// ============================================================================

// ============================================================================
// Key Validation Support
// ============================================================================

static int validate_api_key(const UnsandboxCredentials *creds) {
    if (!creds || !creds->public_key || !creds->secret_key) {
        fprintf(stderr, "Error: Both public and secret keys required for validation\n");
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) return 1;

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char url[256];
    snprintf(url, sizeof(url), "%s/keys/validate", API_BASE);

    // Use HMAC authentication with empty body
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/keys/validate", "");

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: %s\n", curl_easy_strerror(res));
        free(response.data);
        return 1;
    }

    if (http_code != 200) {
        // Parse error response
        if (response.data) {
            char *error = extract_json_string(response.data, "error");
            char *reason = extract_json_string(response.data, "reason");
            if (error) {
                printf("\033[31mInvalid\033[0m: %s\n", error);
                free(error);
            } else if (reason) {
                if (strcmp(reason, "invalid_key") == 0) {
                    printf("\033[31mInvalid\033[0m: key not found\n");
                } else if (strcmp(reason, "expired") == 0) {
                    printf("\033[31mExpired\033[0m\n\n");

                    // Show key details if available
                    char *public_key = extract_json_string(response.data, "public_key");
                    long long tier = extract_json_number(response.data, "tier");
                    char *expired_at = extract_json_string(response.data, "expired_at_datetime");
                    char *expired_ago = extract_json_string(response.data, "expired_ago");
                    char *renew_url = extract_json_string(response.data, "renew_url");

                    if (public_key) {
                        printf("%-20s %s\n", "Public Key:", public_key);
                        free(public_key);
                    }
                    if (tier >= 0) {
                        printf("%-20s %lld\n", "Tier:", tier);
                    }
                    if (expired_at) {
                        printf("%-20s %s", "Expired:", expired_at);
                        if (expired_ago) {
                            printf(" (%s)", expired_ago);
                        }
                        printf("\n");
                        free(expired_at);
                    }
                    if (expired_ago) free(expired_ago);

                    printf("\n\033[33mTo renew:\033[0m Visit %s\n",
                           renew_url ? renew_url : "https://unsandbox.com/pricing");
                    if (renew_url) free(renew_url);
                } else if (strcmp(reason, "suspended") == 0) {
                    printf("\033[31mSuspended\033[0m: key has been suspended\n");
                } else {
                    printf("\033[31mInvalid\033[0m: %s\n", reason);
                }
                free(reason);
            } else {
                printf("\033[31mInvalid\033[0m: HTTP %ld\n", http_code);
            }
        }
        free(response.data);
        return 1;
    }

    // Check for valid:false in 200 response
    const char *valid_check = strstr(response.data, "\"valid\":false");
    if (valid_check) {
        char *reason = extract_json_string(response.data, "reason");
        if (reason) {
            if (strcmp(reason, "invalid_key") == 0) {
                printf("\033[31mInvalid\033[0m: key not found\n");
            } else if (strcmp(reason, "expired") == 0) {
                printf("\033[31mExpired\033[0m\n\n");

                // Show key details if available
                char *public_key = extract_json_string(response.data, "public_key");
                long long tier = extract_json_number(response.data, "tier");
                char *expired_at = extract_json_string(response.data, "expired_at_datetime");
                char *expired_ago = extract_json_string(response.data, "expired_ago");
                char *renew_url = extract_json_string(response.data, "renew_url");

                if (public_key) {
                    printf("%-20s %s\n", "Public Key:", public_key);
                    free(public_key);
                }
                if (tier >= 0) {
                    printf("%-20s %lld\n", "Tier:", tier);
                }
                if (expired_at) {
                    printf("%-20s %s", "Expired:", expired_at);
                    if (expired_ago) {
                        printf(" (%s)", expired_ago);
                    }
                    printf("\n");
                    free(expired_at);
                }
                if (expired_ago) free(expired_ago);

                printf("\n\033[33mTo renew:\033[0m Visit %s\n",
                       renew_url ? renew_url : "https://unsandbox.com/pricing");
                if (renew_url) free(renew_url);
            } else if (strcmp(reason, "suspended") == 0) {
                printf("\033[31mSuspended\033[0m: key has been suspended\n");
            } else {
                printf("\033[31mInvalid\033[0m: %s\n", reason);
            }
            free(reason);
        } else {
            printf("\033[31mInvalid key\033[0m\n");
        }
        free(response.data);
        return 1;
    }

    // Parse valid response
    if (!response.data) {
        fprintf(stderr, "Error: Empty response\n");
        return 1;
    }

    // Check if valid
    const char *valid_str = strstr(response.data, "\"valid\":");
    int valid = 0;
    if (valid_str) {
        valid = (strstr(valid_str, "true") == valid_str + 8);
    }

    if (!valid) {
        printf("\033[31mInvalid key\033[0m\n");
        free(response.data);
        return 1;
    }

    // Extract fields
    long long tier = extract_json_number(response.data, "tier");
    char *status = extract_json_string(response.data, "status");
    char *valid_through = extract_json_string(response.data, "valid_through_datetime");
    char *valid_for = extract_json_string(response.data, "valid_for_human");
    char *public_key = extract_json_string(response.data, "public_key");
    long long rate_per_minute = extract_json_number(response.data, "rate_per_minute");
    long long burst = extract_json_number(response.data, "burst");
    long long concurrency = extract_json_number(response.data, "concurrency");

    // Display key info
    printf("\033[32mValid\033[0m\n\n");
    printf("%-20s %s\n", "Public Key:", public_key ? public_key : "N/A");
    printf("%-20s %lld\n", "Tier:", tier);
    printf("%-20s %s\n", "Status:", status ? status : "N/A");
    printf("%-20s %s\n", "Expires:", valid_through ? valid_through : "N/A");
    printf("%-20s %s\n", "Time Remaining:", valid_for ? valid_for : "N/A");
    printf("%-20s %lld/min\n", "Rate Limit:", rate_per_minute);
    printf("%-20s %lld\n", "Burst:", burst);
    printf("%-20s %lld\n", "Concurrency:", concurrency);

    if (status) free(status);
    if (valid_through) free(valid_through);
    if (valid_for) free(valid_for);
    if (public_key) free(public_key);
    free(response.data);

    return 0;
}

// ============================================================================
// End Key Validation Support
// ============================================================================

void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] <source_file>\n", prog);
    fprintf(stderr, "       %s session [options]\n", prog);
    fprintf(stderr, "       %s service [options]\n", prog);
    fprintf(stderr, "       %s snapshot [options]\n", prog);
    fprintf(stderr, "       %s image [options]\n", prog);
    fprintf(stderr, "       %s languages [--json]\n", prog);
    fprintf(stderr, "       %s key\n\n", prog);
    fprintf(stderr, "Commands:\n");
    fprintf(stderr, "  (default)        Execute source file in sandbox\n");
    fprintf(stderr, "  session          Open interactive shell/REPL session\n");
    fprintf(stderr, "  service          Manage persistent services\n");
    fprintf(stderr, "  snapshot         Manage container snapshots\n");
    fprintf(stderr, "  image            Manage images (publish, spawn, clone)\n");
    fprintf(stderr, "  languages        List available languages (--json for JSON output)\n");
    fprintf(stderr, "  key              Check API key validity and expiration\n");
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -s, --shell LANG Specify language (default: bash if arg is not a file)\n");
    fprintf(stderr, "  -e KEY=VALUE     Set environment variable (can use multiple times)\n");
    fprintf(stderr, "  -f FILE          Add input file to /tmp/ (can use multiple times)\n");
    fprintf(stderr, "  -F FILE          Add input file with path preserved (can use multiple times)\n");
    fprintf(stderr, "  -a               Return and save artifacts (compiled binaries)\n");
    fprintf(stderr, "  -o DIR           Output directory for artifacts (default: current dir)\n");
    fprintf(stderr, "  -p KEY           Public key (or set UNSANDBOX_PUBLIC_KEY env var)\n");
    fprintf(stderr, "  -k KEY           Secret key (or set UNSANDBOX_SECRET_KEY env var)\n");
    fprintf(stderr, "  -n MODE          Network mode: zerotrust (default) or semitrusted\n");
    fprintf(stderr, "  -v N, --vcpu N   vCPU count 1-8, each vCPU gets 2GB RAM. Default: 1\n");
    fprintf(stderr, "  -y               Skip confirmation for large uploads (>1GB)\n");
    fprintf(stderr, "  -h               Show this help\n");
    fprintf(stderr, "\nSession options:\n");
    fprintf(stderr, "  -s, --shell SHELL  Shell/REPL to use (default: bash)\n");
    fprintf(stderr, "  -l, --list         List active sessions\n");
    fprintf(stderr, "  --attach ID        Reconnect to existing session (ID or container name)\n");
    fprintf(stderr, "  --kill ID          Terminate a session (ID or container name)\n");
    fprintf(stderr, "  --audit            Record session for auditing\n");
    fprintf(stderr, "  --tmux             Enable session persistence with tmux (allows reconnect)\n");
    fprintf(stderr, "  --screen           Enable session persistence with screen (allows reconnect)\n");
    fprintf(stderr, "  --snapshot ID      Create snapshot of session (paid tiers only)\n");
    fprintf(stderr, "  --restore SNAPSHOT Restore session from snapshot\n");
    fprintf(stderr, "  --snapshot-name N  Name for the snapshot\n");
    fprintf(stderr, "  --hot              Take snapshot without freezing (live snapshot)\n");
    fprintf(stderr, "\nService options:\n");
    fprintf(stderr, "  --name NAME        Service name (creates new service)\n");
    fprintf(stderr, "  --ports PORTS      Comma-separated ports (e.g., 80,443)\n");
    fprintf(stderr, "  --domains DOMAINS  Comma-separated custom domains (e.g., example.com,www.example.com)\n");
    fprintf(stderr, "  --type TYPE        Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)\n");
    fprintf(stderr, "  --golden-image IMG Use custom LXD image alias (for testing, e.g., jammy-golden-22.04)\n");
    fprintf(stderr, "  --bootstrap CMD    Bootstrap command or URI to run on startup\n");
    fprintf(stderr, "  --bootstrap-file FILE  Upload local file as bootstrap script content\n");
    fprintf(stderr, "  --unfreeze-on-demand   Enable auto-unfreeze when HTTP request arrives\n");
    fprintf(stderr, "  -e, --env KEY=VAL  Set environment variable (can repeat, stored encrypted)\n");
    fprintf(stderr, "  --env-file FILE    Load env vars from .env file (stored encrypted)\n");
    fprintf(stderr, "  -f FILE            Upload file to /tmp/ (can use multiple times)\n");
    fprintf(stderr, "  -F FILE            Upload file with path preserved (can use multiple times)\n");
    fprintf(stderr, "  -l, --list         List all services\n");
    fprintf(stderr, "  --info ID          Get service details\n");
    fprintf(stderr, "  --tail ID          Get last 9000 lines of bootstrap logs\n");
    fprintf(stderr, "  --logs ID          Get all bootstrap logs\n");
    fprintf(stderr, "  --download-logs ID FILE  Download all logs to file\n");
    fprintf(stderr, "  --freeze ID        Freeze a service\n");
    fprintf(stderr, "  --unfreeze ID      Unfreeze a service\n");
    fprintf(stderr, "  --destroy ID       Destroy a service\n");
    fprintf(stderr, "  --lock ID          Lock a service to prevent deletion\n");
    fprintf(stderr, "  --unlock ID        Unlock a service to allow deletion\n");
    fprintf(stderr, "  --auto-unfreeze ID     Enable auto-unfreeze on HTTP request\n");
    fprintf(stderr, "  --no-auto-unfreeze ID  Disable auto-unfreeze on HTTP request\n");
    fprintf(stderr, "  --show-freeze-page ID  Enable freeze page (show payment page when frozen)\n");
    fprintf(stderr, "  --no-show-freeze-page ID  Disable freeze page (return JSON error when frozen)\n");
    fprintf(stderr, "  --resize ID        Resize service vCPU/memory (requires --vcpu)\n");
    fprintf(stderr, "  --redeploy ID      Re-run bootstrap script (optional: --bootstrap or --bootstrap-file)\n");
    fprintf(stderr, "  --execute ID CMD   Run a command in a running service\n");
    fprintf(stderr, "  --dump-bootstrap ID [FILE]  Dump bootstrap script (for migrations)\n");
    fprintf(stderr, "  --snapshot ID      Create snapshot of service (paid tiers only)\n");
    fprintf(stderr, "  --restore SNAPSHOT Restore service from snapshot\n");
    fprintf(stderr, "  --snapshot-name N  Name for the snapshot\n");
    fprintf(stderr, "  --hot              Take snapshot without freezing (live snapshot)\n");
    fprintf(stderr, "\nService environment vault:\n");
    fprintf(stderr, "  env status ID      Show vault status (exists, count, updated)\n");
    fprintf(stderr, "  env set ID         Set vault from --env-file FILE or stdin\n");
    fprintf(stderr, "  env export ID      Export vault contents to stdout\n");
    fprintf(stderr, "  env delete ID      Delete vault\n");
    fprintf(stderr, "\nSnapshot options:\n");
    fprintf(stderr, "  -l, --list         List all snapshots\n");
    fprintf(stderr, "  --info ID          Get snapshot details\n");
    fprintf(stderr, "  --delete ID        Delete a snapshot\n");
    fprintf(stderr, "  --lock ID          Lock a snapshot to prevent deletion\n");
    fprintf(stderr, "  --unlock ID        Unlock a snapshot to allow deletion\n");
    fprintf(stderr, "  --clone ID         Clone snapshot to new session/service\n");
    fprintf(stderr, "  --type TYPE        Clone type: session or service (for --clone)\n");
    fprintf(stderr, "  --name NAME        Name for cloned service (for --clone)\n");
    fprintf(stderr, "  --shell SHELL      Shell for cloned session (for --clone)\n");
    fprintf(stderr, "  --ports PORTS      Ports for cloned service (for --clone)\n");
    fprintf(stderr, "\nImage options:\n");
    fprintf(stderr, "  -l, --list         List all images\n");
    fprintf(stderr, "  --info ID          Get image details\n");
    fprintf(stderr, "  --delete ID        Delete an image\n");
    fprintf(stderr, "  --lock ID          Lock an image to prevent deletion\n");
    fprintf(stderr, "  --unlock ID        Unlock an image to allow deletion\n");
    fprintf(stderr, "  --publish ID       Publish image from service/snapshot (requires --source-type)\n");
    fprintf(stderr, "  --source-type TYPE Source type: service or snapshot (for --publish)\n");
    fprintf(stderr, "  --visibility ID V  Set visibility: private, unlisted, or public\n");
    fprintf(stderr, "  --spawn ID         Spawn new service from image\n");
    fprintf(stderr, "  --clone ID         Clone an image\n");
    fprintf(stderr, "  --name NAME        Name for spawned service or cloned image\n");
    fprintf(stderr, "  --ports PORTS      Ports for spawned service\n");
    fprintf(stderr, "\nAvailable shells/REPLs:\n");
    fprintf(stderr, "  Shells: bash, dash, sh, zsh, fish, ksh, tcsh, csh, elvish, xonsh, ash\n");
    fprintf(stderr, "  REPLs:  python3, bpython, ipython, node, ruby, irb, lua, php, perl\n");
    fprintf(stderr, "          guile, ghci, erl, iex, sbcl, clisp, r, julia, clojure\n");
    fprintf(stderr, "\nSession behavior:\n");
    fprintf(stderr, "  Default:    Session terminates immediately on disconnect (clean exit)\n");
    fprintf(stderr, "  --tmux:     Session persists on disconnect, reconnect with --attach\n");
    fprintf(stderr, "  --screen:   Session persists on disconnect, reconnect with --attach\n");
    fprintf(stderr, "\nExamples:\n");
    fprintf(stderr, "  %s script.py                       # execute Python script\n", prog);
    fprintf(stderr, "  %s -s bash 'echo hello'            # execute inline command\n", prog);
    fprintf(stderr, "  %s -e DEBUG=1 script.py            # with environment variable\n", prog);
    fprintf(stderr, "  %s -f data.csv process.py          # with input file\n", prog);
    fprintf(stderr, "  %s -a -o ./bin main.c              # save compiled artifacts\n", prog);
    fprintf(stderr, "  %s session                         # interactive bash (terminates on disconnect)\n", prog);
    fprintf(stderr, "  %s session --tmux                  # bash with tmux (can reconnect)\n", prog);
    fprintf(stderr, "  %s session --screen                # bash with screen (can reconnect)\n", prog);
    fprintf(stderr, "  %s session --list                  # list active sessions\n", prog);
    fprintf(stderr, "  %s session --kill sandbox-abc      # terminate a session\n", prog);
    fprintf(stderr, "  %s session --freeze sandbox-abc    # freeze session (requires --tmux/--screen)\n", prog);
    fprintf(stderr, "  %s session --unfreeze sandbox-abc  # unfreeze a frozen session\n", prog);
    fprintf(stderr, "  %s session --boost sandbox-abc     # boost to 2 vCPU, 4GB RAM\n", prog);
    fprintf(stderr, "  %s session --boost sandbox-abc --boost-vcpu 4  # 4 vCPU, 8GB RAM\n", prog);
    fprintf(stderr, "  %s session --unboost sandbox-abc   # return to base resources\n", prog);
    fprintf(stderr, "  %s session --attach sandbox-abc    # reconnect by container name\n", prog);
    fprintf(stderr, "  %s session --shell python3         # Python REPL\n", prog);
    fprintf(stderr, "  %s session --shell node --tmux     # Node.js REPL with reconnect\n", prog);
    fprintf(stderr, "  %s session -n semitrusted          # session with network access\n", prog);
    fprintf(stderr, "  %s session --audit -o ./logs       # record session for auditing\n", prog);
    fprintf(stderr, "  %s session -f data.csv             # session with input file in /tmp/\n", prog);
    fprintf(stderr, "  %s service --name web --ports 80,443 --bootstrap \"python3 -m http.server 80\"\n", prog);
    fprintf(stderr, "  %s service --name app --ports 8000 --bootstrap-file ./setup.sh\n", prog);
    fprintf(stderr, "  %s service --name app -f app.tar.gz --bootstrap-file ./setup.sh  # deploy tarball\n", prog);
    fprintf(stderr, "  %s service --name blog --ports 8000 --domains blog.example.com,www.example.com\n", prog);
    fprintf(stderr, "  %s service --list                  # list all services\n", prog);
    fprintf(stderr, "  %s service --info abc123           # get service details\n", prog);
    fprintf(stderr, "  %s service --logs abc123           # get bootstrap logs\n", prog);
    fprintf(stderr, "  %s service --freeze abc123         # freeze a service\n", prog);
    fprintf(stderr, "  %s service --unfreeze abc123       # unfreeze a service\n", prog);
    fprintf(stderr, "  %s service --resize abc123 --vcpu 4  # scale to 4 vCPU, 8GB RAM\n", prog);
    fprintf(stderr, "  %s service --destroy abc123        # destroy a service\n", prog);
    fprintf(stderr, "  %s service --redeploy abc123 --bootstrap-file ./script.sh\n", prog);
    fprintf(stderr, "  %s service --redeploy abc123       # uses stored encrypted bootstrap\n", prog);
    fprintf(stderr, "  %s service --execute maldoror 'journalctl -u myapp -n 50'\n", prog);
    fprintf(stderr, "  %s service --dump-bootstrap maldoror  # print bootstrap to stdout\n", prog);
    fprintf(stderr, "  %s service --dump-bootstrap maldoror backup.sh  # save to file\n", prog);
    fprintf(stderr, "  %s service --name app -e API_KEY=secret -e DEBUG=1  # with env vars\n", prog);
    fprintf(stderr, "  %s service --name app --env-file .env  # with env file\n", prog);
    fprintf(stderr, "  %s service env status myapp        # check vault status\n", prog);
    fprintf(stderr, "  %s service env set myapp -e KEY=val -e SECRET=xxx  # set from flags\n", prog);
    fprintf(stderr, "  %s service env set myapp --env-file .env  # set vault from file\n", prog);
    fprintf(stderr, "  %s service env set myapp < .env    # set vault from stdin\n", prog);
    fprintf(stderr, "  %s service env export myapp        # export vault contents\n", prog);
    fprintf(stderr, "  %s service env delete myapp        # delete vault\n", prog);
    fprintf(stderr, "  %s service --snapshot abc123       # create snapshot of service\n", prog);
    fprintf(stderr, "  %s service --restore unsb-snapshot-xxxx  # restore service\n", prog);
    fprintf(stderr, "  %s session --snapshot abc123       # create snapshot of session\n", prog);
    fprintf(stderr, "  %s session --restore unsb-snapshot-xxxx  # restore session\n", prog);
    fprintf(stderr, "  %s snapshot --list                 # list all snapshots\n", prog);
    fprintf(stderr, "  %s snapshot --info unsb-snapshot-xxxx  # get snapshot details\n", prog);
    fprintf(stderr, "  %s snapshot --delete unsb-snapshot-xxxx  # delete a snapshot\n", prog);
    fprintf(stderr, "  %s snapshot --clone unsb-snapshot-xxxx --type service --name myapp\n", prog);
    fprintf(stderr, "  %s key                             # check API key validity\n", prog);
    fprintf(stderr, "  %s key --extend                    # open portal to extend key\n", prog);
    fprintf(stderr, "\nAuthentication:\n");
    fprintf(stderr, "  Credentials are loaded in order of priority:\n");
    fprintf(stderr, "  1. -p and -k flags (public and secret key)\n");
    fprintf(stderr, "  2. UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars\n");
    fprintf(stderr, "  3. ~/.unsandbox/accounts.csv (format: public_key,secret_key per line)\n");
    fprintf(stderr, "     Use --account N to select account by index (0-based, default: 0)\n");
    fprintf(stderr, "     Or set UNSANDBOX_ACCOUNT=N env var\n");
}

/* ============================================================================
 * LIBRARY API IMPLEMENTATION
 * These functions implement the un.h public API for library use.
 * ============================================================================ */

const char *unsandbox_version(void) {
    return "4.2.40";
}

const char *unsandbox_detect_language(const char *filename) {
    if (!filename) return NULL;
    return detect_language_from_extension(filename);
}

char *unsandbox_hmac_sign(const char *secret_key, const char *message) {
    if (!secret_key || !message) return NULL;
    return hmac_sha256_hex(secret_key, strlen(secret_key), message, strlen(message));
}

int unsandbox_health_check(void) {
    CURL *curl = curl_easy_init();
    if (!curl) return -1;

    curl_easy_setopt(curl, CURLOPT_URL, API_BASE "/health");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
    curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);
    curl_easy_cleanup(curl);

    if (res != CURLE_OK) return -1;
    return (http_code == 200) ? 1 : 0;
}

#ifdef UNSANDBOX_LIBRARY
/* ============================================================================
 * Library API - Only compiled when UNSANDBOX_LIBRARY is defined
 * ============================================================================ */

/* Memory management */
void unsandbox_free_result(unsandbox_result_t *result) {
    if (!result) return;
    free(result->stdout_str);
    free(result->stderr_str);
    free(result->language);
    free(result->error_message);
    free(result);
}

void unsandbox_free_job(unsandbox_job_t *job) {
    if (!job) return;
    free(job->id);
    free(job->language);
    free(job->status);
    free(job->error_message);
    free(job);
}

void unsandbox_free_job_list(unsandbox_job_list_t *jobs) {
    if (!jobs) return;
    for (size_t i = 0; i < jobs->count; i++) {
        free(jobs->jobs[i].id);
        free(jobs->jobs[i].language);
        free(jobs->jobs[i].status);
        free(jobs->jobs[i].error_message);
    }
    free(jobs->jobs);
    free(jobs);
}

void unsandbox_free_languages(unsandbox_languages_t *langs) {
    if (!langs) return;
    for (size_t i = 0; i < langs->count; i++) {
        free(langs->languages[i]);
    }
    free(langs->languages);
    free(langs);
}

void unsandbox_free_session(unsandbox_session_t *session) {
    if (!session) return;
    free(session->id);
    free(session->container_name);
    free(session->status);
    free(session->network_mode);
    free(session);
}

void unsandbox_free_session_list(unsandbox_session_list_t *sessions) {
    if (!sessions) return;
    for (size_t i = 0; i < sessions->count; i++) {
        free(sessions->sessions[i].id);
        free(sessions->sessions[i].container_name);
        free(sessions->sessions[i].status);
        free(sessions->sessions[i].network_mode);
    }
    free(sessions->sessions);
    free(sessions);
}

void unsandbox_free_service(unsandbox_service_t *service) {
    if (!service) return;
    free(service->id);
    free(service->name);
    free(service->status);
    free(service->container_name);
    free(service->network_mode);
    free(service->ports);
    free(service->domains);
    free(service);
}

void unsandbox_free_service_list(unsandbox_service_list_t *services) {
    if (!services) return;
    for (size_t i = 0; i < services->count; i++) {
        free(services->services[i].id);
        free(services->services[i].name);
        free(services->services[i].status);
        free(services->services[i].container_name);
        free(services->services[i].network_mode);
        free(services->services[i].ports);
        free(services->services[i].domains);
    }
    free(services->services);
    free(services);
}

void unsandbox_free_snapshot(unsandbox_snapshot_t *snapshot) {
    if (!snapshot) return;
    free(snapshot->id);
    free(snapshot->name);
    free(snapshot->type);
    free(snapshot->source_id);
    free(snapshot);
}

void unsandbox_free_snapshot_list(unsandbox_snapshot_list_t *snapshots) {
    if (!snapshots) return;
    for (size_t i = 0; i < snapshots->count; i++) {
        free(snapshots->snapshots[i].id);
        free(snapshots->snapshots[i].name);
        free(snapshots->snapshots[i].type);
        free(snapshots->snapshots[i].source_id);
    }
    free(snapshots->snapshots);
    free(snapshots);
}

void unsandbox_free_key_info(unsandbox_key_info_t *info) {
    if (!info) return;
    free(info->tier);
    free(info->error_message);
    free(info);
}

/* ============================================================================
 * Session Functions Implementation
 * ============================================================================ */

int unsandbox_session_destroy(const char *session_id,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = kill_session(creds, session_id);
    free_credentials(creds);
    return result;
}

int unsandbox_session_freeze(const char *session_id,
                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = freeze_session(creds, session_id);
    free_credentials(creds);
    return result;
}

int unsandbox_session_unfreeze(const char *session_id,
                               const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = unfreeze_session(creds, session_id);
    free_credentials(creds);
    return result;
}

int unsandbox_session_boost(const char *session_id, int vcpu,
                            const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = boost_session(creds, session_id, vcpu);
    free_credentials(creds);
    return result;
}

int unsandbox_session_unboost(const char *session_id,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = unboost_session(creds, session_id);
    free_credentials(creds);
    return result;
}

/* ============================================================================
 * Service Functions Implementation
 * ============================================================================ */

int unsandbox_service_destroy(const char *service_id,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = destroy_service(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_freeze(const char *service_id,
                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = freeze_service(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_unfreeze(const char *service_id,
                               const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = unfreeze_service(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_lock(const char *service_id,
                           const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = lock_service(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_unlock(const char *service_id,
                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = unlock_service(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_set_unfreeze_on_demand(const char *service_id, int enabled,
                                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = set_unfreeze_on_demand(creds, service_id, enabled);
    free_credentials(creds);
    return result;
}

int unsandbox_service_redeploy(const char *service_id, const char *bootstrap,
                               const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = redeploy_service(creds, service_id, bootstrap);
    free_credentials(creds);
    return result;
}

char *unsandbox_service_logs(const char *service_id, int all_logs,
                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return NULL;
    char *result = get_service_logs(creds, service_id, all_logs);
    free_credentials(creds);
    return result;
}

int unsandbox_service_env_set(const char *service_id, const char *env_content,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = service_env_set(creds, service_id, env_content);
    free_credentials(creds);
    return result;
}

int unsandbox_service_env_delete(const char *service_id,
                                 const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = service_env_delete(creds, service_id);
    free_credentials(creds);
    return result;
}

int unsandbox_service_resize(const char *service_id, int vcpu,
                             const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = resize_service(creds, service_id, vcpu);
    free_credentials(creds);
    return result;
}

/* ============================================================================
 * Snapshot Functions Implementation
 * ============================================================================ */

int unsandbox_snapshot_delete(const char *snapshot_id,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = delete_snapshot(creds, snapshot_id);
    free_credentials(creds);
    return result;
}

int unsandbox_snapshot_lock(const char *snapshot_id,
                            const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = lock_snapshot(creds, snapshot_id);
    free_credentials(creds);
    return result;
}

int unsandbox_snapshot_unlock(const char *snapshot_id,
                              const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = unlock_snapshot(creds, snapshot_id);
    free_credentials(creds);
    return result;
}

int unsandbox_snapshot_restore(const char *snapshot_id,
                               const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return -1;
    int result = restore_from_snapshot(creds, snapshot_id, NULL);
    free_credentials(creds);
    return result;
}

/* ============================================================================
 * Key Validation Implementation
 * ============================================================================ */

unsandbox_key_info_t *unsandbox_validate_keys(const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) return NULL;

    unsandbox_key_info_t *info = calloc(1, sizeof(unsandbox_key_info_t));
    if (!info) {
        free_credentials(creds);
        return NULL;
    }

    int result = validate_api_key(creds);
    info->valid = (result == 0) ? 1 : 0;

    free_credentials(creds);
    return info;
}

/* ============================================================================
 * Stub Implementations (TODO: Full implementation)
 * These functions are declared in un.h but need HTTP/JSON handling
 * ============================================================================ */

/* Execution - full library implementations */

// Helper to parse execute response into result struct
static unsandbox_result_t *parse_execute_response(const char *json) {
    if (!json) return NULL;

    unsandbox_result_t *result = calloc(1, sizeof(unsandbox_result_t));
    if (!result) {
        set_last_error("Out of memory");
        return NULL;
    }

    result->stdout_str = extract_json_string(json, "stdout");
    result->stderr_str = extract_json_string(json, "stderr");
    result->error_message = extract_json_string(json, "error");
    result->language = extract_json_string(json, "language");
    result->exit_code = (int)extract_json_number(json, "exit_code");
    long long exec_time_ms = extract_json_number(json, "execution_time");
    result->execution_time = exec_time_ms >= 0 ? exec_time_ms / 1000.0 : 0.0;
    result->success = (result->exit_code == 0 && !result->error_message);

    return result;
}

unsandbox_result_t *unsandbox_execute(
    const char *language, const char *code,
    const char *public_key, const char *secret_key) {

    if (!language || !code) {
        set_last_error("Language and code are required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    // Escape code for JSON
    char *escaped_code = escape_json_string(code);
    if (!escaped_code) {
        set_last_error("Failed to escape code");
        free_credentials(creds);
        return NULL;
    }

    // Build JSON payload
    size_t payload_size = strlen(escaped_code) + strlen(language) + 64;
    char *json_payload = malloc(payload_size);
    if (!json_payload) {
        set_last_error("Out of memory");
        free(escaped_code);
        free_credentials(creds);
        return NULL;
    }
    snprintf(json_payload, payload_size, "{\"language\":\"%s\",\"code\":\"%s\"}", language, escaped_code);
    free(escaped_code);

    // Initialize curl
    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free(json_payload);
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/execute", json_payload);

    curl_easy_setopt(curl, CURLOPT_URL, API_URL);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 120L);

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(json_payload);

    if (res != CURLE_OK) {
        set_last_error("Request failed: %s", curl_easy_strerror(res));
        free(response.data);
        free_credentials(creds);
        return NULL;
    }

    if (http_code != 200) {
        set_last_error("HTTP %ld: %s", http_code, response.data ? response.data : "Unknown error");
        free(response.data);
        free_credentials(creds);
        return NULL;
    }

    // Check if we need to poll for job completion
    char *job_id = extract_json_string(response.data, "job_id");
    char *status = extract_json_string(response.data, "status");

    unsandbox_result_t *result = NULL;
    if (job_id && status && (strcmp(status, "pending") == 0 || strcmp(status, "running") == 0)) {
        // Need to poll
        char *final_response = poll_job_status(creds, job_id);
        result = parse_execute_response(final_response);
        free(final_response);
    } else {
        result = parse_execute_response(response.data);
    }

    free(job_id);
    free(status);
    free(response.data);
    free_credentials(creds);
    return result;
}

char *unsandbox_execute_async(
    const char *language, const char *code,
    const char *public_key, const char *secret_key) {

    if (!language || !code) {
        set_last_error("Language and code are required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *escaped_code = escape_json_string(code);
    if (!escaped_code) {
        set_last_error("Failed to escape code");
        free_credentials(creds);
        return NULL;
    }

    size_t payload_size = strlen(escaped_code) + strlen(language) + 64;
    char *json_payload = malloc(payload_size);
    if (!json_payload) {
        set_last_error("Out of memory");
        free(escaped_code);
        free_credentials(creds);
        return NULL;
    }
    snprintf(json_payload, payload_size, "{\"language\":\"%s\",\"code\":\"%s\"}", language, escaped_code);
    free(escaped_code);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free(json_payload);
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/execute", json_payload);

    curl_easy_setopt(curl, CURLOPT_URL, API_URL);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(json_payload);
    free_credentials(creds);

    if (res != CURLE_OK) {
        set_last_error("Request failed: %s", curl_easy_strerror(res));
        free(response.data);
        return NULL;
    }

    if (http_code != 200 && http_code != 202) {
        set_last_error("HTTP %ld: %s", http_code, response.data ? response.data : "Unknown error");
        free(response.data);
        return NULL;
    }

    char *job_id = extract_json_string(response.data, "job_id");
    free(response.data);

    if (!job_id) {
        set_last_error("No job_id in response");
    }
    return job_id;
}

unsandbox_result_t *unsandbox_wait_job(
    const char *job_id,
    const char *public_key, const char *secret_key) {

    if (!job_id) {
        set_last_error("Job ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *final_response = poll_job_status(creds, job_id);
    free_credentials(creds);

    if (!final_response) {
        set_last_error("Failed to poll job status");
        return NULL;
    }

    unsandbox_result_t *result = parse_execute_response(final_response);
    free(final_response);
    return result;
}

unsandbox_job_t *unsandbox_get_job(
    const char *job_id,
    const char *public_key, const char *secret_key) {

    if (!job_id) {
        set_last_error("Job ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/jobs/%s", API_BASE, job_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/jobs/%s", job_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get job: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_job_t *job = calloc(1, sizeof(unsandbox_job_t));
    if (!job) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    job->id = extract_json_string(response.data, "job_id");
    if (!job->id) job->id = strdup(job_id);
    job->language = extract_json_string(response.data, "language");
    job->status = extract_json_string(response.data, "status");
    job->created_at = extract_json_number(response.data, "created_at");
    job->completed_at = extract_json_number(response.data, "completed_at");
    job->error_message = extract_json_string(response.data, "error");

    free(response.data);
    return job;
}

int unsandbox_cancel_job(
    const char *job_id,
    const char *public_key, const char *secret_key) {

    if (!job_id) {
        set_last_error("Job ID is required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/jobs/%s", API_BASE, job_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return -1;
    }

    char path[128];
    snprintf(path, sizeof(path), "/jobs/%s", job_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "DELETE", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK) {
        set_last_error("Request failed: %s", curl_easy_strerror(res));
        return -1;
    }

    return (http_code == 200 || http_code == 204) ? 0 : -1;
}

unsandbox_job_list_t *unsandbox_list_jobs(
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/jobs", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/jobs", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to list jobs: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    int count = count_json_array_objects(response.data, "jobs");
    unsandbox_job_list_t *list = calloc(1, sizeof(unsandbox_job_list_t));
    if (!list) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        list->jobs = calloc(count, sizeof(unsandbox_job_t));
        if (!list->jobs) {
            set_last_error("Out of memory");
            free(list);
            free(response.data);
            return NULL;
        }
        list->count = count;

        const char *jobs_start = strstr(response.data, "\"jobs\":[");
        if (jobs_start) {
            const char *pos = jobs_start + 8;
            for (size_t i = 0; i < list->count && pos; i++) {
                pos = strchr(pos, '{');
                if (!pos) break;

                list->jobs[i].id = extract_json_string(pos, "job_id");
                list->jobs[i].language = extract_json_string(pos, "language");
                list->jobs[i].status = extract_json_string(pos, "status");
                list->jobs[i].created_at = extract_json_number(pos, "created_at");
                list->jobs[i].completed_at = extract_json_number(pos, "completed_at");
                list->jobs[i].error_message = extract_json_string(pos, "error");

                pos = skip_json_object(pos);
            }
        }
    }

    free(response.data);
    return list;
}

unsandbox_languages_t *unsandbox_get_languages(
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/languages", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/languages", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get languages: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    // Count languages - look for "languages":["lang1","lang2",...]
    const char *langs_start = strstr(response.data, "\"languages\":[");
    if (!langs_start) {
        // Try alternative format: just an array
        langs_start = strchr(response.data, '[');
    }

    int count = 0;
    if (langs_start) {
        const char *p = strchr(langs_start, '[');
        if (p) {
            p++;
            while (*p && *p != ']') {
                if (*p == '"') {
                    count++;
                    p++;
                    while (*p && *p != '"') p++;
                }
                if (*p) p++;
            }
        }
    }

    unsandbox_languages_t *langs = calloc(1, sizeof(unsandbox_languages_t));
    if (!langs) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        langs->languages = calloc(count, sizeof(char *));
        if (!langs->languages) {
            set_last_error("Out of memory");
            free(langs);
            free(response.data);
            return NULL;
        }
        langs->count = count;

        // Parse again to extract strings
        const char *p = strchr(langs_start, '[');
        if (p) {
            p++;
            size_t i = 0;
            while (*p && *p != ']' && i < langs->count) {
                if (*p == '"') {
                    p++;
                    const char *end = strchr(p, '"');
                    if (end) {
                        size_t len = end - p;
                        langs->languages[i] = malloc(len + 1);
                        if (langs->languages[i]) {
                            memcpy(langs->languages[i], p, len);
                            langs->languages[i][len] = '\0';
                            i++;
                        }
                        p = end;
                    }
                }
                if (*p) p++;
            }
        }
    }

    free(response.data);
    return langs;
}

/* Session - full library implementations */

unsandbox_session_list_t *unsandbox_session_list(
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/sessions", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to list sessions: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    int count = count_json_array_objects(response.data, "sessions");
    unsandbox_session_list_t *list = calloc(1, sizeof(unsandbox_session_list_t));
    if (!list) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        list->sessions = calloc(count, sizeof(unsandbox_session_t));
        if (!list->sessions) {
            set_last_error("Out of memory");
            free(list);
            free(response.data);
            return NULL;
        }
        list->count = count;

        const char *sessions_start = strstr(response.data, "\"sessions\":[");
        if (sessions_start) {
            const char *pos = sessions_start + 12;
            for (size_t i = 0; i < list->count && pos; i++) {
                pos = strchr(pos, '{');
                if (!pos) break;

                list->sessions[i].id = extract_json_string(pos, "id");
                list->sessions[i].container_name = extract_json_string(pos, "container_name");
                list->sessions[i].status = extract_json_string(pos, "status");
                list->sessions[i].network_mode = extract_json_string(pos, "network_mode");
                list->sessions[i].vcpu = (int)extract_json_number(pos, "vcpu");
                list->sessions[i].created_at = extract_json_number(pos, "created_at");
                list->sessions[i].last_activity = extract_json_number(pos, "last_activity");

                pos = skip_json_object(pos);
            }
        }
    }

    free(response.data);
    return list;
}

unsandbox_session_t *unsandbox_session_get(
    const char *session_id,
    const char *public_key, const char *secret_key) {

    if (!session_id) {
        set_last_error("Session ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions/%s", API_BASE, session_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/sessions/%s", session_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get session: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_session_t *session = calloc(1, sizeof(unsandbox_session_t));
    if (!session) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    session->id = extract_json_string(response.data, "session_id");
    if (!session->id) session->id = extract_json_string(response.data, "id");
    session->container_name = extract_json_string(response.data, "container_name");
    session->status = extract_json_string(response.data, "status");
    session->network_mode = extract_json_string(response.data, "network_mode");
    session->vcpu = (int)extract_json_number(response.data, "vcpu");
    session->created_at = extract_json_number(response.data, "created_at");
    session->last_activity = extract_json_number(response.data, "last_activity");

    free(response.data);
    return session;
}

unsandbox_session_t *unsandbox_session_create(
    const char *network_mode, const char *shell,
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    // Build payload
    char payload[512];
    char *p = payload;
    p += sprintf(p, "{");
    int has_field = 0;
    if (network_mode && strlen(network_mode) > 0) {
        p += sprintf(p, "\"network_mode\":\"%s\"", network_mode);
        has_field = 1;
    }
    if (shell && strlen(shell) > 0) {
        if (has_field) p += sprintf(p, ",");
        p += sprintf(p, "\"shell\":\"%s\"", shell);
    }
    p += sprintf(p, "}");

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/sessions", payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || (http_code != 200 && http_code != 201)) {
        set_last_error("Failed to create session: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_session_t *session = calloc(1, sizeof(unsandbox_session_t));
    if (!session) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    session->id = extract_json_string(response.data, "session_id");
    if (!session->id) session->id = extract_json_string(response.data, "id");
    session->container_name = extract_json_string(response.data, "container_name");
    session->status = extract_json_string(response.data, "status");
    session->network_mode = extract_json_string(response.data, "network_mode");
    session->vcpu = (int)extract_json_number(response.data, "vcpu");
    session->created_at = extract_json_number(response.data, "created_at");
    session->last_activity = extract_json_number(response.data, "last_activity");

    free(response.data);
    return session;
}

unsandbox_result_t *unsandbox_session_execute(
    const char *session_id, const char *command,
    const char *public_key, const char *secret_key) {

    if (!session_id || !command) {
        set_last_error("Session ID and command are required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *escaped_cmd = escape_json_string(command);
    if (!escaped_cmd) {
        set_last_error("Failed to escape command");
        free_credentials(creds);
        return NULL;
    }

    size_t payload_size = strlen(escaped_cmd) + 64;
    char *payload = malloc(payload_size);
    if (!payload) {
        set_last_error("Out of memory");
        free(escaped_cmd);
        free_credentials(creds);
        return NULL;
    }
    snprintf(payload, payload_size, "{\"command\":\"%s\"}", escaped_cmd);
    free(escaped_cmd);

    char url[256];
    snprintf(url, sizeof(url), "%s/sessions/%s/shell", API_BASE, session_id);

    char path[128];
    snprintf(path, sizeof(path), "/sessions/%s/shell", session_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free(payload);
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 120L);

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(payload);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to execute in session: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_result_t *result = parse_execute_response(response.data);
    free(response.data);
    return result;
}

/* Service - full library implementations */

unsandbox_service_list_t *unsandbox_service_list(
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/services", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/services", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to list services: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    int count = count_json_array_objects(response.data, "services");
    unsandbox_service_list_t *list = calloc(1, sizeof(unsandbox_service_list_t));
    if (!list) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        list->services = calloc(count, sizeof(unsandbox_service_t));
        if (!list->services) {
            set_last_error("Out of memory");
            free(list);
            free(response.data);
            return NULL;
        }
        list->count = count;

        const char *services_start = strstr(response.data, "\"services\":[");
        if (services_start) {
            const char *pos = services_start + 12;
            for (size_t i = 0; i < list->count && pos; i++) {
                pos = strchr(pos, '{');
                if (!pos) break;

                list->services[i].id = extract_json_string(pos, "id");
                list->services[i].name = extract_json_string(pos, "name");
                list->services[i].status = extract_json_string(pos, "status");
                list->services[i].container_name = extract_json_string(pos, "container_name");
                list->services[i].network_mode = extract_json_string(pos, "network_mode");
                list->services[i].ports = extract_json_string(pos, "ports");
                list->services[i].domains = extract_json_string(pos, "domains");
                list->services[i].vcpu = (int)extract_json_number(pos, "vcpu");
                list->services[i].locked = (int)extract_json_number(pos, "locked");
                list->services[i].unfreeze_on_demand = (int)extract_json_number(pos, "unfreeze_on_demand");
                list->services[i].created_at = extract_json_number(pos, "created_at");
                list->services[i].last_activity = extract_json_number(pos, "last_activity");

                pos = skip_json_object(pos);
            }
        }
    }

    free(response.data);
    return list;
}

unsandbox_service_t *unsandbox_service_get(
    const char *service_id,
    const char *public_key, const char *secret_key) {

    if (!service_id) {
        set_last_error("Service ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/services/%s", API_BASE, service_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/services/%s", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get service: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_service_t *service = calloc(1, sizeof(unsandbox_service_t));
    if (!service) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    service->id = extract_json_string(response.data, "id");
    service->name = extract_json_string(response.data, "name");
    service->status = extract_json_string(response.data, "status");
    service->container_name = extract_json_string(response.data, "container_name");
    service->network_mode = extract_json_string(response.data, "network_mode");
    service->ports = extract_json_string(response.data, "ports");
    service->domains = extract_json_string(response.data, "domains");
    service->vcpu = (int)extract_json_number(response.data, "vcpu");
    service->locked = (int)extract_json_number(response.data, "locked");
    service->unfreeze_on_demand = (int)extract_json_number(response.data, "unfreeze_on_demand");
    service->created_at = extract_json_number(response.data, "created_at");
    service->last_activity = extract_json_number(response.data, "last_activity");

    free(response.data);
    return service;
}

char *unsandbox_service_create(
    const char *name, const char *ports, const char *domains,
    const char *bootstrap, const char *network_mode,
    const char *public_key, const char *secret_key) {
    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }
    char *result = create_service(creds, name, ports, domains, bootstrap, NULL, network_mode, 0, NULL, NULL, 0, NULL, 0);
    free_credentials(creds);
    return result;
}

unsandbox_result_t *unsandbox_service_execute(
    const char *service_id, const char *command, int timeout_ms,
    const char *public_key, const char *secret_key) {

    if (!service_id || !command) {
        set_last_error("Service ID and command are required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *escaped_cmd = escape_json_string(command);
    if (!escaped_cmd) {
        set_last_error("Failed to escape command");
        free_credentials(creds);
        return NULL;
    }

    size_t payload_size = strlen(escaped_cmd) + 128;
    char *payload = malloc(payload_size);
    if (!payload) {
        set_last_error("Out of memory");
        free(escaped_cmd);
        free_credentials(creds);
        return NULL;
    }
    if (timeout_ms > 0) {
        snprintf(payload, payload_size, "{\"command\":\"%s\",\"timeout\":%d}", escaped_cmd, timeout_ms);
    } else {
        snprintf(payload, payload_size, "{\"command\":\"%s\"}", escaped_cmd);
    }
    free(escaped_cmd);

    char url[256];
    snprintf(url, sizeof(url), "%s/services/%s/execute", API_BASE, service_id);

    char path[128];
    snprintf(path, sizeof(path), "/services/%s/execute", service_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free(payload);
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", path, payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, (timeout_ms > 0) ? (timeout_ms / 1000 + 30) : 120L);

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(payload);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to execute in service: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_result_t *result = parse_execute_response(response.data);
    free(response.data);
    return result;
}

char *unsandbox_service_env_get(
    const char *service_id,
    const char *public_key, const char *secret_key) {

    if (!service_id) {
        set_last_error("Service ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/services/%s/env", API_BASE, service_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/services/%s/env", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get env: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    // Return the raw JSON response (caller can parse as needed)
    return response.data;
}

char *unsandbox_service_env_export(
    const char *service_id,
    const char *public_key, const char *secret_key) {

    if (!service_id) {
        set_last_error("Service ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/services/%s/env?format=export", API_BASE, service_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/services/%s/env", service_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to export env: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    // Return the export-formatted env content
    return response.data;
}

/* Snapshot - full library implementations */

unsandbox_snapshot_list_t *unsandbox_snapshot_list(
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/snapshots", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/snapshots", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to list snapshots: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    int count = count_json_array_objects(response.data, "snapshots");
    unsandbox_snapshot_list_t *list = calloc(1, sizeof(unsandbox_snapshot_list_t));
    if (!list) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        list->snapshots = calloc(count, sizeof(unsandbox_snapshot_t));
        if (!list->snapshots) {
            set_last_error("Out of memory");
            free(list);
            free(response.data);
            return NULL;
        }
        list->count = count;

        const char *snapshots_start = strstr(response.data, "\"snapshots\":[");
        if (snapshots_start) {
            const char *pos = snapshots_start + 13;
            for (size_t i = 0; i < list->count && pos; i++) {
                pos = strchr(pos, '{');
                if (!pos) break;

                list->snapshots[i].id = extract_json_string(pos, "id");
                list->snapshots[i].name = extract_json_string(pos, "name");
                list->snapshots[i].type = extract_json_string(pos, "type");
                list->snapshots[i].source_id = extract_json_string(pos, "source_id");
                list->snapshots[i].hot = (int)extract_json_number(pos, "hot");
                list->snapshots[i].locked = (int)extract_json_number(pos, "locked");
                list->snapshots[i].created_at = extract_json_number(pos, "created_at");
                list->snapshots[i].size_bytes = extract_json_number(pos, "size_bytes");

                pos = skip_json_object(pos);
            }
        }
    }

    free(response.data);
    return list;
}

unsandbox_snapshot_t *unsandbox_snapshot_get(
    const char *snapshot_id,
    const char *public_key, const char *secret_key) {

    if (!snapshot_id) {
        set_last_error("Snapshot ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/snapshots/%s", API_BASE, snapshot_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/snapshots/%s", snapshot_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get snapshot: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_snapshot_t *snapshot = calloc(1, sizeof(unsandbox_snapshot_t));
    if (!snapshot) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    snapshot->id = extract_json_string(response.data, "id");
    snapshot->name = extract_json_string(response.data, "name");
    snapshot->type = extract_json_string(response.data, "type");
    snapshot->source_id = extract_json_string(response.data, "source_id");
    snapshot->hot = (int)extract_json_number(response.data, "hot");
    snapshot->locked = (int)extract_json_number(response.data, "locked");
    snapshot->created_at = extract_json_number(response.data, "created_at");
    snapshot->size_bytes = extract_json_number(response.data, "size_bytes");

    free(response.data);
    return snapshot;
}

char *unsandbox_snapshot_session(
    const char *session_id, const char *name, int hot,
    const char *public_key, const char *secret_key) {

    if (!session_id) {
        set_last_error("Session ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    // Build payload
    char payload[512];
    char *p = payload;
    p += sprintf(p, "{\"source_type\":\"session\",\"source_id\":\"%s\"", session_id);
    if (name && strlen(name) > 0) {
        char *esc_name = escape_json_string(name);
        p += sprintf(p, ",\"name\":\"%s\"", esc_name);
        free(esc_name);
    }
    if (hot) {
        p += sprintf(p, ",\"hot\":true");
    }
    p += sprintf(p, "}");

    char url[256];
    snprintf(url, sizeof(url), "%s/snapshots", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/snapshots", payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || (http_code != 200 && http_code != 201)) {
        set_last_error("Failed to create snapshot: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    char *snapshot_id = extract_json_string(response.data, "id");
    free(response.data);
    return snapshot_id;
}

char *unsandbox_snapshot_service(
    const char *service_id, const char *name, int hot,
    const char *public_key, const char *secret_key) {

    if (!service_id) {
        set_last_error("Service ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    // Build payload
    char payload[512];
    char *p = payload;
    p += sprintf(p, "{\"source_type\":\"service\",\"source_id\":\"%s\"", service_id);
    if (name && strlen(name) > 0) {
        char *esc_name = escape_json_string(name);
        p += sprintf(p, ",\"name\":\"%s\"", esc_name);
        free(esc_name);
    }
    if (hot) {
        p += sprintf(p, ",\"hot\":true");
    }
    p += sprintf(p, "}");

    char url[256];
    snprintf(url, sizeof(url), "%s/snapshots", API_BASE);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/snapshots", payload);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || (http_code != 200 && http_code != 201)) {
        set_last_error("Failed to create snapshot: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    char *snapshot_id = extract_json_string(response.data, "id");
    free(response.data);
    return snapshot_id;
}

/* ============================================================================
 * Image API - Library Implementations
 * ============================================================================ */

unsandbox_image_list_t *unsandbox_image_list(
    const char *filter,
    const char *public_key, const char *secret_key) {

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    char path[128];
    if (filter && strlen(filter) > 0) {
        snprintf(url, sizeof(url), "%s/images?filter=%s", API_BASE, filter);
        snprintf(path, sizeof(path), "/images?filter=%s", filter);
    } else {
        snprintf(url, sizeof(url), "%s/images", API_BASE);
        snprintf(path, sizeof(path), "/images");
    }

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", "/images", NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to list images: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    int count = count_json_array_objects(response.data, "images");
    unsandbox_image_list_t *list = calloc(1, sizeof(unsandbox_image_list_t));
    if (!list) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    if (count > 0) {
        list->images = calloc(count, sizeof(unsandbox_image_t));
        if (!list->images) {
            set_last_error("Out of memory");
            free(list);
            free(response.data);
            return NULL;
        }
        list->count = count;

        const char *images_start = strstr(response.data, "\"images\":[");
        if (images_start) {
            const char *pos = images_start + 10;
            for (size_t i = 0; i < list->count && pos; i++) {
                pos = strchr(pos, '{');
                if (!pos) break;

                list->images[i].id = extract_json_string(pos, "id");
                list->images[i].name = extract_json_string(pos, "name");
                list->images[i].description = extract_json_string(pos, "description");
                list->images[i].visibility = extract_json_string(pos, "visibility");
                list->images[i].source_type = extract_json_string(pos, "source_type");
                list->images[i].source_id = extract_json_string(pos, "source_id");
                list->images[i].owner_api_key = extract_json_string(pos, "owner_api_key");
                list->images[i].locked = (int)extract_json_number(pos, "locked");
                list->images[i].created_at = extract_json_number(pos, "created_at");
                list->images[i].size_bytes = extract_json_number(pos, "size_bytes");

                pos = skip_json_object(pos);
            }
        }
    }

    free(response.data);
    return list;
}

unsandbox_image_t *unsandbox_image_get(
    const char *image_id,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char url[256];
    snprintf(url, sizeof(url), "%s/images/%s", API_BASE, image_id);

    CURL *curl = curl_easy_init();
    if (!curl) {
        set_last_error("Failed to initialize curl");
        free_credentials(creds);
        return NULL;
    }

    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    char path[128];
    snprintf(path, sizeof(path), "/images/%s", image_id);

    struct curl_slist *headers = NULL;
    headers = add_hmac_auth_headers(headers, creds, "GET", path, NULL);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    CURLcode res = curl_easy_perform(curl);
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free_credentials(creds);

    if (res != CURLE_OK || http_code != 200) {
        set_last_error("Failed to get image: HTTP %ld", http_code);
        free(response.data);
        return NULL;
    }

    unsandbox_image_t *image = calloc(1, sizeof(unsandbox_image_t));
    if (!image) {
        set_last_error("Out of memory");
        free(response.data);
        return NULL;
    }

    image->id = extract_json_string(response.data, "id");
    image->name = extract_json_string(response.data, "name");
    image->description = extract_json_string(response.data, "description");
    image->visibility = extract_json_string(response.data, "visibility");
    image->source_type = extract_json_string(response.data, "source_type");
    image->source_id = extract_json_string(response.data, "source_id");
    image->owner_api_key = extract_json_string(response.data, "owner_api_key");
    image->locked = (int)extract_json_number(response.data, "locked");
    image->created_at = extract_json_number(response.data, "created_at");
    image->size_bytes = extract_json_number(response.data, "size_bytes");

    free(response.data);
    return image;
}

char *unsandbox_image_publish(
    const char *source_type, const char *source_id,
    const char *name, const char *description,
    const char *public_key, const char *secret_key) {

    if (!source_type || !source_id) {
        set_last_error("Source type and ID are required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    // Use internal function
    char *result = image_publish(creds, source_type, source_id, name, description);
    free_credentials(creds);

    if (!result) {
        set_last_error("Failed to publish image");
        return NULL;
    }

    char *image_id = extract_json_string(result, "id");
    free(result);
    return image_id;
}

int unsandbox_image_delete(
    const char *image_id,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = delete_image(creds, image_id);
    free_credentials(creds);
    return result;
}

int unsandbox_image_lock(
    const char *image_id,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = lock_image(creds, image_id);
    free_credentials(creds);
    return result;
}

int unsandbox_image_unlock(
    const char *image_id,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = unlock_image(creds, image_id);
    free_credentials(creds);
    return result;
}

int unsandbox_image_set_visibility(
    const char *image_id, const char *visibility,
    const char *public_key, const char *secret_key) {

    if (!image_id || !visibility) {
        set_last_error("Image ID and visibility are required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = set_image_visibility(creds, image_id, visibility);
    free_credentials(creds);
    return result;
}

int unsandbox_image_grant_access(
    const char *image_id, const char *trusted_api_key,
    const char *public_key, const char *secret_key) {

    if (!image_id || !trusted_api_key) {
        set_last_error("Image ID and trusted API key are required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = grant_image_access(creds, image_id, trusted_api_key);
    free_credentials(creds);
    return result;
}

int unsandbox_image_revoke_access(
    const char *image_id, const char *trusted_api_key,
    const char *public_key, const char *secret_key) {

    if (!image_id || !trusted_api_key) {
        set_last_error("Image ID and trusted API key are required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = revoke_image_access(creds, image_id, trusted_api_key);
    free_credentials(creds);
    return result;
}

char **unsandbox_image_list_trusted(
    const char *image_id, size_t *count,
    const char *public_key, const char *secret_key) {

    if (!image_id || !count) {
        set_last_error("Image ID and count pointer are required");
        return NULL;
    }

    *count = 0;

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *result = list_image_trusted(creds, image_id);
    free_credentials(creds);

    if (!result) {
        set_last_error("Failed to list trusted keys");
        return NULL;
    }

    // Count keys in response
    const char *keys_start = strstr(result, "\"trusted_keys\":[");
    if (!keys_start) {
        free(result);
        return NULL;
    }

    size_t key_count = 0;
    const char *p = strchr(keys_start, '[');
    if (p) {
        p++;
        while (*p && *p != ']') {
            if (*p == '"') {
                key_count++;
                p++;
                while (*p && *p != '"') p++;
            }
            if (*p) p++;
        }
    }

    if (key_count == 0) {
        free(result);
        return NULL;
    }

    char **keys = calloc(key_count, sizeof(char *));
    if (!keys) {
        set_last_error("Out of memory");
        free(result);
        return NULL;
    }

    // Parse keys
    p = strchr(keys_start, '[');
    if (p) {
        p++;
        size_t i = 0;
        while (*p && *p != ']' && i < key_count) {
            if (*p == '"') {
                p++;
                const char *end = strchr(p, '"');
                if (end) {
                    size_t len = end - p;
                    keys[i] = malloc(len + 1);
                    if (keys[i]) {
                        memcpy(keys[i], p, len);
                        keys[i][len] = '\0';
                        i++;
                    }
                    p = end;
                }
            }
            if (*p) p++;
        }
        *count = i;
    }

    free(result);
    return keys;
}

int unsandbox_image_transfer(
    const char *image_id, const char *to_api_key,
    const char *public_key, const char *secret_key) {

    if (!image_id || !to_api_key) {
        set_last_error("Image ID and destination API key are required");
        return -1;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return -1;
    }

    int result = transfer_image(creds, image_id, to_api_key);
    free_credentials(creds);
    return result;
}

char *unsandbox_image_spawn(
    const char *image_id, const char *name, const char *ports,
    const char *bootstrap, const char *network_mode,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *result = spawn_from_image(creds, image_id, name, ports, bootstrap, network_mode);
    free_credentials(creds);

    if (!result) {
        set_last_error("Failed to spawn from image");
        return NULL;
    }

    char *service_id = extract_json_string(result, "id");
    free(result);
    return service_id;
}

char *unsandbox_image_clone(
    const char *image_id, const char *name, const char *description,
    const char *public_key, const char *secret_key) {

    if (!image_id) {
        set_last_error("Image ID is required");
        return NULL;
    }

    UnsandboxCredentials *creds = get_credentials(public_key, secret_key, -1);
    if (!creds) {
        set_last_error("No credentials available");
        return NULL;
    }

    char *result = clone_image(creds, image_id, name, description);
    free_credentials(creds);

    if (!result) {
        set_last_error("Failed to clone image");
        return NULL;
    }

    char *cloned_id = extract_json_string(result, "id");
    free(result);
    return cloned_id;
}

/* ============================================================================
 * Memory Management for Images
 * ============================================================================ */

void unsandbox_free_image(unsandbox_image_t *image) {
    if (!image) return;
    free(image->id);
    free(image->name);
    free(image->description);
    free(image->visibility);
    free(image->source_type);
    free(image->source_id);
    free(image->owner_api_key);
    free(image);
}

void unsandbox_free_image_list(unsandbox_image_list_t *images) {
    if (!images) return;
    for (size_t i = 0; i < images->count; i++) {
        free(images->images[i].id);
        free(images->images[i].name);
        free(images->images[i].description);
        free(images->images[i].visibility);
        free(images->images[i].source_type);
        free(images->images[i].source_id);
        free(images->images[i].owner_api_key);
    }
    free(images->images);
    free(images);
}

void unsandbox_free_trusted_keys(char **keys, size_t count) {
    if (!keys) return;
    for (size_t i = 0; i < count; i++) {
        free(keys[i]);
    }
    free(keys);
}

/* Utility */
const char *unsandbox_last_error(void) {
    return unsandbox_error_buffer[0] ? unsandbox_error_buffer : NULL;
}
#endif /* UNSANDBOX_LIBRARY - end of library API */

#ifndef UNSANDBOX_LIBRARY
int main(int argc, char *argv[]) {
    // Disable stdout buffering for real-time output
    setvbuf(stdout, NULL, _IONBF, 0);

    const char *filename = NULL;
    const char *cli_public_key = NULL;   // -p flag
    const char *cli_secret_key = NULL;   // -k flag
    int cli_account_index = -1;          // --account flag (-1 = use env or default)
    const char *artifact_dir = NULL;
    const char *network_mode = NULL;
    const char *shell = NULL;  // -s/--shell for language
    int vcpu = 0;  // 0 = default (1), valid values: 1, 2, 4, 8
    int ttl = 0;   // 0 = default (60s), valid values: 1-900
    int save_artifacts = 0;
    int skip_confirm = 0;  // -y flag to skip large upload confirmation

    struct InputFile input_files[MAX_INPUT_FILES];
    int input_file_count = 0;
    long total_input_size = 0;  // Track total input file size

    struct EnvVar env_vars[MAX_ENV_VARS];
    int env_var_count = 0;

    // Check for key command first
    if (argc >= 2 && strcmp(argv[1], "key") == 0) {
        int do_extend = 0;

        // Parse options
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "--extend") == 0) {
                do_extend = 1;
            }
        }

        // Get credentials (priority: flags > env > file with --account)
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);

        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        // Handle --extend: validate key to get public key, then open portal
        if (do_extend) {
            // If we have public_key from credentials, use it directly
            if (creds->public_key) {
                char extend_url[512];
                snprintf(extend_url, sizeof(extend_url), "%s/keys/extend?pk=%s", PORTAL_BASE, creds->public_key);
                printf("Opening extension page in browser...\n");
                printf("If browser doesn't open, visit: %s\n", extend_url);

                // Try to open URL in browser
                #ifdef __APPLE__
                char cmd[1024];
                snprintf(cmd, sizeof(cmd), "open '%s'", extend_url);
                if (system(cmd) != 0) { /* ignore */ }
                #elif defined(__linux__)
                char cmd[2048];
                snprintf(cmd, sizeof(cmd), "xdg-open '%s' 2>/dev/null || sensible-browser '%s' 2>/dev/null", extend_url, extend_url);
                if (system(cmd) != 0) { /* ignore */ }
                #elif defined(_WIN32)
                char cmd[1024];
                snprintf(cmd, sizeof(cmd), "start %s", extend_url);
                if (system(cmd) != 0) { /* ignore */ }
                #endif

                free_credentials(creds);
                return 0;
            }
        }

        // Default: validate key using HMAC authentication
        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret = validate_api_key(creds);
        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Check for languages command
    if (argc >= 2 && strcmp(argv[1], "languages") == 0) {
        int json_output = 0;

        // Parse options
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "--json") == 0) {
                json_output = 1;
            } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                fprintf(stderr, "Usage: %s languages [options]\n\n", argv[0]);
                fprintf(stderr, "List all available languages for code execution.\n\n");
                fprintf(stderr, "Options:\n");
                fprintf(stderr, "  --json    Output as JSON array (for scripts)\n");
                fprintf(stderr, "  -p KEY    Public key\n");
                fprintf(stderr, "  -k KEY    Secret key\n");
                fprintf(stderr, "  -h        Show this help\n");
                return 0;
            }
        }

        // Get credentials
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);

        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret = list_languages_cli(creds, json_output);
        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Check for snapshot command
    if (argc >= 2 && strcmp(argv[1], "snapshot") == 0) {
        const char *snapshot_id = NULL;
        const char *clone_type = NULL;
        const char *clone_name = NULL;
        const char *clone_shell = NULL;
        const char *clone_ports = NULL;
        int do_list = 0;
        int do_info = 0;
        int do_delete = 0;
        int do_lock = 0;
        int do_unlock = 0;
        int do_clone = 0;
        int show_help = 0;

        // Parse snapshot-specific args
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {
                do_list = 1;
            } else if (strcmp(argv[i], "--info") == 0 && i + 1 < argc) {
                do_info = 1;
                i++;
                snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--delete") == 0 && i + 1 < argc) {
                do_delete = 1;
                i++;
                snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--lock") == 0 && i + 1 < argc) {
                do_lock = 1;
                i++;
                snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--unlock") == 0 && i + 1 < argc) {
                do_unlock = 1;
                i++;
                snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--clone") == 0 && i + 1 < argc) {
                do_clone = 1;
                i++;
                snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--type") == 0 && i + 1 < argc) {
                i++;
                clone_type = argv[i];
            } else if (strcmp(argv[i], "--name") == 0 && i + 1 < argc) {
                i++;
                clone_name = argv[i];
            } else if (strcmp(argv[i], "--shell") == 0 && i + 1 < argc) {
                i++;
                clone_shell = argv[i];
            } else if (strcmp(argv[i], "--ports") == 0 && i + 1 < argc) {
                i++;
                clone_ports = argv[i];
            } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                show_help = 1;
            } else if (argv[i][0] == '-') {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                print_usage(argv[0]);
                return 1;
            }
        }

        if (show_help) {
            print_usage(argv[0]);
            return 0;
        }

        // Get credentials
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);
        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret = 0;

        if (do_list) {
            ret = list_snapshots(creds);
        } else if (do_info) {
            ret = get_snapshot_info(creds, snapshot_id);
        } else if (do_delete) {
            ret = delete_snapshot(creds, snapshot_id);
        } else if (do_lock) {
            ret = lock_snapshot(creds, snapshot_id);
        } else if (do_unlock) {
            ret = unlock_snapshot(creds, snapshot_id);
        } else if (do_clone) {
            if (!clone_type) {
                fprintf(stderr, "Error: --type required for --clone (session or service)\n");
                curl_global_cleanup();
                free_credentials(creds);
                return 1;
            }
            if (strcmp(clone_type, "session") != 0 && strcmp(clone_type, "service") != 0) {
                fprintf(stderr, "Error: --type must be 'session' or 'service'\n");
                curl_global_cleanup();
                free_credentials(creds);
                return 1;
            }
            ret = clone_snapshot(creds, snapshot_id, clone_type, clone_name, clone_shell, clone_ports);
        } else {
            fprintf(stderr, "Error: No snapshot action specified. Use --list, --info, --delete, --lock, --unlock, or --clone\n");
            print_usage(argv[0]);
            curl_global_cleanup();
            free_credentials(creds);
            return 1;
        }

        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Check for image command
    if (argc >= 2 && strcmp(argv[1], "image") == 0) {
        const char *image_id = NULL;
        const char *visibility = NULL;
        const char *spawn_name = NULL;
        const char *spawn_ports = NULL;
        const char *source_type = NULL;
        int do_list = 0;
        int do_info = 0;
        int do_delete = 0;
        int do_lock = 0;
        int do_unlock = 0;
        int do_publish = 0;
        int do_visibility = 0;
        int do_spawn = 0;
        int do_clone = 0;
        int show_help = 0;

        // Parse image-specific args
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {
                do_list = 1;
            } else if (strcmp(argv[i], "--info") == 0 && i + 1 < argc) {
                do_info = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--delete") == 0 && i + 1 < argc) {
                do_delete = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--lock") == 0 && i + 1 < argc) {
                do_lock = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--unlock") == 0 && i + 1 < argc) {
                do_unlock = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--publish") == 0 && i + 1 < argc) {
                do_publish = 1;
                i++;
                image_id = argv[i];  // This is actually the source ID (service/snapshot)
            } else if (strcmp(argv[i], "--visibility") == 0 && i + 2 < argc) {
                do_visibility = 1;
                i++;
                image_id = argv[i];
                i++;
                visibility = argv[i];
            } else if (strcmp(argv[i], "--spawn") == 0 && i + 1 < argc) {
                do_spawn = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--clone") == 0 && i + 1 < argc) {
                do_clone = 1;
                i++;
                image_id = argv[i];
            } else if (strcmp(argv[i], "--name") == 0 && i + 1 < argc) {
                i++;
                spawn_name = argv[i];
            } else if (strcmp(argv[i], "--ports") == 0 && i + 1 < argc) {
                i++;
                spawn_ports = argv[i];
            } else if (strcmp(argv[i], "--source-type") == 0 && i + 1 < argc) {
                i++;
                source_type = argv[i];
            } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                show_help = 1;
            } else if (argv[i][0] == '-') {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                print_usage(argv[0]);
                return 1;
            }
        }

        if (show_help) {
            print_usage(argv[0]);
            return 0;
        }

        // Get credentials
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);
        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret = 0;

        if (do_list) {
            char *response = list_images(creds, NULL);
            if (response) {
                // Parse and display image list from JSON response
                const char *images_start = strstr(response, "\"images\":[");
                if (!images_start) {
                    printf("No images found.\n");
                    free(response);
                } else {
                    // Count images
                    int count = 0;
                    const char *p = images_start;
                    while ((p = strchr(p + 1, '{')) != NULL && strchr(images_start, ']') && p < strchr(images_start, ']')) {
                        count++;
                    }

                    if (count == 0) {
                        printf("No images found.\n");
                    } else {
                        printf("%-40s %-20s %-10s %-8s %-10s\n", "ID", "NAME", "VISIBILITY", "LOCKED", "SOURCE");
                        printf("%-40s %-20s %-10s %-8s %-10s\n", "----------------------------------------",
                               "--------------------", "----------", "--------", "----------");

                        // Parse each image object
                        const char *pos = strchr(images_start, '[');
                        if (pos) {
                            while ((pos = strchr(pos, '{')) != NULL) {
                                // Check we haven't passed the end of the array
                                const char *arr_end = strchr(images_start, ']');
                                if (arr_end && pos > arr_end) break;

                                char *img_id = extract_json_string(pos, "id");
                                char *img_name = extract_json_string(pos, "name");
                                char *img_visibility = extract_json_string(pos, "visibility");
                                char *img_source = extract_json_string(pos, "source_type");

                                // Extract locked boolean
                                const char *locked_str = strstr(pos, "\"locked\":");
                                int locked = 0;
                                if (locked_str && locked_str < strchr(pos, '}')) {
                                    locked_str += 9;
                                    while (*locked_str == ' ') locked_str++;
                                    locked = (strncmp(locked_str, "true", 4) == 0);
                                }

                                printf("%-40s %-20s %-10s %-8s %-10s\n",
                                       img_id ? img_id : "",
                                       img_name ? img_name : "",
                                       img_visibility ? img_visibility : "",
                                       locked ? "yes" : "no",
                                       img_source ? img_source : "");

                                if (img_id) free(img_id);
                                if (img_name) free(img_name);
                                if (img_visibility) free(img_visibility);
                                if (img_source) free(img_source);

                                pos++;
                            }
                        }
                    }
                    free(response);
                }
            } else {
                fprintf(stderr, "Error: Failed to list images\n");
                ret = 1;
            }
        } else if (do_info) {
            char *response = get_image(creds, image_id);
            if (response) {
                printf("%s\n", response);
                free(response);
            } else {
                fprintf(stderr, "Error: Failed to get image info\n");
                ret = 1;
            }
        } else if (do_delete) {
            ret = delete_image(creds, image_id);
            if (ret == 0) {
                printf("Image deleted: %s\n", image_id);
            }
        } else if (do_lock) {
            ret = lock_image(creds, image_id);
            if (ret == 0) {
                printf("Image locked: %s\n", image_id);
            }
        } else if (do_unlock) {
            ret = unlock_image(creds, image_id);
            if (ret == 0) {
                printf("Image unlocked: %s\n", image_id);
            }
        } else if (do_visibility) {
            if (!visibility || (strcmp(visibility, "private") != 0 &&
                               strcmp(visibility, "unlisted") != 0 &&
                               strcmp(visibility, "public") != 0)) {
                fprintf(stderr, "Error: visibility must be 'private', 'unlisted', or 'public'\n");
                ret = 1;
            } else {
                ret = set_image_visibility(creds, image_id, visibility);
                if (ret == 0) {
                    printf("Image visibility set to %s: %s\n", visibility, image_id);
                }
            }
        } else if (do_spawn) {
            char *result = spawn_from_image(creds, image_id, spawn_name, spawn_ports, NULL, NULL);
            if (result) {
                printf("%s\n", result);
                free(result);
            } else {
                fprintf(stderr, "Error: Failed to spawn from image\n");
                ret = 1;
            }
        } else if (do_publish) {
            if (!source_type) {
                fprintf(stderr, "Error: --source-type required for --publish (service or snapshot)\n");
                ret = 1;
            } else {
                char *result = image_publish(creds, source_type, image_id, spawn_name, NULL);
                if (result) {
                    printf("Image published: %s\n", result);
                    free(result);
                } else {
                    fprintf(stderr, "Error: Failed to publish image\n");
                    ret = 1;
                }
            }
        } else if (do_clone) {
            char *result = clone_image(creds, image_id, spawn_name, NULL);
            if (result) {
                printf("Image cloned: %s\n", result);
                free(result);
            } else {
                fprintf(stderr, "Error: Failed to clone image\n");
                ret = 1;
            }
        } else {
            fprintf(stderr, "Error: No image action specified. Use --list, --info, --delete, --lock, --unlock, --visibility, --spawn, --publish, or --clone\n");
            print_usage(argv[0]);
            ret = 1;
        }

        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Check for service command first
    if (argc >= 2 && strcmp(argv[1], "service") == 0) {
        const char *service_name = NULL;
        const char *service_ports = NULL;
        const char *service_domains = NULL;
        const char *service_type = NULL;
        const char *golden_image = NULL;
        const char *service_bootstrap = NULL;
        const char *bootstrap_file = NULL;
        const char *service_id = NULL;
        struct InputFile service_input_files[MAX_INPUT_FILES];
        int service_input_file_count = 0;
        int show_help = 0;
        int do_list = 0;
        int do_info = 0;
        int do_tail = 0;
        int do_logs = 0;
        int do_download_logs = 0;
        const char *download_logs_file = NULL;
        int do_freeze = 0;
        int do_unfreeze = 0;
        int do_destroy = 0;
        int do_lock = 0;
        int do_unlock = 0;
        int do_auto_unfreeze = 0;
        int do_no_auto_unfreeze = 0;
        int do_show_freeze_page = 0;
        int do_no_show_freeze_page = 0;
        int do_resize = 0;
        int do_redeploy = 0;
        int do_execute = 0;
        const char *execute_command = NULL;
        int do_dump_bootstrap = 0;
        const char *dump_bootstrap_file = NULL;
        int do_snapshot = 0;
        int do_restore = 0;
        const char *restore_snapshot_id = NULL;
        const char *snapshot_name = NULL;
        int hot_snapshot = 0;
        int create_unfreeze_on_demand = 0;  // For --unfreeze-on-demand flag on create

        // Environment variables for service create
        char *service_env_content = NULL;  // Accumulated env vars (KEY=VALUE\n...)
        size_t service_env_size = 0;
        size_t service_env_capacity = 0;
        const char *service_env_file = NULL;  // --env-file path

        // Env subcommand: un service env status|set|export|delete <name>
        const char *env_subcommand = NULL;
        const char *env_target_id = NULL;

        // Parse service-specific args (flags like session)
        for (int i = 2; i < argc; i++) {
            // Check for "env" subcommand: un service env <action> <id>
            if (strcmp(argv[i], "env") == 0 && i + 2 < argc) {
                env_subcommand = argv[i + 1];  // status, set, export, delete
                env_target_id = argv[i + 2];   // service name/id
                i += 2;
                // Continue parsing for --env-file in case of "set"
                continue;
            }
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
                i++;
                network_mode = argv[i];
            } else if (strcmp(argv[i], "--name") == 0 && i + 1 < argc) {
                i++;
                service_name = argv[i];
            } else if (strcmp(argv[i], "--ports") == 0 && i + 1 < argc) {
                i++;
                service_ports = argv[i];
            } else if (strcmp(argv[i], "--domains") == 0 && i + 1 < argc) {
                i++;
                service_domains = argv[i];
            } else if (strcmp(argv[i], "--type") == 0 && i + 1 < argc) {
                i++;
                service_type = argv[i];
            } else if (strcmp(argv[i], "--golden-image") == 0 && i + 1 < argc) {
                i++;
                golden_image = argv[i];
            } else if (strcmp(argv[i], "--bootstrap") == 0 && i + 1 < argc) {
                i++;
                service_bootstrap = argv[i];
            } else if (strcmp(argv[i], "--bootstrap-file") == 0 && i + 1 < argc) {
                i++;
                bootstrap_file = argv[i];
            } else if (strcmp(argv[i], "-f") == 0 && i + 1 < argc) {
                i++;
                if (service_input_file_count >= MAX_INPUT_FILES) {
                    fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                    return 1;
                }
                // Read file and base64 encode
                size_t fsize;
                char *content = read_file(argv[i], &fsize);
                if (!content) {
                    fprintf(stderr, "Error: cannot read file '%s'\n", argv[i]);
                    return 1;
                }
                size_t b64_len;
                char *b64 = base64_encode((unsigned char *)content, fsize, &b64_len);
                free(content);
                if (!b64) {
                    fprintf(stderr, "Error: failed to encode file '%s'\n", argv[i]);
                    return 1;
                }
                service_input_files[service_input_file_count].filename = strdup(get_basename(argv[i]));
                service_input_files[service_input_file_count].content_base64 = b64;
                service_input_file_count++;
            } else if (strcmp(argv[i], "-F") == 0 && i + 1 < argc) {
                // -F preserves relative path (for directory structures)
                i++;
                if (service_input_file_count >= MAX_INPUT_FILES) {
                    fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                    return 1;
                }
                size_t fsize;
                char *content = read_file(argv[i], &fsize);
                if (!content) {
                    fprintf(stderr, "Error: cannot read file '%s'\n", argv[i]);
                    return 1;
                }
                size_t b64_len;
                char *b64 = base64_encode((unsigned char *)content, fsize, &b64_len);
                free(content);
                if (!b64) {
                    fprintf(stderr, "Error: failed to encode file '%s'\n", argv[i]);
                    return 1;
                }
                // Use full path instead of basename
                service_input_files[service_input_file_count].filename = strdup(argv[i]);
                service_input_files[service_input_file_count].content_base64 = b64;
                service_input_file_count++;
            } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {
                do_list = 1;
            } else if (strcmp(argv[i], "--info") == 0 && i + 1 < argc) {
                do_info = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--tail") == 0 && i + 1 < argc) {
                do_tail = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--logs") == 0 && i + 1 < argc) {
                do_logs = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--download-logs") == 0 && i + 2 < argc) {
                do_download_logs = 1;
                i++;
                service_id = argv[i];
                i++;
                download_logs_file = argv[i];
            } else if (strcmp(argv[i], "--freeze") == 0 && i + 1 < argc) {
                do_freeze = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--unfreeze") == 0 && i + 1 < argc) {
                do_unfreeze = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--destroy") == 0 && i + 1 < argc) {
                do_destroy = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--lock") == 0 && i + 1 < argc) {
                do_lock = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--unlock") == 0 && i + 1 < argc) {
                do_unlock = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--auto-unfreeze") == 0 && i + 1 < argc) {
                do_auto_unfreeze = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--no-auto-unfreeze") == 0 && i + 1 < argc) {
                do_no_auto_unfreeze = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--show-freeze-page") == 0 && i + 1 < argc) {
                do_show_freeze_page = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--no-show-freeze-page") == 0 && i + 1 < argc) {
                do_no_show_freeze_page = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--resize") == 0 && i + 1 < argc) {
                do_resize = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--redeploy") == 0 && i + 1 < argc) {
                do_redeploy = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--execute") == 0 && i + 2 < argc) {
                do_execute = 1;
                i++;
                service_id = argv[i];
                i++;
                execute_command = argv[i];
            } else if (strcmp(argv[i], "--dump-bootstrap") == 0 && i + 1 < argc) {
                do_dump_bootstrap = 1;
                i++;
                service_id = argv[i];
                // Optional file argument
                if (i + 1 < argc && argv[i + 1][0] != '-') {
                    i++;
                    dump_bootstrap_file = argv[i];
                }
            } else if ((strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--vcpu") == 0) && i + 1 < argc) {
                i++;
                vcpu = atoi(argv[i]);
                if (vcpu < 1 || vcpu > 8) {
                    fprintf(stderr, "Error: -v/--vcpu must be 1-8\n");
                    return 1;
                }
            } else if (strcmp(argv[i], "--snapshot") == 0 && i + 1 < argc) {
                do_snapshot = 1;
                i++;
                service_id = argv[i];
            } else if (strcmp(argv[i], "--restore") == 0 && i + 1 < argc) {
                do_restore = 1;
                i++;
                restore_snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--snapshot-name") == 0 && i + 1 < argc) {
                i++;
                snapshot_name = argv[i];
            } else if (strcmp(argv[i], "--hot") == 0) {
                hot_snapshot = 1;
            } else if (strcmp(argv[i], "--unfreeze-on-demand") == 0) {
                create_unfreeze_on_demand = 1;
            } else if ((strcmp(argv[i], "-e") == 0 || strcmp(argv[i], "--env") == 0) && i + 1 < argc) {
                // -e KEY=VALUE or --env KEY=VALUE - accumulate env vars
                i++;
                const char *env_pair = argv[i];
                // Validate format: must contain '='
                if (!strchr(env_pair, '=')) {
                    fprintf(stderr, "Error: Invalid environment variable format '%s'. Use KEY=VALUE\n", env_pair);
                    return 1;
                }
                // Add to accumulated env content
                size_t pair_len = strlen(env_pair);
                size_t needed = service_env_size + pair_len + 2;  // +1 for newline, +1 for null
                if (needed > service_env_capacity) {
                    service_env_capacity = needed > 4096 ? needed * 2 : 4096;
                    char *new_content = realloc(service_env_content, service_env_capacity);
                    if (!new_content) {
                        fprintf(stderr, "Error: Out of memory\n");
                        free(service_env_content);
                        return 1;
                    }
                    service_env_content = new_content;
                }
                memcpy(service_env_content + service_env_size, env_pair, pair_len);
                service_env_size += pair_len;
                service_env_content[service_env_size++] = '\n';
                service_env_content[service_env_size] = '\0';
            } else if (strcmp(argv[i], "--env-file") == 0 && i + 1 < argc) {
                i++;
                service_env_file = argv[i];
            } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                show_help = 1;
            }
        }

        // Show help if requested or no action specified
        if (show_help) {
            print_usage(argv[0]);
            return 0;
        }

        // Get credentials (priority: env > flags > file)
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);
        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret = 0;

        // Handle env subcommand first
        if (env_subcommand) {
            if (strcmp(env_subcommand, "status") == 0) {
                ret = service_env_status(creds, env_target_id);
            } else if (strcmp(env_subcommand, "set") == 0) {
                // Read env content from --env-file, -e flags, or stdin
                char *env_content = NULL;
                if (service_env_file) {
                    env_content = read_env_file(service_env_file);
                } else if (service_env_content && service_env_size > 0) {
                    env_content = service_env_content;
                    service_env_content = NULL;  // Transfer ownership
                } else {
                    // Check if stdin is a TTY
                    if (isatty(STDIN_FILENO)) {
                        fprintf(stderr, "Reading environment variables from stdin (Ctrl+D to finish):\n");
                    }
                    env_content = read_env_stdin();
                }
                if (env_content) {
                    ret = service_env_set(creds, env_target_id, env_content);
                    free(env_content);
                } else {
                    ret = 1;
                }
            } else if (strcmp(env_subcommand, "export") == 0) {
                ret = service_env_export(creds, env_target_id);
            } else if (strcmp(env_subcommand, "delete") == 0) {
                ret = service_env_delete(creds, env_target_id);
            } else {
                fprintf(stderr, "Error: Unknown env subcommand '%s'. Use: status, set, export, delete\n", env_subcommand);
                ret = 1;
            }
            free(service_env_content);
            curl_global_cleanup();
            free_credentials(creds);
            return ret;
        }

        if (do_list) {
            ret = list_services(creds);
        } else if (do_info) {
            ret = get_service_info(creds, service_id);
        } else if (do_tail) {
            // --tail: last 9000 lines (default)
            char *log = get_service_logs(creds, service_id, 0);
            if (log) {
                printf("%s", log);
                free(log);
                ret = 0;
            } else {
                fprintf(stderr, "Error: Failed to fetch logs (service not found or no logs available)\n");
                ret = 1;
            }
        } else if (do_logs) {
            // --logs: all logs
            char *log = get_service_logs(creds, service_id, 1);
            if (log) {
                printf("%s", log);
                free(log);
                ret = 0;
            } else {
                fprintf(stderr, "Error: Failed to fetch logs (service not found or no logs available)\n");
                ret = 1;
            }
        } else if (do_download_logs) {
            // --download-logs: all logs to file
            char *log = get_service_logs(creds, service_id, 1);
            if (log) {
                FILE *f = fopen(download_logs_file, "w");
                if (f) {
                    fprintf(f, "%s", log);
                    fclose(f);
                    printf("Logs saved to %s\n", download_logs_file);
                    ret = 0;
                } else {
                    fprintf(stderr, "Error: Could not open file %s for writing\n", download_logs_file);
                    ret = 1;
                }
                free(log);
            } else {
                fprintf(stderr, "Error: Failed to fetch logs (service not found or no logs available)\n");
                ret = 1;
            }
        } else if (do_freeze) {
            ret = freeze_service(creds, service_id);
        } else if (do_unfreeze) {
            ret = unfreeze_service(creds, service_id);
        } else if (do_destroy) {
            ret = destroy_service(creds, service_id);
        } else if (do_lock) {
            ret = lock_service(creds, service_id);
        } else if (do_unlock) {
            ret = unlock_service(creds, service_id);
        } else if (do_auto_unfreeze) {
            ret = set_unfreeze_on_demand(creds, service_id, 1);
        } else if (do_no_auto_unfreeze) {
            ret = set_unfreeze_on_demand(creds, service_id, 0);
        } else if (do_show_freeze_page) {
            ret = set_show_freeze_page(creds, service_id, 1);
        } else if (do_no_show_freeze_page) {
            ret = set_show_freeze_page(creds, service_id, 0);
        } else if (do_resize) {
            if (vcpu < 1 || vcpu > 8) {
                fprintf(stderr, "Error: --vcpu must be 1-8 for resize\n");
                curl_global_cleanup();
                free_credentials(creds);
                return 1;
            }
            ret = resize_service(creds, service_id, vcpu);
        } else if (do_redeploy) {
            // Bootstrap is optional for redeploy:
            // - If provided via --bootstrap or --bootstrap-file, use it
            // - If omitted, API will use the stored encrypted bootstrap
            const char *bootstrap_to_use = bootstrap_file ? bootstrap_file : service_bootstrap;
            ret = redeploy_service(creds, service_id, bootstrap_to_use);
        } else if (do_execute) {
            // Default timeout 30 seconds (30000ms)
            ret = execute_service(creds, service_id, execute_command, 30000);
        } else if (do_dump_bootstrap) {
            // Dump bootstrap script from /tmp/bootstrap.sh inside the service
            // This is useful for migrations - the bootstrap is stored at the same path on all instances
            fprintf(stderr, "Fetching bootstrap script from %s...\n", service_id);

            // Use execute to cat the bootstrap file
            char *bootstrap = execute_service_capture(creds, service_id, "cat /tmp/bootstrap.sh", 30000);
            if (bootstrap) {
                if (dump_bootstrap_file) {
                    // Write to file
                    FILE *f = fopen(dump_bootstrap_file, "w");
                    if (f) {
                        fprintf(f, "%s", bootstrap);
                        fclose(f);
                        // Make executable
                        chmod(dump_bootstrap_file, 0755);
                        printf("Bootstrap saved to %s\n", dump_bootstrap_file);
                        ret = 0;
                    } else {
                        fprintf(stderr, "Error: Could not open file %s for writing\n", dump_bootstrap_file);
                        ret = 1;
                    }
                } else {
                    // Print to stdout
                    printf("%s", bootstrap);
                    ret = 0;
                }
                free(bootstrap);
            } else {
                fprintf(stderr, "Error: Failed to fetch bootstrap (service not running or no bootstrap file)\n");
                ret = 1;
            }
        } else if (do_snapshot) {
            ret = create_service_snapshot(creds, service_id, snapshot_name, hot_snapshot);
        } else if (do_restore) {
            ret = restore_from_snapshot(creds, restore_snapshot_id, "Service");
        } else if (service_name) {
            // Create service (default action when --name is provided)
            char *bootstrap_content = NULL;

            // If --bootstrap-file provided, read its contents
            if (bootstrap_file && strlen(bootstrap_file) > 0) {
                struct stat st;
                if (stat(bootstrap_file, &st) != 0 || !S_ISREG(st.st_mode)) {
                    fprintf(stderr, "Error: Bootstrap file not found: '%s'\n", bootstrap_file);
                    curl_global_cleanup();
                    free_credentials(creds);
                    return 1;
                }
                size_t fsize;
                bootstrap_content = read_file(bootstrap_file, &fsize);
                if (!bootstrap_content) {
                    fprintf(stderr, "Error: Failed to read bootstrap file '%s'\n", bootstrap_file);
                    curl_global_cleanup();
                    free_credentials(creds);
                    return 1;
                }
                fprintf(stderr, "Read bootstrap script (%zu bytes) from %s\n", fsize, bootstrap_file);
            }

            fprintf(stderr, "Creating service '%s'...", service_name);
            if (service_input_file_count > 0) {
                fprintf(stderr, " (%d files)...", service_input_file_count);
            }
            fflush(stderr);
            char *created_id = create_service(creds, service_name, service_ports, service_domains, service_bootstrap, bootstrap_content, network_mode, vcpu, service_type, service_input_files, service_input_file_count, golden_image, create_unfreeze_on_demand);
            if (bootstrap_content) free(bootstrap_content);
            // Free input file memory
            for (int i = 0; i < service_input_file_count; i++) {
                free(service_input_files[i].filename);
                free(service_input_files[i].content_base64);
            }

            if (created_id) {
                fprintf(stderr, " done\n");
                printf("\033[32mService created successfully\033[0m\n");
                printf("Service ID: %s\n", created_id);

                // Set environment vault if env vars were provided
                char *env_content_to_set = NULL;
                if (service_env_file) {
                    env_content_to_set = read_env_file(service_env_file);
                } else if (service_env_content && service_env_size > 0) {
                    env_content_to_set = service_env_content;
                    service_env_content = NULL;  // Transfer ownership
                }
                if (env_content_to_set) {
                    fprintf(stderr, "Setting environment vault...\n");
                    int env_ret = service_env_set(creds, created_id, env_content_to_set);
                    free(env_content_to_set);
                    if (env_ret != 0) {
                        fprintf(stderr, "\033[33mWarning: Failed to set environment vault\033[0m\n");
                    }
                }

                // Wait a moment then check bootstrap logs
                if ((service_bootstrap && strlen(service_bootstrap) > 0) ||
                    (bootstrap_file && strlen(bootstrap_file) > 0)) {
                    fprintf(stderr, "Checking bootstrap status...\n");
                    sleep(2);  // Give bootstrap time to start
                    char *log = get_service_logs(creds, created_id, 0);
                    if (log && strlen(log) > 0) {
                        printf("\n--- Bootstrap Log ---\n%s\n--- End Log ---\n", log);
                        free(log);
                    }
                }

                free(created_id);
                ret = 0;
            } else {
                fprintf(stderr, " failed\n");
                // Try to get logs if we can find the service by name
                // The service might exist even if create returned error
                fprintf(stderr, "Attempting to fetch bootstrap logs...\n");
                char *log = get_service_logs(creds, service_name, 0);
                if (log && strlen(log) > 0) {
                    fprintf(stderr, "\n\033[31m--- Bootstrap Log ---\033[0m\n%s\n\033[31m--- End Log ---\033[0m\n", log);
                    free(log);
                }
                ret = 1;
            }
            // Clean up env content if not transferred
            free(service_env_content);
        } else {
            // No action specified, show help
            free(service_env_content);
            fprintf(stderr, "Error: No service action specified. Use --list, --info, --name, etc.\n");
            print_usage(argv[0]);
            curl_global_cleanup();
            free_credentials(creds);
            return 1;
        }

        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Check for session command
    if (argc >= 2 && strcmp(argv[1], "session") == 0) {
        int audit_history = 0;
        int list_only = 0;
        const char *shell = NULL;
        const char *attach_to = NULL;
        const char *kill_target = NULL;
        const char *freeze_target = NULL;
        const char *unfreeze_target = NULL;
        const char *boost_target = NULL;
        int boost_vcpu = 0;
        const char *unboost_target = NULL;
        const char *multiplexer = NULL;  // NULL = no multiplexer, "tmux", or "screen"
        struct InputFile session_input_files[MAX_INPUT_FILES];
        int session_input_file_count = 0;
        const char *snapshot_target = NULL;
        const char *restore_snapshot_id = NULL;
        const char *snapshot_name = NULL;
        int hot_snapshot = 0;
        // Parse shell-specific args
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
                i++;
                cli_public_key = argv[i];
            } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
                i++;
                cli_secret_key = argv[i];
            } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
                i++;
                cli_account_index = atoi(argv[i]);
            } else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
                i++;
                network_mode = argv[i];
            } else if ((strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--shell") == 0) && i + 1 < argc) {
                i++;
                shell = argv[i];
            } else if (strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "--artifacts") == 0) {
                save_artifacts = 1;
            } else if (strcmp(argv[i], "--audit") == 0) {
                audit_history = 1;
                save_artifacts = 1;  // --audit implies -a
            } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
                i++;
                artifact_dir = argv[i];
                save_artifacts = 1;  // -o implies -a
            } else if (strcmp(argv[i], "-l") == 0 || strcmp(argv[i], "--list") == 0) {
                list_only = 1;
            } else if (strcmp(argv[i], "--attach") == 0 && i + 1 < argc) {
                i++;
                attach_to = argv[i];
            } else if (strcmp(argv[i], "--kill") == 0 && i + 1 < argc) {
                i++;
                kill_target = argv[i];
            } else if (strcmp(argv[i], "--freeze") == 0 && i + 1 < argc) {
                i++;
                freeze_target = argv[i];
            } else if (strcmp(argv[i], "--unfreeze") == 0 && i + 1 < argc) {
                i++;
                unfreeze_target = argv[i];
            } else if (strcmp(argv[i], "--boost") == 0 && i + 1 < argc) {
                i++;
                boost_target = argv[i];
            } else if (strcmp(argv[i], "--boost-vcpu") == 0 && i + 1 < argc) {
                i++;
                boost_vcpu = atoi(argv[i]);
            } else if (strcmp(argv[i], "--unboost") == 0 && i + 1 < argc) {
                i++;
                unboost_target = argv[i];
            } else if (strcmp(argv[i], "--tmux") == 0) {
                multiplexer = "tmux";
            } else if (strcmp(argv[i], "--screen") == 0) {
                multiplexer = "screen";
            } else if (strcmp(argv[i], "-f") == 0 && i + 1 < argc) {
                i++;
                if (session_input_file_count >= MAX_INPUT_FILES) {
                    fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                    return 1;
                }
                size_t fsize;
                char *content = read_file(argv[i], &fsize);
                if (!content) {
                    fprintf(stderr, "Error: cannot read file '%s'\n", argv[i]);
                    return 1;
                }
                size_t b64_len;
                char *b64 = base64_encode((unsigned char *)content, fsize, &b64_len);
                free(content);
                if (!b64) {
                    fprintf(stderr, "Error: failed to encode file '%s'\n", argv[i]);
                    return 1;
                }
                session_input_files[session_input_file_count].filename = strdup(get_basename(argv[i]));
                session_input_files[session_input_file_count].content_base64 = b64;
                session_input_file_count++;
            } else if (strcmp(argv[i], "-F") == 0 && i + 1 < argc) {
                // -F preserves relative path (for directory structures)
                i++;
                if (session_input_file_count >= MAX_INPUT_FILES) {
                    fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                    return 1;
                }
                size_t fsize;
                char *content = read_file(argv[i], &fsize);
                if (!content) {
                    fprintf(stderr, "Error: cannot read file '%s'\n", argv[i]);
                    return 1;
                }
                size_t b64_len;
                char *b64 = base64_encode((unsigned char *)content, fsize, &b64_len);
                free(content);
                if (!b64) {
                    fprintf(stderr, "Error: failed to encode file '%s'\n", argv[i]);
                    return 1;
                }
                // Use full path instead of basename
                session_input_files[session_input_file_count].filename = strdup(argv[i]);
                session_input_files[session_input_file_count].content_base64 = b64;
                session_input_file_count++;
            } else if ((strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--vcpu") == 0) && i + 1 < argc) {
                i++;
                vcpu = atoi(argv[i]);
                if (vcpu < 1 || vcpu > 8) {
                    fprintf(stderr, "Error: -v/--vcpu must be 1-8\n");
                    return 1;
                }
            } else if (strcmp(argv[i], "--snapshot") == 0 && i + 1 < argc) {
                i++;
                snapshot_target = argv[i];
            } else if (strcmp(argv[i], "--restore") == 0 && i + 1 < argc) {
                i++;
                restore_snapshot_id = argv[i];
            } else if (strcmp(argv[i], "--snapshot-name") == 0 && i + 1 < argc) {
                i++;
                snapshot_name = argv[i];
            } else if (strcmp(argv[i], "--hot") == 0) {
                hot_snapshot = 1;
            } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
                print_usage(argv[0]);
                return 0;
            } else if (argv[i][0] == '-') {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                print_usage(argv[0]);
                return 1;
            }
        }

        // Get credentials (priority: env > flags > file)
        UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);
        if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
            fprintf(stderr, "Error: API credentials required.\n");
            fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
            fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
            fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
            free_credentials(creds);
            return 1;
        }

        curl_global_init(CURL_GLOBAL_DEFAULT);
        int ret;
        if (list_only) {
            ret = list_sessions(creds);
        } else if (kill_target) {
            ret = kill_session(creds, kill_target);
        } else if (freeze_target) {
            ret = freeze_session(creds, freeze_target);
        } else if (unfreeze_target) {
            ret = unfreeze_session(creds, unfreeze_target);
        } else if (boost_target) {
            if (boost_vcpu == 0) boost_vcpu = 2;  // Default boost: 2 vCPU
            ret = boost_session(creds, boost_target, boost_vcpu);
        } else if (unboost_target) {
            ret = unboost_session(creds, unboost_target);
        } else if (snapshot_target) {
            ret = create_session_snapshot(creds, snapshot_target, snapshot_name, hot_snapshot);
        } else if (restore_snapshot_id) {
            ret = restore_from_snapshot(creds, restore_snapshot_id, "Session");
        } else if (attach_to) {
            ret = reconnect_session(creds, attach_to, save_artifacts, artifact_dir, audit_history);
        } else {
            ret = shell_command(creds, network_mode, save_artifacts, artifact_dir, audit_history, shell, multiplexer, vcpu, session_input_files, session_input_file_count);
        }
        curl_global_cleanup();
        free_credentials(creds);
        return ret;
    }

    // Parse arguments for execute command (default)
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
            i++;
            network_mode = argv[i];
        } else if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
            i++;
            char *eq = strchr(argv[i], '=');
            if (!eq) {
                fprintf(stderr, "Error: -e requires KEY=VALUE format\n");
                return 1;
            }
            if (env_var_count >= MAX_ENV_VARS) {
                fprintf(stderr, "Error: too many env vars (max %d)\n", MAX_ENV_VARS);
                return 1;
            }
            env_vars[env_var_count].key = strndup(argv[i], eq - argv[i]);
            env_vars[env_var_count].value = strdup(eq + 1);
            env_var_count++;
        } else if (strcmp(argv[i], "-f") == 0 && i + 1 < argc) {
            i++;
            if (input_file_count >= MAX_INPUT_FILES) {
                fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                return 1;
            }
            size_t fsize;
            char *content = read_file(argv[i], &fsize);
            if (!content) return 1;

            // Check total size limit
            total_input_size += fsize;
            if (total_input_size > MAX_TOTAL_INPUT_SIZE) {
                fprintf(stderr, "Error: total input file size exceeds limit (max 4GB)\n");
                free(content);
                return 1;
            }

            size_t b64_len;
            char *b64 = base64_encode((unsigned char*)content, fsize, &b64_len);
            free(content);
            if (!b64) {
                fprintf(stderr, "Error: failed to encode file\n");
                return 1;
            }

            input_files[input_file_count].filename = strdup(get_basename(argv[i]));
            input_files[input_file_count].content_base64 = b64;
            input_file_count++;
        } else if (strcmp(argv[i], "-F") == 0 && i + 1 < argc) {
            // -F preserves relative path (for directory structures)
            i++;
            if (input_file_count >= MAX_INPUT_FILES) {
                fprintf(stderr, "Error: too many input files (max %d)\n", MAX_INPUT_FILES);
                return 1;
            }
            size_t fsize;
            char *content = read_file(argv[i], &fsize);
            if (!content) return 1;

            // Check total size limit
            total_input_size += fsize;
            if (total_input_size > MAX_TOTAL_INPUT_SIZE) {
                fprintf(stderr, "Error: total input file size exceeds limit (max 4GB)\n");
                free(content);
                return 1;
            }

            size_t b64_len;
            char *b64 = base64_encode((unsigned char*)content, fsize, &b64_len);
            free(content);
            if (!b64) {
                fprintf(stderr, "Error: failed to encode file\n");
                return 1;
            }

            // Use full path instead of basename
            input_files[input_file_count].filename = strdup(argv[i]);
            input_files[input_file_count].content_base64 = b64;
            input_file_count++;
        } else if (strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "--artifacts") == 0) {
            save_artifacts = 1;
        } else if (strcmp(argv[i], "-y") == 0 || strcmp(argv[i], "--yes") == 0) {
            skip_confirm = 1;
        } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            i++;
            artifact_dir = argv[i];
        } else if (strcmp(argv[i], "-p") == 0 && i + 1 < argc) {
            i++;
            cli_public_key = argv[i];
        } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
            i++;
            cli_secret_key = argv[i];
        } else if (strcmp(argv[i], "--account") == 0 && i + 1 < argc) {
            i++;
            cli_account_index = atoi(argv[i]);
        } else if ((strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--vcpu") == 0) && i + 1 < argc) {
            i++;
            vcpu = atoi(argv[i]);
            if (vcpu < 1 || vcpu > 8) {
                fprintf(stderr, "Error: -v/--vcpu must be 1-8\n");
                return 1;
            }
        } else if ((strcmp(argv[i], "-t") == 0 || strcmp(argv[i], "--ttl") == 0) && i + 1 < argc) {
            i++;
            ttl = atoi(argv[i]);
            if (ttl < 1 || ttl > 900) {
                fprintf(stderr, "Error: -t/--ttl must be 1-900 seconds\n");
                return 1;
            }
        } else if ((strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--shell") == 0) && i + 1 < argc) {
            i++;
            shell = argv[i];
        } else if (argv[i][0] != '-') {
            filename = argv[i];
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            print_usage(argv[0]);
            return 1;
        }
    }

    if (!filename) {
        print_usage(argv[0]);
        return 1;
    }

    // Warn about large uploads and confirm
    if (total_input_size > LARGE_UPLOAD_WARN_SIZE && !skip_confirm) {
        fprintf(stderr, "Warning: uploading %.1f GB of input files. This may take a while (base64 encoded).\n",
                (double)total_input_size / (1024.0 * 1024.0 * 1024.0));
        fprintf(stderr, "Continue? [y/N] ");
        int c = getchar();
        if (c != 'y' && c != 'Y') {
            fprintf(stderr, "Aborted. Use -y to skip this confirmation.\n");
            return 1;
        }
        // Consume rest of line
        while (c != '\n' && c != EOF) c = getchar();
    }

    // Get credentials (priority: env > flags > file)
    UnsandboxCredentials *creds = get_credentials(cli_public_key, cli_secret_key, cli_account_index);
    if (!creds || !creds->public_key || strlen(creds->public_key) == 0) {
        fprintf(stderr, "Error: API credentials required.\n");
        fprintf(stderr, "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
        fprintf(stderr, "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
        fprintf(stderr, "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n");
        free_credentials(creds);
        return 1;
    }

    // Get code: if -s is given OR file doesn't exist, treat as inline code
    size_t code_size;
    char *code;
    int inline_mode = 0;
    if (shell) {
        // Explicit -s flag: treat as inline code
        inline_mode = 1;
    } else if (access(filename, F_OK) != 0) {
        // File doesn't exist: assume bash inline code
        shell = "bash";
        inline_mode = 1;
    }

    if (inline_mode) {
        // Inline code mode: argument is the code itself
        code = strdup(filename);
        code_size = strlen(code);
    } else {
        // File mode: read code from file
        code = read_file(filename, &code_size);
        if (!code) return 1;
    }

    // Detect language (use -s/--shell if provided, otherwise auto-detect)
    const char *language = shell;
    if (!language) {
        language = detect_language_from_extension(filename);
    }
    if (!language) {
        language = detect_language_from_shebang(code);
    }
    if (!language) {
        fprintf(stderr, "Error: cannot detect language from file extension or shebang\n");
        fprintf(stderr, "  Use -s/--shell to specify the language (e.g., -s bash, -s python)\n");
        free(code);
        return 1;
    }

    // Escape code for JSON
    char *escaped_code = escape_json_string(code);
    free(code);
    if (!escaped_code) {
        fprintf(stderr, "Error: failed to escape code\n");
        return 1;
    }

    // Build JSON payload
    size_t payload_size = strlen(escaped_code) + 4096;
    for (int i = 0; i < input_file_count; i++) {
        payload_size += strlen(input_files[i].content_base64) + 256;
    }
    for (int i = 0; i < env_var_count; i++) {
        payload_size += strlen(env_vars[i].key) + strlen(env_vars[i].value) + 32;
    }

    char *json_payload = malloc(payload_size);
    if (!json_payload) {
        fprintf(stderr, "Error: out of memory\n");
        free(escaped_code);
        return 1;
    }

    char *p = json_payload;
    p += sprintf(p, "{\"language\":\"%s\",\"code\":\"%s\"", language, escaped_code);
    free(escaped_code);

    // Add input files
    if (input_file_count > 0) {
        p += sprintf(p, ",\"input_files\":[");
        for (int i = 0; i < input_file_count; i++) {
            if (i > 0) *p++ = ',';
            char *esc_filename = escape_json_string(input_files[i].filename);
            p += sprintf(p, "{\"filename\":\"%s\",\"content\":\"%s\"}",
                        esc_filename, input_files[i].content_base64);
            free(esc_filename);
            free(input_files[i].filename);
            free(input_files[i].content_base64);
        }
        p += sprintf(p, "]");
    }

    // Add env vars
    if (env_var_count > 0) {
        p += sprintf(p, ",\"env\":{");
        for (int i = 0; i < env_var_count; i++) {
            if (i > 0) *p++ = ',';
            char *esc_key = escape_json_string(env_vars[i].key);
            char *esc_val = escape_json_string(env_vars[i].value);
            p += sprintf(p, "\"%s\":\"%s\"", esc_key, esc_val);
            free(esc_key);
            free(esc_val);
            free(env_vars[i].key);
            free(env_vars[i].value);
        }
        p += sprintf(p, "}");
    }

    // Add artifact flag
    if (save_artifacts) {
        p += sprintf(p, ",\"return_artifact\":true");
    }

    // Add vcpu if specified (> 1)
    if (vcpu > 1) {
        p += sprintf(p, ",\"vcpu\":%d", vcpu);
    }

    // Add network_mode if specified
    if (network_mode && strlen(network_mode) > 0) {
        p += sprintf(p, ",\"network_mode\":\"%s\"", network_mode);
    }

    // Add TTL if specified
    if (ttl > 0) {
        p += sprintf(p, ",\"ttl\":%d", ttl);
    }

    p += sprintf(p, "}");

    // Initialize libcurl
    curl_global_init(CURL_GLOBAL_DEFAULT);
    CURL *curl = curl_easy_init();
    if (!curl) {
        fprintf(stderr, "Error: failed to initialize curl\n");
        free(json_payload);
        curl_global_cleanup();
        return 1;
    }

    // Set up response buffer
    struct ResponseBuffer response = {0};
    response.data = malloc(1);
    response.size = 0;

    // Set up request headers with HMAC authentication
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = add_hmac_auth_headers(headers, creds, "POST", "/execute", json_payload);

    // Configure curl
    curl_easy_setopt(curl, CURLOPT_URL, API_URL);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_payload);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&response);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "un-cli/2.0");

    // Set timeout based on TTL (or default to 120 seconds)
    long curl_timeout = (ttl > 0) ? (ttl + 30) : 120;  // Add 30s buffer
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, curl_timeout);

    // Perform request
    CURLcode res = curl_easy_perform(curl);

    if (res != CURLE_OK) {
        fprintf(stderr, "Error: request failed: %s\n", curl_easy_strerror(res));
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        free(json_payload);
        free(response.data);
        curl_global_cleanup();
        return 1;
    }

    // Check HTTP status code
    long http_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

    if (http_code != 200) {
        fprintf(stderr, "Error: HTTP %ld\n", http_code);
        if (response.data) {
            fprintf(stderr, "%s\n", response.data);
        }
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        free(json_payload);
        free(response.data);
        curl_global_cleanup();
        return 1;
    }

    // Check if response contains job_id (async execution)
    char *job_id = NULL;
    char *status = NULL;
    char *final_data = response.data;

    if (response.data) {
        job_id = extract_json_string(response.data, "job_id");
        status = extract_json_string(response.data, "status");
    }

    // If we got a job_id and status isn't terminal, we need to poll
    if (job_id && status) {
        int need_poll = (strcmp(status, "pending") == 0 ||
                        strcmp(status, "running") == 0);

        if (need_poll) {
            // Free initial response, poll for final result
            free(response.data);
            final_data = poll_job_status(creds, job_id);
        }
    }

    // Parse and print final response
    if (final_data) {
        parse_and_print_response(final_data, save_artifacts, artifact_dir, filename);
        if (final_data != response.data) {
            free(final_data);
        }
    }

    // Cleanup
    if (job_id) free(job_id);
    if (status) free(status);
    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
    free(json_payload);
    if (final_data == response.data) {
        free(response.data);
    }
    curl_global_cleanup();
    free_credentials(creds);

    return 0;
}
#endif /* UNSANDBOX_LIBRARY */
