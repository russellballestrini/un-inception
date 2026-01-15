/*
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com C SDK Implementation
 */

#include "unsandbox.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>
#include <ctype.h>
#include <curl/curl.h>
#include <openssl/hmac.h>
#include <openssl/sha.h>

#define API_BASE "https://api.unsandbox.com"
#define LANGUAGES_CACHE_TTL 3600
#define MAX_POLL_ATTEMPTS 100

static char g_last_error[1024] = {0};

/* ============================================================================
 * Utility Macros and Helpers
 * ============================================================================ */

#define SET_ERROR(msg, ...) \
    do { \
        snprintf(g_last_error, sizeof(g_last_error), msg, ##__VA_ARGS__); \
    } while(0)

typedef struct {
    char *data;
    size_t size;
    size_t capacity;
} buffer_t;

static void buffer_init(buffer_t *buf) {
    buf->data = NULL;
    buf->size = 0;
    buf->capacity = 0;
}

static void buffer_append(buffer_t *buf, const char *data, size_t len) {
    if (buf->size + len >= buf->capacity) {
        buf->capacity = (buf->size + len) * 2 + 1;
        buf->data = realloc(buf->data, buf->capacity);
    }
    memcpy(&buf->data[buf->size], data, len);
    buf->size += len;
}

static void buffer_free(buffer_t *buf) {
    if (buf->data) free(buf->data);
    buffer_init(buf);
}

static size_t http_write_callback(void *data, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    buffer_t *buf = (buffer_t *)userp;
    buffer_append(buf, (const char *)data, realsize);
    return realsize;
}

/* ============================================================================
 * HMAC-SHA256
 * ============================================================================ */

char *hmac_sha256(const char *key, const char *message) {
    if (!key || !message) return NULL;

    unsigned char digest[SHA256_DIGEST_LENGTH];
    unsigned int digest_len = SHA256_DIGEST_LENGTH;

    HMAC(EVP_sha256(),
         (unsigned char *)key, strlen(key),
         (unsigned char *)message, strlen(message),
         digest, &digest_len);

    char *result = malloc(SHA256_DIGEST_LENGTH * 2 + 1);
    for (int i = 0; i < SHA256_DIGEST_LENGTH; i++) {
        sprintf(&result[i * 2], "%02x", digest[i]);
    }
    result[SHA256_DIGEST_LENGTH * 2] = '\0';
    return result;
}

/* ============================================================================
 * Language Detection
 * ============================================================================ */

static const char *language_extensions[][2] = {
    {"py", "python"},
    {"js", "javascript"},
    {"ts", "typescript"},
    {"rb", "ruby"},
    {"php", "php"},
    {"pl", "perl"},
    {"sh", "bash"},
    {"r", "r"},
    {"R", "r"},
    {"lua", "lua"},
    {"go", "go"},
    {"rs", "rust"},
    {"c", "c"},
    {"cpp", "cpp"},
    {"cc", "cpp"},
    {"cxx", "cpp"},
    {"java", "java"},
    {"kt", "kotlin"},
    {"m", "objc"},
    {"cs", "csharp"},
    {"fs", "fsharp"},
    {"hs", "haskell"},
    {"ml", "ocaml"},
    {"clj", "clojure"},
    {"scm", "scheme"},
    {"ss", "scheme"},
    {"erl", "erlang"},
    {"ex", "elixir"},
    {"exs", "elixir"},
    {"jl", "julia"},
    {"d", "d"},
    {"nim", "nim"},
    {"zig", "zig"},
    {"v", "v"},
    {"cr", "crystal"},
    {"dart", "dart"},
    {"groovy", "groovy"},
    {"f90", "fortran"},
    {"f95", "fortran"},
    {"lisp", "commonlisp"},
    {"lsp", "commonlisp"},
    {"cob", "cobol"},
    {"tcl", "tcl"},
    {"raku", "raku"},
    {"pro", "prolog"},
    {"p", "prolog"},
    {"4th", "forth"},
    {"forth", "forth"},
    {"fth", "forth"},
    {NULL, NULL}
};

const char *unsandbox_detect_language(const char *filename) {
    if (!filename) return NULL;

    const char *ext = strrchr(filename, '.');
    if (!ext) return NULL;
    ext++;

    for (int i = 0; language_extensions[i][0]; i++) {
        if (strcmp(ext, language_extensions[i][0]) == 0) {
            return language_extensions[i][1];
        }
    }

    return NULL;
}

/* ============================================================================
 * Credential Resolution
 * ============================================================================ */

static char *strdup_safe(const char *str) {
    if (!str) return NULL;
    char *dup = malloc(strlen(str) + 1);
    strcpy(dup, str);
    return dup;
}

static int load_credentials_from_csv(const char *path, int account_index, char **pk, char **sk) {
    FILE *fp = fopen(path, "r");
    if (!fp) return -1;

    char line[2048];
    int current_index = 0;

    while (fgets(line, sizeof(line), fp)) {
        char *p = line;
        while (*p && isspace(*p)) p++;

        if (!*p || *p == '#') continue;

        char *newline = strchr(line, '\n');
        if (newline) *newline = '\0';

        if (current_index == account_index) {
            char *comma = strchr(line, ',');
            if (comma) {
                *comma = '\0';
                *pk = strdup_safe(line);
                *sk = strdup_safe(comma + 1);
                fclose(fp);
                return 0;
            }
        }
        current_index++;
    }

    fclose(fp);
    return -1;
}

int unsandbox_resolve_credentials(
    char **public_key_out,
    char **secret_key_out,
    const char *public_key_hint,
    const char *secret_key_hint
) {
    if (!public_key_out || !secret_key_out) return -1;

    *public_key_out = NULL;
    *secret_key_out = NULL;

    if (public_key_hint && secret_key_hint) {
        *public_key_out = strdup_safe(public_key_hint);
        *secret_key_out = strdup_safe(secret_key_hint);
        return 0;
    }

    const char *env_pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *env_sk = getenv("UNSANDBOX_SECRET_KEY");
    if (env_pk && env_sk) {
        *public_key_out = strdup_safe(env_pk);
        *secret_key_out = strdup_safe(env_sk);
        return 0;
    }

    int account_index = 0;
    const char *account_env = getenv("UNSANDBOX_ACCOUNT");
    if (account_env) {
        account_index = atoi(account_env);
    }

    char home_csv[1024];
    const char *home = getenv("HOME");
    if (home) {
        snprintf(home_csv, sizeof(home_csv), "%s/.unsandbox/accounts.csv", home);
        if (load_credentials_from_csv(home_csv, account_index, public_key_out, secret_key_out) == 0) {
            return 0;
        }
    }

    if (load_credentials_from_csv("./accounts.csv", account_index, public_key_out, secret_key_out) == 0) {
        return 0;
    }

    SET_ERROR("No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY.");
    return -1;
}

/* ============================================================================
 * HTTP Request Helpers
 * ============================================================================ */

typedef struct {
    const char *method;
    const char *path;
    const char *body;
    const char *public_key;
    const char *secret_key;
} request_args_t;

static int make_request(const request_args_t *args, buffer_t *response) {
    CURL *curl = curl_easy_init();
    if (!curl) {
        SET_ERROR("Failed to initialize CURL");
        return -1;
    }

    char url[2048];
    snprintf(url, sizeof(url), "%s%s", API_BASE, args->path);

    time_t now = time(NULL);
    char timestamp_str[32];
    snprintf(timestamp_str, sizeof(timestamp_str), "%ld", now);

    char message[4096];
    snprintf(message, sizeof(message), "%s:%s:%s:%s",
             timestamp_str,
             args->method,
             args->path,
             args->body ? args->body : "");

    char *signature = hmac_sha256(args->secret_key, message);
    if (!signature) {
        curl_easy_cleanup(curl);
        SET_ERROR("Failed to generate signature");
        return -1;
    }

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, "Accept: application/json");

    char auth_header[512];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", args->public_key);
    headers = curl_slist_append(headers, auth_header);

    char timestamp_header[64];
    snprintf(timestamp_header, sizeof(timestamp_header), "X-Timestamp: %s", timestamp_str);
    headers = curl_slist_append(headers, timestamp_header);

    char signature_header[256];
    snprintf(signature_header, sizeof(signature_header), "X-Signature: %s", signature);
    headers = curl_slist_append(headers, signature_header);

    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, http_write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);

    if (strcmp(args->method, "POST") == 0) {
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "POST");
        if (args->body) {
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, args->body);
        }
    } else if (strcmp(args->method, "DELETE") == 0) {
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    } else if (strcmp(args->method, "GET") == 0) {
        curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");
    }

    CURLcode res = curl_easy_perform(curl);

    long response_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);

    curl_slist_free_all(headers);
    curl_easy_cleanup(curl);
    free(signature);

    if (res != CURLE_OK) {
        SET_ERROR("CURL error: %s", curl_easy_strerror(res));
        return -1;
    }

    if (response_code >= 400) {
        SET_ERROR("HTTP %ld: %.*s", response_code, (int)response->size, response->data);
        return -1;
    }

    return 0;
}

/* ============================================================================
 * JSON Parsing (Simple)
 * ============================================================================ */

static char *json_get_string(const char *json, const char *key) {
    char search[512];
    snprintf(search, sizeof(search), "\"%s\":\"", key);

    const char *start = strstr(json, search);
    if (!start) return NULL;

    start += strlen(search);

    const char *end = strchr(start, '"');
    if (!end) return NULL;

    size_t len = end - start;
    char *result = malloc(len + 1);
    memcpy(result, start, len);
    result[len] = '\0';

    for (char *p = result; *p; p++) {
        if (*p == '\\' && *(p + 1) == '"') {
            memmove(p, p + 1, strlen(p));
        }
    }

    return result;
}

static long json_get_long(const char *json, const char *key) {
    char search[512];
    snprintf(search, sizeof(search), "\"%s\":", key);

    const char *start = strstr(json, search);
    if (!start) return 0;

    start += strlen(search);

    while (*start && isspace(*start)) start++;

    return strtol(start, NULL, 10);
}

static int json_get_bool(const char *json, const char *key) {
    char search[512];
    snprintf(search, sizeof(search), "\"%s\":", key);

    const char *start = strstr(json, search);
    if (!start) return 0;

    start += strlen(search);

    while (*start && isspace(*start)) start++;

    return strncmp(start, "true", 4) == 0;
}

/* ============================================================================
 * Execute Functions
 * ============================================================================ */

unsandbox_result_t *unsandbox_execute(
    const char *language,
    const char *code,
    const char *public_key,
    const char *secret_key
) {
    if (!language || !code) {
        SET_ERROR("Language and code are required");
        return NULL;
    }

    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    char body[65536];
    snprintf(body, sizeof(body), "{\"language\":\"%s\",\"code\":%.*s}",
             language,
             (int)(strlen(code) < 65000 ? strlen(code) : 65000), code);

    buffer_t response;
    buffer_init(&response);

    request_args_t req = {
        .method = "POST",
        .path = "/execute",
        .body = body,
        .public_key = pk,
        .secret_key = sk
    };

    int result_code = make_request(&req, &response);
    free(pk);
    free(sk);

    if (result_code != 0) {
        buffer_free(&response);
        return NULL;
    }

    unsandbox_result_t *result = calloc(1, sizeof(unsandbox_result_t));
    result->stdout = json_get_string(response.data, "stdout");
    result->stderr = json_get_string(response.data, "stderr");
    result->exit_code = (int)json_get_long(response.data, "exit_code");
    result->language = json_get_string(response.data, "language");
    result->success = 1;

    buffer_free(&response);
    return result;
}

char *unsandbox_execute_async(
    const char *language,
    const char *code,
    const char *public_key,
    const char *secret_key
) {
    if (!language || !code) {
        SET_ERROR("Language and code are required");
        return NULL;
    }

    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    char body[65536];
    snprintf(body, sizeof(body), "{\"language\":\"%s\",\"code\":%.*s}",
             language,
             (int)(strlen(code) < 65000 ? strlen(code) : 65000), code);

    buffer_t response;
    buffer_init(&response);

    request_args_t req = {
        .method = "POST",
        .path = "/execute_async",
        .body = body,
        .public_key = pk,
        .secret_key = sk
    };

    int result_code = make_request(&req, &response);
    free(pk);
    free(sk);

    if (result_code != 0) {
        buffer_free(&response);
        return NULL;
    }

    char *job_id = json_get_string(response.data, "job_id");
    buffer_free(&response);
    return job_id;
}

unsandbox_result_t *unsandbox_wait_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
) {
    if (!job_id) {
        SET_ERROR("Job ID is required");
        return NULL;
    }

    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    int poll_delays_ms[] = {300, 450, 700, 900, 650, 1600, 2000};
    int poll_count = 0;

    while (poll_count < MAX_POLL_ATTEMPTS) {
        buffer_t response;
        buffer_init(&response);

        char path[256];
        snprintf(path, sizeof(path), "/jobs/%s", job_id);

        request_args_t req = {
            .method = "GET",
            .path = path,
            .body = "",
            .public_key = pk,
            .secret_key = sk
        };

        if (make_request(&req, &response) != 0) {
            buffer_free(&response);
            free(pk);
            free(sk);
            return NULL;
        }

        const char *status = json_get_string(response.data, "status");
        if (status && strcmp(status, "completed") == 0) {
            unsandbox_result_t *result = calloc(1, sizeof(unsandbox_result_t));
            result->stdout = json_get_string(response.data, "stdout");
            result->stderr = json_get_string(response.data, "stderr");
            result->exit_code = (int)json_get_long(response.data, "exit_code");
            result->language = json_get_string(response.data, "language");
            result->success = 1;
            buffer_free(&response);
            free(pk);
            free(sk);
            free((char *)status);
            return result;
        }

        buffer_free(&response);
        free((char *)status);

        if (poll_count < sizeof(poll_delays_ms) / sizeof(poll_delays_ms[0])) {
            usleep(poll_delays_ms[poll_count] * 1000);
        } else {
            usleep(2000 * 1000);
        }

        poll_count++;
    }

    free(pk);
    free(sk);
    SET_ERROR("Job polling timeout");
    return NULL;
}

unsandbox_job_t *unsandbox_get_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
) {
    if (!job_id) {
        SET_ERROR("Job ID is required");
        return NULL;
    }

    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    buffer_t response;
    buffer_init(&response);

    char path[256];
    snprintf(path, sizeof(path), "/jobs/%s", job_id);

    request_args_t req = {
        .method = "GET",
        .path = path,
        .body = "",
        .public_key = pk,
        .secret_key = sk
    };

    if (make_request(&req, &response) != 0) {
        buffer_free(&response);
        free(pk);
        free(sk);
        return NULL;
    }

    unsandbox_job_t *job = calloc(1, sizeof(unsandbox_job_t));
    job->id = json_get_string(response.data, "id");
    job->status = json_get_string(response.data, "status");
    job->language = json_get_string(response.data, "language");
    job->created_at = json_get_long(response.data, "created_at");
    job->completed_at = json_get_long(response.data, "completed_at");
    job->error_message = json_get_string(response.data, "error_message");

    buffer_free(&response);
    free(pk);
    free(sk);
    return job;
}

int unsandbox_cancel_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
) {
    if (!job_id) {
        SET_ERROR("Job ID is required");
        return -1;
    }

    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return -1;
    }

    buffer_t response;
    buffer_init(&response);

    char path[256];
    snprintf(path, sizeof(path), "/jobs/%s", job_id);

    request_args_t req = {
        .method = "DELETE",
        .path = path,
        .body = "",
        .public_key = pk,
        .secret_key = sk
    };

    int result = make_request(&req, &response);
    buffer_free(&response);
    free(pk);
    free(sk);
    return result;
}

unsandbox_job_list_t *unsandbox_list_jobs(
    const char *public_key,
    const char *secret_key
) {
    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    buffer_t response;
    buffer_init(&response);

    request_args_t req = {
        .method = "GET",
        .path = "/jobs",
        .body = "",
        .public_key = pk,
        .secret_key = sk
    };

    if (make_request(&req, &response) != 0) {
        buffer_free(&response);
        free(pk);
        free(sk);
        return NULL;
    }

    unsandbox_job_list_t *list = calloc(1, sizeof(unsandbox_job_list_t));
    list->jobs = calloc(100, sizeof(unsandbox_job_t));
    list->count = 0;

    buffer_free(&response);
    free(pk);
    free(sk);
    return list;
}

unsandbox_languages_t *unsandbox_get_languages(
    const char *public_key,
    const char *secret_key
) {
    char *pk = NULL, *sk = NULL;
    if (unsandbox_resolve_credentials(&pk, &sk, public_key, secret_key) != 0) {
        return NULL;
    }

    buffer_t response;
    buffer_init(&response);

    request_args_t req = {
        .method = "GET",
        .path = "/languages",
        .body = "",
        .public_key = pk,
        .secret_key = sk
    };

    if (make_request(&req, &response) != 0) {
        buffer_free(&response);
        free(pk);
        free(sk);
        return NULL;
    }

    unsandbox_languages_t *langs = calloc(1, sizeof(unsandbox_languages_t));
    langs->languages = calloc(100, sizeof(char *));
    langs->count = 0;

    buffer_free(&response);
    free(pk);
    free(sk);
    return langs;
}

/* ============================================================================
 * Memory Management
 * ============================================================================ */

void unsandbox_free_result(unsandbox_result_t *result) {
    if (!result) return;
    if (result->stdout) free(result->stdout);
    if (result->stderr) free(result->stderr);
    if (result->language) free(result->language);
    if (result->error_message) free(result->error_message);
    free(result);
}

void unsandbox_free_job(unsandbox_job_t *job) {
    if (!job) return;
    if (job->id) free(job->id);
    if (job->status) free(job->status);
    if (job->language) free(job->language);
    if (job->error_message) free(job->error_message);
    free(job);
}

void unsandbox_free_job_list(unsandbox_job_list_t *jobs) {
    if (!jobs) return;
    for (size_t i = 0; i < jobs->count; i++) {
        unsandbox_free_job(&jobs->jobs[i]);
    }
    if (jobs->jobs) free(jobs->jobs);
    free(jobs);
}

void unsandbox_free_languages(unsandbox_languages_t *langs) {
    if (!langs) return;
    for (size_t i = 0; i < langs->count; i++) {
        if (langs->languages[i]) free(langs->languages[i]);
    }
    if (langs->languages) free(langs->languages);
    free(langs);
}

void unsandbox_free_quota(unsandbox_quota_t *quota) {
    if (quota) free(quota);
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

const char *unsandbox_last_error(void) {
    return g_last_error[0] ? g_last_error : NULL;
}

int unsandbox_health_check(void) {
    CURL *curl = curl_easy_init();
    if (!curl) return -1;

    buffer_t response;
    buffer_init(&response);

    curl_easy_setopt(curl, CURLOPT_URL, API_BASE "/health");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, http_write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10L);

    CURLcode res = curl_easy_perform(curl);
    long response_code = 0;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);

    curl_easy_cleanup(curl);
    buffer_free(&response);

    if (res != CURLE_OK) return -1;
    if (response_code != 200) return 0;
    return 1;
}

const char *unsandbox_version(void) {
    return "1.0.0";
}
