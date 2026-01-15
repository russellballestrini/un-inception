/*
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com C SDK (Synchronous)
 *
 * Library Usage:
 *     #include "un.h"
 *
 *     // Execute code synchronously
 *     unsandbox_result_t *result = unsandbox_execute("python", "print(42)", NULL, NULL);
 *     if (result && result->success) {
 *         printf("Output: %s\n", result->stdout);
 *         unsandbox_free_result(result);
 *     }
 *
 *     // Execute asynchronously
 *     char *job_id = unsandbox_execute_async("javascript", "console.log(42)", NULL, NULL);
 *     if (job_id) {
 *         printf("Job ID: %s\n", job_id);
 *         free(job_id);
 *     }
 *
 *     // Wait for job completion
 *     result = unsandbox_wait_job(job_id, NULL, NULL);
 *
 *     // List jobs
 *     unsandbox_job_list_t *jobs = unsandbox_list_jobs(NULL, NULL);
 *
 *     // Get languages
 *     unsandbox_languages_t *langs = unsandbox_get_languages(NULL, NULL);
 *
 *     // Detect language from filename
 *     const char *lang = unsandbox_detect_language("script.py");
 *
 * Authentication Priority (4-tier):
 *     1. Function arguments (public_key, secret_key)
 *     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
 *     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
 *     4. Local directory (./accounts.csv, line 0 by default)
 *
 *     Format: public_key,secret_key (one per line)
 *     Account selection: UNSANDBOX_ACCOUNT=N env var (0-based index)
 *
 * Request Authentication (HMAC-SHA256):
 *     Authorization: Bearer <public_key>                  (identifies account)
 *     X-Timestamp: <unix_seconds>                         (replay prevention)
 *     X-Signature: HMAC-SHA256(secret_key, msg)           (proves secret + body integrity)
 *
 *     Message format: "timestamp:METHOD:path:body"
 *     - timestamp: seconds since epoch
 *     - METHOD: GET, POST, DELETE, etc. (uppercase)
 *     - path: e.g., "/execute", "/jobs/123"
 *     - body: JSON payload (empty string for GET/DELETE)
 *
 * Languages Cache:
 *     - Cached in ~/.unsandbox/languages.json
 *     - TTL: 1 hour
 *     - Updated on successful API calls
 */

#ifndef UNSANDBOX_H
#define UNSANDBOX_H

#include <stdint.h>
#include <stddef.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Data Structures
 * ============================================================================ */

typedef struct {
    char *stdout_str;      /* Use stdout_str to avoid conflict with stdio macro */
    char *stderr_str;      /* Use stderr_str to avoid conflict with stdio macro */
    int exit_code;
    char *language;
    double execution_time;
    int success;
    char *error_message;
} unsandbox_result_t;

typedef struct {
    char *id;
    char *language;
    char *status;
    int64_t created_at;
    int64_t completed_at;
    char *error_message;
} unsandbox_job_t;

typedef struct {
    unsandbox_job_t *jobs;
    size_t count;
} unsandbox_job_list_t;

typedef struct {
    char **languages;
    size_t count;
} unsandbox_languages_t;

typedef struct {
    int rate_limit_per_minute;
    int rate_limit_burst;
    int64_t reset_at;
    int concurrency_limit;
    int active_executions;
} unsandbox_quota_t;

/* ============================================================================
 * Core Execution Functions
 * ============================================================================ */

/**
 * Execute code synchronously
 *
 * @param language Language identifier (e.g., "python", "javascript", "go")
 * @param code Code to execute
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Pointer to result or NULL on error. Must be freed with unsandbox_free_result()
 */
unsandbox_result_t *unsandbox_execute(
    const char *language,
    const char *code,
    const char *public_key,
    const char *secret_key
);

/**
 * Execute code asynchronously (returns immediately with job ID)
 *
 * @param language Language identifier (e.g., "python", "javascript", "go")
 * @param code Code to execute
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Job ID string (must be freed with free()) or NULL on error
 */
char *unsandbox_execute_async(
    const char *language,
    const char *code,
    const char *public_key,
    const char *secret_key
);

/**
 * Wait for async job completion with exponential backoff polling
 *
 * @param job_id Job ID returned from unsandbox_execute_async()
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Pointer to result or NULL on error. Must be freed with unsandbox_free_result()
 */
unsandbox_result_t *unsandbox_wait_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
);

/**
 * Get job status without waiting for completion
 *
 * @param job_id Job ID
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Pointer to job or NULL if not found. Must be freed with unsandbox_free_job()
 */
unsandbox_job_t *unsandbox_get_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
);

/**
 * Cancel a running job
 *
 * @param job_id Job ID
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return 0 on success, -1 on error
 */
int unsandbox_cancel_job(
    const char *job_id,
    const char *public_key,
    const char *secret_key
);

/**
 * List all active jobs
 *
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Pointer to job list or NULL on error. Must be freed with unsandbox_free_job_list()
 */
unsandbox_job_list_t *unsandbox_list_jobs(
    const char *public_key,
    const char *secret_key
);

/* ============================================================================
 * Language Functions
 * ============================================================================ */

/**
 * Get list of supported languages
 * Cached in ~/.unsandbox/languages.json with 1 hour TTL
 *
 * @param public_key API public key (optional, uses env/config if NULL)
 * @param secret_key API secret key (optional, uses env/config if NULL)
 * @return Pointer to language list or NULL on error. Must be freed with unsandbox_free_languages()
 */
unsandbox_languages_t *unsandbox_get_languages(
    const char *public_key,
    const char *secret_key
);

/**
 * Detect language from file extension
 *
 * @param filename Filename (e.g., "script.py", "main.go")
 * @return Language identifier (e.g., "python", "go") or NULL if unknown
 * Note: Returned string should not be freed - it's a static constant
 */
const char *unsandbox_detect_language(const char *filename);

/* ============================================================================
 * Memory Management
 * ============================================================================ */

/**
 * Free execution result
 */
void unsandbox_free_result(unsandbox_result_t *result);

/**
 * Free job
 */
void unsandbox_free_job(unsandbox_job_t *job);

/**
 * Free job list
 */
void unsandbox_free_job_list(unsandbox_job_list_t *jobs);

/**
 * Free language list
 */
void unsandbox_free_languages(unsandbox_languages_t *langs);

/**
 * Free quota info
 */
void unsandbox_free_quota(unsandbox_quota_t *quota);

/* ============================================================================
 * Credential Management
 * ============================================================================ */

/**
 * Resolve credentials from 4-tier priority system
 *
 * Priority:
 *     1. Function arguments
 *     2. Environment variables
 *     3. ~/.unsandbox/accounts.csv
 *     4. ./accounts.csv
 *
 * @param public_key_out Output parameter for public key (must be freed with free())
 * @param secret_key_out Output parameter for secret key (must be freed with free())
 * @return 0 on success, -1 if no credentials found
 */
int unsandbox_resolve_credentials(
    char **public_key_out,
    char **secret_key_out,
    const char *public_key_hint,
    const char *secret_key_hint
);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/**
 * Generate HMAC-SHA256 signature for API authentication
 *
 * @param secret_key The secret key
 * @param message The message to sign (format: "timestamp:METHOD:path:body")
 * @return Hex-encoded signature string (64 chars). Must be freed with free().
 *         Returns NULL if inputs are invalid.
 */
char *unsandbox_hmac_sign(const char *secret_key, const char *message);

/**
 * Get last error message from failed operation
 *
 * @return Error string or NULL if no error. Do not free.
 */
const char *unsandbox_last_error(void);

/**
 * Check if API is available
 *
 * @return 1 if available, 0 if not, -1 on error
 */
int unsandbox_health_check(void);

/**
 * Get library version
 *
 * @return Version string (e.g., "1.0.0")
 */
const char *unsandbox_version(void);

#ifdef __cplusplus
}
#endif

#endif /* UNSANDBOX_H */
