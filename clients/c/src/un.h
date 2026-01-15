/*
 * PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
 *
 * unsandbox.com C SDK (Synchronous)
 *
 * Full API with execution, sessions, services, and snapshots.
 *
 * Library Usage:
 *     #include "un.h"
 *
 *     // Execute code synchronously
 *     unsandbox_result_t *result = unsandbox_execute("python", "print(42)", NULL, NULL);
 *     if (result && result->success) {
 *         printf("Output: %s\n", result->stdout_str);
 *         unsandbox_free_result(result);
 *     }
 *
 *     // Sessions
 *     unsandbox_session_t *session = unsandbox_session_create(NULL, NULL, NULL);
 *     unsandbox_session_list_t *sessions = unsandbox_session_list(NULL, NULL);
 *     unsandbox_session_destroy(session->id, NULL, NULL);
 *
 *     // Services
 *     char *svc_id = unsandbox_service_create("myapp", "8080", NULL, NULL, NULL, NULL);
 *     unsandbox_service_list_t *svcs = unsandbox_service_list(NULL, NULL);
 *     unsandbox_service_destroy(svc_id, NULL, NULL);
 *
 *     // Snapshots
 *     char *snap_id = unsandbox_snapshot_session(session_id, "backup", 0, NULL, NULL);
 *     unsandbox_snapshot_restore(snap_id, NULL, NULL);
 *
 * Authentication Priority (4-tier):
 *     1. Function arguments (public_key, secret_key)
 *     2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
 *     3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
 *     4. Local directory (./accounts.csv, line 0 by default)
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

/* Execution result */
typedef struct {
    char *stdout_str;
    char *stderr_str;
    int exit_code;
    char *language;
    double execution_time;
    int success;
    char *error_message;
} unsandbox_result_t;

/* Job info */
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

/* Languages */
typedef struct {
    char **languages;
    size_t count;
} unsandbox_languages_t;

/* Session info */
typedef struct {
    char *id;
    char *container_name;
    char *status;           /* running, frozen, terminated */
    char *network_mode;
    int vcpu;
    int64_t created_at;
    int64_t last_activity;
} unsandbox_session_t;

typedef struct {
    unsandbox_session_t *sessions;
    size_t count;
} unsandbox_session_list_t;

/* Service info */
typedef struct {
    char *id;
    char *name;
    char *status;           /* running, frozen, sleeping */
    char *container_name;
    char *network_mode;
    char *ports;
    char *domains;
    int vcpu;
    int locked;
    int64_t created_at;
    int64_t last_activity;
} unsandbox_service_t;

typedef struct {
    unsandbox_service_t *services;
    size_t count;
} unsandbox_service_list_t;

/* Snapshot info */
typedef struct {
    char *id;
    char *name;
    char *type;             /* session, service */
    char *source_id;
    int hot;
    int locked;
    int64_t created_at;
    int64_t size_bytes;
} unsandbox_snapshot_t;

typedef struct {
    unsandbox_snapshot_t *snapshots;
    size_t count;
} unsandbox_snapshot_list_t;

/* API key validation result */
typedef struct {
    int valid;
    char *tier;
    int rate_limit_per_minute;
    int rate_limit_burst;
    int concurrency_limit;
    char *error_message;
} unsandbox_key_info_t;

/* ============================================================================
 * Execution Functions (8)
 * ============================================================================ */

unsandbox_result_t *unsandbox_execute(
    const char *language, const char *code,
    const char *public_key, const char *secret_key);

char *unsandbox_execute_async(
    const char *language, const char *code,
    const char *public_key, const char *secret_key);

unsandbox_result_t *unsandbox_wait_job(
    const char *job_id,
    const char *public_key, const char *secret_key);

unsandbox_job_t *unsandbox_get_job(
    const char *job_id,
    const char *public_key, const char *secret_key);

int unsandbox_cancel_job(
    const char *job_id,
    const char *public_key, const char *secret_key);

unsandbox_job_list_t *unsandbox_list_jobs(
    const char *public_key, const char *secret_key);

unsandbox_languages_t *unsandbox_get_languages(
    const char *public_key, const char *secret_key);

const char *unsandbox_detect_language(const char *filename);

/* ============================================================================
 * Session Functions (9)
 * ============================================================================ */

unsandbox_session_list_t *unsandbox_session_list(
    const char *public_key, const char *secret_key);

unsandbox_session_t *unsandbox_session_get(
    const char *session_id,
    const char *public_key, const char *secret_key);

unsandbox_session_t *unsandbox_session_create(
    const char *network_mode,   /* NULL for default (zerotrust) */
    const char *shell,          /* NULL for default (bash) */
    const char *public_key, const char *secret_key);

int unsandbox_session_destroy(
    const char *session_id,
    const char *public_key, const char *secret_key);

int unsandbox_session_freeze(
    const char *session_id,
    const char *public_key, const char *secret_key);

int unsandbox_session_unfreeze(
    const char *session_id,
    const char *public_key, const char *secret_key);

int unsandbox_session_boost(
    const char *session_id, int vcpu,
    const char *public_key, const char *secret_key);

int unsandbox_session_unboost(
    const char *session_id,
    const char *public_key, const char *secret_key);

unsandbox_result_t *unsandbox_session_execute(
    const char *session_id, const char *command,
    const char *public_key, const char *secret_key);

/* ============================================================================
 * Service Functions (17)
 * ============================================================================ */

unsandbox_service_list_t *unsandbox_service_list(
    const char *public_key, const char *secret_key);

unsandbox_service_t *unsandbox_service_get(
    const char *service_id,
    const char *public_key, const char *secret_key);

char *unsandbox_service_create(
    const char *name,
    const char *ports,          /* e.g., "8080", "80,443" */
    const char *domains,        /* e.g., "app.example.com" */
    const char *bootstrap,      /* bootstrap script content */
    const char *network_mode,   /* NULL for default */
    const char *public_key, const char *secret_key);

int unsandbox_service_destroy(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_freeze(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_unfreeze(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_lock(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_unlock(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_redeploy(
    const char *service_id,
    const char *bootstrap,      /* new bootstrap script, NULL to keep existing */
    const char *public_key, const char *secret_key);

char *unsandbox_service_logs(
    const char *service_id, int all_logs,
    const char *public_key, const char *secret_key);

unsandbox_result_t *unsandbox_service_execute(
    const char *service_id, const char *command, int timeout_ms,
    const char *public_key, const char *secret_key);

/* Service environment management */
char *unsandbox_service_env_get(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_env_set(
    const char *service_id, const char *env_content,
    const char *public_key, const char *secret_key);

int unsandbox_service_env_delete(
    const char *service_id,
    const char *public_key, const char *secret_key);

char *unsandbox_service_env_export(
    const char *service_id,
    const char *public_key, const char *secret_key);

int unsandbox_service_resize(
    const char *service_id, int vcpu,
    const char *public_key, const char *secret_key);

/* ============================================================================
 * Snapshot Functions (8)
 * ============================================================================ */

unsandbox_snapshot_list_t *unsandbox_snapshot_list(
    const char *public_key, const char *secret_key);

unsandbox_snapshot_t *unsandbox_snapshot_get(
    const char *snapshot_id,
    const char *public_key, const char *secret_key);

char *unsandbox_snapshot_session(
    const char *session_id, const char *name, int hot,
    const char *public_key, const char *secret_key);

char *unsandbox_snapshot_service(
    const char *service_id, const char *name, int hot,
    const char *public_key, const char *secret_key);

int unsandbox_snapshot_restore(
    const char *snapshot_id,
    const char *public_key, const char *secret_key);

int unsandbox_snapshot_delete(
    const char *snapshot_id,
    const char *public_key, const char *secret_key);

int unsandbox_snapshot_lock(
    const char *snapshot_id,
    const char *public_key, const char *secret_key);

int unsandbox_snapshot_unlock(
    const char *snapshot_id,
    const char *public_key, const char *secret_key);

/* ============================================================================
 * Key Validation (1)
 * ============================================================================ */

unsandbox_key_info_t *unsandbox_validate_keys(
    const char *public_key, const char *secret_key);

/* ============================================================================
 * Memory Management
 * ============================================================================ */

void unsandbox_free_result(unsandbox_result_t *result);
void unsandbox_free_job(unsandbox_job_t *job);
void unsandbox_free_job_list(unsandbox_job_list_t *jobs);
void unsandbox_free_languages(unsandbox_languages_t *langs);
void unsandbox_free_session(unsandbox_session_t *session);
void unsandbox_free_session_list(unsandbox_session_list_t *sessions);
void unsandbox_free_service(unsandbox_service_t *service);
void unsandbox_free_service_list(unsandbox_service_list_t *services);
void unsandbox_free_snapshot(unsandbox_snapshot_t *snapshot);
void unsandbox_free_snapshot_list(unsandbox_snapshot_list_t *snapshots);
void unsandbox_free_key_info(unsandbox_key_info_t *info);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

char *unsandbox_hmac_sign(const char *secret_key, const char *message);
const char *unsandbox_last_error(void);
int unsandbox_health_check(void);
const char *unsandbox_version(void);

#ifdef __cplusplus
}
#endif

#endif /* UNSANDBOX_H */
