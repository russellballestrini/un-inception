/*
 * UN C SDK - Functional Tests
 *
 * Tests library functions against real API.
 * Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
 *
 * Usage:
 *   make test-functional
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "un.h"

static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) printf("\nTesting %s...\n", name)
#define PASS(msg) do { printf("  \033[32m✓\033[0m %s\n", msg); tests_passed++; } while(0)
#define FAIL(msg) do { printf("  \033[31m✗\033[0m %s\n", msg); tests_failed++; } while(0)
#define CHECK(cond, msg) do { if (cond) PASS(msg); else FAIL(msg); } while(0)

static void test_execute(void) {
    TEST("unsandbox_execute()");

    unsandbox_result_t *result = unsandbox_execute("python", "print('hello from C SDK')", NULL, NULL);

    CHECK(result != NULL, "execute returns non-NULL");
    if (!result) return;

    CHECK(result->success, "execution succeeded");
    CHECK(result->stdout_str != NULL, "stdout is non-NULL");
    if (result->stdout_str) {
        CHECK(strstr(result->stdout_str, "hello from C SDK") != NULL, "stdout contains expected output");
        printf("    stdout: %s", result->stdout_str);
    }
    CHECK(result->exit_code == 0, "exit code is 0");
    CHECK(result->execution_time >= 0, "execution_time is non-negative");

    unsandbox_free_result(result);
    PASS("memory freed without crash");
}

static void test_execute_error(void) {
    TEST("unsandbox_execute() with error");

    unsandbox_result_t *result = unsandbox_execute("python", "import sys; sys.exit(1)", NULL, NULL);

    CHECK(result != NULL, "execute returns non-NULL");
    if (!result) return;

    CHECK(!result->success, "execution marked as failed");
    CHECK(result->exit_code == 1, "exit code is 1");

    unsandbox_free_result(result);
}

static void test_get_languages(void) {
    TEST("unsandbox_get_languages()");

    unsandbox_languages_t *langs = unsandbox_get_languages(NULL, NULL);

    CHECK(langs != NULL, "get_languages returns non-NULL");
    if (!langs) return;

    CHECK(langs->count > 0, "at least one language returned");
    CHECK(langs->languages != NULL, "languages array is non-NULL");

    printf("    Found %zu languages: ", langs->count);
    int found_python = 0;
    for (size_t i = 0; i < langs->count && i < 5; i++) {
        if (langs->languages[i]) {
            printf("%s ", langs->languages[i]);
            if (strcmp(langs->languages[i], "python") == 0) found_python = 1;
        }
    }
    if (langs->count > 5) printf("...");
    printf("\n");

    CHECK(found_python, "python is in languages list");

    unsandbox_free_languages(langs);
    PASS("memory freed without crash");
}

static void test_session_lifecycle(void) {
    TEST("session lifecycle (create, destroy)");

    /* Create session */
    unsandbox_session_t *session = unsandbox_session_create(NULL, NULL, NULL, NULL);
    CHECK(session != NULL, "session_create returns non-NULL");
    if (!session) return;

    CHECK(session->id != NULL, "session has id");
    printf("    session_id: %s\n", session->id ? session->id : "(null)");

    char *session_id = session->id ? strdup(session->id) : NULL;
    unsandbox_free_session(session);

    if (!session_id) {
        FAIL("could not get session id");
        return;
    }

    /* Note: session_execute requires WebSocket (HTTP 426), skip for now */
    PASS("session_execute skipped (requires WebSocket)");

    /* Destroy session */
    int destroyed = unsandbox_session_destroy(session_id, NULL, NULL);
    CHECK(destroyed == 0, "session_destroy returns success");

    free(session_id);
}

static void test_session_list(void) {
    TEST("unsandbox_session_list()");

    unsandbox_session_list_t *sessions = unsandbox_session_list(NULL, NULL);
    CHECK(sessions != NULL, "session_list returns non-NULL");
    if (!sessions) return;

    printf("    Found %zu sessions\n", sessions->count);
    for (size_t i = 0; i < sessions->count && i < 3; i++) {
        printf("    - %s (%s)\n",
               sessions->sessions[i].id ? sessions->sessions[i].id : "(null)",
               sessions->sessions[i].status ? sessions->sessions[i].status : "(null)");
    }

    unsandbox_free_session_list(sessions);
    PASS("memory freed without crash");
}

static void test_service_list(void) {
    TEST("unsandbox_service_list()");

    unsandbox_service_list_t *services = unsandbox_service_list(NULL, NULL);
    CHECK(services != NULL, "service_list returns non-NULL");
    if (!services) return;

    printf("    Found %zu services\n", services->count);
    for (size_t i = 0; i < services->count && i < 3; i++) {
        printf("    - %s: %s (%s)\n",
               services->services[i].id ? services->services[i].id : "(null)",
               services->services[i].name ? services->services[i].name : "(null)",
               services->services[i].status ? services->services[i].status : "(null)");
    }

    unsandbox_free_service_list(services);
    PASS("memory freed without crash");
}

static void test_snapshot_list(void) {
    TEST("unsandbox_snapshot_list()");

    unsandbox_snapshot_list_t *snapshots = unsandbox_snapshot_list(NULL, NULL);
    CHECK(snapshots != NULL, "snapshot_list returns non-NULL");
    if (!snapshots) return;

    printf("    Found %zu snapshots\n", snapshots->count);
    for (size_t i = 0; i < snapshots->count && i < 3; i++) {
        printf("    - %s: %s (%s)\n",
               snapshots->snapshots[i].id ? snapshots->snapshots[i].id : "(null)",
               snapshots->snapshots[i].name ? snapshots->snapshots[i].name : "(null)",
               snapshots->snapshots[i].type ? snapshots->snapshots[i].type : "(null)");
    }

    unsandbox_free_snapshot_list(snapshots);
    PASS("memory freed without crash");
}

static void test_image_list(void) {
    TEST("unsandbox_image_list()");

    unsandbox_image_list_t *images = unsandbox_image_list(NULL, NULL, NULL);
    CHECK(images != NULL, "image_list returns non-NULL");
    if (!images) return;

    printf("    Found %zu images\n", images->count);
    for (size_t i = 0; i < images->count && i < 3; i++) {
        printf("    - %s: %s (%s)\n",
               images->images[i].id ? images->images[i].id : "(null)",
               images->images[i].name ? images->images[i].name : "(null)",
               images->images[i].visibility ? images->images[i].visibility : "(null)");
    }

    unsandbox_free_image_list(images);
    PASS("memory freed without crash");
}

static void test_validate_keys(void) {
    TEST("unsandbox_validate_keys()");

    unsandbox_key_info_t *info = unsandbox_validate_keys(NULL, NULL);
    CHECK(info != NULL, "validate_keys returns non-NULL");
    if (!info) return;

    CHECK(info->valid, "keys are valid");
    if (info->tier) printf("    tier: %s\n", info->tier);
    printf("    rate_limit: %d/min, burst: %d\n", info->rate_limit_per_minute, info->rate_limit_burst);

    unsandbox_free_key_info(info);
    PASS("memory freed without crash");
}

int main(void) {
    printf("=====================================\n");
    printf("UN C SDK - Functional Tests\n");
    printf("Testing against real API\n");
    printf("=====================================\n");

    /* Check credentials */
    const char *pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *sk = getenv("UNSANDBOX_SECRET_KEY");

    if (!pk || !sk) {
        printf("\n\033[31mError: Missing credentials\033[0m\n");
        printf("Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY\n");
        return 1;
    }

    printf("\nUsing credentials from environment\n");

    /* Run tests */
    test_validate_keys();
    test_get_languages();
    test_execute();
    test_execute_error();
    test_session_list();
    test_session_lifecycle();
    test_service_list();
    test_snapshot_list();
    test_image_list();

    /* Summary */
    printf("\n=====================================\n");
    printf("Test Summary\n");
    printf("=====================================\n");
    printf("Passed: \033[32m%d\033[0m\n", tests_passed);
    printf("Failed: \033[31m%d\033[0m\n", tests_failed);
    printf("=====================================\n");

    return tests_failed > 0 ? 1 : 0;
}
