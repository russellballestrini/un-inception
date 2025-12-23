// Test suite for UN CLI C implementation
// Compile: gcc -o test_un_c test_un_c.c -lcurl
// Run: ./test_un_c
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include <sys/stat.h>
#include <unistd.h>

struct MemoryStruct {
    char *memory;
    size_t size;
};

static size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    struct MemoryStruct *mem = (struct MemoryStruct *)userp;

    char *ptr = realloc(mem->memory, mem->size + realsize + 1);
    if (!ptr) {
        return 0;
    }

    mem->memory = ptr;
    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;

    return realsize;
}

// Copy of detect_language from un_inception.c for testing
const char* detect_language(const char *filename) {
    const char *ext = strrchr(filename, '.');
    if (!ext) return NULL;

    if (strcmp(ext, ".py") == 0) return "python";
    if (strcmp(ext, ".js") == 0) return "javascript";
    if (strcmp(ext, ".go") == 0) return "go";
    if (strcmp(ext, ".rs") == 0) return "rust";
    if (strcmp(ext, ".c") == 0) return "c";
    if (strcmp(ext, ".cpp") == 0) return "cpp";
    if (strcmp(ext, ".d") == 0) return "d";
    if (strcmp(ext, ".zig") == 0) return "zig";
    if (strcmp(ext, ".nim") == 0) return "nim";
    if (strcmp(ext, ".v") == 0) return "v";

    return NULL;
}

int test_extension_detection() {
    printf("=== Test 1: Extension Detection ===\n");

    struct {
        const char *filename;
        const char *expected;
    } tests[] = {
        {"script.py", "python"},
        {"app.js", "javascript"},
        {"main.go", "go"},
        {"program.rs", "rust"},
        {"code.c", "c"},
        {"app.cpp", "cpp"},
        {"prog.d", "d"},
        {"main.zig", "zig"},
        {"script.nim", "nim"},
        {"app.v", "v"},
        {"unknown.xyz", NULL},
    };

    int passed = 0;
    int failed = 0;
    int num_tests = sizeof(tests) / sizeof(tests[0]);

    for (int i = 0; i < num_tests; i++) {
        const char *result = detect_language(tests[i].filename);

        int test_passed = 0;
        if (tests[i].expected == NULL && result == NULL) {
            test_passed = 1;
        } else if (tests[i].expected != NULL && result != NULL && strcmp(result, tests[i].expected) == 0) {
            test_passed = 1;
        }

        if (test_passed) {
            printf("  PASS: %s -> %s\n", tests[i].filename, result ? result : "NULL");
            passed++;
        } else {
            printf("  FAIL: %s -> got %s, expected %s\n",
                   tests[i].filename,
                   result ? result : "NULL",
                   tests[i].expected ? tests[i].expected : "NULL");
            failed++;
        }
    }

    printf("Extension Detection: %d passed, %d failed\n\n", passed, failed);
    return failed == 0;
}

int test_api_connection() {
    printf("=== Test 2: API Connection ===\n");

    const char *api_key = getenv("UNSANDBOX_API_KEY");
    if (!api_key) {
        printf("  SKIP: UNSANDBOX_API_KEY not set\n");
        printf("API Connection: skipped\n\n");
        return 1;
    }

    CURL *curl = curl_easy_init();
    if (!curl) {
        printf("  FAIL: Failed to initialize curl\n");
        return 0;
    }

    const char *json_body = "{\"language\":\"python\",\"code\":\"print('Hello from API test')\"}";

    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", api_key);

    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, auth_header);

    struct MemoryStruct chunk = {.memory = malloc(1), .size = 0};

    curl_easy_setopt(curl, CURLOPT_URL, "https://api.unsandbox.com/execute");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

    CURLcode res = curl_easy_perform(curl);

    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);

    if (res != CURLE_OK) {
        printf("  FAIL: HTTP request error: %s\n", curl_easy_strerror(res));
        free(chunk.memory);
        return 0;
    }

    if (!strstr(chunk.memory, "Hello from API test")) {
        printf("  FAIL: Unexpected response: %s\n", chunk.memory);
        free(chunk.memory);
        return 0;
    }

    free(chunk.memory);
    printf("  PASS: API connection successful\n");
    printf("API Connection: passed\n\n");
    return 1;
}

int test_fib_execution() {
    printf("=== Test 3: Functional Test (fib.go) ===\n");

    const char *api_key = getenv("UNSANDBOX_API_KEY");
    if (!api_key) {
        printf("  SKIP: UNSANDBOX_API_KEY not set\n");
        printf("Functional Test: skipped\n\n");
        return 1;
    }

    struct stat st;
    if (stat("../un_c", &st) != 0) {
        printf("  SKIP: ../un_c binary not found (run: cd .. && gcc -o un_c un_inception.c -lcurl)\n");
        printf("Functional Test: skipped\n\n");
        return 1;
    }

    if (stat("fib.go", &st) != 0) {
        printf("  SKIP: fib.go not found\n");
        printf("Functional Test: skipped\n\n");
        return 1;
    }

    FILE *fp = popen("../un_c fib.go 2>&1", "r");
    if (!fp) {
        printf("  FAIL: Failed to execute command\n");
        return 0;
    }

    char output[4096] = {0};
    size_t total = 0;
    size_t n;
    while ((n = fread(output + total, 1, sizeof(output) - total - 1, fp)) > 0) {
        total += n;
    }

    int status = pclose(fp);

    if (status != 0) {
        printf("  FAIL: Command failed with exit code: %d\n", WEXITSTATUS(status));
        printf("  Output: %s\n", output);
        return 0;
    }

    if (!strstr(output, "fib(10) = 55")) {
        printf("  FAIL: Expected output to contain 'fib(10) = 55', got: %s\n", output);
        return 0;
    }

    printf("  PASS: fib.go executed successfully\n");
    printf("  Output: %s", output);
    printf("Functional Test: passed\n\n");
    return 1;
}

int main() {
    printf("UN CLI C Implementation Test Suite\n");
    printf("===================================\n\n");

    int all_passed = 1;

    if (!test_extension_detection()) {
        all_passed = 0;
    }

    if (!test_api_connection()) {
        all_passed = 0;
    }

    if (!test_fib_execution()) {
        all_passed = 0;
    }

    printf("===================================\n");
    if (all_passed) {
        printf("RESULT: ALL TESTS PASSED\n");
        return 0;
    } else {
        printf("RESULT: SOME TESTS FAILED\n");
        return 1;
    }
}
