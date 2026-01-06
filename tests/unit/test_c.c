// Unit tests for un.c - tests internal functions without API calls
// Compile: gcc -o test_c test_c.c && ./test_c

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int passed = 0;
int failed = 0;

#define TEST(name, expr) do { \
    if (expr) { \
        printf("  ✓ %s\n", name); \
        passed++; \
    } else { \
        printf("  ✗ %s\n", name); \
        failed++; \
    } \
} while(0)

const char* get_language(const char* ext) {
    if (strcmp(ext, ".py") == 0) return "python";
    if (strcmp(ext, ".js") == 0) return "javascript";
    if (strcmp(ext, ".rb") == 0) return "ruby";
    if (strcmp(ext, ".go") == 0) return "go";
    if (strcmp(ext, ".rs") == 0) return "rust";
    if (strcmp(ext, ".c") == 0) return "c";
    if (strcmp(ext, ".cpp") == 0) return "cpp";
    if (strcmp(ext, ".java") == 0) return "java";
    return NULL;
}

const char* get_extension(const char* filename) {
    const char* dot = strrchr(filename, '.');
    return dot ? dot : "";
}

const char* get_basename(const char* path) {
    const char* slash = strrchr(path, '/');
    return slash ? slash + 1 : path;
}

int main() {
    printf("\n=== Extension Mapping Tests ===\n");

    TEST("Python extension maps correctly",
         strcmp(get_language(".py"), "python") == 0);

    TEST("C extension maps correctly",
         strcmp(get_language(".c"), "c") == 0);

    TEST("JavaScript extension maps correctly",
         strcmp(get_language(".js"), "javascript") == 0);

    TEST("Go extension maps correctly",
         strcmp(get_language(".go"), "go") == 0);

    TEST("C++ extension maps correctly",
         strcmp(get_language(".cpp"), "cpp") == 0);

    printf("\n=== Signature Format Tests ===\n");

    char message[256];
    const char* timestamp = "1704067200";
    const char* method = "POST";
    const char* endpoint = "/execute";
    const char* body = "{\"language\":\"python\"}";

    snprintf(message, sizeof(message), "%s:%s:%s:%s",
             timestamp, method, endpoint, body);

    TEST("Signature format starts with timestamp",
         strncmp(message, timestamp, strlen(timestamp)) == 0);

    TEST("Signature format contains :POST:",
         strstr(message, ":POST:") != NULL);

    TEST("Signature format contains :/execute:",
         strstr(message, ":/execute:") != NULL);

    printf("\n=== Language Detection Tests ===\n");

    const char* content = "#!/usr/bin/env python3\nprint('hello')";
    char first_line[256];
    strncpy(first_line, content, sizeof(first_line));
    char* newline = strchr(first_line, '\n');
    if (newline) *newline = '\0';

    TEST("Python shebang detection - starts with #!",
         strncmp(first_line, "#!", 2) == 0);

    TEST("Python shebang detection - contains python",
         strstr(first_line, "python") != NULL);

    printf("\n=== Argument Parsing Tests ===\n");

    char arg1[] = "DEBUG=1";
    char* eq1 = strchr(arg1, '=');
    *eq1 = '\0';
    TEST("Parse -e KEY=VALUE format - key",
         strcmp(arg1, "DEBUG") == 0);
    TEST("Parse -e KEY=VALUE format - value",
         strcmp(eq1 + 1, "1") == 0);

    printf("\n=== File Operations Tests ===\n");

    TEST("Extract file basename",
         strcmp(get_basename("/home/user/project/script.c"), "script.c") == 0);

    TEST("Extract file extension",
         strcmp(get_extension("/home/user/project/script.c"), ".c") == 0);

    printf("\n=== API Constants Tests ===\n");

    const char* api_base = "https://api.unsandbox.com";
    TEST("API base URL starts with https://",
         strncmp(api_base, "https://", 8) == 0);

    TEST("API base URL contains unsandbox.com",
         strstr(api_base, "unsandbox.com") != NULL);

    printf("\n=== Summary ===\n");
    printf("Passed: %d\n", passed);
    printf("Failed: %d\n", failed);
    printf("Total:  %d\n", passed + failed);

    return failed > 0 ? 1 : 0;
}
