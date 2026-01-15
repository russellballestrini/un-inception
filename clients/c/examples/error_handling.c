/*
 * error_handling.c - Proper error handling patterns
 *
 * Demonstrates comprehensive error handling patterns for unsandbox code execution.
 * This example shows:
 *   - HTTP error responses (401, 429, 500)
 *   - Runtime errors in executed code
 *   - Timeout detection
 *   - Memory leak prevention
 *   - Retry strategies
 *   - Logging errors
 *
 * Compile:
 *   gcc -o error_handling error_handling.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
 *
 * Run:
 *   export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
 *   export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
 *   ./error_handling
 *
 * Expected output:
 *   Demonstrates various error scenarios and proper handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

/* ============================================================================
 * Error type definitions
 * ============================================================================ */

typedef enum {
    ERROR_NONE = 0,
    ERROR_INVALID_CREDS = 1,
    ERROR_RATE_LIMITED = 2,
    ERROR_TIMEOUT = 3,
    ERROR_RUNTIME_ERROR = 4,
    ERROR_SERVER_ERROR = 5,
    ERROR_NETWORK = 6,
    ERROR_OUT_OF_MEMORY = 7,
    ERROR_UNKNOWN = 8
} ErrorType;

typedef struct {
    ErrorType type;
    int http_code;
    char *message;
    char *details;
} ExecutionError;

/* ============================================================================
 * Helper functions
 * ============================================================================ */

/* Create a new error structure */
static ExecutionError* error_create(ErrorType type, int http_code,
                                    const char *message, const char *details) {
    ExecutionError *err = malloc(sizeof(ExecutionError));
    if (!err) return NULL;

    err->type = type;
    err->http_code = http_code;
    err->message = message ? strdup(message) : NULL;
    err->details = details ? strdup(details) : NULL;
    return err;
}

/* Free error structure */
static void error_free(ExecutionError *err) {
    if (!err) return;
    free(err->message);
    free(err->details);
    free(err);
}

/* Parse HTTP response to determine error type */
static ErrorType parse_error_type(const char *response, int http_code) {
    if (!response) {
        if (http_code >= 500) return ERROR_SERVER_ERROR;
        if (http_code == 429) return ERROR_RATE_LIMITED;
        if (http_code == 401) return ERROR_INVALID_CREDS;
        return ERROR_UNKNOWN;
    }

    /* Check for specific error patterns in response */
    if (strstr(response, "401") || strstr(response, "Unauthorized")) {
        return ERROR_INVALID_CREDS;
    }
    if (strstr(response, "429") || strstr(response, "rate_limit")) {
        return ERROR_RATE_LIMITED;
    }
    if (strstr(response, "timeout") || strstr(response, "Timeout")) {
        return ERROR_TIMEOUT;
    }
    if (strstr(response, "Traceback") || strstr(response, "Error") ||
        strstr(response, "Exception") || strstr(response, "error")) {
        return ERROR_RUNTIME_ERROR;
    }
    if (http_code >= 500) {
        return ERROR_SERVER_ERROR;
    }

    return ERROR_UNKNOWN;
}

/* Print human-readable error message */
static void error_print(const ExecutionError *err) {
    if (!err) {
        printf("Error: NULL error object\n");
        return;
    }

    printf("Error [%d]: ", err->http_code);

    switch (err->type) {
        case ERROR_INVALID_CREDS:
            printf("Invalid Credentials\n");
            printf("  Your API key or secret key is incorrect.\n");
            printf("  Check UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY.\n");
            break;

        case ERROR_RATE_LIMITED:
            printf("Rate Limited\n");
            printf("  You've exceeded your rate limit.\n");
            printf("  Wait before retrying. Check response for reset time.\n");
            break;

        case ERROR_TIMEOUT:
            printf("Timeout\n");
            printf("  Code execution took too long and was terminated.\n");
            printf("  Optimize your code or increase timeout if available.\n");
            break;

        case ERROR_RUNTIME_ERROR:
            printf("Runtime Error\n");
            printf("  Your code raised an exception or error.\n");
            printf("  Check the code logic.\n");
            break;

        case ERROR_SERVER_ERROR:
            printf("Server Error\n");
            printf("  The unsandbox server encountered an error.\n");
            printf("  This is typically temporary. Retry later.\n");
            break;

        case ERROR_NETWORK:
            printf("Network Error\n");
            printf("  Could not reach the unsandbox server.\n");
            printf("  Check your internet connection.\n");
            break;

        case ERROR_OUT_OF_MEMORY:
            printf("Out of Memory\n");
            printf("  malloc() failed. System is out of memory.\n");
            break;

        case ERROR_NONE:
            printf("No Error\n");
            break;

        case ERROR_UNKNOWN:
        default:
            printf("Unknown Error\n");
            break;
    }

    if (err->message) {
        printf("  Message: %s\n", err->message);
    }
    if (err->details) {
        printf("  Details: %s\n", err->details);
    }
}

/* ============================================================================
 * Error handling patterns
 * ============================================================================ */

/*
 * Pattern 1: Check for NULL return (allocation failure)
 */
static void pattern_null_check(void) {
    printf("\nPattern 1: Check for NULL (memory allocation failure)\n");
    printf("========================================================\n");
    printf("char *result = execute_code(language, code, pk, sk);\n");
    printf("if (!result) {\n");
    printf("  ExecutionError *err = error_create(\n");
    printf("    ERROR_OUT_OF_MEMORY, 0,\n");
    printf("    \"Failed to allocate result buffer\",\n");
    printf("    \"malloc() returned NULL\"\n");
    printf("  );\n");
    printf("  error_print(err);\n");
    printf("  error_free(err);\n");
    printf("  return 1;\n");
    printf("}\n");
}

/*
 * Pattern 2: Check for HTTP error codes in response
 */
static void pattern_http_error_check(void) {
    printf("\nPattern 2: Check for HTTP errors in response\n");
    printf("=============================================\n");
    printf("char *result = execute_code(language, code, pk, sk);\n");
    printf("if (!result) {\n");
    printf("  fprintf(stderr, \"Execution failed\\\\n\");\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("ErrorType err_type = parse_error_type(result, 0);\n");
    printf("if (err_type != ERROR_NONE) {\n");
    printf("  ExecutionError *err = error_create(\n");
    printf("    err_type, 0, \"Execution failed\", result\n");
    printf("  );\n");
    printf("  error_print(err);\n");
    printf("  error_free(err);\n");
    printf("  free(result);\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("// Success: process result\n");
    printf("printf(\"Output: %%s\\\\n\", result);\n");
    printf("free(result);\n");
}

/*
 * Pattern 3: Retry on transient errors
 */
static void pattern_retry_logic(void) {
    printf("\nPattern 3: Retry logic for transient errors\n");
    printf("==========================================\n");
    printf("const int MAX_RETRIES = 3;\n");
    printf("const int RETRY_DELAY_MS = 1000;\n");
    printf("\n");
    printf("for (int attempt = 0; attempt < MAX_RETRIES; attempt++) {\n");
    printf("  char *result = execute_code(language, code, pk, sk);\n");
    printf("  if (!result) break;  // Success\n");
    printf("\n");
    printf("  ErrorType err_type = parse_error_type(result, 0);\n");
    printf("\n");
    printf("  if (err_type == ERROR_SERVER_ERROR ||\n");
    printf("      err_type == ERROR_NETWORK) {\n");
    printf("    // Transient error: retry\n");
    printf("    printf(\"Retry %%d/%%d after %%dms...\\\\n\", \n");
    printf("           attempt + 1, MAX_RETRIES, RETRY_DELAY_MS);\n");
    printf("    free(result);\n");
    printf("    usleep(RETRY_DELAY_MS * 1000);\n");
    printf("    continue;\n");
    printf("  }\n");
    printf("\n");
    printf("  if (err_type == ERROR_RATE_LIMITED) {\n");
    printf("    // Rate limit: exponential backoff\n");
    printf("    int backoff_ms = RETRY_DELAY_MS * (1 << attempt);\n");
    printf("    printf(\"Rate limited, backoff %%dms...\\\\n\", backoff_ms);\n");
    printf("    free(result);\n");
    printf("    usleep(backoff_ms * 1000);\n");
    printf("    continue;\n");
    printf("  }\n");
    printf("\n");
    printf("  if (err_type == ERROR_INVALID_CREDS ||\n");
    printf("      err_type == ERROR_RUNTIME_ERROR) {\n");
    printf("    // Permanent error: don't retry\n");
    printf("    ExecutionError *err = error_create(\n");
    printf("      err_type, 0, \"Execution failed\", result\n");
    printf("    );\n");
    printf("    error_print(err);\n");
    printf("    error_free(err);\n");
    printf("    free(result);\n");
    printf("    return 1;\n");
    printf("  }\n");
    printf("\n");
    printf("  // Success\n");
    printf("  printf(\"Output: %%s\\\\n\", result);\n");
    printf("  free(result);\n");
    printf("  return 0;\n");
    printf("}\n");
    printf("\n");
    printf("fprintf(stderr, \"Failed after %%d attempts\\\\n\", MAX_RETRIES);\n");
    printf("return 1;\n");
}

/*
 * Pattern 4: Timeout detection
 */
static void pattern_timeout_detection(void) {
    printf("\nPattern 4: Detect and handle timeouts\n");
    printf("=====================================\n");
    printf("char *result = execute_code(language, code, pk, sk);\n");
    printf("if (!result) {\n");
    printf("  fprintf(stderr, \"Execution failed\\\\n\");\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("// Check for timeout indicators\n");
    printf("if (strstr(result, \"timeout\") ||\n");
    printf("    strstr(result, \"Timeout\") ||\n");
    printf("    strstr(result, \"timed out\") ||\n");
    printf("    strstr(result, \"killed\")) {\n");
    printf("  ExecutionError *err = error_create(\n");
    printf("    ERROR_TIMEOUT, 0,\n");
    printf("    \"Code execution timed out\",\n");
    printf("    result\n");
    printf("  );\n");
    printf("  error_print(err);\n");
    printf("  error_free(err);\n");
    printf("  free(result);\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("printf(\"Output: %%s\\\\n\", result);\n");
    printf("free(result);\n");
}

/*
 * Pattern 5: Logging errors to file
 */
static void pattern_error_logging(void) {
    printf("\nPattern 5: Log errors to file\n");
    printf("=============================\n");
    printf("FILE *log_file = fopen(\"execution.log\", \"a\");\n");
    printf("if (!log_file) {\n");
    printf("  perror(\"Failed to open log file\");\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("char *result = execute_code(language, code, pk, sk);\n");
    printf("if (!result) {\n");
    printf("  fprintf(log_file,\n");
    printf("    \"[ERROR] Execution failed at %%s\\\\n\",\n");
    printf("    __func__);\n");
    printf("  fclose(log_file);\n");
    printf("  return 1;\n");
    printf("}\n");
    printf("\n");
    printf("ErrorType err_type = parse_error_type(result, 0);\n");
    printf("if (err_type != ERROR_NONE) {\n");
    printf("  fprintf(log_file,\n");
    printf("    \"[ERROR] Execution error: %%s\\\\n\",\n");
    printf("    result);\n");
    printf("}\n");
    printf("\n");
    printf("free(result);\n");
    printf("fclose(log_file);\n");
}

/* ============================================================================
 * Main demonstration
 * ============================================================================ */

int main(void) {
    printf("Unsandbox Error Handling Example\n");
    printf("=================================\n");

    /* Get credentials */
    const char *public_key = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *secret_key = getenv("UNSANDBOX_SECRET_KEY");

    if (!public_key || !secret_key) {
        fprintf(stderr, "Error: Credentials not found\n");
        fprintf(stderr, "Please set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY\n");
        return 1;
    }

    printf("Credentials loaded\n\n");

    /* Demonstrate error types */
    printf("Error Types:\n");
    printf("============\n");

    ExecutionError *err;

    err = error_create(ERROR_INVALID_CREDS, 401, "Invalid credentials", NULL);
    printf("\n1. ");
    error_print(err);
    error_free(err);

    err = error_create(ERROR_RATE_LIMITED, 429, "Rate limited", "Reset in 60 seconds");
    printf("\n2. ");
    error_print(err);
    error_free(err);

    err = error_create(ERROR_TIMEOUT, 0, "Execution timeout", "Killed after 300 seconds");
    printf("\n3. ");
    error_print(err);
    error_free(err);

    err = error_create(ERROR_RUNTIME_ERROR, 0, "Runtime error",
                      "Traceback: ZeroDivisionError: division by zero");
    printf("\n4. ");
    error_print(err);
    error_free(err);

    err = error_create(ERROR_SERVER_ERROR, 500, "Server error", NULL);
    printf("\n5. ");
    error_print(err);
    error_free(err);

    /* Show patterns */
    pattern_null_check();
    pattern_http_error_check();
    pattern_retry_logic();
    pattern_timeout_detection();
    pattern_error_logging();

    printf("\n\nKey Points:\n");
    printf("===========\n");
    printf("1. Always check return values for NULL\n");
    printf("2. Parse error responses to determine error type\n");
    printf("3. Distinguish transient vs permanent errors\n");
    printf("4. Implement retry logic with exponential backoff\n");
    printf("5. Detect timeouts by checking response content\n");
    printf("6. Log errors for debugging\n");
    printf("7. Always free allocated memory\n");

    return 0;
}
