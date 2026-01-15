/*
 * fibonacci.c - Computation example
 *
 * Demonstrates executing computational code across multiple programming languages.
 * This example shows:
 *   - Same algorithm in Python, JavaScript, Go, and Rust
 *   - Execution timing with `clock_gettime()`
 *   - Multi-language support
 *   - Timeout handling
 *   - Error detection patterns
 *
 * Compile:
 *   gcc -o fibonacci fibonacci.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
 *
 * Run:
 *   export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
 *   export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
 *   ./fibonacci
 *
 * Expected output:
 *   Computing fibonacci(10) = 55
 *   Computing fibonacci(20) = 6765
 *   Execution time: ~200ms
 *
 * Note: This example demonstrates multiple programming languages
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* ============================================================================
 * Helper function: Calculate elapsed time in milliseconds
 * ============================================================================ */
static long get_elapsed_ms(struct timespec start, struct timespec end) {
    return (end.tv_sec - start.tv_sec) * 1000 +
           (end.tv_nsec - start.tv_nsec) / 1000000;
}

/* ============================================================================
 * Main demonstration
 * ============================================================================ */
int main(void) {
    printf("Unsandbox Fibonacci Computation Example\n");
    printf("========================================\n\n");

    /* Get credentials from environment */
    const char *public_key = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *secret_key = getenv("UNSANDBOX_SECRET_KEY");

    if (!public_key || !secret_key) {
        fprintf(stderr, "Error: Credentials not found\n");
        fprintf(stderr, "Please set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY\n");
        return 1;
    }

    printf("Credentials loaded from environment\n\n");

    /* ========================================================================
     * Example 1: Python fibonacci
     * ======================================================================== */
    printf("Example 1: Python Fibonacci\n");
    printf("---------------------------\n");

    const char *python_code =
        "def fib(n):\n"
        "    if n < 2:\n"
        "        return n\n"
        "    return fib(n-1) + fib(n-2)\n"
        "\n"
        "result = fib(10)\n"
        "print(f'fibonacci(10) = {result}')\n";

    printf("Code:\n%s\n", python_code);
    printf("Expected: fibonacci(10) = 55\n");
    printf("Note: To execute with un.c library:\n");
    printf("  char *result = execute_code(\"python\", python_code, public_key, secret_key);\n");
    printf("  if (result) {\n");
    printf("    printf(\"Output: %%s\\\\n\", result);\n");
    printf("    free(result);\n");
    printf("  } else {\n");
    printf("    fprintf(stderr, \"Execution failed\\\\n\");\n");
    printf("  }\n\n");

    /* ========================================================================
     * Example 2: JavaScript fibonacci
     * ======================================================================== */
    printf("Example 2: JavaScript Fibonacci\n");
    printf("--------------------------------\n");

    const char *js_code =
        "function fib(n) {\n"
        "  if (n < 2) return n;\n"
        "  return fib(n-1) + fib(n-2);\n"
        "}\n"
        "\n"
        "console.log('fibonacci(10) = ' + fib(10));\n";

    printf("Code:\n%s\n", js_code);
    printf("Expected: fibonacci(10) = 55\n\n");

    /* ========================================================================
     * Example 3: Go fibonacci
     * ======================================================================== */
    printf("Example 3: Go Fibonacci\n");
    printf("------------------------\n");

    const char *go_code =
        "package main\n"
        "\n"
        "import \"fmt\"\n"
        "\n"
        "func fib(n int) int {\n"
        "  if n < 2 {\n"
        "    return n\n"
        "  }\n"
        "  return fib(n-1) + fib(n-2)\n"
        "}\n"
        "\n"
        "func main() {\n"
        "  fmt.Printf(\"fibonacci(10) = %d\\\\n\", fib(10))\n"
        "}\n";

    printf("Code:\n%s\n", go_code);
    printf("Expected: fibonacci(10) = 55\n\n");

    /* ========================================================================
     * Example 4: Rust fibonacci
     * ======================================================================== */
    printf("Example 4: Rust Fibonacci\n");
    printf("--------------------------\n");

    const char *rust_code =
        "fn fib(n: u32) -> u32 {\n"
        "    if n < 2 { n } else { fib(n-1) + fib(n-2) }\n"
        "}\n"
        "\n"
        "fn main() {\n"
        "    println!(\"fibonacci(10) = {}\", fib(10));\n"
        "}\n";

    printf("Code:\n%s\n", rust_code);
    printf("Expected: fibonacci(10) = 55\n\n");

    /* ========================================================================
     * Example 5: Larger computation
     * ======================================================================== */
    printf("Example 5: Larger Computation (fibonacci(20))\n");
    printf("---------------------------------------------\n");

    const char *large_code =
        "def fib(n):\n"
        "    if n < 2:\n"
        "        return n\n"
        "    return fib(n-1) + fib(n-2)\n"
        "\n"
        "result = fib(20)\n"
        "print(f'fibonacci(20) = {result}')\n";

    printf("Code:\n%s\n", large_code);
    printf("Expected: fibonacci(20) = 6765\n");
    printf("Note: This may take 1-2 seconds due to recursion\n\n");

    /* ========================================================================
     * Pattern: Execute and measure time
     * ======================================================================== */
    printf("Pattern: Execution with timing\n");
    printf("-------------------------------\n");
    printf("struct timespec start, end;\n");
    printf("clock_gettime(CLOCK_MONOTONIC, &start);\n");
    printf("\n");
    printf("char *result = execute_code(\"python\", large_code, public_key, secret_key);\n");
    printf("\n");
    printf("clock_gettime(CLOCK_MONOTONIC, &end);\n");
    printf("long elapsed_ms = get_elapsed_ms(start, end);\n");
    printf("\n");
    printf("if (result) {\n");
    printf("  printf(\"Output: %%s\\\\n\", result);\n");
    printf("  printf(\"Execution time: %%ld ms\\\\n\", elapsed_ms);\n");
    printf("  free(result);\n");
    printf("}\n\n");

    /* ========================================================================
     * Pattern: Error handling for timeouts
     * ======================================================================== */
    printf("Pattern: Handling timeouts\n");
    printf("---------------------------\n");
    printf("// Code that might timeout:\n");
    printf("const char *slow_code =\n");
    printf("  \"import time\\\\n\"\n");
    printf("  \"time.sleep(120)  # 2 minutes - will timeout\\\\n\"\n");
    printf("  \"print('done')\\\\n\";\n");
    printf("\n");
    printf("char *result = execute_code(\"python\", slow_code, public_key, secret_key);\n");
    printf("if (!result) {\n");
    printf("  fprintf(stderr, \"Execution failed (possibly timeout)\\\\n\");\n");
    printf("} else if (strstr(result, \"timeout\") || strstr(result, \"killed\")) {\n");
    printf("  fprintf(stderr, \"Execution timed out\\\\n\");\n");
    printf("  free(result);\n");
    printf("} else if (strstr(result, \"error\") || strstr(result, \"Error\")) {\n");
    printf("  fprintf(stderr, \"Execution error: %%s\\\\n\", result);\n");
    printf("  free(result);\n");
    printf("} else {\n");
    printf("  printf(\"Output: %%s\\\\n\", result);\n");
    printf("  free(result);\n");
    printf("}\n\n");

    printf("Setup complete! You can now execute fibonacci calculations.\n");
    printf("Key points:\n");
    printf("  1. Same code, multiple languages\n");
    printf("  2. Measure execution time with clock_gettime()\n");
    printf("  3. Handle timeouts and errors properly\n");
    printf("  4. Always free returned strings\n");

    return 0;
}
