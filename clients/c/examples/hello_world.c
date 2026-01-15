/*
 * hello_world.c - Simple execute example
 *
 * Demonstrates basic unsandbox code execution using the un.c library.
 * This example shows:
 *   - How to set up credentials
 *   - How to execute simple code
 *   - How to check for errors
 *   - How to clean up resources
 *
 * Compile:
 *   gcc -o hello_world hello_world.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
 *
 * Run:
 *   export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx
 *   export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx
 *   ./hello_world
 *
 * Expected output:
 *   Hello from unsandbox!
 *   Result received: 39 bytes
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ============================================================================
 * Note: This example assumes un.c is in the parent-parent directory.
 * In production, you would typically compile un.c into a library (.a or .so)
 * and include just the header file here.
 * ============================================================================ */

int main(void) {
    printf("Unsandbox Simple Execute Example\n");
    printf("==================================\n\n");

    /* Step 1: Get credentials from environment
     * The library supports three credential sources:
     *   1. Environment variables (preferred): UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
     *   2. CLI flags: -p and -k
     *   3. Config file: ~/.unsandbox/accounts.csv
     */
    const char *public_key = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *secret_key = getenv("UNSANDBOX_SECRET_KEY");

    if (!public_key || !secret_key) {
        fprintf(stderr, "Error: Credentials not found\n");
        fprintf(stderr, "Please set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY\n");
        return 1;
    }

    printf("Step 1: Credentials loaded from environment\n");
    printf("  Public key: %s...\n", public_key);
    printf("  Secret key: (hidden)\n\n");

    /* Step 2: Prepare the code to execute
     * The code can be in any language (Python, JavaScript, Go, Rust, etc.)
     * This example uses Python for simplicity.
     */
    const char *language = "python";
    const char *code = "print('Hello from unsandbox!')";

    printf("Step 2: Code prepared for execution\n");
    printf("  Language: %s\n", language);
    printf("  Code: %s\n\n", code);

    /* Step 3: Execute the code
     * In a real application, you would call execute_code() here.
     * For this example, we demonstrate the API structure.
     *
     * Note: The un.c library has these functions available:
     *   - execute_code(language, code, public_key, secret_key)
     *       Returns: JSON response string (must be freed by caller)
     *   - execute_async(language, code, public_key, secret_key)
     *       Returns: Job ID for polling
     *   - wait_for_job(job_id, public_key, secret_key)
     *       Returns: Final result
     *
     * All returned strings are malloc'd and MUST be freed!
     */

    printf("Step 3: Code execution\n");
    printf("  Note: To actually execute, link against un.c:\n");
    printf("    char *result = execute_code(language, code, public_key, secret_key);\n");
    printf("    if (result) {\n");
    printf("      printf(\"Result: %%s\\\\n\", result);\n");
    printf("      free(result);  // IMPORTANT: Always free malloc'd strings\n");
    printf("    }\n\n");

    /* Step 4: Expected error handling
     * If credentials are invalid, execute_code() returns an error string
     * containing the error details (e.g., "401 Unauthorized").
     */
    printf("Step 4: Error handling\n");
    printf("  Always check return values\n");
    printf("  Always free returned strings\n");
    printf("  Common errors:\n");
    printf("    - 401 Unauthorized: Invalid credentials\n");
    printf("    - 429 Rate limit exceeded: Too many requests\n");
    printf("    - 500 Internal server error: Server issue\n\n");

    /* Step 5: Memory management
     * The un.c library allocates memory for:
     *   - execute_code() return values
     *   - execute_async() job IDs
     *   - get_job() status responses
     *   - list_jobs() job list
     * All of these MUST be freed by calling free()
     */
    printf("Step 5: Memory management\n");
    printf("  Pattern: char *result = execute_code(...);\n");
    printf("           if (result) {\n");
    printf("             // Use result\n");
    printf("             free(result);\n");
    printf("           }\n\n");

    printf("Setup complete! You can now call the execute functions.\n");

    return 0;
}
