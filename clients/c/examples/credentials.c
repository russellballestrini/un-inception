/*
 * credentials.c - Credential loading from 4 sources
 *
 * Demonstrates how the un.c library loads credentials from multiple sources
 * with priority ordering. This example shows:
 *   - Priority ordering (highest to lowest)
 *   - Environment variables
 *   - CLI flags
 *   - Config file (~/.unsandbox/accounts.csv)
 *   - Account selection
 *
 * Compile:
 *   gcc -o credentials credentials.c -I../../.. -lcurl -lwebsockets -lssl -lcrypto
 *
 * Run:
 *   ./credentials
 *
 * Or with specific credentials:
 *   ./credentials --public unsb-pk-xxxxx --secret unsb-sk-xxxxx
 *
 * Expected output:
 *   Demonstrates credential loading from all 4 sources
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <pwd.h>

/* ============================================================================
 * Helper functions
 * ============================================================================ */

/* Print a value, masking sensitive info after first 8 characters */
static void print_masked(const char *label, const char *value) {
    if (!value || strlen(value) == 0) {
        printf("%s: (not set)\n", label);
        return;
    }

    size_t len = strlen(value);
    printf("%s: ", label);

    if (len <= 8) {
        printf("****\n");
    } else {
        printf("%.8s...****\n", value);
    }
}

/* Check if a file exists */
static int file_exists(const char *path) {
    struct stat sb;
    return (stat(path, &sb) == 0 && S_ISREG(sb.st_mode));
}

/* Get home directory */
static const char* get_home_dir(void) {
    const char *home = getenv("HOME");
    if (home) return home;

    struct passwd *pw = getpwuid(getuid());
    if (pw) return pw->pw_dir;

    return ".";
}

/* ============================================================================
 * Credential priority demonstration
 * ============================================================================ */

void explain_priority(void) {
    printf("Credential Priority (Highest to Lowest)\n");
    printf("=======================================\n\n");

    printf("Priority 1: CLI Flags (HIGHEST)\n");
    printf("  un -p PUBLIC_KEY -k SECRET_KEY\n");
    printf("  These always take precedence if provided.\n\n");

    printf("Priority 2: Environment Variables\n");
    printf("  UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY\n");
    printf("  Used if CLI flags not provided.\n\n");

    printf("Priority 3: Config File\n");
    printf("  ~/.unsandbox/accounts.csv\n");
    printf("  Format: public_key,secret_key (one per line)\n");
    printf("  Account selection: --account N (0-based, default: 0)\n");
    printf("  Or: UNSANDBOX_ACCOUNT environment variable\n\n");

    printf("Priority 4: Default (LOWEST)\n");
    printf("  If none of the above are set, execution fails.\n");
    printf("  \"Error: API credentials required\"\n\n");
}

/* ============================================================================
 * Source 1: CLI Flags
 * ============================================================================ */

void explain_cli_flags(void) {
    printf("\nSource 1: CLI Flags\n");
    printf("===================\n");

    printf("Command line format:\n");
    printf("  un -p unsb-pk-xxxxx -k unsb-sk-xxxxx\n");
    printf("  un --public unsb-pk-xxxxx --secret unsb-sk-xxxxx\n\n");

    printf("Example usage in code:\n");
    printf("  int main(int argc, char *argv[]) {\n");
    printf("    const char *public_key = NULL;\n");
    printf("    const char *secret_key = NULL;\n");
    printf("\n");
    printf("    // Parse command line\n");
    printf("    for (int i = 1; i < argc; i++) {\n");
    printf("      if ((strcmp(argv[i], \"-p\") == 0 ||\n");
    printf("           strcmp(argv[i], \"--public\") == 0) &&\n");
    printf("          i + 1 < argc) {\n");
    printf("        public_key = argv[++i];\n");
    printf("      }\n");
    printf("      if ((strcmp(argv[i], \"-k\") == 0 ||\n");
    printf("           strcmp(argv[i], \"--secret\") == 0) &&\n");
    printf("          i + 1 < argc) {\n");
    printf("        secret_key = argv[++i];\n");
    printf("      }\n");
    printf("    }\n");
    printf("\n");
    printf("    // Check if both provided\n");
    printf("    if (!public_key || !secret_key) {\n");
    printf("      // Fall back to next priority\n");
    printf("    }\n");
    printf("  }\n\n");

    printf("Advantages:\n");
    printf("  - Highest priority (can't be accidentally overridden)\n");
    printf("  - Unique credentials per invocation\n");
    printf("  - Good for automation scripts\n\n");

    printf("Disadvantages:\n");
    printf("  - Credentials visible in process list (if not careful)\n");
    printf("  - Must pass on every invocation\n");
}

/* ============================================================================
 * Source 2: Environment Variables
 * ============================================================================ */

void explain_env_vars(void) {
    printf("\nSource 2: Environment Variables\n");
    printf("================================\n");

    const char *pk = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *sk = getenv("UNSANDBOX_SECRET_KEY");
    const char *account = getenv("UNSANDBOX_ACCOUNT");

    printf("Environment variables:\n");
    printf("  UNSANDBOX_PUBLIC_KEY  ");
    print_masked("", pk);

    printf("  UNSANDBOX_SECRET_KEY  ");
    print_masked("", sk);

    printf("  UNSANDBOX_ACCOUNT     %s (default: 0)\n\n", account ? account : "(not set)");

    printf("How to set:\n");
    printf("  export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxxxx\n");
    printf("  export UNSANDBOX_SECRET_KEY=unsb-sk-xxxxx\n");
    printf("  export UNSANDBOX_ACCOUNT=0  # optional\n\n");

    printf("Or in one line:\n");
    printf("  UNSANDBOX_PUBLIC_KEY=xxx UNSANDBOX_SECRET_KEY=yyy ./un script.py\n\n");

    printf("How to read in C:\n");
    printf("  const char *pk = getenv(\"UNSANDBOX_PUBLIC_KEY\");\n");
    printf("  const char *sk = getenv(\"UNSANDBOX_SECRET_KEY\");\n");
    printf("  const char *account_str = getenv(\"UNSANDBOX_ACCOUNT\");\n");
    printf("  int account_idx = account_str ? atoi(account_str) : 0;\n\n");

    printf("Advantages:\n");
    printf("  - Don't appear in process list\n");
    printf("  - Can persist in shell session\n");
    printf("  - Standard Unix convention\n\n");

    printf("Disadvantages:\n");
    printf("  - Can be inherited by child processes\n");
    printf("  - Visible to debuggers\n");
}

/* ============================================================================
 * Source 3: Config File
 * ============================================================================ */

void explain_config_file(void) {
    printf("\nSource 3: Config File (~/.unsandbox/accounts.csv)\n");
    printf("==================================================\n");

    const char *home = get_home_dir();
    char config_path[512];
    snprintf(config_path, sizeof(config_path), "%s/.unsandbox/accounts.csv", home);

    printf("Config file location:\n");
    printf("  %s\n\n", config_path);

    printf("File format (one account per line):\n");
    printf("  unsb-pk-account1,unsb-sk-account1\n");
    printf("  unsb-pk-account2,unsb-sk-account2\n");
    printf("  unsb-pk-account3,unsb-sk-account3\n\n");

    printf("How to create:\n");
    printf("  mkdir -p ~/.unsandbox\n");
    printf("  cat > ~/.unsandbox/accounts.csv << 'EOF'\n");
    printf("  unsb-pk-xxxxx,unsb-sk-xxxxx\n");
    printf("  unsb-pk-yyyyy,unsb-sk-yyyyy\n");
    printf("  EOF\n");
    printf("  chmod 600 ~/.unsandbox/accounts.csv\n\n");

    printf("How to select account:\n");
    printf("  un --account 0 script.py    # First account (default)\n");
    printf("  un --account 1 script.py    # Second account\n");
    printf("  UNSANDBOX_ACCOUNT=2 un script.py  # Environment variable\n\n");

    printf("Current config file status:\n");
    if (file_exists(config_path)) {
        printf("  [EXISTS] %s\n", config_path);
        printf("  Note: Contents not shown for security\n");
    } else {
        printf("  [MISSING] %s\n", config_path);
        printf("  To create: mkdir -p ~/.unsandbox\n");
        printf("            echo 'pk,sk' > %s\n", config_path);
        printf("            chmod 600 %s\n", config_path);
    }

    printf("\nHow to read in C:\n");
    printf("  FILE *f = fopen(config_path, \"r\");\n");
    printf("  if (f) {\n");
    printf("    char line[256];\n");
    printf("    int account_num = 0;\n");
    printf("    while (fgets(line, sizeof(line), f)) {\n");
    printf("      if (account_num == account_idx) {\n");
    printf("        char *pk = strtok(line, \",\");\n");
    printf("        char *sk = strtok(NULL, \"\\\\n\");\n");
    printf("        // Use pk and sk\n");
    printf("        break;\n");
    printf("      }\n");
    printf("      account_num++;\n");
    printf("    }\n");
    printf("    fclose(f);\n");
    printf("  }\n\n");

    printf("Advantages:\n");
    printf("  - Store multiple accounts\n");
    printf("  - Secrets not in shell history\n");
    printf("  - Can be version-controlled (separately)\n");
    printf("  - Standard configuration file approach\n\n");

    printf("Disadvantages:\n");
    printf("  - File must exist at specific location\n");
    printf("  - Must remember account index\n");
    printf("  - Requires explicit setup\n");
    printf("  - File permissions critical (should be 600)\n");
}

/* ============================================================================
 * Source 4: Fallback/Error
 * ============================================================================ */

void explain_fallback(void) {
    printf("\nSource 4: Fallback (No Credentials)\n");
    printf("====================================\n");

    printf("If no credentials are found from sources 1-3:\n");
    printf("  Error: API credentials required.\n");
    printf("    Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n");
    printf("    Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n");
    printf("    Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n\n");

    printf("Exit code: 1 (failure)\n\n");
}

/* ============================================================================
 * Practical examples
 * ============================================================================ */

void show_practical_examples(void) {
    printf("\nPractical Examples\n");
    printf("==================\n\n");

    printf("Example 1: One-off execution\n");
    printf("  UNSANDBOX_PUBLIC_KEY=unsb-pk-xxx UNSANDBOX_SECRET_KEY=unsb-sk-xxx \\\n");
    printf("  ./un script.py\n\n");

    printf("Example 2: Development session\n");
    printf("  export UNSANDBOX_PUBLIC_KEY=unsb-pk-xxx\n");
    printf("  export UNSANDBOX_SECRET_KEY=unsb-sk-xxx\n");
    printf("  ./un script1.py\n");
    printf("  ./un script2.py\n");
    printf("  ./un script3.py\n\n");

    printf("Example 3: Multiple accounts\n");
    printf("  mkdir -p ~/.unsandbox\n");
    printf("  echo 'unsb-pk-account1,unsb-sk-account1' >> ~/.unsandbox/accounts.csv\n");
    printf("  echo 'unsb-pk-account2,unsb-sk-account2' >> ~/.unsandbox/accounts.csv\n");
    printf("  ./un --account 0 script.py  # Uses first account\n");
    printf("  ./un --account 1 script.py  # Uses second account\n\n");

    printf("Example 4: Override with CLI flags\n");
    printf("  export UNSANDBOX_PUBLIC_KEY=unsb-pk-env\n");
    printf("  export UNSANDBOX_SECRET_KEY=unsb-sk-env\n");
    printf("  ./un -p unsb-pk-cli -k unsb-sk-cli script.py  # Uses CLI keys\n\n");

    printf("Example 5: Automation in scripts\n");
    printf("  #!/bin/bash\n");
    printf("  export UNSANDBOX_PUBLIC_KEY=\"$(aws secretsmanager ...)\"\n");
    printf("  export UNSANDBOX_SECRET_KEY=\"$(aws secretsmanager ...)\"\n");
    printf("  ./un process_data.py\n\n");
}

/* ============================================================================
 * Security best practices
 * ============================================================================ */

void show_security_practices(void) {
    printf("\nSecurity Best Practices\n");
    printf("=======================\n\n");

    printf("DO:\n");
    printf("  ✓ Use environment variables for credentials\n");
    printf("  ✓ Set config file permissions to 600 (chmod 600)\n");
    printf("  ✓ Store secrets in secure vaults (AWS Secrets Manager, etc.)\n");
    printf("  ✓ Rotate API keys regularly\n");
    printf("  ✓ Use separate keys for different environments (dev/prod)\n");
    printf("  ✓ Delete credentials from shell history: history -c\n");
    printf("  ✓ Audit who has access to credentials\n\n");

    printf("DON'T:\n");
    printf("  ✗ Hardcode credentials in source code\n");
    printf("  ✗ Commit credentials to version control\n");
    printf("  ✗ Log credentials to console/files\n");
    printf("  ✗ Pass credentials on command line in production\n");
    printf("  ✗ Store credentials in plain text files (except ~/.unsandbox/accounts.csv)\n");
    printf("  ✗ Share credentials between team members\n");
    printf("  ✗ Use same credentials for dev and production\n\n");
}

/* ============================================================================
 * Main demonstration
 * ============================================================================ */

int main(int argc, char *argv[]) {
    printf("Unsandbox Credential Management\n");
    printf("================================\n\n");

    /* Show priority */
    explain_priority();

    /* Show each source */
    explain_cli_flags();
    explain_env_vars();
    explain_config_file();
    explain_fallback();

    /* Show practical examples */
    show_practical_examples();

    /* Show security practices */
    show_security_practices();

    printf("Current System State\n");
    printf("====================\n\n");

    printf("Environment variables:\n");
    printf("  UNSANDBOX_PUBLIC_KEY:  %s\n",
           getenv("UNSANDBOX_PUBLIC_KEY") ? "(set)" : "(not set)");
    printf("  UNSANDBOX_SECRET_KEY:  %s\n",
           getenv("UNSANDBOX_SECRET_KEY") ? "(set)" : "(not set)");
    printf("  UNSANDBOX_ACCOUNT:     %s\n",
           getenv("UNSANDBOX_ACCOUNT") ? getenv("UNSANDBOX_ACCOUNT") : "(not set)");

    char config_path[512];
    snprintf(config_path, sizeof(config_path), "%s/.unsandbox/accounts.csv",
             get_home_dir());
    printf("\nConfig file:\n");
    printf("  %s: %s\n", config_path,
           file_exists(config_path) ? "exists" : "does not exist");

    printf("\nTo use unsandbox, set credentials via one of these methods:\n");
    printf("  1. export UNSANDBOX_PUBLIC_KEY=... UNSANDBOX_SECRET_KEY=...\n");
    printf("  2. Create ~/.unsandbox/accounts.csv\n");
    printf("  3. Pass -p and -k flags to the command\n");

    return 0;
}
