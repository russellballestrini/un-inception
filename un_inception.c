// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software


// UN CLI - C Implementation (using curl subprocess for simplicity)
// Compile: gcc -o un_c un_inception.c
// Usage:
//   un_c script.py
//   un_c -e KEY=VALUE -f data.txt script.py
//   un_c session --list
//   un_c service --name web --ports 8080

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

#define API_BASE "https://api.unsandbox.com"
#define PORTAL_BASE "https://unsandbox.com"
#define BLUE "\033[34m"
#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define RESET "\033[0m"
#define MAX_CMD_LEN 16384

const char* detect_language(const char *filename) {
    const char *ext = strrchr(filename, '.');
    if (!ext) return NULL;

    if (strcmp(ext, ".py") == 0) return "python";
    if (strcmp(ext, ".js") == 0) return "javascript";
    if (strcmp(ext, ".ts") == 0) return "typescript";
    if (strcmp(ext, ".go") == 0) return "go";
    if (strcmp(ext, ".rs") == 0) return "rust";
    if (strcmp(ext, ".c") == 0) return "c";
    if (strcmp(ext, ".cpp") == 0 || strcmp(ext, ".cc") == 0) return "cpp";
    if (strcmp(ext, ".d") == 0) return "d";
    if (strcmp(ext, ".zig") == 0) return "zig";
    if (strcmp(ext, ".nim") == 0) return "nim";
    if (strcmp(ext, ".v") == 0) return "v";
    if (strcmp(ext, ".rb") == 0) return "ruby";
    if (strcmp(ext, ".php") == 0) return "php";
    if (strcmp(ext, ".pl") == 0) return "perl";
    if (strcmp(ext, ".lua") == 0) return "lua";
    if (strcmp(ext, ".sh") == 0) return "bash";

    return NULL;
}

char* read_file(const char *filename) {
    FILE *f = fopen(filename, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *content = malloc(fsize + 1);
    if (!content) {
        fclose(f);
        return NULL;
    }

    fread(content, 1, fsize, f);
    content[fsize] = 0;
    fclose(f);

    return content;
}

void escape_json_char(FILE *out, char c) {
    switch (c) {
        case '"': fputs("\\\"", out); break;
        case '\\': fputs("\\\\", out); break;
        case '\n': fputs("\\n", out); break;
        case '\r': fputs("\\r", out); break;
        case '\t': fputs("\\t", out); break;
        default: fputc(c, out); break;
    }
}

char* compute_hmac(const char *secret_key, const char *message) {
    char cmd[MAX_CMD_LEN];
    snprintf(cmd, sizeof(cmd), "echo -n '%s' | openssl dgst -sha256 -hmac '%s' -hex | sed 's/.*= //'", message, secret_key);

    FILE *pipe = popen(cmd, "r");
    if (!pipe) return NULL;

    static char result[128];
    if (fgets(result, sizeof(result), pipe)) {
        // Trim newline
        size_t len = strlen(result);
        while (len > 0 && (result[len-1] == '\n' || result[len-1] == '\r')) {
            result[--len] = '\0';
        }
    }
    pclose(pipe);
    return result;
}

void get_timestamp(char *buffer, size_t bufsize) {
    snprintf(buffer, bufsize, "%ld", (long)time(NULL));
}

void build_auth_headers(char *buffer, size_t bufsize, const char *method, const char *path, const char *body, const char *public_key, const char *secret_key) {
    if (!secret_key || secret_key[0] == '\0') {
        // Legacy mode: use public_key as bearer token
        snprintf(buffer, bufsize, "-H 'Authorization: Bearer %s'", public_key);
        return;
    }

    // HMAC mode
    char timestamp[32];
    get_timestamp(timestamp, sizeof(timestamp));

    char message[MAX_CMD_LEN];
    snprintf(message, sizeof(message), "%s:%s:%s:%s", timestamp, method, path, body ? body : "");

    char *signature = compute_hmac(secret_key, message);
    if (!signature) {
        fprintf(stderr, "%sError computing HMAC signature%s\n", RED, RESET);
        exit(1);
    }

    snprintf(buffer, bufsize, "-H 'Authorization: Bearer %s' -H 'X-Timestamp: %s' -H 'X-Signature: %s'",
             public_key, timestamp, signature);
}

void cmd_execute(const char *source_file, char **envs, int env_count, char **files, int file_count, int artifacts, const char *output_dir, const char *network, int vcpu, const char *public_key, const char *secret_key) {
    const char *language = detect_language(source_file);
    if (!language) {
        fprintf(stderr, "%sError: Cannot detect language%s\n", RED, RESET);
        exit(1);
    }

    char *code = read_file(source_file);
    if (!code) {
        fprintf(stderr, "%sError reading file%s\n", RED, RESET);
        exit(1);
    }

    // Build JSON request body
    FILE *jsonf = tmpfile();
    fprintf(jsonf, "{\"language\":\"%s\",\"code\":\"", language);
    for (const char *p = code; *p; p++) {
        escape_json_char(jsonf, *p);
    }
    fprintf(jsonf, "\"");

    // Add env vars
    if (env_count > 0) {
        fprintf(jsonf, ",\"env\":{");
        for (int i = 0; i < env_count; i++) {
            char *eq = strchr(envs[i], '=');
            if (eq) {
                if (i > 0) fprintf(jsonf, ",");
                fprintf(jsonf, "\"");
                for (char *p = envs[i]; p < eq; p++) fputc(*p, jsonf);
                fprintf(jsonf, "\":\"");
                for (char *p = eq + 1; *p; p++) {
                    escape_json_char(jsonf, *p);
                }
                fprintf(jsonf, "\"");
            }
        }
        fprintf(jsonf, "}");
    }

    // Note: Input files would require base64 encoding - skipped for simplicity

    if (artifacts) {
        fprintf(jsonf, ",\"return_artifacts\":true");
    }
    if (network) {
        fprintf(jsonf, ",\"network\":\"%s\"", network);
    }
    if (vcpu > 0) {
        fprintf(jsonf, ",\"vcpu\":%d", vcpu);
    }

    fprintf(jsonf, "}");
    fflush(jsonf);
    rewind(jsonf);

    // Read JSON to string
    fseek(jsonf, 0, SEEK_END);
    long json_size = ftell(jsonf);
    rewind(jsonf);
    char *json_body = malloc(json_size + 1);
    fread(json_body, 1, json_size, jsonf);
    json_body[json_size] = 0;
    fclose(jsonf);

    // Make API request using curl
    char auth_headers[1024];
    build_auth_headers(auth_headers, sizeof(auth_headers), "POST", "/execute", json_body, public_key, secret_key);

    char cmd[MAX_CMD_LEN];
    snprintf(cmd, sizeof(cmd),
        "curl -s -X POST '%s/execute' "
        "-H 'Content-Type: application/json' "
        "%s "
        "-d @- << 'EOF'\n%s\nEOF",
        API_BASE, auth_headers, json_body);

    FILE *curl = popen(cmd, "r");
    if (!curl) {
        fprintf(stderr, "%sError running curl%s\n", RED, RESET);
        exit(1);
    }

    // Read response
    char response[1048576];
    size_t resp_len = fread(response, 1, sizeof(response) - 1, curl);
    response[resp_len] = 0;
    pclose(curl);

    // Parse simple JSON (stdout, stderr, exit_code)
    char *stdout_start = strstr(response, "\"stdout\":\"");
    char *stderr_start = strstr(response, "\"stderr\":\"");
    char *exit_code_start = strstr(response, "\"exit_code\":");

    int exit_code = 1;
    if (exit_code_start) {
        exit_code = atoi(exit_code_start + 12);
    }

    // Print stdout
    if (stdout_start) {
        stdout_start += 10;
        printf("%s", BLUE);
        for (char *p = stdout_start; *p && !(*p == '"' && *(p-1) != '\\'); p++) {
            if (*p == '\\' && *(p+1) == 'n') {
                putchar('\n');
                p++;
            } else if (*p == '\\' && *(p+1) == 't') {
                putchar('\t');
                p++;
            } else if (*p == '\\' && *(p+1) == '"') {
                putchar('"');
                p++;
            } else if (*p == '\\' && *(p+1) == '\\') {
                putchar('\\');
                p++;
            } else {
                putchar(*p);
            }
        }
        printf("%s", RESET);
    }

    // Print stderr
    if (stderr_start) {
        stderr_start += 10;
        fprintf(stderr, "%s", RED);
        for (char *p = stderr_start; *p && !(*p == '"' && *(p-1) != '\\'); p++) {
            if (*p == '\\' && *(p+1) == 'n') {
                fputc('\n', stderr);
                p++;
            } else if (*p == '\\' && *(p+1) == 't') {
                fputc('\t', stderr);
                p++;
            } else if (*p == '\\' && *(p+1) == '"') {
                fputc('"', stderr);
                p++;
            } else if (*p == '\\' && *(p+1) == '\\') {
                fputc('\\', stderr);
                p++;
            } else {
                fputc(*p, stderr);
            }
        }
        fprintf(stderr, "%s", RESET);
    }

    free(code);
    free(json_body);
    exit(exit_code);
}

void cmd_session(int list, const char *kill, const char *shell, const char *network, int vcpu, int tmux, int screen, const char *public_key, const char *secret_key) {
    char cmd[8192];
    char auth_headers[1024];

    if (list) {
        build_auth_headers(auth_headers, sizeof(auth_headers), "GET", "/sessions", "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X GET '%s/sessions' %s",
            API_BASE, auth_headers);
        system(cmd);
        printf("\n");
        return;
    }

    if (kill) {
        char path[512];
        snprintf(path, sizeof(path), "/sessions/%s", kill);
        build_auth_headers(auth_headers, sizeof(auth_headers), "DELETE", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X DELETE '%s/sessions/%s' %s",
            API_BASE, kill, auth_headers);
        system(cmd);
        printf("%sSession terminated: %s%s\n", GREEN, kill, RESET);
        return;
    }

    // Create session
    char json[1024];
    snprintf(json, sizeof(json), "{\"shell\":\"%s\"", shell ? shell : "bash");
    if (network) {
        char temp[128];
        snprintf(temp, sizeof(temp), ",\"network\":\"%s\"", network);
        strcat(json, temp);
    }
    if (vcpu > 0) {
        char temp[64];
        snprintf(temp, sizeof(temp), ",\"vcpu\":%d", vcpu);
        strcat(json, temp);
    }
    if (tmux) {
        strcat(json, ",\"persistence\":\"tmux\"");
    }
    if (screen) {
        strcat(json, ",\"persistence\":\"screen\"");
    }
    strcat(json, "}");

    printf("%sCreating session...%s\n", YELLOW, RESET);
    build_auth_headers(auth_headers, sizeof(auth_headers), "POST", "/sessions", json, public_key, secret_key);
    snprintf(cmd, sizeof(cmd),
        "curl -s -X POST '%s/sessions' -H 'Content-Type: application/json' "
        "%s -d '%s'",
        API_BASE, auth_headers, json);
    system(cmd);
    printf("\n%sSession created%s\n", GREEN, RESET);
}

void cmd_key(int extend, const char *public_key, const char *secret_key) {
    char cmd[8192];
    char auth_headers[1024];

    build_auth_headers(auth_headers, sizeof(auth_headers), "POST", "/keys/validate", "", public_key, secret_key);
    snprintf(cmd, sizeof(cmd),
        "curl -s -X POST '%s/keys/validate' "
        "-H 'Content-Type: application/json' "
        "%s",
        PORTAL_BASE, auth_headers);

    FILE *curl = popen(cmd, "r");
    if (!curl) {
        fprintf(stderr, "%sError running curl%s\n", RED, RESET);
        exit(1);
    }

    char response[16384];
    size_t resp_len = fread(response, 1, sizeof(response) - 1, curl);
    response[resp_len] = 0;
    pclose(curl);

    // Parse JSON response (simple string matching)
    char *public_key_start = strstr(response, "\"public_key\":\"");
    char public_key[256] = "";
    if (public_key_start) {
        public_key_start += 14;
        char *end = strchr(public_key_start, '"');
        if (end) {
            size_t len = end - public_key_start;
            if (len < sizeof(public_key)) {
                strncpy(public_key, public_key_start, len);
                public_key[len] = 0;
            }
        }
    }

    // Handle --extend flag
    if (extend) {
        if (public_key[0]) {
            printf("%sOpening browser to extend key...%s\n", BLUE, RESET);
            char open_cmd[512];
            #ifdef __APPLE__
                snprintf(open_cmd, sizeof(open_cmd), "open '%s/keys/extend?pk=%s'", PORTAL_BASE, public_key);
            #elif __linux__
                snprintf(open_cmd, sizeof(open_cmd), "xdg-open '%s/keys/extend?pk=%s'", PORTAL_BASE, public_key);
            #else
                snprintf(open_cmd, sizeof(open_cmd), "start '%s/keys/extend?pk=%s'", PORTAL_BASE, public_key);
            #endif
            system(open_cmd);
            return;
        } else {
            fprintf(stderr, "%sError: Could not retrieve public key%s\n", RED, RESET);
            exit(1);
        }
    }

    // Check if expired
    if (strstr(response, "\"expired\":true")) {
        printf("%sExpired%s\n", RED, RESET);
        printf("Public Key: %s\n", public_key[0] ? public_key : "N/A");

        char *tier_start = strstr(response, "\"tier\":\"");
        if (tier_start) {
            tier_start += 8;
            char *end = strchr(tier_start, '"');
            if (end) printf("Tier: %.*s\n", (int)(end - tier_start), tier_start);
        }

        char *expires_start = strstr(response, "\"expires_at\":\"");
        if (expires_start) {
            expires_start += 14;
            char *end = strchr(expires_start, '"');
            if (end) printf("Expired: %.*s\n", (int)(end - expires_start), expires_start);
        }

        printf("%sTo renew: Visit %s/keys/extend%s\n", YELLOW, PORTAL_BASE, RESET);
        exit(1);
    }

    // Valid key
    printf("%sValid%s\n", GREEN, RESET);
    printf("Public Key: %s\n", public_key[0] ? public_key : "N/A");

    // Extract and print other fields
    char *tier_start = strstr(response, "\"tier\":\"");
    if (tier_start) {
        tier_start += 8;
        char *end = strchr(tier_start, '"');
        if (end) printf("Tier: %.*s\n", (int)(end - tier_start), tier_start);
    }

    char *status_start = strstr(response, "\"status\":\"");
    if (status_start) {
        status_start += 10;
        char *end = strchr(status_start, '"');
        if (end) printf("Status: %.*s\n", (int)(end - status_start), status_start);
    }

    char *expires_start = strstr(response, "\"expires_at\":\"");
    if (expires_start) {
        expires_start += 14;
        char *end = strchr(expires_start, '"');
        if (end) printf("Expires: %.*s\n", (int)(end - expires_start), expires_start);
    }

    char *time_rem_start = strstr(response, "\"time_remaining\":\"");
    if (time_rem_start) {
        time_rem_start += 18;
        char *end = strchr(time_rem_start, '"');
        if (end) printf("Time Remaining: %.*s\n", (int)(end - time_rem_start), time_rem_start);
    }

    char *rate_start = strstr(response, "\"rate_limit\":");
    if (rate_start) {
        rate_start += 13;
        char *end = strchr(rate_start, ',');
        if (!end) end = strchr(rate_start, '}');
        if (end) printf("Rate Limit: %.*s\n", (int)(end - rate_start), rate_start);
    }

    char *burst_start = strstr(response, "\"burst\":");
    if (burst_start) {
        burst_start += 8;
        char *end = strchr(burst_start, ',');
        if (!end) end = strchr(burst_start, '}');
        if (end) printf("Burst: %.*s\n", (int)(end - burst_start), burst_start);
    }

    char *conc_start = strstr(response, "\"concurrency\":");
    if (conc_start) {
        conc_start += 14;
        char *end = strchr(conc_start, ',');
        if (!end) end = strchr(conc_start, '}');
        if (end) printf("Concurrency: %.*s\n", (int)(end - conc_start), conc_start);
    }
}

void cmd_service(const char *name, const char *ports, const char *domains, const char *service_type, const char *bootstrap, int list, const char *info, const char *logs, const char *tail, const char *sleep_svc, const char *wake, const char *destroy, const char *network, int vcpu, const char *public_key, const char *secret_key) {
    char cmd[16384];
    char auth_headers[1024];

    if (list) {
        build_auth_headers(auth_headers, sizeof(auth_headers), "GET", "/services", "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X GET '%s/services' %s",
            API_BASE, auth_headers);
        system(cmd);
        printf("\n");
        return;
    }

    if (info) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s", info);
        build_auth_headers(auth_headers, sizeof(auth_headers), "GET", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X GET '%s/services/%s' %s",
            API_BASE, info, auth_headers);
        system(cmd);
        printf("\n");
        return;
    }

    if (logs) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s/logs", logs);
        build_auth_headers(auth_headers, sizeof(auth_headers), "GET", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X GET '%s/services/%s/logs' %s",
            API_BASE, logs, auth_headers);
        system(cmd);
        return;
    }

    if (tail) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s/logs", tail);
        build_auth_headers(auth_headers, sizeof(auth_headers), "GET", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X GET '%s/services/%s/logs?lines=9000' %s",
            API_BASE, tail, auth_headers);
        system(cmd);
        return;
    }

    if (sleep_svc) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s/sleep", sleep_svc);
        build_auth_headers(auth_headers, sizeof(auth_headers), "POST", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X POST '%s/services/%s/sleep' %s",
            API_BASE, sleep_svc, auth_headers);
        system(cmd);
        printf("%sService sleeping: %s%s\n", GREEN, sleep_svc, RESET);
        return;
    }

    if (wake) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s/wake", wake);
        build_auth_headers(auth_headers, sizeof(auth_headers), "POST", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X POST '%s/services/%s/wake' %s",
            API_BASE, wake, auth_headers);
        system(cmd);
        printf("%sService waking: %s%s\n", GREEN, wake, RESET);
        return;
    }

    if (destroy) {
        char path[512];
        snprintf(path, sizeof(path), "/services/%s", destroy);
        build_auth_headers(auth_headers, sizeof(auth_headers), "DELETE", path, "", public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X DELETE '%s/services/%s' %s",
            API_BASE, destroy, auth_headers);
        system(cmd);
        printf("%sService destroyed: %s%s\n", GREEN, destroy, RESET);
        return;
    }

    if (name) {
        char json[4096];
        snprintf(json, sizeof(json), "{\"name\":\"%s\"", name);
        if (ports) {
            char temp[256];
            snprintf(temp, sizeof(temp), ",\"ports\":[%s]", ports);
            strcat(json, temp);
        }
        if (service_type) {
            char temp[128];
            snprintf(temp, sizeof(temp), ",\"service_type\":\"%s\"", service_type);
            strcat(json, temp);
        }
        if (bootstrap) {
            // Check if file
            struct stat st;
            if (stat(bootstrap, &st) == 0) {
                char *boot_code = read_file(bootstrap);
                if (boot_code) {
                    strcat(json, ",\"bootstrap\":\"");
                    for (char *p = boot_code; *p; p++) {
                        // Simplified escaping
                        if (*p == '"') strcat(json, "\\\"");
                        else if (*p == '\n') strcat(json, "\\n");
                        else { char c[2] = {*p, 0}; strcat(json, c); }
                    }
                    strcat(json, "\"");
                    free(boot_code);
                }
            } else {
                strcat(json, ",\"bootstrap\":\"");
                strcat(json, bootstrap);
                strcat(json, "\"");
            }
        }
        strcat(json, "}");

        printf("%sCreating service...%s\n", YELLOW, RESET);
        build_auth_headers(auth_headers, sizeof(auth_headers), "POST", "/services", json, public_key, secret_key);
        snprintf(cmd, sizeof(cmd),
            "curl -s -X POST '%s/services' -H 'Content-Type: application/json' "
            "%s -d '%s'",
            API_BASE, auth_headers, json);
        system(cmd);
        printf("\n%sService created%s\n", GREEN, RESET);
        return;
    }

    fprintf(stderr, "%sError: Specify --name to create a service%s\n", RED, RESET);
    exit(1);
}

int main(int argc, char *argv[]) {
    const char *public_key = getenv("UNSANDBOX_PUBLIC_KEY");
    const char *secret_key = getenv("UNSANDBOX_SECRET_KEY");

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if (!public_key) {
        public_key = getenv("UNSANDBOX_API_KEY");
    }
    if (!secret_key) {
        secret_key = "";
    }

    if (argc < 2) {
        fprintf(stderr, "Usage: %s [options] <source_file>\n", argv[0]);
        fprintf(stderr, "       %s session [options]\n", argv[0]);
        fprintf(stderr, "       %s service [options]\n", argv[0]);
        fprintf(stderr, "       %s key [options]\n", argv[0]);
        return 1;
    }

    // Parse command
    if (strcmp(argv[1], "key") == 0) {
        int extend = 0;
        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "--extend") == 0) extend = 1;
            else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) public_key = argv[++i];
        }
        cmd_key(extend, public_key, secret_key);
        return 0;
    }

    if (strcmp(argv[1], "session") == 0) {
        int list = 0;
        const char *kill = NULL;
        const char *shell = NULL;
        const char *network = NULL;
        int vcpu = 0;
        int tmux = 0;
        int screen = 0;

        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "--list") == 0) list = 1;
            else if (strcmp(argv[i], "--kill") == 0 && i + 1 < argc) kill = argv[++i];
            else if (strcmp(argv[i], "--shell") == 0 && i + 1 < argc) shell = argv[++i];
            else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) network = argv[++i];
            else if (strcmp(argv[i], "-v") == 0 && i + 1 < argc) vcpu = atoi(argv[++i]);
            else if (strcmp(argv[i], "--tmux") == 0) tmux = 1;
            else if (strcmp(argv[i], "--screen") == 0) screen = 1;
            else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) public_key = argv[++i];
        }

        cmd_session(list, kill, shell, network, vcpu, tmux, screen, public_key, secret_key);
        return 0;
    }

    if (strcmp(argv[1], "service") == 0) {
        const char *name = NULL;
        const char *ports = NULL;
        const char *domains = NULL;
        const char *service_type = NULL;
        const char *bootstrap = NULL;
        int list = 0;
        const char *info = NULL;
        const char *logs = NULL;
        const char *tail = NULL;
        const char *sleep_svc = NULL;
        const char *wake = NULL;
        const char *destroy = NULL;
        const char *network = NULL;
        int vcpu = 0;

        for (int i = 2; i < argc; i++) {
            if (strcmp(argv[i], "--name") == 0 && i + 1 < argc) name = argv[++i];
            else if (strcmp(argv[i], "--ports") == 0 && i + 1 < argc) ports = argv[++i];
            else if (strcmp(argv[i], "--domains") == 0 && i + 1 < argc) domains = argv[++i];
            else if (strcmp(argv[i], "--type") == 0 && i + 1 < argc) service_type = argv[++i];
            else if (strcmp(argv[i], "--bootstrap") == 0 && i + 1 < argc) bootstrap = argv[++i];
            else if (strcmp(argv[i], "--list") == 0) list = 1;
            else if (strcmp(argv[i], "--info") == 0 && i + 1 < argc) info = argv[++i];
            else if (strcmp(argv[i], "--logs") == 0 && i + 1 < argc) logs = argv[++i];
            else if (strcmp(argv[i], "--tail") == 0 && i + 1 < argc) tail = argv[++i];
            else if (strcmp(argv[i], "--freeze") == 0 && i + 1 < argc) sleep_svc = argv[++i];
            else if (strcmp(argv[i], "--unfreeze") == 0 && i + 1 < argc) wake = argv[++i];
            else if (strcmp(argv[i], "--destroy") == 0 && i + 1 < argc) destroy = argv[++i];
            else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) network = argv[++i];
            else if (strcmp(argv[i], "-v") == 0 && i + 1 < argc) vcpu = atoi(argv[++i]);
            else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) public_key = argv[++i];
        }

        cmd_service(name, ports, domains, service_type, bootstrap, list, info, logs, tail, sleep_svc, wake, destroy, network, vcpu, public_key, secret_key);
        return 0;
    }

    // Execute mode
    char *envs[32];
    int env_count = 0;
    char *files[32];
    int file_count = 0;
    int artifacts = 0;
    const char *output_dir = NULL;
    const char *network = NULL;
    int vcpu = 0;
    const char *source_file = NULL;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-e") == 0 && i + 1 < argc) {
            envs[env_count++] = argv[++i];
        } else if (strcmp(argv[i], "-f") == 0 && i + 1 < argc) {
            files[file_count++] = argv[++i];
        } else if (strcmp(argv[i], "-a") == 0) {
            artifacts = 1;
        } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_dir = argv[++i];
        } else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
            network = argv[++i];
        } else if (strcmp(argv[i], "-v") == 0 && i + 1 < argc) {
            vcpu = atoi(argv[++i]);
        } else if (strcmp(argv[i], "-k") == 0 && i + 1 < argc) {
            public_key = argv[++i];
        } else if (!source_file && argv[i][0] != '-') {
            source_file = argv[i];
        }
    }

    if (!source_file) {
        fprintf(stderr, "%sError: No source file specified%s\n", RED, RESET);
        return 1;
    }

    cmd_execute(source_file, envs, env_count, files, file_count, artifacts, output_dir, network, vcpu, public_key, secret_key);
    return 0;
}
