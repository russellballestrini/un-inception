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


// UN CLI - D Implementation (using curl subprocess for simplicity)
// Compile: dmd un.d -of=un_d
// Or with LDC: ldc2 un.d -of=un_d
// Usage:
//   un_d script.py
//   un_d -e KEY=VALUE script.py
//   un_d session --list
//   un_d service --name web --ports 8080

import std.stdio;
import std.file;
import std.path;
import std.process;
import std.string;
import std.conv;
import std.array;
import std.algorithm;

immutable string API_BASE = "https://api.unsandbox.com";
immutable string PORTAL_BASE = "https://unsandbox.com";
immutable string BLUE = "\033[34m";
immutable string RED = "\033[31m";
immutable string GREEN = "\033[32m";
immutable string YELLOW = "\033[33m";
immutable string RESET = "\033[0m";
immutable size_t MAX_ENV_CONTENT_SIZE = 65536;

string detectLanguage(string filename) {
    string[string] langMap = [
        ".py": "python", ".js": "javascript", ".ts": "typescript",
        ".go": "go", ".rs": "rust", ".c": "c", ".cpp": "cpp",
        ".d": "d", ".zig": "zig", ".nim": "nim", ".v": "v",
        ".rb": "ruby", ".php": "php", ".sh": "bash"
    ];

    string ext = extension(filename);
    return langMap.get(ext, "");
}

string escapeJson(string s) {
    string result;
    foreach (c; s) {
        switch (c) {
            case '"': result ~= "\\\""; break;
            case '\\': result ~= "\\\\"; break;
            case '\n': result ~= "\\n"; break;
            case '\r': result ~= "\\r"; break;
            case '\t': result ~= "\\t"; break;
            default: result ~= c; break;
        }
    }
    return result;
}

string readAndBase64(string filepath) {
    import std.base64 : Base64;
    try {
        auto content = readText(filepath);
        return Base64.encode(cast(ubyte[])content);
    } catch (Exception e) {
        stderr.writefln("%sError: Cannot read file: %s%s", RED, filepath, RESET);
        return "";
    }
}

string buildInputFilesJson(string[] files) {
    if (files.length == 0) return "";
    string[] fileJsons;
    foreach (f; files) {
        string b64 = readAndBase64(f);
        if (b64.empty) continue;
        string basename = baseName(f);
        fileJsons ~= format(`{"filename":"%s","content":"%s"}`, escapeJson(basename), b64);
    }
    if (fileJsons.length == 0) return "";
    import std.array : join;
    return format(`,"input_files":[%s]`, fileJsons.join(","));
}

string computeHmac(string secretKey, string message) {
    import std.process : pipeShell, Redirect, wait;
    import std.stdio : File;

    auto cmd = format("echo -n '%s' | openssl dgst -sha256 -hmac '%s' -hex 2>/dev/null | sed 's/.*= //'", message, secretKey);
    auto pipes = pipeShell(cmd, Redirect.stdout);
    string result = pipes.stdout.readln().strip();
    wait(pipes.pid);
    return result;
}

string getTimestamp() {
    import std.datetime.systime : Clock;
    return format("%d", Clock.currTime.toUnixTime());
}

string buildAuthHeaders(string method, string path, string body, string publicKey, string secretKey) {
    if (secretKey.empty) {
        // Legacy mode: use public_key as bearer token
        return format("-H 'Authorization: Bearer %s'", publicKey);
    }

    // HMAC mode
    string timestamp = getTimestamp();
    string message = format("%s:%s:%s:%s", timestamp, method, path, body);
    string signature = computeHmac(secretKey, message);

    return format("-H 'Authorization: Bearer %s' -H 'X-Timestamp: %s' -H 'X-Signature: %s'",
                  publicKey, timestamp, signature);
}

string execCurl(string cmd) {
    auto result = executeShell(cmd);
    string output = result.output;

    // Check for timestamp authentication errors
    import std.algorithm : canFind;
    if (output.canFind("timestamp") &&
        (output.canFind("401") || output.canFind("expired") || output.canFind("invalid"))) {
        stderr.writefln("%sError: Request timestamp expired (must be within 5 minutes of server time)%s", RED, RESET);
        stderr.writefln("%sYour computer's clock may have drifted.%s", YELLOW, RESET);
        stderr.writeln("Check your system time and sync with NTP if needed:");
        stderr.writeln("  Linux:   sudo ntpdate -s time.nist.gov");
        stderr.writeln("  macOS:   sudo sntp -sS time.apple.com");
        stderr.writeln("  Windows: w32tm /resync");
        exit(1);
    }

    return output;
}

bool execCurlPut(string endpoint, string body, string publicKey, string secretKey) {
    import std.file : write, remove;
    import std.random : uniform;
    string tmpFile = format("/tmp/un_d_%d.txt", uniform(0, 999999));
    write(tmpFile, body);
    string authHeaders = buildAuthHeaders("PUT", endpoint, body, publicKey, secretKey);
    string cmd = format(`curl -s -o /dev/null -w '%%{http_code}' -X PUT '%s%s' -H 'Content-Type: text/plain' %s -d @%s`, API_BASE, endpoint, authHeaders, tmpFile);
    auto result = executeShell(cmd);
    remove(tmpFile);
    try {
        int status = to!int(result.output.strip());
        return status >= 200 && status < 300;
    } catch (Exception e) {
        return false;
    }
}

string readEnvFile(string path) {
    if (!exists(path)) {
        stderr.writefln("%sError: Env file not found: %s%s", RED, path, RESET);
        exit(1);
    }
    return readText(path);
}

string buildEnvContent(string[] envs, string envFile) {
    string[] lines = envs.dup;
    if (!envFile.empty) {
        string content = readEnvFile(envFile);
        foreach (line; content.split("\n")) {
            string trimmed = line.strip();
            if (!trimmed.empty && !trimmed.startsWith("#")) {
                lines ~= trimmed;
            }
        }
    }
    import std.array : join;
    return lines.join("\n");
}

string extractJsonField(string response, string field) {
    import std.algorithm : findSplitAfter;
    auto search = response.findSplitAfter(format(`"%s":"`, field));
    if (search[0].length > 0 && search[1].length > 0) {
        auto endSearch = search[1].findSplitAfter(`"`);
        if (endSearch[0].length > 1) {
            return endSearch[0][0..$-1];
        }
    }
    return "";
}

void cmdServiceEnv(string action, string target, string[] svcEnvs, string svcEnvFile, string publicKey, string secretKey) {
    if (action == "status") {
        if (target.empty) {
            stderr.writefln("%sError: service env status requires service ID%s", RED, RESET);
            exit(1);
        }
        string path = format("/services/%s/env", target);
        string authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/services/%s/env' %s`, API_BASE, target, authHeaders);
        string response = execCurl(cmd);

        import std.algorithm : canFind;
        if (response.canFind(`"has_vault":true`)) {
            writefln("%sVault: configured%s", GREEN, RESET);
            string envCount = extractJsonField(response, "env_count");
            if (!envCount.empty) writefln("Variables: %s", envCount);
            string updatedAt = extractJsonField(response, "updated_at");
            if (!updatedAt.empty) writefln("Updated: %s", updatedAt);
        } else {
            writefln("%sVault: not configured%s", YELLOW, RESET);
        }
        return;
    }

    if (action == "set") {
        if (target.empty) {
            stderr.writefln("%sError: service env set requires service ID%s", RED, RESET);
            exit(1);
        }
        if (svcEnvs.length == 0 && svcEnvFile.empty) {
            stderr.writefln("%sError: service env set requires -e or --env-file%s", RED, RESET);
            exit(1);
        }
        string envContent = buildEnvContent(svcEnvs, svcEnvFile);
        if (envContent.length > MAX_ENV_CONTENT_SIZE) {
            stderr.writefln("%sError: Env content exceeds maximum size of 64KB%s", RED, RESET);
            exit(1);
        }
        if (execCurlPut(format("/services/%s/env", target), envContent, publicKey, secretKey)) {
            writefln("%sVault updated for service %s%s", GREEN, target, RESET);
        } else {
            stderr.writefln("%sError: Failed to update vault%s", RED, RESET);
            exit(1);
        }
        return;
    }

    if (action == "export") {
        if (target.empty) {
            stderr.writefln("%sError: service env export requires service ID%s", RED, RESET);
            exit(1);
        }
        string path = format("/services/%s/env/export", target);
        string authHeaders = buildAuthHeaders("POST", path, "{}", publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services/%s/env/export' -H 'Content-Type: application/json' %s -d '{}'`, API_BASE, target, authHeaders);
        string response = execCurl(cmd);
        string content = extractJsonField(response, "content");
        if (!content.empty) {
            content = content.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\");
            write(content);
        }
        return;
    }

    if (action == "delete") {
        if (target.empty) {
            stderr.writefln("%sError: service env delete requires service ID%s", RED, RESET);
            exit(1);
        }
        string path = format("/services/%s/env", target);
        string authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -o /dev/null -w '%%{http_code}' -X DELETE '%s/services/%s/env' %s`, API_BASE, target, authHeaders);
        auto result = executeShell(cmd);
        try {
            int status = to!int(result.output.strip());
            if (status >= 200 && status < 300) {
                writefln("%sVault deleted for service %s%s", GREEN, target, RESET);
            } else {
                stderr.writefln("%sError: Failed to delete vault%s", RED, RESET);
                exit(1);
            }
        } catch (Exception e) {
            stderr.writefln("%sError: Failed to delete vault%s", RED, RESET);
            exit(1);
        }
        return;
    }

    stderr.writefln("%sError: Unknown env action: %s%s", RED, action, RESET);
    stderr.writeln("Usage: un.d service env <status|set|export|delete> <service_id>");
    exit(1);
}

void cmdExecute(string sourceFile, string[] envs, bool artifacts, string network, int vcpu, string publicKey, string secretKey) {
    string lang = detectLanguage(sourceFile);
    if (lang.empty) {
        stderr.writefln("%sError: Cannot detect language%s", RED, RESET);
        exit(1);
    }

    string code = readText(sourceFile);
    string json = format(`{"language":"%s","code":"%s"`, lang, escapeJson(code));

    if (envs.length > 0) {
        json ~= `,"env":{`;
        foreach (i, e; envs) {
            auto parts = e.split("=");
            if (parts.length == 2) {
                if (i > 0) json ~= ",";
                json ~= format(`"%s":"%s"`, parts[0], escapeJson(parts[1]));
            }
        }
        json ~= "}";
    }

    if (artifacts) json ~= `,"return_artifacts":true`;
    if (!network.empty) json ~= format(`,"network":"%s"`, network);
    if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
    json ~= "}";

    string authHeaders = buildAuthHeaders("POST", "/execute", json, publicKey, secretKey);
    string cmd = format(`curl -s -X POST '%s/execute' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, authHeaders, json);
    string result = execCurl(cmd);

    writeln(result);
}

void cmdSession(bool list, string kill, string shell, string network, int vcpu, bool tmux, bool screen, string[] inputFiles, string publicKey, string secretKey) {
    if (list) {
        string authHeaders = buildAuthHeaders("GET", "/sessions", "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/sessions' %s`, API_BASE, authHeaders);
        writeln(execCurl(cmd));
        return;
    }

    if (!kill.empty) {
        string path = format("/sessions/%s", kill);
        string authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X DELETE '%s/sessions/%s' %s`, API_BASE, kill, authHeaders);
        execCurl(cmd);
        writefln("%sSession terminated: %s%s", GREEN, kill, RESET);
        return;
    }

    string json = format(`{"shell":"%s"`, shell.empty ? "bash" : shell);
    if (!network.empty) json ~= format(`,"network":"%s"`, network);
    if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
    if (tmux) json ~= `,"persistence":"tmux"`;
    if (screen) json ~= `,"persistence":"screen"`;
    json ~= buildInputFilesJson(inputFiles);
    json ~= "}";

    writefln("%sCreating session...%s", YELLOW, RESET);
    string authHeaders = buildAuthHeaders("POST", "/sessions", json, publicKey, secretKey);
    string cmd = format(`curl -s -X POST '%s/sessions' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, authHeaders, json);
    writeln(execCurl(cmd));
}

void cmdService(string name, string ports, string bootstrap, string bootstrapFile, string type, bool list, string info, string logs, string tail, string sleep, string wake, string destroy, string resize, int resizeVcpu, string execute, string command, string dumpBootstrap, string dumpFile, string network, int vcpu, string[] inputFiles, string[] svcEnvs, string svcEnvFile, string envAction, string envTarget, string publicKey, string secretKey) {
    // Handle env subcommand
    if (!envAction.empty) {
        cmdServiceEnv(envAction, envTarget, svcEnvs, svcEnvFile, publicKey, secretKey);
        return;
    }

    if (list) {
        string authHeaders = buildAuthHeaders("GET", "/services", "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/services' %s`, API_BASE, authHeaders);
        writeln(execCurl(cmd));
        return;
    }

    if (!info.empty) {
        string path = format("/services/%s", info);
        string authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/services/%s' %s`, API_BASE, info, authHeaders);
        writeln(execCurl(cmd));
        return;
    }

    if (!logs.empty) {
        string path = format("/services/%s/logs", logs);
        string authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/services/%s/logs' %s`, API_BASE, logs, authHeaders);
        write(execCurl(cmd));
        return;
    }

    if (!tail.empty) {
        string path = format("/services/%s/logs", tail);
        string authHeaders = buildAuthHeaders("GET", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X GET '%s/services/%s/logs?lines=9000' %s`, API_BASE, tail, authHeaders);
        write(execCurl(cmd));
        return;
    }

    if (!sleep.empty) {
        string path = format("/services/%s/freeze", sleep);
        string authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services/%s/freeze' %s`, API_BASE, sleep, authHeaders);
        execCurl(cmd);
        writefln("%sService frozen: %s%s", GREEN, sleep, RESET);
        return;
    }

    if (!wake.empty) {
        string path = format("/services/%s/unfreeze", wake);
        string authHeaders = buildAuthHeaders("POST", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services/%s/unfreeze' %s`, API_BASE, wake, authHeaders);
        execCurl(cmd);
        writefln("%sService unfreezing: %s%s", GREEN, wake, RESET);
        return;
    }

    if (!destroy.empty) {
        string path = format("/services/%s", destroy);
        string authHeaders = buildAuthHeaders("DELETE", path, "", publicKey, secretKey);
        string cmd = format(`curl -s -X DELETE '%s/services/%s' %s`, API_BASE, destroy, authHeaders);
        execCurl(cmd);
        writefln("%sService destroyed: %s%s", GREEN, destroy, RESET);
        return;
    }

    if (!resize.empty) {
        if (resizeVcpu < 1 || resizeVcpu > 8) {
            stderr.writefln("%sError: --vcpu must be between 1 and 8%s", RED, RESET);
            exit(1);
        }
        string json = format(`{"vcpu":%d}`, resizeVcpu);
        string path = format("/services/%s", resize);
        string authHeaders = buildAuthHeaders("PATCH", path, json, publicKey, secretKey);
        string cmd = format(`curl -s -X PATCH '%s/services/%s' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, resize, authHeaders, json);
        execCurl(cmd);
        int ram = resizeVcpu * 2;
        writefln("%sService resized to %d vCPU, %d GB RAM%s", GREEN, resizeVcpu, ram, RESET);
        return;
    }

    if (!execute.empty) {
        string json = format(`{"command":"%s"}`, escapeJson(command));
        string path = format("/services/%s/execute", execute);
        string authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services/%s/execute' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, execute, authHeaders, json);
        string result = execCurl(cmd);

        // Simple JSON parsing for stdout/stderr
        import std.algorithm : findSplitAfter;
        auto stdoutSearch = result.findSplitAfter(`"stdout":"`);
        if (stdoutSearch[0].length > 0 && stdoutSearch[1].length > 0) {
            auto stdoutEnd = stdoutSearch[1].findSplitAfter(`"`);
            if (stdoutEnd[0].length > 1) {
                string output = stdoutEnd[0][0..$-1];
                output = output.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\");
                write(output);
            }
        }

        auto stderrSearch = result.findSplitAfter(`"stderr":"`);
        if (stderrSearch[0].length > 0 && stderrSearch[1].length > 0) {
            auto stderrEnd = stderrSearch[1].findSplitAfter(`"`);
            if (stderrEnd[0].length > 1) {
                string errout = stderrEnd[0][0..$-1];
                errout = errout.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\");
                stderr.write(errout);
            }
        }
        return;
    }

    if (!dumpBootstrap.empty) {
        stderr.writefln("Fetching bootstrap script from %s...", dumpBootstrap);
        string json = `{"command":"cat /tmp/bootstrap.sh"}`;
        string path = format("/services/%s/execute", dumpBootstrap);
        string authHeaders = buildAuthHeaders("POST", path, json, publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services/%s/execute' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, dumpBootstrap, authHeaders, json);
        string result = execCurl(cmd);

        import std.algorithm : findSplitAfter;
        auto stdoutSearch = result.findSplitAfter(`"stdout":"`);
        if (stdoutSearch[0].length > 0 && stdoutSearch[1].length > 0) {
            auto stdoutEnd = stdoutSearch[1].findSplitAfter(`"`);
            if (stdoutEnd[0].length > 1) {
                string bootstrapScript = stdoutEnd[0][0..$-1];
                bootstrapScript = bootstrapScript.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\"", "\"").replace("\\\\", "\\");

                if (!dumpFile.empty) {
                    try {
                        std.file.write(dumpFile, bootstrapScript);
                        version(Posix) {
                            import core.sys.posix.sys.stat;
                            chmod(dumpFile.toStringz(), octal!755);
                        }
                        writefln("Bootstrap saved to %s", dumpFile);
                    } catch (Exception e) {
                        stderr.writefln("%sError: Could not write to %s: %s%s", RED, dumpFile, e.msg, RESET);
                        exit(1);
                    }
                } else {
                    write(bootstrapScript);
                }
            } else {
                stderr.writefln("%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s", RED, RESET);
                exit(1);
            }
        } else {
            stderr.writefln("%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s", RED, RESET);
            exit(1);
        }
        return;
    }

    if (!name.empty) {
        string json = format(`{"name":"%s"`, name);
        if (!ports.empty) json ~= format(`,"ports":[%s]`, ports);
        if (!type.empty) json ~= format(`,"service_type":"%s"`, type);
        if (!bootstrap.empty) {
            json ~= format(`,"bootstrap":"%s"`, escapeJson(bootstrap));
        }
        if (!bootstrapFile.empty) {
            if (exists(bootstrapFile)) {
                string bootCode = readText(bootstrapFile);
                json ~= format(`,"bootstrap_content":"%s"`, escapeJson(bootCode));
            } else {
                stderr.writefln("%sError: Bootstrap file not found: %s%s", RED, bootstrapFile, RESET);
                exit(1);
            }
        }
        if (!network.empty) json ~= format(`,"network":"%s"`, network);
        if (vcpu > 0) json ~= format(`,"vcpu":%d`, vcpu);
        json ~= buildInputFilesJson(inputFiles);
        json ~= "}";

        writefln("%sCreating service...%s", YELLOW, RESET);
        string authHeaders = buildAuthHeaders("POST", "/services", json, publicKey, secretKey);
        string cmd = format(`curl -s -X POST '%s/services' -H 'Content-Type: application/json' %s -d '%s'`, API_BASE, authHeaders, json);
        string response = execCurl(cmd);
        writeln(response);

        // Auto-set vault if -e or --env-file provided
        if (svcEnvs.length > 0 || !svcEnvFile.empty) {
            string serviceId = extractJsonField(response, "service_id");
            if (serviceId.empty) serviceId = extractJsonField(response, "id");
            if (!serviceId.empty) {
                string envContent = buildEnvContent(svcEnvs, svcEnvFile);
                if (execCurlPut(format("/services/%s/env", serviceId), envContent, publicKey, secretKey)) {
                    writefln("%sVault configured for service %s%s", GREEN, serviceId, RESET);
                } else {
                    stderr.writefln("%sWarning: Failed to set vault%s", YELLOW, RESET);
                }
            }
        }
        return;
    }

    stderr.writefln("%sError: Specify --name to create a service%s", RED, RESET);
    exit(1);
}

void openBrowser(string url) {
    version(linux) {
        executeShell("xdg-open \"" ~ url ~ "\" 2>/dev/null &");
    } else version(OSX) {
        executeShell("open \"" ~ url ~ "\"");
    } else version(Windows) {
        executeShell("start \"\" \"" ~ url ~ "\"");
    } else {
        stderr.writefln("%sError: Unsupported platform for browser opening%s", RED, RESET);
    }
}

string formatDuration(long totalMinutes) {
    long days = totalMinutes / (24 * 60);
    long hours = (totalMinutes % (24 * 60)) / 60;
    long minutes = totalMinutes % 60;

    if (days > 0) {
        return format("%dd %dh %dm", days, hours, minutes);
    } else if (hours > 0) {
        return format("%dh %dm", hours, minutes);
    } else {
        return format("%dm", minutes);
    }
}

void validateKey(string publicKey, string secretKey, bool extend) {
    import std.json;
    import std.datetime;

    string authHeaders = buildAuthHeaders("POST", "/keys/validate", "", publicKey, secretKey);
    string cmd = format(`curl -s -w '\n%%{http_code}' -X POST '%s/keys/validate' -H 'Content-Type: application/json' %s`, PORTAL_BASE, authHeaders);
    string response = execCurl(cmd);

    auto lines = response.split("\n");
    string body = lines.length > 1 ? lines[0..$-1].join("\n") : response;
    string statusCode = lines.length > 1 ? lines[$-1] : "200";

    JSONValue result;
    try {
        result = parseJSON(body);
    } catch (Exception e) {
        stderr.writefln("%sError parsing response: %s%s", RED, e.msg, RESET);
        exit(1);
    }

    if (statusCode[0] == '4' || statusCode[0] == '5') {
        // Invalid key
        writefln("%sInvalid%s", RED, RESET);
        if ("error" in result) {
            writefln("Reason: %s", result["error"].str);
        } else if ("message" in result) {
            writefln("Reason: %s", result["message"].str);
        }
        exit(1);
    }

    bool valid = result["valid"].type == JSONType.true_;
    bool expired = result["expired"].type == JSONType.true_;
    string publicKey = "public_key" in result ? result["public_key"].str : "";
    string tier = "tier" in result ? result["tier"].str : "";
    string status = "status" in result ? result["status"].str : "";

    if (expired) {
        // Expired key
        writefln("%sExpired%s", RED, RESET);
        writefln("Public Key: %s", publicKey);
        writefln("Tier: %s", tier);
        if ("expires_at" in result) {
            writefln("Expired: %s", result["expires_at"].str);
        }
        writefln("%sTo renew: Visit https://unsandbox.com/keys/extend%s", YELLOW, RESET);

        if (extend) {
            string extendURL = PORTAL_BASE ~ "/keys/extend?pk=" ~ publicKey;
            writefln("\n%sOpening browser to extend key...%s", GREEN, RESET);
            openBrowser(extendURL);
        }
        exit(1);
    }

    if (valid) {
        // Valid key
        writefln("%sValid%s", GREEN, RESET);
        writefln("Public Key: %s", publicKey);
        writefln("Tier: %s", tier);
        writefln("Status: %s", status);

        if ("expires_at" in result) {
            string expiresAt = result["expires_at"].str;
            writefln("Expires: %s", expiresAt);

            // Calculate time remaining (simplified - just show the date)
            // Full datetime parsing would require additional complexity
        }

        if ("rate_limit" in result && result["rate_limit"].type != JSONType.null_) {
            writefln("Rate Limit: %.0f req/min", result["rate_limit"].floating);
        }
        if ("burst" in result && result["burst"].type != JSONType.null_) {
            writefln("Burst: %.0f req", result["burst"].floating);
        }
        if ("concurrency" in result && result["concurrency"].type != JSONType.null_) {
            writefln("Concurrency: %.0f", result["concurrency"].floating);
        }

        if (extend) {
            string extendURL = PORTAL_BASE ~ "/keys/extend?pk=" ~ publicKey;
            writefln("\n%sOpening browser to extend key...%s", GREEN, RESET);
            openBrowser(extendURL);
        }
    } else {
        // Invalid key
        writefln("%sInvalid%s", RED, RESET);
        if ("error" in result) {
            writefln("Reason: %s", result["error"].str);
        }
        exit(1);
    }
}

int main(string[] args) {
    string publicKey = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string secretKey = environment.get("UNSANDBOX_SECRET_KEY", "");

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if (publicKey.empty) {
        publicKey = environment.get("UNSANDBOX_API_KEY", "");
    }

    if (args.length < 2) {
        stderr.writefln("Usage: %s [options] <source_file>", args[0]);
        stderr.writefln("       %s session [options]", args[0]);
        stderr.writefln("       %s service [options]", args[0]);
        stderr.writefln("       %s service env <action> <service_id> [options]", args[0]);
        stderr.writefln("       %s key [options]", args[0]);
        stderr.writeln("");
        stderr.writeln("Service env commands:");
        stderr.writeln("  env status <id>     Show vault status");
        stderr.writeln("  env set <id>        Set vault (-e KEY=VALUE or --env-file FILE)");
        stderr.writeln("  env export <id>     Export vault contents");
        stderr.writeln("  env delete <id>     Delete vault");
        return 1;
    }

    if (args[1] == "session") {
        bool list = false;
        string kill, shell, network;
        int vcpu = 0;
        bool tmux = false, screen = false;
        string[] inputFiles;

        for (size_t i = 2; i < args.length; i++) {
            if (args[i] == "--list") list = true;
            else if (args[i] == "--kill" && i+1 < args.length) kill = args[++i];
            else if (args[i] == "--shell" && i+1 < args.length) shell = args[++i];
            else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
            else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
            else if (args[i] == "--tmux") tmux = true;
            else if (args[i] == "--screen") screen = true;
            else if (args[i] == "-f" && i+1 < args.length) inputFiles ~= args[++i];
            else if (args[i] == "-k" && i+1 < args.length) publicKey = args[++i];
        }

        cmdSession(list, kill, shell, network, vcpu, tmux, screen, inputFiles, publicKey, secretKey);
        return 0;
    }

    if (args[1] == "service") {
        string name, ports, bootstrap, bootstrapFile, type;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, resize, execute, command, dumpBootstrap, dumpFile, network;
        int vcpu = 0;
        int resizeVcpu = 0;
        string[] inputFiles;
        string[] svcEnvs;
        string svcEnvFile;
        string envAction, envTarget;

        // Check for env subcommand
        if (args.length > 2 && args[2] == "env") {
            if (args.length > 3) envAction = args[3];
            if (args.length > 4 && !args[4].startsWith("-")) envTarget = args[4];
            for (size_t i = 5; i < args.length; i++) {
                if (args[i] == "-e" && i+1 < args.length) svcEnvs ~= args[++i];
                else if (args[i] == "--env-file" && i+1 < args.length) svcEnvFile = args[++i];
                else if (args[i] == "-k" && i+1 < args.length) publicKey = args[++i];
            }
            cmdService(name, ports, bootstrap, bootstrapFile, type, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey);
            return 0;
        }

        for (size_t i = 2; i < args.length; i++) {
            if (args[i] == "--name" && i+1 < args.length) name = args[++i];
            else if (args[i] == "--ports" && i+1 < args.length) ports = args[++i];
            else if (args[i] == "--bootstrap" && i+1 < args.length) bootstrap = args[++i];
            else if (args[i] == "--bootstrap-file" && i+1 < args.length) bootstrapFile = args[++i];
            else if (args[i] == "--type" && i+1 < args.length) type = args[++i];
            else if (args[i] == "--list") list = true;
            else if (args[i] == "--info" && i+1 < args.length) info = args[++i];
            else if (args[i] == "--logs" && i+1 < args.length) logs = args[++i];
            else if (args[i] == "--tail" && i+1 < args.length) tail = args[++i];
            else if (args[i] == "--freeze" && i+1 < args.length) sleep = args[++i];
            else if (args[i] == "--unfreeze" && i+1 < args.length) wake = args[++i];
            else if (args[i] == "--destroy" && i+1 < args.length) destroy = args[++i];
            else if (args[i] == "--resize" && i+1 < args.length) resize = args[++i];
            else if (args[i] == "--vcpu" && i+1 < args.length) resizeVcpu = to!int(args[++i]);
            else if (args[i] == "--execute" && i+1 < args.length) execute = args[++i];
            else if (args[i] == "--command" && i+1 < args.length) command = args[++i];
            else if (args[i] == "--dump-bootstrap" && i+1 < args.length) dumpBootstrap = args[++i];
            else if (args[i] == "--dump-file" && i+1 < args.length) dumpFile = args[++i];
            else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
            else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
            else if (args[i] == "-f" && i+1 < args.length) inputFiles ~= args[++i];
            else if (args[i] == "-e" && i+1 < args.length) svcEnvs ~= args[++i];
            else if (args[i] == "--env-file" && i+1 < args.length) svcEnvFile = args[++i];
            else if (args[i] == "-k" && i+1 < args.length) publicKey = args[++i];
        }

        cmdService(name, ports, bootstrap, bootstrapFile, type, list, info, logs, tail, sleep, wake, destroy, resize, resizeVcpu, execute, command, dumpBootstrap, dumpFile, network, vcpu, inputFiles, svcEnvs, svcEnvFile, envAction, envTarget, publicKey, secretKey);
        return 0;
    }

    if (args[1] == "key") {
        bool extend = false;

        for (size_t i = 2; i < args.length; i++) {
            if (args[i] == "--extend") extend = true;
            else if (args[i] == "-k" && i+1 < args.length) publicKey = args[++i];
        }

        if (publicKey.empty) {
            stderr.writefln("%sError: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY not set%s", RED, RESET);
            return 1;
        }

        validateKey(publicKey, secretKey, extend);
        return 0;
    }

    // Execute mode
    string[] envs;
    bool artifacts = false;
    string network, sourceFile;
    int vcpu = 0;

    for (size_t i = 1; i < args.length; i++) {
        if (args[i] == "-e" && i+1 < args.length) envs ~= args[++i];
        else if (args[i] == "-a") artifacts = true;
        else if (args[i] == "-n" && i+1 < args.length) network = args[++i];
        else if (args[i] == "-v" && i+1 < args.length) vcpu = to!int(args[++i]);
        else if (args[i] == "-k" && i+1 < args.length) publicKey = args[++i];
        else if (args[i].startsWith("-")) {
            stderr.writefln("%sUnknown option: %s%s", RED, args[i], RESET);
            return 1;
        }
        else sourceFile = args[i];
    }

    if (sourceFile.empty) {
        stderr.writefln("%sError: No source file specified%s", RED, RESET);
        return 1;
    }

    cmdExecute(sourceFile, envs, artifacts, network, vcpu, publicKey, secretKey);
    return 0;
}
