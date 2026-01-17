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


// UN CLI and Library - C++ Implementation (using curl subprocess for simplicity)
// Compile: g++ -o un_cpp un.cpp -std=c++17
//
// Library Usage (C++):
//   std::string execute(const std::string& language, const std::string& code,
//                      const std::string& public_key, const std::string& secret_key)
//   std::string execute_async(const std::string& language, const std::string& code,
//                            const std::string& public_key, const std::string& secret_key)
//   std::string get_job(const std::string& job_id,
//                      const std::string& public_key, const std::string& secret_key)
//   std::string wait_for_job(const std::string& job_id,
//                           const std::string& public_key, const std::string& secret_key)
//   std::string cancel_job(const std::string& job_id,
//                         const std::string& public_key, const std::string& secret_key)
//   std::string list_jobs(const std::string& public_key, const std::string& secret_key)
//   std::string get_languages(const std::string& public_key, const std::string& secret_key)
//   std::string detect_language(const std::string& filename)
//
// CLI Usage:
//   un_cpp script.py
//   un_cpp -e KEY=VALUE -f data.txt script.py
//   un_cpp session --list
//   un_cpp service --name web --ports 8080

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cstdlib>
#include <ctime>
#include <sys/stat.h>

using namespace std;

const string API_BASE = "https://api.unsandbox.com";
const string PORTAL_BASE = "https://unsandbox.com";
const int LANGUAGES_CACHE_TTL = 3600;  // 1 hour in seconds
const string BLUE = "\033[34m";
const string RED = "\033[31m";
const string GREEN = "\033[32m";
const string YELLOW = "\033[33m";
const string RESET = "\033[0m";

map<string, string> lang_map = {
    {".py", "python"}, {".js", "javascript"}, {".ts", "typescript"},
    {".rb", "ruby"}, {".php", "php"}, {".pl", "perl"}, {".lua", "lua"},
    {".sh", "bash"}, {".go", "go"}, {".rs", "rust"}, {".c", "c"},
    {".cpp", "cpp"}, {".cc", "cpp"}, {".d", "d"}, {".zig", "zig"},
    {".nim", "nim"}, {".v", "v"}
};

string detect_language(const string& filename) {
    size_t dot = filename.rfind('.');
    if (dot == string::npos) return "";
    string ext = filename.substr(dot);
    return (lang_map.count(ext)) ? lang_map[ext] : "";
}

string read_file(const string& filename) {
    ifstream f(filename);
    stringstream buf;
    buf << f.rdbuf();
    return buf.str();
}

string escape_json(const string& s) {
    ostringstream o;
    for (char c : s) {
        switch (c) {
            case '"': o << "\\\""; break;
            case '\\': o << "\\\\"; break;
            case '\n': o << "\\n"; break;
            case '\r': o << "\\r"; break;
            case '\t': o << "\\t"; break;
            default: o << c; break;
        }
    }
    return o.str();
}

// Base64 encoding
static const char b64_table[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

string base64_encode(const string& input) {
    string output;
    int val = 0, valb = -6;
    for (unsigned char c : input) {
        val = (val << 8) + c;
        valb += 8;
        while (valb >= 0) {
            output.push_back(b64_table[(val >> valb) & 0x3F]);
            valb -= 6;
        }
    }
    if (valb > -6) output.push_back(b64_table[((val << 8) >> (valb + 8)) & 0x3F]);
    while (output.size() % 4) output.push_back('=');
    return output;
}

string exec_curl(const string& cmd) {
    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) return "";
    char buffer[4096];
    string result;
    while (fgets(buffer, sizeof(buffer), pipe)) {
        result += buffer;
    }
    pclose(pipe);

    // Check for timestamp authentication errors
    if (result.find("timestamp") != string::npos &&
        (result.find("401") != string::npos || result.find("expired") != string::npos || result.find("invalid") != string::npos)) {
        cerr << RED << "Error: Request timestamp expired (must be within 5 minutes of server time)" << RESET << endl;
        cerr << YELLOW << "Your computer's clock may have drifted." << RESET << endl;
        cerr << "Check your system time and sync with NTP if needed:" << endl;
        cerr << "  Linux:   sudo ntpdate -s time.nist.gov" << endl;
        cerr << "  macOS:   sudo sntp -sS time.apple.com" << endl;
        cerr << "  Windows: w32tm /resync" << endl;
        exit(1);
    }

    return result;
}

string compute_hmac(const string& secret_key, const string& message) {
    string cmd = "echo -n '" + message + "' | openssl dgst -sha256 -hmac '" + secret_key + "' -hex | sed 's/.*= //'";
    string result = exec_curl(cmd);
    // Trim newline
    while (!result.empty() && (result.back() == '\n' || result.back() == '\r')) {
        result.pop_back();
    }
    return result;
}

string get_timestamp() {
    return to_string(time(nullptr));
}

string build_auth_headers(const string& method, const string& path, const string& body, const string& public_key, const string& secret_key) {
    if (secret_key.empty()) {
        // Legacy mode: use public_key as bearer token
        return "-H 'Authorization: Bearer " + public_key + "'";
    }

    // HMAC mode
    string timestamp = get_timestamp();
    string message = timestamp + ":" + method + ":" + path + ":" + body;
    string signature = compute_hmac(secret_key, message);

    return "-H 'Authorization: Bearer " + public_key + "' "
           "-H 'X-Timestamp: " + timestamp + "' "
           "-H 'X-Signature: " + signature + "'";
}

string build_env_content(const vector<string>& envs, const string& env_file) {
    ostringstream parts;
    if (!env_file.empty()) {
        string content = read_file(env_file);
        // Trim trailing whitespace
        while (!content.empty() && (content.back() == '\n' || content.back() == '\r' || content.back() == ' ')) {
            content.pop_back();
        }
        parts << content;
    }
    for (const auto& e : envs) {
        if (e.find('=') != string::npos) {
            if (parts.str().length() > 0) parts << "\n";
            parts << e;
        }
    }
    return parts.str();
}

string get_languages_cache_path() {
    const char* home = getenv("HOME");
    if (!home) home = ".";
    return string(home) + "/.unsandbox/languages.json";
}

vector<string> load_languages_cache() {
    vector<string> empty;
    string cache_path = get_languages_cache_path();

    struct stat st;
    if (stat(cache_path.c_str(), &st) != 0) {
        return empty;  // File doesn't exist
    }

    // Check if cache is fresh (< 1 hour old)
    time_t now = time(nullptr);
    if (now - st.st_mtime >= LANGUAGES_CACHE_TTL) {
        return empty;  // Cache expired
    }

    string content = read_file(cache_path);
    if (content.empty()) {
        return empty;
    }

    // Parse languages from JSON {"languages": [...], "timestamp": ...}
    vector<string> languages;
    size_t pos = content.find("\"languages\":");
    if (pos == string::npos) return empty;

    pos = content.find('[', pos);
    if (pos == string::npos) return empty;
    pos++;

    while (pos < content.length()) {
        // Skip whitespace
        while (pos < content.length() && (content[pos] == ' ' || content[pos] == '\n' || content[pos] == '\t')) pos++;
        if (content[pos] == ']') break;
        if (content[pos] == '"') {
            pos++;
            size_t end = content.find('"', pos);
            if (end != string::npos) {
                languages.push_back(content.substr(pos, end - pos));
                pos = end + 1;
            }
        }
        // Skip comma
        while (pos < content.length() && (content[pos] == ',' || content[pos] == ' ' || content[pos] == '\n' || content[pos] == '\t')) pos++;
    }

    return languages;
}

void save_languages_cache(const vector<string>& languages) {
    string cache_path = get_languages_cache_path();

    // Create directory if needed
    string dir = cache_path.substr(0, cache_path.rfind('/'));
    mkdir(dir.c_str(), 0755);

    ofstream f(cache_path);
    if (!f) return;

    f << "{\"languages\":[";
    for (size_t i = 0; i < languages.size(); i++) {
        if (i > 0) f << ",";
        f << "\"" << languages[i] << "\"";
    }
    f << "],\"timestamp\":" << time(nullptr) << "}";
    f.close();
}

string service_env_status(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/env";
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

bool service_env_set(const string& service_id, const string& env_content, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/env";
    string timestamp = get_timestamp();
    string message = timestamp + ":PUT:" + path + ":" + env_content;
    string signature = compute_hmac(secret_key, message);

    string cmd = "curl -s -X PUT '" + API_BASE + path + "' "
                 "-H 'Content-Type: text/plain' "
                 "-H 'Authorization: Bearer " + public_key + "' "
                 "-H 'X-Timestamp: " + timestamp + "' "
                 "-H 'X-Signature: " + signature + "' "
                 "-d '" + env_content + "'";
    exec_curl(cmd);
    return true;
}

string service_env_export(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/env/export";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

bool service_env_delete(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/env";
    string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
    string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    exec_curl(cmd);
    return true;
}

void cmd_service_env(const string& action, const string& target, const vector<string>& envs, const string& env_file, const string& public_key, const string& secret_key) {
    if (action == "status") {
        if (target.empty()) {
            cerr << RED << "Error: Usage: service env status <service_id>" << RESET << endl;
            exit(1);
        }
        string result = service_env_status(target, public_key, secret_key);
        bool has_env = result.find("\"has_env\":true") != string::npos;
        cout << "Service: " << target << endl;
        cout << "Has Vault: " << (has_env ? "Yes" : "No") << endl;
        if (has_env) {
            size_t size_pos = result.find("\"size\":");
            if (size_pos != string::npos) {
                size_pos += 7;
                size_t end = result.find_first_not_of("0123456789", size_pos);
                cout << "Size: " << result.substr(size_pos, end - size_pos) << " bytes" << endl;
            }
            size_t updated_pos = result.find("\"updated_at\":\"");
            if (updated_pos != string::npos) {
                updated_pos += 14;
                size_t end = result.find("\"", updated_pos);
                cout << "Updated: " << result.substr(updated_pos, end - updated_pos) << endl;
            }
        }
    } else if (action == "set") {
        if (target.empty()) {
            cerr << RED << "Error: Usage: service env set <service_id> [-e KEY=VAL] [--env-file FILE]" << RESET << endl;
            exit(1);
        }
        string env_content = build_env_content(envs, env_file);
        if (env_content.empty()) {
            cerr << RED << "Error: No environment variables specified. Use -e KEY=VAL or --env-file FILE" << RESET << endl;
            exit(1);
        }
        if (env_content.length() > 65536) {
            cerr << RED << "Error: Environment content exceeds 64KB limit" << RESET << endl;
            exit(1);
        }
        service_env_set(target, env_content, public_key, secret_key);
        cout << GREEN << "Vault updated for service: " << target << RESET << endl;
    } else if (action == "export") {
        if (target.empty()) {
            cerr << RED << "Error: Usage: service env export <service_id>" << RESET << endl;
            exit(1);
        }
        string result = service_env_export(target, public_key, secret_key);
        size_t content_pos = result.find("\"content\":\"");
        if (content_pos != string::npos) {
            content_pos += 11;
            size_t end = result.find("\"", content_pos);
            while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
            if (end != string::npos) {
                string content = result.substr(content_pos, end - content_pos);
                // Unescape
                size_t pos = 0;
                while ((pos = content.find("\\n", pos)) != string::npos) {
                    content.replace(pos, 2, "\n");
                }
                cout << content;
                if (!content.empty() && content.back() != '\n') cout << endl;
            }
        } else {
            cerr << YELLOW << "Vault is empty" << RESET << endl;
        }
    } else if (action == "delete") {
        if (target.empty()) {
            cerr << RED << "Error: Usage: service env delete <service_id>" << RESET << endl;
            exit(1);
        }
        service_env_delete(target, public_key, secret_key);
        cout << GREEN << "Vault deleted for service: " << target << RESET << endl;
    } else {
        cerr << RED << "Error: Unknown env action: " << action << ". Use status, set, export, or delete" << RESET << endl;
        exit(1);
    }
}

void cmd_execute(const string& source_file, const vector<string>& envs, const vector<string>& files, bool artifacts, const string& network, int vcpu, const string& public_key, const string& secret_key) {
    string lang = detect_language(source_file);
    if (lang.empty()) {
        cerr << RED << "Error: Cannot detect language" << RESET << endl;
        exit(1);
    }

    string code = read_file(source_file);
    ostringstream json;
    json << "{\"language\":\"" << lang << "\",\"code\":\"" << escape_json(code) << "\"";

    if (!envs.empty()) {
        json << ",\"env\":{";
        for (size_t i = 0; i < envs.size(); i++) {
            size_t eq = envs[i].find('=');
            if (eq != string::npos) {
                if (i > 0) json << ",";
                json << "\"" << envs[i].substr(0, eq) << "\":\""
                     << escape_json(envs[i].substr(eq + 1)) << "\"";
            }
        }
        json << "}";
    }

    if (artifacts) json << ",\"return_artifacts\":true";
    if (!network.empty()) json << ",\"network\":\"" << network << "\"";
    if (vcpu > 0) json << ",\"vcpu\":" << vcpu;
    json << "}";

    string auth_headers = build_auth_headers("POST", "/execute", json.str(), public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/execute' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + json.str() + "'";

    string result = exec_curl(cmd);

    // Simple parsing (stdout/stderr/exit_code)
    size_t stdout_pos = result.find("\"stdout\":\"");
    size_t stderr_pos = result.find("\"stderr\":\"");
    size_t exit_pos = result.find("\"exit_code\":");

    if (stdout_pos != string::npos) {
        stdout_pos += 10;
        size_t end = result.find("\"", stdout_pos);
        while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
        if (end != string::npos) {
            string out = result.substr(stdout_pos, end - stdout_pos);
            // Unescape
            size_t pos = 0;
            while ((pos = out.find("\\n", pos)) != string::npos) {
                out.replace(pos, 2, "\n");
            }
            cout << BLUE << out << RESET;
        }
    }

    if (stderr_pos != string::npos) {
        stderr_pos += 10;
        size_t end = result.find("\"", stderr_pos);
        while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
        if (end != string::npos) {
            string err = result.substr(stderr_pos, end - stderr_pos);
            size_t pos = 0;
            while ((pos = err.find("\\n", pos)) != string::npos) {
                err.replace(pos, 2, "\n");
            }
            cerr << RED << err << RESET;
        }
    }

    int exit_code = 1;
    if (exit_pos != string::npos) {
        exit_code = stoi(result.substr(exit_pos + 12));
    }
    exit(exit_code);
}

void cmd_session(bool list, const string& kill, const string& shell, const string& network, int vcpu, bool tmux, bool screen, const vector<string>& files, const string& public_key, const string& secret_key) {
    if (list) {
        string auth_headers = build_auth_headers("GET", "/sessions", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/sessions' " + auth_headers;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!kill.empty()) {
        string auth_headers = build_auth_headers("DELETE", "/sessions/" + kill, "", public_key, secret_key);
        string cmd = "curl -s -X DELETE '" + API_BASE + "/sessions/" + kill + "' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Session terminated: " << kill << RESET << endl;
        return;
    }

    ostringstream json;
    json << "{\"shell\":\"" << (shell.empty() ? "bash" : shell) << "\"";
    if (!network.empty()) json << ",\"network\":\"" << network << "\"";
    if (vcpu > 0) json << ",\"vcpu\":" << vcpu;
    if (tmux) json << ",\"persistence\":\"tmux\"";
    if (screen) json << ",\"persistence\":\"screen\"";

    // Input files
    if (!files.empty()) {
        json << ",\"input_files\":[";
        for (size_t i = 0; i < files.size(); i++) {
            if (i > 0) json << ",";
            ifstream file(files[i], ios::binary);
            if (!file) {
                cerr << RED << "Error: Input file not found: " << files[i] << RESET << endl;
                exit(1);
            }
            ostringstream content;
            content << file.rdbuf();
            string b64 = base64_encode(content.str());
            string filename = files[i].substr(files[i].find_last_of("/\\") + 1);
            json << "{\"filename\":\"" << filename << "\",\"content_base64\":\"" << b64 << "\"}";
        }
        json << "]";
    }

    json << "}";

    cout << YELLOW << "Creating session..." << RESET << endl;
    string auth_headers = build_auth_headers("POST", "/sessions", json.str(), public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/sessions' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + json.str() + "'";
    cout << exec_curl(cmd) << endl;
}

void cmd_service(const string& name, const string& ports, const string& type, const string& bootstrap, const string& bootstrap_file, const vector<string>& files, bool list, const string& info, const string& logs, const string& tail, const string& sleep, const string& wake, const string& destroy, const string& resize, const string& execute, const string& command, const string& dump_bootstrap, const string& dump_file, const string& network, int vcpu, const vector<string>& envs, const string& env_file, const string& env_action, const string& env_target, const string& public_key, const string& secret_key) {
    // Handle service env subcommand
    if (!env_action.empty()) {
        cmd_service_env(env_action, env_target, envs, env_file, public_key, secret_key);
        return;
    }

    if (list) {
        string auth_headers = build_auth_headers("GET", "/services", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/services' " + auth_headers;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!info.empty()) {
        string auth_headers = build_auth_headers("GET", "/services/" + info, "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + info + "' " + auth_headers;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!logs.empty()) {
        string auth_headers = build_auth_headers("GET", "/services/" + logs + "/logs", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + logs + "/logs' " + auth_headers;
        exec_curl(cmd);
        return;
    }

    if (!tail.empty()) {
        string auth_headers = build_auth_headers("GET", "/services/" + tail + "/logs?lines=9000", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + tail + "/logs?lines=9000' " + auth_headers;
        exec_curl(cmd);
        return;
    }

    if (!sleep.empty()) {
        string auth_headers = build_auth_headers("POST", "/services/" + sleep + "/freeze", "", public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + sleep + "/freeze' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service frozen: " << sleep << RESET << endl;
        return;
    }

    if (!wake.empty()) {
        string auth_headers = build_auth_headers("POST", "/services/" + wake + "/unfreeze", "", public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + wake + "/unfreeze' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service unfreezing: " << wake << RESET << endl;
        return;
    }

    if (!destroy.empty()) {
        string auth_headers = build_auth_headers("DELETE", "/services/" + destroy, "", public_key, secret_key);
        string cmd = "curl -s -X DELETE '" + API_BASE + "/services/" + destroy + "' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service destroyed: " << destroy << RESET << endl;
        return;
    }

    if (!resize.empty()) {
        if (vcpu <= 0) {
            cerr << RED << "Error: --resize requires -v <vcpu>" << RESET << endl;
            exit(1);
        }
        ostringstream json;
        json << "{\"vcpu\":" << vcpu << "}";
        string auth_headers = build_auth_headers("PATCH", "/services/" + resize, json.str(), public_key, secret_key);
        string cmd = "curl -s -X PATCH '" + API_BASE + "/services/" + resize + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " "
                     "-d '" + json.str() + "'";
        exec_curl(cmd);
        cout << GREEN << "Service resized to " << vcpu << " vCPU, " << (vcpu * 2) << " GB RAM" << RESET << endl;
        return;
    }

    if (!execute.empty()) {
        ostringstream json;
        json << "{\"command\":\"" << escape_json(command) << "\"}";
        string auth_headers = build_auth_headers("POST", "/services/" + execute + "/execute", json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + execute + "/execute' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " "
                     "-d '" + json.str() + "'";
        string result = exec_curl(cmd);

        size_t stdout_pos = result.find("\"stdout\":\"");
        size_t stderr_pos = result.find("\"stderr\":\"");

        if (stdout_pos != string::npos) {
            stdout_pos += 10;
            size_t end = result.find("\"", stdout_pos);
            while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
            if (end != string::npos) {
                string out = result.substr(stdout_pos, end - stdout_pos);
                size_t pos = 0;
                while ((pos = out.find("\\n", pos)) != string::npos) {
                    out.replace(pos, 2, "\n");
                }
                cout << BLUE << out << RESET;
            }
        }

        if (stderr_pos != string::npos) {
            stderr_pos += 10;
            size_t end = result.find("\"", stderr_pos);
            while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
            if (end != string::npos) {
                string err = result.substr(stderr_pos, end - stderr_pos);
                size_t pos = 0;
                while ((pos = err.find("\\n", pos)) != string::npos) {
                    err.replace(pos, 2, "\n");
                }
                cerr << RED << err << RESET;
            }
        }
        return;
    }

    if (!dump_bootstrap.empty()) {
        cerr << "Fetching bootstrap script from " << dump_bootstrap << "..." << endl;
        string json_body = "{\"command\":\"cat /tmp/bootstrap.sh\"}";
        string auth_headers = build_auth_headers("POST", "/services/" + dump_bootstrap + "/execute", json_body, public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + dump_bootstrap + "/execute' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " "
                     "-d '" + json_body + "'";
        string result = exec_curl(cmd);

        size_t stdout_pos = result.find("\"stdout\":\"");
        if (stdout_pos != string::npos) {
            stdout_pos += 10;
            size_t end = result.find("\"", stdout_pos);
            while (end > 0 && result[end-1] == '\\') end = result.find("\"", end+1);
            if (end != string::npos) {
                string bootstrap_script = result.substr(stdout_pos, end - stdout_pos);
                size_t pos = 0;
                while ((pos = bootstrap_script.find("\\n", pos)) != string::npos) {
                    bootstrap_script.replace(pos, 2, "\n");
                }

                if (!dump_file.empty()) {
                    ofstream outfile(dump_file);
                    if (outfile) {
                        outfile << bootstrap_script;
                        outfile.close();
                        chmod(dump_file.c_str(), 0755);
                        cout << "Bootstrap saved to " << dump_file << endl;
                    } else {
                        cerr << RED << "Error: Could not write to " << dump_file << RESET << endl;
                        exit(1);
                    }
                } else {
                    cout << bootstrap_script;
                }
            } else {
                cerr << RED << "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" << RESET << endl;
                exit(1);
            }
        } else {
            cerr << RED << "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" << RESET << endl;
            exit(1);
        }
        return;
    }

    if (!name.empty()) {
        ostringstream json;
        json << "{\"name\":\"" << name << "\"";
        if (!ports.empty()) json << ",\"ports\":[" << ports << "]";
        if (!type.empty()) json << ",\"service_type\":\"" << type << "\"";
        if (!bootstrap.empty()) {
            json << ",\"bootstrap\":\"" << escape_json(bootstrap) << "\"";
        }
        if (!bootstrap_file.empty()) {
            struct stat st;
            if (stat(bootstrap_file.c_str(), &st) == 0) {
                string boot_code = read_file(bootstrap_file);
                json << ",\"bootstrap_content\":\"" << escape_json(boot_code) << "\"";
            } else {
                cerr << RED << "Error: Bootstrap file not found: " << bootstrap_file << RESET << endl;
                exit(1);
            }
        }
        // Input files
        if (!files.empty()) {
            json << ",\"input_files\":[";
            for (size_t i = 0; i < files.size(); i++) {
                if (i > 0) json << ",";
                ifstream file(files[i], ios::binary);
                if (!file) {
                    cerr << RED << "Error: Input file not found: " << files[i] << RESET << endl;
                    exit(1);
                }
                ostringstream content;
                content << file.rdbuf();
                string b64 = base64_encode(content.str());
                string filename = files[i].substr(files[i].find_last_of("/\\") + 1);
                json << "{\"filename\":\"" << filename << "\",\"content_base64\":\"" << b64 << "\"}";
            }
            json << "]";
        }
        if (!network.empty()) json << ",\"network\":\"" << network << "\"";
        if (vcpu > 0) json << ",\"vcpu\":" << vcpu;
        json << "}";

        cout << YELLOW << "Creating service..." << RESET << endl;
        string auth_headers = build_auth_headers("POST", "/services", json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " "
                     "-d '" + json.str() + "'";
        string result = exec_curl(cmd);
        cout << result << endl;

        // Auto-set vault if env vars provided
        if (!envs.empty() || !env_file.empty()) {
            // Extract service ID from result
            size_t id_pos = result.find("\"id\":\"");
            if (id_pos != string::npos) {
                id_pos += 6;
                size_t id_end = result.find("\"", id_pos);
                if (id_end != string::npos) {
                    string service_id = result.substr(id_pos, id_end - id_pos);
                    string env_content = build_env_content(envs, env_file);
                    if (!env_content.empty() && env_content.length() <= 65536) {
                        service_env_set(service_id, env_content, public_key, secret_key);
                        cout << GREEN << "Vault configured with environment variables" << RESET << endl;
                    }
                }
            }
        }
        return;
    }

    cerr << RED << "Error: Specify --name to create a service" << RESET << endl;
    exit(1);
}

void cmd_languages(bool json_output, const string& public_key, const string& secret_key) {
    // Try cache first
    vector<string> names = load_languages_cache();

    if (names.empty()) {
        // Fetch from API
        string auth_headers = build_auth_headers("GET", "/languages", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/languages' " + auth_headers;
        string result = exec_curl(cmd);

        // Extract language names from response
        size_t pos = 0;
        string search = "\"name\":\"";
        while ((pos = result.find(search, pos)) != string::npos) {
            pos += search.length();
            size_t end = result.find("\"", pos);
            if (end != string::npos) {
                names.push_back(result.substr(pos, end - pos));
                pos = end;
            }
        }

        // Save to cache
        if (!names.empty()) {
            save_languages_cache(names);
        }
    }

    if (json_output) {
        cout << "[";
        for (size_t i = 0; i < names.size(); i++) {
            if (i > 0) cout << ",";
            cout << "\"" << names[i] << "\"";
        }
        cout << "]" << endl;
    } else {
        // Output one language per line
        for (const auto& name : names) {
            cout << name << endl;
        }
    }
}

void cmd_image(bool list, const string& info, const string& del, const string& lock, const string& unlock,
               const string& publish, const string& source_type, const string& visibility_id, const string& visibility,
               const string& spawn, const string& clone, const string& name, const string& ports,
               const string& public_key, const string& secret_key) {
    if (list) {
        string auth_headers = build_auth_headers("GET", "/images", "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + "/images' " + auth_headers;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!info.empty()) {
        string path = "/images/" + info;
        string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
        string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!del.empty()) {
        string path = "/images/" + del;
        string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
        string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Image deleted: " << del << RESET << endl;
        return;
    }

    if (!lock.empty()) {
        string path = "/images/" + lock + "/lock";
        string body = "{}";
        string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + body + "'";
        exec_curl(cmd);
        cout << GREEN << "Image locked: " << lock << RESET << endl;
        return;
    }

    if (!unlock.empty()) {
        string path = "/images/" + unlock + "/unlock";
        string body = "{}";
        string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + body + "'";
        exec_curl(cmd);
        cout << GREEN << "Image unlocked: " << unlock << RESET << endl;
        return;
    }

    if (!publish.empty()) {
        if (source_type.empty()) {
            cerr << RED << "Error: --publish requires --source-type (service or snapshot)" << RESET << endl;
            exit(1);
        }
        ostringstream json;
        json << "{\"source_type\":\"" << source_type << "\",\"source_id\":\"" << publish << "\"";
        if (!name.empty()) {
            json << ",\"name\":\"" << escape_json(name) << "\"";
        }
        json << "}";
        string path = "/images/publish";
        string auth_headers = build_auth_headers("POST", path, json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + json.str() + "'";
        cout << GREEN << "Image published" << RESET << endl;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!visibility_id.empty()) {
        if (visibility.empty()) {
            cerr << RED << "Error: --visibility requires visibility mode (private, unlisted, public)" << RESET << endl;
            exit(1);
        }
        ostringstream json;
        json << "{\"visibility\":\"" << visibility << "\"}";
        string path = "/images/" + visibility_id + "/visibility";
        string auth_headers = build_auth_headers("POST", path, json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + json.str() + "'";
        exec_curl(cmd);
        cout << GREEN << "Image visibility set to: " << visibility << RESET << endl;
        return;
    }

    if (!spawn.empty()) {
        ostringstream json;
        json << "{";
        bool has_field = false;
        if (!name.empty()) {
            json << "\"name\":\"" << escape_json(name) << "\"";
            has_field = true;
        }
        if (!ports.empty()) {
            if (has_field) json << ",";
            json << "\"ports\":[" << ports << "]";
        }
        json << "}";
        string path = "/images/" + spawn + "/spawn";
        string auth_headers = build_auth_headers("POST", path, json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + json.str() + "'";
        cout << GREEN << "Service spawned from image" << RESET << endl;
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!clone.empty()) {
        ostringstream json;
        json << "{";
        if (!name.empty()) {
            json << "\"name\":\"" << escape_json(name) << "\"";
        }
        json << "}";
        string path = "/images/" + clone + "/clone";
        string auth_headers = build_auth_headers("POST", path, json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " -d '" + json.str() + "'";
        cout << GREEN << "Image cloned" << RESET << endl;
        cout << exec_curl(cmd) << endl;
        return;
    }

    // Default: list images
    string auth_headers = build_auth_headers("GET", "/images", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/images' " + auth_headers;
    cout << exec_curl(cmd) << endl;
}

void cmd_validate_key(bool extend, const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("POST", "/keys/validate", "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + PORTAL_BASE + "/keys/validate' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers;

    string result = exec_curl(cmd);

    // Parse JSON response
    size_t status_pos = result.find("\"status\":\"");
    size_t public_key_pos = result.find("\"public_key\":\"");
    size_t tier_pos = result.find("\"tier\":\"");
    size_t expires_pos = result.find("\"expires_at\":\"");

    if (status_pos == string::npos) {
        cerr << RED << "Error: Invalid API response" << RESET << endl;
        exit(1);
    }

    // Extract status
    status_pos += 10;
    size_t status_end = result.find("\"", status_pos);
    string status = result.substr(status_pos, status_end - status_pos);

    // Extract public_key from response
    string resp_public_key;
    if (public_key_pos != string::npos) {
        public_key_pos += 14;
        size_t pk_end = result.find("\"", public_key_pos);
        resp_public_key = result.substr(public_key_pos, pk_end - public_key_pos);
    }

    // Extract tier
    string tier;
    if (tier_pos != string::npos) {
        tier_pos += 8;
        size_t tier_end = result.find("\"", tier_pos);
        tier = result.substr(tier_pos, tier_end - tier_pos);
    }

    // Extract expires_at
    string expires_at;
    if (expires_pos != string::npos) {
        expires_pos += 14;
        size_t expires_end = result.find("\"", expires_pos);
        expires_at = result.substr(expires_pos, expires_end - expires_pos);
    }

    if (status == "valid") {
        cout << GREEN << "Valid" << RESET << endl;
        if (!resp_public_key.empty()) {
            cout << "Public Key: " << resp_public_key << endl;
        }
        if (!tier.empty()) {
            cout << "Tier: " << tier << endl;
        }
        if (!expires_at.empty()) {
            cout << "Expires: " << expires_at << endl;
        }
    } else if (status == "expired") {
        cout << RED << "Expired" << RESET << endl;
        if (!resp_public_key.empty()) {
            cout << "Public Key: " << resp_public_key << endl;
        }
        if (!tier.empty()) {
            cout << "Tier: " << tier << endl;
        }
        if (!expires_at.empty()) {
            cout << "Expired: " << expires_at << endl;
        }
        cout << YELLOW << "To renew: Visit " << PORTAL_BASE << "/keys/extend" << RESET << endl;

        if (extend && !resp_public_key.empty()) {
            string url = PORTAL_BASE + "/keys/extend?pk=" + resp_public_key;
            string browser_cmd = "xdg-open '" + url + "' 2>/dev/null || open '" + url + "' 2>/dev/null";
            system(browser_cmd.c_str());
        }
    } else {
        cout << RED << "Invalid" << RESET << endl;
    }
}

int main(int argc, char* argv[]) {
    string public_key = getenv("UNSANDBOX_PUBLIC_KEY") ? getenv("UNSANDBOX_PUBLIC_KEY") : "";
    string secret_key = getenv("UNSANDBOX_SECRET_KEY") ? getenv("UNSANDBOX_SECRET_KEY") : "";

    // Fall back to UNSANDBOX_API_KEY for backwards compatibility
    if (public_key.empty()) {
        public_key = getenv("UNSANDBOX_API_KEY") ? getenv("UNSANDBOX_API_KEY") : "";
    }

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " [options] <source_file>" << endl;
        cerr << "       " << argv[0] << " languages [--json]" << endl;
        cerr << "       " << argv[0] << " session [options]" << endl;
        cerr << "       " << argv[0] << " service [options]" << endl;
        cerr << "       " << argv[0] << " image [options]" << endl;
        cerr << "       " << argv[0] << " key [options]" << endl;
        return 1;
    }

    string cmd_type = argv[1];

    if (cmd_type == "image") {
        bool list = false;
        string info, del, lock, unlock, publish, source_type, visibility_id, visibility;
        string spawn, clone, name, ports;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--list" || arg == "-l") list = true;
            else if (arg == "--info" && i+1 < argc) info = argv[++i];
            else if (arg == "--delete" && i+1 < argc) del = argv[++i];
            else if (arg == "--lock" && i+1 < argc) lock = argv[++i];
            else if (arg == "--unlock" && i+1 < argc) unlock = argv[++i];
            else if (arg == "--publish" && i+1 < argc) publish = argv[++i];
            else if (arg == "--source-type" && i+1 < argc) source_type = argv[++i];
            else if (arg == "--visibility" && i+2 < argc) {
                visibility_id = argv[++i];
                visibility = argv[++i];
            }
            else if (arg == "--spawn" && i+1 < argc) spawn = argv[++i];
            else if (arg == "--clone" && i+1 < argc) clone = argv[++i];
            else if (arg == "--name" && i+1 < argc) name = argv[++i];
            else if (arg == "--ports" && i+1 < argc) ports = argv[++i];
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_image(list, info, del, lock, unlock, publish, source_type, visibility_id, visibility, spawn, clone, name, ports, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "session") {
        bool list = false;
        string kill, shell, network;
        int vcpu = 0;
        bool tmux = false, screen = false;
        vector<string> files;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--list") list = true;
            else if (arg == "--kill" && i+1 < argc) kill = argv[++i];
            else if (arg == "--shell" && i+1 < argc) shell = argv[++i];
            else if (arg == "-f" && i+1 < argc) files.push_back(argv[++i]);
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "--tmux") tmux = true;
            else if (arg == "--screen") screen = true;
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_session(list, kill, shell, network, vcpu, tmux, screen, files, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "service") {
        string name, ports, type, bootstrap, bootstrap_file;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, resize, execute, command, dump_bootstrap, dump_file, network;
        int vcpu = 0;
        vector<string> files;
        vector<string> envs;
        string env_file, env_action, env_target;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--name" && i+1 < argc) name = argv[++i];
            else if (arg == "--ports" && i+1 < argc) ports = argv[++i];
            else if (arg == "--type" && i+1 < argc) type = argv[++i];
            else if (arg == "--bootstrap" && i+1 < argc) bootstrap = argv[++i];
            else if (arg == "--bootstrap-file" && i+1 < argc) bootstrap_file = argv[++i];
            else if (arg == "-f" && i+1 < argc) files.push_back(argv[++i]);
            else if (arg == "-e" && i+1 < argc) envs.push_back(argv[++i]);
            else if (arg == "--env-file" && i+1 < argc) env_file = argv[++i];
            else if (arg == "env" && env_action.empty()) {
                // service env <action> <target>
                if (i+1 < argc && string(argv[i+1])[0] != '-') {
                    env_action = argv[++i];
                    if (i+1 < argc && string(argv[i+1])[0] != '-') {
                        env_target = argv[++i];
                    }
                }
            }
            else if (arg == "--list") list = true;
            else if (arg == "--info" && i+1 < argc) info = argv[++i];
            else if (arg == "--logs" && i+1 < argc) logs = argv[++i];
            else if (arg == "--tail" && i+1 < argc) tail = argv[++i];
            else if (arg == "--freeze" && i+1 < argc) sleep = argv[++i];
            else if (arg == "--unfreeze" && i+1 < argc) wake = argv[++i];
            else if (arg == "--destroy" && i+1 < argc) destroy = argv[++i];
            else if (arg == "--resize" && i+1 < argc) resize = argv[++i];
            else if (arg == "--execute" && i+1 < argc) execute = argv[++i];
            else if (arg == "--command" && i+1 < argc) command = argv[++i];
            else if (arg == "--dump-bootstrap" && i+1 < argc) dump_bootstrap = argv[++i];
            else if (arg == "--dump-file" && i+1 < argc) dump_file = argv[++i];
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_service(name, ports, type, bootstrap, bootstrap_file, files, list, info, logs, tail, sleep, wake, destroy, resize, execute, command, dump_bootstrap, dump_file, network, vcpu, envs, env_file, env_action, env_target, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "key") {
        bool extend = false;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--extend") extend = true;
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_validate_key(extend, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "languages") {
        bool json_output = false;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--json") json_output = true;
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_languages(json_output, public_key, secret_key);
        return 0;
    }

    // Execute mode
    vector<string> envs, files;
    bool artifacts = false;
    string network, source_file;
    int vcpu = 0;

    for (int i = 1; i < argc; i++) {
        string arg = argv[i];
        if (arg == "-e" && i+1 < argc) envs.push_back(argv[++i]);
        else if (arg == "-f" && i+1 < argc) files.push_back(argv[++i]);
        else if (arg == "-a") artifacts = true;
        else if (arg == "-n" && i+1 < argc) network = argv[++i];
        else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
        else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        else if (arg[0] == '-') {
            cerr << RED << "Unknown option: " << arg << RESET << endl;
            return 1;
        }
        else source_file = arg;
    }

    if (source_file.empty()) {
        cerr << RED << "Error: No source file specified" << RESET << endl;
        return 1;
    }

    cmd_execute(source_file, envs, files, artifacts, network, vcpu, public_key, secret_key);
    return 0;
}
