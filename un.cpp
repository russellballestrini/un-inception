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


// UN CLI - C++ Implementation (using curl subprocess for simplicity)
// Compile: g++ -o un_cpp un.cpp -std=c++17
// Usage:
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

string exec_curl(const string& cmd) {
    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) return "";
    char buffer[4096];
    string result;
    while (fgets(buffer, sizeof(buffer), pipe)) {
        result += buffer;
    }
    pclose(pipe);
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

void cmd_session(bool list, const string& kill, const string& shell, const string& network, int vcpu, bool tmux, bool screen, const string& public_key, const string& secret_key) {
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
    json << "}";

    cout << YELLOW << "Creating session..." << RESET << endl;
    string auth_headers = build_auth_headers("POST", "/sessions", json.str(), public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/sessions' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + json.str() + "'";
    cout << exec_curl(cmd) << endl;
}

void cmd_service(const string& name, const string& ports, const string& type, const string& bootstrap, bool list, const string& info, const string& logs, const string& tail, const string& sleep, const string& wake, const string& destroy, const string& execute, const string& command, const string& dump_bootstrap, const string& dump_file, const string& network, int vcpu, const string& public_key, const string& secret_key) {
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
        string auth_headers = build_auth_headers("POST", "/services/" + sleep + "/sleep", "", public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + sleep + "/sleep' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service sleeping: " << sleep << RESET << endl;
        return;
    }

    if (!wake.empty()) {
        string auth_headers = build_auth_headers("POST", "/services/" + wake + "/wake", "", public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + wake + "/wake' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service waking: " << wake << RESET << endl;
        return;
    }

    if (!destroy.empty()) {
        string auth_headers = build_auth_headers("DELETE", "/services/" + destroy, "", public_key, secret_key);
        string cmd = "curl -s -X DELETE '" + API_BASE + "/services/" + destroy + "' " + auth_headers;
        exec_curl(cmd);
        cout << GREEN << "Service destroyed: " << destroy << RESET << endl;
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
            struct stat st;
            if (stat(bootstrap.c_str(), &st) == 0) {
                string boot_code = read_file(bootstrap);
                json << ",\"bootstrap\":\"" << escape_json(boot_code) << "\"";
            } else {
                json << ",\"bootstrap\":\"" << escape_json(bootstrap) << "\"";
            }
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
        cout << exec_curl(cmd) << endl;
        return;
    }

    cerr << RED << "Error: Specify --name to create a service" << RESET << endl;
    exit(1);
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

    // Extract public_key
    string public_key;
    if (public_key_pos != string::npos) {
        public_key_pos += 14;
        size_t pk_end = result.find("\"", public_key_pos);
        public_key = result.substr(public_key_pos, pk_end - public_key_pos);
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
        if (!public_key.empty()) {
            cout << "Public Key: " << public_key << endl;
        }
        if (!tier.empty()) {
            cout << "Tier: " << tier << endl;
        }
        if (!expires_at.empty()) {
            cout << "Expires: " << expires_at << endl;
        }
    } else if (status == "expired") {
        cout << RED << "Expired" << RESET << endl;
        if (!public_key.empty()) {
            cout << "Public Key: " << public_key << endl;
        }
        if (!tier.empty()) {
            cout << "Tier: " << tier << endl;
        }
        if (!expires_at.empty()) {
            cout << "Expired: " << expires_at << endl;
        }
        cout << YELLOW << "To renew: Visit " << PORTAL_BASE << "/keys/extend" << RESET << endl;

        if (extend && !public_key.empty()) {
            string url = PORTAL_BASE + "/keys/extend?pk=" + public_key;
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
        cerr << "       " << argv[0] << " session [options]" << endl;
        cerr << "       " << argv[0] << " service [options]" << endl;
        cerr << "       " << argv[0] << " key [options]" << endl;
        return 1;
    }

    string cmd_type = argv[1];

    if (cmd_type == "session") {
        bool list = false;
        string kill, shell, network;
        int vcpu = 0;
        bool tmux = false, screen = false;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--list") list = true;
            else if (arg == "--kill" && i+1 < argc) kill = argv[++i];
            else if (arg == "--shell" && i+1 < argc) shell = argv[++i];
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "--tmux") tmux = true;
            else if (arg == "--screen") screen = true;
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_session(list, kill, shell, network, vcpu, tmux, screen, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "service") {
        string name, ports, type, bootstrap;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, execute, command, dump_bootstrap, dump_file, network;
        int vcpu = 0;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--name" && i+1 < argc) name = argv[++i];
            else if (arg == "--ports" && i+1 < argc) ports = argv[++i];
            else if (arg == "--type" && i+1 < argc) type = argv[++i];
            else if (arg == "--bootstrap" && i+1 < argc) bootstrap = argv[++i];
            else if (arg == "--list") list = true;
            else if (arg == "--info" && i+1 < argc) info = argv[++i];
            else if (arg == "--logs" && i+1 < argc) logs = argv[++i];
            else if (arg == "--tail" && i+1 < argc) tail = argv[++i];
            else if (arg == "--freeze" && i+1 < argc) sleep = argv[++i];
            else if (arg == "--unfreeze" && i+1 < argc) wake = argv[++i];
            else if (arg == "--destroy" && i+1 < argc) destroy = argv[++i];
            else if (arg == "--execute" && i+1 < argc) execute = argv[++i];
            else if (arg == "--command" && i+1 < argc) command = argv[++i];
            else if (arg == "--dump-bootstrap" && i+1 < argc) dump_bootstrap = argv[++i];
            else if (arg == "--dump-file" && i+1 < argc) dump_file = argv[++i];
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
        }

        cmd_service(name, ports, type, bootstrap, list, info, logs, tail, sleep, wake, destroy, execute, command, dump_bootstrap, dump_file, network, vcpu, public_key, secret_key);
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
        else if (arg[0] != '-') source_file = arg;
    }

    if (source_file.empty()) {
        cerr << RED << "Error: No source file specified" << RESET << endl;
        return 1;
    }

    cmd_execute(source_file, envs, files, artifacts, network, vcpu, public_key, secret_key);
    return 0;
}
