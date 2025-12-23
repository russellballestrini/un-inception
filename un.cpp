// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - Source code must be open source & freely distributed
//   FREEDOM  - Voluntary participation without corporate control
//   HARMONY  - Systems operating with minimal waste that self-renew
//   LOVE     - Individual rights protected while fostering cooperation
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
#include <sys/stat.h>

using namespace std;

const string API_BASE = "https://api.unsandbox.com";
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

void cmd_execute(const string& source_file, const vector<string>& envs, const vector<string>& files, bool artifacts, const string& network, int vcpu, const string& api_key) {
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

    string cmd = "curl -s -X POST '" + API_BASE + "/execute' "
                 "-H 'Content-Type: application/json' "
                 "-H 'Authorization: Bearer " + api_key + "' "
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

void cmd_session(bool list, const string& kill, const string& shell, const string& network, int vcpu, bool tmux, bool screen, const string& api_key) {
    if (list) {
        string cmd = "curl -s -X GET '" + API_BASE + "/sessions' -H 'Authorization: Bearer " + api_key + "'";
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!kill.empty()) {
        string cmd = "curl -s -X DELETE '" + API_BASE + "/sessions/" + kill + "' -H 'Authorization: Bearer " + api_key + "'";
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
    string cmd = "curl -s -X POST '" + API_BASE + "/sessions' "
                 "-H 'Content-Type: application/json' "
                 "-H 'Authorization: Bearer " + api_key + "' "
                 "-d '" + json.str() + "'";
    cout << exec_curl(cmd) << endl;
}

void cmd_service(const string& name, const string& ports, const string& bootstrap, bool list, const string& info, const string& logs, const string& tail, const string& sleep, const string& wake, const string& destroy, const string& network, int vcpu, const string& api_key) {
    if (list) {
        string cmd = "curl -s -X GET '" + API_BASE + "/services' -H 'Authorization: Bearer " + api_key + "'";
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!info.empty()) {
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + info + "' -H 'Authorization: Bearer " + api_key + "'";
        cout << exec_curl(cmd) << endl;
        return;
    }

    if (!logs.empty()) {
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + logs + "/logs' -H 'Authorization: Bearer " + api_key + "'";
        exec_curl(cmd);
        return;
    }

    if (!tail.empty()) {
        string cmd = "curl -s -X GET '" + API_BASE + "/services/" + tail + "/logs?lines=9000' -H 'Authorization: Bearer " + api_key + "'";
        exec_curl(cmd);
        return;
    }

    if (!sleep.empty()) {
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + sleep + "/sleep' -H 'Authorization: Bearer " + api_key + "'";
        exec_curl(cmd);
        cout << GREEN << "Service sleeping: " << sleep << RESET << endl;
        return;
    }

    if (!wake.empty()) {
        string cmd = "curl -s -X POST '" + API_BASE + "/services/" + wake + "/wake' -H 'Authorization: Bearer " + api_key + "'";
        exec_curl(cmd);
        cout << GREEN << "Service waking: " << wake << RESET << endl;
        return;
    }

    if (!destroy.empty()) {
        string cmd = "curl -s -X DELETE '" + API_BASE + "/services/" + destroy + "' -H 'Authorization: Bearer " + api_key + "'";
        exec_curl(cmd);
        cout << GREEN << "Service destroyed: " << destroy << RESET << endl;
        return;
    }

    if (!name.empty()) {
        ostringstream json;
        json << "{\"name\":\"" << name << "\"";
        if (!ports.empty()) json << ",\"ports\":[" << ports << "]";
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
        string cmd = "curl -s -X POST '" + API_BASE + "/services' "
                     "-H 'Content-Type: application/json' "
                     "-H 'Authorization: Bearer " + api_key + "' "
                     "-d '" + json.str() + "'";
        cout << exec_curl(cmd) << endl;
        return;
    }

    cerr << RED << "Error: Specify --name to create a service" << RESET << endl;
    exit(1);
}

int main(int argc, char* argv[]) {
    string api_key = getenv("UNSANDBOX_API_KEY") ? getenv("UNSANDBOX_API_KEY") : "";

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " [options] <source_file>" << endl;
        cerr << "       " << argv[0] << " session [options]" << endl;
        cerr << "       " << argv[0] << " service [options]" << endl;
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
            else if (arg == "-k" && i+1 < argc) api_key = argv[++i];
        }

        cmd_session(list, kill, shell, network, vcpu, tmux, screen, api_key);
        return 0;
    }

    if (cmd_type == "service") {
        string name, ports, bootstrap;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, network;
        int vcpu = 0;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--name" && i+1 < argc) name = argv[++i];
            else if (arg == "--ports" && i+1 < argc) ports = argv[++i];
            else if (arg == "--bootstrap" && i+1 < argc) bootstrap = argv[++i];
            else if (arg == "--list") list = true;
            else if (arg == "--info" && i+1 < argc) info = argv[++i];
            else if (arg == "--logs" && i+1 < argc) logs = argv[++i];
            else if (arg == "--tail" && i+1 < argc) tail = argv[++i];
            else if (arg == "--sleep" && i+1 < argc) sleep = argv[++i];
            else if (arg == "--wake" && i+1 < argc) wake = argv[++i];
            else if (arg == "--destroy" && i+1 < argc) destroy = argv[++i];
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "-k" && i+1 < argc) api_key = argv[++i];
        }

        cmd_service(name, ports, bootstrap, list, info, logs, tail, sleep, wake, destroy, network, vcpu, api_key);
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
        else if (arg == "-k" && i+1 < argc) api_key = argv[++i];
        else if (arg[0] != '-') source_file = arg;
    }

    if (source_file.empty()) {
        cerr << RED << "Error: No source file specified" << RESET << endl;
        return 1;
    }

    cmd_execute(source_file, envs, files, artifacts, network, vcpu, api_key);
    return 0;
}
