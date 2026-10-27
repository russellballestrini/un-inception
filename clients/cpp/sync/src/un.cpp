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
#include <cstdint>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <cctype>
#include <iterator>

using namespace std;

// API base URL: UNSANDBOX_API_URL (default https://api.unsandbox.com, no trailing slash)
static string resolve_api_base() {
    const char* env = getenv("UNSANDBOX_API_URL");
    string base = (env && *env) ? env : "https://api.unsandbox.com";
    while (!base.empty() && base.back() == '/') base.pop_back();
    return base;
}
const string API_BASE = resolve_api_base();
// Portal base URL: UNSANDBOX_PORTAL_URL (default https://unsandbox.com, no trailing slash)
static string resolve_portal_base() {
    const char* env = getenv("UNSANDBOX_PORTAL_URL");
    string base = (env && *env) ? env : "https://unsandbox.com";
    while (!base.empty() && base.back() == '/') base.pop_back();
    return base;
}
const string PORTAL_BASE = resolve_portal_base();
const int LANGUAGES_CACHE_TTL = 3600;  // 1 hour in seconds

// Target OS (--os): validates a name case-insensitively into its canonical
// form, "linux" or "freebsd". Returns false with err set for anything else.
bool normalize_os(const string& value, string& out, string& err) {
    string lower;
    for (char c : value) lower += (char)tolower((unsigned char)c);
    if (lower == "linux" || lower == "freebsd") {
        out = lower;
        return true;
    }
    if (value.empty()) err = "--os requires a value: linux or freebsd";
    else err = "invalid --os '" + value + "' (expected linux or freebsd)";
    return false;
}

// CLI --os value, already normalized ("" = flag not given, field omitted).
static string TARGET_OS;

// JSON fragment ",\"os\":\"...\"" for a request body, "" when os is empty.
// Returns false with err set when os is not linux or freebsd.
bool os_json_field(const string& os, string& field, string& err) {
    field.clear();
    if (os.empty()) return true;
    string normalized;
    if (!normalize_os(os, normalized, err)) return false;
    field = ",\"os\":\"" + normalized + "\"";
    return true;
}
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

// Load a row from an accounts.csv file (format: public_key,secret_key per line).
// Lines starting with '#' and blank lines are skipped. Returns the Nth data row.
pair<string,string> loadAccountsCSV(const string& path, int index) {
    ifstream f(path);
    if (!f) return {"", ""};
    string line;
    int row = 0;
    while (getline(f, line)) {
        // Trim trailing carriage return
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (line.empty() || line[0] == '#') continue;
        if (row == index) {
            size_t comma = line.find(',');
            if (comma == string::npos) return {"", ""};
            // Extra columns (e.g. a label) follow the secret; stop at the next comma.
            size_t end = line.find(',', comma + 1);
            return {line.substr(0, comma), line.substr(comma + 1, end == string::npos ? string::npos : end - comma - 1)};
        }
        row++;
    }
    return {"", ""};
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

// Defined below; used by the sudo OTP retry helpers.
string build_auth_headers(const string& method, const string& path, const string& body, const string& public_key, const string& secret_key);

// Execute curl and get HTTP status code
pair<string, int> exec_curl_with_status(const string& cmd) {
    // Modify cmd to include status code output
    string full_cmd = cmd + " -w '\\n%{http_code}'";
    string result = exec_curl(full_cmd);

    // Extract status code from end of response
    size_t last_newline = result.rfind('\n');
    if (last_newline != string::npos && last_newline > 0) {
        // Find the status code after the last newline
        size_t status_start = last_newline + 1;
        // Trim any trailing whitespace
        while (!result.empty() && (result.back() == '\n' || result.back() == '\r' || result.back() == ' ')) {
            result.pop_back();
        }
        // Now find the status code at the end
        size_t end = result.length();
        size_t start = result.rfind('\n');
        if (start == string::npos) start = 0;
        else start++;

        string status_str = result.substr(start);
        int status = 0;
        try {
            status = stoi(status_str);
        } catch (...) {
            status = 0;
        }
        string body = result.substr(0, start > 0 ? start - 1 : 0);
        return {body, status};
    }
    return {result, 0};
}

// Extract challenge_id from JSON response
string extract_challenge_id(const string& response) {
    size_t pos = response.find("\"challenge_id\":\"");
    if (pos == string::npos) return "";
    pos += 16; // Length of "challenge_id":"
    size_t end = response.find("\"", pos);
    if (end == string::npos) return "";
    return response.substr(pos, end - pos);
}

// Handle 428 sudo OTP challenge - prompts user for OTP and retries request
bool handle_sudo_challenge(const string& method, const string& path, const string& body,
                           const string& public_key, const string& secret_key, const string& response) {
    // Extract challenge_id from response
    string challenge_id = extract_challenge_id(response);

    cerr << YELLOW << "Confirmation required. Check your email for a one-time code." << RESET << endl;
    cerr << "Enter OTP: ";

    string otp;
    if (!getline(cin, otp)) {
        cerr << RED << "Error: Failed to read OTP" << RESET << endl;
        return false;
    }

    // Trim whitespace
    while (!otp.empty() && (otp.back() == '\n' || otp.back() == '\r' || otp.back() == ' ')) {
        otp.pop_back();
    }
    while (!otp.empty() && (otp.front() == ' ')) {
        otp.erase(0, 1);
    }

    if (otp.empty()) {
        cerr << RED << "Error: Operation cancelled" << RESET << endl;
        return false;
    }

    // Retry the request with sudo headers
    string auth_headers = build_auth_headers(method, path, body, public_key, secret_key);
    auth_headers += " -H 'X-Sudo-OTP: " + otp + "'";
    if (!challenge_id.empty()) {
        auth_headers += " -H 'X-Sudo-Challenge: " + challenge_id + "'";
    }

    string cmd;
    if (method == "DELETE") {
        cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    } else if (method == "POST") {
        cmd = "curl -s -X POST '" + API_BASE + path + "' "
              "-H 'Content-Type: application/json' "
              + auth_headers + " "
              "-d '" + body + "'";
    } else {
        cmd = "curl -s -X " + method + " '" + API_BASE + path + "' " + auth_headers;
    }

    auto [retry_response, status] = exec_curl_with_status(cmd);

    if (status >= 200 && status < 300) {
        cout << GREEN << "Operation completed successfully" << RESET << endl;
        return true;
    }

    // Extract error message if available
    size_t error_pos = retry_response.find("\"error\":\"");
    if (error_pos != string::npos) {
        error_pos += 9;
        size_t error_end = retry_response.find("\"", error_pos);
        if (error_end != string::npos) {
            cerr << RED << "Error: " << retry_response.substr(error_pos, error_end - error_pos) << RESET << endl;
        } else {
            cerr << RED << "Error: " << retry_response << RESET << endl;
        }
    } else {
        cerr << RED << "Error: HTTP " << status << RESET << endl;
        cerr << retry_response << endl;
    }
    return false;
}

// Execute a destructive operation that may require sudo OTP confirmation
bool exec_destructive_curl(const string& method, const string& path, const string& body,
                           const string& public_key, const string& secret_key, const string& success_msg) {
    string auth_headers = build_auth_headers(method, path, body, public_key, secret_key);

    string cmd;
    if (method == "DELETE") {
        cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    } else if (method == "POST" && !body.empty()) {
        cmd = "curl -s -X POST '" + API_BASE + path + "' "
              "-H 'Content-Type: application/json' "
              + auth_headers + " "
              "-d '" + body + "'";
    } else if (method == "POST") {
        cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    } else {
        cmd = "curl -s -X " + method + " '" + API_BASE + path + "' " + auth_headers;
    }

    auto [response, status] = exec_curl_with_status(cmd);

    // Handle 428 sudo challenge
    if (status == 428) {
        return handle_sudo_challenge(method, path, body, public_key, secret_key, response);
    }

    if (status >= 200 && status < 300) {
        if (!success_msg.empty()) {
            cout << GREEN << success_msg << RESET << endl;
        }
        return true;
    }

    if (status == 404) {
        cerr << RED << "Error: Not found" << RESET << endl;
    } else {
        cerr << RED << "Error: HTTP " << status << RESET << endl;
        if (!response.empty()) cerr << response << endl;
    }
    return false;
}

// Self-contained SHA-256 (FIPS 180-4) for in-process HMAC. No external
// dependency, so the documented single-file compile line still works.
static const uint32_t SHA256_K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

static inline uint32_t sha256_rotr(uint32_t x, unsigned n) {
    return (x >> n) | (x << (32 - n));
}

static string sha256_raw(const string& data) {
    uint32_t h[8] = {
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
    };

    string msg = data;
    uint64_t bit_len = static_cast<uint64_t>(data.size()) * 8;
    msg.push_back(static_cast<char>(0x80));
    while (msg.size() % 64 != 56) msg.push_back('\0');
    for (int i = 7; i >= 0; --i) msg.push_back(static_cast<char>((bit_len >> (i * 8)) & 0xff));

    for (size_t off = 0; off < msg.size(); off += 64) {
        uint32_t w[64];
        for (int i = 0; i < 16; ++i) {
            w[i] = (static_cast<uint32_t>(static_cast<unsigned char>(msg[off + i * 4])) << 24) |
                   (static_cast<uint32_t>(static_cast<unsigned char>(msg[off + i * 4 + 1])) << 16) |
                   (static_cast<uint32_t>(static_cast<unsigned char>(msg[off + i * 4 + 2])) << 8) |
                   static_cast<uint32_t>(static_cast<unsigned char>(msg[off + i * 4 + 3]));
        }
        for (int i = 16; i < 64; ++i) {
            uint32_t s0 = sha256_rotr(w[i - 15], 7) ^ sha256_rotr(w[i - 15], 18) ^ (w[i - 15] >> 3);
            uint32_t s1 = sha256_rotr(w[i - 2], 17) ^ sha256_rotr(w[i - 2], 19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16] + s0 + w[i - 7] + s1;
        }

        uint32_t a = h[0], b = h[1], c = h[2], d = h[3], e = h[4], f = h[5], g = h[6], hh = h[7];
        for (int i = 0; i < 64; ++i) {
            uint32_t S1 = sha256_rotr(e, 6) ^ sha256_rotr(e, 11) ^ sha256_rotr(e, 25);
            uint32_t ch = (e & f) ^ (~e & g);
            uint32_t t1 = hh + S1 + ch + SHA256_K[i] + w[i];
            uint32_t S0 = sha256_rotr(a, 2) ^ sha256_rotr(a, 13) ^ sha256_rotr(a, 22);
            uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
            uint32_t t2 = S0 + maj;
            hh = g; g = f; f = e; e = d + t1;
            d = c; c = b; b = a; a = t1 + t2;
        }
        h[0] += a; h[1] += b; h[2] += c; h[3] += d;
        h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
    }

    string out;
    out.reserve(32);
    for (int i = 0; i < 8; ++i) {
        out.push_back(static_cast<char>((h[i] >> 24) & 0xff));
        out.push_back(static_cast<char>((h[i] >> 16) & 0xff));
        out.push_back(static_cast<char>((h[i] >> 8) & 0xff));
        out.push_back(static_cast<char>(h[i] & 0xff));
    }
    return out;
}

// HMAC-SHA256 (RFC 2104) computed in-process, returned as lowercase hex.
// Never shell out for this: a subprocess would expose secret_key in argv (ps)
// and interpolate message into a shell.
string compute_hmac(const string& secret_key, const string& message) {
    const size_t block_size = 64;
    string key = secret_key.size() > block_size ? sha256_raw(secret_key) : secret_key;
    key.resize(block_size, '\0');

    string ipad(block_size, '\0'), opad(block_size, '\0');
    for (size_t i = 0; i < block_size; ++i) {
        ipad[i] = static_cast<char>(static_cast<unsigned char>(key[i]) ^ 0x36);
        opad[i] = static_cast<char>(static_cast<unsigned char>(key[i]) ^ 0x5c);
    }

    string mac = sha256_raw(opad + sha256_raw(ipad + message));

    static const char hex_chars[] = "0123456789abcdef";
    string result;
    result.reserve(mac.size() * 2);
    for (unsigned char c : mac) {
        result.push_back(hex_chars[c >> 4]);
        result.push_back(hex_chars[c & 0x0f]);
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
    // Server verifies against our path without its query string (conn.request_path)
    string message = timestamp + ":" + method + ":" + path.substr(0, path.find('?')) + ":" + body;
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

void set_unfreeze_on_demand(const string& service_id, bool enabled, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id;
    string enabled_str = enabled ? "true" : "false";
    string body = "{\"unfreeze_on_demand\":" + enabled_str + "}";
    string auth_headers = build_auth_headers("PATCH", path, body, public_key, secret_key);
    string cmd = "curl -s -X PATCH '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + body + "'";
    exec_curl(cmd);
    cout << GREEN << "Service unfreeze_on_demand set to " << enabled_str << RESET << endl;
}

// ============================================================================
// Library Functions for C++ SDK (matching C reference un.h)
// ============================================================================

const string SDK_VERSION = "4.2.0";

// Execute code synchronously
// os: optional target OS, "linux" or "freebsd" ("" = omitted, API default linux)
string execute(const string& language, const string& code, const string& public_key, const string& secret_key, const string& os = "") {
    string os_field, os_err;
    if (!os_json_field(os, os_field, os_err)) return "{\"error\":\"" + escape_json(os_err) + "\"}";
    string body = "{\"language\":\"" + escape_json(language) + "\",\"code\":\"" + escape_json(code) + "\"" + os_field + "}";
    string auth_headers = build_auth_headers("POST", "/execute", body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/execute' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + body + "'";
    return exec_curl(cmd);
}

// Execute code asynchronously (returns job_id)
string execute_async(const string& language, const string& code, const string& public_key, const string& secret_key, const string& os = "") {
    string os_field, os_err;
    if (!os_json_field(os, os_field, os_err)) return "{\"error\":\"" + escape_json(os_err) + "\"}";
    string body = "{\"language\":\"" + escape_json(language) + "\",\"code\":\"" + escape_json(code) + "\",\"async\":true" + os_field + "}";
    string auth_headers = build_auth_headers("POST", "/execute", body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/execute' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " "
                 "-d '" + body + "'";
    return exec_curl(cmd);
}

// Get job status
string get_job(const string& job_id, const string& public_key, const string& secret_key) {
    string path = "/jobs/" + job_id;
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

// Wait for job completion
string wait_for_job(const string& job_id, const string& public_key, const string& secret_key) {
    const int poll_delays[] = {300, 450, 700, 900, 650, 1600, 2000};
    const int poll_count = 7;
    int delay_idx = 0;

    while (true) {
        string result = get_job(job_id, public_key, secret_key);
        if (result.find("\"status\":\"completed\"") != string::npos ||
            result.find("\"status\":\"failed\"") != string::npos ||
            result.find("\"status\":\"timeout\"") != string::npos ||
            result.find("\"status\":\"cancelled\"") != string::npos) {
            return result;
        }

        usleep(poll_delays[delay_idx % poll_count] * 1000);
        if (delay_idx < poll_count - 1) delay_idx++;
    }
}

// Cancel a job
string cancel_job(const string& job_id, const string& public_key, const string& secret_key) {
    string path = "/jobs/" + job_id + "/cancel";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

// List all jobs
string list_jobs(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("GET", "/jobs", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/jobs' " + auth_headers;
    return exec_curl(cmd);
}

// Get supported languages
string get_languages(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("GET", "/languages", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/languages' " + auth_headers;
    return exec_curl(cmd);
}

// Session functions
string session_list(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("GET", "/sessions", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/sessions' " + auth_headers;
    return exec_curl(cmd);
}

string session_get(const string& session_id, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id;
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string session_create(const string& shell, const string& network, const string& public_key, const string& secret_key, const string& os = "") {
    string os_field, os_err;
    if (!os_json_field(os, os_field, os_err)) return "{\"error\":\"" + escape_json(os_err) + "\"}";
    string body = "{\"shell\":\"" + (shell.empty() ? "bash" : shell) + "\"";
    if (!network.empty()) body += ",\"network\":\"" + network + "\"";
    body += os_field;
    body += "}";
    string auth_headers = build_auth_headers("POST", "/sessions", body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/sessions' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string session_destroy(const string& session_id, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id;
    string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
    string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string session_freeze(const string& session_id, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/freeze";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string session_unfreeze(const string& session_id, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/unfreeze";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string session_boost(const string& session_id, int vcpu, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/boost";
    string body = vcpu > 0 ? "{\"vcpu\":" + to_string(vcpu) + "}" : "{}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string session_unboost(const string& session_id, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/unboost";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string session_execute(const string& session_id, const string& command, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/shell";
    string body = "{\"command\":\"" + escape_json(command) + "\"}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

// Service functions
string service_list(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("GET", "/services", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/services' " + auth_headers;
    return exec_curl(cmd);
}

string service_get(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id;
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_create(const string& name, const string& ports, const string& bootstrap, const string& network, const string& public_key, const string& secret_key, const string& os = "") {
    string os_field, os_err;
    if (!os_json_field(os, os_field, os_err)) return "{\"error\":\"" + escape_json(os_err) + "\"}";
    string body = "{\"name\":\"" + escape_json(name) + "\"";
    if (!ports.empty()) body += ",\"ports\":\"" + ports + "\"";
    if (!bootstrap.empty()) body += ",\"bootstrap\":\"" + escape_json(bootstrap) + "\"";
    if (!network.empty()) body += ",\"network\":\"" + network + "\"";
    body += os_field;
    body += "}";
    string auth_headers = build_auth_headers("POST", "/services", body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/services' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string service_destroy(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id;
    string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
    string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_freeze(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/freeze";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_unfreeze(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/unfreeze";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_lock(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/lock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_unlock(const string& service_id, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/unlock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_redeploy(const string& service_id, const string& bootstrap, const vector<string>& input_files, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/redeploy";
    ostringstream json;
    json << "{";
    bool has_field = false;
    if (!bootstrap.empty()) {
        json << "\"bootstrap\":\"" << escape_json(bootstrap) << "\"";
        has_field = true;
    }
    if (!input_files.empty()) {
        if (has_field) json << ",";
        json << "\"input_files\":[";
        for (size_t i = 0; i < input_files.size(); i++) {
            if (i > 0) json << ",";
            ifstream file(input_files[i], ios::binary);
            if (!file) continue;
            ostringstream content;
            content << file.rdbuf();
            string b64 = base64_encode(content.str());
            string filename = input_files[i].substr(input_files[i].find_last_of("/\\") + 1);
            json << "{\"filename\":\"" << filename << "\",\"content_base64\":\"" << b64 << "\"}";
        }
        json << "]";
    }
    json << "}";
    string body = json.str();
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string service_logs(const string& service_id, bool all, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/logs" + (all ? "?all=true" : "");
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string service_execute(const string& service_id, const string& command, int timeout_ms, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/execute";
    string body = "{\"command\":\"" + escape_json(command) + "\"";
    if (timeout_ms > 0) body += ",\"timeout\":" + to_string(timeout_ms);
    body += "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string service_resize(const string& service_id, int vcpu, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/resize";
    string body = "{\"vcpu\":" + to_string(vcpu) + "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

// Snapshot functions
string snapshot_list(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("GET", "/snapshots", "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + "/snapshots' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_get(const string& snapshot_id, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id;
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_session(const string& session_id, const string& name, bool hot, const string& public_key, const string& secret_key) {
    string path = "/sessions/" + session_id + "/snapshot";
    string body = "{";
    if (!name.empty()) body += "\"name\":\"" + escape_json(name) + "\",";
    body += "\"hot\":" + string(hot ? "true" : "false") + "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string snapshot_service(const string& service_id, const string& name, bool hot, const string& public_key, const string& secret_key) {
    string path = "/services/" + service_id + "/snapshot";
    string body = "{";
    if (!name.empty()) body += "\"name\":\"" + escape_json(name) + "\",";
    body += "\"hot\":" + string(hot ? "true" : "false") + "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string snapshot_restore(const string& snapshot_id, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id + "/restore";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_delete(const string& snapshot_id, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id;
    string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
    string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_lock(const string& snapshot_id, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id + "/lock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_unlock(const string& snapshot_id, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id + "/unlock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string snapshot_clone(const string& snapshot_id, const string& clone_type, const string& name, const string& ports, const string& shell, const string& public_key, const string& secret_key) {
    string path = "/snapshots/" + snapshot_id + "/clone";
    string body = "{\"type\":\"" + clone_type + "\"";
    if (!name.empty()) body += ",\"name\":\"" + escape_json(name) + "\"";
    if (!ports.empty()) body += ",\"ports\":\"" + ports + "\"";
    if (!shell.empty()) body += ",\"shell\":\"" + shell + "\"";
    body += "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

// Image functions
string image_list(const string& filter, const string& public_key, const string& secret_key) {
    string path = "/images" + (filter.empty() ? "" : "?filter=" + filter);
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_get(const string& image_id, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id;
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_publish(const string& source_type, const string& source_id, const string& name, const string& description, const string& public_key, const string& secret_key) {
    string body = "{\"source_type\":\"" + source_type + "\",\"source_id\":\"" + source_id + "\"";
    if (!name.empty()) body += ",\"name\":\"" + escape_json(name) + "\"";
    if (!description.empty()) body += ",\"description\":\"" + escape_json(description) + "\"";
    body += "}";
    string auth_headers = build_auth_headers("POST", "/images", body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/images' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_delete(const string& image_id, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id;
    string auth_headers = build_auth_headers("DELETE", path, "", public_key, secret_key);
    string cmd = "curl -s -X DELETE '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_lock(const string& image_id, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/lock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_unlock(const string& image_id, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/unlock";
    string auth_headers = build_auth_headers("POST", path, "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_set_visibility(const string& image_id, const string& visibility, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/visibility";
    string body = "{\"visibility\":\"" + visibility + "\"}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_grant_access(const string& image_id, const string& trusted_key, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/grant";
    string body = "{\"trusted_api_key\":\"" + trusted_key + "\"}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_revoke_access(const string& image_id, const string& trusted_key, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/revoke";
    string body = "{\"trusted_api_key\":\"" + trusted_key + "\"}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_list_trusted(const string& image_id, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/trusted";
    string auth_headers = build_auth_headers("GET", path, "", public_key, secret_key);
    string cmd = "curl -s -X GET '" + API_BASE + path + "' " + auth_headers;
    return exec_curl(cmd);
}

string image_transfer(const string& image_id, const string& to_api_key, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/transfer";
    string body = "{\"to_api_key\":\"" + to_api_key + "\"}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_spawn(const string& image_id, const string& name, const string& ports, const string& bootstrap, const string& network, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/spawn";
    string body = "{";
    bool has_field = false;
    if (!name.empty()) { body += "\"name\":\"" + escape_json(name) + "\""; has_field = true; }
    if (!ports.empty()) { body += string(has_field ? "," : "") + "\"ports\":\"" + ports + "\""; has_field = true; }
    if (!bootstrap.empty()) { body += string(has_field ? "," : "") + "\"bootstrap\":\"" + escape_json(bootstrap) + "\""; has_field = true; }
    if (!network.empty()) { body += string(has_field ? "," : "") + "\"network\":\"" + network + "\""; }
    body += "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

string image_clone(const string& image_id, const string& name, const string& description, const string& public_key, const string& secret_key) {
    string path = "/images/" + image_id + "/clone";
    string body = "{";
    bool has_field = false;
    if (!name.empty()) { body += "\"name\":\"" + escape_json(name) + "\""; has_field = true; }
    if (!description.empty()) { body += string(has_field ? "," : "") + "\"description\":\"" + escape_json(description) + "\""; }
    body += "}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl(cmd);
}

static string json_string_field(const string& json, const string& key);

// PaaS Logs functions: GET {PORTAL_BASE}/logs/{source}[/stream] (partner log access)

// Percent-encode everything outside the unreserved set (keep '/' when keep_slash),
// so the value is safe inside a single-quoted shell word and a URL.
static string pct_encode(const string& s, bool keep_slash = false) {
    static const char* hex = "0123456789ABCDEF";
    string out;
    for (unsigned char c : s) {
        if (isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~' || (keep_slash && c == '/')) out += (char)c;
        else { out += '%'; out += hex[c >> 4]; out += hex[c & 15]; }
    }
    return out;
}

static string logs_path(const string& source, bool stream) {
    return "/logs/" + pct_encode(source.empty() ? "all" : source, true) + (stream ? "/stream" : "");
}

static string logs_query(const vector<pair<string, string>>& kv) {
    string q;
    for (const auto& p : kv) {
        if (p.second.empty()) continue;
        q += (q.empty() ? "?" : "&") + p.first + "=" + pct_encode(p.second);
    }
    return q;
}

// HMAC covers the path only (the server verifies conn.request_path, no query string).
// Only the public key, timestamp and signature reach curl's argv; the secret stays here.
static string portal_curl_cmd(const string& path, const string& query, const string& public_key, const string& secret_key, bool stream) {
    string ts = get_timestamp();
    string sig = compute_hmac(secret_key, ts + ":GET:" + path + ":");
    string pk = public_key;
    for (char c : pk) if (!isalnum((unsigned char)c) && c != '-' && c != '_') { pk = "invalid-public-key"; break; }
    string base = PORTAL_BASE;
    for (char c : base) if (c == '\'') { base = "https://unsandbox.com"; break; }
    return string("curl -s ") + (stream ? "-N --http1.1 -H 'Accept: text/event-stream' " : "") +
           "-H 'Authorization: Bearer " + pk + "' -H 'X-Timestamp: " + ts + "' -H 'X-Signature: " + sig + "' "
           "-H 'User-Agent: un-cpp/2.0' -w '\\n__UN_HTTP__%{http_code}\\n' '" + base + path + query + "' 2>/dev/null";
}

// GET {portal}/logs/{source}; returns (body, http status; 0 = network error).
static pair<string, int> logs_fetch_raw(const string& source, int lines, const string& since, const string& grep, const string& level,
                                        const string& public_key, const string& secret_key) {
    string q = logs_query({{"lines", to_string(lines > 0 ? lines : 100)}, {"since", since}, {"grep", grep}, {"level", level}});
    FILE* pipe = popen(portal_curl_cmd(logs_path(source, false), q, public_key, secret_key, false).c_str(), "r");
    if (!pipe) return {"", 0};
    string out; char buf[4096]; size_t n;
    while ((n = fread(buf, 1, sizeof(buf), pipe)) > 0) out.append(buf, n);
    pclose(pipe);
    size_t m = out.rfind("\n__UN_HTTP__");
    if (m == string::npos) return {out, 0};
    int code = atoi(out.c_str() + m + 12);
    return {out.substr(0, m), code};
}

// Fetch batch logs from the portal (GET {portal}/logs/{source}); returns the JSON body.
string logs_fetch(const string& source, int lines, const string& since, const string& grep, const string& public_key, const string& secret_key) {
    return logs_fetch_raw(source, lines, since, grep, "", public_key, secret_key).first;
}

// Minimal JSON helpers for the logs response shapes.
static size_t json_skip_ws(const string& s, size_t i) { while (i < s.size() && isspace((unsigned char)s[i])) i++; return i; }

static string json_parse_string(const string& s, size_t& i) {  // s[i] == '"'
    string out;
    for (i++; i < s.size(); i++) {
        char c = s[i];
        if (c == '"') { i++; return out; }
        if (c == '\\' && i + 1 < s.size()) {
            char e = s[++i];
            switch (e) {
                case 'n': out += '\n'; break;
                case 't': out += '\t'; break;
                case 'r': out += '\r'; break;
                case 'b': out += '\b'; break;
                case 'f': out += '\f'; break;
                case 'u':
                    if (i + 4 < s.size()) {
                        unsigned cp = (unsigned)strtoul(s.substr(i + 1, 4).c_str(), nullptr, 16);
                        i += 4;
                        if (cp < 0x80) out += (char)cp;
                        else if (cp < 0x800) { out += (char)(0xC0 | (cp >> 6)); out += (char)(0x80 | (cp & 0x3F)); }
                        else { out += (char)(0xE0 | (cp >> 12)); out += (char)(0x80 | ((cp >> 6) & 0x3F)); out += (char)(0x80 | (cp & 0x3F)); }
                    }
                    break;
                default: out += e;
            }
        } else out += c;
    }
    return out;
}

// Skip any JSON value starting at s[i].
static void json_skip_value(const string& s, size_t& i) {
    i = json_skip_ws(s, i);
    if (i >= s.size()) return;
    if (s[i] == '"') { json_parse_string(s, i); return; }
    if (s[i] == '{' || s[i] == '[') {
        int depth = 0;
        while (i < s.size()) {
            if (s[i] == '"') { json_parse_string(s, i); continue; }
            if (s[i] == '{' || s[i] == '[') depth++;
            else if (s[i] == '}' || s[i] == ']') { depth--; if (depth == 0) { i++; return; } }
            i++;
        }
        return;
    }
    while (i < s.size() && s[i] != ',' && s[i] != '}' && s[i] != ']') i++;
}

// Object at s[i] == '{': string-valued fields.
static map<string, string> json_parse_flat_object(const string& s, size_t& i) {
    map<string, string> m;
    i++;
    while (i < s.size()) {
        i = json_skip_ws(s, i);
        if (i < s.size() && s[i] == '}') { i++; break; }
        if (i >= s.size() || s[i] != '"') { i++; continue; }
        string key = json_parse_string(s, i);
        i = json_skip_ws(s, i);
        if (i < s.size() && s[i] == ':') i++;
        i = json_skip_ws(s, i);
        if (i < s.size() && s[i] == '"') m[key] = json_parse_string(s, i);
        else json_skip_value(s, i);
        i = json_skip_ws(s, i);
        if (i < s.size() && s[i] == ',') i++;
    }
    return m;
}

static void print_source_line(const string& source, const string& line) {
    printf("\033[36m%-12s\033[0m %s\n", source.c_str(), line.c_str());
}

// Print a /logs response: string lines as is, {source,line} entries as "%-12s %s".
static bool print_logs_body(const string& body) {
    size_t p = body.find("\"lines\"");
    if (p == string::npos) return false;
    p = json_skip_ws(body, p + 7);
    if (p >= body.size() || body[p] != ':') return false;
    p = json_skip_ws(body, p + 1);
    if (p >= body.size() || body[p] != '[') return false;
    p++;
    while (p < body.size()) {
        p = json_skip_ws(body, p);
        if (p >= body.size() || body[p] == ']') break;
        if (body[p] == '"') printf("%s\n", json_parse_string(body, p).c_str());
        else if (body[p] == '{') { auto m = json_parse_flat_object(body, p); print_source_line(m["source"], m["line"]); }
        else json_skip_value(body, p);
        p = json_skip_ws(body, p);
        if (p < body.size() && body[p] == ',') p++;
    }
    return true;
}

// One SSE connection; returns HTTP status (0 = network error / dropped before a status).
static int logs_stream_once(const string& source, const string& grep, const string& level, const string& since,
                            const string& public_key, const string& secret_key, time_t& last_recv) {
    string q = logs_query({{"grep", grep}, {"level", level}, {"since", since}});
    FILE* pipe = popen(portal_curl_cmd(logs_path(source, true), q, public_key, secret_key, true).c_str(), "r");
    if (!pipe) return 0;
    int code = 0;
    string line; char buf[8192];
    while (fgets(buf, sizeof(buf), pipe)) {
        line += buf;
        if (line.empty() || line.back() != '\n') continue;
        line.pop_back();
        if (!line.empty() && line.back() == '\r') line.pop_back();
        if (line.rfind("__UN_HTTP__", 0) == 0) code = atoi(line.c_str() + 11);
        else if (line.rfind("data:", 0) == 0) {
            string data = line.substr(5);
            size_t a = data.find_first_not_of(' ');
            data = a == string::npos ? "" : data.substr(a);
            if (!data.empty() && data != ":keepalive") {
                if (data[0] == '{') {
                    size_t i = 0;
                    auto m = json_parse_flat_object(data, i);
                    if (m.count("line")) {
                        if (m["source"].empty()) printf("%s\n", m["line"].c_str());
                        else print_source_line(m["source"], m["line"]);
                        last_recv = time(nullptr);
                    }
                } else { printf("%s\n", data.c_str()); last_recv = time(nullptr); }
                fflush(stdout);
            }
        }
        line.clear();
    }
    pclose(pipe);
    return code;
}

// Stream until a 4xx, reconnecting on drops (2 s backoff doubling to 30 s, since= overlap).
static int logs_follow(const string& source, const string& grep, const string& level, const string& public_key, const string& secret_key) {
    int delay = 2;
    time_t last_recv = 0;
    string since;
    for (;;) {
        time_t start = time(nullptr);
        int code = logs_stream_once(source, grep, level, since, public_key, secret_key, last_recv);
        if (time(nullptr) - start > 30) delay = 2;
        if (code >= 400 && code < 500) return code;
        sleep(delay);
        delay = delay * 2 > 30 ? 30 : delay * 2;
        if (last_recv > 0) {
            long gap = (long)(time(nullptr) - last_recv) + 5;
            since = to_string(gap < 10 ? 10 : gap) + "s";
        }
    }
}

static string log_cursor_path() {
    const char* home = getenv("HOME");
    return home ? string(home) + "/.unsandbox/paas_log_cursor" : "";
}

static string read_log_cursor() {
    string p = log_cursor_path();
    if (p.empty()) return "";
    ifstream f(p);
    long ts = 0;
    if (!(f >> ts) || ts <= 0) return "";
    long ago = (long)time(nullptr) - ts;
    return to_string(ago < 5 ? 5 : ago) + "s";
}

static void write_log_cursor() {
    string p = log_cursor_path();
    if (p.empty()) return;
    mkdir((string(getenv("HOME")) + "/.unsandbox").c_str(), 0700);
    ofstream f(p);
    f << (long)time(nullptr) << "\n";
}

static void print_paas_logs_help() {
    cerr << "Usage: un paas logs [options]\n\n"
            "Fetch or stream PaaS production logs.\n\n"
            "Sources:\n"
            "  --all              All sources (default)\n"
            "  --api              API server logs\n"
            "  --portal           Portal server logs\n"
            "  --pool NODE        Pool node logs (e.g., cammy, ai)\n"
            "  --source SOURCE    Source string (api, portal, pool/cammy, all)\n\n"
            "Options:\n"
            "  --lines N, -n N    Number of lines (default: 100, max: 10000)\n"
            "  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)\n"
            "  --grep PATTERN     Filter log lines by pattern\n"
            "  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit\n"
            "  --follow, -f       Follow logs in real-time (Ctrl+C to stop)\n"
            "  --json             Output raw JSON response\n"
            "  -p KEY             Public key\n"
            "  -k KEY             Secret key\n"
            "  -h                 Show this help\n\n"
            "Requires a partner API key with log access enabled.\n";
}

// `un paas <command>`: currently `logs`. Credentials arrive resolved (env / csv / --account / -p).
static int cmd_paas(int argc, char* argv[], string public_key, string secret_key) {
    if (argc < 3) {
        cerr << "Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n\n"
                "Run 'un paas <command> -h' for help on a specific command.\n";
        return 1;
    }
    if (string(argv[2]) != "logs") {
        cerr << "Error: Unknown paas command '" << argv[2] << "'\nRun 'un paas' for available commands.\n";
        return 1;
    }
    string source = "all", since, grep, level;
    int lines = 100;
    bool follow = false, json = false;
    for (int i = 3; i < argc; i++) {
        string a = argv[i];
        bool more = i + 1 < argc;
        if (a == "-p" && more) public_key = argv[++i];
        else if (a == "-k" && more) secret_key = argv[++i];
        else if (a == "--account" && more) i++;  // handled in the first pass
        else if (a == "--source" && more) source = argv[++i];
        else if (a == "--all") source = "all";
        else if (a == "--api") source = "api";
        else if (a == "--portal") source = "portal";
        else if (a == "--pool" && more) source = string("pool/") + argv[++i];
        else if ((a == "--lines" || a == "-n") && more) { lines = atoi(argv[++i]); if (lines < 1) lines = 1; if (lines > 10000) lines = 10000; }
        else if (a == "--since" && more) since = argv[++i];
        else if (a == "--grep" && more) grep = argv[++i];
        else if ((a == "--level" || a == "-l") && more) level = argv[++i];
        else if (a == "--follow" || a == "-f") follow = true;
        else if (a == "--json") json = true;
        else if (a == "-h" || a == "--help") { print_paas_logs_help(); return 0; }
    }
    if (public_key.empty() || secret_key.empty()) {
        cerr << "Error: API credentials required.\n"
                "  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n"
                "  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n"
                "  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n";
        return 1;
    }
    if (follow) {
        cerr << "Streaming " << source << " logs... (Ctrl+C to stop)" << endl;
        int code = logs_follow(source, grep, level, public_key, secret_key);
        if (code == 401 || code == 403) cerr << "Error: HTTP " << code << " (auth failed)" << endl;
        else cerr << "Error: HTTP " << code << endl;
        return 1;
    }
    if (since.empty()) { since = read_log_cursor(); if (since.empty()) since = "5m"; }
    auto res = logs_fetch_raw(source, lines, since, grep, level, public_key, secret_key);
    if (res.second == 0) {
        cerr << "Error: Network error fetching logs" << endl;
        return 1;
    }
    if (res.second != 200) {
        string msg = json_string_field(res.first, "error");
        cerr << "Error: HTTP " << res.second << (msg.empty() ? "" : " — " + msg) << endl;
        return 1;
    }
    write_log_cursor();
    if (json) { printf("%s\n", res.first.c_str()); return 0; }
    if (!print_logs_body(res.first)) {
        cerr << "Error: Invalid response format" << endl;
        return 1;
    }
    return 0;
}

// Key validation
string validate_keys(const string& public_key, const string& secret_key) {
    string auth_headers = build_auth_headers("POST", "/keys/validate", "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/keys/validate' " + auth_headers;
    return exec_curl(cmd);
}

// Utility functions
string hmac_sign(const string& secret_key, const string& message) {
    return compute_hmac(secret_key, message);
}

bool health_check() {
    string cmd = "curl -s -o /dev/null -w '%{http_code}' '" + API_BASE + "/health' 2>/dev/null";
    string result = exec_curl(cmd);
    return result.find("200") != string::npos;
}

string version() {
    return SDK_VERSION;
}

static string last_error_msg;

void set_last_error(const string& msg) {
    last_error_msg = msg;
}

string last_error() {
    return last_error_msg;
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
    if (!TARGET_OS.empty()) json << ",\"os\":\"" << TARGET_OS << "\"";
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
    if (!TARGET_OS.empty()) json << ",\"os\":\"" << TARGET_OS << "\"";
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

void cmd_service(const string& name, const string& ports, const string& type, const string& bootstrap, const string& bootstrap_file, const vector<string>& files, bool list, const string& info, const string& logs, const string& tail, const string& sleep, const string& wake, const string& destroy, const string& resize, const string& execute, const string& command, const string& dump_bootstrap, const string& dump_file, const string& redeploy, const string& network, int vcpu, const vector<string>& envs, const string& env_file, const string& env_action, const string& env_target, const string& set_unfreeze_on_demand_id, int set_unfreeze_on_demand_enabled, int unfreeze_on_demand, const string& public_key, const string& secret_key) {
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
        string path = "/services/" + destroy;
        exec_destructive_curl("DELETE", path, "", public_key, secret_key, "Service destroyed: " + destroy);
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

    if (!set_unfreeze_on_demand_id.empty()) {
        if (set_unfreeze_on_demand_enabled < 0) {
            cerr << RED << "Error: --set-unfreeze-on-demand requires --enabled true|false" << RESET << endl;
            exit(1);
        }
        set_unfreeze_on_demand(set_unfreeze_on_demand_id, set_unfreeze_on_demand_enabled == 1, public_key, secret_key);
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

    if (!redeploy.empty()) {
        // Bootstrap is optional for redeploy:
        // - If provided via --bootstrap or --bootstrap-file, use it
        // - If omitted, API will use the stored encrypted bootstrap
        string bootstrap_to_use = bootstrap;
        if (!bootstrap_file.empty()) {
            struct stat st;
            if (stat(bootstrap_file.c_str(), &st) == 0) {
                bootstrap_to_use = read_file(bootstrap_file);
            } else {
                cerr << RED << "Error: Bootstrap file not found: " << bootstrap_file << RESET << endl;
                exit(1);
            }
        }
        cout << YELLOW << "Redeploying service " << redeploy << "..." << RESET << endl;
        ostringstream json;
        json << "{";
        bool has_field = false;
        if (!bootstrap_to_use.empty()) {
            if (!bootstrap_file.empty()) {
                json << "\"bootstrap_content\":\"" << escape_json(bootstrap_to_use) << "\"";
            } else {
                json << "\"bootstrap\":\"" << escape_json(bootstrap_to_use) << "\"";
            }
            has_field = true;
        }
        if (!files.empty()) {
            if (has_field) json << ",";
            json << "\"input_files\":[";
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
        string path = "/services/" + redeploy + "/redeploy";
        string auth_headers = build_auth_headers("POST", path, json.str(), public_key, secret_key);
        string cmd = "curl -s -X POST '" + API_BASE + path + "' "
                     "-H 'Content-Type: application/json' "
                     + auth_headers + " "
                     "-d '" + json.str() + "'";
        string result = exec_curl(cmd);
        cout << result << endl;
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
        if (!TARGET_OS.empty()) json << ",\"os\":\"" << TARGET_OS << "\"";
        if (vcpu > 0) json << ",\"vcpu\":" << vcpu;
        if (unfreeze_on_demand >= 0) json << ",\"unfreeze_on_demand\":" << (unfreeze_on_demand ? "true" : "false");
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
        exec_destructive_curl("DELETE", path, "", public_key, secret_key, "Image deleted: " + del);
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
        exec_destructive_curl("POST", path, "{}", public_key, secret_key, "Image unlocked: " + unlock);
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

static string json_string_field(const string& json, const string& key);

// Extract a JSON number (integer part) for key; false when absent or null.
static bool json_number_field(const string& json, const string& key, long long& out) {
    string needle = "\"" + key + "\"";
    for (size_t p = json.find(needle); p != string::npos; p = json.find(needle, p + 1)) {
        size_t q = p + needle.size();
        while (q < json.size() && isspace(static_cast<unsigned char>(json[q]))) q++;
        if (q >= json.size() || json[q] != ':') continue;
        q++;
        while (q < json.size() && isspace(static_cast<unsigned char>(json[q]))) q++;
        if (q >= json.size() || !(isdigit(static_cast<unsigned char>(json[q])) || json[q] == '-')) return false;
        out = atoll(json.c_str() + q);
        return true;
    }
    return false;
}

// un key — response handling per CLI_SPEC.md "`un key` output".
void cmd_validate_key(bool extend, const string& public_key, const string& secret_key) {
    if (extend) {
        string url = PORTAL_BASE + "/keys/extend?pk=" + public_key;
        cout << "Opening browser to extend key..." << endl;
        string browser_cmd = "xdg-open '" + url + "' 2>/dev/null || open '" + url + "' 2>/dev/null";
        if (system(browser_cmd.c_str()) != 0) {}
        return;
    }

    string auth_headers = build_auth_headers("POST", "/keys/validate", "", public_key, secret_key);
    string cmd = "curl -s -X POST '" + API_BASE + "/keys/validate' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers;

    auto res = exec_curl_with_status(cmd);
    const string& body = res.first;
    int code = res.second;

    string state, reason;
    if (code >= 200 && code < 300) {
        state = json_string_field(body, "status");
        if (state.empty()) {
            size_t v = body.find("\"valid\"");
            bool valid = false;
            if (v != string::npos) {
                size_t q = body.find(':', v);
                if (q != string::npos) {
                    q++;
                    while (q < body.size() && isspace(static_cast<unsigned char>(body[q]))) q++;
                    valid = body.compare(q, 4, "true") == 0;
                }
            }
            state = valid ? "active" : "invalid";
        }
        reason = "status " + state;
    } else if (code == 401) {
        reason = json_string_field(body, "error");
        if (reason.empty()) reason = "unauthorized";
        if (reason == ":expired" || reason == "expired") state = "expired";
        else if (reason == ":suspended" || reason == "suspended") state = "suspended";
        else state = "invalid";
    } else if (code == 0) {
        cerr << RED << "Error: request to " << API_BASE << "/keys/validate failed" << RESET << endl;
        exit(1);
    } else {
        string msg = json_string_field(body, "error");
        if (msg.empty()) msg = json_string_field(body, "message");
        if (msg.empty()) msg = body;
        cerr << RED << "Error: HTTP " << code << " " << msg << RESET << endl;
        exit(4);
    }

    if (state == "expired") {
        cout << RED << "Expired" << RESET << endl;
        cout << "Public Key: " << public_key << endl;
        cout << YELLOW << "To renew: Visit " << PORTAL_BASE << "/keys/extend" << RESET << endl;
        exit(3);
    } else if (state == "suspended") {
        cout << RED << "Suspended" << RESET << endl;
        cout << "Public Key: " << public_key << endl;
        exit(3);
    } else if (state != "active") {
        cout << RED << "Invalid" << RESET << endl;
        cout << "Public Key: " << public_key << endl;
        cout << "Reason: " << reason << endl;
        exit(3);
    }

    string tier = "N/A", status = json_string_field(body, "status"), expires = "N/A", remaining = "N/A";
    if (status.empty()) status = "N/A";
    long long n = 0;
    if (json_number_field(body, "tier", n)) tier = to_string(n);
    if (json_number_field(body, "valid_through", n)) {
        time_t secs = static_cast<time_t>(n / 1000);
        struct tm tmv;
        char buf[32];
        if (gmtime_r(&secs, &tmv) && strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &tmv)) expires = buf;
        long long left = static_cast<long long>(secs) - static_cast<long long>(time(nullptr));
        if (left <= 0) remaining = "expired";
        else remaining = to_string(left / 86400) + "d " + to_string(left % 86400 / 3600) + "h " + to_string(left % 3600 / 60) + "m";
    }

    cout << GREEN << "Valid" << RESET << endl;
    cout << "Public Key: " << public_key << endl;
    cout << "Tier: " << tier << endl;
    cout << "Status: " << status << endl;
    cout << "Expires: " << expires << endl;
    cout << "Time Remaining: " << remaining << endl;
}

// ---------------------------------------------------------------------------
// un key --rotate [--otp CODE] — rotate this key's secret (see CLI_SPEC.md)
// ---------------------------------------------------------------------------

// Extract a JSON string value, tolerating whitespace around the colon.
static string json_string_field(const string& json, const string& key) {
    string needle = "\"" + key + "\"";
    for (size_t p = json.find(needle); p != string::npos; p = json.find(needle, p + 1)) {
        size_t q = p + needle.size();
        while (q < json.size() && isspace(static_cast<unsigned char>(json[q]))) q++;
        if (q >= json.size() || json[q] != ':') continue;
        q++;
        while (q < json.size() && isspace(static_cast<unsigned char>(json[q]))) q++;
        if (q >= json.size() || json[q] != '"') continue;
        string out;
        for (size_t i = q + 1; i < json.size(); i++) {
            if (json[i] == '\\' && i + 1 < json.size()) { out += json[++i]; continue; }
            if (json[i] == '"') return out;
            out += json[i];
        }
        return out;
    }
    return "";
}

// Header values go into a curl shell command: allow only a safe alphabet.
static bool safe_header_token(const string& s) {
    if (s.empty() || s.size() > 200) return false;
    for (char c : s) {
        if (!isalnum(static_cast<unsigned char>(c)) && c != '-' && c != '_' && c != '.') return false;
    }
    return true;
}

static string trim_ws(string s) {
    while (!s.empty() && isspace(static_cast<unsigned char>(s.back()))) s.pop_back();
    size_t i = 0;
    while (i < s.size() && isspace(static_cast<unsigned char>(s[i]))) i++;
    return s.substr(i);
}

// POST /keys/self/rotate-secret, body {}, fresh signature each call.
static pair<string, int> rotate_secret_post(const string& public_key, const string& secret_key,
                                            const string& otp, const string& challenge) {
    const string path = "/keys/self/rotate-secret";
    const string body = "{}";
    string auth_headers = build_auth_headers("POST", path, body, public_key, secret_key);
    if (!otp.empty()) {
        auth_headers += " -H 'X-Sudo-OTP: " + otp + "'";
        auth_headers += " -H 'X-Sudo-Challenge: " + challenge + "'";
    }
    string cmd = "curl -s --max-time 120 -X POST '" + API_BASE + path + "' "
                 "-H 'Content-Type: application/json' "
                 + auth_headers + " -d '" + body + "'";
    return exec_curl_with_status(cmd);
}

// Replace the secret (2nd field) on the line whose 1st field equals public_key.
// Every other line, field, comment and the order stay as they are.
// Temp file (0600) in the same directory, then rename.
static bool rewrite_accounts_csv(const string& path, const string& public_key, const string& new_secret) {
    ifstream in(path, ios::binary);
    if (!in) return false;
    string content((istreambuf_iterator<char>(in)), istreambuf_iterator<char>());
    in.close();

    string out;
    bool changed = false;
    size_t pos = 0;
    while (pos < content.size()) {
        size_t nl = content.find('\n', pos);
        size_t end = (nl == string::npos) ? content.size() : nl + 1;
        string line = content.substr(pos, end - pos);
        pos = end;
        string t = trim_ws(line);
        if (!changed && !t.empty() && t[0] != '#') {
            size_t c1 = line.find(',');
            if (c1 != string::npos && trim_ws(line.substr(0, c1)) == public_key) {
                size_t c2 = line.find_first_of(",\r\n", c1 + 1);
                string rest = (c2 == string::npos) ? "" : line.substr(c2);
                line = line.substr(0, c1 + 1) + new_secret + rest;
                changed = true;
            }
        }
        out += line;
    }
    if (!changed) return false;

    string tmp = path + "." + to_string(getpid()) + ".tmp";
    int fd = open(tmp.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd < 0) return false;
    bool ok = write(fd, out.data(), out.size()) == static_cast<ssize_t>(out.size());
    fchmod(fd, 0600);
    if (close(fd) != 0) ok = false;
    if (ok && rename(tmp.c_str(), path.c_str()) != 0) ok = false;
    if (!ok) unlink(tmp.c_str());
    return ok;
}

static int rotate_fail(int status, const string& response) {
    string error = json_string_field(response, "error");
    string message = json_string_field(response, "message");
    if (status == 403 && error == "email_required") {
        cerr << "Error: this key has no email; add one or use POST /keys/recover" << endl;
        return 3;
    }
    if (status == 401) {
        cerr << "Error: authentication failed" << endl;
        return 3;
    }
    if (!message.empty()) cerr << "Error: " << message << endl;
    else if (!error.empty()) cerr << "Error: " << error << endl;
    else cerr << "Error: HTTP " << status << endl;
    return 4;
}

// csv_path: the accounts.csv the credentials came from ("" for flags / env vars).
int cmd_rotate_key(const string& public_key, const string& secret_key, const string& otp_arg,
                   const string& csv_path) {
    if (public_key.empty() || secret_key.empty()) {
        cerr << "Error: API credentials required" << endl;
        return 3;
    }
    const char* home = getenv("HOME");
    string dir = string(home ? home : ".") + "/.unsandbox";
    string digest = sha256_raw(public_key);
    static const char hex_chars[] = "0123456789abcdef";
    string prefix;
    for (size_t i = 0; i < 8; i++) {
        unsigned char c = static_cast<unsigned char>(digest[i]);
        prefix.push_back(hex_chars[c >> 4]);
        prefix.push_back(hex_chars[c & 0x0f]);
    }
    string challenge_file = dir + "/rotate-" + prefix + ".challenge";

    pair<string, int> resp;
    if (!otp_arg.empty()) {
        ifstream cf(challenge_file);
        if (!cf) {
            cerr << "Error: no rotation in progress; run: un key --rotate" << endl;
            return 2;
        }
        string challenge;
        getline(cf, challenge);
        cf.close();
        challenge = trim_ws(challenge);
        string otp = trim_ws(otp_arg);
        if (!safe_header_token(otp) || !safe_header_token(challenge)) {
            cerr << "Error: invalid code or challenge" << endl;
            return 2;
        }
        resp = rotate_secret_post(public_key, secret_key, otp, challenge);
        unlink(challenge_file.c_str());
    } else {
        resp = rotate_secret_post(public_key, secret_key, "", "");
        if (resp.second == 428) {
            string challenge = json_string_field(resp.first, "challenge_id");
            if (isatty(STDIN_FILENO)) {
                cerr << "One-time code emailed to the key owner." << endl;
                cerr << "Code: " << flush;
                string otp;
                if (!getline(cin, otp)) otp = "";
                otp = trim_ws(otp);
                if (otp.empty()) {
                    cerr << "Error: cancelled" << endl;
                    return 1;
                }
                if (!safe_header_token(otp) || !safe_header_token(challenge)) {
                    cerr << "Error: invalid code or challenge" << endl;
                    return 2;
                }
                resp = rotate_secret_post(public_key, secret_key, otp, challenge);
            } else {
                mkdir(dir.c_str(), 0700);
                int fd = open(challenge_file.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0600);
                bool ok = fd >= 0 && write(fd, challenge.data(), challenge.size()) == static_cast<ssize_t>(challenge.size());
                if (fd >= 0) {
                    fchmod(fd, 0600);
                    if (close(fd) != 0) ok = false;
                }
                if (!ok) {
                    cerr << "Error: cannot save challenge to " << challenge_file << endl;
                    return 1;
                }
                cout << "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE" << endl;
                return 0;
            }
        }
    }

    if (resp.second != 200) return rotate_fail(resp.second, resp.first);
    string new_secret = json_string_field(resp.first, "secret");
    if (new_secret.empty()) {
        cerr << "Error: API returned no secret" << endl;
        return 4;
    }
    if (!csv_path.empty() && rewrite_accounts_csv(csv_path, public_key, new_secret)) {
        cout << "Secret rotated. Updated " << csv_path << " (public key " << public_key << ")." << endl;
    } else {
        cout << "New secret (shown once, store it now): " << new_secret << endl;
        cout << "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret." << endl;
    }
    return 0;
}

#ifndef UN_NO_MAIN
int main(int argc, char* argv[]) {
    // Global --os OS / --os=OS: strip it from argv and validate before any
    // credential lookup or request.
    vector<char*> kept_args;
    for (int i = 0; i < argc; i++) {
        string a = argv[i];
        bool given = false;
        string value;
        if (i > 0 && a == "--os") {
            given = true;
            if (i + 1 < argc) value = argv[++i];
        } else if (i > 0 && a.rfind("--os=", 0) == 0) {
            given = true;
            value = a.substr(5);
        }
        if (!given) {
            kept_args.push_back(argv[i]);
            continue;
        }
        string err;
        if (!normalize_os(value, TARGET_OS, err)) {
            cerr << RED << "Error: " << err << RESET << endl;
            return 1;
        }
    }
    kept_args.push_back(nullptr);
    argc = (int)kept_args.size() - 1;
    argv = kept_args.data();

    string public_key;
    string secret_key;
    int account_index = -1;  // -1 = not set
    string creds_csv_path;   // accounts.csv the credentials came from ("" = flags / env)

    // First pass: scan for --account N and -p/-k flags before full arg parsing
    for (int i = 1; i < argc; i++) {
        string a = argv[i];
        if (a == "--account" && i+1 < argc) {
            account_index = atoi(argv[++i]);
        } else if (a == "-p" && i+1 < argc) {
            public_key = argv[++i];
        }
    }

    if (account_index >= 0) {
        // --account N: load from ~/.unsandbox/accounts.csv, bypassing env vars
        const char* home = getenv("HOME");
        string csv_path = string(home ? home : ".") + "/.unsandbox/accounts.csv";
        auto creds = loadAccountsCSV(csv_path, account_index);
        if (creds.first.empty()) {
            // fall back to ./accounts.csv
            csv_path = "accounts.csv";
            creds = loadAccountsCSV(csv_path, account_index);
        }
        if (!creds.first.empty()) {
            if (public_key.empty()) public_key = creds.first;
            secret_key = creds.second;
            creds_csv_path = csv_path;
        }
    } else {
        // Priority: env vars, then ~/.unsandbox/accounts.csv row 0, then ./accounts.csv row 0
        if (public_key.empty()) {
            public_key = getenv("UNSANDBOX_PUBLIC_KEY") ? getenv("UNSANDBOX_PUBLIC_KEY") : "";
        }
        secret_key = getenv("UNSANDBOX_SECRET_KEY") ? getenv("UNSANDBOX_SECRET_KEY") : "";

        // Fall back to UNSANDBOX_API_KEY for backwards compatibility
        if (public_key.empty()) {
            public_key = getenv("UNSANDBOX_API_KEY") ? getenv("UNSANDBOX_API_KEY") : "";
        }

        // Try UNSANDBOX_ACCOUNT env var to pick a row
        int env_account = -1;
        const char* env_acct = getenv("UNSANDBOX_ACCOUNT");
        if (env_acct) env_account = atoi(env_acct);

        if (public_key.empty()) {
            const char* home = getenv("HOME");
            string csv_path = string(home ? home : ".") + "/.unsandbox/accounts.csv";
            auto creds = loadAccountsCSV(csv_path, env_account >= 0 ? env_account : 0);
            if (creds.first.empty()) {
                csv_path = "accounts.csv";
                creds = loadAccountsCSV(csv_path, env_account >= 0 ? env_account : 0);
            }
            if (!creds.first.empty()) {
                public_key = creds.first;
                secret_key = creds.second;
                creds_csv_path = csv_path;
            }
        }
    }

    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " [options] <source_file>" << endl;
        cerr << "       " << argv[0] << " languages [--json]" << endl;
        cerr << "       " << argv[0] << " session [options]" << endl;
        cerr << "       " << argv[0] << " service [options]" << endl;
        cerr << "       " << argv[0] << " image [options]" << endl;
        cerr << "       " << argv[0] << " key [options]" << endl;
        cerr << "  --os OS   Target OS: linux (default) or freebsd (execute, session, service)" << endl;
        return 1;
    }

    string cmd_type = argv[1];

    // paas has its own flags (-n, -l, -f mean lines/level/follow there)
    if (cmd_type == "paas") return cmd_paas(argc, argv, public_key, secret_key);

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
            else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
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
            else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
        }

        cmd_session(list, kill, shell, network, vcpu, tmux, screen, files, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "service") {
        string name, ports, type, bootstrap, bootstrap_file;
        bool list = false;
        string info, logs, tail, sleep, wake, destroy, resize, execute, command, dump_bootstrap, dump_file, network, redeploy;
        int vcpu = 0;
        vector<string> files;
        vector<string> envs;
        string env_file, env_action, env_target;
        string set_unfreeze_on_demand_id;
        int set_unfreeze_on_demand_enabled = -1;  // -1 = not specified, 0 = false, 1 = true
        int unfreeze_on_demand = -1;  // For service creation

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
            else if (arg == "--redeploy" && i+1 < argc) redeploy = argv[++i];
            else if (arg == "--dump-bootstrap" && i+1 < argc) dump_bootstrap = argv[++i];
            else if (arg == "--dump-file" && i+1 < argc) dump_file = argv[++i];
            else if (arg == "-n" && i+1 < argc) network = argv[++i];
            else if (arg == "-v" && i+1 < argc) vcpu = stoi(argv[++i]);
            else if (arg == "--set-unfreeze-on-demand" && i+1 < argc) set_unfreeze_on_demand_id = argv[++i];
            else if (arg == "--enabled" && i+1 < argc) {
                string val = argv[++i];
                set_unfreeze_on_demand_enabled = (val == "true") ? 1 : 0;
            }
            else if (arg == "--unfreeze-on-demand" && i+1 < argc) {
                string val = argv[++i];
                unfreeze_on_demand = (val == "true") ? 1 : 0;
            }
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
            else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
        }

        cmd_service(name, ports, type, bootstrap, bootstrap_file, files, list, info, logs, tail, sleep, wake, destroy, resize, execute, command, dump_bootstrap, dump_file, redeploy, network, vcpu, envs, env_file, env_action, env_target, set_unfreeze_on_demand_id, set_unfreeze_on_demand_enabled, unfreeze_on_demand, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "key") {
        bool extend = false;
        bool rotate = false;
        string otp;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--extend") extend = true;
            else if (arg == "--rotate") rotate = true;
            else if (arg == "--otp" && i+1 < argc) otp = argv[++i];
            else if (arg == "-k" && i+1 < argc) { public_key = argv[++i]; creds_csv_path.clear(); }
            else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
        }

        if (rotate) return cmd_rotate_key(public_key, secret_key, otp, creds_csv_path);
        cmd_validate_key(extend, public_key, secret_key);
        return 0;
    }

    if (cmd_type == "languages") {
        bool json_output = false;

        for (int i = 2; i < argc; i++) {
            string arg = argv[i];
            if (arg == "--json") json_output = true;
            else if (arg == "-k" && i+1 < argc) public_key = argv[++i];
            else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
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
        else if (arg == "--account" && i+1 < argc) i++;  // already handled in first pass
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
#endif  // UN_NO_MAIN
