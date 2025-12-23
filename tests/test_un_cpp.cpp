// Test suite for UN CLI C++ implementation
// Compile: g++ -o test_un_cpp test_un_cpp.cpp -lcurl
// Run: ./test_un_cpp
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cstdlib>
#include <cstring>
#include <curl/curl.h>
#include <sys/stat.h>
#include <array>
#include <memory>

static size_t write_callback(void *contents, size_t size, size_t nmemb, std::string *userp) {
    size_t realsize = size * nmemb;
    userp->append((char*)contents, realsize);
    return realsize;
}

// Copy of detect_language from un.cpp for testing
std::string detect_language(const std::string &filename) {
    std::map<std::string, std::string> lang_map = {
        {".py", "python"},
        {".js", "javascript"},
        {".go", "go"},
        {".rs", "rust"},
        {".c", "c"},
        {".cpp", "cpp"},
        {".d", "d"},
        {".zig", "zig"},
        {".nim", "nim"},
        {".v", "v"}
    };

    size_t dot_pos = filename.rfind('.');
    if (dot_pos == std::string::npos) return "";

    std::string ext = filename.substr(dot_pos);
    auto it = lang_map.find(ext);
    return (it != lang_map.end()) ? it->second : "";
}

bool test_extension_detection() {
    std::cout << "=== Test 1: Extension Detection ===" << std::endl;

    struct TestCase {
        std::string filename;
        std::string expected;
    };

    TestCase tests[] = {
        {"script.py", "python"},
        {"app.js", "javascript"},
        {"main.go", "go"},
        {"program.rs", "rust"},
        {"code.c", "c"},
        {"app.cpp", "cpp"},
        {"prog.d", "d"},
        {"main.zig", "zig"},
        {"script.nim", "nim"},
        {"app.v", "v"},
        {"unknown.xyz", ""},
    };

    int passed = 0;
    int failed = 0;

    for (const auto &test : tests) {
        std::string result = detect_language(test.filename);
        if (result == test.expected) {
            std::cout << "  PASS: " << test.filename << " -> " << result << std::endl;
            passed++;
        } else {
            std::cout << "  FAIL: " << test.filename << " -> got " << result
                      << ", expected " << test.expected << std::endl;
            failed++;
        }
    }

    std::cout << "Extension Detection: " << passed << " passed, " << failed << " failed\n" << std::endl;
    return failed == 0;
}

bool test_api_connection() {
    std::cout << "=== Test 2: API Connection ===" << std::endl;

    const char *api_key = std::getenv("UNSANDBOX_API_KEY");
    if (!api_key) {
        std::cout << "  SKIP: UNSANDBOX_API_KEY not set" << std::endl;
        std::cout << "API Connection: skipped\n" << std::endl;
        return true;
    }

    CURL *curl = curl_easy_init();
    if (!curl) {
        std::cout << "  FAIL: Failed to initialize curl" << std::endl;
        return false;
    }

    std::string json_body = "{\"language\":\"python\",\"code\":\"print('Hello from API test')\"}";
    std::string auth_header = "Authorization: Bearer " + std::string(api_key);

    struct curl_slist *headers = nullptr;
    headers = curl_slist_append(headers, "Content-Type: application/json");
    headers = curl_slist_append(headers, auth_header.c_str());

    std::string response;

    curl_easy_setopt(curl, CURLOPT_URL, "https://api.unsandbox.com/execute");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_body.c_str());
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    CURLcode res = curl_easy_perform(curl);

    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);

    if (res != CURLE_OK) {
        std::cout << "  FAIL: HTTP request error: " << curl_easy_strerror(res) << std::endl;
        return false;
    }

    if (response.find("Hello from API test") == std::string::npos) {
        std::cout << "  FAIL: Unexpected response: " << response << std::endl;
        return false;
    }

    std::cout << "  PASS: API connection successful" << std::endl;
    std::cout << "API Connection: passed\n" << std::endl;
    return true;
}

std::string exec(const char* cmd) {
    std::array<char, 128> buffer;
    std::string result;
    std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }
    while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
        result += buffer.data();
    }
    return result;
}

bool test_fib_execution() {
    std::cout << "=== Test 3: Functional Test (fib.go) ===" << std::endl;

    const char *api_key = std::getenv("UNSANDBOX_API_KEY");
    if (!api_key) {
        std::cout << "  SKIP: UNSANDBOX_API_KEY not set" << std::endl;
        std::cout << "Functional Test: skipped\n" << std::endl;
        return true;
    }

    struct stat st;
    if (stat("../un_cpp", &st) != 0) {
        std::cout << "  SKIP: ../un_cpp binary not found (run: cd .. && g++ -o un_cpp un.cpp -lcurl)" << std::endl;
        std::cout << "Functional Test: skipped\n" << std::endl;
        return true;
    }

    if (stat("fib.go", &st) != 0) {
        std::cout << "  SKIP: fib.go not found" << std::endl;
        std::cout << "Functional Test: skipped\n" << std::endl;
        return true;
    }

    try {
        std::string output = exec("../un_cpp fib.go 2>&1");

        if (output.find("fib(10) = 55") == std::string::npos) {
            std::cout << "  FAIL: Expected output to contain 'fib(10) = 55', got: " << output << std::endl;
            return false;
        }

        std::cout << "  PASS: fib.go executed successfully" << std::endl;
        std::cout << "  Output: " << output;
        std::cout << "Functional Test: passed\n" << std::endl;
        return true;
    } catch (const std::exception &e) {
        std::cout << "  FAIL: Execution error: " << e.what() << std::endl;
        return false;
    }
}

int main() {
    std::cout << "UN CLI C++ Implementation Test Suite" << std::endl;
    std::cout << "=====================================" << std::endl << std::endl;

    bool all_passed = true;

    if (!test_extension_detection()) {
        all_passed = false;
    }

    if (!test_api_connection()) {
        all_passed = false;
    }

    if (!test_fib_execution()) {
        all_passed = false;
    }

    std::cout << "=====================================" << std::endl;
    if (all_passed) {
        std::cout << "RESULT: ALL TESTS PASSED" << std::endl;
        return 0;
    } else {
        std::cout << "RESULT: SOME TESTS FAILED" << std::endl;
        return 1;
    }
}
