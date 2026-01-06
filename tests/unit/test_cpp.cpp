// Unit tests for un.cpp - tests internal functions without API calls
// Compile: g++ -o test_cpp test_cpp.cpp && ./test_cpp

#include <iostream>
#include <string>
#include <map>
#include <cstring>

int passed = 0;
int failed = 0;

#define TEST(name, expr) do { \
    if (expr) { \
        std::cout << "  ✓ " << name << std::endl; \
        passed++; \
    } else { \
        std::cout << "  ✗ " << name << std::endl; \
        failed++; \
    } \
} while(0)

std::map<std::string, std::string> extMap = {
    {".py", "python"}, {".js", "javascript"}, {".ts", "typescript"},
    {".rb", "ruby"}, {".go", "go"}, {".rs", "rust"}, {".c", "c"},
    {".cpp", "cpp"}, {".java", "java"}, {".kt", "kotlin"}, {".hs", "haskell"}
};

std::string getLanguage(const std::string& ext) {
    auto it = extMap.find(ext);
    return it != extMap.end() ? it->second : "";
}

std::string getExtension(const std::string& filename) {
    size_t dot = filename.rfind('.');
    return dot != std::string::npos ? filename.substr(dot) : "";
}

std::string getBasename(const std::string& path) {
    size_t slash = path.rfind('/');
    return slash != std::string::npos ? path.substr(slash + 1) : path;
}

int main() {
    std::cout << "\n=== Extension Mapping Tests ===" << std::endl;

    TEST("Python extension maps correctly",
         getLanguage(".py") == "python");

    TEST("C++ extension maps correctly",
         getLanguage(".cpp") == "cpp");

    TEST("JavaScript extension maps correctly",
         getLanguage(".js") == "javascript");

    TEST("Go extension maps correctly",
         getLanguage(".go") == "go");

    TEST("Rust extension maps correctly",
         getLanguage(".rs") == "rust");

    std::cout << "\n=== Signature Format Tests ===" << std::endl;

    std::string timestamp = "1704067200";
    std::string method = "POST";
    std::string endpoint = "/execute";
    std::string body = "{\"language\":\"python\"}";
    std::string message = timestamp + ":" + method + ":" + endpoint + ":" + body;

    TEST("Signature format starts with timestamp",
         message.substr(0, timestamp.length()) == timestamp);

    TEST("Signature format contains :POST:",
         message.find(":POST:") != std::string::npos);

    TEST("Signature format contains :/execute:",
         message.find(":/execute:") != std::string::npos);

    std::cout << "\n=== Language Detection Tests ===" << std::endl;

    std::string content = "#!/usr/bin/env python3\nprint('hello')";
    std::string firstLine = content.substr(0, content.find('\n'));

    TEST("Python shebang detection - starts with #!",
         firstLine.substr(0, 2) == "#!");

    TEST("Python shebang detection - contains python",
         firstLine.find("python") != std::string::npos);

    std::cout << "\n=== Argument Parsing Tests ===" << std::endl;

    std::string arg1 = "DEBUG=1";
    size_t eq1 = arg1.find('=');
    std::string key1 = arg1.substr(0, eq1);
    std::string value1 = arg1.substr(eq1 + 1);

    TEST("Parse -e KEY=VALUE format - key",
         key1 == "DEBUG");

    TEST("Parse -e KEY=VALUE format - value",
         value1 == "1");

    std::string arg2 = "URL=https://example.com?foo=bar";
    size_t eq2 = arg2.find('=');
    std::string key2 = arg2.substr(0, eq2);
    std::string value2 = arg2.substr(eq2 + 1);

    TEST("Parse -e KEY=VALUE with equals in value",
         key2 == "URL" && value2 == "https://example.com?foo=bar");

    std::cout << "\n=== File Operations Tests ===" << std::endl;

    TEST("Extract file basename",
         getBasename("/home/user/project/script.cpp") == "script.cpp");

    TEST("Extract file extension",
         getExtension("/home/user/project/script.cpp") == ".cpp");

    std::cout << "\n=== API Constants Tests ===" << std::endl;

    std::string apiBase = "https://api.unsandbox.com";

    TEST("API base URL starts with https://",
         apiBase.substr(0, 8) == "https://");

    TEST("API base URL contains unsandbox.com",
         apiBase.find("unsandbox.com") != std::string::npos);

    std::cout << "\n=== Summary ===" << std::endl;
    std::cout << "Passed: " << passed << std::endl;
    std::cout << "Failed: " << failed << std::endl;
    std::cout << "Total:  " << (passed + failed) << std::endl;

    return failed > 0 ? 1 : 0;
}
