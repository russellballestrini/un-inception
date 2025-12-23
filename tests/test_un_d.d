// Test suite for UN CLI D implementation
// Compile: dmd test_un_d.d -of=test_un_d
// Or with LDC: ldc2 test_un_d.d -of=test_un_d
// Run: ./test_un_d
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

import std.stdio;
import std.file;
import std.path;
import std.process;
import std.net.curl;
import std.json;
import std.string;
import std.algorithm;
import std.conv;

// Copy of detectLanguage from un.d for testing
string detectLanguage(string filename) {
    string[string] langMap = [
        ".py": "python",
        ".js": "javascript",
        ".go": "go",
        ".rs": "rust",
        ".c": "c",
        ".cpp": "cpp",
        ".d": "d",
        ".zig": "zig",
        ".nim": "nim",
        ".v": "v"
    ];

    string ext = extension(filename);
    if (ext in langMap) {
        return langMap[ext];
    }
    return null;
}

bool testExtensionDetection() {
    writeln("=== Test 1: Extension Detection ===");

    struct Test {
        string filename;
        string expected;
    }

    Test[] tests = [
        Test("script.py", "python"),
        Test("app.js", "javascript"),
        Test("main.go", "go"),
        Test("program.rs", "rust"),
        Test("code.c", "c"),
        Test("app.cpp", "cpp"),
        Test("prog.d", "d"),
        Test("main.zig", "zig"),
        Test("script.nim", "nim"),
        Test("app.v", "v"),
        Test("unknown.xyz", null),
    ];

    int passed = 0;
    int failed = 0;

    foreach (test; tests) {
        string result = detectLanguage(test.filename);

        bool testPassed = false;
        if (test.expected is null && result is null) {
            testPassed = true;
        } else if (test.expected !is null && result !is null && result == test.expected) {
            testPassed = true;
        }

        if (testPassed) {
            writefln("  PASS: %s -> %s", test.filename, result is null ? "null" : result);
            passed++;
        } else {
            writefln("  FAIL: %s -> got %s, expected %s",
                     test.filename,
                     result is null ? "null" : result,
                     test.expected is null ? "null" : test.expected);
            failed++;
        }
    }

    writefln("Extension Detection: %d passed, %d failed\n", passed, failed);
    return failed == 0;
}

bool testApiConnection() {
    writeln("=== Test 2: API Connection ===");

    string apiKey = environment.get("UNSANDBOX_API_KEY");
    if (apiKey is null || apiKey.length == 0) {
        writeln("  SKIP: UNSANDBOX_API_KEY not set");
        writeln("API Connection: skipped\n");
        return true;
    }

    JSONValue requestBody = JSONValue([
        "language": JSONValue("python"),
        "code": JSONValue("print('Hello from API test')")
    ]);

    string jsonBody = requestBody.toString();

    auto http = HTTP();
    http.addRequestHeader("Content-Type", "application/json");
    http.addRequestHeader("Authorization", "Bearer " ~ apiKey);

    string response;
    try {
        response = cast(string) post("https://api.unsandbox.com/execute", jsonBody, http);
    } catch (Exception e) {
        writefln("  FAIL: HTTP request error: %s", e.msg);
        return false;
    }

    JSONValue result;
    try {
        result = parseJSON(response);
    } catch (Exception e) {
        writefln("  FAIL: JSON parse error: %s", e.msg);
        return false;
    }

    string stdoutStr = result["stdout"].str;
    if (stdoutStr.indexOf("Hello from API test") == -1) {
        writefln("  FAIL: Unexpected response: %s", stdoutStr);
        return false;
    }

    writeln("  PASS: API connection successful");
    writeln("API Connection: passed\n");
    return true;
}

bool testFibExecution() {
    writeln("=== Test 3: Functional Test (fib.go) ===");

    string apiKey = environment.get("UNSANDBOX_API_KEY");
    if (apiKey is null || apiKey.length == 0) {
        writeln("  SKIP: UNSANDBOX_API_KEY not set");
        writeln("Functional Test: skipped\n");
        return true;
    }

    if (!exists("../un_d")) {
        writeln("  SKIP: ../un_d binary not found (run: cd .. && dmd un.d -of=un_d)");
        writeln("Functional Test: skipped\n");
        return true;
    }

    if (!exists("fib.go")) {
        writeln("  SKIP: fib.go not found");
        writeln("Functional Test: skipped\n");
        return true;
    }

    try {
        auto result = execute(["../un_d", "fib.go"]);

        if (result.status != 0) {
            writefln("  FAIL: Command failed with exit code: %d", result.status);
            writefln("  Output: %s", result.output);
            return false;
        }

        if (result.output.indexOf("fib(10) = 55") == -1) {
            writefln("  FAIL: Expected output to contain 'fib(10) = 55', got: %s", result.output);
            return false;
        }

        writeln("  PASS: fib.go executed successfully");
        writef("  Output: %s", result.output);
        writeln("Functional Test: passed\n");
        return true;
    } catch (Exception e) {
        writefln("  FAIL: Execution error: %s", e.msg);
        return false;
    }
}

int main() {
    writeln("UN CLI D Implementation Test Suite");
    writeln("===================================\n");

    bool allPassed = true;

    if (!testExtensionDetection()) {
        allPassed = false;
    }

    if (!testApiConnection()) {
        allPassed = false;
    }

    if (!testFibExecution()) {
        allPassed = false;
    }

    writeln("===================================");
    if (allPassed) {
        writeln("RESULT: ALL TESTS PASSED");
        return 0;
    } else {
        writeln("RESULT: SOME TESTS FAILED");
        return 1;
    }
}
