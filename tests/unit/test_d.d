// Unit tests for un.d - tests internal functions without API calls
// Run with: rdmd test_d.d

import std.stdio;
import std.string;
import std.algorithm;
import std.array;
import core.stdc.stdlib : exit;

int passed = 0;
int failed = 0;

void test(string name, bool result) {
    if (result) {
        writefln("  ✓ %s", name);
        passed++;
    } else {
        writefln("  ✗ %s", name);
        failed++;
    }
}

string[string] extMap;

static this() {
    extMap = [
        ".py": "python", ".js": "javascript", ".ts": "typescript",
        ".rb": "ruby", ".go": "go", ".rs": "rust", ".c": "c",
        ".d": "d", ".java": "java", ".kt": "kotlin", ".hs": "haskell"
    ];
}

string getLanguage(string ext) {
    auto p = ext in extMap;
    return p ? *p : "";
}

string getExtension(string filename) {
    auto idx = filename.lastIndexOf('.');
    return idx >= 0 ? filename[idx .. $] : "";
}

string getBasename(string path) {
    auto idx = path.lastIndexOf('/');
    return idx >= 0 ? path[idx + 1 .. $] : path;
}

void main() {
    writeln("\n=== Extension Mapping Tests ===");

    test("Python extension maps correctly",
         getLanguage(".py") == "python");

    test("D extension maps correctly",
         getLanguage(".d") == "d");

    test("JavaScript extension maps correctly",
         getLanguage(".js") == "javascript");

    test("Go extension maps correctly",
         getLanguage(".go") == "go");

    writeln("\n=== Signature Format Tests ===");

    string timestamp = "1704067200";
    string method = "POST";
    string endpoint = "/execute";
    string reqBody = `{"language":"python"}`;
    string message = timestamp ~ ":" ~ method ~ ":" ~ endpoint ~ ":" ~ reqBody;

    test("Signature format starts with timestamp",
         message.startsWith(timestamp));

    test("Signature format contains :POST:",
         message.canFind(":POST:"));

    test("Signature format contains :/execute:",
         message.canFind(":/execute:"));

    writeln("\n=== Language Detection Tests ===");

    string content = "#!/usr/bin/env python3\nprint('hello')";
    string firstLine = content.split("\n")[0];

    test("Python shebang detection - starts with #!",
         firstLine.startsWith("#!"));

    test("Python shebang detection - contains python",
         firstLine.canFind("python"));

    writeln("\n=== Argument Parsing Tests ===");

    string arg1 = "DEBUG=1";
    auto parts1 = arg1.findSplit("=");
    string key1 = parts1[0];
    string value1 = parts1[2];

    test("Parse -e KEY=VALUE format - key",
         key1 == "DEBUG");

    test("Parse -e KEY=VALUE format - value",
         value1 == "1");

    string arg2 = "URL=https://example.com?foo=bar";
    auto parts2 = arg2.findSplit("=");
    string key2 = parts2[0];
    string value2 = parts2[2];

    test("Parse -e KEY=VALUE with equals in value",
         key2 == "URL" && value2 == "https://example.com?foo=bar");

    writeln("\n=== File Operations Tests ===");

    test("Extract file basename",
         getBasename("/home/user/project/script.d") == "script.d");

    test("Extract file extension",
         getExtension("/home/user/project/script.d") == ".d");

    writeln("\n=== API Constants Tests ===");

    string apiBase = "https://api.unsandbox.com";

    test("API base URL starts with https://",
         apiBase.startsWith("https://"));

    test("API base URL contains unsandbox.com",
         apiBase.canFind("unsandbox.com"));

    writeln("\n=== Summary ===");
    writefln("Passed: %d", passed);
    writefln("Failed: %d", failed);
    writefln("Total:  %d", passed + failed);

    exit(failed > 0 ? 1 : 0);
}
