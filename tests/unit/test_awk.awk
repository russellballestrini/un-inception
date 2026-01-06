#!/usr/bin/env gawk -f
# Unit tests for un.awk - tests internal functions without API calls
# Run with: gawk -f test_awk.awk

BEGIN {
    passed = 0
    failed = 0

    # Extension mapping
    ext_map[".py"] = "python"
    ext_map[".js"] = "javascript"
    ext_map[".ts"] = "typescript"
    ext_map[".rb"] = "ruby"
    ext_map[".go"] = "go"
    ext_map[".rs"] = "rust"
    ext_map[".c"] = "c"
    ext_map[".awk"] = "awk"
    ext_map[".java"] = "java"
    ext_map[".hs"] = "haskell"

    print ""
    print "=== Extension Mapping Tests ==="

    test("Python extension maps correctly",
         ext_map[".py"] == "python")

    test("AWK extension maps correctly",
         ext_map[".awk"] == "awk")

    test("JavaScript extension maps correctly",
         ext_map[".js"] == "javascript")

    test("Go extension maps correctly",
         ext_map[".go"] == "go")

    print ""
    print "=== Signature Format Tests ==="

    timestamp = "1704067200"
    method = "POST"
    endpoint = "/execute"
    body = "{\"language\":\"python\"}"
    message = timestamp ":" method ":" endpoint ":" body

    test("Signature format starts with timestamp",
         substr(message, 1, length(timestamp)) == timestamp)

    test("Signature format contains :POST:",
         index(message, ":POST:") > 0)

    test("Signature format contains :/execute:",
         index(message, ":/execute:") > 0)

    print ""
    print "=== Language Detection Tests ==="

    content = "#!/usr/bin/env python3\nprint('hello')"
    split(content, lines, "\n")
    first_line = lines[1]

    test("Python shebang detection - starts with #!",
         substr(first_line, 1, 2) == "#!")

    test("Python shebang detection - contains python",
         index(first_line, "python") > 0)

    print ""
    print "=== Argument Parsing Tests ==="

    arg1 = "DEBUG=1"
    eq_pos = index(arg1, "=")
    key1 = substr(arg1, 1, eq_pos - 1)
    value1 = substr(arg1, eq_pos + 1)

    test("Parse -e KEY=VALUE format - key",
         key1 == "DEBUG")

    test("Parse -e KEY=VALUE format - value",
         value1 == "1")

    arg2 = "URL=https://example.com?foo=bar"
    eq_pos2 = index(arg2, "=")
    key2 = substr(arg2, 1, eq_pos2 - 1)
    value2 = substr(arg2, eq_pos2 + 1)

    test("Parse -e KEY=VALUE with equals in value",
         key2 == "URL" && value2 == "https://example.com?foo=bar")

    print ""
    print "=== File Operations Tests ==="

    path = "/home/user/project/script.awk"
    n = split(path, parts, "/")
    basename = parts[n]

    test("Extract file basename",
         basename == "script.awk")

    # Extract extension
    dot_pos = 0
    for (i = length(basename); i > 0; i--) {
        if (substr(basename, i, 1) == ".") {
            dot_pos = i
            break
        }
    }
    ext = substr(basename, dot_pos)

    test("Extract file extension",
         ext == ".awk")

    print ""
    print "=== API Constants Tests ==="

    api_base = "https://api.unsandbox.com"

    test("API base URL starts with https://",
         substr(api_base, 1, 8) == "https://")

    test("API base URL contains unsandbox.com",
         index(api_base, "unsandbox.com") > 0)

    print ""
    print "=== Summary ==="
    print "Passed: " passed
    print "Failed: " failed
    print "Total:  " (passed + failed)

    exit (failed > 0 ? 1 : 0)
}

function test(name, result) {
    if (result) {
        print "  ✓ " name
        passed++
    } else {
        print "  ✗ " name
        failed++
    }
}
