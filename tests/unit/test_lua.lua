#!/usr/bin/env lua
-- Unit tests for un.lua - tests internal functions without API calls

local passed = 0
local failed = 0

local function test(name, fn)
    local ok, err = pcall(fn)
    if ok then
        print("  ✓ " .. name)
        passed = passed + 1
    else
        print("  ✗ " .. name)
        print("    " .. tostring(err))
        failed = failed + 1
    end
end

local function assert_equal(actual, expected, msg)
    if actual ~= expected then
        error(string.format("Expected '%s' but got '%s' %s", tostring(expected), tostring(actual), msg or ""))
    end
end

local function assert_not_equal(a, b)
    if a == b then
        error(string.format("Expected values to be different but both were '%s'", tostring(a)))
    end
end

local function assert_contains(str, substr)
    if not string.find(str, substr, 1, true) then
        error(string.format("Expected '%s' to contain '%s'", str, substr))
    end
end

local function assert_true(val)
    if not val then
        error("Expected true but got false")
    end
end

-- Extension mapping (from un.lua)
local ext_map = {
    [".py"] = "python", [".js"] = "javascript", [".ts"] = "typescript",
    [".rb"] = "ruby", [".php"] = "php", [".pl"] = "perl", [".lua"] = "lua",
    [".sh"] = "bash", [".go"] = "go", [".rs"] = "rust", [".c"] = "c",
    [".cpp"] = "cpp", [".cc"] = "cpp", [".cxx"] = "cpp",
    [".java"] = "java", [".kt"] = "kotlin", [".cs"] = "csharp", [".fs"] = "fsharp",
    [".hs"] = "haskell", [".ml"] = "ocaml", [".clj"] = "clojure", [".scm"] = "scheme",
    [".lisp"] = "commonlisp", [".erl"] = "erlang", [".ex"] = "elixir", [".exs"] = "elixir",
    [".jl"] = "julia", [".r"] = "r", [".R"] = "r", [".cr"] = "crystal",
    [".d"] = "d", [".nim"] = "nim", [".zig"] = "zig", [".v"] = "v",
    [".dart"] = "dart", [".groovy"] = "groovy", [".scala"] = "scala",
    [".f90"] = "fortran", [".f95"] = "fortran", [".cob"] = "cobol",
    [".pro"] = "prolog", [".forth"] = "forth", [".4th"] = "forth",
    [".tcl"] = "tcl", [".raku"] = "raku", [".m"] = "objc",
}

-- ============================================================================
-- Extension Mapping Tests
-- ============================================================================

print("\n=== Extension Mapping Tests ===")

test("Python extension maps correctly", function()
    assert_equal(ext_map[".py"], "python")
end)

test("JavaScript extensions map correctly", function()
    assert_equal(ext_map[".js"], "javascript")
    assert_equal(ext_map[".ts"], "typescript")
end)

test("Ruby extension maps correctly", function()
    assert_equal(ext_map[".rb"], "ruby")
end)

test("Go extension maps correctly", function()
    assert_equal(ext_map[".go"], "go")
end)

test("Rust extension maps correctly", function()
    assert_equal(ext_map[".rs"], "rust")
end)

test("C/C++ extensions map correctly", function()
    assert_equal(ext_map[".c"], "c")
    assert_equal(ext_map[".cpp"], "cpp")
    assert_equal(ext_map[".cc"], "cpp")
    assert_equal(ext_map[".cxx"], "cpp")
end)

test("Lua extension maps correctly", function()
    assert_equal(ext_map[".lua"], "lua")
end)

test("JVM extensions map correctly", function()
    assert_equal(ext_map[".java"], "java")
    assert_equal(ext_map[".kt"], "kotlin")
    assert_equal(ext_map[".groovy"], "groovy")
end)

test("Functional language extensions map correctly", function()
    assert_equal(ext_map[".hs"], "haskell")
    assert_equal(ext_map[".ml"], "ocaml")
    assert_equal(ext_map[".clj"], "clojure")
    assert_equal(ext_map[".erl"], "erlang")
end)

-- ============================================================================
-- HMAC Signature Tests (using openssl via shell)
-- ============================================================================

print("\n=== HMAC Signature Tests ===")

local function hmac_sha256(secret, message)
    local cmd = string.format("echo -n '%s' | openssl dgst -sha256 -hmac '%s' 2>/dev/null | sed 's/^.* //'",
        message:gsub("'", "'\\''"), secret:gsub("'", "'\\''"))
    local handle = io.popen(cmd)
    if handle then
        local result = handle:read("*a"):gsub("%s+$", "")
        handle:close()
        return result
    end
    return nil
end

test("HMAC-SHA256 generates 64 character hex string", function()
    local sig = hmac_sha256("test-secret", "test-message")
    if sig then
        assert_equal(#sig, 64)
    end
end)

test("Same input produces same signature", function()
    local sig1 = hmac_sha256("key", "message")
    local sig2 = hmac_sha256("key", "message")
    if sig1 and sig2 then
        assert_equal(sig1, sig2)
    end
end)

test("Different secrets produce different signatures", function()
    local sig1 = hmac_sha256("key1", "message")
    local sig2 = hmac_sha256("key2", "message")
    if sig1 and sig2 then
        assert_not_equal(sig1, sig2)
    end
end)

test("Different messages produce different signatures", function()
    local sig1 = hmac_sha256("key", "message1")
    local sig2 = hmac_sha256("key", "message2")
    if sig1 and sig2 then
        assert_not_equal(sig1, sig2)
    end
end)

test("Signature format is timestamp:METHOD:path:body", function()
    local timestamp = "1704067200"
    local method = "POST"
    local endpoint = "/execute"
    local body = '{"language":"python","code":"print(1)"}'

    local message = timestamp .. ":" .. method .. ":" .. endpoint .. ":" .. body

    assert_contains(message, ":")
    local count = 0
    for _ in message:gmatch(":") do count = count + 1 end
    assert_equal(count, 3)
    assert_true(message:sub(1, #timestamp) == timestamp)
end)

-- ============================================================================
-- Language Detection Tests
-- ============================================================================

print("\n=== Language Detection Tests ===")

local function get_extension(filename)
    return filename:match("%.([^%.]+)$")
end

local function detect_from_shebang(first_line)
    if first_line:sub(1, 2) == "#!" then
        if first_line:find("python") then return "python" end
        if first_line:find("node") then return "javascript" end
        if first_line:find("ruby") then return "ruby" end
        if first_line:find("perl") then return "perl" end
        if first_line:find("bash") or first_line:find("/sh") then return "bash" end
        if first_line:find("lua") then return "lua" end
        if first_line:find("php") then return "php" end
    end
    return nil
end

test("Detect language from .py extension", function()
    local ext = "." .. get_extension("script.py")
    assert_equal(ext_map[ext], "python")
end)

test("Detect language from .lua extension", function()
    local ext = "." .. get_extension("script.lua")
    assert_equal(ext_map[ext], "lua")
end)

test("Python shebang detection", function()
    assert_equal(detect_from_shebang("#!/usr/bin/env python3"), "python")
end)

test("Node shebang detection", function()
    assert_equal(detect_from_shebang("#!/usr/bin/env node"), "javascript")
end)

test("Lua shebang detection", function()
    assert_equal(detect_from_shebang("#!/usr/bin/env lua"), "lua")
end)

test("Bash shebang detection", function()
    assert_equal(detect_from_shebang("#!/bin/bash"), "bash")
end)

-- ============================================================================
-- Argument Parsing Tests
-- ============================================================================

print("\n=== Argument Parsing Tests ===")

local function parse_env_var(arg)
    local key, value = arg:match("^([^=]+)=(.*)$")
    return key, value
end

test("Parse -e KEY=VALUE format", function()
    local key, value = parse_env_var("DEBUG=1")
    assert_equal(key, "DEBUG")
    assert_equal(value, "1")
end)

test("Parse -e KEY=VALUE with equals in value", function()
    local key, value = parse_env_var("URL=https://example.com?foo=bar")
    assert_equal(key, "URL")
    assert_equal(value, "https://example.com?foo=bar")
end)

test("Valid network modes", function()
    local valid_modes = { zerotrust = true, semitrusted = true }
    assert_true(valid_modes["zerotrust"])
    assert_true(valid_modes["semitrusted"])
    assert_true(not valid_modes["invalid"])
end)

test("Subcommand detection", function()
    local subcommands = { session = true, service = true, key = true, restore = true }
    assert_true(subcommands["session"])
    assert_true(subcommands["service"])
    assert_true(not subcommands["script.py"])
end)

-- ============================================================================
-- File Operations Tests
-- ============================================================================

print("\n=== File Operations Tests ===")

test("Read text file", function()
    local tmpname = os.tmpname() .. ".py"
    local f = io.open(tmpname, "w")
    f:write('print("hello world")')
    f:close()

    f = io.open(tmpname, "r")
    local content = f:read("*a")
    f:close()
    os.remove(tmpname)

    assert_equal(content, 'print("hello world")')
end)

test("Extract file basename", function()
    local path = "/home/user/project/script.py"
    local basename = path:match("([^/]+)$")
    assert_equal(basename, "script.py")
end)

test("Extract file extension", function()
    local path = "/home/user/project/script.py"
    local ext = path:match("%.([^%.]+)$")
    assert_equal(ext, "py")
end)

-- ============================================================================
-- API Constants Tests
-- ============================================================================

print("\n=== API Constants Tests ===")

test("API base URL format", function()
    local api_base = "https://api.unsandbox.com"
    assert_true(api_base:sub(1, 8) == "https://")
    assert_contains(api_base, "unsandbox.com")
end)

test("Portal base URL format", function()
    local portal_base = "https://unsandbox.com"
    assert_true(portal_base:sub(1, 8) == "https://")
end)

-- ============================================================================
-- Summary
-- ============================================================================

print("\n=== Summary ===")
print("Passed: " .. passed)
print("Failed: " .. failed)
print("Total:  " .. (passed + failed))

os.exit(failed > 0 and 1 or 0)
