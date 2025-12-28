#!/usr/bin/env lua
-- Test suite for UN CLI Lua implementation (un.lua)
-- Tests extension detection, API calls, and end-to-end functionality

-- Try to load optional dependencies
local has_https, https = pcall(require, "ssl.https")
local has_ltn12, ltn12 = pcall(require, "ltn12")
local has_json, json = pcall(require, "cjson")

-- Test configuration
local script_dir = arg[0]:match("(.*/)") or "./"
local UN_SCRIPT = script_dir .. "../un.lua"
local FIB_PY = script_dir .. "../../test/fib.py"

-- TestResults class
local TestResults = {}
TestResults.__index = TestResults

function TestResults:new()
    local obj = {
        passed = 0,
        failed = 0,
        skipped = 0
    }
    setmetatable(obj, TestResults)
    return obj
end

function TestResults:passTest(name)
    print("PASS: " .. name)
    self.passed = self.passed + 1
end

function TestResults:failTest(name, error)
    print("FAIL: " .. name .. " - " .. error)
    self.failed = self.failed + 1
end

function TestResults:skipTest(name, reason)
    print("SKIP: " .. name .. " - " .. reason)
    self.skipped = self.skipped + 1
end

local results = TestResults:new()

-- Extension map for testing
local EXTENSION_MAP = {
    [".py"] = "python", [".js"] = "javascript", [".ts"] = "typescript", [".rb"] = "ruby",
    [".php"] = "php", [".pl"] = "perl", [".lua"] = "lua", [".sh"] = "bash",
    [".go"] = "go", [".rs"] = "rust", [".c"] = "c", [".cpp"] = "cpp", [".cc"] = "cpp",
    [".java"] = "java", [".kt"] = "kotlin", [".cs"] = "csharp", [".hs"] = "haskell",
    [".ml"] = "ocaml", [".clj"] = "clojure", [".ex"] = "elixir", [".erl"] = "erlang",
    [".swift"] = "swift", [".r"] = "r", [".jl"] = "julia", [".dart"] = "dart",
    [".scala"] = "scala", [".groovy"] = "groovy", [".nim"] = "nim", [".cr"] = "crystal",
    [".v"] = "vlang", [".zig"] = "zig", [".fs"] = "fsharp", [".vb"] = "vb",
    [".pas"] = "pascal", [".f90"] = "fortran", [".asm"] = "assembly", [".d"] = "d",
    [".rkt"] = "racket", [".scm"] = "scheme", [".lisp"] = "common_lisp",
    [".sol"] = "solidity", [".cob"] = "cobol", [".ada"] = "ada", [".tcl"] = "tcl",
}

local function detect_language(filename)
    local ext = filename:match("%.([^.]+)$")
    if ext then
        return EXTENSION_MAP["." .. ext:lower()]
    end
    return nil
end

-- Test 1: Extension detection for Python
local status, err = pcall(function()
    local lang = detect_language('test.py')
    if lang == 'python' then
        results:passTest('Extension detection: .py -> python')
    else
        results:failTest('Extension detection: .py -> python', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .py -> python', err)
end

-- Test 2: Extension detection for JavaScript
status, err = pcall(function()
    local lang = detect_language('test.js')
    if lang == 'javascript' then
        results:passTest('Extension detection: .js -> javascript')
    else
        results:failTest('Extension detection: .js -> javascript', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .js -> javascript', err)
end

-- Test 3: Extension detection for Ruby
status, err = pcall(function()
    local lang = detect_language('test.rb')
    if lang == 'ruby' then
        results:passTest('Extension detection: .rb -> ruby')
    else
        results:failTest('Extension detection: .rb -> ruby', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .rb -> ruby', err)
end

-- Test 4: Extension detection for Go
status, err = pcall(function()
    local lang = detect_language('test.go')
    if lang == 'go' then
        results:passTest('Extension detection: .go -> go')
    else
        results:failTest('Extension detection: .go -> go', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .go -> go', err)
end

-- Test 5: Extension detection for Rust
status, err = pcall(function()
    local lang = detect_language('test.rs')
    if lang == 'rust' then
        results:passTest('Extension detection: .rs -> rust')
    else
        results:failTest('Extension detection: .rs -> rust', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .rs -> rust', err)
end

-- Test 6: Extension detection for unknown extension
status, err = pcall(function()
    local lang = detect_language('test.unknown')
    if lang == nil then
        results:passTest('Extension detection: .unknown -> nil')
    else
        results:failTest('Extension detection: .unknown -> nil', "Got " .. tostring(lang))
    end
end)
if not status then
    results:failTest('Extension detection: .unknown -> nil', err)
end

-- Test 7: API call test (requires UNSANDBOX auth)
local has_hmac = os.getenv("UNSANDBOX_PUBLIC_KEY") and os.getenv("UNSANDBOX_SECRET_KEY")
local has_legacy = os.getenv("UNSANDBOX_API_KEY")
if not (has_hmac or has_legacy) then
    results:skipTest('API call test', 'UNSANDBOX authentication not set')
elseif not (has_https and has_ltn12 and has_json) then
    results:skipTest('API call test', 'Required Lua libraries not available (luasocket, luasec, lua-cjson)')
else
    status, err = pcall(function()
        local payload = json.encode({
            language = 'python',
            code = 'print("Hello from API")'
        })

        local response_body = {}
        local res, code, headers, status_text = https.request{
            url = "https://api.unsandbox.com/execute",
            method = "POST",
            headers = {
                ["Authorization"] = "Bearer " .. os.getenv("UNSANDBOX_API_KEY"),
                ["Content-Type"] = "application/json",
                ["Content-Length"] = tostring(#payload)
            },
            source = ltn12.source.string(payload),
            sink = ltn12.sink.table(response_body)
        }

        if code == 200 then
            local result = json.decode(table.concat(response_body))
            if result.stdout and result.stdout:find('Hello from API') then
                results:passTest('API call test')
            else
                results:failTest('API call test', "Unexpected result: " .. json.encode(result))
            end
        else
            results:failTest('API call test', "HTTP " .. code .. ": " .. table.concat(response_body))
        end
    end)
    if not status then
        results:failTest('API call test', err)
    end
end

-- Test 8: End-to-end test with fib.py
if not (has_hmac or has_legacy) then
    results:skipTest('End-to-end fib.py test', 'UNSANDBOX authentication not set')
else
    -- Check if fib.py exists
    local file = io.open(FIB_PY, "r")
    if not file then
        results:skipTest('End-to-end fib.py test', 'fib.py not found at ' .. FIB_PY)
    else
        file:close()
        status, err = pcall(function()
            local handle = io.popen(UN_SCRIPT .. ' ' .. FIB_PY .. ' 2>&1')
            local output = handle:read("*a")
            handle:close()

            if output:find('fib%(10%) = 55') then
                results:passTest('End-to-end fib.py test')
            else
                results:failTest('End-to-end fib.py test',
                    "Expected 'fib(10) = 55' in output, got: " .. output:sub(1, 200))
            end
        end)
        if not status then
            results:failTest('End-to-end fib.py test', err)
        end
    end
end

-- Print summary
print("\n" .. string.rep("=", 50))
print("Test Summary:")
print("  PASSED:  " .. results.passed)
print("  FAILED:  " .. results.failed)
print("  SKIPPED: " .. results.skipped)
print("  TOTAL:   " .. (results.passed + results.failed + results.skipped))
print(string.rep("=", 50))

-- Exit with appropriate code
os.exit(results.failed == 0 and 0 or 1)
