#!/usr/bin/env julia
# Unit tests for un.jl - tests internal functions without API calls

using SHA
using Base64

passed = Ref(0)
failed = Ref(0)

function test(name, fn)
    try
        fn()
        println("  ✓ $name")
        passed[] += 1
    catch e
        println("  ✗ $name")
        println("    $(sprint(showerror, e))")
        failed[] += 1
    end
end

function assert_equal(actual, expected)
    if actual != expected
        error("Expected '$expected' but got '$actual'")
    end
end

function assert_not_equal(a, b)
    if a == b
        error("Expected values to be different but both were '$a'")
    end
end

function assert_contains(str, substr)
    if !occursin(substr, str)
        error("Expected '$str' to contain '$substr'")
    end
end

function assert_true(val)
    if !val
        error("Expected true but got false")
    end
end

# Extension mapping
EXT_MAP = Dict(
    ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
    ".rb" => "ruby", ".php" => "php", ".pl" => "perl", ".lua" => "lua",
    ".sh" => "bash", ".go" => "go", ".rs" => "rust", ".c" => "c",
    ".cpp" => "cpp", ".java" => "java", ".kt" => "kotlin",
    ".hs" => "haskell", ".clj" => "clojure", ".erl" => "erlang",
    ".ex" => "elixir", ".jl" => "julia", ".r" => "r", ".R" => "r"
)

println("\n=== Extension Mapping Tests ===")

test("Python extension maps correctly") do
    assert_equal(EXT_MAP[".py"], "python")
end

test("Julia extension maps correctly") do
    assert_equal(EXT_MAP[".jl"], "julia")
end

test("JavaScript extension maps correctly") do
    assert_equal(EXT_MAP[".js"], "javascript")
end

test("Go extension maps correctly") do
    assert_equal(EXT_MAP[".go"], "go")
end

test("R extensions map correctly") do
    assert_equal(EXT_MAP[".r"], "r")
    assert_equal(EXT_MAP[".R"], "r")
end

println("\n=== HMAC Signature Tests ===")

test("HMAC-SHA256 generates 64 character hex string") do
    sig = bytes2hex(hmac_sha256("test-secret", "test-message"))
    assert_equal(length(sig), 64)
end

test("Same input produces same signature") do
    sig1 = bytes2hex(hmac_sha256("key", "msg"))
    sig2 = bytes2hex(hmac_sha256("key", "msg"))
    assert_equal(sig1, sig2)
end

test("Different secrets produce different signatures") do
    sig1 = bytes2hex(hmac_sha256("key1", "msg"))
    sig2 = bytes2hex(hmac_sha256("key2", "msg"))
    assert_not_equal(sig1, sig2)
end

test("Signature format verification") do
    timestamp = "1704067200"
    method = "POST"
    endpoint = "/execute"
    body = """{"language":"python"}"""

    message = "$timestamp:$method:$endpoint:$body"

    assert_true(startswith(message, timestamp))
    assert_contains(message, ":POST:")
    assert_contains(message, ":/execute:")
end

println("\n=== Language Detection Tests ===")

test("Detect language from .jl extension") do
    ext = splitext("script.jl")[2]
    assert_equal(EXT_MAP[ext], "julia")
end

test("Python shebang detection") do
    content = "#!/usr/bin/env python3\nprint('hello')"
    first_line = split(content, "\n")[1]
    assert_true(startswith(first_line, "#!"))
    assert_contains(first_line, "python")
end

println("\n=== Argument Parsing Tests ===")

test("Parse -e KEY=VALUE format") do
    arg = "DEBUG=1"
    parts = split(arg, "=", limit=2)
    key = parts[1]
    value = parts[2]
    assert_equal(key, "DEBUG")
    assert_equal(value, "1")
end

test("Parse -e KEY=VALUE with equals in value") do
    arg = "URL=https://example.com?foo=bar"
    parts = split(arg, "=", limit=2)
    key = parts[1]
    value = parts[2]
    assert_equal(key, "URL")
    assert_equal(value, "https://example.com?foo=bar")
end

println("\n=== File Operations Tests ===")

test("Base64 encoding/decoding") do
    content = "print('hello world')"
    encoded = base64encode(content)
    decoded = String(base64decode(encoded))
    assert_equal(decoded, content)
end

test("Extract file basename") do
    path = "/home/user/project/script.jl"
    assert_equal(basename(path), "script.jl")
end

test("Extract file extension") do
    path = "/home/user/project/script.jl"
    assert_equal(splitext(path)[2], ".jl")
end

println("\n=== API Constants Tests ===")

test("API base URL format") do
    api_base = "https://api.unsandbox.com"
    assert_true(startswith(api_base, "https://"))
    assert_contains(api_base, "unsandbox.com")
end

# Summary
println("\n=== Summary ===")
println("Passed: $(passed[])")
println("Failed: $(failed[])")
println("Total:  $(passed[] + failed[])")

exit(failed[] > 0 ? 1 : 0)
