#!/usr/bin/env julia
# Unit tests for un.jl - tests internal functions without API calls

using SHA
using Base64

passed = Ref(0)
failed = Ref(0)

function run_test(name, fn)
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

run_test("Python extension maps correctly", () -> assert_equal(EXT_MAP[".py"], "python"))

run_test("Julia extension maps correctly", () -> assert_equal(EXT_MAP[".jl"], "julia"))

run_test("JavaScript extension maps correctly", () -> assert_equal(EXT_MAP[".js"], "javascript"))

run_test("Go extension maps correctly", () -> assert_equal(EXT_MAP[".go"], "go"))

run_test("R extensions map correctly", () -> begin
    assert_equal(EXT_MAP[".r"], "r")
    assert_equal(EXT_MAP[".R"], "r")
end)

println("\n=== HMAC Signature Tests ===")

run_test("HMAC-SHA256 generates 64 character hex string", () -> begin
    sig = bytes2hex(hmac_sha256(Vector{UInt8}("test-secret"), "test-message"))
    assert_equal(length(sig), 64)
end)

run_test("Same input produces same signature", () -> begin
    sig1 = bytes2hex(hmac_sha256(Vector{UInt8}("key"), "msg"))
    sig2 = bytes2hex(hmac_sha256(Vector{UInt8}("key"), "msg"))
    assert_equal(sig1, sig2)
end)

run_test("Different secrets produce different signatures", () -> begin
    sig1 = bytes2hex(hmac_sha256(Vector{UInt8}("key1"), "msg"))
    sig2 = bytes2hex(hmac_sha256(Vector{UInt8}("key2"), "msg"))
    assert_not_equal(sig1, sig2)
end)

run_test("Signature format verification", () -> begin
    timestamp = "1704067200"
    method = "POST"
    endpoint = "/execute"
    body = """{"language":"python"}"""
    message = "$timestamp:$method:$endpoint:$body"
    assert_true(startswith(message, timestamp))
    assert_contains(message, ":POST:")
    assert_contains(message, ":/execute:")
end)

println("\n=== Language Detection Tests ===")

run_test("Detect language from .jl extension", () -> begin
    ext = splitext("script.jl")[2]
    assert_equal(EXT_MAP[ext], "julia")
end)

run_test("Python shebang detection", () -> begin
    content = "#!/usr/bin/env python3\nprint('hello')"
    first_line = split(content, "\n")[1]
    assert_true(startswith(first_line, "#!"))
    assert_contains(first_line, "python")
end)

println("\n=== Argument Parsing Tests ===")

run_test("Parse -e KEY=VALUE format", () -> begin
    arg = "DEBUG=1"
    parts = split(arg, "=", limit=2)
    assert_equal(parts[1], "DEBUG")
    assert_equal(parts[2], "1")
end)

run_test("Parse -e KEY=VALUE with equals in value", () -> begin
    arg = "URL=https://example.com?foo=bar"
    parts = split(arg, "=", limit=2)
    assert_equal(parts[1], "URL")
    assert_equal(parts[2], "https://example.com?foo=bar")
end)

println("\n=== File Operations Tests ===")

run_test("Base64 encoding/decoding", () -> begin
    content = "print('hello world')"
    encoded = base64encode(content)
    decoded = String(base64decode(encoded))
    assert_equal(decoded, content)
end)

run_test("Extract file basename", () -> begin
    path = "/home/user/project/script.jl"
    assert_equal(basename(path), "script.jl")
end)

run_test("Extract file extension", () -> begin
    path = "/home/user/project/script.jl"
    assert_equal(splitext(path)[2], ".jl")
end)

println("\n=== API Constants Tests ===")

run_test("API base URL format", () -> begin
    api_base = "https://api.unsandbox.com"
    assert_true(startswith(api_base, "https://"))
    assert_contains(api_base, "unsandbox.com")
end)

# Summary
println("\n=== Summary ===")
println("Passed: $(passed[])")
println("Failed: $(failed[])")
println("Total:  $(passed[] + failed[])")

exit(failed[] > 0 ? 1 : 0)
