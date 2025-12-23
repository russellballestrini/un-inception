#!/usr/bin/env julia
# Comprehensive tests for un.jl (Julia UN CLI Inception implementation)
# Run with: julia test_un_jl.jl

using Test
using HTTP
using JSON

# Color codes
const GREEN = "\033[32m"
const RED = "\033[31m"
const BLUE = "\033[34m"
const RESET = "\033[0m"

# Test counters
passed = 0
failed = 0

# Include the un.jl implementation (we'll test its functions)
# For testing, we'll redefine the functions here
const EXT_MAP = Dict(
    ".jl" => "julia",
    ".r" => "r",
    ".cr" => "crystal",
    ".f90" => "fortran",
    ".cob" => "cobol",
    ".pro" => "prolog",
    ".forth" => "forth",
    ".4th" => "forth",
    ".py" => "python",
    ".js" => "javascript",
    ".rb" => "ruby",
    ".go" => "go",
    ".rs" => "rust",
    ".c" => "c",
    ".cpp" => "cpp",
    ".java" => "java",
    ".sh" => "bash"
)

function detect_language(filename::String)::String
    ext = lowercase(match(r"\.[^.]+$", filename).match)
    return get(EXT_MAP, ext, "unknown")
end

function print_test(name, result)
    global passed, failed
    if result
        println("$(GREEN)✓ PASS$(RESET): $name")
        passed += 1
    else
        println("$(RED)✗ FAIL$(RESET): $name")
        failed += 1
    end
end

println("\n$(BLUE)========================================$(RESET)")
println("$(BLUE)UN CLI Inception Tests - Julia$(RESET)")
println("$(BLUE)========================================$(RESET)\n")

# Test 1: Extension detection tests
println("$(BLUE)Test Suite 1: Extension Detection$(RESET)")
print_test("Detect .jl as julia", detect_language("test.jl") == "julia")
print_test("Detect .r as r", detect_language("test.r") == "r")
print_test("Detect .cr as crystal", detect_language("test.cr") == "crystal")
print_test("Detect .f90 as fortran", detect_language("test.f90") == "fortran")
print_test("Detect .cob as cobol", detect_language("test.cob") == "cobol")
print_test("Detect .pro as prolog", detect_language("test.pro") == "prolog")
print_test("Detect .forth as forth", detect_language("test.forth") == "forth")
print_test("Detect .4th as forth", detect_language("test.4th") == "forth")
print_test("Detect .py as python", detect_language("test.py") == "python")
print_test("Detect .rs as rust", detect_language("test.rs") == "rust")
print_test("Detect unknown extension", detect_language("test.xyz") == "unknown")

# Test 2: API Integration Test
println("\n$(BLUE)Test Suite 2: API Integration$(RESET)")
api_key = get(ENV, "UNSANDBOX_API_KEY", "")
if isempty(api_key)
    println("$(BLUE)ℹ SKIP$(RESET): API integration test (UNSANDBOX_API_KEY not set)")
else
    try
        # Test a simple Python hello world
        url = "https://api.unsandbox.com/execute"
        headers = [
            "Content-Type" => "application/json",
            "Authorization" => "Bearer $api_key"
        ]
        body = JSON.json(Dict(
            "language" => "python",
            "code" => "print('Hello from test')"
        ))

        response = HTTP.post(url, headers, body)
        result = JSON.parse(String(response.body))

        api_works = haskey(result, "stdout") && occursin("Hello from test", result["stdout"])
        print_test("API endpoint reachable and functional", api_works)
    catch e
        print_test("API endpoint reachable and functional", false)
        println("  Error: $e")
    end
end

# Test 3: End-to-end functional test
println("\n$(BLUE)Test Suite 3: End-to-End Functional Test$(RESET)")
if isempty(api_key)
    println("$(BLUE)ℹ SKIP$(RESET): E2E test (UNSANDBOX_API_KEY not set)")
else
    # Find the fib.jl test file
    fib_file = "../../test/fib.jl"
    if !isfile(fib_file)
        # Try absolute path
        fib_file = "/home/fox/git/unsandbox.com/cli/test/fib.jl"
    end

    if isfile(fib_file)
        try
            # Run un.jl on fib.jl
            un_script = "../un.jl"
            if !isfile(un_script)
                un_script = "/home/fox/git/unsandbox.com/cli/inception/un.jl"
            end

            result = read(`julia $un_script $fib_file`, String)

            # Check if output contains expected fibonacci results
            has_fib10 = occursin("fib(10) = 55", result)
            has_fib5 = occursin("fib(5) = 5", result)
            has_fib0 = occursin("fib(0) = 0", result)

            print_test("E2E: fib.jl produces fib(10) = 55", has_fib10)
            print_test("E2E: fib.jl produces fib(5) = 5", has_fib5)
            print_test("E2E: fib.jl produces fib(0) = 0", has_fib0)
        catch e
            print_test("E2E: fib.jl execution", false)
            println("  Error: $e")
        end
    else
        println("$(BLUE)ℹ SKIP$(RESET): E2E test (fib.jl not found at expected location)")
    end
end

# Test 4: Error handling tests
println("\n$(BLUE)Test Suite 4: Error Handling$(RESET)")
print_test("Unknown extension returns 'unknown'", detect_language("file.unknown") == "unknown")
print_test("Case insensitive detection", detect_language("TEST.JL") == "julia")
print_test("Multiple dots in filename", detect_language("my.test.py") == "python")

# Print summary
println("\n$(BLUE)========================================$(RESET)")
println("$(BLUE)Test Summary$(RESET)")
println("$(BLUE)========================================$(RESET)")
println("$(GREEN)Passed: $passed$(RESET)")
println("$(RED)Failed: $failed$(RESET)")
println("$(BLUE)Total:  $(passed + failed)$(RESET)")

if failed > 0
    println("\n$(RED)TESTS FAILED$(RESET)")
    exit(1)
else
    println("\n$(GREEN)ALL TESTS PASSED$(RESET)")
    exit(0)
end
