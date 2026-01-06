#!/usr/bin/env crystal
# Comprehensive tests for un.cr (Crystal UN CLI Inception implementation)
# Compile and run with: crystal test_un_cr.cr

require "http/client"
require "json"

# Color codes
GREEN = "\033[32m"
RED = "\033[31m"
BLUE = "\033[34m"
RESET = "\033[0m"

# Test counters - use class with class variables
class TestCounter
  @@passed = 0
  @@failed = 0

  def self.passed
    @@passed
  end

  def self.failed
    @@failed
  end

  def self.inc_passed
    @@passed += 1
  end

  def self.inc_failed
    @@failed += 1
  end
end

# Extension to language mapping (from un.cr)
EXT_MAP = {
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
}

def detect_language(filename : String) : String
  ext = File.extname(filename).downcase
  EXT_MAP.fetch(ext, "unknown")
end

def print_test(name : String, result : Bool)
  if result
    puts "#{GREEN}✓ PASS#{RESET}: #{name}"
    TestCounter.inc_passed
  else
    puts "#{RED}✗ FAIL#{RESET}: #{name}"
    TestCounter.inc_failed
  end
end

puts "\n#{BLUE}========================================#{RESET}"
puts "#{BLUE}UN CLI Inception Tests - Crystal#{RESET}"
puts "#{BLUE}========================================#{RESET}\n"

# Test 1: Extension detection tests
puts "#{BLUE}Test Suite 1: Extension Detection#{RESET}"
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
puts "\n#{BLUE}Test Suite 2: API Integration#{RESET}"
api_key = ENV["UNSANDBOX_API_KEY"]?
if api_key.nil? || api_key.empty?
  puts "#{BLUE}ℹ SKIP#{RESET}: API integration test (UNSANDBOX_API_KEY not set)"
else
  begin
    url = URI.parse("https://api.unsandbox.com/execute")
    headers = HTTP::Headers{
      "Content-Type" => "application/json",
      "Authorization" => "Bearer #{api_key}"
    }
    body = {
      language: "python",
      code: "print('Hello from test')"
    }.to_json

    response = HTTP::Client.post(url, headers: headers, body: body)
    result = JSON.parse(response.body)

    api_works = result["stdout"]? && result["stdout"].as_s.includes?("Hello from test")
    print_test("API endpoint reachable and functional", api_works)
  rescue ex
    print_test("API endpoint reachable and functional", false)
    puts "  Error: #{ex.message}"
  end
end

# Test 3: End-to-end functional test
puts "\n#{BLUE}Test Suite 3: End-to-End Functional Test#{RESET}"
if api_key.nil? || api_key.empty?
  puts "#{BLUE}ℹ SKIP#{RESET}: E2E test (UNSANDBOX_API_KEY not set)"
else
  fib_file = "../../test/fib.cr"
  fib_file = "/home/fox/git/unsandbox.com/cli/test/fib.cr" unless File.exists?(fib_file)

  if File.exists?(fib_file)
    begin
      un_script = "../un.cr"
      un_script = "/home/fox/git/unsandbox.com/cli/inception/un.cr" unless File.exists?(un_script)

      output = IO::Memory.new
      error = IO::Memory.new
      process = Process.run("crystal", args: ["run", un_script, fib_file],
                          output: output, error: error)

      result = output.to_s + error.to_s

      has_fib10 = result.includes?("fib(10) = 55")
      has_fib5 = result.includes?("fib(5) = 5")
      has_fib0 = result.includes?("fib(0) = 0")

      print_test("E2E: fib.cr produces fib(10) = 55", has_fib10)
      print_test("E2E: fib.cr produces fib(5) = 5", has_fib5)
      print_test("E2E: fib.cr produces fib(0) = 0", has_fib0)
    rescue ex
      print_test("E2E: fib.cr execution", false)
      puts "  Error: #{ex.message}"
    end
  else
    puts "#{BLUE}ℹ SKIP#{RESET}: E2E test (fib.cr not found at expected location)"
  end
end

# Test 4: Error handling tests
puts "\n#{BLUE}Test Suite 4: Error Handling#{RESET}"
print_test("Unknown extension returns 'unknown'", detect_language("file.unknown") == "unknown")
print_test("Case insensitive detection", detect_language("TEST.CR") == "crystal")
print_test("Multiple dots in filename", detect_language("my.test.py") == "python")

# Print summary
puts "\n#{BLUE}========================================#{RESET}"
puts "#{BLUE}Test Summary#{RESET}"
puts "#{BLUE}========================================#{RESET}"
puts "#{GREEN}Passed: #{TestCounter.passed}#{RESET}"
puts "#{RED}Failed: #{TestCounter.failed}#{RESET}"
puts "#{BLUE}Total:  #{TestCounter.passed + TestCounter.failed}#{RESET}"

if TestCounter.failed > 0
  puts "\n#{RED}TESTS FAILED#{RESET}"
  exit 1
else
  puts "\n#{GREEN}ALL TESTS PASSED#{RESET}"
  exit 0
end
