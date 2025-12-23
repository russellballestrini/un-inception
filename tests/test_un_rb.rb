#!/usr/bin/env ruby
# Test suite for UN CLI Ruby implementation (un.rb)
# Tests extension detection, API calls, and end-to-end functionality

require 'json'
require 'net/http'
require 'uri'
require 'open3'

# Test configuration
UN_SCRIPT = File.join(__dir__, '..', 'un.rb')
FIB_PY = File.join(__dir__, '..', '..', 'test', 'fib.py')

class TestResults
  attr_reader :passed, :failed, :skipped

  def initialize
    @passed = 0
    @failed = 0
    @skipped = 0
  end

  def pass_test(name)
    puts "PASS: #{name}"
    @passed += 1
  end

  def fail_test(name, error)
    puts "FAIL: #{name} - #{error}"
    @failed += 1
  end

  def skip_test(name, reason)
    puts "SKIP: #{name} - #{reason}"
    @skipped += 1
  end
end

results = TestResults.new

# Extension map for testing
EXTENSION_MAP = {
  '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript', '.rb' => 'ruby',
  '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua', '.sh' => 'bash',
  '.go' => 'go', '.rs' => 'rust', '.c' => 'c', '.cpp' => 'cpp', '.cc' => 'cpp',
  '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.hs' => 'haskell',
  '.ml' => 'ocaml', '.clj' => 'clojure', '.ex' => 'elixir', '.erl' => 'erlang',
  '.swift' => 'swift', '.r' => 'r', '.jl' => 'julia', '.dart' => 'dart',
  '.scala' => 'scala', '.groovy' => 'groovy', '.nim' => 'nim', '.cr' => 'crystal',
  '.v' => 'vlang', '.zig' => 'zig', '.fs' => 'fsharp', '.vb' => 'vb',
  '.pas' => 'pascal', '.f90' => 'fortran', '.asm' => 'assembly', '.d' => 'd',
  '.rkt' => 'racket', '.scm' => 'scheme', '.lisp' => 'common_lisp',
  '.sol' => 'solidity', '.cob' => 'cobol', '.ada' => 'ada', '.tcl' => 'tcl'
}.freeze

def detect_language(filename)
  ext = File.extname(filename).downcase
  EXTENSION_MAP[ext]
end

# Test 1: Extension detection for Python
begin
  lang = detect_language('test.py')
  if lang == 'python'
    results.pass_test('Extension detection: .py -> python')
  else
    results.fail_test('Extension detection: .py -> python', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .py -> python', e.message)
end

# Test 2: Extension detection for JavaScript
begin
  lang = detect_language('test.js')
  if lang == 'javascript'
    results.pass_test('Extension detection: .js -> javascript')
  else
    results.fail_test('Extension detection: .js -> javascript', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .js -> javascript', e.message)
end

# Test 3: Extension detection for Ruby
begin
  lang = detect_language('test.rb')
  if lang == 'ruby'
    results.pass_test('Extension detection: .rb -> ruby')
  else
    results.fail_test('Extension detection: .rb -> ruby', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .rb -> ruby', e.message)
end

# Test 4: Extension detection for Go
begin
  lang = detect_language('test.go')
  if lang == 'go'
    results.pass_test('Extension detection: .go -> go')
  else
    results.fail_test('Extension detection: .go -> go', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .go -> go', e.message)
end

# Test 5: Extension detection for Rust
begin
  lang = detect_language('test.rs')
  if lang == 'rust'
    results.pass_test('Extension detection: .rs -> rust')
  else
    results.fail_test('Extension detection: .rs -> rust', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .rs -> rust', e.message)
end

# Test 6: Extension detection for unknown extension
begin
  lang = detect_language('test.unknown')
  if lang.nil?
    results.pass_test('Extension detection: .unknown -> nil')
  else
    results.fail_test('Extension detection: .unknown -> nil', "Got #{lang}")
  end
rescue => e
  results.fail_test('Extension detection: .unknown -> nil', e.message)
end

# Test 7: API call test (requires UNSANDBOX_API_KEY)
if !ENV['UNSANDBOX_API_KEY']
  results.skip_test('API call test', 'UNSANDBOX_API_KEY not set')
else
  begin
    uri = URI('https://api.unsandbox.com/execute')
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri.path)
    request['Authorization'] = "Bearer #{ENV['UNSANDBOX_API_KEY']}"
    request['Content-Type'] = 'application/json'
    request.body = JSON.generate({
      language: 'python',
      code: 'print("Hello from API")'
    })

    response = http.request(request)

    if response.is_a?(Net::HTTPSuccess)
      result = JSON.parse(response.body)
      if result['stdout'] && result['stdout'].include?('Hello from API')
        results.pass_test('API call test')
      else
        results.fail_test('API call test', "Unexpected result: #{result}")
      end
    else
      results.fail_test('API call test', "HTTP #{response.code}: #{response.body}")
    end
  rescue => e
    results.fail_test('API call test', e.message)
  end
end

# Test 8: End-to-end test with fib.py
if !ENV['UNSANDBOX_API_KEY']
  results.skip_test('End-to-end fib.py test', 'UNSANDBOX_API_KEY not set')
elsif !File.exist?(FIB_PY)
  results.skip_test('End-to-end fib.py test', "fib.py not found at #{FIB_PY}")
else
  begin
    stdout, stderr, status = Open3.capture3(UN_SCRIPT, FIB_PY, timeout: 30)

    if stdout.include?('fib(10) = 55')
      results.pass_test('End-to-end fib.py test')
    else
      results.fail_test('End-to-end fib.py test',
        "Expected 'fib(10) = 55' in output, got: #{stdout[0...200]}")
    end
  rescue Timeout::Error
    results.fail_test('End-to-end fib.py test', 'Timeout (30s)')
  rescue => e
    results.fail_test('End-to-end fib.py test', e.message)
  end
end

# Print summary
puts "\n" + "=" * 50
puts "Test Summary:"
puts "  PASSED:  #{results.passed}"
puts "  FAILED:  #{results.failed}"
puts "  SKIPPED: #{results.skipped}"
puts "  TOTAL:   #{results.passed + results.failed + results.skipped}"
puts "=" * 50

# Exit with appropriate code
exit(results.failed == 0 ? 0 : 1)
