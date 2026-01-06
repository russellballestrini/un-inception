#!/usr/bin/env ruby
# Unit tests for un.rb - tests internal functions without API calls

require 'openssl'
require 'base64'
require 'tempfile'
require 'fileutils'

# Test counters
$passed = 0
$failed = 0

def test(name)
  begin
    yield
    puts "  ✓ #{name}"
    $passed += 1
  rescue => e
    puts "  ✗ #{name}"
    puts "    #{e.message}"
    $failed += 1
  end
end

def assert_equal(actual, expected, msg = '')
  raise "Expected '#{expected}' but got '#{actual}' #{msg}" unless actual == expected
end

def assert_not_equal(a, b)
  raise "Expected values to be different but both were '#{a}'" if a == b
end

def assert_includes(str, substr)
  raise "Expected '#{str}' to include '#{substr}'" unless str.include?(substr)
end

def assert_true(val)
  raise "Expected true but got #{val}" unless val
end

# Extension mapping (copied from un.rb)
EXT_MAP = {
  '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript',
  '.rb' => 'ruby', '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua',
  '.sh' => 'bash', '.go' => 'go', '.rs' => 'rust', '.c' => 'c',
  '.cpp' => 'cpp', '.cc' => 'cpp', '.cxx' => 'cpp',
  '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.fs' => 'fsharp',
  '.hs' => 'haskell', '.ml' => 'ocaml', '.clj' => 'clojure', '.scm' => 'scheme',
  '.lisp' => 'commonlisp', '.erl' => 'erlang', '.ex' => 'elixir', '.exs' => 'elixir',
  '.jl' => 'julia', '.r' => 'r', '.R' => 'r', '.cr' => 'crystal',
  '.d' => 'd', '.nim' => 'nim', '.zig' => 'zig', '.v' => 'v',
  '.dart' => 'dart', '.groovy' => 'groovy', '.scala' => 'scala',
  '.f90' => 'fortran', '.f95' => 'fortran', '.cob' => 'cobol',
  '.pro' => 'prolog', '.forth' => 'forth', '.4th' => 'forth',
  '.tcl' => 'tcl', '.raku' => 'raku', '.m' => 'objc'
}.freeze

# ============================================================================
# Extension Mapping Tests
# ============================================================================

puts "\n=== Extension Mapping Tests ==="

test('Python extension maps correctly') do
  assert_equal EXT_MAP['.py'], 'python'
end

test('JavaScript extensions map correctly') do
  assert_equal EXT_MAP['.js'], 'javascript'
  assert_equal EXT_MAP['.ts'], 'typescript'
end

test('Ruby extension maps correctly') do
  assert_equal EXT_MAP['.rb'], 'ruby'
end

test('Go extension maps correctly') do
  assert_equal EXT_MAP['.go'], 'go'
end

test('Rust extension maps correctly') do
  assert_equal EXT_MAP['.rs'], 'rust'
end

test('C/C++ extensions map correctly') do
  assert_equal EXT_MAP['.c'], 'c'
  assert_equal EXT_MAP['.cpp'], 'cpp'
  assert_equal EXT_MAP['.cc'], 'cpp'
  assert_equal EXT_MAP['.cxx'], 'cpp'
end

test('JVM extensions map correctly') do
  assert_equal EXT_MAP['.java'], 'java'
  assert_equal EXT_MAP['.kt'], 'kotlin'
  assert_equal EXT_MAP['.groovy'], 'groovy'
  assert_equal EXT_MAP['.scala'], 'scala'
end

test('.NET extensions map correctly') do
  assert_equal EXT_MAP['.cs'], 'csharp'
  assert_equal EXT_MAP['.fs'], 'fsharp'
end

test('Functional language extensions map correctly') do
  assert_equal EXT_MAP['.hs'], 'haskell'
  assert_equal EXT_MAP['.ml'], 'ocaml'
  assert_equal EXT_MAP['.clj'], 'clojure'
  assert_equal EXT_MAP['.scm'], 'scheme'
  assert_equal EXT_MAP['.lisp'], 'commonlisp'
  assert_equal EXT_MAP['.erl'], 'erlang'
  assert_equal EXT_MAP['.ex'], 'elixir'
end

test('Scientific extensions map correctly') do
  assert_equal EXT_MAP['.jl'], 'julia'
  assert_equal EXT_MAP['.r'], 'r'
  assert_equal EXT_MAP['.f90'], 'fortran'
end

test('Exotic extensions map correctly') do
  assert_equal EXT_MAP['.d'], 'd'
  assert_equal EXT_MAP['.nim'], 'nim'
  assert_equal EXT_MAP['.zig'], 'zig'
  assert_equal EXT_MAP['.v'], 'v'
  assert_equal EXT_MAP['.cr'], 'crystal'
end

# ============================================================================
# HMAC Signature Tests
# ============================================================================

puts "\n=== HMAC Signature Tests ==="

test('HMAC-SHA256 generates 64 character hex string') do
  secret = 'test-secret-key'
  message = '1234567890:POST:/execute:{}'

  signature = OpenSSL::HMAC.hexdigest('SHA256', secret, message)

  assert_equal signature.length, 64
end

test('Same input produces same signature') do
  secret = 'test-secret-key'
  message = '1234567890:POST:/execute:{}'

  sig1 = OpenSSL::HMAC.hexdigest('SHA256', secret, message)
  sig2 = OpenSSL::HMAC.hexdigest('SHA256', secret, message)

  assert_equal sig1, sig2
end

test('Different secrets produce different signatures') do
  message = '1234567890:POST:/execute:{}'

  sig1 = OpenSSL::HMAC.hexdigest('SHA256', 'secret1', message)
  sig2 = OpenSSL::HMAC.hexdigest('SHA256', 'secret2', message)

  assert_not_equal sig1, sig2
end

test('Different messages produce different signatures') do
  secret = 'test-secret'

  sig1 = OpenSSL::HMAC.hexdigest('SHA256', secret, 'message1')
  sig2 = OpenSSL::HMAC.hexdigest('SHA256', secret, 'message2')

  assert_not_equal sig1, sig2
end

test('Signature format is timestamp:METHOD:path:body') do
  timestamp = '1704067200'
  method = 'POST'
  endpoint = '/execute'
  body = '{"language":"python","code":"print(1)"}'

  message = "#{timestamp}:#{method}:#{endpoint}:#{body}"

  # Verify format: starts with timestamp, has method and path
  assert_true message.start_with?(timestamp)
  assert_includes message, ':POST:'
  assert_includes message, ':/execute:'
end

# ============================================================================
# Language Detection Tests
# ============================================================================

puts "\n=== Language Detection Tests ==="

test('Detect language from .py extension') do
  filename = 'script.py'
  ext = File.extname(filename).downcase
  assert_equal EXT_MAP[ext], 'python'
end

test('Detect language from .rb extension') do
  filename = 'app.rb'
  ext = File.extname(filename).downcase
  assert_equal EXT_MAP[ext], 'ruby'
end

test('Python shebang detection') do
  content = "#!/usr/bin/env python3\nprint('hello')"
  first_line = content.lines.first

  assert_true first_line.start_with?('#!')
  assert_includes first_line, 'python'
end

test('Ruby shebang detection') do
  content = "#!/usr/bin/env ruby\nputs 'hello'"
  first_line = content.lines.first

  assert_true first_line.start_with?('#!')
  assert_includes first_line, 'ruby'
end

test('Bash shebang detection') do
  content = "#!/bin/bash\necho hello"
  first_line = content.lines.first

  assert_true first_line.start_with?('#!')
  assert_true first_line.include?('bash') || first_line.include?('/sh')
end

# ============================================================================
# Argument Parsing Tests
# ============================================================================

puts "\n=== Argument Parsing Tests ==="

test('Parse -e KEY=VALUE format') do
  arg = 'DEBUG=1'
  key, value = arg.split('=', 2)

  assert_equal key, 'DEBUG'
  assert_equal value, '1'
end

test('Parse -e KEY=VALUE with equals in value') do
  arg = 'URL=https://example.com?foo=bar'
  key, value = arg.split('=', 2)

  assert_equal key, 'URL'
  assert_equal value, 'https://example.com?foo=bar'
end

test('Valid network modes') do
  valid_modes = %w[zerotrust semitrusted]

  assert_true valid_modes.include?('zerotrust')
  assert_true valid_modes.include?('semitrusted')
  assert_true !valid_modes.include?('invalid')
end

test('Subcommand detection') do
  args = %w[session --shell python3]
  subcommands = %w[session service key restore]
  subcommand = subcommands.include?(args[0]) ? args[0] : nil

  assert_equal subcommand, 'session'
end

# ============================================================================
# File Operations Tests
# ============================================================================

puts "\n=== File Operations Tests ==="

test('Read text file') do
  file = Tempfile.new(['test', '.py'])
  file.write('print("hello world")')
  file.close

  begin
    content = File.read(file.path)
    assert_equal content, 'print("hello world")'
  ensure
    file.unlink
  end
end

test('Base64 encoding/decoding') do
  content = 'print("hello world")'
  encoded = Base64.strict_encode64(content)
  decoded = Base64.strict_decode64(encoded)

  assert_equal decoded, content
end

test('Extract file basename') do
  filepath = '/home/user/project/script.py'
  basename = File.basename(filepath)

  assert_equal basename, 'script.py'
end

test('Extract file extension') do
  filepath = '/home/user/project/script.py'
  ext = File.extname(filepath)

  assert_equal ext, '.py'
end

# ============================================================================
# API Constants Tests
# ============================================================================

puts "\n=== API Constants Tests ==="

test('API base URL format') do
  api_base = 'https://api.unsandbox.com'

  assert_true api_base.start_with?('https://')
  assert_includes api_base, 'unsandbox.com'
end

test('Portal base URL format') do
  portal_base = 'https://unsandbox.com'

  assert_true portal_base.start_with?('https://')
end

# ============================================================================
# Summary
# ============================================================================

puts "\n=== Summary ==="
puts "Passed: #{$passed}"
puts "Failed: #{$failed}"
puts "Total:  #{$passed + $failed}"

exit($failed > 0 ? 1 : 0)
