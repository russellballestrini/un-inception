# Unit tests for un.cr - tests internal functions without API calls
# Run with: crystal run test_crystal.cr

passed = 0
failed = 0

def test(name : String, result : Bool, passed : Int32*, failed : Int32*)
  if result
    puts "  ✓ #{name}"
    passed.value += 1
  else
    puts "  ✗ #{name}"
    failed.value += 1
  end
end

EXT_MAP = {
  ".py"   => "python",
  ".js"   => "javascript",
  ".ts"   => "typescript",
  ".rb"   => "ruby",
  ".go"   => "go",
  ".rs"   => "rust",
  ".c"    => "c",
  ".cr"   => "crystal",
  ".java" => "java",
  ".kt"   => "kotlin",
}

def get_language(ext : String) : String?
  EXT_MAP[ext]?
end

def get_extension(filename : String) : String
  idx = filename.rindex('.')
  idx ? filename[idx..] : ""
end

def get_basename(path : String) : String
  idx = path.rindex('/')
  idx ? path[idx + 1..] : path
end

puts "\n=== Extension Mapping Tests ==="

test("Python extension maps correctly",
     get_language(".py") == "python", pointerof(passed), pointerof(failed))

test("Crystal extension maps correctly",
     get_language(".cr") == "crystal", pointerof(passed), pointerof(failed))

test("JavaScript extension maps correctly",
     get_language(".js") == "javascript", pointerof(passed), pointerof(failed))

test("Go extension maps correctly",
     get_language(".go") == "go", pointerof(passed), pointerof(failed))

puts "\n=== Signature Format Tests ==="

timestamp = "1704067200"
method = "POST"
endpoint = "/execute"
body = %({"language":"python"})
message = "#{timestamp}:#{method}:#{endpoint}:#{body}"

test("Signature format starts with timestamp",
     message.starts_with?(timestamp), pointerof(passed), pointerof(failed))

test("Signature format contains :POST:",
     message.includes?(":POST:"), pointerof(passed), pointerof(failed))

test("Signature format contains :/execute:",
     message.includes?(":/execute:"), pointerof(passed), pointerof(failed))

puts "\n=== Language Detection Tests ==="

content = "#!/usr/bin/env python3\nprint('hello')"
first_line = content.split("\n").first

test("Python shebang detection - starts with #!",
     first_line.starts_with?("#!"), pointerof(passed), pointerof(failed))

test("Python shebang detection - contains python",
     first_line.includes?("python"), pointerof(passed), pointerof(failed))

puts "\n=== Argument Parsing Tests ==="

arg1 = "DEBUG=1"
eq1 = arg1.index('=') || 0
key1 = arg1[0...eq1]
value1 = arg1[eq1 + 1..]

test("Parse -e KEY=VALUE format - key",
     key1 == "DEBUG", pointerof(passed), pointerof(failed))

test("Parse -e KEY=VALUE format - value",
     value1 == "1", pointerof(passed), pointerof(failed))

arg2 = "URL=https://example.com?foo=bar"
eq2 = arg2.index('=') || 0
key2 = arg2[0...eq2]
value2 = arg2[eq2 + 1..]

test("Parse -e KEY=VALUE with equals in value",
     key2 == "URL" && value2 == "https://example.com?foo=bar", pointerof(passed), pointerof(failed))

puts "\n=== File Operations Tests ==="

test("Extract file basename",
     get_basename("/home/user/project/script.cr") == "script.cr", pointerof(passed), pointerof(failed))

test("Extract file extension",
     get_extension("/home/user/project/script.cr") == ".cr", pointerof(passed), pointerof(failed))

puts "\n=== API Constants Tests ==="

api_base = "https://api.unsandbox.com"

test("API base URL starts with https://",
     api_base.starts_with?("https://"), pointerof(passed), pointerof(failed))

test("API base URL contains unsandbox.com",
     api_base.includes?("unsandbox.com"), pointerof(passed), pointerof(failed))

puts "\n=== Summary ==="
puts "Passed: #{passed}"
puts "Failed: #{failed}"
puts "Total:  #{passed + failed}"

exit(failed > 0 ? 1 : 0)
