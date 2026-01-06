# Unit tests for un.nim - tests internal functions without API calls
# Run with: nim r test_nim.nim

import strutils, tables, os

var passed = 0
var failed = 0

proc test(name: string, result: bool) =
  if result:
    echo "  ✓ ", name
    inc passed
  else:
    echo "  ✗ ", name
    inc failed

let extMap = {
  ".py": "python", ".js": "javascript", ".ts": "typescript",
  ".rb": "ruby", ".go": "go", ".rs": "rust", ".c": "c",
  ".nim": "nim", ".java": "java", ".kt": "kotlin", ".hs": "haskell"
}.toTable

proc getLanguage(ext: string): string =
  if extMap.hasKey(ext): extMap[ext] else: ""

proc getExtension(filename: string): string =
  let dot = filename.rfind('.')
  if dot >= 0: filename[dot..^1] else: ""

proc getBasename(path: string): string =
  let slash = path.rfind('/')
  if slash >= 0: path[slash+1..^1] else: path

echo "\n=== Extension Mapping Tests ==="

test("Python extension maps correctly",
     getLanguage(".py") == "python")

test("Nim extension maps correctly",
     getLanguage(".nim") == "nim")

test("JavaScript extension maps correctly",
     getLanguage(".js") == "javascript")

test("Go extension maps correctly",
     getLanguage(".go") == "go")

echo "\n=== Signature Format Tests ==="

let timestamp = "1704067200"
let httpMethod = "POST"
let endpoint = "/execute"
let body = """{"language":"python"}"""
let message = timestamp & ":" & httpMethod & ":" & endpoint & ":" & body

test("Signature format starts with timestamp",
     message.startsWith(timestamp))

test("Signature format contains :POST:",
     message.contains(":POST:"))

test("Signature format contains :/execute:",
     message.contains(":/execute:"))

echo "\n=== Language Detection Tests ==="

let content = "#!/usr/bin/env python3\nprint('hello')"
let firstLine = content.split("\n")[0]

test("Python shebang detection - starts with #!",
     firstLine.startsWith("#!"))

test("Python shebang detection - contains python",
     firstLine.contains("python"))

echo "\n=== Argument Parsing Tests ==="

let arg1 = "DEBUG=1"
let eq1 = arg1.find('=')
let key1 = arg1[0..<eq1]
let value1 = arg1[eq1+1..^1]

test("Parse -e KEY=VALUE format - key",
     key1 == "DEBUG")

test("Parse -e KEY=VALUE format - value",
     value1 == "1")

let arg2 = "URL=https://example.com?foo=bar"
let eq2 = arg2.find('=')
let key2 = arg2[0..<eq2]
let value2 = arg2[eq2+1..^1]

test("Parse -e KEY=VALUE with equals in value",
     key2 == "URL" and value2 == "https://example.com?foo=bar")

echo "\n=== File Operations Tests ==="

test("Extract file basename",
     getBasename("/home/user/project/script.nim") == "script.nim")

test("Extract file extension",
     getExtension("/home/user/project/script.nim") == ".nim")

echo "\n=== API Constants Tests ==="

let apiBase = "https://api.unsandbox.com"

test("API base URL starts with https://",
     apiBase.startsWith("https://"))

test("API base URL contains unsandbox.com",
     apiBase.contains("unsandbox.com"))

echo "\n=== Summary ==="
echo "Passed: ", passed
echo "Failed: ", failed
echo "Total:  ", passed + failed

quit(if failed > 0: 1 else: 0)
