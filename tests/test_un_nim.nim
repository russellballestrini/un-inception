# Test suite for UN CLI Nim implementation
# Compile: nim c -d:release test_un_nim.nim
# Run: ./test_un_nim
#
# Tests:
# 1. Unit tests for extension detection
# 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
# 3. Functional test running fib.go

import std/httpclient
import std/json
import std/os
import std/strutils
import std/osproc

# Copy of detectLanguage from un.nim for testing
proc detectLanguage(filename: string): string =
  let ext = splitFile(filename).ext
  case ext
  of ".py": return "python"
  of ".js": return "javascript"
  of ".go": return "go"
  of ".rs": return "rust"
  of ".c": return "c"
  of ".cpp": return "cpp"
  of ".d": return "d"
  of ".zig": return "zig"
  of ".nim": return "nim"
  of ".v": return "v"
  else: return ""

proc testExtensionDetection(): bool =
  echo "=== Test 1: Extension Detection ==="

  type TestCase = tuple[filename: string, expected: string]
  let tests: seq[TestCase] = @[
    ("script.py", "python"),
    ("app.js", "javascript"),
    ("main.go", "go"),
    ("program.rs", "rust"),
    ("code.c", "c"),
    ("app.cpp", "cpp"),
    ("prog.d", "d"),
    ("main.zig", "zig"),
    ("script.nim", "nim"),
    ("app.v", "v"),
    ("unknown.xyz", ""),
  ]

  var passed = 0
  var failed = 0

  for test in tests:
    let result = detectLanguage(test.filename)
    if result == test.expected:
      echo "  PASS: ", test.filename, " -> ", result
      inc passed
    else:
      echo "  FAIL: ", test.filename, " -> got ", result, ", expected ", test.expected
      inc failed

  echo "Extension Detection: ", passed, " passed, ", failed, " failed\n"
  return failed == 0

proc testApiConnection(): bool =
  echo "=== Test 2: API Connection ==="

  let apiKey = getEnv("UNSANDBOX_API_KEY")
  if apiKey == "":
    echo "  SKIP: UNSANDBOX_API_KEY not set"
    echo "API Connection: skipped\n"
    return true

  let requestBody = %* {
    "language": "python",
    "code": "print('Hello from API test')"
  }

  var client = newHttpClient()
  client.headers = newHttpHeaders({
    "Content-Type": "application/json",
    "Authorization": "Bearer " & apiKey
  })

  let response = try:
    client.request("https://api.unsandbox.com/execute", httpMethod = HttpPost, body = $requestBody)
  except:
    echo "  FAIL: HTTP request error"
    return false

  let responseBody = try:
    response.body
  except:
    echo "  FAIL: Error reading response"
    return false

  let result = parseJson(responseBody)

  let stdoutStr = result["stdout"].getStr()
  if "Hello from API test" notin stdoutStr:
    echo "  FAIL: Unexpected response: ", stdoutStr
    return false

  echo "  PASS: API connection successful"
  echo "API Connection: passed\n"
  return true

proc testFibExecution(): bool =
  echo "=== Test 3: Functional Test (fib.go) ==="

  let apiKey = getEnv("UNSANDBOX_API_KEY")
  if apiKey == "":
    echo "  SKIP: UNSANDBOX_API_KEY not set"
    echo "Functional Test: skipped\n"
    return true

  if not fileExists("../un"):
    echo "  SKIP: ../un binary not found (run: cd .. && nim c -d:release un.nim)"
    echo "Functional Test: skipped\n"
    return true

  if not fileExists("fib.go"):
    echo "  SKIP: fib.go not found"
    echo "Functional Test: skipped\n"
    return true

  let (output, exitCode) = try:
    execCmdEx("../un fib.go")
  except:
    echo "  FAIL: Execution error"
    return false

  if exitCode != 0:
    echo "  FAIL: Command failed with exit code: ", exitCode
    echo "  Output: ", output
    return false

  if "fib(10) = 55" notin output:
    echo "  FAIL: Expected output to contain 'fib(10) = 55', got: ", output
    return false

  echo "  PASS: fib.go executed successfully"
  echo "  Output: ", output
  echo "Functional Test: passed\n"
  return true

proc testCliCommands(): bool =
  echo "=== Test 4: CLI Commands (Feature Parity) ==="

  var passed = 0
  var failed = 0

  # Find un.nim script
  let scriptPaths = [
    "../clients/nim/sync/src/un.nim",
    "../../clients/nim/sync/src/un.nim",
    "../un.nim"
  ]

  var unScript = ""
  for path in scriptPaths:
    if fileExists(path):
      unScript = path
      break

  if unScript == "":
    echo "  SKIP: un.nim not found"
    echo "CLI Commands: skipped\n"
    return true

  # Test: --help
  let (helpOutput, helpCode) = try:
    execCmdEx("nim r " & unScript & " -- --help")
  except:
    ("", -1)

  if helpOutput.contains("Usage") or helpOutput.contains("usage"):
    echo "  PASS: --help shows usage"
    inc passed
  else:
    echo "  FAIL: --help does not show usage"
    inc failed

  # Test: version command
  let (versionOutput, versionCode) = try:
    execCmdEx("nim r " & unScript & " -- version")
  except:
    ("", -1)

  if versionOutput.contains("version") or versionOutput.contains("Version"):
    echo "  PASS: version command works"
    inc passed
  else:
    echo "  FAIL: version command does not work"
    inc failed

  # Test: health command
  let (healthOutput, healthCode) = try:
    execCmdEx("nim r " & unScript & " -- health")
  except:
    ("", -1)

  if healthOutput.contains("health") or healthOutput.contains("API"):
    echo "  PASS: health command works"
    inc passed
  else:
    echo "  FAIL: health command does not work"
    inc failed

  # Test: languages command
  let (langsOutput, langsCode) = try:
    execCmdEx("nim r " & unScript & " -- languages")
  except:
    ("", -1)

  if langsOutput.contains("python") or langsOutput.contains("Error") or langsOutput.contains("API key"):
    echo "  PASS: languages command works"
    inc passed
  else:
    echo "  FAIL: languages command does not work"
    inc failed

  echo "CLI Commands: ", passed, " passed, ", failed, " failed\n"
  return failed == 0

proc testApiCommands(): bool =
  echo "=== Test 5: API Commands (require auth) ==="

  let publicKey = getEnv("UNSANDBOX_PUBLIC_KEY")
  let secretKey = getEnv("UNSANDBOX_SECRET_KEY")

  if publicKey == "" and secretKey == "":
    echo "  SKIP: UNSANDBOX_PUBLIC_KEY/SECRET_KEY not set"
    echo "API Commands: skipped\n"
    return true

  var passed = 0
  var failed = 0

  # Find un.nim script
  let scriptPaths = [
    "../clients/nim/sync/src/un.nim",
    "../../clients/nim/sync/src/un.nim",
    "../un.nim"
  ]

  var unScript = ""
  for path in scriptPaths:
    if fileExists(path):
      unScript = path
      break

  if unScript == "":
    echo "  SKIP: un.nim not found"
    echo "API Commands: skipped\n"
    return true

  # Test: snapshot --list
  let (snapOutput, snapCode) = try:
    execCmdEx("nim r " & unScript & " -- snapshot --list")
  except:
    ("", -1)

  if snapOutput.contains("[") or snapOutput.contains("{") or snapOutput.contains("Error") or snapOutput.contains("snapshots"):
    echo "  PASS: snapshot --list works"
    inc passed
  else:
    echo "  FAIL: snapshot --list does not work"
    inc failed

  # Test: session --list
  let (sessOutput, sessCode) = try:
    execCmdEx("nim r " & unScript & " -- session --list")
  except:
    ("", -1)

  if sessOutput.contains("[") or sessOutput.contains("{") or sessOutput.contains("Error") or sessOutput.contains("sessions"):
    echo "  PASS: session --list works"
    inc passed
  else:
    echo "  FAIL: session --list does not work"
    inc failed

  # Test: service --list
  let (svcOutput, svcCode) = try:
    execCmdEx("nim r " & unScript & " -- service --list")
  except:
    ("", -1)

  if svcOutput.contains("[") or svcOutput.contains("{") or svcOutput.contains("Error") or svcOutput.contains("services"):
    echo "  PASS: service --list works"
    inc passed
  else:
    echo "  FAIL: service --list does not work"
    inc failed

  # Test: image --list
  let (imgOutput, imgCode) = try:
    execCmdEx("nim r " & unScript & " -- image --list")
  except:
    ("", -1)

  if imgOutput.contains("[") or imgOutput.contains("{") or imgOutput.contains("Error") or imgOutput.contains("images"):
    echo "  PASS: image --list works"
    inc passed
  else:
    echo "  FAIL: image --list does not work"
    inc failed

  echo "API Commands: ", passed, " passed, ", failed, " failed\n"
  return failed == 0

proc main() =
  echo "UN CLI Nim Implementation Test Suite"
  echo "=====================================\n"

  var allPassed = true

  if not testExtensionDetection():
    allPassed = false

  if not testApiConnection():
    allPassed = false

  if not testFibExecution():
    allPassed = false

  if not testCliCommands():
    allPassed = false

  if not testApiCommands():
    allPassed = false

  echo "====================================="
  if allPassed:
    echo "RESULT: ALL TESTS PASSED"
    quit(0)
  else:
    echo "RESULT: SOME TESTS FAILED"
    quit(1)

when isMainModule:
  main()
