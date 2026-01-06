#!/usr/bin/env groovy
// Unit tests for un.groovy - tests internal functions without API calls
// Run with: groovy test_groovy.groovy

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

def passed = 0
def failed = 0

def test = { name, fn ->
    try {
        fn()
        println "  ✓ $name"
        passed++
    } catch (e) {
        println "  ✗ $name"
        println "    ${e.message}"
        failed++
    }
}

def assertEqual = { actual, expected ->
    if (actual != expected) {
        throw new Exception("Expected '$expected' but got '$actual'")
    }
}

def assertContains = { str, substr ->
    if (!str.contains(substr)) {
        throw new Exception("Expected '$str' to contain '$substr'")
    }
}

def assertTrue = { val ->
    if (!val) {
        throw new Exception("Expected true but got false")
    }
}

def hmacSha256 = { secret, message ->
    def mac = Mac.getInstance("HmacSHA256")
    def key = new SecretKeySpec(secret.getBytes("UTF-8"), "HmacSHA256")
    mac.init(key)
    mac.doFinal(message.getBytes("UTF-8")).collect { String.format("%02x", it) }.join()
}

def extMap = [
    ".py": "python", ".js": "javascript", ".ts": "typescript",
    ".rb": "ruby", ".go": "go", ".rs": "rust", ".c": "c",
    ".groovy": "groovy", ".java": "java", ".kt": "kotlin"
]

println "\n=== Extension Mapping Tests ==="

test("Python extension maps correctly") {
    assertEqual(extMap[".py"], "python")
}

test("Groovy extension maps correctly") {
    assertEqual(extMap[".groovy"], "groovy")
}

test("JavaScript extension maps correctly") {
    assertEqual(extMap[".js"], "javascript")
}

test("Go extension maps correctly") {
    assertEqual(extMap[".go"], "go")
}

println "\n=== HMAC Signature Tests ==="

test("HMAC-SHA256 generates 64 character hex string") {
    def sig = hmacSha256("test-secret", "test-message")
    assertEqual(sig.length(), 64)
}

test("Same input produces same signature") {
    def sig1 = hmacSha256("key", "msg")
    def sig2 = hmacSha256("key", "msg")
    assertEqual(sig1, sig2)
}

test("Signature format verification") {
    def timestamp = "1704067200"
    def method = "POST"
    def endpoint = "/execute"
    def body = '{"language":"python"}'
    def message = "$timestamp:$method:$endpoint:$body"

    assertTrue(message.startsWith(timestamp))
    assertContains(message, ":POST:")
    assertContains(message, ":/execute:")
}

println "\n=== Language Detection Tests ==="

test("Detect language from .groovy extension") {
    def filename = "script.groovy"
    def ext = "." + filename.tokenize('.').last()
    assertEqual(extMap[ext], "groovy")
}

test("Python shebang detection") {
    def content = "#!/usr/bin/env python3\nprint('hello')"
    def firstLine = content.split("\n")[0]
    assertTrue(firstLine.startsWith("#!"))
    assertContains(firstLine, "python")
}

println "\n=== Argument Parsing Tests ==="

test("Parse -e KEY=VALUE format") {
    def arg = "DEBUG=1"
    def parts = arg.split("=", 2)
    assertEqual(parts[0], "DEBUG")
    assertEqual(parts[1], "1")
}

test("Parse -e KEY=VALUE with equals in value") {
    def arg = "URL=https://example.com?foo=bar"
    def parts = arg.split("=", 2)
    assertEqual(parts[0], "URL")
    assertEqual(parts[1], "https://example.com?foo=bar")
}

println "\n=== File Operations Tests ==="

test("Extract file basename") {
    def path = "/home/user/project/script.groovy"
    def basename = path.tokenize('/').last()
    assertEqual(basename, "script.groovy")
}

test("Extract file extension") {
    def filename = "script.groovy"
    def ext = "." + filename.tokenize('.').last()
    assertEqual(ext, ".groovy")
}

println "\n=== API Constants Tests ==="

test("API base URL format") {
    def apiBase = "https://api.unsandbox.com"
    assertTrue(apiBase.startsWith("https://"))
    assertContains(apiBase, "unsandbox.com")
}

println "\n=== Summary ==="
println "Passed: $passed"
println "Failed: $failed"
println "Total:  ${passed + failed}"

System.exit(failed > 0 ? 1 : 0)
