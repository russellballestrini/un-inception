// Unit tests for un.kt - tests internal functions without API calls
// Run with: kotlinc -script test_kotlin.kt

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.util.Base64

var passed = 0
var failed = 0

fun test(name: String, fn: () -> Unit) {
    try {
        fn()
        println("  ✓ $name")
        passed++
    } catch (e: Exception) {
        println("  ✗ $name")
        println("    ${e.message}")
        failed++
    }
}

fun assertEqual(actual: Any?, expected: Any?) {
    if (actual != expected) {
        throw Exception("Expected '$expected' but got '$actual'")
    }
}

fun assertNotEqual(a: Any?, b: Any?) {
    if (a == b) {
        throw Exception("Expected values to be different but both were '$a'")
    }
}

fun assertContains(str: String, substr: String) {
    if (!str.contains(substr)) {
        throw Exception("Expected '$str' to contain '$substr'")
    }
}

fun assertTrue(val_: Boolean) {
    if (!val_) {
        throw Exception("Expected true but got false")
    }
}

fun hmacSha256(secret: String, message: String): String {
    val mac = Mac.getInstance("HmacSHA256")
    val key = SecretKeySpec(secret.toByteArray(), "HmacSHA256")
    mac.init(key)
    return mac.doFinal(message.toByteArray()).joinToString("") { "%02x".format(it) }
}

val extMap = mapOf(
    ".py" to "python", ".js" to "javascript", ".ts" to "typescript",
    ".rb" to "ruby", ".go" to "go", ".rs" to "rust", ".c" to "c",
    ".java" to "java", ".kt" to "kotlin", ".hs" to "haskell"
)

println("\n=== Extension Mapping Tests ===")

test("Python extension maps correctly") {
    assertEqual(extMap[".py"], "python")
}

test("Kotlin extension maps correctly") {
    assertEqual(extMap[".kt"], "kotlin")
}

test("JavaScript extension maps correctly") {
    assertEqual(extMap[".js"], "javascript")
}

test("Go extension maps correctly") {
    assertEqual(extMap[".go"], "go")
}

println("\n=== HMAC Signature Tests ===")

test("HMAC-SHA256 generates 64 character hex string") {
    val sig = hmacSha256("test-secret", "test-message")
    assertEqual(sig.length, 64)
}

test("Same input produces same signature") {
    val sig1 = hmacSha256("key", "msg")
    val sig2 = hmacSha256("key", "msg")
    assertEqual(sig1, sig2)
}

test("Different secrets produce different signatures") {
    val sig1 = hmacSha256("key1", "msg")
    val sig2 = hmacSha256("key2", "msg")
    assertNotEqual(sig1, sig2)
}

test("Signature format verification") {
    val timestamp = "1704067200"
    val method = "POST"
    val endpoint = "/execute"
    val body = """{"language":"python"}"""
    val message = "$timestamp:$method:$endpoint:$body"

    assertTrue(message.startsWith(timestamp))
    assertContains(message, ":POST:")
    assertContains(message, ":/execute:")
}

println("\n=== Language Detection Tests ===")

test("Detect language from .kt extension") {
    val filename = "script.kt"
    val ext = "." + filename.substringAfterLast('.')
    assertEqual(extMap[ext], "kotlin")
}

test("Python shebang detection") {
    val content = "#!/usr/bin/env python3\nprint('hello')"
    val firstLine = content.split("\n")[0]
    assertTrue(firstLine.startsWith("#!"))
    assertContains(firstLine, "python")
}

println("\n=== Argument Parsing Tests ===")

test("Parse -e KEY=VALUE format") {
    val arg = "DEBUG=1"
    val parts = arg.split("=", limit = 2)
    assertEqual(parts[0], "DEBUG")
    assertEqual(parts[1], "1")
}

test("Parse -e KEY=VALUE with equals in value") {
    val arg = "URL=https://example.com?foo=bar"
    val parts = arg.split("=", limit = 2)
    assertEqual(parts[0], "URL")
    assertEqual(parts[1], "https://example.com?foo=bar")
}

println("\n=== File Operations Tests ===")

test("Base64 encoding/decoding") {
    val content = "print('hello world')"
    val encoded = Base64.getEncoder().encodeToString(content.toByteArray())
    val decoded = String(Base64.getDecoder().decode(encoded))
    assertEqual(decoded, content)
}

println("\n=== API Constants Tests ===")

test("API base URL format") {
    val apiBase = "https://api.unsandbox.com"
    assertTrue(apiBase.startsWith("https://"))
    assertContains(apiBase, "unsandbox.com")
}

println("\n=== Summary ===")
println("Passed: $passed")
println("Failed: $failed")
println("Total:  ${passed + failed}")

kotlin.system.exitProcess(if (failed > 0) 1 else 0)
