// test_un_kt.kt - Comprehensive tests for un.kt CLI implementation
// Compile: kotlinc -cp .. test_un_kt.kt -include-runtime -d test_un_kt.jar
// Run: java -jar test_un_kt.jar
// Note: Requires un.kt to be compiled in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import kotlin.system.exitProcess

var testsRun = 0
var testsPassed = 0
var testsFailed = 0

fun main() {
    println("=== Running un.kt Tests ===\n")

    // Unit Tests - Extension Detection
    testExtensionDetection()

    // Integration Tests - API Call (skip if no API key)
    val apiKey = System.getenv("UNSANDBOX_API_KEY")
    if (!apiKey.isNullOrEmpty()) {
        testApiCall()
        testFibExecution()
    } else {
        println("SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n")
    }

    // Print summary
    println("=== Test Summary ===")
    println("Tests run: $testsRun")
    println("Passed: $testsPassed")
    println("Failed: $testsFailed")

    if (testsFailed > 0) {
        exitProcess(1)
    } else {
        println("\nAll tests PASSED!")
        exitProcess(0)
    }
}

fun testExtensionDetection() {
    println("--- Unit Tests: Extension Detection ---")

    testDetectLanguage("test.java", "java")
    testDetectLanguage("test.kt", "kotlin")
    testDetectLanguage("test.cs", "csharp")
    testDetectLanguage("test.fs", "fsharp")
    testDetectLanguage("test.groovy", "groovy")
    testDetectLanguage("test.dart", "dart")
    testDetectLanguage("test.py", "python")
    testDetectLanguage("test.js", "javascript")
    testDetectLanguage("test.rs", "rust")
    testDetectLanguage("test.go", "go")

    testDetectLanguageError("noextension")
    testDetectLanguageError("test.unknown")

    println()
}

fun testDetectLanguage(filename: String, expectedLang: String) {
    testsRun++
    try {
        // Use reflection to call detectLanguage function
        val unKtClass = Class.forName("UnKt")
        val detectLanguage = unKtClass.getDeclaredMethod("detectLanguage", String::class.java)

        val result = detectLanguage.invoke(null, filename) as String

        if (result == expectedLang) {
            testsPassed++
            println("PASS: detectLanguage(\"$filename\") = \"$expectedLang\"")
        } else {
            testsFailed++
            println("FAIL: detectLanguage(\"$filename\") expected \"$expectedLang\", got \"$result\"")
        }
    } catch (e: Exception) {
        testsFailed++
        println("FAIL: detectLanguage(\"$filename\") threw exception: ${e.message}")
    }
}

fun testDetectLanguageError(filename: String) {
    testsRun++
    try {
        val unKtClass = Class.forName("UnKt")
        val detectLanguage = unKtClass.getDeclaredMethod("detectLanguage", String::class.java)

        try {
            detectLanguage.invoke(null, filename)
            testsFailed++
            println("FAIL: detectLanguage(\"$filename\") should throw exception")
        } catch (e: java.lang.reflect.InvocationTargetException) {
            // Expected to throw RuntimeException
            if (e.cause is RuntimeException) {
                testsPassed++
                println("PASS: detectLanguage(\"$filename\") correctly throws exception")
            } else {
                testsFailed++
                println("FAIL: detectLanguage(\"$filename\") threw wrong exception: ${e.cause}")
            }
        }
    } catch (e: Exception) {
        testsFailed++
        println("FAIL: detectLanguage(\"$filename\") test setup failed: ${e.message}")
    }
}

fun testApiCall() {
    println("--- Integration Test: API Call ---")
    testsRun++

    try {
        // Create a simple test file
        val testCode = "console.log('Hello from Kotlin test');"
        val testFile = File("test_api_kt.js")
        testFile.writeText(testCode)

        try {
            // Execute kotlin CLI with the test file
            val pb = ProcessBuilder("kotlin", "-cp", "..", "UnKt", "test_api_kt.js")
            pb.redirectErrorStream(true)
            val p = pb.start()

            // Read output
            val reader = BufferedReader(InputStreamReader(p.inputStream))
            val output = StringBuilder()
            var line: String? = reader.readLine()
            while (line != null) {
                output.append(line).append("\n")
                line = reader.readLine()
            }

            val exitCode = p.waitFor()

            if (exitCode == 0 && output.contains("Hello from Kotlin test")) {
                testsPassed++
                println("PASS: API call succeeded and returned expected output")
            } else {
                testsFailed++
                println("FAIL: API call failed or unexpected output")
                println("Exit code: $exitCode")
                println("Output: $output")
            }
        } finally {
            testFile.delete()
        }
    } catch (e: Exception) {
        testsFailed++
        println("FAIL: API call test threw exception: ${e.message}")
        e.printStackTrace()
    }
    println()
}

fun testFibExecution() {
    println("--- Functional Test: fib.java Execution ---")
    testsRun++

    try {
        // Check if fib.java exists
        val fibFile = File("fib.java")
        if (!fibFile.exists()) {
            testsFailed++
            println("FAIL: fib.java not found in tests directory")
            println()
            return
        }

        // Execute Kotlin CLI with fib.java
        val pb = ProcessBuilder("kotlin", "-cp", "..", "UnKt", "fib.java")
        pb.redirectErrorStream(true)
        val p = pb.start()

        // Read output
        val reader = BufferedReader(InputStreamReader(p.inputStream))
        val output = StringBuilder()
        var line: String? = reader.readLine()
        while (line != null) {
            output.append(line).append("\n")
            line = reader.readLine()
        }

        val exitCode = p.waitFor()

        val outputStr = output.toString()
        if (exitCode == 0 && outputStr.contains("fib(10) = 55")) {
            testsPassed++
            println("PASS: fib.java execution succeeded")
            println("Output: ${outputStr.trim()}")
        } else {
            testsFailed++
            println("FAIL: fib.java execution failed or unexpected output")
            println("Exit code: $exitCode")
            println("Output: $outputStr")
        }
    } catch (e: Exception) {
        testsFailed++
        println("FAIL: fib.java execution test threw exception: ${e.message}")
        e.printStackTrace()
    }
    println()
}
