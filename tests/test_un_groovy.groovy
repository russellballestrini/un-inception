#!/usr/bin/env groovy
// test_un_groovy.groovy - Comprehensive tests for un.groovy CLI implementation
// Run: groovy test_un_groovy.groovy
// Note: Requires un.groovy to be in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

import java.lang.reflect.*

class TestUnGroovy {
    static int testsRun = 0
    static int testsPassed = 0
    static int testsFailed = 0

    static void main(String[] args) {
        println "=== Running un.groovy Tests ===\n"

        // Unit Tests - Extension Detection
        testExtensionDetection()

        // Integration Tests - API Call (skip if no API key)
        def apiKey = System.getenv('UNSANDBOX_API_KEY')
        if (apiKey) {
            testApiCall()
            testFibExecution()
        } else {
            println "SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n"
        }

        // Print summary
        println "=== Test Summary ==="
        println "Tests run: ${testsRun}"
        println "Passed: ${testsPassed}"
        println "Failed: ${testsFailed}"

        if (testsFailed > 0) {
            System.exit(1)
        } else {
            println "\nAll tests PASSED!"
            System.exit(0)
        }
    }

    static void testExtensionDetection() {
        println "--- Unit Tests: Extension Detection ---"

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

        println ""
    }

    static void testDetectLanguage(String filename, String expectedLang) {
        testsRun++
        try {
            // Load and execute Un.groovy script to access detectLanguage method
            def binding = new Binding()
            def shell = new GroovyShell(binding)
            def script = shell.parse(new File('../un.groovy'))

            // Get the Un class
            def unClass = Class.forName('Un')
            def detectLanguage = unClass.getDeclaredMethod('detectLanguage', String)
            detectLanguage.accessible = true

            def result = detectLanguage.invoke(null, filename)

            if (result == expectedLang) {
                testsPassed++
                println "PASS: detectLanguage(\"${filename}\") = \"${expectedLang}\""
            } else {
                testsFailed++
                println "FAIL: detectLanguage(\"${filename}\") expected \"${expectedLang}\", got \"${result}\""
            }
        } catch (Exception e) {
            testsFailed++
            println "FAIL: detectLanguage(\"${filename}\") threw exception: ${e.message}"
        }
    }

    static void testDetectLanguageError(String filename) {
        testsRun++
        try {
            def unClass = Class.forName('Un')
            def detectLanguage = unClass.getDeclaredMethod('detectLanguage', String)
            detectLanguage.accessible = true

            try {
                detectLanguage.invoke(null, filename)
                testsFailed++
                println "FAIL: detectLanguage(\"${filename}\") should throw exception"
            } catch (InvocationTargetException e) {
                // Expected to throw RuntimeException
                if (e.cause instanceof RuntimeException) {
                    testsPassed++
                    println "PASS: detectLanguage(\"${filename}\") correctly throws exception"
                } else {
                    testsFailed++
                    println "FAIL: detectLanguage(\"${filename}\") threw wrong exception: ${e.cause}"
                }
            }
        } catch (Exception e) {
            testsFailed++
            println "FAIL: detectLanguage(\"${filename}\") test setup failed: ${e.message}"
        }
    }

    static void testApiCall() {
        println "--- Integration Test: API Call ---"
        testsRun++

        try {
            // Create a simple test file
            def testCode = "console.log('Hello from Groovy test');"
            def testFile = new File('test_api_groovy.js')
            testFile.text = testCode

            try {
                // Execute groovy CLI with the test file
                def proc = ['groovy', '../un.groovy', 'test_api_groovy.js'].execute()
                def output = new StringBuilder()
                def error = new StringBuilder()
                proc.consumeProcessOutput(output, error)
                def exitCode = proc.waitFor()

                if (exitCode == 0 && output.toString().contains("Hello from Groovy test")) {
                    testsPassed++
                    println "PASS: API call succeeded and returned expected output"
                } else {
                    testsFailed++
                    println "FAIL: API call failed or unexpected output"
                    println "Exit code: ${exitCode}"
                    println "Output: ${output}"
                    println "Error: ${error}"
                }
            } finally {
                testFile.delete()
            }
        } catch (Exception e) {
            testsFailed++
            println "FAIL: API call test threw exception: ${e.message}"
            e.printStackTrace()
        }
        println ""
    }

    static void testFibExecution() {
        println "--- Functional Test: fib.java Execution ---"
        testsRun++

        try {
            // Check if fib.java exists
            def fibFile = new File('fib.java')
            if (!fibFile.exists()) {
                testsFailed++
                println "FAIL: fib.java not found in tests directory"
                println ""
                return
            }

            // Execute Groovy CLI with fib.java
            def proc = ['groovy', '../un.groovy', 'fib.java'].execute()
            def output = new StringBuilder()
            def error = new StringBuilder()
            proc.consumeProcessOutput(output, error)
            def exitCode = proc.waitFor()

            def outputStr = output.toString()
            if (exitCode == 0 && outputStr.contains("fib(10) = 55")) {
                testsPassed++
                println "PASS: fib.java execution succeeded"
                println "Output: ${outputStr.trim()}"
            } else {
                testsFailed++
                println "FAIL: fib.java execution failed or unexpected output"
                println "Exit code: ${exitCode}"
                println "Output: ${outputStr}"
                println "Error: ${error}"
            }
        } catch (Exception e) {
            testsFailed++
            println "FAIL: fib.java execution test threw exception: ${e.message}"
            e.printStackTrace()
        }
        println ""
    }
}

// Run the tests
TestUnGroovy.main(args)
