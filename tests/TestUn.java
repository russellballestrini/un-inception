// TestUn.java - Comprehensive tests for Un.java CLI implementation
// Compile: javac -cp .. TestUn.java
// Run: java -cp ..:. TestUn
// Note: Requires Un.class to be compiled in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

import java.io.*;
import java.lang.reflect.*;
import java.nio.file.*;

public class TestUn {
    private static int testsRun = 0;
    private static int testsPassed = 0;
    private static int testsFailed = 0;

    public static void main(String[] args) {
        System.out.println("=== Running Un.java Tests ===\n");

        // Unit Tests - Extension Detection
        testExtensionDetection();

        // Integration Tests - API Call (skip if no API key)
        String apiKey = System.getenv("UNSANDBOX_API_KEY");
        if (apiKey != null && !apiKey.isEmpty()) {
            testApiCall();
            testFibExecution();
        } else {
            System.out.println("SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n");
        }

        // Print summary
        System.out.println("=== Test Summary ===");
        System.out.println("Tests run: " + testsRun);
        System.out.println("Passed: " + testsPassed);
        System.out.println("Failed: " + testsFailed);

        if (testsFailed > 0) {
            System.exit(1);
        } else {
            System.out.println("\nAll tests PASSED!");
            System.exit(0);
        }
    }

    private static void testExtensionDetection() {
        System.out.println("--- Unit Tests: Extension Detection ---");

        testDetectLanguage("test.java", "java");
        testDetectLanguage("test.kt", "kotlin");
        testDetectLanguage("test.cs", "csharp");
        testDetectLanguage("test.fs", "fsharp");
        testDetectLanguage("test.groovy", "groovy");
        testDetectLanguage("test.dart", "dart");
        testDetectLanguage("test.py", "python");
        testDetectLanguage("test.js", "javascript");
        testDetectLanguage("test.rs", "rust");
        testDetectLanguage("test.go", "go");

        testDetectLanguageError("noextension");
        testDetectLanguageError("test.unknown");

        System.out.println();
    }

    private static void testDetectLanguage(String filename, String expectedLang) {
        testsRun++;
        try {
            // Use reflection to call private detectLanguage method
            Class<?> unClass = Class.forName("Un");
            Method detectLanguage = unClass.getDeclaredMethod("detectLanguage", String.class);
            detectLanguage.setAccessible(true);

            String result = (String) detectLanguage.invoke(null, filename);

            if (result.equals(expectedLang)) {
                testsPassed++;
                System.out.println("PASS: detectLanguage(\"" + filename + "\") = \"" + expectedLang + "\"");
            } else {
                testsFailed++;
                System.out.println("FAIL: detectLanguage(\"" + filename + "\") expected \"" + expectedLang + "\", got \"" + result + "\"");
            }
        } catch (Exception e) {
            testsFailed++;
            System.out.println("FAIL: detectLanguage(\"" + filename + "\") threw exception: " + e.getMessage());
        }
    }

    private static void testDetectLanguageError(String filename) {
        testsRun++;
        try {
            Class<?> unClass = Class.forName("Un");
            Method detectLanguage = unClass.getDeclaredMethod("detectLanguage", String.class);
            detectLanguage.setAccessible(true);

            try {
                detectLanguage.invoke(null, filename);
                testsFailed++;
                System.out.println("FAIL: detectLanguage(\"" + filename + "\") should throw exception");
            } catch (InvocationTargetException e) {
                // Expected to throw RuntimeException
                if (e.getCause() instanceof RuntimeException) {
                    testsPassed++;
                    System.out.println("PASS: detectLanguage(\"" + filename + "\") correctly throws exception");
                } else {
                    testsFailed++;
                    System.out.println("FAIL: detectLanguage(\"" + filename + "\") threw wrong exception: " + e.getCause());
                }
            }
        } catch (Exception e) {
            testsFailed++;
            System.out.println("FAIL: detectLanguage(\"" + filename + "\") test setup failed: " + e.getMessage());
        }
    }

    private static void testApiCall() {
        System.out.println("--- Integration Test: API Call ---");
        testsRun++;

        try {
            // Create a simple test file
            String testCode = "console.log('Hello from test');";
            Path testFile = Paths.get("test_api.js");
            Files.write(testFile, testCode.getBytes());

            try {
                // Execute Un with the test file
                ProcessBuilder pb = new ProcessBuilder("java", "-cp", "..", "Un", "test_api.js");
                pb.redirectErrorStream(true);
                Process p = pb.start();

                // Read output
                BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
                StringBuilder output = new StringBuilder();
                String line;
                while ((line = reader.readLine()) != null) {
                    output.append(line).append("\n");
                }

                int exitCode = p.waitFor();

                if (exitCode == 0 && output.toString().contains("Hello from test")) {
                    testsPassed++;
                    System.out.println("PASS: API call succeeded and returned expected output");
                } else {
                    testsFailed++;
                    System.out.println("FAIL: API call failed or unexpected output");
                    System.out.println("Exit code: " + exitCode);
                    System.out.println("Output: " + output.toString());
                }
            } finally {
                Files.deleteIfExists(testFile);
            }
        } catch (Exception e) {
            testsFailed++;
            System.out.println("FAIL: API call test threw exception: " + e.getMessage());
            e.printStackTrace();
        }
        System.out.println();
    }

    private static void testFibExecution() {
        System.out.println("--- Functional Test: fib.java Execution ---");
        testsRun++;

        try {
            // Check if fib.java exists
            Path fibFile = Paths.get("fib.java");
            if (!Files.exists(fibFile)) {
                testsFailed++;
                System.out.println("FAIL: fib.java not found in tests directory");
                System.out.println();
                return;
            }

            // Execute Un with fib.java
            ProcessBuilder pb = new ProcessBuilder("java", "-cp", "..", "Un", "fib.java");
            pb.redirectErrorStream(true);
            Process p = pb.start();

            // Read output
            BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
            StringBuilder output = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                output.append(line).append("\n");
            }

            int exitCode = p.waitFor();

            String outputStr = output.toString();
            if (exitCode == 0 && outputStr.contains("fib(10) = 55")) {
                testsPassed++;
                System.out.println("PASS: fib.java execution succeeded");
                System.out.println("Output: " + outputStr.trim());
            } else {
                testsFailed++;
                System.out.println("FAIL: fib.java execution failed or unexpected output");
                System.out.println("Exit code: " + exitCode);
                System.out.println("Output: " + outputStr);
            }
        } catch (Exception e) {
            testsFailed++;
            System.out.println("FAIL: fib.java execution test threw exception: " + e.getMessage());
            e.printStackTrace();
        }
        System.out.println();
    }
}
