// TestUn.cs - Comprehensive tests for Un.cs CLI implementation
// Compile: csc TestUn.cs (or mcs TestUn.cs)
// Run: ./TestUn.exe (Windows) or mono TestUn.exe (Linux/macOS)
// Note: Requires Un.exe to be compiled in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

using System;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;

class TestUn
{
    private static int testsRun = 0;
    private static int testsPassed = 0;
    private static int testsFailed = 0;

    static void Main(string[] args)
    {
        Console.WriteLine("=== Running Un.cs Tests ===\n");

        // Unit Tests - Extension Detection
        TestExtensionDetection();

        // Integration Tests - API Call (skip if no API key)
        string apiKey = Environment.GetEnvironmentVariable("UNSANDBOX_API_KEY");
        if (!string.IsNullOrEmpty(apiKey))
        {
            TestApiCall();
            TestFibExecution();
        }
        else
        {
            Console.WriteLine("SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n");
        }

        // Print summary
        Console.WriteLine("=== Test Summary ===");
        Console.WriteLine($"Tests run: {testsRun}");
        Console.WriteLine($"Passed: {testsPassed}");
        Console.WriteLine($"Failed: {testsFailed}");

        if (testsFailed > 0)
        {
            Environment.Exit(1);
        }
        else
        {
            Console.WriteLine("\nAll tests PASSED!");
            Environment.Exit(0);
        }
    }

    static void TestExtensionDetection()
    {
        Console.WriteLine("--- Unit Tests: Extension Detection ---");

        TestDetectLanguage("test.java", "java");
        TestDetectLanguage("test.kt", "kotlin");
        TestDetectLanguage("test.cs", "csharp");
        TestDetectLanguage("test.fs", "fsharp");
        TestDetectLanguage("test.groovy", "groovy");
        TestDetectLanguage("test.dart", "dart");
        TestDetectLanguage("test.py", "python");
        TestDetectLanguage("test.js", "javascript");
        TestDetectLanguage("test.rs", "rust");
        TestDetectLanguage("test.go", "go");

        TestDetectLanguageError("noextension");
        TestDetectLanguageError("test.unknown");

        Console.WriteLine();
    }

    static void TestDetectLanguage(string filename, string expectedLang)
    {
        testsRun++;
        try
        {
            // Load Un assembly and call DetectLanguage via reflection
            Assembly unAssembly = Assembly.LoadFrom("../Un.exe");
            Type unType = unAssembly.GetType("Un");
            MethodInfo detectLanguage = unType.GetMethod("DetectLanguage",
                BindingFlags.NonPublic | BindingFlags.Static);

            string result = (string)detectLanguage.Invoke(null, new object[] { filename });

            if (result == expectedLang)
            {
                testsPassed++;
                Console.WriteLine($"PASS: DetectLanguage(\"{filename}\") = \"{expectedLang}\"");
            }
            else
            {
                testsFailed++;
                Console.WriteLine($"FAIL: DetectLanguage(\"{filename}\") expected \"{expectedLang}\", got \"{result}\"");
            }
        }
        catch (Exception e)
        {
            testsFailed++;
            Console.WriteLine($"FAIL: DetectLanguage(\"{filename}\") threw exception: {e.Message}");
        }
    }

    static void TestDetectLanguageError(string filename)
    {
        testsRun++;
        try
        {
            Assembly unAssembly = Assembly.LoadFrom("../Un.exe");
            Type unType = unAssembly.GetType("Un");
            MethodInfo detectLanguage = unType.GetMethod("DetectLanguage",
                BindingFlags.NonPublic | BindingFlags.Static);

            try
            {
                detectLanguage.Invoke(null, new object[] { filename });
                testsFailed++;
                Console.WriteLine($"FAIL: DetectLanguage(\"{filename}\") should throw exception");
            }
            catch (TargetInvocationException e)
            {
                // Expected to throw Exception
                if (e.InnerException is Exception)
                {
                    testsPassed++;
                    Console.WriteLine($"PASS: DetectLanguage(\"{filename}\") correctly throws exception");
                }
                else
                {
                    testsFailed++;
                    Console.WriteLine($"FAIL: DetectLanguage(\"{filename}\") threw wrong exception: {e.InnerException}");
                }
            }
        }
        catch (Exception e)
        {
            testsFailed++;
            Console.WriteLine($"FAIL: DetectLanguage(\"{filename}\") test setup failed: {e.Message}");
        }
    }

    static void TestApiCall()
    {
        Console.WriteLine("--- Integration Test: API Call ---");
        testsRun++;

        try
        {
            // Create a simple test file
            string testCode = "console.log('Hello from C# test');";
            string testFile = "test_api_cs.js";
            File.WriteAllText(testFile, testCode);

            try
            {
                // Execute Un with the test file
                ProcessStartInfo psi = new ProcessStartInfo
                {
                    FileName = "mono",
                    Arguments = "../Un.exe test_api_cs.js",
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false
                };

                Process p = Process.Start(psi);
                string output = p.StandardOutput.ReadToEnd();
                string error = p.StandardError.ReadToEnd();
                p.WaitForExit();

                if (p.ExitCode == 0 && output.Contains("Hello from C# test"))
                {
                    testsPassed++;
                    Console.WriteLine("PASS: API call succeeded and returned expected output");
                }
                else
                {
                    testsFailed++;
                    Console.WriteLine("FAIL: API call failed or unexpected output");
                    Console.WriteLine($"Exit code: {p.ExitCode}");
                    Console.WriteLine($"Output: {output}");
                    Console.WriteLine($"Error: {error}");
                }
            }
            finally
            {
                if (File.Exists(testFile))
                    File.Delete(testFile);
            }
        }
        catch (Exception e)
        {
            testsFailed++;
            Console.WriteLine($"FAIL: API call test threw exception: {e.Message}");
        }
        Console.WriteLine();
    }

    static void TestFibExecution()
    {
        Console.WriteLine("--- Functional Test: fib.java Execution ---");
        testsRun++;

        try
        {
            // Check if fib.java exists
            if (!File.Exists("fib.java"))
            {
                testsFailed++;
                Console.WriteLine("FAIL: fib.java not found in tests directory");
                Console.WriteLine();
                return;
            }

            // Execute Un with fib.java
            ProcessStartInfo psi = new ProcessStartInfo
            {
                FileName = "mono",
                Arguments = "../Un.exe fib.java",
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

            Process p = Process.Start(psi);
            string output = p.StandardOutput.ReadToEnd();
            string error = p.StandardError.ReadToEnd();
            p.WaitForExit();

            if (p.ExitCode == 0 && output.Contains("fib(10) = 55"))
            {
                testsPassed++;
                Console.WriteLine("PASS: fib.java execution succeeded");
                Console.WriteLine($"Output: {output.Trim()}");
            }
            else
            {
                testsFailed++;
                Console.WriteLine("FAIL: fib.java execution failed or unexpected output");
                Console.WriteLine($"Exit code: {p.ExitCode}");
                Console.WriteLine($"Output: {output}");
                Console.WriteLine($"Error: {error}");
            }
        }
        catch (Exception e)
        {
            testsFailed++;
            Console.WriteLine($"FAIL: fib.java execution test threw exception: {e.Message}");
        }
        Console.WriteLine();
    }
}
