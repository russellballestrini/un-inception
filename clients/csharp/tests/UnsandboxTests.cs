// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
// Unit and Functional Tests for Unsandbox C# SDK (Mono)

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Security.Cryptography;
using System.Text;

/// <summary>
/// Unit tests for the Unsandbox SDK library functions.
/// These tests verify that exported library functions work correctly.
/// </summary>
public class UnitTests
{
    public static void Run()
    {
        Console.WriteLine("=== Unsandbox C# SDK Unit Tests ===\n");

        TestDetectLanguage();
        TestHmacSign();
        TestExtensionMap();
        TestBuildEnvContent();

        Console.WriteLine("\n=== Unit Tests Complete ===");
    }

    static void TestDetectLanguage()
    {
        Console.Write("DetectLanguage: ");
        var tests = new Dictionary<string, string>
        {
            { "test.py", "python" },
            { "script.js", "javascript" },
            { "main.go", "go" },
            { "app.rs", "rust" },
            { "Program.cs", "csharp" },
            { "Module.fs", "fsharp" },
            { "script.ps1", "powershell" }
        };

        int passed = 0;
        foreach (var test in tests)
        {
            var result = Unsandbox.DetectLanguage(test.Key);
            if (result == test.Value) passed++;
            else Console.Write($"[FAIL: {test.Key} -> {result}, expected {test.Value}] ");
        }

        if (passed == tests.Count)
            Console.WriteLine($"PASS ({passed}/{tests.Count})");
        else
            Console.WriteLine($"FAIL ({passed}/{tests.Count})");
    }

    static void TestHmacSign()
    {
        Console.Write("HmacSign: ");
        // Test vector: HMAC-SHA256("key", "message")
        var result = Unsandbox.HmacSign("key", "message");
        // Expected: 6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a
        var expected = "6e9ef29b75fffc5b7abae527d58fdadb2fe42e7219011976917343065f58ed4a";
        if (result == expected)
            Console.WriteLine("PASS");
        else
            Console.WriteLine($"FAIL (got {result}, expected {expected})");
    }

    static void TestExtensionMap()
    {
        Console.Write("ExtensionMap: ");
        // Test that the extension map contains expected entries
        var tests = new Dictionary<string, string>
        {
            { ".py", "python" },
            { ".js", "javascript" },
            { ".go", "go" },
            { ".rs", "rust" },
            { ".cs", "csharp" }
        };

        int passed = 0;
        foreach (var test in tests)
        {
            if (Unsandbox.ExtMap.TryGetValue(test.Key, out var lang) && lang == test.Value)
                passed++;
        }

        if (passed == tests.Count)
            Console.WriteLine($"PASS ({passed}/{tests.Count})");
        else
            Console.WriteLine($"FAIL ({passed}/{tests.Count})");
    }

    static void TestBuildEnvContent()
    {
        Console.Write("BuildEnvContent: ");
        var envs = new List<string> { "KEY1=value1", "KEY2=value2" };
        var result = Unsandbox.BuildEnvContent(envs, null);
        var hasKey1 = result.Contains("KEY1=value1");
        var hasKey2 = result.Contains("KEY2=value2");
        if (hasKey1 && hasKey2)
            Console.WriteLine("PASS");
        else
            Console.WriteLine($"FAIL (got: {result})");
    }
}

/// <summary>
/// Functional tests that require API credentials.
/// Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables.
/// </summary>
public class FunctionalTests
{
    public static void Run()
    {
        var publicKey = Environment.GetEnvironmentVariable("UNSANDBOX_PUBLIC_KEY");
        var secretKey = Environment.GetEnvironmentVariable("UNSANDBOX_SECRET_KEY");

        if (string.IsNullOrEmpty(publicKey) || string.IsNullOrEmpty(secretKey))
        {
            Console.WriteLine("=== Functional Tests Skipped (no API credentials) ===");
            return;
        }

        Console.WriteLine("=== Unsandbox C# SDK Functional Tests ===\n");

        TestHealthCheck();
        TestValidateKeys();
        TestGetLanguages();
        TestExecute();
        TestSessionList();
        TestServiceList();
        TestSnapshotList();
        TestImageList();

        Console.WriteLine("\n=== Functional Tests Complete ===");
    }

    static void TestHealthCheck()
    {
        Console.Write("HealthCheck: ");
        var result = Unsandbox.HealthCheck();
        Console.WriteLine(result ? "PASS" : "FAIL");
    }

    static void TestValidateKeys()
    {
        Console.Write("ValidateKeys: ");
        var result = Unsandbox.ValidateKeys();
        if (result != null && result.Valid)
            Console.WriteLine($"PASS (tier: {result.Tier})");
        else
            Console.WriteLine($"FAIL ({Unsandbox.LastError()})");
    }

    static void TestGetLanguages()
    {
        Console.Write("GetLanguages: ");
        var result = Unsandbox.GetLanguages();
        if (result.Count > 0)
            Console.WriteLine($"PASS ({result.Count} languages)");
        else
            Console.WriteLine($"FAIL ({Unsandbox.LastError()})");
    }

    static void TestExecute()
    {
        Console.Write("Execute: ");
        var result = Unsandbox.Execute("python", "print('hello from C# SDK')");
        if (result.Success && result.Stdout != null && result.Stdout.Contains("hello"))
            Console.WriteLine("PASS");
        else
            Console.WriteLine($"FAIL ({result.ErrorMessage ?? Unsandbox.LastError()})");
    }

    static void TestSessionList()
    {
        Console.Write("SessionList: ");
        var result = Unsandbox.SessionList();
        // Empty list is valid - just checking API call works
        Console.WriteLine($"PASS ({result.Count} sessions)");
    }

    static void TestServiceList()
    {
        Console.Write("ServiceList: ");
        var result = Unsandbox.ServiceList();
        Console.WriteLine($"PASS ({result.Count} services)");
    }

    static void TestSnapshotList()
    {
        Console.Write("SnapshotList: ");
        var result = Unsandbox.SnapshotList();
        Console.WriteLine($"PASS ({result.Count} snapshots)");
    }

    static void TestImageList()
    {
        Console.Write("ImageList: ");
        var result = Unsandbox.ImageList();
        Console.WriteLine($"PASS ({result.Count} images)");
    }
}

public class Program
{
    public static int Main(string[] args)
    {
        try
        {
            Console.WriteLine("Unsandbox C# SDK Tests (Mono)");
            Console.WriteLine("=============================\n");

            UnitTests.Run();
            Console.WriteLine();
            FunctionalTests.Run();
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Test error: {ex.Message}");
            return 1;
        }
    }
}
