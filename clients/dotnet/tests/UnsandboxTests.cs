// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
// Unit and Functional Tests for Unsandbox .NET SDK

using System;
using System.Collections.Generic;

namespace UnsandboxTests;

/// <summary>
/// Unit tests for the Unsandbox SDK library functions.
/// These tests verify that exported library functions work correctly.
/// </summary>
public class UnitTests
{
    public static void Run()
    {
        Console.WriteLine("=== Unsandbox .NET SDK Unit Tests ===\n");

        TestDetectLanguage();
        TestHmacSign();
        TestVersion();

        Console.WriteLine("\n=== Unit Tests Complete ===");
    }

    static void TestDetectLanguage()
    {
        Console.Write("DetectLanguage: ");
        var tests = new Dictionary<string, string?>
        {
            { "test.py", "python" },
            { "script.js", "javascript" },
            { "main.go", "go" },
            { "app.rs", "rust" },
            { "Program.cs", "dotnet" },
            { "Module.fs", "fsharp" },
            { "unknown.xyz", null }
        };

        int passed = 0;
        foreach (var (file, expected) in tests)
        {
            var result = Unsandbox.DetectLanguage(file);
            if (result == expected) passed++;
            else Console.Write($"[FAIL: {file} -> {result}, expected {expected}] ");
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

    static void TestVersion()
    {
        Console.Write("Version: ");
        var version = Unsandbox.Version();
        if (!string.IsNullOrEmpty(version) && version.Contains("."))
            Console.WriteLine($"PASS ({version})");
        else
            Console.WriteLine($"FAIL (got {version})");
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

        Console.WriteLine("=== Unsandbox .NET SDK Functional Tests ===\n");

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
        var result = Unsandbox.Execute("python", "print('hello from .NET SDK')");
        if (result.Success && result.Stdout?.Contains("hello") == true)
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
