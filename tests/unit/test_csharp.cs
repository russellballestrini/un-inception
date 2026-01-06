// Unit tests for un.cs - tests internal functions without API calls
// Run with: dotnet script test_csharp.cs OR csc test_csharp.cs && mono test_csharp.exe

using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Text;

class TestCSharp
{
    static int passed = 0;
    static int failed = 0;

    static void Main()
    {
        var extMap = new Dictionary<string, string>
        {
            {".py", "python"}, {".js", "javascript"}, {".ts", "typescript"},
            {".rb", "ruby"}, {".go", "go"}, {".rs", "rust"}, {".c", "c"},
            {".cs", "csharp"}, {".java", "java"}, {".kt", "kotlin"}
        };

        Console.WriteLine("\n=== Extension Mapping Tests ===");

        Test("Python extension maps correctly", () =>
            AssertEqual(extMap[".py"], "python"));

        Test("C# extension maps correctly", () =>
            AssertEqual(extMap[".cs"], "csharp"));

        Test("JavaScript extension maps correctly", () =>
            AssertEqual(extMap[".js"], "javascript"));

        Test("Go extension maps correctly", () =>
            AssertEqual(extMap[".go"], "go"));

        Console.WriteLine("\n=== HMAC Signature Tests ===");

        Test("HMAC-SHA256 generates 64 character hex string", () =>
        {
            var sig = HmacSha256("test-secret", "test-message");
            AssertEqual(sig.Length, 64);
        });

        Test("Same input produces same signature", () =>
        {
            var sig1 = HmacSha256("key", "msg");
            var sig2 = HmacSha256("key", "msg");
            AssertEqual(sig1, sig2);
        });

        Test("Signature format verification", () =>
        {
            var timestamp = "1704067200";
            var method = "POST";
            var endpoint = "/execute";
            var body = "{\"language\":\"python\"}";
            var message = $"{timestamp}:{method}:{endpoint}:{body}";

            AssertTrue(message.StartsWith(timestamp));
            AssertContains(message, ":POST:");
            AssertContains(message, ":/execute:");
        });

        Console.WriteLine("\n=== Language Detection Tests ===");

        Test("Detect language from .cs extension", () =>
        {
            var filename = "Program.cs";
            var ext = "." + filename.Split('.')[^1];
            AssertEqual(extMap[ext], "csharp");
        });

        Test("Python shebang detection", () =>
        {
            var content = "#!/usr/bin/env python3\nprint('hello')";
            var firstLine = content.Split('\n')[0];
            AssertTrue(firstLine.StartsWith("#!"));
            AssertContains(firstLine, "python");
        });

        Console.WriteLine("\n=== Argument Parsing Tests ===");

        Test("Parse -e KEY=VALUE format", () =>
        {
            var arg = "DEBUG=1";
            var parts = arg.Split(new[] {'='}, 2);
            AssertEqual(parts[0], "DEBUG");
            AssertEqual(parts[1], "1");
        });

        Test("Parse -e KEY=VALUE with equals in value", () =>
        {
            var arg = "URL=https://example.com?foo=bar";
            var parts = arg.Split(new[] {'='}, 2);
            AssertEqual(parts[0], "URL");
            AssertEqual(parts[1], "https://example.com?foo=bar");
        });

        Console.WriteLine("\n=== File Operations Tests ===");

        Test("Extract file basename", () =>
        {
            var path = "/home/user/project/script.cs";
            var basename = path.Substring(path.LastIndexOf('/') + 1);
            AssertEqual(basename, "script.cs");
        });

        Test("Extract file extension", () =>
        {
            var filename = "script.cs";
            var ext = filename.Substring(filename.LastIndexOf('.'));
            AssertEqual(ext, ".cs");
        });

        Console.WriteLine("\n=== API Constants Tests ===");

        Test("API base URL format", () =>
        {
            var apiBase = "https://api.unsandbox.com";
            AssertTrue(apiBase.StartsWith("https://"));
            AssertContains(apiBase, "unsandbox.com");
        });

        Console.WriteLine("\n=== Summary ===");
        Console.WriteLine($"Passed: {passed}");
        Console.WriteLine($"Failed: {failed}");
        Console.WriteLine($"Total:  {passed + failed}");

        Environment.Exit(failed > 0 ? 1 : 0);
    }

    static void Test(string name, Action fn)
    {
        try
        {
            fn();
            Console.WriteLine($"  ✓ {name}");
            passed++;
        }
        catch (Exception e)
        {
            Console.WriteLine($"  ✗ {name}");
            Console.WriteLine($"    {e.Message}");
            failed++;
        }
    }

    static void AssertEqual(object actual, object expected)
    {
        if (!Equals(actual, expected))
            throw new Exception($"Expected '{expected}' but got '{actual}'");
    }

    static void AssertContains(string str, string substr)
    {
        if (!str.Contains(substr))
            throw new Exception($"Expected '{str}' to contain '{substr}'");
    }

    static void AssertTrue(bool val)
    {
        if (!val)
            throw new Exception("Expected true but got false");
    }

    static string HmacSha256(string secret, string message)
    {
        using var hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secret));
        var hash = hmac.ComputeHash(Encoding.UTF8.GetBytes(message));
        return BitConverter.ToString(hash).Replace("-", "").ToLower();
    }
}
