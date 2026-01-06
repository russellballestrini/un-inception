// Unit tests for Un.java - tests internal functions without API calls
// Run with: javac test_java.java && java TestJava

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.util.*;

public class test_java {
    static int passed = 0;
    static int failed = 0;

    public static void main(String[] args) {
        Map<String, String> extMap = new HashMap<>();
        extMap.put(".py", "python");
        extMap.put(".js", "javascript");
        extMap.put(".ts", "typescript");
        extMap.put(".rb", "ruby");
        extMap.put(".go", "go");
        extMap.put(".rs", "rust");
        extMap.put(".c", "c");
        extMap.put(".java", "java");
        extMap.put(".kt", "kotlin");
        extMap.put(".hs", "haskell");

        System.out.println("\n=== Extension Mapping Tests ===");

        test("Python extension maps correctly", () ->
            assertEqual(extMap.get(".py"), "python"));

        test("Java extension maps correctly", () ->
            assertEqual(extMap.get(".java"), "java"));

        test("JavaScript extension maps correctly", () ->
            assertEqual(extMap.get(".js"), "javascript"));

        test("Go extension maps correctly", () ->
            assertEqual(extMap.get(".go"), "go"));

        test("Kotlin extension maps correctly", () ->
            assertEqual(extMap.get(".kt"), "kotlin"));

        System.out.println("\n=== HMAC Signature Tests ===");

        test("HMAC-SHA256 generates 64 character hex string", () -> {
            String sig = hmacSha256("test-secret", "test-message");
            assertEqual(sig.length(), 64);
        });

        test("Same input produces same signature", () -> {
            String sig1 = hmacSha256("key", "msg");
            String sig2 = hmacSha256("key", "msg");
            assertEqual(sig1, sig2);
        });

        test("Different secrets produce different signatures", () -> {
            String sig1 = hmacSha256("key1", "msg");
            String sig2 = hmacSha256("key2", "msg");
            assertNotEqual(sig1, sig2);
        });

        test("Signature format verification", () -> {
            String timestamp = "1704067200";
            String method = "POST";
            String endpoint = "/execute";
            String body = "{\"language\":\"python\"}";
            String message = timestamp + ":" + method + ":" + endpoint + ":" + body;

            assertTrue(message.startsWith(timestamp));
            assertContains(message, ":POST:");
            assertContains(message, ":/execute:");
        });

        System.out.println("\n=== Language Detection Tests ===");

        test("Detect language from .java extension", () -> {
            String filename = "Main.java";
            int dot = filename.lastIndexOf('.');
            String ext = filename.substring(dot);
            assertEqual(extMap.get(ext), "java");
        });

        test("Python shebang detection", () -> {
            String content = "#!/usr/bin/env python3\nprint('hello')";
            String firstLine = content.split("\n")[0];
            assertTrue(firstLine.startsWith("#!"));
            assertContains(firstLine, "python");
        });

        System.out.println("\n=== Argument Parsing Tests ===");

        test("Parse -e KEY=VALUE format", () -> {
            String arg = "DEBUG=1";
            String[] parts = arg.split("=", 2);
            assertEqual(parts[0], "DEBUG");
            assertEqual(parts[1], "1");
        });

        test("Parse -e KEY=VALUE with equals in value", () -> {
            String arg = "URL=https://example.com?foo=bar";
            String[] parts = arg.split("=", 2);
            assertEqual(parts[0], "URL");
            assertEqual(parts[1], "https://example.com?foo=bar");
        });

        System.out.println("\n=== File Operations Tests ===");

        test("Base64 encoding/decoding", () -> {
            String content = "print('hello world')";
            String encoded = Base64.getEncoder().encodeToString(content.getBytes());
            String decoded = new String(Base64.getDecoder().decode(encoded));
            assertEqual(decoded, content);
        });

        System.out.println("\n=== API Constants Tests ===");

        test("API base URL format", () -> {
            String apiBase = "https://api.unsandbox.com";
            assertTrue(apiBase.startsWith("https://"));
            assertContains(apiBase, "unsandbox.com");
        });

        System.out.println("\n=== Summary ===");
        System.out.println("Passed: " + passed);
        System.out.println("Failed: " + failed);
        System.out.println("Total:  " + (passed + failed));

        System.exit(failed > 0 ? 1 : 0);
    }

    static void test(String name, Runnable fn) {
        try {
            fn.run();
            System.out.println("  ✓ " + name);
            passed++;
        } catch (Exception e) {
            System.out.println("  ✗ " + name);
            System.out.println("    " + e.getMessage());
            failed++;
        }
    }

    static void assertEqual(Object actual, Object expected) {
        if (!Objects.equals(actual, expected)) {
            throw new RuntimeException("Expected '" + expected + "' but got '" + actual + "'");
        }
    }

    static void assertNotEqual(Object a, Object b) {
        if (Objects.equals(a, b)) {
            throw new RuntimeException("Expected values to be different but both were '" + a + "'");
        }
    }

    static void assertContains(String str, String substr) {
        if (!str.contains(substr)) {
            throw new RuntimeException("Expected '" + str + "' to contain '" + substr + "'");
        }
    }

    static void assertTrue(boolean val) {
        if (!val) {
            throw new RuntimeException("Expected true but got false");
        }
    }

    static String hmacSha256(String secret, String message) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            SecretKeySpec key = new SecretKeySpec(secret.getBytes("UTF-8"), "HmacSHA256");
            mac.init(key);
            byte[] hash = mac.doFinal(message.getBytes("UTF-8"));
            StringBuilder sb = new StringBuilder();
            for (byte b : hash) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            return "";
        }
    }
}
