// Tests for the D unsandbox SDK
// Compile: dmd -unittest -main test_un.d ../src/un.d -of=test_un
// Run: ./test_un

import std.stdio;
import std.process : environment;
import std.algorithm : canFind;

// Note: In D, unittest blocks are automatically discovered and run
// when compiling with -unittest flag

// ============================================================================
// Unit Tests - Test exported library functions
// ============================================================================

unittest {
    writeln("Testing detectLanguage...");
    assert(detectLanguage("script.py") == "python");
    assert(detectLanguage("script.js") == "javascript");
    assert(detectLanguage("script.ts") == "typescript");
    assert(detectLanguage("script.go") == "go");
    assert(detectLanguage("script.rs") == "rust");
    assert(detectLanguage("script.c") == "c");
    assert(detectLanguage("script.cpp") == "cpp");
    assert(detectLanguage("script.d") == "d");
    assert(detectLanguage("script.zig") == "zig");
    assert(detectLanguage("script.sh") == "bash");
    assert(detectLanguage("script.rb") == "ruby");
    assert(detectLanguage("script.php") == "php");
    assert(detectLanguage("script.unknown") == "");
    assert(detectLanguage("script") == "");
    writeln("  PASS");
}

unittest {
    writeln("Testing hmacSign...");
    string secretKey = "test-secret";
    string message = "test-message";

    string result = hmacSign(secretKey, message);

    // Should return a 64-character hex string
    assert(result.length == 64, "HMAC signature should be 64 characters");

    // Should be deterministic
    string result2 = hmacSign(secretKey, message);
    assert(result == result2, "HMAC sign should be deterministic");

    // Different inputs should produce different outputs
    string result3 = hmacSign(secretKey, "different-message");
    assert(result != result3, "Different inputs should produce different signatures");
    writeln("  PASS");
}

unittest {
    writeln("Testing sdkVersion...");
    string v = sdkVersion();
    assert(v.length > 0, "Version should not be empty");
    // Should be in semver format (at least "0.0.0")
    assert(v.length >= 5, "Version should be in semver format");
    writeln("  PASS");
}

unittest {
    writeln("Testing lastError...");
    // Set an error
    setLastError("test error message");

    // Retrieve it
    string err = lastError();
    assert(err == "test error message");

    // Clear it
    setLastError("");
    err = lastError();
    assert(err == "", "Error should be cleared");
    writeln("  PASS");
}

unittest {
    writeln("Testing escapeJson...");
    assert(escapeJson("hello") == "hello");
    assert(escapeJson("hello\"world") == "hello\\\"world");
    assert(escapeJson("line1\nline2") == "line1\\nline2");
    assert(escapeJson("tab\there") == "tab\\there");
    assert(escapeJson("back\\slash") == "back\\\\slash");
    writeln("  PASS");
}

// ============================================================================
// Integration Tests - Test SDK internal consistency
// ============================================================================

unittest {
    writeln("Testing computeHmac...");
    string key = "test-key";
    string msg = "test-message";

    string sig1 = computeHmac(key, msg);
    string sig2 = computeHmac(key, msg);

    // Should be deterministic
    assert(sig1 == sig2, "HMAC should be deterministic");

    // Should produce different results for different inputs
    string sig3 = computeHmac(key, "different");
    assert(sig1 != sig3, "Different inputs should produce different signatures");
    writeln("  PASS");
}

unittest {
    writeln("Testing buildAuthHeaders...");
    string pk = "unsb-pk-test-test-test-test";
    string sk = "unsb-sk-test1-test2-test3-test4";

    string headers = buildAuthHeaders("POST", "/execute", "{}", pk, sk);

    // Should contain auth header
    assert(headers.canFind("Authorization: Bearer " ~ pk));
    // Should contain timestamp header
    assert(headers.canFind("X-Timestamp:"));
    // Should contain signature header
    assert(headers.canFind("X-Signature:"));
    writeln("  PASS");
}

// ============================================================================
// Functional Tests - Test against real API (requires credentials)
// ============================================================================

bool hasCredentials() {
    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");
    return pk.length > 0 && sk.length > 0;
}

unittest {
    writeln("Testing healthCheck (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    bool healthy = healthCheck();
    writeln("  Health check result: ", healthy ? "healthy" : "unhealthy");
    writeln("  PASS");
}

unittest {
    writeln("Testing getLanguages (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = getLanguages(pk, sk);
    assert(result.length > 0, "Languages result should not be empty");
    // Should contain python
    assert(result.canFind("python"), "Languages should include python");
    writeln("  PASS");
}

unittest {
    writeln("Testing validateKeysFn (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = validateKeysFn(pk, sk);
    assert(result.length > 0, "Validate keys result should not be empty");
    writeln("  PASS");
}

unittest {
    writeln("Testing execute (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = execute("python", "print('hello from d test')", pk, sk);
    assert(result.length > 0, "Execute result should not be empty");
    // Should contain output
    assert(result.canFind("stdout") || result.canFind("output"), "Result should contain output");
    writeln("  PASS");
}

unittest {
    writeln("Testing sessionList (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = sessionList(pk, sk);
    assert(result.length > 0, "Session list result should not be empty");
    writeln("  PASS");
}

unittest {
    writeln("Testing serviceListFn (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = serviceListFn(pk, sk);
    assert(result.length > 0, "Service list result should not be empty");
    writeln("  PASS");
}

unittest {
    writeln("Testing snapshotList (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = snapshotList(pk, sk);
    assert(result.length > 0, "Snapshot list result should not be empty");
    writeln("  PASS");
}

unittest {
    writeln("Testing imageList (functional)...");
    if (!hasCredentials()) {
        writeln("  SKIP (no credentials)");
        return;
    }

    string pk = environment.get("UNSANDBOX_PUBLIC_KEY", "");
    string sk = environment.get("UNSANDBOX_SECRET_KEY", "");

    string result = imageList("", pk, sk);
    assert(result.length > 0, "Image list result should not be empty");
    writeln("  PASS");
}

void main() {
    writeln("===== D SDK Tests Complete =====");
}
