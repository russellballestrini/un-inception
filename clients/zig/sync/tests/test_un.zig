// Tests for the Zig unsandbox SDK
// Run with: zig test tests/test_un.zig

const std = @import("std");
const un = @import("../src/un.zig");
const testing = std.testing;
const mem = std.mem;

// ============================================================================
// Unit Tests - Test exported library functions
// ============================================================================

test "detectLanguage" {
    try testing.expectEqualStrings("python", un.detectLanguage("script.py") orelse "");
    try testing.expectEqualStrings("javascript", un.detectLanguage("script.js") orelse "");
    try testing.expectEqualStrings("typescript", un.detectLanguage("script.ts") orelse "");
    try testing.expectEqualStrings("go", un.detectLanguage("script.go") orelse "");
    try testing.expectEqualStrings("rust", un.detectLanguage("script.rs") orelse "");
    try testing.expectEqualStrings("c", un.detectLanguage("script.c") orelse "");
    try testing.expectEqualStrings("cpp", un.detectLanguage("script.cpp") orelse "");
    try testing.expectEqualStrings("d", un.detectLanguage("script.d") orelse "");
    try testing.expectEqualStrings("zig", un.detectLanguage("script.zig") orelse "");
    try testing.expectEqualStrings("bash", un.detectLanguage("script.sh") orelse "");
    try testing.expectEqualStrings("ruby", un.detectLanguage("script.rb") orelse "");
    try testing.expectEqualStrings("php", un.detectLanguage("script.php") orelse "");
    try testing.expect(un.detectLanguage("script.unknown") == null);
    try testing.expect(un.detectLanguage("script") == null);
}

test "version" {
    const v = un.version();
    try testing.expect(v.len > 0);
    // Should be in semver format (at least "0.0.0")
    try testing.expect(v.len >= 5);
}

test "lastError" {
    // Set an error
    un.setLastError("test error message");

    // Retrieve it
    const err = un.lastError();
    try testing.expectEqualStrings("test error message", err);

    // Clear it
    un.setLastError("");
    const err2 = un.lastError();
    try testing.expectEqualStrings("", err2);
}

test "SDK_VERSION constant" {
    try testing.expect(un.SDK_VERSION.len > 0);
    try testing.expect(un.SDK_VERSION.len >= 5);
}

// ============================================================================
// Integration Tests - Test SDK internal consistency
// ============================================================================

test "hmacSign" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const secret_key = "test-secret";
    const message = "test-message";

    const result = try un.hmacSign(allocator, secret_key, message);
    defer allocator.free(result);

    // Should return a 64-character hex string
    try testing.expectEqual(@as(usize, 64), result.len);

    // Should be deterministic
    const result2 = try un.hmacSign(allocator, secret_key, message);
    defer allocator.free(result2);
    try testing.expectEqualStrings(result, result2);

    // Different inputs should produce different outputs
    const result3 = try un.hmacSign(allocator, secret_key, "different-message");
    defer allocator.free(result3);
    try testing.expect(!mem.eql(u8, result, result3));
}

// ============================================================================
// Functional Tests - Test against real API (requires credentials)
// These tests are marked as skipped by default since they require credentials
// ============================================================================

fn hasCredentials() bool {
    const pk = std.posix.getenv("UNSANDBOX_PUBLIC_KEY") orelse return false;
    const sk = std.posix.getenv("UNSANDBOX_SECRET_KEY") orelse return false;
    return pk.len > 0 and sk.len > 0;
}

fn getCredentials() ?struct { pk: []const u8, sk: []const u8 } {
    const pk = std.posix.getenv("UNSANDBOX_PUBLIC_KEY") orelse return null;
    const sk = std.posix.getenv("UNSANDBOX_SECRET_KEY") orelse return null;
    if (pk.len == 0 or sk.len == 0) return null;
    return .{ .pk = pk, .sk = sk };
}

test "healthCheck functional" {
    if (!hasCredentials()) {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const healthy = try un.healthCheck(allocator);
    std.debug.print("Health check result: {}\n", .{healthy});
}

test "getLanguages functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.getLanguages(allocator, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
    // Should contain python
    try testing.expect(mem.indexOf(u8, result, "python") != null);
}

test "validateKeys functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.validateKeys(allocator, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
}

test "sessionList functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.sessionList(allocator, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
}

test "serviceList functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.serviceList(allocator, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
}

test "snapshotList functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.snapshotList(allocator, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
}

test "imageList functional" {
    const creds = getCredentials() orelse {
        std.debug.print("SKIP (no credentials)\n", .{});
        return;
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const result = try un.imageList(allocator, null, creds.pk, creds.sk);
    defer allocator.free(result);

    try testing.expect(result.len > 0);
}
