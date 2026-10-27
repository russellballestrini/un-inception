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

test "normalizeOs and osJsonField (--os)" {
    try testing.expectEqualStrings("linux", un.normalizeOs("linux").?);
    try testing.expectEqualStrings("freebsd", un.normalizeOs("FreeBSD").?);
    try testing.expect(un.normalizeOs("openbsd") == null);
    try testing.expect(un.normalizeOs("") == null);
    try testing.expectEqualStrings("", try un.osJsonField(null));
    try testing.expectEqualStrings(",\"os\":\"freebsd\"", try un.osJsonField("FREEBSD"));
    try testing.expectError(error.InvalidOs, un.osJsonField("openbsd"));
    try testing.expectError(error.InvalidOs, un.osJsonField(""));
}

test "invalid os is rejected before any request" {
    // error.InvalidOs comes back before makeApiRequest, so nothing is sent
    try testing.expectError(error.InvalidOs, un.executeWithOs(testing.allocator, "bash", "echo hi", "openbsd", "pk", "sk"));
    try testing.expectError(error.InvalidOs, un.executeAsyncWithOs(testing.allocator, "bash", "echo hi", "", "pk", "sk"));
    try testing.expectError(error.InvalidOs, un.sessionCreateWithOs(testing.allocator, null, null, "windows", "pk", "sk"));
}

test "extractOsFlag (--os)" {
    var a0 = "un".*;
    var a1 = "--os".*;
    var a2 = "FreeBSD".*;
    var a3 = "script.py".*;
    var a4 = "--os=linux".*;
    var a5 = "openbsd".*;
    var a6 = "".*;
    var out: [8][:0]u8 = undefined;

    un.g_target_os = null;
    const r1 = un.extractOsFlag(&[_][:0]u8{ &a0, &a1, &a2, &a3 }, &out).?;
    try testing.expectEqual(@as(usize, 2), r1.len);
    try testing.expectEqualStrings("script.py", r1[1]);
    try testing.expectEqualStrings("freebsd", un.g_target_os.?);

    un.g_target_os = null;
    const r2 = un.extractOsFlag(&[_][:0]u8{ &a0, &a4, &a3 }, &out).?;
    try testing.expectEqual(@as(usize, 2), r2.len);
    try testing.expectEqualStrings("linux", un.g_target_os.?);

    un.g_target_os = null;
    const r3 = un.extractOsFlag(&[_][:0]u8{ &a0, &a3 }, &out).?;
    try testing.expectEqual(@as(usize, 2), r3.len);
    try testing.expect(un.g_target_os == null);

    try testing.expect(un.extractOsFlag(&[_][:0]u8{ &a0, &a1, &a5, &a3 }, &out) == null);
    try testing.expect(un.extractOsFlag(&[_][:0]u8{ &a0, &a1, &a6, &a3 }, &out) == null);
    try testing.expect(un.extractOsFlag(&[_][:0]u8{ &a0, &a1 }, &out) == null);
    un.g_target_os = null;
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
