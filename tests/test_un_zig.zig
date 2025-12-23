// Test suite for UN CLI Zig implementation
// Compile: zig build-exe test_un_zig.zig -O ReleaseFast
// Run: ./test_un_zig
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

const std = @import("std");
const http = std.http;
const json = std.json;
const fs = std.fs;

// Copy of detectLanguage from un.zig for testing
fn detectLanguage(filename: []const u8) ?[]const u8 {
    const ext = std.fs.path.extension(filename);

    const lang_map = .{
        .{ ".py", "python" },
        .{ ".js", "javascript" },
        .{ ".go", "go" },
        .{ ".rs", "rust" },
        .{ ".c", "c" },
        .{ ".cpp", "cpp" },
        .{ ".d", "d" },
        .{ ".zig", "zig" },
        .{ ".nim", "nim" },
        .{ ".v", "v" },
    };

    inline for (lang_map) |pair| {
        if (std.mem.eql(u8, ext, pair[0])) {
            return pair[1];
        }
    }

    return null;
}

fn testExtensionDetection(allocator: std.mem.Allocator) !bool {
    std.debug.print("=== Test 1: Extension Detection ===\n", .{});

    const TestCase = struct {
        filename: []const u8,
        expected: ?[]const u8,
    };

    const tests = [_]TestCase{
        .{ .filename = "script.py", .expected = "python" },
        .{ .filename = "app.js", .expected = "javascript" },
        .{ .filename = "main.go", .expected = "go" },
        .{ .filename = "program.rs", .expected = "rust" },
        .{ .filename = "code.c", .expected = "c" },
        .{ .filename = "app.cpp", .expected = "cpp" },
        .{ .filename = "prog.d", .expected = "d" },
        .{ .filename = "main.zig", .expected = "zig" },
        .{ .filename = "script.nim", .expected = "nim" },
        .{ .filename = "app.v", .expected = "v" },
        .{ .filename = "unknown.xyz", .expected = null },
    };

    var passed: usize = 0;
    var failed: usize = 0;

    for (tests) |test_case| {
        const result = detectLanguage(test_case.filename);

        const test_passed = blk: {
            if (test_case.expected == null and result == null) {
                break :blk true;
            } else if (test_case.expected != null and result != null) {
                if (std.mem.eql(u8, result.?, test_case.expected.?)) {
                    break :blk true;
                }
            }
            break :blk false;
        };

        if (test_passed) {
            std.debug.print("  PASS: {s} -> {s}\n", .{ test_case.filename, result orelse "null" });
            passed += 1;
        } else {
            std.debug.print("  FAIL: {s} -> got {s}, expected {s}\n", .{
                test_case.filename,
                result orelse "null",
                test_case.expected orelse "null",
            });
            failed += 1;
        }
    }

    std.debug.print("Extension Detection: {} passed, {} failed\n\n", .{ passed, failed });
    _ = allocator;
    return failed == 0;
}

fn testApiConnection(allocator: std.mem.Allocator) !bool {
    std.debug.print("=== Test 2: API Connection ===\n", .{});

    const api_key = std.process.getEnvVarOwned(allocator, "UNSANDBOX_API_KEY") catch {
        std.debug.print("  SKIP: UNSANDBOX_API_KEY not set\n", .{});
        std.debug.print("API Connection: skipped\n\n", .{});
        return true;
    };
    defer allocator.free(api_key);

    const json_body = "{\"language\":\"python\",\"code\":\"print('Hello from API test')\"}";

    var client = http.Client{ .allocator = allocator };
    defer client.deinit();

    const uri = try std.Uri.parse("https://api.unsandbox.com/execute");

    const auth_header_value = try std.fmt.allocPrint(allocator, "Bearer {s}", .{api_key});
    defer allocator.free(auth_header_value);

    var header_buffer: [8192]u8 = undefined;
    var req = try client.open(.POST, uri, .{
        .server_header_buffer = &header_buffer,
        .extra_headers = &.{
            .{ .name = "Content-Type", .value = "application/json" },
            .{ .name = "Authorization", .value = auth_header_value },
        },
    });
    defer req.deinit();

    req.transfer_encoding = .{ .content_length = json_body.len };
    try req.send();
    try req.writeAll(json_body);
    try req.finish();
    try req.wait();

    const response_body = try req.reader().readAllAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(response_body);

    if (std.mem.indexOf(u8, response_body, "Hello from API test") == null) {
        std.debug.print("  FAIL: Unexpected response: {s}\n", .{response_body});
        return false;
    }

    std.debug.print("  PASS: API connection successful\n", .{});
    std.debug.print("API Connection: passed\n\n", .{});
    return true;
}

fn testFibExecution(allocator: std.mem.Allocator) !bool {
    std.debug.print("=== Test 3: Functional Test (fib.go) ===\n", .{});

    _ = std.process.getEnvVarOwned(allocator, "UNSANDBOX_API_KEY") catch {
        std.debug.print("  SKIP: UNSANDBOX_API_KEY not set\n", .{});
        std.debug.print("Functional Test: skipped\n\n", .{});
        return true;
    };

    // Check if un binary exists
    fs.cwd().access("../un", .{}) catch {
        std.debug.print("  SKIP: ../un binary not found (run: cd .. && zig build-exe un.zig -O ReleaseFast)\n", .{});
        std.debug.print("Functional Test: skipped\n\n", .{});
        return true;
    };

    // Check if fib.go exists
    fs.cwd().access("fib.go", .{}) catch {
        std.debug.print("  SKIP: fib.go not found\n", .{});
        std.debug.print("Functional Test: skipped\n\n", .{});
        return true;
    };

    var child = std.process.Child.init(&[_][]const u8{ "../un", "fib.go" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;

    try child.spawn();

    const stdout = try child.stdout.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(stdout);

    const stderr = try child.stderr.?.readToEndAlloc(allocator, 10 * 1024 * 1024);
    defer allocator.free(stderr);

    const term = try child.wait();

    if (term.Exited != 0) {
        std.debug.print("  FAIL: Command failed with exit code: {}\n", .{term.Exited});
        std.debug.print("  STDERR: {s}\n", .{stderr});
        return false;
    }

    if (std.mem.indexOf(u8, stdout, "fib(10) = 55") == null) {
        std.debug.print("  FAIL: Expected output to contain 'fib(10) = 55', got: {s}\n", .{stdout});
        return false;
    }

    std.debug.print("  PASS: fib.go executed successfully\n", .{});
    std.debug.print("  Output: {s}", .{stdout});
    std.debug.print("Functional Test: passed\n\n", .{});
    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    std.debug.print("UN CLI Zig Implementation Test Suite\n", .{});
    std.debug.print("=====================================\n\n", .{});

    var all_passed = true;

    if (!try testExtensionDetection(allocator)) {
        all_passed = false;
    }

    if (!try testApiConnection(allocator)) {
        all_passed = false;
    }

    if (!try testFibExecution(allocator)) {
        all_passed = false;
    }

    std.debug.print("=====================================\n", .{});
    if (all_passed) {
        std.debug.print("RESULT: ALL TESTS PASSED\n", .{});
        std.process.exit(0);
    } else {
        std.debug.print("RESULT: SOME TESTS FAILED\n", .{});
        std.process.exit(1);
    }
}
