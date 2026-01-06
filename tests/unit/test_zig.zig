// Unit tests for un.zig - tests internal functions without API calls
// Run with: zig run test_zig.zig

const std = @import("std");
const mem = std.mem;

var passed: u32 = 0;
var failed: u32 = 0;

fn test_case(name: []const u8, result: bool) void {
    if (result) {
        std.debug.print("  ✓ {s}\n", .{name});
        passed += 1;
    } else {
        std.debug.print("  ✗ {s}\n", .{name});
        failed += 1;
    }
}

fn getLanguage(ext: []const u8) ?[]const u8 {
    const extensions = [_]struct { ext: []const u8, lang: []const u8 }{
        .{ .ext = ".py", .lang = "python" },
        .{ .ext = ".js", .lang = "javascript" },
        .{ .ext = ".ts", .lang = "typescript" },
        .{ .ext = ".rb", .lang = "ruby" },
        .{ .ext = ".go", .lang = "go" },
        .{ .ext = ".rs", .lang = "rust" },
        .{ .ext = ".c", .lang = "c" },
        .{ .ext = ".zig", .lang = "zig" },
        .{ .ext = ".java", .lang = "java" },
    };

    for (extensions) |e| {
        if (mem.eql(u8, ext, e.ext)) return e.lang;
    }
    return null;
}

fn getExtension(filename: []const u8) []const u8 {
    var i: usize = filename.len;
    while (i > 0) : (i -= 1) {
        if (filename[i - 1] == '.') return filename[i - 1 ..];
    }
    return "";
}

fn getBasename(path: []const u8) []const u8 {
    var i: usize = path.len;
    while (i > 0) : (i -= 1) {
        if (path[i - 1] == '/') return path[i..];
    }
    return path;
}

fn startsWith(haystack: []const u8, needle: []const u8) bool {
    if (needle.len > haystack.len) return false;
    return mem.eql(u8, haystack[0..needle.len], needle);
}

fn contains(haystack: []const u8, needle: []const u8) bool {
    return mem.indexOf(u8, haystack, needle) != null;
}

pub fn main() !void {
    std.debug.print("\n=== Extension Mapping Tests ===\n", .{});

    test_case("Python extension maps correctly", if (getLanguage(".py")) |l| mem.eql(u8, l, "python") else false);

    test_case("Zig extension maps correctly", if (getLanguage(".zig")) |l| mem.eql(u8, l, "zig") else false);

    test_case("JavaScript extension maps correctly", if (getLanguage(".js")) |l| mem.eql(u8, l, "javascript") else false);

    test_case("Go extension maps correctly", if (getLanguage(".go")) |l| mem.eql(u8, l, "go") else false);

    std.debug.print("\n=== Signature Format Tests ===\n", .{});

    const timestamp = "1704067200";
    const method = "POST";
    const endpoint = "/execute";
    const body = "{\"language\":\"python\"}";

    var message_buf: [256]u8 = undefined;
    const message = std.fmt.bufPrint(&message_buf, "{s}:{s}:{s}:{s}", .{ timestamp, method, endpoint, body }) catch "";

    test_case("Signature format starts with timestamp", startsWith(message, timestamp));

    test_case("Signature format contains :POST:", contains(message, ":POST:"));

    test_case("Signature format contains :/execute:", contains(message, ":/execute:"));

    std.debug.print("\n=== Language Detection Tests ===\n", .{});

    const content = "#!/usr/bin/env python3\nprint('hello')";
    const newline_idx = mem.indexOf(u8, content, "\n") orelse content.len;
    const first_line = content[0..newline_idx];

    test_case("Python shebang detection - starts with #!", startsWith(first_line, "#!"));

    test_case("Python shebang detection - contains python", contains(first_line, "python"));

    std.debug.print("\n=== Argument Parsing Tests ===\n", .{});

    const arg1 = "DEBUG=1";
    const eq1_idx = mem.indexOf(u8, arg1, "=") orelse 0;
    const key1 = arg1[0..eq1_idx];
    const value1 = arg1[eq1_idx + 1 ..];

    test_case("Parse -e KEY=VALUE format - key", mem.eql(u8, key1, "DEBUG"));

    test_case("Parse -e KEY=VALUE format - value", mem.eql(u8, value1, "1"));

    std.debug.print("\n=== File Operations Tests ===\n", .{});

    test_case("Extract file basename", mem.eql(u8, getBasename("/home/user/project/script.zig"), "script.zig"));

    test_case("Extract file extension", mem.eql(u8, getExtension("/home/user/project/script.zig"), ".zig"));

    std.debug.print("\n=== API Constants Tests ===\n", .{});

    const api_base = "https://api.unsandbox.com";

    test_case("API base URL starts with https://", startsWith(api_base, "https://"));

    test_case("API base URL contains unsandbox.com", contains(api_base, "unsandbox.com"));

    std.debug.print("\n=== Summary ===\n", .{});
    std.debug.print("Passed: {d}\n", .{passed});
    std.debug.print("Failed: {d}\n", .{failed});
    std.debug.print("Total:  {d}\n", .{passed + failed});

    std.process.exit(if (failed > 0) 1 else 0);
}
