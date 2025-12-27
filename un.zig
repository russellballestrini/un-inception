// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software


// UN CLI - Zig Implementation (using curl subprocess for simplicity)
// Compile: zig build-exe un.zig -O ReleaseFast
// Usage:
//   un.zig script.py
//   un.zig -e KEY=VALUE script.py
//   un.zig session --list
//   un.zig service --name web --ports 8080

// Note: This implementation uses system() to call curl for simplicity
// A production version would use Zig's HTTP client library

const std = @import("std");
const fs = std.fs;
const process = std.process;
const mem = std.mem;

const API_BASE = "https://api.unsandbox.com";
const PORTAL_BASE = "https://unsandbox.com";

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try process.argsAlloc(allocator);
    defer process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} [options] <source_file>\n", .{args[0]});
        std.debug.print("       {s} session [options]\n", .{args[0]});
        std.debug.print("       {s} service [options]\n", .{args[0]});
        std.debug.print("       {s} key [--extend]\n", .{args[0]});
        return 1;
    }

    const api_key = std.process.getEnvVarOwned(allocator, "UNSANDBOX_API_KEY") catch blk: {
        break :blk try allocator.dupe(u8, "");
    };
    defer allocator.free(api_key);

    // Handle session command
    if (mem.eql(u8, args[1], "session")) {
        var list = false;
        var kill: ?[]const u8 = null;
        var shell: ?[]const u8 = null;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--list")) {
                list = true;
            } else if (mem.eql(u8, args[i], "--kill") and i + 1 < args.len) {
                i += 1;
                kill = args[i];
            } else if (mem.eql(u8, args[i], "--shell") and i + 1 < args.len) {
                i += 1;
                shell = args[i];
            }
        }

        if (list) {
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/sessions' -H 'Authorization: Bearer {s}'", .{ API_BASE, api_key });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (kill) |k| {
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}/sessions/{s}' -H 'Authorization: Bearer {s}'", .{ API_BASE, k, api_key });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\x1b[32mSession terminated: {s}\x1b[0m\n", .{k});
        } else {
            const sh = shell orelse "bash";
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer {s}' -d '{{\"shell\":\"{s}\"}}'", .{ API_BASE, api_key, sh });
            defer allocator.free(cmd);
            std.debug.print("\x1b[33mCreating session...\x1b[0m\n", .{});
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        }
        return 0;
    }

    // Handle service command
    if (mem.eql(u8, args[1], "service")) {
        var list = false;
        var name: ?[]const u8 = null;
        var ports: ?[]const u8 = null;
        var service_type: ?[]const u8 = null;
        var info: ?[]const u8 = null;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--list")) {
                list = true;
            } else if (mem.eql(u8, args[i], "--name") and i + 1 < args.len) {
                i += 1;
                name = args[i];
            } else if (mem.eql(u8, args[i], "--ports") and i + 1 < args.len) {
                i += 1;
                ports = args[i];
            } else if (mem.eql(u8, args[i], "--type") and i + 1 < args.len) {
                i += 1;
                service_type = args[i];
            } else if (mem.eql(u8, args[i], "--info") and i + 1 < args.len) {
                i += 1;
                info = args[i];
            }
        }

        if (list) {
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services' -H 'Authorization: Bearer {s}'", .{ API_BASE, api_key });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (info) |inf| {
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services/{s}' -H 'Authorization: Bearer {s}'", .{ API_BASE, inf, api_key });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (name) |n| {
            var json_buf: [4096]u8 = undefined;
            var json_stream = std.io.fixedBufferStream(&json_buf);
            const writer = json_stream.writer();
            try writer.print("{{\"name\":\"{s}\"", .{n});
            if (ports) |p| {
                try writer.print(",\"ports\":[{s}]", .{p});
            }
            if (service_type) |t| {
                try writer.print(",\"service_type\":\"{s}\"", .{t});
            }
            try writer.writeAll("}");
            const json_str = json_stream.getWritten();

            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer {s}' -d '{s}'", .{ API_BASE, api_key, json_str });
            defer allocator.free(cmd);
            std.debug.print("\x1b[33mCreating service...\x1b[0m\n", .{});
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        }
        return 0;
    }

    // Handle key command
    if (mem.eql(u8, args[1], "key")) {
        var extend = false;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--extend")) {
                extend = true;
            }
        }

        if (extend) {
            // First validate to get the public_key
            const json_file = "/tmp/unsandbox_key_validate.json";
            const cmd_validate = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/keys/validate' -H 'Content-Type: application/json' -H 'Authorization: Bearer {s}' -o {s}", .{ PORTAL_BASE, api_key, json_file });
            defer allocator.free(cmd_validate);
            _ = std.c.system(cmd_validate.ptr);

            // Read the JSON response to extract public_key
            const json_content = fs.cwd().readFileAlloc(allocator, json_file, 1024 * 1024) catch |err| {
                std.debug.print("\x1b[31mError reading validation response: {}\x1b[0m\n", .{err});
                std.fs.cwd().deleteFile(json_file) catch {};
                return 1;
            };
            defer allocator.free(json_content);
            std.fs.cwd().deleteFile(json_file) catch {};

            // Simple JSON parsing to find public_key (looking for "public_key":"value")
            const pk_prefix = "\"public_key\":\"";
            var public_key: ?[]const u8 = null;
            if (mem.indexOf(u8, json_content, pk_prefix)) |start_idx| {
                const value_start = start_idx + pk_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    public_key = json_content[value_start..end_idx];
                }
            }

            if (public_key) |pk| {
                const url = try std.fmt.allocPrint(allocator, "{s}/keys/extend?pk={s}", .{ PORTAL_BASE, pk });
                defer allocator.free(url);
                std.debug.print("\x1b[33mOpening browser to extend key...\x1b[0m\n", .{});
                const open_cmd = try std.fmt.allocPrint(allocator, "xdg-open '{s}' 2>/dev/null || open '{s}' 2>/dev/null || start '{s}' 2>/dev/null", .{ url, url, url });
                defer allocator.free(open_cmd);
                _ = std.c.system(open_cmd.ptr);
            } else {
                std.debug.print("\x1b[31mError: Could not extract public_key from response\x1b[0m\n", .{});
                return 1;
            }
        } else {
            // Regular validation
            const json_file = "/tmp/unsandbox_key_validate.json";
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/keys/validate' -H 'Content-Type: application/json' -H 'Authorization: Bearer {s}' -o {s}", .{ PORTAL_BASE, api_key, json_file });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);

            // Read and parse the response
            const json_content = fs.cwd().readFileAlloc(allocator, json_file, 1024 * 1024) catch |err| {
                std.debug.print("\x1b[31mError reading validation response: {}\x1b[0m\n", .{err});
                std.fs.cwd().deleteFile(json_file) catch {};
                return 1;
            };
            defer allocator.free(json_content);
            std.fs.cwd().deleteFile(json_file) catch {};

            // Simple JSON parsing (looking for specific fields)
            const status_prefix = "\"status\":\"";
            var status: ?[]const u8 = null;
            if (mem.indexOf(u8, json_content, status_prefix)) |start_idx| {
                const value_start = start_idx + status_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    status = json_content[value_start..end_idx];
                }
            }

            if (status == null) {
                std.debug.print("\x1b[31mError: Invalid response from server\x1b[0m\n", .{});
                return 1;
            }

            // Extract other fields
            var public_key: ?[]const u8 = null;
            var tier: ?[]const u8 = null;
            var expires_at: ?[]const u8 = null;

            const pk_prefix = "\"public_key\":\"";
            if (mem.indexOf(u8, json_content, pk_prefix)) |start_idx| {
                const value_start = start_idx + pk_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    public_key = json_content[value_start..end_idx];
                }
            }

            const tier_prefix = "\"tier\":\"";
            if (mem.indexOf(u8, json_content, tier_prefix)) |start_idx| {
                const value_start = start_idx + tier_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    tier = json_content[value_start..end_idx];
                }
            }

            const expires_prefix = "\"expires_at\":\"";
            if (mem.indexOf(u8, json_content, expires_prefix)) |start_idx| {
                const value_start = start_idx + expires_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    expires_at = json_content[value_start..end_idx];
                }
            }

            // Display results based on status
            if (status) |s| {
                if (mem.eql(u8, s, "valid")) {
                    std.debug.print("\x1b[32mValid\x1b[0m\n", .{});
                    if (public_key) |pk| std.debug.print("Public Key: {s}\n", .{pk});
                    if (tier) |t| std.debug.print("Tier: {s}\n", .{t});
                    if (expires_at) |exp| std.debug.print("Expires: {s}\n", .{exp});
                } else if (mem.eql(u8, s, "expired")) {
                    std.debug.print("\x1b[31mExpired\x1b[0m\n", .{});
                    if (public_key) |pk| std.debug.print("Public Key: {s}\n", .{pk});
                    if (tier) |t| std.debug.print("Tier: {s}\n", .{t});
                    if (expires_at) |exp| std.debug.print("Expired: {s}\n", .{exp});
                    std.debug.print("\x1b[33mTo renew: Visit {s}/keys/extend\x1b[0m\n", .{PORTAL_BASE});
                } else if (mem.eql(u8, s, "invalid")) {
                    std.debug.print("\x1b[31mInvalid\x1b[0m\n", .{});
                } else {
                    std.debug.print("Status: {s}\n", .{s});
                }
            }
        }
        return 0;
    }

    // Execute mode - find source file
    var source_file: ?[]const u8 = null;
    for (args[1..]) |arg| {
        if (!mem.startsWith(u8, arg, "-")) {
            source_file = arg;
            break;
        }
    }

    if (source_file == null) {
        std.debug.print("\x1b[31mError: No source file specified\x1b[0m\n", .{});
        return 1;
    }

    const filename = source_file.?;

    // Detect language
    const ext = fs.path.extension(filename);
    const lang = blk: {
        if (mem.eql(u8, ext, ".py")) break :blk "python";
        if (mem.eql(u8, ext, ".js")) break :blk "javascript";
        if (mem.eql(u8, ext, ".go")) break :blk "go";
        if (mem.eql(u8, ext, ".rs")) break :blk "rust";
        if (mem.eql(u8, ext, ".c")) break :blk "c";
        if (mem.eql(u8, ext, ".cpp")) break :blk "cpp";
        if (mem.eql(u8, ext, ".d")) break :blk "d";
        if (mem.eql(u8, ext, ".zig")) break :blk "zig";
        if (mem.eql(u8, ext, ".nim")) break :blk "nim";
        if (mem.eql(u8, ext, ".v")) break :blk "v";
        std.debug.print("\x1b[31mError: Cannot detect language\x1b[0m\n", .{});
        return 1;
    };

    // Read source file
    const code = fs.cwd().readFileAlloc(allocator, filename, 10 * 1024 * 1024) catch |err| {
        std.debug.print("\x1b[31mError reading file: {}\x1b[0m\n", .{err});
        return 1;
    };
    defer allocator.free(code);

    // Build JSON (simplified - doesn't handle all escape sequences)
    const json_file = "/tmp/unsandbox_request.json";
    const file = try std.fs.cwd().createFile(json_file, .{});
    defer file.close();
    const writer = file.writer();
    try writer.print("{{\"language\":\"{s}\",\"code\":\"", .{lang});

    // Escape JSON
    for (code) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => try writer.writeByte(c),
        }
    }
    try writer.writeAll("\"}");

    // Execute with curl
    const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer {s}' -d @{s}", .{ API_BASE, api_key, json_file });
    defer allocator.free(cmd);

    const result = std.c.system(cmd.ptr);
    std.debug.print("\n", .{});

    // Cleanup
    std.fs.cwd().deleteFile(json_file) catch {};

    return if (result == 0) 0 else 1;
}
