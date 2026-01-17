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


// UN CLI and Library - Zig Implementation (using curl subprocess for simplicity)
// Compile: zig build-exe un.zig -O ReleaseFast
//
// Library Usage (Zig):
//   pub fn execute(allocator: std.mem.Allocator, language: []const u8, code: []const u8,
//                 public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn execute_async(allocator: std.mem.Allocator, language: []const u8, code: []const u8,
//                       public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn get_job(allocator: std.mem.Allocator, job_id: []const u8,
//                 public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn wait_for_job(allocator: std.mem.Allocator, job_id: []const u8,
//                      public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn cancel_job(allocator: std.mem.Allocator, job_id: []const u8,
//                    public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn list_jobs(allocator: std.mem.Allocator,
//                   public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn get_languages(allocator: std.mem.Allocator,
//                       public_key: []const u8, secret_key: []const u8) ![]const u8
//   pub fn detect_language(filename: []const u8) ?[]const u8
//
// CLI Usage:
//   un.zig script.py
//   un.zig -e KEY=VALUE script.py
//   un.zig session --list
//   un.zig service --name web --ports 8080
//
// Note: This implementation uses system() to call curl for simplicity
// A production version would use Zig's HTTP client library

const std = @import("std");
const fs = std.fs;
const process = std.process;
const mem = std.mem;
const time = std.time;

const API_BASE = "https://api.unsandbox.com";
const PORTAL_BASE = "https://unsandbox.com";
const MAX_ENV_CONTENT_SIZE: usize = 65536;
const LANGUAGES_CACHE_TTL: i64 = 3600; // 1 hour in seconds
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

fn computeHmacCmd(allocator: std.mem.Allocator, secret_key: []const u8, message: []const u8) ![]const u8 {
    return try std.fmt.allocPrint(allocator, "echo -n '{s}' | openssl dgst -sha256 -hmac '{s}' -hex 2>/dev/null | sed 's/.*= //'", .{ message, secret_key });
}

fn getTimestamp(allocator: std.mem.Allocator) ![]const u8 {
    const timestamp = std.time.timestamp();
    return try std.fmt.allocPrint(allocator, "{d}", .{timestamp});
}

fn buildAuthCmd(allocator: std.mem.Allocator, method: []const u8, path: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    if (secret_key.len == 0) {
        // Legacy mode: use public_key as bearer token
        return try std.fmt.allocPrint(allocator, "-H 'Authorization: Bearer {s}'", .{public_key});
    }

    // HMAC mode
    const timestamp_str = try getTimestamp(allocator);
    defer allocator.free(timestamp_str);

    const message = try std.fmt.allocPrint(allocator, "{s}:{s}:{s}:{s}", .{ timestamp_str, method, path, body });
    defer allocator.free(message);

    const hmac_cmd = try computeHmacCmd(allocator, secret_key, message);
    defer allocator.free(hmac_cmd);

    // Execute HMAC command to get signature
    var signature_buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&signature_buf);
    const signature_len = blk: {
        const result = try std.process.Child.run(.{
            .allocator = allocator,
            .argv = &[_][]const u8{ "sh", "-c", hmac_cmd },
        });
        defer allocator.free(result.stdout);
        defer allocator.free(result.stderr);

        const trimmed = mem.trim(u8, result.stdout, &std.ascii.whitespace);
        @memcpy(signature_buf[0..trimmed.len], trimmed);
        break :blk trimmed.len;
    };

    const signature = signature_buf[0..signature_len];

    return try std.fmt.allocPrint(allocator, "-H 'Authorization: Bearer {s}' -H 'X-Timestamp: {s}' -H 'X-Signature: {s}'", .{ public_key, timestamp_str, signature });
}

fn base64EncodeFile(allocator: std.mem.Allocator, filename: []const u8) ![]u8 {
    const cmd = try std.fmt.allocPrint(allocator, "base64 -w0 '{s}'", .{filename});
    defer allocator.free(cmd);

    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "sh", "-c", cmd },
    }) catch return try allocator.dupe(u8, "");
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const trimmed = mem.trim(u8, result.stdout, &std.ascii.whitespace);
    return try allocator.dupe(u8, trimmed);
}

fn readEnvFile(allocator: std.mem.Allocator, filename: []const u8) ![]u8 {
    const content = fs.cwd().readFileAlloc(allocator, filename, MAX_ENV_CONTENT_SIZE) catch |err| {
        std.debug.print("{s}Error: Cannot read env file: {s} ({s}){s}\n", .{ RED, filename, @errorName(err), RESET });
        return try allocator.dupe(u8, "");
    };
    return content;
}

fn buildEnvContent(allocator: std.mem.Allocator, envs: std.ArrayList([]const u8), env_file: ?[]const u8) ![]u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();

    // Add environment variables from -e flags
    for (envs.items) |env| {
        try list.appendSlice(env);
        try list.append('\n');
    }

    // Add content from env file
    if (env_file) |ef| {
        const file_content = try readEnvFile(allocator, ef);
        defer allocator.free(file_content);

        // Process line by line, skip comments and empty lines
        var lines = mem.splitScalar(u8, file_content, '\n');
        while (lines.next()) |line| {
            const trimmed = mem.trim(u8, line, &std.ascii.whitespace);
            if (trimmed.len == 0) continue;
            if (trimmed[0] == '#') continue;
            try list.appendSlice(trimmed);
            try list.append('\n');
        }
    }

    return list.toOwnedSlice();
}

fn extractJsonField(json: []const u8, field: []const u8) ?[]const u8 {
    // Build search pattern: "field":"
    var pattern_buf: [256]u8 = undefined;
    const pattern = std.fmt.bufPrint(&pattern_buf, "\"{s}\":\"", .{field}) catch return null;

    if (mem.indexOf(u8, json, pattern)) |start_idx| {
        const value_start = start_idx + pattern.len;
        if (mem.indexOfPos(u8, json, value_start, "\"")) |end_idx| {
            return json[value_start..end_idx];
        }
    }
    return null;
}

fn execCurlPut(allocator: std.mem.Allocator, endpoint: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8) !bool {
    const url = try std.fmt.allocPrint(allocator, "{s}{s}", .{ API_BASE, endpoint });
    defer allocator.free(url);

    const auth_headers = try buildAuthCmd(allocator, "PUT", endpoint, body, public_key, secret_key);
    defer allocator.free(auth_headers);

    // Write body to temp file to avoid shell escaping issues
    const body_file = "/tmp/unsandbox_env_body.txt";
    const file = try fs.cwd().createFile(body_file, .{});
    try file.writeAll(body);
    file.close();
    defer fs.cwd().deleteFile(body_file) catch {};

    const cmd = try std.fmt.allocPrint(allocator, "curl -s -X PUT '{s}' -H 'Content-Type: text/plain' {s} --data-binary @{s}", .{ url, auth_headers, body_file });
    defer allocator.free(cmd);

    const ret = std.c.system(cmd.ptr);
    return ret == 0;
}

fn cmdServiceEnv(allocator: std.mem.Allocator, action: []const u8, target: []const u8, envs: std.ArrayList([]const u8), env_file: ?[]const u8, public_key: []const u8, secret_key: []const u8) !void {
    if (mem.eql(u8, action, "status")) {
        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{target});
        defer allocator.free(path);
        const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
        defer allocator.free(auth_headers);
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}{s}' {s}", .{ API_BASE, path, auth_headers });
        defer allocator.free(cmd);
        _ = std.c.system(cmd.ptr);
        std.debug.print("\n", .{});
    } else if (mem.eql(u8, action, "set")) {
        if (envs.items.len == 0 and env_file == null) {
            std.debug.print("{s}Error: No environment variables specified. Use -e KEY=VALUE or --env-file FILE{s}\n", .{ RED, RESET });
            return;
        }
        const content = try buildEnvContent(allocator, envs, env_file);
        defer allocator.free(content);

        if (content.len > MAX_ENV_CONTENT_SIZE) {
            std.debug.print("{s}Error: Environment content exceeds 64KB limit{s}\n", .{ RED, RESET });
            return;
        }

        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{target});
        defer allocator.free(path);

        _ = try execCurlPut(allocator, path, content, public_key, secret_key);
        std.debug.print("\n{s}Vault updated for service {s}{s}\n", .{ GREEN, target, RESET });
    } else if (mem.eql(u8, action, "export")) {
        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env/export", .{target});
        defer allocator.free(path);
        const auth_headers = try buildAuthCmd(allocator, "POST", path, "", public_key, secret_key);
        defer allocator.free(auth_headers);
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}{s}' {s}", .{ API_BASE, path, auth_headers });
        defer allocator.free(cmd);
        _ = std.c.system(cmd.ptr);
        std.debug.print("\n", .{});
    } else if (mem.eql(u8, action, "delete")) {
        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{target});
        defer allocator.free(path);
        const auth_headers = try buildAuthCmd(allocator, "DELETE", path, "", public_key, secret_key);
        defer allocator.free(auth_headers);
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}{s}' {s}", .{ API_BASE, path, auth_headers });
        defer allocator.free(cmd);
        _ = std.c.system(cmd.ptr);
        std.debug.print("\n{s}Vault deleted for service {s}{s}\n", .{ GREEN, target, RESET });
    } else {
        std.debug.print("{s}Error: Unknown env action: {s}{s}\n", .{ RED, action, RESET });
        std.debug.print("Usage: un service env <status|set|export|delete> <service_id>\n", .{});
    }
}

fn serviceEnvSet(allocator: std.mem.Allocator, service_id: []const u8, content: []const u8, public_key: []const u8, secret_key: []const u8) !bool {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{service_id});
    defer allocator.free(path);
    return try execCurlPut(allocator, path, content, public_key, secret_key);
}

fn buildInputFilesJson(allocator: std.mem.Allocator, files: std.ArrayList([]const u8)) ![]u8 {
    if (files.items.len == 0) {
        return try allocator.dupe(u8, "");
    }

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    try list.appendSlice(",\"input_files\":[");

    for (files.items, 0..) |file, i| {
        if (i > 0) try list.append(',');

        // Get basename
        var basename: []const u8 = file;
        if (mem.lastIndexOfScalar(u8, file, '/')) |idx| {
            basename = file[idx + 1 ..];
        }

        // Base64 encode file content
        const content = try base64EncodeFile(allocator, file);
        defer allocator.free(content);

        const entry = try std.fmt.allocPrint(allocator, "{{\"filename\":\"{s}\",\"content\":\"{s}\"}}", .{ basename, content });
        defer allocator.free(entry);

        try list.appendSlice(entry);
    }

    try list.append(']');

    return list.toOwnedSlice();
}

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
        std.debug.print("       {s} service env <action> <service_id> [options]\n", .{args[0]});
        std.debug.print("       {s} image [options]\n", .{args[0]});
        std.debug.print("       {s} key [--extend]\n", .{args[0]});
        std.debug.print("       {s} languages [--json]\n", .{args[0]});
        std.debug.print("\nVault commands:\n", .{});
        std.debug.print("  service env status <id>   Check vault status\n", .{});
        std.debug.print("  service env set <id>      Set vault (-e KEY=VAL or --env-file FILE)\n", .{});
        std.debug.print("  service env export <id>   Export vault contents\n", .{});
        std.debug.print("  service env delete <id>   Delete vault\n", .{});
        std.debug.print("\nImage commands:\n", .{});
        std.debug.print("  image --list              List all images\n", .{});
        std.debug.print("  image --info ID           Get image details\n", .{});
        std.debug.print("  image --delete ID         Delete an image\n", .{});
        std.debug.print("  image --lock ID           Lock image to prevent deletion\n", .{});
        std.debug.print("  image --unlock ID         Unlock image\n", .{});
        std.debug.print("  image --publish ID --source-type TYPE  Publish from service/snapshot\n", .{});
        std.debug.print("  image --visibility ID MODE  Set visibility (private/unlisted/public)\n", .{});
        std.debug.print("  image --spawn ID --name NAME  Spawn service from image\n", .{});
        std.debug.print("  image --clone ID --name NAME  Clone an image\n", .{});
        return 1;
    }

    var public_key = std.process.getEnvVarOwned(allocator, "UNSANDBOX_PUBLIC_KEY") catch blk: {
        // Fall back to UNSANDBOX_API_KEY for backwards compatibility
        break :blk std.process.getEnvVarOwned(allocator, "UNSANDBOX_API_KEY") catch try allocator.dupe(u8, "");
    };
    defer allocator.free(public_key);

    const secret_key = std.process.getEnvVarOwned(allocator, "UNSANDBOX_SECRET_KEY") catch blk: {
        break :blk try allocator.dupe(u8, "");
    };
    defer allocator.free(secret_key);

    // Handle session command
    if (mem.eql(u8, args[1], "session")) {
        var list = false;
        var kill: ?[]const u8 = null;
        var shell: ?[]const u8 = null;
        var input_files = std.ArrayList([]const u8).init(allocator);
        defer input_files.deinit();
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
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            } else if (mem.eql(u8, args[i], "-f") and i + 1 < args.len) {
                i += 1;
                const file = args[i];
                // Check if file exists
                fs.cwd().access(file, .{}) catch {
                    std.debug.print("Error: File not found: {s}\n", .{file});
                    return 1;
                };
                try input_files.append(file);
            }
        }

        if (list) {
            const auth_headers = try buildAuthCmd(allocator, "GET", "/sessions", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/sessions' {s}", .{ API_BASE, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (kill) |k| {
            const path = try std.fmt.allocPrint(allocator, "/sessions/{s}", .{k});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "DELETE", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}/sessions/{s}' {s}", .{ API_BASE, k, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\x1b[32mSession terminated: {s}\x1b[0m\n", .{k});
        } else {
            const sh = shell orelse "bash";
            const input_files_json = try buildInputFilesJson(allocator, input_files);
            defer allocator.free(input_files_json);
            const json = try std.fmt.allocPrint(allocator, "{{\"shell\":\"{s}\"{s}}}", .{ sh, input_files_json });
            defer allocator.free(json);
            const auth_headers = try buildAuthCmd(allocator, "POST", "/sessions", json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/sessions' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, auth_headers, json });
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
        var bootstrap: ?[]const u8 = null;
        var bootstrap_file: ?[]const u8 = null;
        var info: ?[]const u8 = null;
        var execute: ?[]const u8 = null;
        var command: ?[]const u8 = null;
        var dump_bootstrap: ?[]const u8 = null;
        var dump_file: ?[]const u8 = null;
        var resize: ?[]const u8 = null;
        var vcpu: i32 = 0;
        var input_files = std.ArrayList([]const u8).init(allocator);
        defer input_files.deinit();
        var svc_envs = std.ArrayList([]const u8).init(allocator);
        defer svc_envs.deinit();
        var svc_env_file: ?[]const u8 = null;
        var env_action: ?[]const u8 = null;
        var env_target: ?[]const u8 = null;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--list")) {
                list = true;
            } else if (mem.eql(u8, args[i], "env") and i + 2 < args.len) {
                // service env <action> <service_id>
                i += 1;
                env_action = args[i];
                i += 1;
                env_target = args[i];
            } else if (mem.eql(u8, args[i], "--name") and i + 1 < args.len) {
                i += 1;
                name = args[i];
            } else if (mem.eql(u8, args[i], "--ports") and i + 1 < args.len) {
                i += 1;
                ports = args[i];
            } else if (mem.eql(u8, args[i], "--type") and i + 1 < args.len) {
                i += 1;
                service_type = args[i];
            } else if (mem.eql(u8, args[i], "--bootstrap") and i + 1 < args.len) {
                i += 1;
                bootstrap = args[i];
            } else if (mem.eql(u8, args[i], "--bootstrap-file") and i + 1 < args.len) {
                i += 1;
                bootstrap_file = args[i];
            } else if (mem.eql(u8, args[i], "--info") and i + 1 < args.len) {
                i += 1;
                info = args[i];
            } else if (mem.eql(u8, args[i], "--execute") and i + 1 < args.len) {
                i += 1;
                execute = args[i];
            } else if (mem.eql(u8, args[i], "--command") and i + 1 < args.len) {
                i += 1;
                command = args[i];
            } else if (mem.eql(u8, args[i], "--dump-bootstrap") and i + 1 < args.len) {
                i += 1;
                dump_bootstrap = args[i];
            } else if (mem.eql(u8, args[i], "--dump-file") and i + 1 < args.len) {
                i += 1;
                dump_file = args[i];
            } else if (mem.eql(u8, args[i], "--resize") and i + 1 < args.len) {
                i += 1;
                resize = args[i];
            } else if (mem.eql(u8, args[i], "-v") and i + 1 < args.len) {
                i += 1;
                vcpu = std.fmt.parseInt(i32, args[i], 10) catch 0;
            } else if (mem.eql(u8, args[i], "-e") and i + 1 < args.len) {
                i += 1;
                try svc_envs.append(args[i]);
            } else if (mem.eql(u8, args[i], "--env-file") and i + 1 < args.len) {
                i += 1;
                svc_env_file = args[i];
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            } else if (mem.eql(u8, args[i], "-f") and i + 1 < args.len) {
                i += 1;
                const file = args[i];
                // Check if file exists
                fs.cwd().access(file, .{}) catch {
                    std.debug.print("Error: File not found: {s}\n", .{file});
                    return 1;
                };
                try input_files.append(file);
            }
        }

        // Handle env subcommand
        if (env_action) |action| {
            if (env_target) |target| {
                try cmdServiceEnv(allocator, action, target, svc_envs, svc_env_file, public_key, secret_key);
                return 0;
            }
        }

        if (list) {
            const auth_headers = try buildAuthCmd(allocator, "GET", "/services", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services' {s}", .{ API_BASE, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (info) |inf| {
            const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{inf});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services/{s}' {s}", .{ API_BASE, inf, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (execute) |exec_id| {
            const cmd_text = command orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"command\":\"{s}\"}}", .{cmd_text});
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/services/{s}/execute", .{exec_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services/{s}/execute' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, exec_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (dump_bootstrap) |bootstrap_id| {
            std.debug.print("Fetching bootstrap script from {s}...\n", .{bootstrap_id});
            const tmp_file = "/tmp/unsandbox_bootstrap_dump.txt";
            const json = "{{\"command\":\"cat /tmp/bootstrap.sh\"}}";
            const path = try std.fmt.allocPrint(allocator, "/services/{s}/execute", .{bootstrap_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services/{s}/execute' -H 'Content-Type: application/json' {s} -d '{s}' -o {s}", .{ API_BASE, bootstrap_id, auth_headers, json, tmp_file });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);

            // Read the JSON response
            const json_content = fs.cwd().readFileAlloc(allocator, tmp_file, 1024 * 1024) catch |err| {
                std.debug.print("\x1b[31mError reading response: {}\x1b[0m\n", .{err});
                std.fs.cwd().deleteFile(tmp_file) catch {};
                return 1;
            };
            defer allocator.free(json_content);
            std.fs.cwd().deleteFile(tmp_file) catch {};

            // Extract stdout from JSON (simple string search)
            const stdout_prefix = "\"stdout\":\"";
            if (mem.indexOf(u8, json_content, stdout_prefix)) |start_idx| {
                const value_start = start_idx + stdout_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    const bootstrap_content = json_content[value_start..end_idx];

                    if (dump_file) |file_path| {
                        const file = try std.fs.cwd().createFile(file_path, .{});
                        defer file.close();
                        try file.writeAll(bootstrap_content);
                        // Set permissions (Unix only)
                        if (@import("builtin").os.tag != .windows) {
                            const chmod_cmd = try std.fmt.allocPrint(allocator, "chmod 755 {s}", .{file_path});
                            defer allocator.free(chmod_cmd);
                            _ = std.c.system(chmod_cmd.ptr);
                        }
                        std.debug.print("Bootstrap saved to {s}\n", .{file_path});
                    } else {
                        std.debug.print("{s}", .{bootstrap_content});
                    }
                } else {
                    std.debug.print("\x1b[31mError: Failed to parse bootstrap response\x1b[0m\n", .{});
                    return 1;
                }
            } else {
                std.debug.print("\x1b[31mError: Failed to fetch bootstrap (service not running or no bootstrap file)\x1b[0m\n", .{});
                return 1;
            }
        } else if (resize) |resize_id| {
            // Validate vcpu
            if (vcpu < 1 or vcpu > 8) {
                std.debug.print("{s}Error: --resize requires -v N (1-8){s}\n", .{ RED, RESET });
                return 1;
            }

            // Build JSON body
            var vcpu_buf: [16]u8 = undefined;
            const vcpu_str = std.fmt.bufPrint(&vcpu_buf, "{d}", .{vcpu}) catch "0";
            const json = try std.fmt.allocPrint(allocator, "{{\"vcpu\":{s}}}", .{vcpu_str});
            defer allocator.free(json);

            // Build path
            const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{resize_id});
            defer allocator.free(path);

            // Build auth headers
            const auth_headers = try buildAuthCmd(allocator, "PATCH", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);

            // Execute PATCH request
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X PATCH '{s}/services/{s}' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, resize_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);

            // Calculate RAM
            const ram = vcpu * 2;
            std.debug.print("\n{s}Service resized to {d} vCPU, {d} GB RAM{s}\n", .{ GREEN, vcpu, ram, RESET });
        } else if (name) |n| {
            var json_buf: [65536]u8 = undefined;
            var json_stream = std.io.fixedBufferStream(&json_buf);
            const writer = json_stream.writer();
            try writer.print("{{\"name\":\"{s}\"", .{n});
            if (ports) |p| {
                try writer.print(",\"ports\":[{s}]", .{p});
            }
            if (service_type) |t| {
                try writer.print(",\"service_type\":\"{s}\"", .{t});
            }
            if (bootstrap) |b| {
                try writer.writeAll(",\"bootstrap\":\"");
                // Escape JSON
                for (b) |c| {
                    switch (c) {
                        '"' => try writer.writeAll("\\\""),
                        '\\' => try writer.writeAll("\\\\"),
                        '\n' => try writer.writeAll("\\n"),
                        '\r' => try writer.writeAll("\\r"),
                        '\t' => try writer.writeAll("\\t"),
                        else => try writer.writeByte(c),
                    }
                }
                try writer.writeAll("\"");
            }
            if (bootstrap_file) |bf| {
                const boot_content = fs.cwd().readFileAlloc(allocator, bf, 10 * 1024 * 1024) catch |err| {
                    std.debug.print("\x1b[31mError: Bootstrap file not found: {s} ({})\x1b[0m\n", .{ bf, err });
                    return 1;
                };
                defer allocator.free(boot_content);
                try writer.writeAll(",\"bootstrap_content\":\"");
                // Escape JSON
                for (boot_content) |c| {
                    switch (c) {
                        '"' => try writer.writeAll("\\\""),
                        '\\' => try writer.writeAll("\\\\"),
                        '\n' => try writer.writeAll("\\n"),
                        '\r' => try writer.writeAll("\\r"),
                        '\t' => try writer.writeAll("\\t"),
                        else => try writer.writeByte(c),
                    }
                }
                try writer.writeAll("\"");
            }
            // Add input_files JSON
            const input_files_json = try buildInputFilesJson(allocator, input_files);
            defer allocator.free(input_files_json);
            try writer.writeAll(input_files_json);
            try writer.writeAll("}");
            const json_str = json_stream.getWritten();

            const auth_headers = try buildAuthCmd(allocator, "POST", "/services", json_str, public_key, secret_key);
            defer allocator.free(auth_headers);

            // Check if we need auto-vault
            const has_env = svc_envs.items.len > 0 or svc_env_file != null;

            if (has_env) {
                // Capture response to temp file to extract service_id
                const response_file = "/tmp/unsandbox_service_create.json";
                const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services' -H 'Content-Type: application/json' {s} -d '{s}' -o {s}", .{ API_BASE, auth_headers, json_str, response_file });
                defer allocator.free(cmd);
                std.debug.print("{s}Creating service...{s}\n", .{ YELLOW, RESET });
                _ = std.c.system(cmd.ptr);

                // Read response
                const response_content = fs.cwd().readFileAlloc(allocator, response_file, 1024 * 1024) catch {
                    std.debug.print("{s}Error: Failed to read service creation response{s}\n", .{ RED, RESET });
                    return 1;
                };
                defer allocator.free(response_content);
                fs.cwd().deleteFile(response_file) catch {};

                // Print the response
                std.debug.print("{s}\n", .{response_content});

                // Extract service_id and auto-set vault
                if (extractJsonField(response_content, "service_id")) |service_id| {
                    const env_content = try buildEnvContent(allocator, svc_envs, svc_env_file);
                    defer allocator.free(env_content);

                    if (env_content.len > 0) {
                        if (try serviceEnvSet(allocator, service_id, env_content, public_key, secret_key)) {
                            std.debug.print("\n{s}Vault configured for service {s}{s}\n", .{ GREEN, service_id, RESET });
                        }
                    }
                }
            } else {
                const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, auth_headers, json_str });
                defer allocator.free(cmd);
                std.debug.print("{s}Creating service...{s}\n", .{ YELLOW, RESET });
                _ = std.c.system(cmd.ptr);
                std.debug.print("\n", .{});
            }
        }
        return 0;
    }

    // Handle languages command
    if (mem.eql(u8, args[1], "languages")) {
        var json_output = false;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--json")) {
                json_output = true;
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            }
        }

        // Get cache path (~/.unsandbox/languages.json)
        const home_env = std.process.getEnvVarOwned(allocator, "HOME") catch try allocator.dupe(u8, "/tmp");
        defer allocator.free(home_env);
        const cache_dir = try std.fmt.allocPrint(allocator, "{s}/.unsandbox", .{home_env});
        defer allocator.free(cache_dir);
        const cache_file = try std.fmt.allocPrint(allocator, "{s}/languages.json", .{cache_dir});
        defer allocator.free(cache_file);

        // Ensure cache directory exists
        fs.cwd().makeDir(cache_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {},
            else => {},
        };

        // Check cache first
        var use_cache = false;
        const cache_content = fs.cwd().readFileAlloc(allocator, cache_file, 1024 * 1024) catch null;
        defer if (cache_content) |cc| allocator.free(cc);

        if (cache_content) |cc| {
            // Check timestamp
            const ts_prefix = "\"timestamp\":";
            if (mem.indexOf(u8, cc, ts_prefix)) |ts_start_idx| {
                const ts_value_start = ts_start_idx + ts_prefix.len;
                var ts_value_end = ts_value_start;
                while (ts_value_end < cc.len and (cc[ts_value_end] >= '0' and cc[ts_value_end] <= '9')) {
                    ts_value_end += 1;
                }
                if (ts_value_end > ts_value_start) {
                    const ts_str = cc[ts_value_start..ts_value_end];
                    const cache_timestamp = std.fmt.parseInt(i64, ts_str, 10) catch 0;
                    const current_timestamp = std.time.timestamp();
                    if (current_timestamp - cache_timestamp < LANGUAGES_CACHE_TTL) {
                        use_cache = true;
                    }
                }
            }
        }

        var json_content: []const u8 = undefined;
        var json_content_owned = false;

        if (use_cache) {
            json_content = cache_content.?;
        } else {
            // Fetch languages from API
            const json_file = "/tmp/unsandbox_languages_tmp.json";
            const auth_headers = try buildAuthCmd(allocator, "GET", "/languages", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/languages' {s} -o {s}", .{ API_BASE, auth_headers, json_file });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);

            // Read the JSON response
            const api_content = fs.cwd().readFileAlloc(allocator, json_file, 1024 * 1024) catch |err| {
                std.debug.print("{s}Error reading languages response: {}{s}\n", .{ RED, err, RESET });
                std.fs.cwd().deleteFile(json_file) catch {};
                return 1;
            };
            json_content = api_content;
            json_content_owned = true;
            std.fs.cwd().deleteFile(json_file) catch {};

            // Save to cache with timestamp
            const arr_prefix = "\"languages\":[";
            if (mem.indexOf(u8, api_content, arr_prefix)) |start_idx| {
                const arr_start = start_idx + arr_prefix.len - 1; // Include the '['
                if (mem.indexOfPos(u8, api_content, arr_start, "]")) |end_idx| {
                    const languages_array = api_content[arr_start .. end_idx + 1];
                    const current_ts = std.time.timestamp();
                    const cache_data = try std.fmt.allocPrint(allocator, "{{\"languages\":{s},\"timestamp\":{d}}}", .{ languages_array, current_ts });
                    defer allocator.free(cache_data);

                    // Write cache file
                    const cache_file_handle = fs.cwd().createFile(cache_file, .{}) catch null;
                    if (cache_file_handle) |fh| {
                        fh.writeAll(cache_data) catch {};
                        fh.close();
                    }
                }
            }
        }

        defer if (json_content_owned) allocator.free(json_content);

        if (json_output) {
            // Find and print just the languages array
            const arr_prefix = "\"languages\":[";
            if (mem.indexOf(u8, json_content, arr_prefix)) |start_idx| {
                const arr_start = start_idx + arr_prefix.len - 1; // Include the '['
                if (mem.indexOfPos(u8, json_content, arr_start, "]")) |end_idx| {
                    std.debug.print("{s}\n", .{json_content[arr_start .. end_idx + 1]});
                } else {
                    std.debug.print("{s}\n", .{json_content});
                }
            } else {
                std.debug.print("{s}\n", .{json_content});
            }
        } else {
            // Parse and print one language per line
            const arr_prefix = "\"languages\":[";
            if (mem.indexOf(u8, json_content, arr_prefix)) |start_idx| {
                const arr_start = start_idx + arr_prefix.len;
                if (mem.indexOfPos(u8, json_content, arr_start, "]")) |end_idx| {
                    const arr_content = json_content[arr_start..end_idx];
                    // Split by comma and extract language names
                    var it = mem.splitSequence(u8, arr_content, ",");
                    while (it.next()) |item| {
                        // Trim whitespace and quotes
                        const trimmed = mem.trim(u8, item, &std.ascii.whitespace);
                        if (trimmed.len > 2 and trimmed[0] == '"') {
                            // Remove quotes
                            const lang = trimmed[1 .. trimmed.len - 1];
                            std.debug.print("{s}\n", .{lang});
                        }
                    }
                } else {
                    std.debug.print("{s}\n", .{json_content});
                }
            } else {
                std.debug.print("{s}\n", .{json_content});
            }
        }
        return 0;
    }

    // Handle image command
    if (mem.eql(u8, args[1], "image")) {
        var list = false;
        var info: ?[]const u8 = null;
        var delete: ?[]const u8 = null;
        var lock: ?[]const u8 = null;
        var unlock: ?[]const u8 = null;
        var publish: ?[]const u8 = null;
        var source_type: ?[]const u8 = null;
        var visibility_id: ?[]const u8 = null;
        var visibility_mode: ?[]const u8 = null;
        var spawn: ?[]const u8 = null;
        var clone: ?[]const u8 = null;
        var name: ?[]const u8 = null;
        var ports: ?[]const u8 = null;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--list") or mem.eql(u8, args[i], "-l")) {
                list = true;
            } else if (mem.eql(u8, args[i], "--info") and i + 1 < args.len) {
                i += 1;
                info = args[i];
            } else if (mem.eql(u8, args[i], "--delete") and i + 1 < args.len) {
                i += 1;
                delete = args[i];
            } else if (mem.eql(u8, args[i], "--lock") and i + 1 < args.len) {
                i += 1;
                lock = args[i];
            } else if (mem.eql(u8, args[i], "--unlock") and i + 1 < args.len) {
                i += 1;
                unlock = args[i];
            } else if (mem.eql(u8, args[i], "--publish") and i + 1 < args.len) {
                i += 1;
                publish = args[i];
            } else if (mem.eql(u8, args[i], "--source-type") and i + 1 < args.len) {
                i += 1;
                source_type = args[i];
            } else if (mem.eql(u8, args[i], "--visibility") and i + 2 < args.len) {
                i += 1;
                visibility_id = args[i];
                i += 1;
                visibility_mode = args[i];
            } else if (mem.eql(u8, args[i], "--spawn") and i + 1 < args.len) {
                i += 1;
                spawn = args[i];
            } else if (mem.eql(u8, args[i], "--clone") and i + 1 < args.len) {
                i += 1;
                clone = args[i];
            } else if (mem.eql(u8, args[i], "--name") and i + 1 < args.len) {
                i += 1;
                name = args[i];
            } else if (mem.eql(u8, args[i], "--ports") and i + 1 < args.len) {
                i += 1;
                ports = args[i];
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            }
        }

        if (list) {
            const auth_headers = try buildAuthCmd(allocator, "GET", "/images", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/images' {s}", .{ API_BASE, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (info) |inf| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{inf});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/images/{s}' {s}", .{ API_BASE, inf, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (delete) |del_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{del_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "DELETE", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}/images/{s}' {s}", .{ API_BASE, del_id, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n{s}Image deleted: {s}{s}\n", .{ GREEN, del_id, RESET });
        } else if (lock) |lock_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/lock", .{lock_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/lock' {s}", .{ API_BASE, lock_id, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n{s}Image locked: {s}{s}\n", .{ GREEN, lock_id, RESET });
        } else if (unlock) |unlock_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/unlock", .{unlock_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/unlock' {s}", .{ API_BASE, unlock_id, auth_headers });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n{s}Image unlocked: {s}{s}\n", .{ GREEN, unlock_id, RESET });
        } else if (publish) |pub_id| {
            if (source_type == null) {
                std.debug.print("{s}Error: --publish requires --source-type (service or snapshot){s}\n", .{ RED, RESET });
                return 1;
            }
            const nm = name orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"source_type\":\"{s}\",\"source_id\":\"{s}\",\"name\":\"{s}\"}}", .{ source_type.?, pub_id, nm });
            defer allocator.free(json);
            const auth_headers = try buildAuthCmd(allocator, "POST", "/images/publish", json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/publish' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (visibility_id) |vis_id| {
            if (visibility_mode == null) {
                std.debug.print("{s}Error: --visibility requires a mode (private, unlisted, or public){s}\n", .{ RED, RESET });
                return 1;
            }
            const json = try std.fmt.allocPrint(allocator, "{{\"visibility\":\"{s}\"}}", .{visibility_mode.?});
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/visibility", .{vis_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/visibility' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, vis_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n{s}Image visibility set to {s}: {s}{s}\n", .{ GREEN, visibility_mode.?, vis_id, RESET });
        } else if (spawn) |spawn_id| {
            const nm = name orelse "";
            const pt = ports orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"name\":\"{s}\",\"ports\":[{s}]}}", .{ nm, pt });
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/spawn", .{spawn_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/spawn' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, spawn_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else if (clone) |clone_id| {
            const nm = name orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"name\":\"{s}\"}}", .{nm});
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/clone", .{clone_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/clone' -H 'Content-Type: application/json' {s} -d '{s}'", .{ API_BASE, clone_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = std.c.system(cmd.ptr);
            std.debug.print("\n", .{});
        } else {
            std.debug.print("{s}Error: No image action specified. Use --list, --info, --delete, --publish, etc.{s}\n", .{ RED, RESET });
            return 1;
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
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            }
        }

        if (extend) {
            // First validate to get the public_key
            const json_file = "/tmp/unsandbox_key_validate.json";
            const auth_headers = try buildAuthCmd(allocator, "POST", "/keys/validate", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd_validate = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/keys/validate' -H 'Content-Type: application/json' {s} -o {s}", .{ PORTAL_BASE, auth_headers, json_file });
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

            // Check for clock drift errors
            if (mem.indexOf(u8, json_content, "timestamp") != null and
                (mem.indexOf(u8, json_content, "401") != null or
                 mem.indexOf(u8, json_content, "expired") != null or
                 mem.indexOf(u8, json_content, "invalid") != null))
            {
                std.debug.print("\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\x1b[0m\n", .{});
                std.debug.print("\x1b[33mYour computer's clock may have drifted.\x1b[0m\n", .{});
                std.debug.print("\x1b[33mCheck your system time and sync with NTP if needed:\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  Linux:   sudo ntpdate -s time.nist.gov\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  macOS:   sudo sntp -sS time.apple.com\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  Windows: w32tm /resync\x1b[0m\n", .{});
                return 1;
            }

            // Simple JSON parsing to find public_key (looking for "public_key":"value")
            const pk_prefix = "\"public_key\":\"";
            var public_key_value: ?[]const u8 = null;
            if (mem.indexOf(u8, json_content, pk_prefix)) |start_idx| {
                const value_start = start_idx + pk_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    public_key_value = json_content[value_start..end_idx];
                }
            }

            if (public_key_value) |pk| {
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
            const auth_headers = try buildAuthCmd(allocator, "POST", "/keys/validate", "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/keys/validate' -H 'Content-Type: application/json' {s} -o {s}", .{ PORTAL_BASE, auth_headers, json_file });
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

            // Check for clock drift errors
            if (mem.indexOf(u8, json_content, "timestamp") != null and
                (mem.indexOf(u8, json_content, "401") != null or
                 mem.indexOf(u8, json_content, "expired") != null or
                 mem.indexOf(u8, json_content, "invalid") != null))
            {
                std.debug.print("\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\x1b[0m\n", .{});
                std.debug.print("\x1b[33mYour computer's clock may have drifted.\x1b[0m\n", .{});
                std.debug.print("\x1b[33mCheck your system time and sync with NTP if needed:\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  Linux:   sudo ntpdate -s time.nist.gov\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  macOS:   sudo sntp -sS time.apple.com\x1b[0m\n", .{});
                std.debug.print("\x1b[33m  Windows: w32tm /resync\x1b[0m\n", .{});
                return 1;
            }

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
            var pub_key: ?[]const u8 = null;
            var tier: ?[]const u8 = null;
            var expires_at: ?[]const u8 = null;

            const pk_prefix = "\"public_key\":\"";
            if (mem.indexOf(u8, json_content, pk_prefix)) |start_idx| {
                const value_start = start_idx + pk_prefix.len;
                if (mem.indexOfPos(u8, json_content, value_start, "\"")) |end_idx| {
                    pub_key = json_content[value_start..end_idx];
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
                    if (pub_key) |pk| std.debug.print("Public Key: {s}\n", .{pk});
                    if (tier) |t| std.debug.print("Tier: {s}\n", .{t});
                    if (expires_at) |exp| std.debug.print("Expires: {s}\n", .{exp});
                } else if (mem.eql(u8, s, "expired")) {
                    std.debug.print("\x1b[31mExpired\x1b[0m\n", .{});
                    if (pub_key) |pk| std.debug.print("Public Key: {s}\n", .{pk});
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
        if (mem.startsWith(u8, arg, "-")) {
            const stderr = std.io.getStdErr().writer();
            stderr.print("{s}Unknown option: {s}{s}\n", .{ RED, arg, RESET }) catch {};
            std.os.exit(1);
        } else {
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

    // Read back the JSON to compute HMAC
    const json_content = try fs.cwd().readFileAlloc(allocator, json_file, 10 * 1024 * 1024);
    defer allocator.free(json_content);

    // Execute with curl
    const auth_headers = try buildAuthCmd(allocator, "POST", "/execute", json_content, public_key, secret_key);
    defer allocator.free(auth_headers);
    const response_file = "/tmp/unsandbox_response.json";
    const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/execute' -H 'Content-Type: application/json' {s} -d @{s} -o {s}", .{ API_BASE, auth_headers, json_file, response_file });
    defer allocator.free(cmd);

    _ = std.c.system(cmd.ptr);

    // Read response to check for clock drift errors
    const response_content = fs.cwd().readFileAlloc(allocator, response_file, 10 * 1024 * 1024) catch |err| {
        std.debug.print("\x1b[31mError reading response: {}\x1b[0m\n", .{err});
        std.fs.cwd().deleteFile(json_file) catch {};
        std.fs.cwd().deleteFile(response_file) catch {};
        return 1;
    };
    defer allocator.free(response_content);

    // Check for clock drift errors
    if (mem.indexOf(u8, response_content, "timestamp") != null and
        (mem.indexOf(u8, response_content, "401") != null or
         mem.indexOf(u8, response_content, "expired") != null or
         mem.indexOf(u8, response_content, "invalid") != null))
    {
        std.debug.print("\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\x1b[0m\n", .{});
        std.debug.print("\x1b[33mYour computer's clock may have drifted.\x1b[0m\n", .{});
        std.debug.print("\x1b[33mCheck your system time and sync with NTP if needed:\x1b[0m\n", .{});
        std.debug.print("\x1b[33m  Linux:   sudo ntpdate -s time.nist.gov\x1b[0m\n", .{});
        std.debug.print("\x1b[33m  macOS:   sudo sntp -sS time.apple.com\x1b[0m\n", .{});
        std.debug.print("\x1b[33m  Windows: w32tm /resync\x1b[0m\n", .{});
        std.fs.cwd().deleteFile(json_file) catch {};
        std.fs.cwd().deleteFile(response_file) catch {};
        return 1;
    }

    // Print response
    std.debug.print("{s}\n", .{response_content});

    // Cleanup
    std.fs.cwd().deleteFile(json_file) catch {};
    std.fs.cwd().deleteFile(response_file) catch {};

    return 0;
}
