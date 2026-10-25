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

const DEFAULT_API_BASE = "https://api.unsandbox.com";

// API base URL: UNSANDBOX_API_URL (trailing slash stripped), default https://api.unsandbox.com
fn apiBase() []const u8 {
    const v = std.posix.getenv("UNSANDBOX_API_URL") orelse return DEFAULT_API_BASE;
    const trimmed = mem.trimRight(u8, v, "/");
    return if (trimmed.len == 0) DEFAULT_API_BASE else trimmed;
}
const DEFAULT_PORTAL_BASE = "https://unsandbox.com";

// Portal base URL: UNSANDBOX_PORTAL_URL (trailing slash stripped), default https://unsandbox.com.
// Serves /logs/{source} (partner log access) and /keys/extend.
fn portalBase() []const u8 {
    const v = std.posix.getenv("UNSANDBOX_PORTAL_URL") orelse return DEFAULT_PORTAL_BASE;
    const trimmed = mem.trimRight(u8, v, "/");
    return if (trimmed.len == 0) DEFAULT_PORTAL_BASE else trimmed;
}
const MAX_ENV_CONTENT_SIZE: usize = 65536;
const LANGUAGES_CACHE_TTL: i64 = 3600; // 1 hour in seconds
const GREEN = "\x1b[32m";
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const RESET = "\x1b[0m";

extern "c" fn system(command: [*:0]const u8) c_int;
extern "c" fn getpid() c_int;

// Run a shell command via libc system(); cmd need not be null-terminated.
fn runShell(cmd: []const u8) c_int {
    const z = std.heap.c_allocator.dupeZ(u8, cmd) catch return -1;
    defer std.heap.c_allocator.free(z);
    return system(z.ptr);
}

// HMAC-SHA256 computed in-process. Returns lowercase hex (caller frees).
// Never shell out for this: a subprocess would expose secret_key in argv (ps)
// and interpolate message into a shell.
fn computeHmac(allocator: std.mem.Allocator, secret_key: []const u8, message: []const u8) ![]u8 {
    const HmacSha256 = std.crypto.auth.hmac.sha2.HmacSha256;
    var mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&mac, message, secret_key);

    const hex_chars = "0123456789abcdef";
    const out = try allocator.alloc(u8, mac.len * 2);
    for (mac, 0..) |b, i| {
        out[i * 2] = hex_chars[b >> 4];
        out[i * 2 + 1] = hex_chars[b & 0x0f];
    }
    return out;
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

    // The server verifies against the path without its query string
    // (conn.request_path), so sign only the part before '?'.
    const sign_path = if (mem.indexOfScalar(u8, path, '?')) |q| path[0..q] else path;
    const message = try std.fmt.allocPrint(allocator, "{s}:{s}:{s}:{s}", .{ timestamp_str, method, sign_path, body });
    defer allocator.free(message);

    const signature = try computeHmac(allocator, secret_key, message);
    defer allocator.free(signature);

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

const CurlResult = struct {
    body: []const u8,
    status: i32,
};

fn execCurlWithStatus(allocator: std.mem.Allocator, method: []const u8, endpoint: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8, extra_headers: []const u8) !CurlResult {
    const url = try std.fmt.allocPrint(allocator, "{s}{s}", .{ apiBase(), endpoint });
    defer allocator.free(url);

    const auth_headers = try buildAuthCmd(allocator, method, endpoint, body, public_key, secret_key);
    defer allocator.free(auth_headers);

    // Response body + status come back on curl's stdout, never via a file:
    // a rotate-secret response carries the new secret, and a fixed /tmp path
    // would hold it world-readable (and symlink-redirectable) until deleted.
    const body_file = "/tmp/unsandbox_curl_body.txt";
    const cmd = if (body.len > 0) blk: {
        const file = try fs.cwd().createFile(body_file, .{});
        try file.writeAll(body);
        file.close();

        break :blk try std.fmt.allocPrint(allocator, "curl -s -X {s} '{s}' -H 'Content-Type: application/json' {s} {s} --data-binary @{s} -w '\\n%{{http_code}}'", .{ method, url, auth_headers, extra_headers, body_file });
    } else blk: {
        break :blk try std.fmt.allocPrint(allocator, "curl -s -X {s} '{s}' {s} {s} -w '\\n%{{http_code}}'", .{ method, url, auth_headers, extra_headers });
    };
    defer allocator.free(cmd);
    defer if (body.len > 0) fs.cwd().deleteFile(body_file) catch {};

    const run = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "sh", "-c", cmd },
        .max_output_bytes = 1024 * 1024 + 16,
    }) catch return CurlResult{ .body = try allocator.dupe(u8, ""), .status = 0 };
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);

    // Last line holds the status code (from -w); everything before it is the body.
    const split = mem.lastIndexOfScalar(u8, run.stdout, '\n');
    const response_body = try allocator.dupe(u8, if (split) |i| run.stdout[0..i] else "");
    const status_str = if (split) |i| run.stdout[i + 1 ..] else run.stdout;
    const trimmed_status = mem.trim(u8, status_str, &std.ascii.whitespace);
    const status = std.fmt.parseInt(i32, trimmed_status, 10) catch 0;

    return CurlResult{
        .body = response_body,
        .status = status,
    };
}

fn handleSudoChallenge(allocator: std.mem.Allocator, method: []const u8, endpoint: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8, challenge_response: []const u8) !bool {
    // Extract challenge_id from response
    const challenge_id = extractJsonField(challenge_response, "challenge_id") orelse {
        std.debug.print("{s}Error: No challenge_id in 428 response{s}\n", .{ RED, RESET });
        return false;
    };

    // Prompt for OTP
    std.debug.print("{s}Confirmation required. Check your email for a one-time code.{s}\n", .{ YELLOW, RESET });
    std.debug.print("Enter OTP: ", .{});

    // Read OTP from stdin
    const stdin = std.io.getStdIn().reader();
    var otp_buf: [64]u8 = undefined;
    const otp_line = stdin.readUntilDelimiterOrEof(&otp_buf, '\n') catch null;
    if (otp_line == null or otp_line.?.len == 0) {
        std.debug.print("{s}Error: No OTP provided{s}\n", .{ RED, RESET });
        return false;
    }
    const otp = mem.trim(u8, otp_line.?, &std.ascii.whitespace);

    // Build sudo headers
    const sudo_headers = try std.fmt.allocPrint(allocator, "-H 'X-Sudo-OTP: {s}' -H 'X-Sudo-Challenge: {s}'", .{ otp, challenge_id });
    defer allocator.free(sudo_headers);

    // Retry with sudo headers
    const result = try execCurlWithStatus(allocator, method, endpoint, body, public_key, secret_key, sudo_headers);
    defer allocator.free(result.body);

    if (result.status >= 200 and result.status < 300) {
        return true;
    } else {
        std.debug.print("{s}", .{result.body});
        return false;
    }
}

fn execDestructiveCurl(allocator: std.mem.Allocator, method: []const u8, endpoint: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8, success_msg: []const u8) !bool {
    const result = try execCurlWithStatus(allocator, method, endpoint, body, public_key, secret_key, "");

    if (result.status == 428) {
        // Handle sudo challenge
        const success = try handleSudoChallenge(allocator, method, endpoint, body, public_key, secret_key, result.body);
        allocator.free(result.body);
        if (success) {
            std.debug.print("{s}{s}{s}\n", .{ GREEN, success_msg, RESET });
        }
        return success;
    } else if (result.status >= 200 and result.status < 300) {
        allocator.free(result.body);
        std.debug.print("{s}{s}{s}\n", .{ GREEN, success_msg, RESET });
        return true;
    } else {
        std.debug.print("{s}\n", .{result.body});
        allocator.free(result.body);
        return false;
    }
}

fn execCurlPut(allocator: std.mem.Allocator, endpoint: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8) !bool {
    const url = try std.fmt.allocPrint(allocator, "{s}{s}", .{ apiBase(), endpoint });
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

    const ret = runShell(cmd);
    return ret == 0;
}

// ============================================================================
// Library Functions for Zig SDK (matching C reference un.h)
// ============================================================================

pub const SDK_VERSION = "4.2.0";

// Generic API request helper
fn makeApiRequest(allocator: std.mem.Allocator, method: []const u8, path: []const u8, body: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const result = try execCurlWithStatus(allocator, method, path, body, public_key, secret_key, "");
    return result.body;
}

// Execute code synchronously
pub fn execute(allocator: std.mem.Allocator, language: []const u8, code: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const body = try std.fmt.allocPrint(allocator, "{{\"language\":\"{s}\",\"code\":\"{s}\"}}", .{ language, code });
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", "/execute", body, public_key, secret_key);
}

// Execute code asynchronously (returns job_id)
pub fn executeAsync(allocator: std.mem.Allocator, language: []const u8, code: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const body = try std.fmt.allocPrint(allocator, "{{\"language\":\"{s}\",\"code\":\"{s}\",\"async\":true}}", .{ language, code });
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", "/execute", body, public_key, secret_key);
}

// Get job status
pub fn getJob(allocator: std.mem.Allocator, job_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/jobs/{s}", .{job_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

// Wait for job completion
pub fn waitForJob(allocator: std.mem.Allocator, job_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const poll_delays = [_]u64{ 300, 450, 700, 900, 650, 1600, 2000 };
    var delay_idx: usize = 0;

    while (true) {
        const result = try getJob(allocator, job_id, public_key, secret_key);

        // Check for terminal states
        if (mem.indexOf(u8, result, "\"status\":\"completed\"") != null or
            mem.indexOf(u8, result, "\"status\":\"failed\"") != null or
            mem.indexOf(u8, result, "\"status\":\"timeout\"") != null or
            mem.indexOf(u8, result, "\"status\":\"cancelled\"") != null)
        {
            return result;
        }

        allocator.free(result);
        std.time.sleep(poll_delays[delay_idx] * std.time.ns_per_ms);
        if (delay_idx < poll_delays.len - 1) delay_idx += 1;
    }
}

// Cancel a job
pub fn cancelJob(allocator: std.mem.Allocator, job_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/jobs/{s}/cancel", .{job_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

// List all jobs
pub fn listJobs(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "GET", "/jobs", "", public_key, secret_key);
}

// Get supported languages
pub fn getLanguages(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "GET", "/languages", "", public_key, secret_key);
}

// Detect language from filename
pub fn detectLanguage(filename: []const u8) ?[]const u8 {
    const lang_map = [_]struct { ext: []const u8, lang: []const u8 }{
        .{ .ext = ".py", .lang = "python" },
        .{ .ext = ".js", .lang = "javascript" },
        .{ .ext = ".ts", .lang = "typescript" },
        .{ .ext = ".go", .lang = "go" },
        .{ .ext = ".rs", .lang = "rust" },
        .{ .ext = ".c", .lang = "c" },
        .{ .ext = ".cpp", .lang = "cpp" },
        .{ .ext = ".cc", .lang = "cpp" },
        .{ .ext = ".d", .lang = "d" },
        .{ .ext = ".zig", .lang = "zig" },
        .{ .ext = ".rb", .lang = "ruby" },
        .{ .ext = ".php", .lang = "php" },
        .{ .ext = ".sh", .lang = "bash" },
        .{ .ext = ".lua", .lang = "lua" },
        .{ .ext = ".nim", .lang = "nim" },
        .{ .ext = ".v", .lang = "v" },
    };

    const idx = mem.lastIndexOfScalar(u8, filename, '.') orelse return null;
    const ext = filename[idx..];

    for (lang_map) |entry| {
        if (mem.eql(u8, ext, entry.ext)) {
            return entry.lang;
        }
    }
    return null;
}

// Session functions
pub fn sessionList(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "GET", "/sessions", "", public_key, secret_key);
}

pub fn sessionGet(allocator: std.mem.Allocator, session_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}", .{session_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn sessionCreate(allocator: std.mem.Allocator, shell: ?[]const u8, network: ?[]const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    var body_buf: [512]u8 = undefined;
    var stream = std.io.fixedBufferStream(&body_buf);
    const writer = stream.writer();
    try writer.print("{{\"shell\":\"{s}\"", .{shell orelse "bash"});
    if (network) |n| try writer.print(",\"network\":\"{s}\"", .{n});
    try writer.writeAll("}");
    const body = stream.getWritten();
    return try makeApiRequest(allocator, "POST", "/sessions", body, public_key, secret_key);
}

pub fn sessionDestroy(allocator: std.mem.Allocator, session_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}", .{session_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "DELETE", path, "", public_key, secret_key);
}

pub fn sessionFreeze(allocator: std.mem.Allocator, session_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}/freeze", .{session_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn sessionUnfreeze(allocator: std.mem.Allocator, session_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}/unfreeze", .{session_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn sessionBoost(allocator: std.mem.Allocator, session_id: []const u8, vcpu: ?u32, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}/boost", .{session_id});
    defer allocator.free(path);
    var body: []const u8 = "{}";
    if (vcpu) |v| {
        body = try std.fmt.allocPrint(allocator, "{{\"vcpu\":{d}}}", .{v});
    }
    return try makeApiRequest(allocator, "POST", path, body, public_key, secret_key);
}

pub fn sessionUnboost(allocator: std.mem.Allocator, session_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}/unboost", .{session_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn sessionExecute(allocator: std.mem.Allocator, session_id: []const u8, command: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/sessions/{s}/shell", .{session_id});
    defer allocator.free(path);
    const body = try std.fmt.allocPrint(allocator, "{{\"command\":\"{s}\"}}", .{command});
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", path, body, public_key, secret_key);
}

// Service functions
pub fn serviceList(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "GET", "/services", "", public_key, secret_key);
}

pub fn serviceGet(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn serviceDestroy(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "DELETE", path, "", public_key, secret_key);
}

pub fn serviceFreeze(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/freeze", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn serviceUnfreeze(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/unfreeze", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn serviceLock(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/lock", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn serviceUnlock(allocator: std.mem.Allocator, service_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/unlock", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn serviceRedeploy(allocator: std.mem.Allocator, service_id: []const u8, bootstrap: ?[]const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/redeploy", .{service_id});
    defer allocator.free(path);
    const body = if (bootstrap) |b| try std.fmt.allocPrint(allocator, "{{\"bootstrap\":\"{s}\"}}", .{b}) else try allocator.dupe(u8, "{}");
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", path, body, public_key, secret_key);
}

pub fn serviceLogs(allocator: std.mem.Allocator, service_id: []const u8, all: bool, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = if (all)
        try std.fmt.allocPrint(allocator, "/services/{s}/logs?all=true", .{service_id})
    else
        try std.fmt.allocPrint(allocator, "/services/{s}/logs", .{service_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn serviceExecute(allocator: std.mem.Allocator, service_id: []const u8, command: []const u8, timeout_ms: ?u32, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/execute", .{service_id});
    defer allocator.free(path);
    const body = if (timeout_ms) |t|
        try std.fmt.allocPrint(allocator, "{{\"command\":\"{s}\",\"timeout\":{d}}}", .{ command, t })
    else
        try std.fmt.allocPrint(allocator, "{{\"command\":\"{s}\"}}", .{command});
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", path, body, public_key, secret_key);
}

pub fn serviceResize(allocator: std.mem.Allocator, service_id: []const u8, vcpu: u32, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/services/{s}/resize", .{service_id});
    defer allocator.free(path);
    const body = try std.fmt.allocPrint(allocator, "{{\"vcpu\":{d}}}", .{vcpu});
    defer allocator.free(body);
    return try makeApiRequest(allocator, "POST", path, body, public_key, secret_key);
}

// Snapshot functions
pub fn snapshotList(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "GET", "/snapshots", "", public_key, secret_key);
}

pub fn snapshotGet(allocator: std.mem.Allocator, snapshot_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/snapshots/{s}", .{snapshot_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn snapshotRestore(allocator: std.mem.Allocator, snapshot_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/snapshots/{s}/restore", .{snapshot_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn snapshotDelete(allocator: std.mem.Allocator, snapshot_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/snapshots/{s}", .{snapshot_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "DELETE", path, "", public_key, secret_key);
}

pub fn snapshotLock(allocator: std.mem.Allocator, snapshot_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/snapshots/{s}/lock", .{snapshot_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn snapshotUnlock(allocator: std.mem.Allocator, snapshot_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/snapshots/{s}/unlock", .{snapshot_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

// Image functions
pub fn imageList(allocator: std.mem.Allocator, filter: ?[]const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = if (filter) |f|
        try std.fmt.allocPrint(allocator, "/images?filter={s}", .{f})
    else
        try allocator.dupe(u8, "/images");
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn imageGet(allocator: std.mem.Allocator, image_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{image_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "GET", path, "", public_key, secret_key);
}

pub fn imageDelete(allocator: std.mem.Allocator, image_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{image_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "DELETE", path, "", public_key, secret_key);
}

pub fn imageLock(allocator: std.mem.Allocator, image_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/images/{s}/lock", .{image_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

pub fn imageUnlock(allocator: std.mem.Allocator, image_id: []const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const path = try std.fmt.allocPrint(allocator, "/images/{s}/unlock", .{image_id});
    defer allocator.free(path);
    return try makeApiRequest(allocator, "POST", path, "", public_key, secret_key);
}

// PaaS Logs (CLI_SPEC.md "PaaS Logs Command"): GET {portal}/logs/{source}[/stream],
// query percent-encoded, HMAC over "ts:GET:/logs/{source}:" (path only).

fn pctAppend(list: *std.ArrayList(u8), v: []const u8) !void {
    for (v) |c| {
        if (std.ascii.isAlphanumeric(c) or c == '-' or c == '_' or c == '.' or c == '~') {
            try list.append(c);
        } else {
            try list.writer().print("%{X:0>2}", .{c});
        }
    }
}

fn addQuery(list: *std.ArrayList(u8), key: []const u8, val: ?[]const u8) !void {
    const v = val orelse return;
    if (v.len == 0) return;
    try list.append(if (mem.indexOfScalar(u8, list.items, '?') != null) '&' else '?');
    try list.appendSlice(key);
    try list.append('=');
    try pctAppend(list, v);
}

fn logsPath(allocator: std.mem.Allocator, source: []const u8, stream: bool) ![]u8 {
    const src = if (source.len == 0) "all" else source;
    return if (stream) try std.fmt.allocPrint(allocator, "/logs/{s}/stream", .{src}) else try std.fmt.allocPrint(allocator, "/logs/{s}", .{src});
}

const PortalAuth = struct {
    auth: []u8,
    ts: []u8,
    sig: []u8,
    fn deinit(self: PortalAuth, allocator: std.mem.Allocator) void {
        allocator.free(self.auth);
        allocator.free(self.ts);
        allocator.free(self.sig);
    }
};

// Headers for curl argv (no shell). Secret key stays in-process; only the signature leaves.
fn portalAuth(allocator: std.mem.Allocator, path: []const u8, public_key: []const u8, secret_key: []const u8) !PortalAuth {
    const ts = try getTimestamp(allocator);
    defer allocator.free(ts);
    const message = try std.fmt.allocPrint(allocator, "{s}:GET:{s}:", .{ ts, path });
    defer allocator.free(message);
    const sig = try computeHmac(allocator, secret_key, message);
    defer allocator.free(sig);
    return .{
        .auth = try std.fmt.allocPrint(allocator, "Authorization: Bearer {s}", .{public_key}),
        .ts = try std.fmt.allocPrint(allocator, "X-Timestamp: {s}", .{ts}),
        .sig = try std.fmt.allocPrint(allocator, "X-Signature: {s}", .{sig}),
    };
}

/// Raw portal logs fetch. Caller frees .body.
fn logsFetchRaw(allocator: std.mem.Allocator, source: []const u8, lines: u32, since: ?[]const u8, grep: ?[]const u8, level: ?[]const u8, public_key: []const u8, secret_key: []const u8) !CurlResult {
    const path = try logsPath(allocator, source, false);
    defer allocator.free(path);
    var url = std.ArrayList(u8).init(allocator);
    defer url.deinit();
    try url.writer().print("{s}{s}?lines={d}", .{ portalBase(), path, lines });
    try addQuery(&url, "since", since);
    try addQuery(&url, "grep", grep);
    try addQuery(&url, "level", level);
    const h = try portalAuth(allocator, path, public_key, secret_key);
    defer h.deinit(allocator);
    const run = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "curl", "-s", "-X", "GET", url.items, "-H", h.auth, "-H", h.ts, "-H", h.sig, "-A", "un-cli/2.0", "-w", "\n%{http_code}" },
        .max_output_bytes = 64 * 1024 * 1024,
    }) catch return CurlResult{ .body = try allocator.dupe(u8, ""), .status = 0 };
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);
    const split = mem.lastIndexOfScalar(u8, run.stdout, '\n');
    const status_str = mem.trim(u8, if (split) |i| run.stdout[i + 1 ..] else run.stdout, &std.ascii.whitespace);
    return CurlResult{
        .body = try allocator.dupe(u8, if (split) |i| run.stdout[0..i] else ""),
        .status = std.fmt.parseInt(i32, status_str, 10) catch 0,
    };
}

// PaaS Logs functions
pub fn logsFetch(allocator: std.mem.Allocator, source: ?[]const u8, lines: ?u32, since: ?[]const u8, grep: ?[]const u8, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    const r = try logsFetchRaw(allocator, source orelse "all", std.math.clamp(lines orelse 100, 1, 10000), since, grep, null, public_key, secret_key);
    return r.body;
}

fn paasCursorPath(allocator: std.mem.Allocator) !?[]u8 {
    const home = std.posix.getenv("HOME") orelse return null;
    if (home.len == 0) return null;
    return try std.fmt.allocPrint(allocator, "{s}/.unsandbox/paas_log_cursor", .{home});
}

// Seconds since the last successful fetch as "Ns" (min 5), or null. Caller frees.
fn readPaasCursor(allocator: std.mem.Allocator) !?[]u8 {
    const p = (try paasCursorPath(allocator)) orelse return null;
    defer allocator.free(p);
    const data = fs.cwd().readFileAlloc(allocator, p, 64) catch return null;
    defer allocator.free(data);
    const ts = std.fmt.parseInt(i64, mem.trim(u8, data, &std.ascii.whitespace), 10) catch return null;
    if (ts <= 0) return null;
    var ago = std.time.timestamp() - ts;
    if (ago < 5) ago = 5;
    return try std.fmt.allocPrint(allocator, "{d}s", .{ago});
}

fn writePaasCursor(allocator: std.mem.Allocator) void {
    const p = (paasCursorPath(allocator) catch return) orelse return;
    defer allocator.free(p);
    if (fs.path.dirname(p)) |d| fs.cwd().makePath(d) catch {};
    const f = fs.cwd().createFile(p, .{}) catch return;
    defer f.close();
    f.writer().print("{d}\n", .{std.time.timestamp()}) catch {};
}

fn jsonStr(v: std.json.Value, key: []const u8) ?[]const u8 {
    if (v != .object) return null;
    const x = v.object.get(key) orelse return null;
    return if (x == .string) x.string else null;
}

fn printPaasError(allocator: std.mem.Allocator, status: i32, body: []const u8) void {
    const stderr = std.io.getStdErr().writer();
    if (std.json.parseFromSlice(std.json.Value, allocator, body, .{})) |parsed| {
        defer parsed.deinit();
        if (jsonStr(parsed.value, "error")) |e| {
            stderr.print("Error: HTTP {d} \u{2014} {s}\n", .{ status, e }) catch {};
            return;
        }
    } else |_| {}
    stderr.print("Error: HTTP {d}\n", .{status}) catch {};
}

fn paasLogsHelp() void {
    std.debug.print(
        \\Usage: un paas logs [options]
        \\
        \\Fetch or stream PaaS production logs.
        \\
        \\Sources:
        \\  --all              All sources (default)
        \\  --api              API server logs
        \\  --portal           Portal server logs
        \\  --pool NODE        Pool node logs (e.g., cammy, ai)
        \\  --source SOURCE    Source string (api, portal, pool/cammy, all)
        \\
        \\Options:
        \\  --lines N, -n N    Number of lines (default: 100, max: 10000)
        \\  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)
        \\  --grep PATTERN     Filter log lines by pattern
        \\  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit
        \\  --follow, -f       Follow logs in real-time (Ctrl+C to stop)
        \\  --json             Output raw JSON response
        \\  -p KEY             Public key
        \\  -k KEY             Secret key
        \\  -h                 Show this help
        \\
        \\Examples:
        \\  un paas logs                             # last 100 lines from all sources
        \\  un paas logs --api --lines 500           # last 500 API log lines
        \\  un paas logs --portal --grep error       # portal logs matching 'error'
        \\  un paas logs --pool cammy --since 1h     # cammy pool logs from last hour
        \\  un paas logs -l warning                  # warnings and above from all sources
        \\  un paas logs --follow                    # follow all logs in real-time
        \\  un paas logs --follow -l warning         # follow warnings+ in real-time
        \\
        \\Requires a partner API key with log access enabled.
        \\
    , .{});
}

/// `un paas ...`; args[0] is the program, args[1] == "paas".
fn cmdPaas(allocator: std.mem.Allocator, args: []const [:0]u8, public_key: []const u8, secret_key: []const u8) !u8 {
    if (args.len < 3) {
        std.debug.print("Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n\nRun 'un paas <command> -h' for help on a specific command.\n", .{});
        return 1;
    }
    if (!mem.eql(u8, args[2], "logs")) {
        std.debug.print("Error: Unknown paas command '{s}'\nRun 'un paas' for available commands.\n", .{args[2]});
        return 1;
    }
    var source: []const u8 = "all";
    var pool_buf: [256]u8 = undefined;
    var lines: u32 = 100;
    var since: ?[]const u8 = null;
    var grep: ?[]const u8 = null;
    var level: ?[]const u8 = null;
    var follow = false;
    var json_out = false;
    var i: usize = 3;
    while (i < args.len) : (i += 1) {
        const a = args[i];
        const has_next = i + 1 < args.len;
        if ((mem.eql(u8, a, "-p") or mem.eql(u8, a, "-k") or mem.eql(u8, a, "--account")) and has_next) {
            i += 1; // resolved by main's pre-scan
        } else if (mem.eql(u8, a, "--source") and has_next) {
            i += 1;
            source = args[i];
        } else if (mem.eql(u8, a, "--all")) {
            source = "all";
        } else if (mem.eql(u8, a, "--api")) {
            source = "api";
        } else if (mem.eql(u8, a, "--portal")) {
            source = "portal";
        } else if (mem.eql(u8, a, "--pool") and has_next) {
            i += 1;
            source = std.fmt.bufPrint(&pool_buf, "pool/{s}", .{args[i]}) catch "all";
        } else if ((mem.eql(u8, a, "--lines") or mem.eql(u8, a, "-n")) and has_next) {
            i += 1;
            const n = std.fmt.parseInt(i64, args[i], 10) catch 0;
            lines = @intCast(std.math.clamp(n, 1, 10000));
        } else if (mem.eql(u8, a, "--since") and has_next) {
            i += 1;
            since = args[i];
        } else if (mem.eql(u8, a, "--grep") and has_next) {
            i += 1;
            grep = args[i];
        } else if ((mem.eql(u8, a, "--level") or mem.eql(u8, a, "-l")) and has_next) {
            i += 1;
            level = args[i];
        } else if (mem.eql(u8, a, "--follow") or mem.eql(u8, a, "-f")) {
            follow = true;
        } else if (mem.eql(u8, a, "--json")) {
            json_out = true;
        } else if (mem.eql(u8, a, "-h") or mem.eql(u8, a, "--help")) {
            paasLogsHelp();
            return 0;
        }
    }
    if (public_key.len == 0) {
        std.debug.print("Error: API credentials required.\n  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n  Create ~/.unsandbox/accounts.csv with: public_key,secret_key\n", .{});
        return 1;
    }
    if (follow) return try streamPaasLogs(allocator, source, grep, level, public_key, secret_key);

    const cursor = if (since == null) try readPaasCursor(allocator) else null;
    defer if (cursor) |c| allocator.free(c);
    const eff_since: []const u8 = since orelse (cursor orelse "5m");
    const r = try logsFetchRaw(allocator, source, lines, eff_since, grep, level, public_key, secret_key);
    defer allocator.free(r.body);
    if (r.status == 0) {
        std.debug.print("Error: Network error fetching logs\n", .{});
        return 1;
    }
    if (r.status != 200) {
        printPaasError(allocator, r.status, r.body);
        return 1;
    }
    writePaasCursor(allocator);
    const stdout = std.io.getStdOut().writer();
    if (json_out) {
        try stdout.print("{s}\n", .{r.body});
        return 0;
    }
    const parsed = std.json.parseFromSlice(std.json.Value, allocator, r.body, .{}) catch {
        std.debug.print("Error: Invalid response format\n", .{});
        return 1;
    };
    defer parsed.deinit();
    const root = parsed.value;
    const arr = if (root == .object) root.object.get("lines") else null;
    if (arr == null or arr.? != .array) {
        std.debug.print("Error: Invalid response format\n", .{});
        return 1;
    }
    const is_all = root.object.get("sources") != null;
    for (arr.?.array.items) |item| {
        if (item == .string) {
            try stdout.print("{s}\n", .{item.string});
        } else if (jsonStr(item, "line")) |ln| {
            if (is_all) {
                if (jsonStr(item, "source")) |src| {
                    try stdout.print("\x1b[36m{s: <12}\x1b[0m {s}\n", .{ src, ln });
                    continue;
                }
            }
            try stdout.print("{s}\n", .{ln});
        }
    }
    return 0;
}

const STATUS_MARK = "__UNSB_HTTP_STATUS__";

// Follow {portal}/logs/{source}/stream via curl -N; reconnects (2s doubling to 30s)
// with since= covering the gap and a fresh signature. 401/403 is fatal.
fn streamPaasLogs(allocator: std.mem.Allocator, source: []const u8, grep: ?[]const u8, level: ?[]const u8, public_key: []const u8, secret_key: []const u8) !u8 {
    const path = try logsPath(allocator, source, true);
    defer allocator.free(path);
    var base = std.ArrayList(u8).init(allocator);
    defer base.deinit();
    try base.writer().print("{s}{s}", .{ portalBase(), path });
    try addQuery(&base, "grep", grep);
    try addQuery(&base, "level", level);
    std.debug.print("Streaming {s} logs... (Ctrl+C to stop)\n", .{source});
    const stdout = std.io.getStdOut().writer();
    var delay: u64 = 2;
    var last_recv: i64 = 0;
    var reconnect = false;
    while (true) {
        var url = std.ArrayList(u8).init(allocator);
        defer url.deinit();
        try url.appendSlice(base.items);
        if (reconnect and last_recv > 0) {
            var gap = std.time.timestamp() - last_recv + 5;
            if (gap < 10) gap = 10;
            try url.writer().print("{c}since={d}s", .{ @as(u8, if (mem.indexOfScalar(u8, url.items, '?') != null) '&' else '?'), gap });
        }
        const h = try portalAuth(allocator, path, public_key, secret_key);
        defer h.deinit(allocator);
        const connected_at = std.time.timestamp();
        var child = std.process.Child.init(&[_][]const u8{ "curl", "-s", "-N", "--http1.1", "-H", "Accept: text/event-stream", "-H", h.auth, "-H", h.ts, "-H", h.sig, "-A", "un-cli/2.0", "-w", "\n" ++ STATUS_MARK ++ "%{http_code}\n", url.items }, allocator);
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Ignore;
        try child.spawn();
        var status: i32 = 0;
        var br = std.io.bufferedReader(child.stdout.?.reader());
        const reader = br.reader();
        var line_buf = std.ArrayList(u8).init(allocator);
        defer line_buf.deinit();
        while (true) {
            line_buf.clearRetainingCapacity();
            reader.streamUntilDelimiter(line_buf.writer(), '\n', null) catch break;
            const line = line_buf.items;
            if (mem.startsWith(u8, line, STATUS_MARK)) {
                status = std.fmt.parseInt(i32, mem.trim(u8, line[STATUS_MARK.len..], &std.ascii.whitespace), 10) catch 0;
                continue;
            }
            if (!mem.startsWith(u8, line, "data:")) continue;
            const data = mem.trimLeft(u8, line[5..], " ");
            if (data.len == 0 or mem.eql(u8, data, ":keepalive")) continue;
            if (std.json.parseFromSlice(std.json.Value, allocator, data, .{})) |parsed| {
                defer parsed.deinit();
                if (jsonStr(parsed.value, "line")) |ln| {
                    try stdout.print("\x1b[36m{s: <12}\x1b[0m {s}\n", .{ jsonStr(parsed.value, "source") orelse source, ln });
                    last_recv = std.time.timestamp();
                }
            } else |_| {
                try stdout.print("{s}\n", .{data});
                last_recv = std.time.timestamp();
            }
        }
        _ = child.wait() catch {};
        if (status == 401 or status == 403) {
            std.debug.print("Error: HTTP {d} (auth failed)\n", .{status});
            return 1;
        }
        if (status != 0 and status != 200 and status < 500) {
            std.debug.print("Error: HTTP {d}\n", .{status});
            return 1;
        }
        if (status == 0 and !reconnect and last_recv == 0) {
            std.debug.print("Error: Stream error: could not connect\n", .{});
            return 1;
        }
        if (std.time.timestamp() - connected_at > 30) delay = 2;
        std.time.sleep(delay * std.time.ns_per_s);
        delay = @min(delay * 2, 30);
        reconnect = true;
    }
}

// Key validation
pub fn validateKeys(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) ![]const u8 {
    return try makeApiRequest(allocator, "POST", "/keys/validate", "", public_key, secret_key);
}

// Utility functions
pub fn hmacSign(allocator: std.mem.Allocator, secret_key: []const u8, message: []const u8) ![]const u8 {
    return try computeHmac(allocator, secret_key, message);
}

pub fn healthCheck(allocator: std.mem.Allocator) !bool {
    const cmd = try std.fmt.allocPrint(allocator, "curl -s -o /dev/null -w '%{{http_code}}' '{s}/health' 2>/dev/null", .{apiBase()});
    defer allocator.free(cmd);

    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "sh", "-c", cmd },
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    const trimmed = mem.trim(u8, result.stdout, &std.ascii.whitespace);
    return mem.eql(u8, trimmed, "200");
}

pub fn version() []const u8 {
    return SDK_VERSION;
}

var last_error_msg: []const u8 = "";

pub fn setLastError(msg: []const u8) void {
    last_error_msg = msg;
}

pub fn lastError() []const u8 {
    return last_error_msg;
}

fn cmdServiceEnv(allocator: std.mem.Allocator, action: []const u8, target: []const u8, envs: std.ArrayList([]const u8), env_file: ?[]const u8, public_key: []const u8, secret_key: []const u8) !void {
    if (mem.eql(u8, action, "status")) {
        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{target});
        defer allocator.free(path);
        const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
        defer allocator.free(auth_headers);
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}{s}' {s}", .{ apiBase(), path, auth_headers });
        defer allocator.free(cmd);
        _ = runShell(cmd);
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
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}{s}' {s}", .{ apiBase(), path, auth_headers });
        defer allocator.free(cmd);
        _ = runShell(cmd);
        std.debug.print("\n", .{});
    } else if (mem.eql(u8, action, "delete")) {
        const path = try std.fmt.allocPrint(allocator, "/services/{s}/env", .{target});
        defer allocator.free(path);
        const auth_headers = try buildAuthCmd(allocator, "DELETE", path, "", public_key, secret_key);
        defer allocator.free(auth_headers);
        const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}{s}' {s}", .{ apiBase(), path, auth_headers });
        defer allocator.free(cmd);
        _ = runShell(cmd);
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

/// Load credentials from a CSV file at the given 0-based row index,
/// skipping blank lines and lines starting with '#'.
/// Returns allocated pk and sk slices, or null if not found.
fn loadCsvRow(allocator: std.mem.Allocator, csv_path: []const u8, row_index: usize) !?struct { pk: []const u8, sk: []const u8 } {
    const file = fs.cwd().openFile(csv_path, .{}) catch return null;
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var reader = buf_reader.reader();
    var line_buf: [4096]u8 = undefined;
    var data_index: usize = 0;

    while (true) {
        const line = reader.readUntilDelimiterOrEof(&line_buf, '\n') catch break orelse break;
        const trimmed = std.mem.trim(u8, line, " \t\r\n");
        if (trimmed.len == 0 or trimmed[0] == '#') continue;
        if (data_index == row_index) {
            if (std.mem.indexOfScalar(u8, trimmed, ',')) |comma_pos| {
                const pk = std.mem.trim(u8, trimmed[0..comma_pos], " \t");
                // Extra columns (e.g. a label) follow the secret; stop at the next comma.
                const rest = trimmed[comma_pos + 1 ..];
                const sk_end = std.mem.indexOfScalar(u8, rest, ',') orelse rest.len;
                const sk = std.mem.trim(u8, rest[0..sk_end], " \t");
                if (pk.len > 0 and sk.len > 0) {
                    return .{ .pk = try allocator.dupe(u8, pk), .sk = try allocator.dupe(u8, sk) };
                }
            }
            return null;
        }
        data_index += 1;
    }
    return null;
}

/// Resolve credentials using 5-tier priority:
///   1. arg_pk / arg_sk (explicit -p/-k flags) if both non-empty
///   2. account_index >= 0 -> accounts.csv row N (bypasses env vars)
///   3. UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY env vars
///   4. ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env var)
///   5. ./accounts.csv row 0
/// Returns allocated pk and sk slices.
fn resolveCredentials(allocator: std.mem.Allocator, arg_pk: []const u8, arg_sk: []const u8, account_index: i64) !struct { pk: []u8, sk: []u8 } {
    // Tier 1: explicit key flags
    if (arg_pk.len > 0 and arg_sk.len > 0) {
        return .{ .pk = try allocator.dupe(u8, arg_pk), .sk = try allocator.dupe(u8, arg_sk) };
    }

    // Tier 2: --account N bypasses env vars
    if (account_index >= 0) {
        const acct_idx: usize = @intCast(account_index);
        // try ~/.unsandbox/accounts.csv
        const home_opt = process.getEnvVarOwned(allocator, "HOME") catch null;
        if (home_opt) |home| {
            defer allocator.free(home);
            const home_csv = try std.fmt.allocPrint(allocator, "{s}/.unsandbox/accounts.csv", .{home});
            defer allocator.free(home_csv);
            if (try loadCsvRow(allocator, home_csv, acct_idx)) |creds| {
                return .{ .pk = @constCast(creds.pk), .sk = @constCast(creds.sk) };
            }
        }
        // try ./accounts.csv
        if (try loadCsvRow(allocator, "accounts.csv", acct_idx)) |creds| {
            return .{ .pk = @constCast(creds.pk), .sk = @constCast(creds.sk) };
        }
        const stderr = std.io.getStdErr().writer();
        try stderr.print("{s}Error: No credentials found for account index {d} in accounts.csv{s}\n", .{ RED, account_index, RESET });
        std.process.exit(1);
    }

    // Tier 3: env vars
    const env_pk = process.getEnvVarOwned(allocator, "UNSANDBOX_PUBLIC_KEY") catch blk: {
        break :blk process.getEnvVarOwned(allocator, "UNSANDBOX_API_KEY") catch try allocator.dupe(u8, "");
    };
    const env_sk = process.getEnvVarOwned(allocator, "UNSANDBOX_SECRET_KEY") catch try allocator.dupe(u8, "");
    if (env_pk.len > 0 and env_sk.len > 0) {
        return .{ .pk = env_pk, .sk = env_sk };
    }

    // Tier 4: ~/.unsandbox/accounts.csv (default row)
    const default_index_str = process.getEnvVarOwned(allocator, "UNSANDBOX_ACCOUNT") catch try allocator.dupe(u8, "0");
    defer allocator.free(default_index_str);
    const default_index = std.fmt.parseInt(usize, std.mem.trim(u8, default_index_str, " \t"), 10) catch 0;

    const home_opt = process.getEnvVarOwned(allocator, "HOME") catch null;
    if (home_opt) |home| {
        defer allocator.free(home);
        const home_csv = try std.fmt.allocPrint(allocator, "{s}/.unsandbox/accounts.csv", .{home});
        defer allocator.free(home_csv);
        if (try loadCsvRow(allocator, home_csv, default_index)) |creds| {
            allocator.free(env_pk);
            allocator.free(env_sk);
            return .{ .pk = @constCast(creds.pk), .sk = @constCast(creds.sk) };
        }
    }

    // Tier 5: ./accounts.csv
    if (try loadCsvRow(allocator, "accounts.csv", default_index)) |creds| {
        allocator.free(env_pk);
        allocator.free(env_sk);
        return .{ .pk = @constCast(creds.pk), .sk = @constCast(creds.sk) };
    }

    if (env_pk.len > 0) {
        return .{ .pk = env_pk, .sk = env_sk };
    }

    allocator.free(env_pk);
    allocator.free(env_sk);
    const stderr = std.io.getStdErr().writer();
    try stderr.print("{s}Error: No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY{s}\n", .{ RED, RESET });
    std.process.exit(1);
}

/// The accounts.csv the credentials came from (caller frees), or null for
/// flags / env vars. Mirrors resolveCredentials' tiers.
fn credentialsCsvPath(allocator: std.mem.Allocator, arg_pk: []const u8, arg_sk: []const u8, account_index: i64) !?[]u8 {
    if (arg_pk.len > 0 and arg_sk.len > 0) return null;
    var index: usize = 0;
    if (account_index >= 0) {
        index = @intCast(account_index);
    } else {
        const env_pk = std.posix.getenv("UNSANDBOX_PUBLIC_KEY") orelse (std.posix.getenv("UNSANDBOX_API_KEY") orelse "");
        const env_sk = std.posix.getenv("UNSANDBOX_SECRET_KEY") orelse "";
        if (env_pk.len > 0 and env_sk.len > 0) return null;
        const acct = std.posix.getenv("UNSANDBOX_ACCOUNT") orelse "0";
        index = std.fmt.parseInt(usize, mem.trim(u8, acct, " \t"), 10) catch 0;
    }
    if (std.posix.getenv("HOME")) |home| {
        const home_csv = try std.fmt.allocPrint(allocator, "{s}/.unsandbox/accounts.csv", .{home});
        if (try loadCsvRow(allocator, home_csv, index)) |row| {
            allocator.free(row.pk);
            allocator.free(row.sk);
            return home_csv;
        }
        allocator.free(home_csv);
    }
    if (try loadCsvRow(allocator, "accounts.csv", index)) |row| {
        allocator.free(row.pk);
        allocator.free(row.sk);
        return try allocator.dupe(u8, "accounts.csv");
    }
    return null;
}

/// Replace the secret on the line whose first field is public_key. Every other
/// line, field, comment and the order stay as they are. Temp file (0600) + rename.
fn rewriteAccountsCsv(allocator: std.mem.Allocator, path: []const u8, public_key: []const u8, new_secret: []const u8) !bool {
    const content = fs.cwd().readFileAlloc(allocator, path, 16 * 1024 * 1024) catch return false;
    defer allocator.free(content);

    var out = std.ArrayList(u8).init(allocator);
    defer out.deinit();
    var changed = false;
    var lines = mem.splitScalar(u8, content, '\n');
    var first = true;
    while (lines.next()) |line| {
        if (!first) try out.append('\n');
        first = false;
        const stripped = mem.trim(u8, line, " \t\r");
        if (!changed and stripped.len > 0 and stripped[0] != '#') {
            if (mem.indexOfScalar(u8, line, ',')) |c1| {
                if (mem.eql(u8, mem.trim(u8, line[0..c1], " \t\r"), public_key)) {
                    const rest = line[c1 + 1 ..];
                    const c2 = mem.indexOfScalar(u8, rest, ',') orelse rest.len;
                    try out.appendSlice(line[0 .. c1 + 1]);
                    try out.appendSlice(new_secret);
                    try out.appendSlice(rest[c2..]);
                    changed = true;
                    continue;
                }
            }
        }
        try out.appendSlice(line);
    }
    if (!changed) return false;

    const dir = fs.path.dirname(path) orelse ".";
    const tmp = try std.fmt.allocPrint(allocator, "{s}/.{s}.{d}.tmp", .{ dir, fs.path.basename(path), getpid() });
    defer allocator.free(tmp);
    {
        const f = fs.cwd().createFile(tmp, .{ .mode = 0o600, .truncate = true }) catch return false;
        defer f.close();
        f.writeAll(out.items) catch {
            fs.cwd().deleteFile(tmp) catch {};
            return false;
        };
        f.chmod(0o600) catch {};
    }
    fs.cwd().rename(tmp, path) catch {
        fs.cwd().deleteFile(tmp) catch {};
        return false;
    };
    return true;
}

/// Raw (unescaped-as-is) string value of "field" in a flat JSON object,
/// tolerating whitespace around the colon. Null when absent or not a string.
fn jsonStringField(json: []const u8, field: []const u8) ?[]const u8 {
    var pos: usize = 0;
    while (pos < json.len) {
        const q = mem.indexOfPos(u8, json, pos, "\"") orelse return null;
        if (q + 1 + field.len + 1 <= json.len and mem.eql(u8, json[q + 1 .. q + 1 + field.len], field) and json[q + 1 + field.len] == '"') {
            var i = q + field.len + 2;
            while (i < json.len and std.ascii.isWhitespace(json[i])) i += 1;
            if (i < json.len and json[i] == ':') {
                i += 1;
                while (i < json.len and std.ascii.isWhitespace(json[i])) i += 1;
                if (i >= json.len or json[i] != '"') return null;
                const value_start = i + 1;
                var j = value_start;
                while (j < json.len and json[j] != '"') : (j += 1) {
                    if (json[j] == '\\') j += 1;
                }
                if (j >= json.len) return null;
                return json[value_start..j];
            }
        }
        pos = q + 1;
    }
    return null;
}

/// Only [A-Za-z0-9_-] may reach the curl command line (OTP / challenge id).
fn isSafeToken(v: []const u8) bool {
    if (v.len == 0) return false;
    for (v) |ch| {
        if (!(std.ascii.isAlphanumeric(ch) or ch == '-' or ch == '_')) return false;
    }
    return true;
}

fn postRotateSecret(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8, otp: ?[]const u8, challenge: []const u8) !CurlResult {
    if (otp) |code| {
        if (!isSafeToken(code) or !isSafeToken(challenge)) {
            std.debug.print("Error: invalid one-time code or challenge\n", .{});
            std.process.exit(2);
        }
        const sudo_headers = try std.fmt.allocPrint(allocator, "-H 'X-Sudo-OTP: {s}' -H 'X-Sudo-Challenge: {s}'", .{ code, challenge });
        defer allocator.free(sudo_headers);
        return try execCurlWithStatus(allocator, "POST", "/keys/self/rotate-secret", "{}", public_key, secret_key, sudo_headers);
    }
    return try execCurlWithStatus(allocator, "POST", "/keys/self/rotate-secret", "{}", public_key, secret_key, "");
}

// JSON number for key ("key": 123), tolerating whitespace; null when absent.
fn jsonNumberField(json: []const u8, key: []const u8) ?i64 {
    var buf: [128]u8 = undefined;
    const needle = std.fmt.bufPrint(&buf, "\"{s}\"", .{key}) catch return null;
    var from: usize = 0;
    while (mem.indexOfPos(u8, json, from, needle)) |p| {
        from = p + 1;
        var q = p + needle.len;
        while (q < json.len and std.ascii.isWhitespace(json[q])) q += 1;
        if (q >= json.len or json[q] != ':') continue;
        q += 1;
        while (q < json.len and std.ascii.isWhitespace(json[q])) q += 1;
        var e = q;
        if (e < json.len and json[e] == '-') e += 1;
        while (e < json.len and std.ascii.isDigit(json[e])) e += 1;
        return std.fmt.parseInt(i64, json[q..e], 10) catch null;
    }
    return null;
}

/// un key: POST /keys/validate, display per CLI_SPEC.md "`un key` output".
/// The payload carries no public key: print the one we signed with.
fn cmdKeyValidate(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8) !u8 {
    const stdout = std.io.getStdOut().writer();
    const res = try execCurlWithStatus(allocator, "POST", "/keys/validate", "", public_key, secret_key, "-H 'Content-Type: application/json'");
    defer allocator.free(res.body);
    const body = res.body;

    var state: []const u8 = "invalid";
    var reason_buf: [256]u8 = undefined;
    var reason: []const u8 = "unauthorized";
    if (res.status >= 200 and res.status < 300) {
        if (jsonStringField(body, "status")) |st| {
            state = st;
        } else {
            const v = mem.indexOf(u8, body, "\"valid\": true") != null or mem.indexOf(u8, body, "\"valid\":true") != null;
            state = if (v) "active" else "invalid";
        }
        reason = std.fmt.bufPrint(&reason_buf, "status {s}", .{state}) catch "status";
    } else if (res.status == 401) {
        if (jsonStringField(body, "error")) |e| reason = e;
        if (mem.eql(u8, reason, ":expired") or mem.eql(u8, reason, "expired")) {
            state = "expired";
        } else if (mem.eql(u8, reason, ":suspended") or mem.eql(u8, reason, "suspended")) {
            state = "suspended";
        } else {
            state = "invalid";
        }
    } else if (res.status == 0) {
        std.debug.print("{s}Error: request to {s}/keys/validate failed{s}\n", .{ RED, apiBase(), RESET });
        return 1;
    } else {
        const msg = jsonStringField(body, "error") orelse (jsonStringField(body, "message") orelse body);
        std.debug.print("{s}Error: HTTP {d} {s}{s}\n", .{ RED, res.status, msg, RESET });
        return 4;
    }

    if (mem.eql(u8, state, "expired")) {
        try stdout.print("{s}Expired{s}\nPublic Key: {s}\n{s}To renew: Visit {s}/keys/extend{s}\n", .{ RED, RESET, public_key, YELLOW, portalBase(), RESET });
        return 3;
    } else if (mem.eql(u8, state, "suspended")) {
        try stdout.print("{s}Suspended{s}\nPublic Key: {s}\n", .{ RED, RESET, public_key });
        return 3;
    } else if (!mem.eql(u8, state, "active")) {
        try stdout.print("{s}Invalid{s}\nPublic Key: {s}\nReason: {s}\n", .{ RED, RESET, public_key, reason });
        return 3;
    }

    var tier_buf: [32]u8 = undefined;
    const tier: []const u8 = if (jsonNumberField(body, "tier")) |t| (std.fmt.bufPrint(&tier_buf, "{d}", .{t}) catch "N/A") else "N/A";
    const status = jsonStringField(body, "status") orelse "N/A";
    var exp_buf: [32]u8 = undefined;
    var rem_buf: [64]u8 = undefined;
    var expires: []const u8 = "N/A";
    var remaining: []const u8 = "N/A";
    if (jsonNumberField(body, "valid_through")) |ms| {
        const secs: i64 = @divFloor(ms, 1000);
        const es = std.time.epoch.EpochSeconds{ .secs = @intCast(@max(secs, 0)) };
        const yd = es.getEpochDay().calculateYearDay();
        const md = yd.calculateMonthDay();
        const ds = es.getDaySeconds();
        expires = std.fmt.bufPrint(&exp_buf, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z", .{
            yd.year, md.month.numeric(), md.day_index + 1, ds.getHoursIntoDay(), ds.getMinutesIntoHour(), ds.getSecondsIntoMinute(),
        }) catch "N/A";
        const left: i64 = secs - std.time.timestamp();
        if (left <= 0) {
            remaining = "expired";
        } else {
            remaining = std.fmt.bufPrint(&rem_buf, "{d}d {d}h {d}m", .{ @divFloor(left, 86400), @divFloor(@mod(left, 86400), 3600), @divFloor(@mod(left, 3600), 60) }) catch "N/A";
        }
    }
    try stdout.print("{s}Valid{s}\nPublic Key: {s}\nTier: {s}\nStatus: {s}\nExpires: {s}\nTime Remaining: {s}\n", .{ GREEN, RESET, public_key, tier, status, expires, remaining });
    return 0;
}

/// un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
fn cmdKeyRotate(allocator: std.mem.Allocator, public_key: []const u8, secret_key: []const u8, otp_flag: ?[]const u8, arg_pk: []const u8, arg_sk: []const u8, account_index: i64) !u8 {
    const stdout = std.io.getStdOut().writer();
    const home = std.posix.getenv("HOME") orelse {
        std.debug.print("Error: HOME not set\n", .{});
        return 1;
    };
    const unsandbox_dir = try std.fmt.allocPrint(allocator, "{s}/.unsandbox", .{home});
    defer allocator.free(unsandbox_dir);
    var digest: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(public_key, &digest, .{});
    const challenge_file = try std.fmt.allocPrint(allocator, "{s}/rotate-{s}.challenge", .{ unsandbox_dir, std.fmt.fmtSliceHexLower(digest[0..8]) });
    defer allocator.free(challenge_file);

    var result: CurlResult = undefined;
    if (otp_flag) |otp_raw| {
        const saved = fs.cwd().readFileAlloc(allocator, challenge_file, 4096) catch {
            std.debug.print("Error: no rotation in progress; run: un key --rotate\n", .{});
            return 2;
        };
        defer allocator.free(saved);
        result = try postRotateSecret(allocator, public_key, secret_key, mem.trim(u8, otp_raw, &std.ascii.whitespace), mem.trim(u8, saved, &std.ascii.whitespace));
        // status 0: curl never reached the API; keep the challenge for a retry
        if (result.status != 0) fs.cwd().deleteFile(challenge_file) catch {};
    } else {
        result = try postRotateSecret(allocator, public_key, secret_key, null, "");
        if (result.status == 428) {
            const challenge_body = result.body;
            defer allocator.free(challenge_body);
            const challenge = jsonStringField(challenge_body, "challenge_id") orelse "";
            if (std.posix.isatty(std.io.getStdIn().handle)) {
                std.debug.print("One-time code emailed to the key owner.\n", .{});
                std.debug.print("Code: ", .{});
                var otp_buf: [128]u8 = undefined;
                const line = std.io.getStdIn().reader().readUntilDelimiterOrEof(&otp_buf, '\n') catch null;
                const otp = if (line) |l| mem.trim(u8, l, &std.ascii.whitespace) else "";
                if (otp.len == 0) {
                    std.debug.print("Error: cancelled\n", .{});
                    return 1;
                }
                result = try postRotateSecret(allocator, public_key, secret_key, otp, challenge);
            } else {
                std.posix.mkdir(unsandbox_dir, 0o700) catch |err| switch (err) {
                    error.PathAlreadyExists => {},
                    else => {
                        std.debug.print("Error: cannot create {s}\n", .{unsandbox_dir});
                        return 1;
                    },
                };
                const f = fs.cwd().createFile(challenge_file, .{ .mode = 0o600, .truncate = true }) catch {
                    std.debug.print("Error: cannot write {s}\n", .{challenge_file});
                    return 1;
                };
                defer f.close();
                try f.writeAll(challenge);
                f.chmod(0o600) catch {};
                try stdout.print("One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE\n", .{});
                return 0;
            }
        }
    }
    defer allocator.free(result.body);

    if (result.status != 200) {
        const err_field = jsonStringField(result.body, "error") orelse "";
        if (result.status == 403 and mem.eql(u8, err_field, "email_required")) {
            std.debug.print("Error: this key has no email; add one or use POST /keys/recover\n", .{});
            return 3;
        }
        if (result.status == 401) {
            std.debug.print("Error: authentication failed\n", .{});
            return 3;
        }
        if (jsonStringField(result.body, "message")) |msg| {
            std.debug.print("Error: {s}\n", .{msg});
        } else if (err_field.len > 0) {
            std.debug.print("Error: {s}\n", .{err_field});
        } else {
            std.debug.print("Error: HTTP {d}\n", .{result.status});
        }
        return 4;
    }
    const new_secret = jsonStringField(result.body, "secret") orelse "";
    if (new_secret.len == 0) {
        std.debug.print("Error: API returned no secret\n", .{});
        return 4;
    }

    if (try credentialsCsvPath(allocator, arg_pk, arg_sk, account_index)) |csv_path| {
        defer allocator.free(csv_path);
        if (try rewriteAccountsCsv(allocator, csv_path, public_key, new_secret)) {
            try stdout.print("Secret rotated. Updated {s} (public key {s}).\n", .{ csv_path, public_key });
            return 0;
        }
    }
    try stdout.print("New secret (shown once, store it now): {s}\n", .{new_secret});
    try stdout.print("Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.\n", .{});
    return 0;
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
        std.debug.print("       {s} key --rotate [--otp CODE]\n", .{args[0]});
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
        std.debug.print("\nCredential options (global):\n", .{});
        std.debug.print("  -p PUBLIC_KEY       Explicit public key\n", .{});
        std.debug.print("  -k SECRET_KEY       Explicit secret key\n", .{});
        std.debug.print("  --account N         Use row N from accounts.csv (bypasses env vars)\n", .{});
        return 1;
    }

    // Pre-scan for --account N, -p, and -k before subcommand dispatch
    var account_index: i64 = -1;
    var arg_pk: []const u8 = "";
    var arg_sk: []const u8 = "";
    {
        var scan_i: usize = 1;
        while (scan_i < args.len) : (scan_i += 1) {
            if (mem.eql(u8, args[scan_i], "--account") and scan_i + 1 < args.len) {
                scan_i += 1;
                account_index = std.fmt.parseInt(i64, args[scan_i], 10) catch -1;
            } else if (mem.eql(u8, args[scan_i], "-p") and scan_i + 1 < args.len) {
                scan_i += 1;
                arg_pk = args[scan_i];
            } else if (mem.eql(u8, args[scan_i], "-k") and scan_i + 1 < args.len) {
                scan_i += 1;
                arg_sk = args[scan_i];
            }
        }
    }

    const creds = try resolveCredentials(allocator, arg_pk, arg_sk, account_index);
    var public_key = creds.pk;
    defer allocator.free(public_key);
    const secret_key = creds.sk;
    defer allocator.free(secret_key);

    // Handle paas command (portal platform logs)
    if (mem.eql(u8, args[1], "paas")) {
        return try cmdPaas(allocator, args, public_key, secret_key);
    }

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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/sessions' {s}", .{ apiBase(), auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (kill) |k| {
            const path = try std.fmt.allocPrint(allocator, "/sessions/{s}", .{k});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "DELETE", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X DELETE '{s}/sessions/{s}' {s}", .{ apiBase(), k, auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\x1b[32mSession terminated: {s}\x1b[0m\n", .{k});
        } else {
            const sh = shell orelse "bash";
            const input_files_json = try buildInputFilesJson(allocator, input_files);
            defer allocator.free(input_files_json);
            const json = try std.fmt.allocPrint(allocator, "{{\"shell\":\"{s}\"{s}}}", .{ sh, input_files_json });
            defer allocator.free(json);
            const auth_headers = try buildAuthCmd(allocator, "POST", "/sessions", json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/sessions' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), auth_headers, json });
            defer allocator.free(cmd);
            std.debug.print("\x1b[33mCreating session...\x1b[0m\n", .{});
            _ = runShell(cmd);
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
        var execute_id: ?[]const u8 = null;
        var command: ?[]const u8 = null;
        var dump_bootstrap: ?[]const u8 = null;
        var dump_file: ?[]const u8 = null;
        var resize: ?[]const u8 = null;
        var vcpu: i32 = 0;
        var unfreeze_on_demand = false;
        var set_unfreeze_on_demand_id: ?[]const u8 = null;
        var set_unfreeze_on_demand_enabled: ?[]const u8 = null;
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
                execute_id = args[i];
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
            } else if (mem.eql(u8, args[i], "--unfreeze-on-demand")) {
                unfreeze_on_demand = true;
            } else if (mem.eql(u8, args[i], "--set-unfreeze-on-demand") and i + 2 < args.len) {
                i += 1;
                set_unfreeze_on_demand_id = args[i];
                i += 1;
                set_unfreeze_on_demand_enabled = args[i];
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services' {s}", .{ apiBase(), auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (info) |inf| {
            const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{inf});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/services/{s}' {s}", .{ apiBase(), inf, auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (execute_id) |exec_id| {
            const cmd_text = command orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"command\":\"{s}\"}}", .{cmd_text});
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/services/{s}/execute", .{exec_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services/{s}/execute' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), exec_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (dump_bootstrap) |bootstrap_id| {
            std.debug.print("Fetching bootstrap script from {s}...\n", .{bootstrap_id});
            const tmp_file = "/tmp/unsandbox_bootstrap_dump.txt";
            const json = "{{\"command\":\"cat /tmp/bootstrap.sh\"}}";
            const path = try std.fmt.allocPrint(allocator, "/services/{s}/execute", .{bootstrap_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services/{s}/execute' -H 'Content-Type: application/json' {s} -d '{s}' -o {s}", .{ apiBase(), bootstrap_id, auth_headers, json, tmp_file });
            defer allocator.free(cmd);
            _ = runShell(cmd);

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
                            _ = runShell(chmod_cmd);
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X PATCH '{s}/services/{s}' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), resize_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);

            // Calculate RAM
            const ram = vcpu * 2;
            std.debug.print("\n{s}Service resized to {d} vCPU, {d} GB RAM{s}\n", .{ GREEN, vcpu, ram, RESET });
        } else if (set_unfreeze_on_demand_id) |svc_id| {
            // Handle set-unfreeze-on-demand
            const enabled = if (set_unfreeze_on_demand_enabled) |e|
                mem.eql(u8, e, "true") or mem.eql(u8, e, "1")
            else
                false;
            const enabled_str = if (enabled) "true" else "false";
            const json = try std.fmt.allocPrint(allocator, "{{\"unfreeze_on_demand\":{s}}}", .{enabled_str});
            defer allocator.free(json);

            const path = try std.fmt.allocPrint(allocator, "/services/{s}", .{svc_id});
            defer allocator.free(path);

            const auth_headers = try buildAuthCmd(allocator, "PATCH", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);

            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X PATCH '{s}/services/{s}' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), svc_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);

            if (enabled) {
                std.debug.print("\n{s}Unfreeze-on-demand enabled for service {s}{s}\n", .{ GREEN, svc_id, RESET });
            } else {
                std.debug.print("\n{s}Unfreeze-on-demand disabled for service {s}{s}\n", .{ GREEN, svc_id, RESET });
            }
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
            if (unfreeze_on_demand) {
                try writer.writeAll(",\"unfreeze_on_demand\":true");
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
                const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services' -H 'Content-Type: application/json' {s} -d '{s}' -o {s}", .{ apiBase(), auth_headers, json_str, response_file });
                defer allocator.free(cmd);
                std.debug.print("{s}Creating service...{s}\n", .{ YELLOW, RESET });
                _ = runShell(cmd);

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
                const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/services' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), auth_headers, json_str });
                defer allocator.free(cmd);
                std.debug.print("{s}Creating service...{s}\n", .{ YELLOW, RESET });
                _ = runShell(cmd);
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/languages' {s} -o {s}", .{ apiBase(), auth_headers, json_file });
            defer allocator.free(cmd);
            _ = runShell(cmd);

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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/images' {s}", .{ apiBase(), auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (info) |inf| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{inf});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "GET", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X GET '{s}/images/{s}' {s}", .{ apiBase(), inf, auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (delete) |del_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}", .{del_id});
            defer allocator.free(path);
            const success_msg = try std.fmt.allocPrint(allocator, "Image deleted: {s}", .{del_id});
            defer allocator.free(success_msg);
            _ = try execDestructiveCurl(allocator, "DELETE", path, "", public_key, secret_key, success_msg);
        } else if (lock) |lock_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/lock", .{lock_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, "", public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/lock' {s}", .{ apiBase(), lock_id, auth_headers });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n{s}Image locked: {s}{s}\n", .{ GREEN, lock_id, RESET });
        } else if (unlock) |unlock_id| {
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/unlock", .{unlock_id});
            defer allocator.free(path);
            const success_msg = try std.fmt.allocPrint(allocator, "Image unlocked: {s}", .{unlock_id});
            defer allocator.free(success_msg);
            _ = try execDestructiveCurl(allocator, "POST", path, "", public_key, secret_key, success_msg);
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/publish' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/visibility' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), vis_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);
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
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/spawn' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), spawn_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);
            std.debug.print("\n", .{});
        } else if (clone) |clone_id| {
            const nm = name orelse "";
            const json = try std.fmt.allocPrint(allocator, "{{\"name\":\"{s}\"}}", .{nm});
            defer allocator.free(json);
            const path = try std.fmt.allocPrint(allocator, "/images/{s}/clone", .{clone_id});
            defer allocator.free(path);
            const auth_headers = try buildAuthCmd(allocator, "POST", path, json, public_key, secret_key);
            defer allocator.free(auth_headers);
            const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/images/{s}/clone' -H 'Content-Type: application/json' {s} -d '{s}'", .{ apiBase(), clone_id, auth_headers, json });
            defer allocator.free(cmd);
            _ = runShell(cmd);
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
        var rotate = false;
        var otp_flag: ?[]const u8 = null;
        var i: usize = 2;
        while (i < args.len) : (i += 1) {
            if (mem.eql(u8, args[i], "--extend")) {
                extend = true;
            } else if (mem.eql(u8, args[i], "--rotate")) {
                rotate = true;
            } else if (mem.eql(u8, args[i], "--otp") and i + 1 < args.len) {
                i += 1;
                otp_flag = args[i];
            } else if (mem.eql(u8, args[i], "-k") and i + 1 < args.len) {
                i += 1;
                allocator.free(public_key);
                public_key = try allocator.dupe(u8, args[i]);
            }
        }

        if (rotate) {
            return try cmdKeyRotate(allocator, public_key, secret_key, otp_flag, arg_pk, arg_sk, account_index);
        }

        if (extend) {
            const url = try std.fmt.allocPrint(allocator, "{s}/keys/extend?pk={s}", .{ portalBase(), public_key });
            defer allocator.free(url);
            std.debug.print("\x1b[33mOpening browser to extend key...\x1b[0m\n", .{});
            const open_cmd = try std.fmt.allocPrint(allocator, "xdg-open '{s}' 2>/dev/null || open '{s}' 2>/dev/null || start '{s}' 2>/dev/null", .{ url, url, url });
            defer allocator.free(open_cmd);
            _ = runShell(open_cmd);
            return 0;
        }
        return try cmdKeyValidate(allocator, public_key, secret_key);
    }

    // Execute mode - find source file (skip known flags and their values)
    var source_file: ?[]const u8 = null;
    {
        var exec_i: usize = 1;
        while (exec_i < args.len) : (exec_i += 1) {
            const arg = args[exec_i];
            if (mem.eql(u8, arg, "--account") or mem.eql(u8, arg, "-p") or
                mem.eql(u8, arg, "-k") or mem.eql(u8, arg, "-e") or
                mem.eql(u8, arg, "-n") or mem.eql(u8, arg, "-v"))
            {
                exec_i += 1; // skip value
            } else if (mem.eql(u8, arg, "-a")) {
                // no value
            } else if (mem.startsWith(u8, arg, "-")) {
                const stderr = std.io.getStdErr().writer();
                stderr.print("{s}Unknown option: {s}{s}\n", .{ RED, arg, RESET }) catch {};
                std.process.exit(1);
            } else {
                source_file = arg;
                break;
            }
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
    const cmd = try std.fmt.allocPrint(allocator, "curl -s -X POST '{s}/execute' -H 'Content-Type: application/json' {s} -d @{s} -o {s}", .{ apiBase(), auth_headers, json_file, response_file });
    defer allocator.free(cmd);

    _ = runShell(cmd);

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
