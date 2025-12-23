const std = @import("std");

fn fib(n: u32) u32 {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var i: u32 = 0;
    while (i <= 10) : (i += 1) {
        try stdout.print("fib({d}) = {d}\n", .{i, fib(i)});
    }
}
