const std = @import("std");
const c = @cImport({
    @cInclude("qrencode.h");
});

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const qr = c.QRcode_encodeString("unsandbox-qr-ok", 0, c.QR_ECLEVEL_M, c.QR_MODE_8, 1);
    if (qr == null) return error.EncodeFailed;
    defer c.QRcode_free(qr);
    try stdout.print("QR:unsandbox-qr-ok:ROWS:{d}\n", .{qr.*.width});
}
