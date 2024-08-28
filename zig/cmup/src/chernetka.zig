const std = @import("std");

fn testa(ptr: *?*u8) void {
    var smth: u8 = 40;

    ptr.* = &smth;
}

pub fn main() void {
    var ptr: ?*u8 = null;

    testa(&ptr);

    std.debug.print("{}\n", .{ptr.?.*});
}
