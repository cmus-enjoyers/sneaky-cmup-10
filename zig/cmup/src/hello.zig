const std = @import("std");
const math = std.math;

pub fn main() void {
    std.debug.print("Hello, world!\n", .{});

    // const number: ?u8 = 100 / 0;
    //
    // std.debug.print("num {}", .{number});

    std.debug.print(":) " ** 100, .{});
    std.debug.print("\n", .{});

    var x: u8 = 100;

    std.debug.print("x: {}\n", .{x});

    const ptr = &x;

    ptr.* = 101;

    std.debug.print("x: {}\n", .{x});
}
