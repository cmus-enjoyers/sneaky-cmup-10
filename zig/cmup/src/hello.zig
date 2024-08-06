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

    const @"Funny variable" = 50;

    std.debug.print("{}\n", .{@"Funny variable"});

    const a = [_:255]i32{
        1,
        2,
        4,
        0,
        1,
        2,
    };

    std.debug.print("{}\n", .{a[6]}); // should print 255
    std.debug.print("{any}\n", .{a});
}
