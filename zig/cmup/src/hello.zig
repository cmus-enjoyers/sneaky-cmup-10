const std = @import("std");
const math = std.math;

pub fn main() !void {
    std.debug.print("Hello, world!\n", .{});

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

    var music = try std.fs.openDirAbsolute("/", .{ .iterate = true });

    defer music.close();

    var iter = music.iterate();

    while (try iter.next()) |entry| {
        std.debug.print("Something: {s}\n", .{entry.name});
    }

    const ruski = "афвафывавф";
    const ruski_num = [_]u8{ 208, 176 };
    const ruski_num_ruski_num = ruski_num ** 100;
    std.debug.print("Something again: {any}, {s}, {any}, {s}, {s}", .{ ruski, ruski, @TypeOf(ruski), ruski_num, ruski_num_ruski_num });
}
