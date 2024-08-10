const std = @import("std");

const CmupPlaylist = struct { name: []const u8 = "Hello World", content: []const u8 };

const TestEnum = enum {
    b,
    c,
};

const Test = union(TestEnum) { b: u8, c: []const u8 };

const Complex = enum { a, b };

fn getComplex(value: Complex) []const u8 {
    return switch (value) {
        Complex.a => "A is bad :)",
        Complex.b => "B is bad :(",
    };
}

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
    std.debug.print("Something again: {any}, {s}, {any}, {s}, {s}\n", .{ ruski, ruski, @TypeOf(ruski), ruski_num, ruski_num_ruski_num });

    const testing = CmupPlaylist{ .content = "speedcore", .name = "Hello World World Vktrenokh" };

    std.debug.print("Vktrenokh playlist: Name: {s}, Content: {s}\n", .{ testing.name, testing.content });

    std.debug.print("Enum: {s} \n", .{getComplex(Complex.b)});

    const y = Test{ .c = "dafdasf" };

    switch (y) {
        TestEnum.b => |v| std.debug.print("Tagged Union Number: {}", .{v}),
        TestEnum.c => |v| std.debug.print("Tagged Union: {s}", .{v}),
    }
}
