const std = @import("std");

const gpa = std.heap.GeneralPurposeAllocator(.{}){};

pub fn getCmupThings(allocator: std.mem.Allocator) anyerror!std.ArrayListAligned([]const u8, null) {
    var dir = try std.fs.openDirAbsolute("/", .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();

    var result = std.ArrayList([]const u8).init(allocator);

    while (try iter.next()) |value| {
        try result.append(value.name);
    }

    return result;
}

pub fn printArrayOfStrings(str_array: [][]const u8) void {
    for (str_array) |item| {
        std.debug.print("{s}\n", .{item});
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cmup = try getCmupThings(allocator);
    printArrayOfStrings(cmup.items);
}
