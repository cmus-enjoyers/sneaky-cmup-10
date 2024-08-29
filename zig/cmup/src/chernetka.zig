const std = @import("std");

const Tsettb = struct { hello: []const u8 };

fn testa(allocator: std.mem.Allocator, ptr: *?*Tsettb) !void {
    const smth = try allocator.create(Tsettb);

    smth.* = Tsettb{ .hello = "Hello!" };

    ptr.* = smth;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var ptr: ?*Tsettb = null;

    const allocator = arena.allocator();

    try testa(allocator, &ptr);

    std.debug.print("nig {any}\n", .{ptr});
}
