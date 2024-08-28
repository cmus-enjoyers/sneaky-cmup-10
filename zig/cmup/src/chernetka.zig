const std = @import("std");

const Tsettb = struct { hello: []const u8 };

fn testa(ptr: *?*Tsettb) void {
    var smth = Tsettb{
        .hello = "dfasdfasdfasdf",
    };

    ptr.* = &smth;
}

fn wrapper() ?*Tsettb {
    var ptr: ?*Tsettb = null;

    testa(&ptr);

    return &ptr;
}

pub fn main() void {
    const t = wrapper();
    std.debug.print("{}\n", .{t.?.*});
}
