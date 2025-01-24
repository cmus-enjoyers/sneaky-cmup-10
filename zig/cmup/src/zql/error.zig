const std = @import("std");
const colors = @import("../utils/colors.zig");

pub fn print(allocator: std.mem.Allocator, out: std.fs.File, position: usize, detail: []const u8) !void {
    const message = try std.fmt.allocPrint(
        allocator,
        colors.red_text("Error") ++ colors.dim_text(" => ") ++ "Unterminated string at {}\n" ++ colors.redUndercurledText("{s}"),
        .{ position, detail },
    );

    try out.writeAll(message);

    allocator.free(message);
}
