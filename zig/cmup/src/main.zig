const zql = @import("zql/zql.zig");
const cmup = @import("cmup/cmup.zig");
const std = @import("std");

pub fn printSuccess() !void {
    const writer = std.io.getStdOut().writer();

    try writer.writeAll("Updated playlists :)\n");
}

pub fn printInfo() !void {
    const writer = std.io.getStdOut().writer();

    try writer.writeAll("If you wish to write playlists into your cmus config playlists\ntry adding --write flag");
}

pub fn hasArg(args: [][]u8, comptime arg_name: []const u8) bool {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, arg_name)) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    // var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // defer arena.deinit();
    //
    // const allocator = arena.allocator();
    //
    // const args = try std.process.argsAlloc(allocator);
    //
    // defer std.process.argsFree(allocator, args);
    // const has_write = hasArg(args, "--write");
    //
    // const home = std.posix.getenv("HOME");
    //
    // if (home) |value| {
    //     const cmus_playlist_path = try std.fs.path.join(allocator, &[_][]const u8{ value, ".config/cmus/playlists" });
    //     const cmus_music_path = try std.fs.path.join(allocator, &.{ value, "Music" });
    //
    //     const result = try cmup.cmup(allocator, has_write, cmus_music_path, cmus_playlist_path);
    //     defer result.deinit();
    //
    //     if (hasArg(args, "--print-everything")) {
    //         try cmup.printCmupPlaylists(result.items, "");
    //     }
    //
    //     if (has_write) {
    //         try printSuccess();
    //     } else {
    //         try printInfo();
    //     }
    // } else {
    //     return error.NoHome;
    // }
    //
    try zql.main();
}
