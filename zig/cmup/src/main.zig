const zql = @import("zql/zql.zig");
const cmup = @import("cmup/cmup.zig");
const std = @import("std");
const CmupPlaylist = cmup.CmupPlaylist;

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

pub fn putCmupPlaylist(map: *std.StringHashMap(CmupPlaylist), playlist: CmupPlaylist) !void {
    try map.put(playlist.name, playlist);

    for (playlist.sub_playlists) |sub_playlist| {
        try putCmupPlaylist(map, sub_playlist.*);
    }
}

pub fn cmupPlaylistsToHashMap(
    allocator: std.mem.Allocator,
    playlists: []CmupPlaylist,
) !std.StringHashMap(CmupPlaylist) {
    var map = std.StringHashMap(CmupPlaylist).init(allocator);

    for (playlists) |playlist| {
        try putCmupPlaylist(&map, playlist);
    }

    return map;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);

    defer std.process.argsFree(allocator, args);
    const has_write = hasArg(args, "--write");

    const home = std.posix.getenv("HOME");

    if (home) |value| {
        const cmus_playlist_path = try std.fs.path.join(allocator, &[_][]const u8{ value, ".config/cmus/playlists" });
        const cmus_music_path = try std.fs.path.join(allocator, &.{ value, "Music" });

        const result = try cmup.cmup(allocator, has_write, cmus_music_path, cmus_playlist_path);
        defer result.deinit();

        var map = try cmupPlaylistsToHashMap(allocator, result.items);
        defer map.deinit();

        if (hasArg(args, "--print-everything")) {
            try cmup.printCmupPlaylists(result.items, "");
        }

        try zql.run(allocator, map, "/home/vktrenokh/Documents/sneaky-cmup-10/zig/cmup/src/zql/test.zql");

        if (has_write) {
            try printSuccess();
        } else {
            try printInfo();
        }
    } else {
        return error.NoHome;
    }
}
