const zql = @import("zql/zql.zig");
const cmup = @import("cmup/cmup.zig");
const clear = @import("cmup/clear.zig").clearPlaylists;
const std = @import("std");
const colors = @import("utils/colors.zig");
const CmupPlaylist = cmup.CmupPlaylist;

// TODO: implement dollar for zql queries

pub fn printSuccess() !void {
    const writer = std.io.getStdOut().writer();

    try writer.writeAll("\nUpdated playlists :)\n");
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

pub fn printQueriesInfo(queries_amount: usize) !void {
    const out = std.io.getStdOut().writer();

    try out.print(
        colors.green_text("\nZql" ++ colors.dim_text(": ") ++ "{} queries found \n\n"),
        .{queries_amount},
    );
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

        var result = try cmup.cmup(allocator, has_write, cmus_music_path, cmus_playlist_path);
        defer result.deinit();

        var map = try cmupPlaylistsToHashMap(allocator, result.playlists.items);
        defer map.deinit();

        if (hasArg(args, "--print-everything")) {
            try cmup.printCmupPlaylists(result.playlists.items, "");
        }

        if (args.len == 2 and std.mem.eql(u8, args[1], "clear")) {
            try clear(cmus_playlist_path);
        }

        try printQueriesInfo(result.zql.items.len);

        const stdout = std.io.getStdOut().writer();

        if (has_write) {
            for (result.zql.items) |path| {
                const playlist = try zql.run(allocator, map, stdout, path);

                try cmup.writeCmupPlaylist(playlist, cmus_playlist_path);
            }

            try printSuccess();
        } else {
            try printInfo();
        }
    } else {
        return error.NoHome;
    }
}
