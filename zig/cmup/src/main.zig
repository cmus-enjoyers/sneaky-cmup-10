const std = @import("std");

const cmus_path = "/home/vktrenokh/music";

const CmupPlaylist = struct {
    name: []const u8,
    content: []const u8,
    path: []const u8,
};

pub fn createCmupPlaylist(allocator: std.mem.Allocator, entry: std.fs.Dir.Entry) CmupPlaylist {
    // NOTE: value.name string will be released and will not be avaailable,
    // to fix this we copy it
    const name = try allocator.dupe(u8, entry.name);

    const path = try std.fs.path.join(allocator, &.{ cmus_path, name });

    return CmupPlaylist{ .name = name, .path = path, .content = "" };
}

pub fn getCmupThings(allocator: std.mem.Allocator) anyerror!std.ArrayList(CmupPlaylist) {
    var dir = try std.fs.openDirAbsolute(cmus_path, .{ .iterate = true });

    var iter = dir.iterate();

    var result = std.ArrayList(CmupPlaylist).init(allocator);

    while (try iter.next()) |value| {
        try result.append(createCmupPlaylist(allocator, value));
    }

    return result;
}

pub fn printCmupPlaylist(playlist: CmupPlaylist) !void {
    const outfile = std.io.getStdOut();

    try outfile.writer().print("Playlist {s} on path {s} with musics: {}\n", .{ playlist.name, playlist.path, playlist.content.len });
}

pub fn printCmupPlaylists(playlists: []const CmupPlaylist) !void {
    for (playlists) |item| {
        try printCmupPlaylist(item);
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cmup = try getCmupThings(allocator);
    try printCmupPlaylists(cmup.items);
    cmup.deinit();
}
