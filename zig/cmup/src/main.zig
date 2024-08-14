const std = @import("std");

const cmus_path = "/home/vktrenokh/music";

const CmupPlaylist = struct {
    name: []const u8,
    content: []const u8,
    path: []const u8,
};

pub fn getCmupThings(allocator: std.mem.Allocator) anyerror!std.ArrayListAligned(CmupPlaylist, null) {
    var dir = try std.fs.openDirAbsolute(cmus_path, .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();

    var result = std.ArrayList(CmupPlaylist).init(allocator);

    while (try iter.next()) |value| {
        const playlist = CmupPlaylist{ .name = value.name, .path = try std.fs.path.join(allocator, &.{ cmus_path, value.name }), .content = "" };
        try result.append(playlist);
    }

    return result;
}

pub fn printCmupPlaylist(playlist: CmupPlaylist) void {
    std.debug.print("Playlist {s}, on path {s}, with music amount {}\n", .{ playlist.name, playlist.path, playlist.content.len });
}

pub fn printCmupPlaylists(playlists: []const CmupPlaylist) void {
    for (playlists) |item| {
        printCmupPlaylist(item);
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const cmup = try getCmupThings(allocator);
    printCmupPlaylists(cmup.items);
}
