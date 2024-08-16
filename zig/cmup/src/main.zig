const std = @import("std");

const cmus_path = "/home/vktrenokh/music";
const cmus_playlist_path = "/home/vktrenokh/.config/cmus/playlists";

const CmupPlaylist = struct { name: []const u8, content: [][]const u8, path: []const u8, sub_playlist: ?*CmupPlaylist = null };

const cmup_used_music_extensions: []const []const u8 = &[_][]const u8{ "flac", "mp3", "opus" };

pub fn isMusic(file_name: []const u8) bool {
    for (cmup_used_music_extensions) |ext| {
        if (file_name.len <= ext.len) {
            return false;
        }

        if (std.ascii.endsWithIgnoreCase(file_name, ext)) {
            return true;
        }
    }

    return false;
}

pub fn getDirEntryNames(allocator: std.mem.Allocator, path: []const u8) anyerror!std.ArrayList([]const u8) {
    var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    defer dir.close();
    var iterator = dir.iterate();

    var result = std.ArrayList([]const u8).init(allocator);

    while (try iterator.next()) |value| {
        try result.append(try allocator.dupe(u8, value.name));
    }

    return result;
}

pub fn addMusicToPlaylist(allocator: std.mem.Allocator, path: []const u8, result: *std.ArrayList([]const u8), entry: std.fs.Dir.Entry) !void {
    if (isMusic(entry.name)) {
        try result.append(try std.fs.path.join(allocator, &.{ path, entry.name }));
    }
}

pub fn readCmupPlaylist(allocator: std.mem.Allocator, path: []const u8) anyerror![][]const u8 {
    var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    var iterator = dir.iterate();

    var result = std.ArrayList([]const u8).init(allocator);

    while (try iterator.next()) |item| {
        switch (item.kind) {
            .file => try addMusicToPlaylist(allocator, path, &result, item),
            // TODO: do something with subplaylist
            else => {},
        }
    }

    return result.items;
}

pub fn createCmupPlaylist(allocator: std.mem.Allocator, entry: []const u8) anyerror!CmupPlaylist {
    const path = try std.fs.path.join(allocator, &.{ cmus_path, entry });
    const content = try readCmupPlaylist(allocator, path);

    return CmupPlaylist{ .name = entry, .path = path, .content = content };
}

pub fn writeCmupPlaylist(allocator: std.mem.Allocator, playlist: CmupPlaylist) !void {
    var dir = try std.fs.openDirAbsolute(cmus_playlist_path, .{});
    defer dir.close();

    var file = try dir.createFile(playlist.name, .{});
    defer file.close();

    for (playlist.content) |music| {
        const size = try file.write(try std.mem.concat(allocator, u8, &.{ music, "\n" }));
        _ = size;
    }

    std.debug.print("wrote {s}\n", .{playlist.name});
}

pub fn cmup(allocator: std.mem.Allocator, write: ?bool) anyerror!std.ArrayList(CmupPlaylist) {
    const playlists = try getDirEntryNames(allocator, cmus_path);

    var result = std.ArrayList(CmupPlaylist).init(allocator);

    for (playlists.items) |value| {
        if (std.ascii.startsWithIgnoreCase(value, ".")) {
            continue;
        }

        const playlist = try createCmupPlaylist(allocator, value);
        try result.append(playlist);

        if (write orelse false) {
            try writeCmupPlaylist(allocator, playlist);
        }
    }

    return result;
}

pub fn printCmupPlaylist(playlist: CmupPlaylist) !void {
    const writer = std.io.getStdOut().writer();

    try writer.print("Playlist {s} on path {s} with musics amount {}\n", .{ playlist.name, playlist.path, playlist.content.len });

    for (playlist.content) |value| {
        try writer.print("  {s}\n", .{value});
    }
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

    const result = try cmup(allocator, true);
    try printCmupPlaylists(result.items);
    result.deinit();
}
