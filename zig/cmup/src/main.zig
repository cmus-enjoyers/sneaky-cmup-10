const std = @import("std");

const cmus_path = "/home/vktrenokh/music";
const cmus_playlist_path = "/home/vktrenokh/.config/cmus/playlists";

const CmupPlaylist = struct {
    name: []const u8,
    content: [][]const u8,
    path: []const u8,
    sub_playlists: []*CmupPlaylist,
};

const CmupReadPlaylist = struct {
    items: [][]const u8,
    sub_playlists: []*CmupPlaylist,
};

const cmup_used_music_extensions: []const []const u8 = &[_][]const u8{
    "flac",
    "mp3",
    "opus",
};

const reset = "\x1b[0m";
const green = "\x1b[32m";
const red = "\x1b[31m";

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
        switch (value.kind) {
            .directory => try result.append(try allocator.dupe(u8, value.name)),
            else => try printUnsuportedEntryError(value.name),
        }
    }

    return result;
}

pub fn addMusicToPlaylist(allocator: std.mem.Allocator, path: []const u8, result: *std.ArrayList([]const u8), entry: std.fs.Dir.Entry) !void {
    if (isMusic(entry.name)) {
        try result.append(try std.fs.path.join(allocator, &.{ path, entry.name }));
    }
}
pub fn printUnsuportedEntryError(name: []const u8) !void {
    const writer = std.io.getStdErr().writer();

    try writer.print(red ++ "CmupErr" ++ reset ++ ": Unknown entry format at {s}\n", .{name});
}

pub fn createCmusSubPlaylist(allocator: std.mem.Allocator, ptrs: *std.ArrayList(*CmupPlaylist), parent_name: []const u8, name: []const u8) anyerror!void {
    const path = try std.fs.path.join(allocator, &.{ parent_name, name });

    const playlist = try allocator.create(CmupPlaylist);

    playlist.* = try createCmupPlaylist(allocator, path);

    try ptrs.append(playlist);
}

pub fn readCmupPlaylist(allocator: std.mem.Allocator, path: []const u8, name: []const u8) anyerror!CmupReadPlaylist {
    var dir = try std.fs.openDirAbsolute(path, .{ .iterate = true });
    var iterator = dir.iterate();

    var ptrs = std.ArrayList(*CmupPlaylist).init(allocator);

    var result = std.ArrayList([]const u8).init(allocator);

    while (try iterator.next()) |item| {
        switch (item.kind) {
            .file => try addMusicToPlaylist(allocator, path, &result, item),
            .directory => try createCmusSubPlaylist(allocator, &ptrs, name, item.name),
            else => try printUnsuportedEntryError(item.name),
        }
    }

    return CmupReadPlaylist{
        .items = result.items,
        .sub_playlists = ptrs.items,
    };
}

pub fn createCmupPlaylist(allocator: std.mem.Allocator, entry: []const u8) anyerror!CmupPlaylist {
    const path = try std.fs.path.join(allocator, &.{ cmus_path, entry });
    const content = try readCmupPlaylist(allocator, path, entry);

    return CmupPlaylist{
        .name = entry,
        .path = path,
        .content = content.items,
        .sub_playlists = content.sub_playlists,
    };
}

pub fn writeCmupPlaylist(playlist: CmupPlaylist) !void {
    var dir = try std.fs.openDirAbsolute(cmus_playlist_path, .{});
    defer dir.close();

    var file = try dir.createFile(playlist.name, .{});
    defer file.close();

    const newline = comptime "\n";

    for (playlist.content) |music| {
        try file.writeAll(music);
        try file.writeAll(newline);
    }
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
            try writeCmupPlaylist(playlist);
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

    for (playlist.sub_playlists) |sub_playlist| {
        try printCmupPlaylist(sub_playlist.*);
    }
}

pub fn printCmupPlaylists(playlists: []const CmupPlaylist) !void {
    for (playlists) |item| {
        try printCmupPlaylist(item);
    }
}

pub fn hasArg(args: [][]u8, arg_name: []const u8) bool {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, arg_name)) {
            return true;
        }
    }
    return false;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const result = try cmup(allocator, hasArg(args, "--write"));
    defer result.deinit();

    try printCmupPlaylists(result.items);
}
