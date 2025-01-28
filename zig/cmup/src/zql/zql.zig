// Example of a ZQL query:
//
// require jump-bangers, vktrenokh-stwv
//
// add all from jump-bangers
// add all from vktrenokh-stwv where name contains 'voj'
//

// The name of the new playlist should be derived from the ZQL file name
// (e.g., "vktrenokh-eurobeat.zql" => "vktrenokh-eurobeat").
//
// The `require` statement specifies which playlists will be used to create the new playlist.
// - If a playlist is referenced later in the query but is not defined using `require`,
//   the query should terminate with an error.
// - If a playlist is defined in the `require` statement but is not found by the `zmup` program,
//   the query should also terminate with an error.
//
// String literals should only be created using single quotes ('').
// The single quote character (') within a string can be escaped using the backslash (\) character.

// TODO: add comments
//
// Example 1:
//
// -- silly comment
//
// Example 2:
//
// ; silly comment
//
// Example 3:
//
// // silly comment
//
// Example 4:
//
// $ silly comment
//
// Example 5:
//
// slikedollar: silly comment
//

// TODO: hide playlist statement
//
// Example:
//
// require playlist-1
//
// hide playlist-1

// TODO: implement identifier renaming

// Example:
//
// require very-long-playlist-name-that-just-makes-sense as playlist-one
//
// add all from playlist-one

const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("ast.zig").Parser;
const Executor = @import("executor.zig").Executor;

const CmupPlaylist = @import("../cmup/cmup.zig").CmupPlaylist;

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

pub fn run(playlists: []CmupPlaylist) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(@embedFile("./test.zql"), allocator);
    defer lexer.deinit();

    try lexer.parse();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    try parser.parse();

    var hash_map = try cmupPlaylistsToHashMap(allocator, playlists);
    defer hash_map.deinit();

    var executor = Executor.init(allocator, hash_map, parser.nodes.items);

    const result = try executor.execute();

    std.debug.print("result of zql query {}\n", .{std.json.fmt(result, .{ .whitespace = .indent_2 })});
}
