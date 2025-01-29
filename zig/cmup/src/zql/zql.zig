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
//
// TODO: hide playlist statement
//
// Statement that will delete `identifier` from ~/.config/cmus/playlists
//
// Example:
//
// require playlist-1
//
// hide playlist-1
//
// TODO: implement identifier renaming
//
// identifier renaming will be helpful because of playlists like "( ´-ω･)︻┻┳══━一~"
//
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

pub fn run(parent_allocator: std.mem.Allocator, map: std.StringHashMap(CmupPlaylist), path: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var file = try std.fs.cwd().openFile(path, .{ .mode = .read_only });
    var buf_reader = std.io.bufferedReader(file.reader());
    var stream = buf_reader.reader();

    const query = try stream.readAllAlloc(allocator, 102400);

    var lexer = Lexer.init(query, allocator);
    defer lexer.deinit();

    try lexer.parse();

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    try parser.parse();

    var executor = Executor.init(allocator, map, parser.nodes.items);

    const result = try executor.execute("test");

    std.debug.print("result of zql query {}\n", .{std.json.fmt(result, .{ .whitespace = .indent_2 })});
}
