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
// TODO: hide playlist statement

const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("ast.zig").Parser;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(@embedFile("./test.zql"), allocator);
    defer lexer.deinit();

    while (lexer.nextToken() catch std.process.exit(1)) |val| {
        if (val.type == .EOL) {
            break;
        }
    }

    std.debug.print("\n", .{});

    var parser = Parser.init(&lexer, allocator);
    defer parser.deinit();

    try parser.parse();

    std.debug.print("{}", .{std.json.fmt(parser.nodes.items, .{ .whitespace = .indent_2 })});
}
