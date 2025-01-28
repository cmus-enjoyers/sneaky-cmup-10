const std = @import("std");
const CmupPlaylist = @import("../cmup/cmup.zig").CmupPlaylist;
const ASTNode = @import("ast.zig").ASTNode;

pub const Executor = struct {
    playlists: []CmupPlaylist,
    ast: []ASTNode,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, playlists: []CmupPlaylist, ast: []ASTNode) Executor {
        return Executor{
            .playlists = playlists,
            .allocator = allocator,
            .ast = ast,
        };
    }

    pub fn execute(executor: *Executor) !std.ArrayList(CmupPlaylist) {
        var result = std.ArrayList(CmupPlaylist).init(executor.allocator);

        _ = &result;

        std.debug.print("{}\n", .{std.json.fmt(executor.ast, .{ .whitespace = .indent_2 })});

        return result;
    }
};
