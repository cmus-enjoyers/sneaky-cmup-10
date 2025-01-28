const std = @import("std");
const CmupPlaylist = @import("../cmup/cmup.zig").CmupPlaylist;
const Ast = @import("ast.zig");
const NodeType = Ast.NodeType;
const ASTNode = Ast.ASTNode;

pub const Executor = struct {
    playlists: std.StringHashMap(CmupPlaylist),
    ast: []ASTNode,
    allocator: std.mem.Allocator,
    identifiers: std.StringHashMap(CmupPlaylist),

    pub fn init(allocator: std.mem.Allocator, playlists: std.StringHashMap(CmupPlaylist), ast: []ASTNode) Executor {
        return Executor{
            .playlists = playlists,
            .allocator = allocator,
            .identifiers = std.StringHashMap(CmupPlaylist).init(allocator),
            .ast = ast,
        };
    }

    pub fn deinit(executor: *Executor) void {
        executor.identifiers.deinit();
    }

    pub fn executeRequire(executor: *Executor, data: Ast.RequireData) !void {
        for (data.sources) |source| {
            if (executor.playlists.get(source)) |playlist| {
                try executor.identifiers.put(source, playlist);

                continue;
            }

            return error.NoPlaylist;
        }
    }

    pub fn execute(executor: *Executor) !std.ArrayList(CmupPlaylist) {
        var result = std.ArrayList(CmupPlaylist).init(executor.allocator);

        for (executor.ast) |node| {
            try switch (node.data) {
                NodeType.RequireStatement => |data| executor.executeRequire(data),
                NodeType.AddStatement => {},
            };
        }

        _ = &result;

        var iter = executor.identifiers.iterator();

        while (iter.next()) |next| {
            std.debug.print(
                "{s} = {}\n",
                .{
                    next.key_ptr.*,
                    std.json.fmt(next.value_ptr, .{ .whitespace = .indent_2 }),
                },
            );
        }

        return result;
    }
};
