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

    pub fn handleFilters() void {}

    pub fn executeAdd(executor: *Executor, result: *std.ArrayList([]const u8), data: Ast.AddData) !void {
        if (executor.identifiers.get(data.source)) |value| {
            if (data.filters) |filters| {
                _ = filters;
                return;
            }

            try result.appendSlice(value.content);

            return;
        }

        return error.ReferenceError;
    }

    pub fn execute(executor: *Executor) !CmupPlaylist {
        var result = std.ArrayList([]const u8).init(executor.allocator);

        for (executor.ast) |node| {
            try switch (node.data) {
                NodeType.RequireStatement => |data| executor.executeRequire(data),
                NodeType.AddStatement => |data| executor.executeAdd(&result, data),
            };
        }

        return CmupPlaylist{
            .name = "test",
            .content = result.items,
            .path = "",
            .sub_playlists = &[_]*CmupPlaylist{},
        };
    }
};
