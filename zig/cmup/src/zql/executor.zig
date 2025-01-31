// NOTE: move filtering logic into separate module cuz it wlll grow

const std = @import("std");
const path = @import("../utils/path.zig");
const CmupPlaylist = @import("../cmup/cmup.zig").CmupPlaylist;
const Ast = @import("ast.zig");
const Token = @import("lexer.zig").Token;
const NodeType = Ast.NodeType;
const ASTNode = Ast.ASTNode;
const filterByName = @import("filters/filter-by-name.zig").filterByName;
const err = @import("error.zig");

pub const Executor = struct {
    playlists: std.StringHashMap(CmupPlaylist),
    ast: []ASTNode,
    allocator: std.mem.Allocator,
    identifiers: std.StringHashMap(CmupPlaylist),
    stderr: std.fs.File,
    input: []const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        playlists: std.StringHashMap(CmupPlaylist),
        ast: []ASTNode,
        stderr: std.fs.File,
        input: []const u8,
    ) Executor {
        return Executor{
            .playlists = playlists,
            .allocator = allocator,
            .identifiers = std.StringHashMap(CmupPlaylist).init(allocator),
            .ast = ast,
            .stderr = stderr,
            .input = input,
        };
    }

    pub fn deinit(executor: *Executor) void {
        executor.identifiers.deinit();
    }

    pub fn printErr(executor: *Executor, token: Token, er: err.Error) !void {
        try token.printErr(
            executor.allocator,
            executor.stderr,
            er,
            executor.input,
        );
    }

    pub fn executeRequire(executor: *Executor, data: Ast.RequireData) !void {
        for (data.sources) |source| {
            const key = source.lexeme;

            if (executor.playlists.get(key)) |playlist| {
                try executor.identifiers.put(key, playlist);

                continue;
            }

            try executor.printErr(source, err.Error.PlaylistNotFound);
            return error.NoPlaylist;
        }
    }

    pub fn filterPlaylist(executor: *Executor, filters: []Ast.ASTFilter, playlist: CmupPlaylist) !std.ArrayList([]const u8) {
        var result = std.ArrayList([]const u8).init(executor.allocator);

        for (filters) |filter| {
            if (std.mem.eql(u8, filter.data.field.lexeme, "name")) {
                try filterByName(&result, playlist, filter.data);
            }
        }

        return result;
    }

    pub fn executeAdd(executor: *Executor, result: *std.ArrayList([]const u8), data: Ast.AddData) !void {
        if (executor.identifiers.get(data.source.lexeme)) |value| {
            if (data.filters) |filters| {
                const tracks = try executor.filterPlaylist(filters, value);

                try result.appendSlice(tracks.items);

                return;
            }

            try result.appendSlice(value.content);

            return;
        }

        try executor.printErr(data.source, err.Error.ReferenceError);
        return error.ReferenceError;
    }

    pub fn execute(executor: *Executor, name: []const u8) !CmupPlaylist {
        var result = std.ArrayList([]const u8).init(executor.allocator);

        for (executor.ast) |node| {
            try switch (node.data) {
                NodeType.RequireStatement => |data| executor.executeRequire(data),
                NodeType.AddStatement => |data| executor.executeAdd(&result, data),
            };
        }

        return CmupPlaylist{
            .name = name,
            .content = result.items,
            .path = "",
            .sub_playlists = &[_]*CmupPlaylist{},
        };
    }
};
