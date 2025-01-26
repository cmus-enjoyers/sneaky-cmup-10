// TODO: implement abstract syntax tree
const Lexer = @import("lexer.zig").Lexer;
const std = @import("std");

pub const NodeType = enum {
    RequireStatement,
    AddStatement,
};

pub const RequireData = struct {
    sources: [][]const u8,
};

pub const AddData = struct {
    source: []const u8,
};

const NodeData = union(enum) {
    RequireStatement: RequireData,
    AddStatement: AddData,
};

pub const ASTNode = struct {
    type: NodeType,
    data: NodeData,
};

pub const Parser = struct {
    lexer: *Lexer,
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(ASTNode),

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        return Parser{
            .lexer = lexer,
            .allocator = allocator,
            .nodes = std.ArrayList(ASTNode).init(allocator),
        };
    }

    pub fn deinit(parser: *Parser) void {
        parser.nodes.deinit();
    }
};
