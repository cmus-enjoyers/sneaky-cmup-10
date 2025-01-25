// TODO: implement abstract syntax tree
const lexer = @import("lexer.zig");

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
