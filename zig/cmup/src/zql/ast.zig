// TODO: implement abstract syntax tree
const lxer = @import("lexer.zig");
const Lexer = lxer.Lexer;
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
    position: usize,
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(ASTNode),

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        return Parser{
            .lexer = lexer,
            .allocator = allocator,
            .nodes = std.ArrayList(ASTNode).init(allocator),
            .position = 0,
        };
    }

    pub fn deinit(parser: *Parser) void {
        parser.nodes.deinit();
    }

    pub fn peekNextToken(parser: *Parser) ?lxer.Token {
        if (parser.position + 1 >= parser.lexer.tokens.items.len - 1) {
            return null;
        }

        return parser.lexer.tokens.items[parser.position + 1];
    }

    pub fn parseRequire(parser: *Parser) !void {
        // TODO: fix memory leak here later
        var sources = std.ArrayList([]const u8).init(parser.allocator);

        while (parser.peekNextToken()) |value| {
            if (value.type != .Identifier) {
                std.debug.print("break\n", .{});
                break;
            }

            parser.position += 1;

            try sources.append(value.lexeme);
        }

        const node = ASTNode{
            .type = .RequireStatement,
            .data = .{
                .RequireStatement = .{
                    .sources = sources.items,
                },
            },
        };

        try parser.nodes.append(node);
    }

    pub fn parse(parser: *Parser) !void {
        while (parser.position < parser.lexer.tokens.items.len) : ({
            parser.position += 1;
        }) {
            const item = parser.lexer.tokens.items[parser.position];

            try switch (item.type) {
                .Require => parser.parseRequire(),
                else => std.debug.print("unknown token\n", .{}),
            };
        }
    }
};
