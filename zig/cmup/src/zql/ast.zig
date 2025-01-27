// TODO: implement abstract syntax tree
const lxer = @import("lexer.zig");
const Lexer = lxer.Lexer;
const std = @import("std");
const err = @import("error.zig");

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

    pub fn parseRequire(parser: *Parser, token: lxer.Token) !void {
        // TODO: fix memory leak here later
        var sources = std.ArrayList([]const u8).init(parser.allocator);

        while (parser.peekNextToken()) |value| {
            if (value.type != .Identifier) {
                break;
            }

            parser.position += 1;

            try sources.append(value.lexeme);
        }

        if (sources.items.len == 0) {
            try token.printErr(
                parser.allocator,
                std.io.getStdErr(),
                err.Error.SyntaxError,
                parser.lexer.input,
            );

            return error.SyntaxError;
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
                .Require => parser.parseRequire(item),
                else => std.debug.print("unknown token\n", .{}),
            };
        }
    }
};
