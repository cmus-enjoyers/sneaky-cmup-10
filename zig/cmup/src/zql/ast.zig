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

pub const Filter = struct {};

pub const AddData = struct {
    source: []const u8,
    filters: ?[]Filter,
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

    pub fn move(parser: *Parser) void {
        parser.position += 1;
    }

    pub fn printSyntaxError(parser: *Parser, token: lxer.Token) !void {
        try token.printErr(
            parser.allocator,
            std.io.getStdErr(),
            err.Error.SyntaxError,
            parser.lexer.input,
        );
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

            parser.move();

            try sources.append(value.lexeme);
        }

        if (sources.items.len == 0) {
            try parser.printSyntaxError(token);

            return error.SyntaxError;
        }

        try parser.nodes.append(ASTNode{
            .type = .RequireStatement,
            .data = .{
                .RequireStatement = .{
                    .sources = sources.items,
                },
            },
        });
    }

    pub fn expectTokenType(parser: *Parser, current_token: lxer.Token, expectedType: lxer.TokenType) !lxer.Token {
        const token = parser.peekNextToken() orelse {
            try parser.printSyntaxError(current_token);
            return error.SyntaxError;
        };

        if (token.type != expectedType) {
            try parser.printSyntaxError(token);
            return error.SyntaxError;
        }

        parser.move();

        return token;
    }

    pub fn parseAdd(parser: *Parser, token: lxer.Token) !void {
        const next = try parser.expectTokenType(token, .All);
        const from = try parser.expectTokenType(next, .From);
        const source = try parser.expectTokenType(from, .Identifier);

        // TODO: implement filtering nodes

        try parser.nodes.append(ASTNode{
            .type = .AddStatement,
            .data = .{
                .AddStatement = .{
                    .filters = null,
                    .source = source.lexeme,
                },
            },
        });
    }

    pub fn parse(parser: *Parser) !void {
        while (parser.position < parser.lexer.tokens.items.len) : ({
            parser.move();
        }) {
            const item = parser.lexer.tokens.items[parser.position];

            try switch (item.type) {
                .Require => parser.parseRequire(item),
                .Add => parser.parseAdd(item),
                else => {},
            };
        }
    }
};
