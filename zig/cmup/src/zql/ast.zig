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

// NOTE: move filtering things into separate module cuz they will grow
pub const MatchType = enum {
    Is,
    Contains,

    pub fn toMatchType(value: []const u8) !MatchType {
        if (std.mem.eql(u8, value, "is")) {
            return MatchType.Is;
        }

        if (std.mem.eql(u8, value, "contains")) {
            return MatchType.Contains;
        }

        return error.NotMatchType;
    }
};

pub const Filter = struct {
    field: []const u8,
    match_type: MatchType,
    target: []const u8,
};

pub const AddData = struct {
    source: []const u8,
    filters: ?[]Filter,
};

const NodeData = union(NodeType) {
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

    pub fn parseFilters(parser: *Parser) !?[]Filter {
        const nextToken = parser.peekNextToken();

        if (nextToken) |value| {
            if (value.type != .Where) {
                return null;
            }

            parser.move();

            // TODO: fix memory leaks here
            var filters = std.ArrayList(Filter).init(parser.allocator);

            const name = try parser.expectTokenType(value, .Identifier);
            const match_type = try parser.expectTokenType(name, .MatchType);
            const target = try parser.expectTokenType(match_type, .String);

            // TODO: impleemnt multiple filters parsing in lexer

            try filters.append(Filter{
                .field = name.lexeme,
                .match_type = try MatchType.toMatchType(match_type.lexeme),
                .target = target.lexeme,
            });

            return filters.items;
        }

        return null;
    }

    pub fn parseAdd(parser: *Parser, token: lxer.Token) !void {
        const next = try parser.expectTokenType(token, .All);
        const from = try parser.expectTokenType(next, .From);
        const source = try parser.expectTokenType(from, .Identifier);

        const filters = try parser.parseFilters();

        try parser.nodes.append(ASTNode{
            .type = .AddStatement,
            .data = .{
                .AddStatement = .{
                    .filters = filters,
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
                .Unknown => {
                    err.printSimple("UnknownSyntax", item.line);
                    return error.UnknownSyntax;
                },
                else => {},
            };
        }
    }
};
