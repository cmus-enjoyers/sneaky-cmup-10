const std = @import("std");
const colors = @import("../utils/colors.zig");
const err = @import("error.zig");

pub const TokenType = enum {
    Require,
    Identifier,
    String,
    EOL,
    Add,
    All,
    From,
    Where,
    Contains,

    Unknown, // remove in future
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,

    pub fn print(self: Token) void {
        const token_type_fmt = comptime colors.green_text("{s}");
        const token_type = @tagName(self.type);

        if (self.lexeme.len > 0) {
            std.debug.print(
                token_type_fmt ++ colors.dim_text(":") ++ " {s}; ",
                .{ token_type, self.lexeme },
            );
        } else {
            std.debug.print(
                token_type_fmt ++ ";",
                .{token_type},
            );
        }
    }
};

const ContextType = enum {
    Require,
};

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    tokens: std.ArrayList(Token),
    context_stack: std.ArrayList(ContextType),
    allocator: std.mem.Allocator,
    line_position: usize,
    line: usize,

    pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .input = input,
            .position = 0,
            .tokens = std.ArrayList(Token).init(allocator),
            .context_stack = std.ArrayList(ContextType).init(allocator),
            .allocator = allocator,
            .line_position = 0,
            .line = 1,
        };
    }

    pub fn pushContext(lexer: *Lexer, context: ContextType) !void {
        try lexer.context_stack.append(context);
    }

    pub fn popContext(lexer: *Lexer) ContextType {
        return lexer.context_stack.pop();
    }

    pub fn peekContext(lexer: *Lexer) ?ContextType {
        if (lexer.context_stack.items.len > 0) {
            return lexer.context_stack.items[lexer.context_stack.items.len - 1];
        }

        return null;
    }

    pub inline fn getCurrentSymbol(lexer: *Lexer) u8 {
        return lexer.input[lexer.position];
    }

    pub fn getLastToken(lexer: Lexer) ?Token {
        return lexer.tokens.items[lexer.tokens.items.len - 1];
    }

    pub fn getTokenType(lexer: *Lexer, lexeme: []const u8) TokenType {
        if (std.mem.eql(u8, lexeme, "require")) {
            // FIX: fix this later
            lexer.pushContext(ContextType.Require) catch unreachable;
            return TokenType.Require;
        }

        if (std.mem.eql(u8, lexeme, "add")) {
            return TokenType.Add;
        }

        if (std.mem.eql(u8, lexeme, "from")) {
            return TokenType.From;
        }

        if (std.mem.eql(u8, lexeme, "all")) {
            return TokenType.All;
        }

        if (std.mem.eql(u8, lexeme, "where")) {
            return TokenType.Where;
        }

        if (std.mem.eql(u8, lexeme, "contains")) {
            return TokenType.Contains;
        }

        if (lexer.peekContext()) |context| {
            return switch (context) {
                .Require => TokenType.Identifier,
            };
        }

        return TokenType.Unknown;
    }

    pub fn shouldConsume(lexer: *Lexer, isString: bool) bool {
        if (isString) {
            if (lexer.getCurrentSymbol() != '\'') {
                return true;
            }

            lexer.position += 1;

            return false;
        }

        return !std.ascii.isWhitespace(lexer.getCurrentSymbol());
    }

    pub fn addEolToken(lexer: *Lexer) !Token {
        const token = Token{ .type = .EOL, .lexeme = "" };

        try lexer.tokens.append(token);

        return token;
    }

    pub fn nextToken(lexer: *Lexer) !?Token {
        if (lexer.position == lexer.input.len - 1) {
            return try lexer.addEolToken();
        }

        while (std.ascii.isWhitespace(lexer.getCurrentSymbol())) {
            lexer.position += 1;

            if (lexer.getCurrentSymbol() == '\n' and lexer.position != lexer.input.len - 1) {
                lexer.line_position = lexer.position;
                lexer.line += 1;
            }
        }

        const start = lexer.position;

        const is_string = lexer.input[start] == '\'';

        if (is_string) {
            lexer.position += 1;
        }

        const stderr = std.io.getStdErr();

        while (lexer.shouldConsume(is_string)) {
            if (is_string) {
                if (lexer.position == lexer.input.len - 1) {
                    try err.print(
                        lexer.allocator,
                        stderr,
                        err.Error.UnterminatedString,
                        lexer.line_position,
                        lexer.line,
                        lexer.input[start..lexer.position],
                        lexer.input,
                    );

                    return error.UnterminatedString;
                }
            }

            lexer.position += 1;

            if (lexer.getCurrentSymbol() == '\n' and lexer.position != lexer.input.len - 1) {
                lexer.line_position = lexer.position;
                lexer.line += 1;
            }
        }

        const lexeme = lexer.input[start..lexer.position];

        const token = Token{
            .type = if (is_string) TokenType.String else lexer.getTokenType(lexeme),
            .lexeme = lexeme,
        };

        try lexer.tokens.append(token);

        return token;
    }
};
