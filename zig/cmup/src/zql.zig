// Example of a ZQL query:
//
// require playlists jump-bangers, vktrenokh-stwv
//
// add all from jump-bangers and add all from vktrenokh-stwv where name contains 'voj'
//

// The name of the new playlist should be derived from the ZQL file name
// (e.g., "vktrenokh-eurobeat.zql" => "vktrenokh-eurobeat").
//
// The `require` statement specifies which playlists will be used to create the new playlist.
// - If a playlist is referenced later in the query but is not defined using `require`,
//   the query should terminate with an error.
// - If a playlist is defined in the `require` statement but is not found by the `zmup` program,
//   the query should also terminate with an error.
//
// String literals should only be created using single quotes ('').
// The single quote character (') within a string can be escaped using the backslash (\) character.

const std = @import("std");
const colors = @import("colors.zig");

pub const TokenType = enum {
    Require,
    Identifier,
    String,
    EOL,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,

    pub fn print(self: Token) void {
        std.debug.print(
            colors.green_text("{s}") ++ colors.dim_text(":") ++ " {s}; ",
            .{ @tagName(self.type), self.lexeme },
        );
    }
};

const Lexer = struct {
    input: []const u8,
    position: usize,
    tokens: std.ArrayList(Token),
    allocator: std.mem.Allocator,

    pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
        return Lexer{ .input = input, .position = 0, .tokens = std.ArrayList(Token).init(allocator), .allocator = allocator };
    }

    pub inline fn getCurrentSymbol(lexer: *Lexer) u8 {
        return lexer.input[lexer.position];
    }

    pub fn getTokenType(lexeme: []const u8) TokenType {
        if (std.mem.eql(u8, lexeme, "require")) {
            return TokenType.Require;
        }

        return TokenType.Identifier;
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

    pub fn nextToken(lexer: *Lexer) !?Token {
        if (lexer.position == lexer.input.len - 1) {
            return null;
        }

        while (std.ascii.isWhitespace(lexer.getCurrentSymbol())) {
            lexer.position += 1;
        }

        const start = lexer.position;

        const isString = lexer.input[start] == '\'';

        if (isString) {
            lexer.position += 1;
        }

        const stderr = std.io.getStdErr();

        while (lexer.shouldConsume(isString)) {
            if (isString) {
                if (lexer.position == lexer.input.len - 1) {
                    const message = try std.fmt.allocPrint(
                        lexer.allocator,
                        colors.red_text("Error") ++ colors.dim_text(" => ") ++ "Unterminated string at {}",
                        .{lexer.position},
                    );

                    try stderr.writeAll(message);

                    return error.UnterminatedString;
                }
            }

            lexer.position += 1;
        }

        const lexeme = lexer.input[start..lexer.position];

        const token = Token{
            .type = if (isString) TokenType.String else Lexer.getTokenType(lexeme),
            .lexeme = lexeme,
        };

        try lexer.tokens.append(token);

        return token;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init(@embedFile("./test.zql"), allocator);

    while (lexer.nextToken() catch return) |val| {
        _ = val;
    }

    for (lexer.tokens.items) |token| {
        token.print();
    }

    std.debug.print("\n\n\n{any}", .{lexer.tokens.items});
}
