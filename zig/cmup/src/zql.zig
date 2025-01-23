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
    Indentifier,
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

    pub fn init(input: []const u8, allocator: std.mem.Allocator) Lexer {
        return Lexer{
            .input = input,
            .position = 0,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn getTokenType(lexeme: []const u8) TokenType {
        if (std.mem.eql(u8, lexeme, "require")) {
            return TokenType.Require;
        }

        return TokenType.Indentifier;
    }

    pub fn nextToken(lexer: *Lexer) !Token {
        while (std.ascii.isWhitespace(lexer.input[lexer.position])) {
            lexer.position += 1;
        }

        const start = lexer.position;

        while (!std.ascii.isWhitespace(lexer.input[lexer.position])) {
            lexer.position += 1;
        }

        const token = Token{
            .type = Lexer.getTokenType(lexer.input[start..lexer.position]),
            .lexeme = lexer.input[start..lexer.position],
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

    _ = try lexer.nextToken();
    _ = try lexer.nextToken();

    for (lexer.tokens.items) |token| {
        token.print();
    }

    std.debug.print("\n\n\n{any}", .{lexer.tokens.items});
}
