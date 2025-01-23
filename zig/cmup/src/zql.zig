const std = @import("std");

pub const TokenType = enum {
    Require,
    Indentifier,
    EOL,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,

    pub fn print(self: Token) void {
        std.debug.print("{s}", .{@tagName(self.type)});
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

    pub fn nextToken(lexer: *Lexer) !Token {
        const token = Token{
            .type = .Require,
            .lexeme = "require",
        };

        lexer.position += 1;

        try lexer.tokens.append(token);

        return token;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var lexer = Lexer.init("", allocator);

    _ = try lexer.nextToken();

    for (lexer.tokens.items) |token| {
        token.print();
    }
}
