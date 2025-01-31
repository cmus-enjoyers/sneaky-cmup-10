const std = @import("std");
const colors = @import("../utils/colors.zig");

pub const Error = enum {
    UnterminatedString,
    SyntaxError,
};

pub fn getErrorMessage(err: Error) []const u8 {
    return switch (err) {
        .UnterminatedString => "Unterminated string",
        .SyntaxError => "Syntax error",
    };
}

const error_prefix = colors.red_text("Zql Error") ++ colors.dim_text(" => ");

// TODO: implement hints

pub fn printToken(
    allocator: std.mem.Allocator,
    out: std.fs.File,
    err: Error,
    line_position: usize,
    line: usize,
    lexeme: []const u8,
    input: []const u8,
) !void {
    const line_end = if (std.ascii.indexOfIgnoreCasePos(input, line_position + 1, "\n")) |value| value else input.len;

    const err_line = input[line_position..line_end];

    const index = std.ascii.indexOfIgnoreCase(err_line, lexeme) orelse {
        std.debug.print(error_prefix ++ "UnknownSyntax", .{});
        return error.UnknownSyntax;
    };

    const text = try colors.redUndercurledTextRuntime(allocator, lexeme);
    defer allocator.free(text);

    const content_before_invalid_text = try allocator.dupe(u8, err_line[0..index]);
    defer allocator.free(content_before_invalid_text);

    std.mem.replaceScalar(u8, content_before_invalid_text, '\n', 0);

    const msg = try std.mem.join(allocator, "", &[_][]const u8{
        content_before_invalid_text,
        text,
        err_line[index + lexeme.len ..],
    });
    defer allocator.free(msg);

    const message = try std.fmt.allocPrint(
        allocator,
        colors.red_text("Error") ++ colors.dim_text(" => ") ++ "{s} at line {}\n\n" ++ colors.dim_text("{} ▎ ") ++ "{s}\n",
        .{
            getErrorMessage(err),
            line,
            line,
            msg,
        },
    );
    defer allocator.free(message);

    try out.writeAll(message);
}
