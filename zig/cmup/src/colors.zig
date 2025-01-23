pub const reset = "\x1b[0m";
pub const green = "\x1b[32m";
pub const dim = "\x1b[2m";

pub fn dim_text(comptime text: []const u8) []const u8 {
    return dim ++ text ++ reset;
}

pub fn green_text(comptime text: []const u8) []const u8 {
    return green ++ text ++ reset;
}
