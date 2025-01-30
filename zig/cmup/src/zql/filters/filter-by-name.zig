const std = @import("std");
const CmupPlaylist = @import("../../cmup/cmup.zig").CmupPlaylist;
const Filter = @import("../ast.zig").Filter;
const path = @import("../../utils/path.zig");

pub fn filterByName(result: *std.ArrayList([]const u8), playlist: CmupPlaylist, filter: Filter) !void {
    for (playlist.content) |track| {
        const name = path.getFileNameWithoutExtension(track);

        switch (filter.match_type) {
            .Contains => {
                if (std.ascii.indexOfIgnoreCase(name, filter.target) != null) {
                    try result.append(track);
                }
            },
            .Is => {
                if (std.mem.eql(u8, name, filter.target)) {
                    try result.append(track);
                }
            },
        }
    }
}
