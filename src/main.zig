const std = @import("std");
const lex = @import("lex.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), "test/main.cpl", 30_000);
    defer gpa.allocator().free(source);

    var tokens = try lex.lex(source, gpa.allocator());
    defer tokens.deinit(gpa.allocator());

    for (0..tokens.len) |i| {
        const token = tokens.get(i);

        std.debug.print("{} ({}..{}) '{s}'\n", .{
            token.tag,
            token.location.start,
            token.location.end,
            source[token.location.start..token.location.end],
        });
    }
}
