const std = @import("std");
const lex = @import("lex.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), "test/main.cpl", 30_000);
    defer gpa.allocator().free(source);

    var tokens = try lex.lex(source, gpa.allocator());
    defer tokens.deinit(gpa.allocator());

    var ast = try Parser.parse(gpa.allocator(), tokens, source);
    defer ast.deinit(gpa.allocator());

    {
        const print = std.debug.print;
        const data = ast.nodes.items(.data);

        for (ast.nodes.items(.tag), 0..) |tag, idx| {
            defer print("\n", .{});
            print("{}: {}({}, {})", .{ idx, tag, data[idx].lhs, data[idx].rhs });

            switch (tag) {
                .ident => print(" '{s}'", .{ast.literals.items[ast.nodes.get(idx).data.lhs]}),
                else => {},
            }
        }
    }
}
