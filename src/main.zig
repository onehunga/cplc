const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();

    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), "test/main.cpl", 30_000);
    defer gpa.allocator().free(source);

    var tokens = try Lexer.lex(source, gpa.allocator());
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
                .ident => print(" '{s}'", .{ast.literals.items[data[idx].lhs]}),
                .int => {
                    var bits: u64 = data[idx].rhs;
                    bits |= @as(u64, @intCast(data[idx].lhs)) << 32;

                    print(" '{}'", .{bits});
                },
                .float => {
                    var bits: u64 = data[idx].rhs;
                    bits |= @as(u64, @intCast(data[idx].lhs)) << 32;
                    const float: f64 = @bitCast(bits);

                    print(" '{}'", .{float});
                },
                else => {},
            }
        }
    }
}
