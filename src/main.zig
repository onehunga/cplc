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

    // try ast.dump(std.io.getStdOut().writer());
    try ast.prettyPrint(std.io.getStdOut().writer());
}
