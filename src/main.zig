const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Table = @import("ast/Table.zig");
const collectSymbols = @import("sema/Collect.zig").collect;
const solve_types = @import("sema/solve_types.zig");
const compiler = @import("compiler.zig");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();

    try compiler.init(gpa.allocator());
    defer compiler.deinit();

    {
        const test_root_dir = try std.fs.cwd().realpathAlloc(gpa.allocator(), "test");
        try compiler.addModule(test_root_dir);

        try compiler.compileAll();
    }

    // try solve_types.collectTypes(&ast, gpa.allocator(), &table);
    // var ctx = try solve_types.solveTypes(gpa.allocator(), &ast, &table);
    // defer ctx.free(gpa.allocator());

    // try ast.dump(std.io.getStdOut().writer());
    // try ast.prettyPrint(std.io.getStdOut().writer());
}
