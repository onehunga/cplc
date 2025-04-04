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

    // printSymbols(&table, 0);
    // try solve_types.collectTypes(&ast, gpa.allocator(), &table);
    // var ctx = try solve_types.solveTypes(gpa.allocator(), &ast, &table);
    // defer ctx.free(gpa.allocator());

    // try ast.dump(std.io.getStdOut().writer());
    // try ast.prettyPrint(std.io.getStdOut().writer());
}

// fn printSymbols(table: *const Table, scope: Table.Scope.Id) void {
//     const symbols = table.getSymbols(table.scopes[scope].symbols);

//     for (symbols) |sym| {
//         printSymbol(table, sym);
//     }
// }

// fn printSymbol(table: *const Table, symbol: Table.Symbol) void {
//     std.debug.print("{s}: {}\n", .{ symbol.name, symbol.tag });

//     switch (symbol.tag) {
//         .@"struct" => {
//             printSymbols(table, symbol.data.@"struct".body);
//         },
//         .func => {
//             printSymbols(table, symbol.data.func.args);
//             printSymbols(table, symbol.data.func.body);
//         },
//         .field, .@"var" => {},
//     }
// }
