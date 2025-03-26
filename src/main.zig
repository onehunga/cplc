const std = @import("std");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Table = @import("ast/Table.zig");
const collect = @import("sema/Collect.zig").collect;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();

    const source = try std.fs.cwd().readFileAlloc(gpa.allocator(), "test/main.cpl", 30_000);
    defer gpa.allocator().free(source);

    var tokens = try Lexer.lex(source, gpa.allocator());
    defer tokens.deinit(gpa.allocator());

    var ast = try Parser.parse(gpa.allocator(), tokens, source);
    defer ast.deinit(gpa.allocator());

    var table = collect(&ast, gpa.allocator());
    defer table.deinit(gpa.allocator());

    printSymbols(&table, table.root);

    // try ast.dump(std.io.getStdOut().writer());
    // try ast.prettyPrint(std.io.getStdOut().writer());
}

fn printSymbols(table: *const Table, symbols: []const Table.Symbol) void {
    for (symbols) |sym| {
        printSymbol(table, sym);
    }
}

fn printSymbol(table: *const Table, symbol: Table.Symbol) void {
    std.debug.print("{s}: {}\n", .{ symbol.name, symbol.tag });

    switch (symbol.tag) {
        .@"struct" => {
            printSymbols(table, table.getSymbols(symbol.data.@"struct".children));
        },
        .func => {
            printSymbols(table, table.getSymbols(symbol.data.func.args));
            printSymbols(table, table.getSymbols(symbol.data.func.body));
        },
        .field, .@"var" => {},
    }
}
