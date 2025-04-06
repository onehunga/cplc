const std = @import("std");
const Parser = @import("Parser.zig");
const Table = @import("ast/Table.zig");
const Lexer = @import("Lexer.zig");
const Collect = @import("sema/Collect.zig");
const Module = @import("ast/Module.zig");
const type_interner = @import("ast/type_interner.zig");

var gpa: std.mem.Allocator = undefined;
var modules: std.ArrayListUnmanaged(*Module) = .empty;
var module_refs: std.StringHashMapUnmanaged(u32) = .empty;

/// initilize the compiler state
pub fn init(alloc: std.mem.Allocator) !void {
    gpa = alloc;

    try type_interner.initialize(gpa);
}

/// deinitilize the compiler state
pub fn deinit() void {
    type_interner.free();

    {
        for (modules.items) |module| {
            module.deinit(gpa);

            gpa.destroy(module);
        }
    }

    modules.deinit(gpa);
    module_refs.deinit(gpa);
}

pub fn compileAll() !void {
    for (modules.items) |module| {
        try parseModule(module);
    }
}

pub fn addModule(path: []const u8) !void {
    if (module_refs.contains(path)) {
        return;
    }

    const ref: u32 = @truncate(modules.items.len);
    const module = try gpa.create(Module);
    module.* = try .init(gpa, path);
    try modules.append(gpa, module);
    try module_refs.put(gpa, path, ref);
}

fn parseModule(module: *Module) !void {
    std.log.debug("parsing dir: {s}", .{module.path});
    var dir = try std.fs.openDirAbsolute(module.path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();

    var root: std.ArrayListUnmanaged(Table.Symbol) = .empty;
    defer root.deinit(gpa);
    while (try it.next()) |file| {
        if (file.kind != .file) continue;
        std.log.debug("parsing file: {s}", .{file.name});

        const source = try dir.readFileAlloc(gpa, file.name, 30_000);
        var toks = try Lexer.lex(source, gpa);
        defer toks.deinit(gpa);
        const ast = try Parser.parse(gpa, toks, source);

        try Collect.collect(&ast, gpa, &module.table, &root);

        try module.addFile(gpa, source, ast);
    }
    // add  the root scope
    {
        const len = root.items.len;
        const first = module.table.symbols.items.len;
        try module.table.symbols.appendSlice(gpa, root.items);

        module.table.scopes.items[0] = .{
            .parent = null,
            .symbols = .{
                .start = @truncate(first),
                .len = @truncate(len),
            },
        };
    }

    printSymbols(&module.table, 0);
}

fn printSymbols(table: *const Table, scope: Table.Scope.Id) void {
    const symbols = table.getSymbols(table.scopes.items[scope].symbols);

    for (symbols) |sym| {
        printSymbol(table, sym);
    }
}

fn printSymbol(table: *const Table, symbol: Table.Symbol) void {
    std.debug.print("{s}: {}\n", .{ symbol.name, symbol.tag });

    switch (symbol.tag) {
        .@"struct" => {
            printSymbols(table, symbol.data.@"struct".body);
        },
        .func => {
            printSymbols(table, symbol.data.func.args);
            printSymbols(table, symbol.data.func.body);
        },
        .field, .@"var" => {},
    }
}
