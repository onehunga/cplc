const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @import("Parser.zig");
const Table = @import("ast/Table.zig");
const Lexer = @import("Lexer.zig");
const Collect = @import("sema/Collect.zig");
const Module = @import("ast/Module.zig");
const solve_types = @import("sema/solve_types.zig");
const type_interner = @import("ast/type_interner.zig");

pub const CompileError = std.mem.Allocator.Error || std.fs.File.OpenError || std.fs.File.SeekError || std.fs.File.ReadError || Parser.ParseError;

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

pub fn addModule(path: []const u8) !void {
    if (module_refs.contains(path)) {
        return;
    }

    const ref: u32 = @truncate(modules.items.len);
    const module = try gpa.create(Module);
    module.* = try .init(gpa, path);
    try modules.append(gpa, module);
    try module_refs.put(gpa, path, ref);

    try parseModule(module);
}

pub fn collectAllModules() !void {
    for (modules.items) |module| {
        for (module.files.items) |file| {
            try solve_types.collectTypes(&file.ast, gpa, &module.table);
        }
    }

    for (modules.items) |module| {
        for (module.files.items) |file| {
            var tc = try solve_types.solveTypes(gpa, &file.ast, &module.table);
            defer tc.free(gpa);
        }
    }
}

pub fn getModuleRelative(module: u32, relative: []const u8) !u32 {
    const m = modules.items[module];
    const relative_path = try std.fs.path.resolve(gpa, &.{ m.path, relative });
    defer gpa.free(relative_path);

    return module_refs.get(relative_path).?;
}

pub fn getSymbolTable(module: u32) *Table {
    const m = modules.items[module];
    return &m.table;
}

pub fn debugPrintAll() !void {
    var target = try std.fs.cwd().makeOpenPath("target", .{});
    defer target.close();

    for (modules.items, 0..) |module, idx| {
        const module_name = try std.fmt.allocPrint(gpa, "m{}", .{idx});
        defer gpa.free(module_name);
        var mod_dir = try target.makeOpenPath(module_name, .{});
        defer mod_dir.close();

        for (module.files.items, 0..) |file, fidx| {
            const file_name = try std.fmt.allocPrint(gpa, "{}.ast", .{fidx});
            defer gpa.free(file_name);

            var f = try mod_dir.createFile(file_name, .{});
            defer f.close();

            try file.ast.prettyPrint(f.writer());
        }
    }
}

fn parseModule(module: *Module) !void {
    std.log.debug("parsing dir: {s}", .{module.path});
    var dir = try std.fs.openDirAbsolute(module.path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();

    var root: std.ArrayListUnmanaged(Table.Symbol) = .empty;
    defer root.deinit(gpa);
    while (try it.next()) |file| {
        if (file.kind != .file) {
            continue;
        }
        std.log.debug("parsing file: {s}", .{file.name});

        const source = try dir.readFileAlloc(gpa, file.name, 30_000);
        var toks = try Lexer.lex(source, gpa);
        defer toks.deinit(gpa);
        const ast = try Parser.parse(gpa, toks, source);

        try handleImports(module, &ast);

        try Collect.collect(&ast, module_refs.get(module.path).?, gpa, &module.table, &root);

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

    // printSymbols(&module.table, 0);
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
        .import => {},
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

fn handleImports(module: *const Module, ast: *const Ast) CompileError!void {
    var imports = try ast.collectImports(gpa);
    defer imports.deinit(gpa);

    if (imports.items.len == 0) {
        return;
    }

    const data = ast.nodes.items(.data);
    for (imports.items) |import| {
        const relative_path = ast.literals.items[data[import].lhs];

        const absolute_path = try std.fmt.allocPrint(gpa, "{s}/{s}", .{
            module.path,
            relative_path,
        });

        try addModule(absolute_path);
    }
}
