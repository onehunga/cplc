const std = @import("std");
const Parser = @import("Parser.zig");
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
    module.* = .init(path);
    try modules.append(gpa, module);
    try module_refs.put(gpa, path, ref);
}

fn parseModule(module: *Module) !void {
    std.log.debug("parsing dir: {s}", .{module.path});
    var dir = try std.fs.openDirAbsolute(module.path, .{ .iterate = true });
    defer dir.close();

    var it = dir.iterate();
    while (try it.next()) |file| {
        if (file.kind != .file) continue;
        std.log.debug("parsing file: {s}", .{file.name});

        const source = try dir.readFileAlloc(gpa, file.name, 30_000);
        var toks = try Lexer.lex(source, gpa);
        defer toks.deinit(gpa);
        const ast = try Parser.parse(gpa, toks, source);

        const table = Collect.collect(&ast, gpa);

        try module.addFile(gpa, source, ast, table);
    }
}
