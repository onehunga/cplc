const Self = @This();
const std = @import("std");

const Table = @import("Table.zig");
const Ast = @import("../Ast.zig");

pub const File = struct {
    source: []const u8,
    ast: Ast,
    table: Table,
};

path: []const u8,
files: std.ArrayListUnmanaged(File),

/// path must be allocated as most paths won't be known at compile time or stored elsewhere
pub fn init(path: []const u8) Self {
    return .{
        .path = path,
        .files = .empty,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    alloc.free(self.path);

    for (self.files.items) |*file| {
        alloc.free(file.source);

        file.ast.deinit(alloc);
        file.table.deinit(alloc);
    }

    self.files.deinit(alloc);
}

pub fn addFile(self: *Self, alloc: std.mem.Allocator, source: []const u8, ast: Ast, table: Table) !void {
    const file = File{
        .source = source,
        .ast = ast,
        .table = table,
    };

    try self.files.append(alloc, file);
}
