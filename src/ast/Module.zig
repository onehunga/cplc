const Self = @This();
const std = @import("std");

const Table = @import("Table.zig");
const Ast = @import("../Ast.zig");

pub const File = struct {
    source: []const u8,
    ast: Ast,
};

path: []const u8,
files: std.ArrayListUnmanaged(File),
table: Table,

/// path must be allocated as most paths won't be known at compile time or stored elsewhere
pub fn init(gpa: std.mem.Allocator, path: []const u8) !Self {
    return .{
        .path = path,
        .files = .empty,
        .table = try .init(gpa),
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    alloc.free(self.path);

    for (self.files.items) |*file| {
        alloc.free(file.source);

        file.ast.deinit(alloc);
    }

    self.files.deinit(alloc);
    self.table.deinit(alloc);
}

pub fn addFile(self: *Self, alloc: std.mem.Allocator, source: []const u8, ast: Ast) !void {
    const file = File{
        .source = source,
        .ast = ast,
    };

    try self.files.append(alloc, file);
}
