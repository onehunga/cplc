const Self = @This();
const std = @import("std");
const builtin = @import("builtin");
const Type = @import("Type.zig");

var self: Self = undefined;

alloc: std.mem.Allocator,
types: std.ArrayListUnmanaged(Type),

pub fn initialize(alloc: std.mem.Allocator) !void {
    self.alloc = alloc;
    self.types = .empty;

    try fillBuiltinTypes();
}

pub fn free() void {
    for (self.types.items) |ty| {
        freeTypeData(ty);
    }

    self.types.deinit(self.alloc);
}

/// reserve a type, that gets filled out later
pub fn reserveType() !Type.Id {
    return addType(.{ .tag = .unknown, .data = .empty });
}

pub fn setType(id: Type.Id, ty: Type) void {
    if (builtin.mode == .Debug) {
        if (id.id == 0) {
            @panic("cannot set type for unknown type");
        }

        if (id.id >= self.types.items.len) {
            @panic("type id out of bounds");
        }

        if (self.types.items[id.id].tag != .unknown) {
            @panic("type already set");
        }
    }

    self.types.items[id.id] = ty;
}

pub fn getOrPut(ty: Type) !Type.Id {
    if (findType(ty)) |id| {
        freeTypeData(ty);
        return id;
    }

    return addType(ty);
}

fn fillBuiltinTypes() !void {
    _ = try addType(.{ .tag = .unknown, .data = .empty });
    _ = try addType(.{ .tag = .void, .data = .empty });
    _ = try addType(.{ .tag = .bool, .data = .empty });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = false, .bits = 8 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = false, .bits = 16 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = false, .bits = 32 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = false, .bits = 64 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = true, .bits = 8 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = true, .bits = 16 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = true, .bits = 32 } } });
    _ = try addType(.{ .tag = .int, .data = .{ .int = .{ .signed = true, .bits = 64 } } });
    _ = try addType(.{ .tag = .float, .data = .{ .float = .{ .bits = 32 } } });
    _ = try addType(.{ .tag = .float, .data = .{ .float = .{ .bits = 64 } } });
}

fn addType(sym: Type) !Type.Id {
    const id: Type.Id = .init(@truncate(self.types.items.len));

    try self.types.append(self.alloc, sym);

    return id;
}

fn findType(ty: Type) ?Type.Id {
    for (self.types.items, 0..) |sym, i| {
        if (Type.equals(sym, ty)) {
            return .init(@truncate(i));
        }
    }

    return null;
}

fn freeTypeData(ty: Type) void {
    switch (ty.tag) {
        .@"struct" => {
            self.alloc.free(ty.data.@"struct".fields);
        },
        .tuple => {
            self.alloc.free(ty.data.tuple.fields);
        },
        else => {},
    }
}
