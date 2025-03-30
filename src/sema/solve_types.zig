const Self = @This();
const std = @import("std");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Table = @import("../ast/Table.zig");
const Type = @import("../ast/Type.zig");
const type_interner = @import("../ast/type_interner.zig");

pub const TypeContext = struct {};

const BUILTIN_TYPES = std.StaticStringMap(Type.Id).initComptime(.{
    .{ "void", Type.builtin.VOID },
    .{ "bool", Type.builtin.BOOL },
    .{ "u8", Type.builtin.U8 },
    .{ "u16", Type.builtin.U16 },
    .{ "u32", Type.builtin.U32 },
    .{ "u64", Type.builtin.U64 },
    .{ "s8", Type.builtin.S8 },
    .{ "s16", Type.builtin.S16 },
    .{ "s32", Type.builtin.S32 },
    .{ "s64", Type.builtin.S64 },
    .{ "f32", Type.builtin.F32 },
    .{ "f64", Type.builtin.F64 },
});

alloc: std.mem.Allocator,
ast: *const Ast,
tags: []Node.Tag,
data: []Node.Data,
table: *Table,

current_scope: Table.Scope.Id = 0,

pub fn collectTypes(ast: *const Ast, alloc: std.mem.Allocator, table: *Table) !void {
    var self: Self = .{
        .alloc = alloc,
        .ast = ast,
        .tags = ast.nodes.items(.tag),
        .data = ast.nodes.items(.data),
        .table = table,
    };

    for (table.symbols) |sym| {
        try self.collectType(&sym);
    }
}

fn collectType(self: *Self, sym: *const Table.Symbol) !void {
    return switch (sym.tag) {
        .@"struct" => self.collectStructType(sym.ref),
        else => {},
    };
}

fn collectStructType(self: *Self, idx: usize) !void {
    const fields = self.data[idx];
    const name = self.ast.literals.items[self.data[fields.lhs].lhs];

    var struct_fields: std.ArrayListUnmanaged(Type.StructField) = .empty;
    defer struct_fields.deinit(self.alloc);
    const sym = self.table.lookupSymbol(self.current_scope, name).?;

    const s = self.current_scope;
    defer self.current_scope = s;
    self.current_scope = sym.data.@"struct".body;

    for (fields.lhs + 1..fields.rhs) |node_ptr| {
        if (self.tags[node_ptr] != .field_decl) {
            continue;
        }
        const field_data = self.data[node_ptr];
        const field_name = self.ast.literals.items[self.data[field_data.lhs].lhs];

        const field: Type.StructField = .{
            .name = field_name,
            .ty = self.solveType(field_data.rhs).?,
        };
        try struct_fields.append(self.alloc, field);
    }

    const ty: Type = .{
        .tag = .@"struct",
        .data = .{
            .@"struct" = .{
                .name = name,
                .fields = try struct_fields.toOwnedSlice(self.alloc),
            },
        },
    };
    type_interner.setType(self.table.lookupSymbol(self.current_scope, name).?.data.@"struct".ty, ty);
}

fn solveType(self: *Self, node_ptr: usize) ?Type.Id {
    return switch (self.tags[node_ptr]) {
        .ident => {
            const name = self.ast.literals.items[self.data[node_ptr].lhs];
            const sym = self.table.lookupSymbol(self.current_scope, name) orelse return self.solveBuiltinType(node_ptr);

            return switch (sym.tag) {
                .@"struct" => sym.data.@"struct".ty,
                else => null,
            };
        },
        .tuple => self.solveTupleType(node_ptr),
        .slice => self.solveSliceType(node_ptr),
        else => null,
    };
}

fn solveTupleType(self: *Self, node_ptr: usize) ?Type.Id {
    const data = self.data[node_ptr];

    var tuple_fields: std.ArrayListUnmanaged(Type.Id) = .empty;

    for (data.lhs..data.rhs) |field_ptr| {
        const field_ty = self.solveType(field_ptr).?;
        tuple_fields.append(self.alloc, field_ty) catch unreachable;
    }

    const ty: Type = .{
        .tag = .tuple,
        .data = .{
            .tuple = .{
                .fields = tuple_fields.toOwnedSlice(self.alloc) catch unreachable,
            },
        },
    };

    return type_interner.getOrPut(ty) catch unreachable;
}

fn solveSliceType(self: *Self, node_ptr: usize) ?Type.Id {
    const data = self.data[node_ptr];

    const ty: Type = .{
        .tag = .slice,
        .data = .{
            .slice = .{
                .element = self.solveType(data.lhs).?,
            },
        },
    };

    return type_interner.getOrPut(ty) catch unreachable;
}

fn solveBuiltinType(self: *Self, node_ptr: usize) ?Type.Id {
    return switch (self.tags[node_ptr]) {
        .ident => {
            const name = self.ast.literals.items[self.data[node_ptr].lhs];
            return BUILTIN_TYPES.get(name);
        },
        else => Type.builtin.UNKNOWN,
    };
}
