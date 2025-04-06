const Self = @This();
const std = @import("std");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Table = @import("../ast/Table.zig");
const type_interner = @import("../ast/type_interner.zig");

alloc: std.mem.Allocator,
ast: *const Ast,
tags: []Node.Tag,
data: []Node.Data,
table: *Table,
scratch: std.ArrayListUnmanaged(Table.Symbol),

current_scope: Table.Scope.Id = 0,

pub fn collect(ast: *const Ast, alloc: std.mem.Allocator, table: *Table, root_scope: *std.ArrayListUnmanaged(Table.Symbol)) !void {
    var self = Self{
        .alloc = alloc,
        .ast = ast,
        .tags = ast.nodes.items(.tag),
        .data = ast.nodes.items(.data),
        .table = table,
        .scratch = .empty,
    };
    defer self.scratch.deinit(alloc);

    const root = self.data[0];

    for (ast.nodes.items(.tag)[root.lhs..root.rhs], root.lhs..) |tag, idx| {
        self.collectNode(tag, idx);
    }

    table.symbols = self.table.symbols;
    table.scopes = self.table.scopes;
    try root_scope.appendSlice(alloc, self.scratch.items);
}

fn collectNode(self: *Self, tag: Node.Tag, idx: usize) void {
    switch (tag) {
        .root => unreachable,
        .struct_decl => self.collectStruct(idx),
        .field_decl => self.collectField(idx),
        .func_decl => self.collectFunction(idx),
        .func_param => self.collectParameter(idx),
        .var_decl, .typed_var_decl => self.collectVariable(idx),
        else => {},
    }
}

fn collectStruct(self: *Self, idx: usize) void {
    const data = self.data[idx];
    const name_idx = self.data[data.lhs].lhs;
    const name = self.ast.literals.items[name_idx];

    const start = self.scratch.items.len;

    for (data.lhs + 1..data.rhs) |node_ptr| {
        self.collectNode(self.tags[node_ptr], node_ptr);
    }

    const scope = self.addScope(.{
        .parent = self.current_scope,
        .symbols = self.addFromScratch(start),
    });

    const sym: Table.Symbol = .{
        .tag = .@"struct",
        .name = name,
        .data = .{
            .@"struct" = .{
                .ty = type_interner.reserveType() catch unreachable,
                .body = scope,
            },
        },
        .ref = @truncate(idx),
    };
    self.addToScratch(sym);
}

fn collectField(self: *Self, idx: usize) void {
    const data = self.data[idx];
    const name_idx = self.data[data.lhs].lhs;
    const name = self.ast.literals.items[name_idx];

    const sym: Table.Symbol = .{
        .tag = .field,
        .name = name,
        .data = .empty,
        .ref = @truncate(idx),
    };
    self.addToScratch(sym);
}

fn collectFunction(self: *Self, idx: usize) void {
    const data = self.data[idx];
    const name_idx = self.data[data.lhs].lhs;
    const name = self.ast.literals.items[name_idx];

    const start_args = self.scratch.items.len;
    const proto_data = self.data[data.lhs + 1];
    for (proto_data.lhs + 1..proto_data.rhs) |node_ptr| {
        self.collectNode(self.tags[node_ptr], node_ptr);
    }
    const args = self.addScope(.{
        .parent = self.current_scope,
        .symbols = self.addFromScratch(start_args),
    });

    const start_body = self.scratch.items.len;
    for (data.lhs + 2..data.rhs) |node_ptr| {
        self.collectNode(self.tags[node_ptr], node_ptr);
    }
    const body = self.addScope(.{
        .parent = args,
        .symbols = self.addFromScratch(start_body),
    });

    const sym: Table.Symbol = .{
        .tag = .func,
        .name = name,
        .data = .{
            .func = .{
                .args = args,
                .body = body,
            },
        },
        .ref = @truncate(idx),
    };
    self.addToScratch(sym);
}

fn collectParameter(self: *Self, idx: usize) void {
    const data = self.data[idx];
    const name_idx = self.data[data.lhs].lhs;
    const name = self.ast.literals.items[name_idx];

    const sym: Table.Symbol = .{
        .tag = .field,
        .name = name,
        .data = .empty,
        .ref = @truncate(idx),
    };
    self.addToScratch(sym);
}

fn collectVariable(self: *Self, idx: usize) void {
    const data = self.data[idx];
    const name_idx = self.data[data.lhs].lhs;
    const name = self.ast.literals.items[name_idx];

    const sym: Table.Symbol = .{
        .tag = .@"var",
        .name = name,
        .data = .empty,
        .ref = @truncate(idx),
    };
    self.addToScratch(sym);
}

fn addToScratch(self: *Self, symbol: Table.Symbol) void {
    self.scratch.append(self.alloc, symbol) catch unreachable;
}

fn addFromScratch(self: *Self, start: usize) Table.Symbols {
    const first = self.table.symbols.items.len;

    for (self.scratch.items[start..]) |symbol| {
        self.table.symbols.append(self.alloc, symbol) catch unreachable;
    }
    self.scratch.resize(self.alloc, start) catch unreachable;

    return .{
        .start = @truncate(first),
        .len = @truncate(self.table.symbols.items.len - first),
    };
}

fn addScope(self: *Self, scope: Table.Scope) Table.Scope.Id {
    self.table.scopes.append(self.alloc, scope) catch unreachable;
    return @truncate(self.table.scopes.items.len - 1);
}
