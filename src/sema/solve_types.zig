//! solves the types of the AST nodes
//!
//! for now it will also do the type checking

const Self = @This();
const std = @import("std");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Lexer = @import("../Lexer.zig");
const Location = Lexer.Token.Location;
const Table = @import("../ast/Table.zig");
const Type = @import("../ast/Type.zig");
const type_interner = @import("../ast/type_interner.zig");
const compiler = @import("../compiler.zig");
const Diagnostig = @import("../diagnostic/Diagnostic.zig");

pub const TypeContext = struct {
    types: []Type.Id,

    pub fn allocate(alloc: std.mem.Allocator, size: usize) !TypeContext {
        return .{
            .types = try alloc.alloc(Type.Id, size),
        };
    }

    pub fn free(self: TypeContext, alloc: std.mem.Allocator) void {
        alloc.free(self.types);
    }
};

const CodeSymbol = struct {
    name: []const u8,
    is_type: bool,
    ty: Type.Id,

    scope: Table.Scope.Id,
};

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
locs: []Lexer.Token.Location,
table: *Table,

current_scope: Table.Scope.Id = 0,

context: TypeContext,

/// the current resolved type
current_type: Type.Id = Type.builtin.UNKNOWN,
expected_type: Type.Id = Type.builtin.UNKNOWN,

expected_return: ?Type.Id = null,

symbols: std.ArrayListUnmanaged(CodeSymbol) = .empty,

source: []const u8,
diagnostic: Diagnostig,

pub fn collectTypes(ast: *const Ast, alloc: std.mem.Allocator, table: *Table) !void {
    var self: Self = .{
        .alloc = alloc,
        .ast = ast,
        .tags = ast.nodes.items(.tag),
        .data = ast.nodes.items(.data),
        .locs = ast.nodes.items(.loc),
        .table = table,
        .context = undefined,
        .source = undefined,
        .diagnostic = undefined,
    };

    for (table.symbols.items) |*sym| {
        try self.collectType(sym);
    }
}

fn collectType(self: *Self, sym: *Table.Symbol) !void {
    return switch (sym.tag) {
        .@"struct" => self.collectStructType(sym.ref),
        .param => self.collectParamType(sym),
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

fn collectParamType(self: *Self, sym: *Table.Symbol) !void {
    const data = self.data[sym.ref];

    const ty = self.solveType(data.rhs).?;
    sym.data.param.ty = ty;
}

pub fn solveTypes(alloc: std.mem.Allocator, ast: *const Ast, table: *Table, source: []const u8) !TypeContext {
    var self: Self = .{
        .alloc = alloc,
        .ast = ast,
        .tags = ast.nodes.items(.tag),
        .data = ast.nodes.items(.data),
        .locs = ast.nodes.items(.loc),
        .table = table,
        .context = try TypeContext.allocate(alloc, ast.nodes.len),
        .source = source,
        .diagnostic = Diagnostig.init(std.io.getStdOut().writer(), source),
    };
    defer self.symbols.deinit(alloc);
    errdefer self.context.free(alloc);

    try self.solveNodeType(0);

    return self.context;
}

fn solveNodeType(self: *Self, node_ptr: usize) error{OutOfMemory}!void {
    switch (self.tags[node_ptr]) {
        .root => {
            const data = self.data[node_ptr];
            for (data.lhs..data.rhs) |child_ptr| {
                try self.solveNodeType(child_ptr);
            }
        },
        .struct_decl => {
            const data = self.data[node_ptr];

            const scope = self.current_scope;
            defer self.popScope(scope);

            self.current_scope = self.table.lookupSymbol(scope, self.ast.literals.items[self.data[data.lhs].lhs]).?.data.@"struct".body;

            for (data.lhs + 1..data.rhs) |child_ptr| {
                try self.solveNodeType(child_ptr);
            }
        },
        .func_decl => try self.solveFunctionNodeType(node_ptr),
        .var_decl, .typed_var_decl => try self.solveVariableNodeType(node_ptr),
        .@"return" => try self.solveReturnNodeType(node_ptr),
        .undefined => self.solveUndefinedNodeType(node_ptr),
        .int => self.solveIntNodeType(node_ptr),
        .float => self.solveFloatNodeType(node_ptr),
        .bool => self.solveBoolNodeType(node_ptr),
        .ident => self.solveIdentNodeType(node_ptr),
        .tuple_literal => try self.solveTupleLiteralNodeType(node_ptr),
        .array_literal => try self.solveArrayLiteralNodeType(node_ptr),
        .scope => try self.solveScopeNodeType(node_ptr),
        .@"if" => try self.solveIfNodeType(node_ptr),
        .add, .sub, .mul, .div, .equal, .not_equal => try self.solveBinaryNodeType(node_ptr),
        .call => try self.solveCallNodeType(node_ptr),
        .member => try self.solveMemberNodeType(node_ptr),
        .import_relative_module, .range, .field_decl, .func_proto, .func_param => {},
        .tuple, .slice => {},
    }
}

fn solveFunctionNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];
    const sym = self.table.lookupSymbol(self.current_scope, self.ast.literals.items[self.data[data.lhs].lhs]).?;

    const scope = self.current_scope;
    defer self.popScope(scope);
    self.current_scope = sym.data.func.args;

    const proto = self.data[data.lhs + 1];
    const return_type = self.solveType(proto.lhs).?;
    const last_return = self.expected_return;
    defer self.expected_return = last_return;
    self.expected_return = return_type;

    for (proto.lhs + 1..proto.rhs) |child_ptr| {
        const arg = self.data[child_ptr];

        const name = self.ast.literals.items[self.data[arg.lhs].lhs];
        const ty = self.solveType(arg.rhs).?;

        self.context.types[child_ptr] = ty;
        try self.addSymbol(name, false, ty);
    }

    self.current_scope = sym.data.func.body;

    for (data.lhs + 2..data.rhs) |child_ptr| {
        try self.solveNodeType(child_ptr);
    }
}

fn solveVariableNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];
    const name = self.ast.literals.items[self.data[data.lhs].lhs];

    if (self.tags[node_ptr] == .typed_var_decl) {
        const ty = self.solveType(data.lhs + 1) orelse {
            std.process.exit(1);
        };
        self.context.types[node_ptr] = ty;

        self.expected_type = ty;
    } else {
        self.expected_type = Type.builtin.UNKNOWN;
    }

    try self.solveNodeType(data.rhs - 1);

    if (self.tags[node_ptr] == .typed_var_decl) {
        if (self.context.types[node_ptr].id != self.current_type.id) {
            self.diagnostic.report("types mismatch", .{}, self.locs[data.rhs - 1]);
            // std.debug.print("Type mismatch\n", .{});
        }
    }

    self.context.types[node_ptr] = self.current_type;

    try self.addSymbol(name, false, self.current_type);

    self.expected_type = Type.builtin.UNKNOWN;
    self.current_type = Type.builtin.UNKNOWN;
}

fn solveReturnNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];
    self.expected_type = self.expected_return.?;

    if (data.lhs != 0) {
        try self.solveNodeType(data.lhs);

        const exp = self.expected_return.?;
        if (self.current_type.id != exp.id) {
            self.diagnostic.report("return type mismatch", .{}, self.locs[data.lhs]);
        }
    }
}

fn solveUndefinedNodeType(self: *Self, node_ptr: usize) void {
    self.context.types[node_ptr] = self.expected_type;
    self.current_type = self.expected_type;
}

fn solveIntNodeType(self: *Self, node_ptr: usize) void {
    const ty = switch (self.expected_type.id) {
        Type.builtin.U8.id,
        Type.builtin.U16.id,
        Type.builtin.U32.id,
        Type.builtin.U64.id,
        Type.builtin.S8.id,
        Type.builtin.S16.id,
        Type.builtin.S32.id,
        Type.builtin.S64.id,
        Type.builtin.F32.id,
        Type.builtin.F64.id,
        => self.expected_type,
        else => Type.builtin.U32,
    };

    self.context.types[node_ptr] = ty;
    self.current_type = ty;
}

fn solveFloatNodeType(self: *Self, node_ptr: usize) void {
    self.context.types[node_ptr] = Type.builtin.F32;
    self.current_type = Type.builtin.F32;
}

fn solveBoolNodeType(self: *Self, node_ptr: usize) void {
    self.context.types[node_ptr] = Type.builtin.BOOL;
    self.current_type = Type.builtin.BOOL;
}

fn solveIdentNodeType(self: *Self, node_ptr: usize) void {
    const data = self.data[node_ptr];
    const name = self.ast.literals.items[data.lhs];

    const sym = self.lookupSymbol(name) orelse {
        return;
    };

    self.current_type = sym.ty;
}

fn solveTupleLiteralNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    var types: std.ArrayListUnmanaged(Type.Id) = .empty;
    errdefer types.deinit(self.alloc);

    for (data.lhs..data.rhs) |child_ptr| {
        try self.solveNodeType(child_ptr);
        try types.append(self.alloc, self.current_type);
    }

    self.current_type = try type_interner.getOrPut(.{
        .tag = .tuple,
        .data = .{
            .tuple = .{
                .fields = try types.toOwnedSlice(self.alloc),
            },
        },
    });
}

fn solveArrayLiteralNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    var ty: Type.Id = undefined;
    for (data.lhs..data.rhs, 0..) |child_ptr, idx| {
        try self.solveNodeType(child_ptr);

        if (idx == 0) {
            ty = self.current_type;
        } else {
            if (ty.id != self.current_type.id) {
                std.debug.print("Type mismatch\n", .{});
            }
        }
    }

    self.current_type = try type_interner.getOrPut(.{
        .tag = .slice,
        .data = .{
            .slice = .{
                .element = ty,
            },
        },
    });
}

fn solveScopeNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    for (data.lhs..data.rhs) |child_ptr| {
        try self.solveNodeType(child_ptr);
    }

    self.current_type = Type.builtin.VOID;
}

fn solveIfNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];
    const elements = data.rhs - data.lhs + 1;

    const cond = c: {
        const exp = self.expected_type;
        defer self.expected_type = exp;
        self.expected_type = Type.builtin.BOOL;

        try self.solveNodeType(data.lhs);

        break :c self.current_type;
    };

    if (cond.id != Type.builtin.BOOL.id) {
        self.diagnostic.report("condition must be a boolean", .{}, self.locs[data.lhs]);
    }

    if (elements == 2) {
        try self.solveNodeType(data.lhs + 1);
    } else if (elements == 3) {
        try self.solveNodeType(data.lhs + 1);
        const then = self.current_type;

        const else_ = c: {
            const exp = self.expected_type;
            defer self.expected_type = exp;
            self.expected_type = then;

            try self.solveNodeType(data.lhs + 2);
            break :c self.current_type;
        };

        if (then.id != else_.id) {
            self.diagnostic.report("branches must have the same type", .{}, self.locs[data.lhs + 2]);
        }
    } else unreachable;
}

fn solveBinaryNodeType(self: *Self, node_ptr: usize) !void {
    const tag = self.tags[node_ptr];
    const data = self.data[node_ptr];

    try self.solveNodeType(data.lhs);
    const lhs = self.current_type;

    try self.solveNodeType(data.rhs);
    const rhs = self.current_type;

    if (lhs.id != rhs.id) {
        const loc = Location.span(self.locs[data.lhs], self.locs[data.rhs]);

        self.diagnostic.report("types mismatch", .{}, loc);
    }

    switch (tag) {
        .add, .sub, .mul, .div => {
            self.current_type = lhs;
        },
        .equal, .not_equal => {
            self.current_type = Type.builtin.BOOL;
        },
        else => unreachable,
    }
}

fn solveCallNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    const sym = self.solveSymbol(data.lhs) orelse return;
    if (sym.tag != .func) {
        self.diagnostic.report("not a function", .{}, self.locs[data.lhs]);
        return;
    }
    const args = self.table.getSymbolsInScope(sym.data.func.args);

    const exp = self.expected_type;
    defer self.expected_type = exp;

    for (data.lhs + 1..data.rhs, 0..) |child_ptr, idx| {
        self.expected_type = args[idx].data.param.ty;
        try self.solveNodeType(child_ptr);
        const ty = self.current_type;

        if (ty.id != self.expected_type.id) {
            self.diagnostic.report("argument type mismatch {} {}", .{ self.expected_type.id, ty.id }, self.locs[child_ptr]);
        }
    }
}

fn solveMemberNodeType(self: *Self, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    try self.solveNodeType(data.lhs);
    const lhs = self.current_type;
    const sym = type_interner.getSymbol(lhs) orelse {
        std.debug.print("Type not found\n", .{});
        return;
    };

    switch (self.tags[data.rhs]) {
        .int => try self.accessTupleMember(sym, data.rhs),
        .ident => try self.accessNamedMember(sym, data.rhs),
        else => {},
    }
}

fn accessTupleMember(self: *Self, sym: Type, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    const idx: usize = @intCast(data.decodeInt());
    const member = sym.data.tuple.fields[idx];

    self.current_type = member;
}

fn accessNamedMember(self: *Self, sym: Type, node_ptr: usize) !void {
    const data = self.data[node_ptr];

    const name = self.ast.literals.items[data.lhs];
    var field: ?Type.StructField = null;
    for (sym.data.@"struct".fields) |f| {
        if (std.mem.eql(u8, f.name, name)) {
            field = f;
        }
    }

    if (field) |f| {
        self.current_type = f.ty;
    } else {
        std.debug.print("Field {s} not found\n", .{name});
    }
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
        .member => {
            const sym = self.solveSymbol(node_ptr) orelse return null;

            switch (sym.tag) {
                .@"struct" => {
                    return sym.data.@"struct".ty;
                },
                else => unreachable,
            }
        },
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

fn solveSymbol(self: *Self, node_ptr: usize) ?Table.Symbol {
    switch (self.tags[node_ptr]) {
        .ident => {
            const name = self.ast.literals.items[self.data[node_ptr].lhs];
            const symbol = self.table.lookupSymbol(self.current_scope, name) orelse {
                return null;
            };

            return symbol;
        },
        .member => {
            const data = self.data[node_ptr];
            const of = self.solveSymbol(data.lhs) orelse return null;
            const what = self.ast.literals.items[self.data[data.rhs].lhs];

            return switch (of.tag) {
                .import => {
                    const table = compiler.getSymbolTable(of.data.import.module);
                    return table.lookupSymbol(0, what) orelse {
                        return null;
                    };
                },
                else => unreachable,
            };
        },
        else => {
            std.log.err("unknown symbol lookup", .{});
            return null;
        },
    }
}

fn popScope(self: *Self, scope: u32) void {
    self.current_scope = scope;

    var i = self.symbols.items.len;
    while (i > 0) : (i -= 1) {
        if (self.symbols.items[i - 1].scope == scope) {
            break;
        }

        self.symbols.items.len -= 1;
    }
}

fn addSymbol(self: *Self, name: []const u8, is_type: bool, ty: Type.Id) !void {
    const sym: CodeSymbol = .{
        .name = name,
        .is_type = is_type,
        .ty = ty,
        .scope = self.current_scope,
    };

    try self.symbols.append(self.alloc, sym);
}

fn lookupSymbol(self: *Self, name: []const u8) ?CodeSymbol {
    var i = self.symbols.items.len;
    while (i > 0) : (i -= 1) {
        const sym = self.symbols.items[i - 1];

        if (std.mem.eql(u8, sym.name, name)) {
            return sym;
        }
    }
    return null;
}
