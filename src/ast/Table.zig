const Self = @This();
const std = @import("std");
const Type = @import("Type.zig");

symbols: std.ArrayListUnmanaged(Symbol),
scopes: std.ArrayListUnmanaged(Scope),

pub fn init(gpa: std.mem.Allocator) !Self {
    var self: Self = .{
        .symbols = .empty,
        .scopes = .empty,
    };
    try self.scopes.append(gpa, undefined);

    return self;
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.symbols.deinit(alloc);
    self.scopes.deinit(alloc);
}

pub fn getSymbols(self: *const Self, symbols: Symbols) []const Symbol {
    return self.symbols.items[symbols.start..][0..symbols.len];
}

pub fn getSymbolsInScope(self: *const Self, scope: Scope.Id) []const Symbol {
    return self.getSymbols(self.scopes.items[scope].symbols);
}

pub fn lookupSymbol(self: *const Self, scope: Scope.Id, name: []const u8) ?Symbol {
    const symbols = self.getSymbols(self.scopes.items[scope].symbols);

    for (symbols) |symbol| {
        if (std.mem.eql(u8, symbol.name, name)) {
            return symbol;
        }
    }

    if (self.scopes.items[scope].parent) |parent| {
        return self.lookupSymbol(parent, name);
    }

    return null;
}

pub const Scope = struct {
    parent: ?Id,
    symbols: Symbols,

    pub const Id = u32;

    pub const Scopes = struct {
        start: u32,
        len: u32,
    };
};

pub const Symbols = struct {
    start: u32,
    len: u32,
};

pub const Symbol = struct {
    tag: Tag,
    name: []const u8,
    data: Data,
    ref: u32, // ast index

    pub const Tag = enum {
        import,
        @"struct",
        func,
        param,
        field,
        @"var",
    };

    pub const Data = union {
        none: void,
        import: ImportData,
        @"struct": StructData,
        func: FunctionData,
        param: ParamData,

        pub const empty: Data = .{
            .none = void{},
        };

        pub const initialParam: Data = .{
            .param = ParamData{
                .ty = Type.builtin.UNKNOWN,
            },
        };
    };
};

pub const ImportData = struct {
    module: u32,
};

pub const StructData = struct {
    ty: Type.Id,
    body: Scope.Id,
};

pub const FunctionData = struct {
    args: Scope.Id,
    body: Scope.Id,
};

pub const ParamData = struct {
    ty: Type.Id,
};
