const Self = @This();
const std = @import("std");
const Type = @import("Type.zig");

symbols: []const Symbol,
scopes: []const Scope,

pub fn init(symbols: []const Symbol, scopes: []const Scope) Self {
    return .{
        .symbols = symbols,
        .scopes = scopes,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    alloc.free(self.symbols);
    alloc.free(self.scopes);
}

pub fn getSymbols(self: *const Self, symbols: Symbols) []const Symbol {
    return self.symbols[symbols.start..][0..symbols.len];
}

pub fn lookupSymbol(self: *const Self, scope: Scope.Id, name: []const u8) ?Symbol {
    const symbols = self.getSymbols(self.scopes[scope].symbols);

    for (symbols) |symbol| {
        if (std.mem.eql(u8, symbol.name, name)) {
            return symbol;
        }
    }

    if (self.scopes[scope].parent) |parent| {
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
        @"struct",
        func,
        field,
        @"var",
    };

    pub const Data = union {
        none: void,
        @"struct": StructData,
        func: FunctionData,

        pub const empty: Data = .{
            .none = void{},
        };
    };
};

pub const StructData = struct {
    ty: Type.Id,
    body: Scope.Id,
};

pub const FunctionData = struct {
    args: Scope.Id,
    body: Scope.Id,
};
