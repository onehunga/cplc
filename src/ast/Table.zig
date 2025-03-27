const Self = @This();
const std = @import("std");

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
    body: Scope.Id,
};

pub const FunctionData = struct {
    args: Scope.Id,
    body: Scope.Id,
};
