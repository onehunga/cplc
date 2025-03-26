const Self = @This();
const std = @import("std");

root: []const Symbol,
symbols: []const Symbol,

pub fn init(root: Symbols, symbols: []const Symbol) Self {
    return .{
        .root = symbols[root.start..][0..root.len],
        .symbols = symbols,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    alloc.free(self.symbols);
}

pub fn getSymbols(self: *const Self, symbols: Symbols) []const Symbol {
    return self.symbols[symbols.start..][0..symbols.len];
}

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
    children: Symbols,
};

pub const FunctionData = struct {
    args: Symbols,
    body: Symbols,
};
