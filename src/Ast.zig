//! Abstract syntax tree for the language
//!
//! The ast is implemented as a data oriented tree

const Self = @This();
const std = @import("std");
const mem = std.mem;
const Lexer = @import("Lexer.zig");

nodes: Node.List,
literals: std.ArrayListUnmanaged([]const u8),

pub fn init(nodes: Node.List, literals: std.ArrayListUnmanaged([]const u8)) Self {
    return .{
        .nodes = nodes,
        .literals = literals,
    };
}

pub fn deinit(self: *Self, allocator: mem.Allocator) void {
    self.nodes.deinit(allocator);
    self.literals.deinit(allocator);
}

pub const Node = struct {
    tag: Tag,
    data: Data,
    loc: Lexer.Token.Location,

    pub const Tag = enum {
        /// root node is a list of statements
        ///
        /// nodes[lhs..rhs]
        root,

        /// a function declaration is represented in a range of nodes
        ///
        /// the range has a garanteed length of at least 2 and is of the form:
        ///
        /// nodes[lhs] = name
        /// nodes[lhs + 1] = function prototype
        /// nodes[lhs + 2..rhs] = function body
        func_decl,

        /// a function prototype is represented in a range of nodes
        ///
        /// the range has a garanteed length of at least 2 and is of the form:
        ///
        /// nodes[lhs] = return type
        /// nodes[lhs + 1..rhs] = parameters
        func_proto,

        /// a function parameter is represented as as:
        ///
        /// nodes[lhs] = type
        ///
        /// nodes[rhs] = name
        func_param,

        /// a general range of nodes
        ///
        /// nodes[lhs..rhs]
        range,

        /// return statement
        ///
        /// if lhs != 0
        @"return",

        /// integer uses the lhs and rhs fields to store a 64 bit unsigned integer
        int,

        /// stores a 64bit float number in the lhs and rhs field
        float,

        /// stores either a 1 or 0 in lhs depending if the value is true or false
        bool,

        /// represents a range of nodes as a block
        scope,

        /// Represents an if expression or statement.
        ///
        /// Contains multiple elements defined as `nodes[lhs..rhs]`.
        /// If the range contains two elements it has no else,
        /// otherwise it has an else as a third statement
        @"if",

        add,
        sub,
        mul,
        div,
        equal,
        not_equal,

        /// identifier
        ///
        /// lhs = index into literals
        ident,
    };

    pub const Data = struct {
        lhs: u32 = 0,
        rhs: u32 = 0,
    };

    pub const List = std.MultiArrayList(Node);
};
