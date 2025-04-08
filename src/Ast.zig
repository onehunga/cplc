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

pub fn prettyPrint(self: *const Self, writer: std.fs.File.Writer) !void {
    var printer = PrettyPrinter.init(self, writer);
    return printer.print();
}

pub fn dump(self: *const Self, writer: std.fs.File.Writer) !void {
    const data = self.nodes.items(.data);

    for (self.nodes.items(.tag), 0..) |tag, idx| {
        defer writer.writeByte('\n') catch unreachable;
        try writer.print("{}: {}({}, {})", .{ idx, tag, data[idx].lhs, data[idx].rhs });

        switch (tag) {
            .ident => try writer.print(" '{s}'", .{self.literals.items[data[idx].lhs]}),
            .int => {
                var bits: u64 = data[idx].rhs;
                bits |= @as(u64, @intCast(data[idx].lhs)) << 32;

                try writer.print(" '{}'", .{bits});
            },
            .float => {
                var bits: u64 = data[idx].rhs;
                bits |= @as(u64, @intCast(data[idx].lhs)) << 32;
                const float: f64 = @bitCast(bits);

                try writer.print(" '{}'", .{float});
            },
            else => {},
        }
    }
}

pub fn collectImports(self: *const Self, gpa: std.mem.Allocator) !std.ArrayListUnmanaged(u32) {
    const root = self.nodes.items(.data)[0];
    const tags = self.nodes.items(.tag)[root.lhs..root.rhs];

    var imports: std.ArrayListUnmanaged(u32) = .empty;

    for (tags, 0..) |tag, idx| {
        switch (tag) {
            .import_relative_module => {
                try imports.append(gpa, @truncate(idx + root.lhs));
            },
            else => continue,
        }
    }

    return imports;
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

        /// import a module relative to the current module
        ///
        /// nodes[lhs] = name
        import_relative_module,

        /// struct definition
        ///
        /// nodes[lhs] = name
        /// nodes[lhs + 1..rhs] = fields in the struct
        struct_decl,

        field_decl,

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
        /// nodes[lhs] = name
        /// nodes[rhs] = type
        func_param,

        /// a general range of nodes
        ///
        /// nodes[lhs..rhs]
        range,

        /// nodes[lhs] = name
        /// nodes[rhs] = value
        var_decl,

        /// nodes[lhs] = name
        /// nodes[lhs + 1] = type
        /// nodes[rhs] = value
        typed_var_decl,

        /// return statement
        ///
        /// if lhs != 0
        @"return",

        /// initialize an empty value
        undefined,

        /// integer uses the lhs and rhs fields to store a 64 bit unsigned integer
        int,

        /// stores a 64bit float number in the lhs and rhs field
        float,

        /// stores either a 1 or 0 in lhs depending if the value is true or false
        bool,

        /// identifier
        ///
        /// lhs = index into literals
        ident,

        /// represents a range of nodes as a block
        scope,

        /// nodes[lhs..rhs]
        tuple_literal,

        /// represents a range of nodes
        ///
        /// nodes[lhs..rhs]
        array_literal,

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

        /// nodes[lhs] = callee
        /// nodes[lhs + 1..rhs] = arguments
        call,

        member,

        // ---------------------------
        // --- Type Specific Nodes ---
        // ---------------------------

        /// a tuple type definition
        ///
        /// nodes[lhs..rhs] = types of the tuple
        tuple,

        /// a slice type is an array type without a compile time known size
        ///
        /// nodes[lhs] = base type
        slice,
    };

    pub const Data = struct {
        lhs: u32 = 0,
        rhs: u32 = 0,

        pub fn decodeInt(self: Data) u64 {
            var value: u64 = @intCast(self.lhs);
            value <<= 32;
            value |= @intCast(self.rhs);
            return value;
        }
    };

    pub const List = std.MultiArrayList(Node);
};

/// pretty print the ast
const PrettyPrinter = struct {
    ast: *const Self,
    tags: []const Node.Tag,
    data: []const Node.Data,
    writer: std.fs.File.Writer,
    /// this is the only allocator that really makes sense with the current design
    allocator: mem.Allocator = std.heap.smp_allocator,

    last_indents: std.bit_set.ArrayBitSet(usize, 1024),
    current_indent: usize = 0,

    pub fn init(ast: *const Self, writer: std.fs.File.Writer) PrettyPrinter {
        return .{
            .ast = ast,
            .tags = ast.nodes.items(.tag),
            .data = ast.nodes.items(.data),
            .writer = writer,
            .last_indents = .initEmpty(),
        };
    }

    pub fn print(self: *PrettyPrinter) !void {
        return self.printRoot();
    }

    fn printRoot(self: *PrettyPrinter) !void {
        const root = self.data[0];

        try self.writer.writeAll("file\n");

        for (root.lhs..root.rhs) |idx| {
            try self.printNode(idx, idx == root.rhs - 1);
        }
    }

    fn printNode(self: *PrettyPrinter, idx: usize, last: bool) std.fs.File.WriteError!void {
        const tag = self.tags[idx];

        try switch (tag) {
            .struct_decl => self.printStruct(idx, last),
            .field_decl => self.printFieldDecl(idx, last),
            .func_decl => self.printFunction(idx, last),
            .var_decl => self.printVarDecl(idx, last),
            .typed_var_decl => self.printVarDecl(idx, last),
            .@"return" => self.printReturn(idx, last),
            .undefined => self.printUndefined(last),
            .bool => self.printBool(idx, last),
            .int => self.printInt(idx, last),
            .float => self.printFloat(idx, last),
            .ident => self.printIdent(idx, last),
            .scope => self.printScope(idx, last),
            .tuple_literal => self.printTupleLiteral(idx, last),
            .array_literal => self.printArrayLiteral(idx, last),
            .@"if" => self.printIf(idx, last),
            .add => self.printBinop(idx, last, "add"),
            .sub => self.printBinop(idx, last, "sub"),
            .mul => self.printBinop(idx, last, "mul"),
            .div => self.printBinop(idx, last, "div"),
            .equal => self.printBinop(idx, last, "equal"),
            .not_equal => self.printBinop(idx, last, "not_equal"),
            .call => self.printCall(idx, last),
            .member => self.printMember(idx, last),
            else => {},
        };
    }

    fn printStruct(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("struct\n");

        self.pushIndent(last);
        defer self.popIndent();

        {
            const name_idx = self.data[data.lhs].lhs;
            const name = self.ast.literals.items[name_idx];

            try self.printIndentation(false);
            try self.writer.print("name: '{s}'\n", .{name});
        }

        {
            try self.printIndentation(true);
            try self.writer.writeAll("body\n");

            self.pushIndent(true);
            defer self.popIndent();

            for (data.lhs + 1..data.rhs) |node_ptr| {
                try self.printNode(node_ptr, node_ptr == data.rhs - 1);
            }
        }
    }

    fn printFieldDecl(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("field\n");

        self.pushIndent(last);
        defer self.popIndent();

        {
            try self.printIndentation(false);
            try self.writer.writeAll("name\n");

            self.pushIndent(false);
            defer self.popIndent();
            try self.printNode(data.lhs, true);
        }

        {
            try self.printIndentation(true);
            try self.writer.writeAll("type\n");

            self.pushIndent(true);
            defer self.popIndent();
            try self.printType(data.rhs, true);
        }
    }

    fn printFunction(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];
        const name = self.ast.literals.items[self.data[data.lhs].lhs];

        try self.printIndentation(last);
        try self.writer.writeAll("func\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printIndentation(false);
            try self.writer.print("name: '{s}'\n", .{name});

            try self.printFunctionProto(data.lhs + 1);
        }

        // body
        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printIndentation(true);
            try self.writer.writeAll("body\n");

            self.pushIndent(true);
            defer self.popIndent();

            for (data.lhs + 2..data.rhs) |index| {
                try self.printNode(index, index == data.rhs - 1);
            }
        }
    }

    fn printFunctionProto(self: *PrettyPrinter, idx: usize) !void {
        const data = self.data[idx];

        try self.printIndentation(false);
        try self.writer.writeAll("args\n");

        {
            self.pushIndent(false);
            defer self.popIndent();

            for (data.lhs + 1..data.rhs) |index| {
                try self.printFunctionParameter(index, index == data.rhs - 1);
            }
        }
    }

    fn printFunctionParameter(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll(
            "arg:\n",
        );

        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printNode(data.lhs, false);

            try self.printIndentation(true);
            try self.writer.writeAll("type\n");

            self.pushIndent(true);
            defer self.popIndent();
            try self.printType(data.rhs, true);
        }
    }

    fn printVarDecl(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const tag = self.tags[idx];
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("var\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printNode(data.lhs, false);

            if (tag == .typed_var_decl) {
                try self.printIndentation(false);
                try self.writer.writeAll("type\n");

                self.pushIndent(false);
                defer self.popIndent();
                try self.printType(data.lhs + 1, true);
            }

            try self.printIndentation(true);
            try self.writer.writeAll("value\n");

            self.pushIndent(true);
            defer self.popIndent();
            try self.printNode(data.rhs - 1, true);
        }
    }

    fn printReturn(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("return\n");

        if (data.lhs != 0) {
            self.pushIndent(true);
            defer self.popIndent();

            try self.printNode(data.lhs, true);
        }
    }

    fn printUndefined(self: *PrettyPrinter, last: bool) !void {
        try self.printIndentation(last);
        try self.writer.writeAll("undefined\n");
    }

    fn printBool(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.print("bool: {}\n", .{data.lhs == 1});
    }

    fn printInt(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.print("int: {d}\n", .{data.decodeInt()});
    }

    fn printFloat(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];
        var bits: u64 = @intCast(data.lhs);
        bits <<= 32;
        bits |= @intCast(data.rhs);
        const value: f64 = @bitCast(bits);

        try self.printIndentation(last);
        try self.writer.print("float: {}\n", .{value});
    }

    fn printIdent(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];
        const name = self.ast.literals.items[data.lhs];

        try self.printIndentation(last);
        try self.writer.print("ident: '{s}'\n", .{name});
    }

    fn printScope(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("scope\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            for (data.lhs..data.rhs) |index| {
                try self.printNode(index, index == data.rhs - 1);
            }
        }
    }

    fn printTupleLiteral(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("tuple\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            for (data.lhs..data.rhs) |index| {
                try self.printNode(index, index == data.rhs - 1);
            }
        }
    }

    fn printArrayLiteral(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("array\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            for (data.lhs..data.rhs) |index| {
                try self.printNode(index, index == data.rhs - 1);
            }
        }
    }

    fn printIf(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];
        const elems = data.rhs - data.lhs + 1;

        try self.printIndentation(last);
        try self.writer.writeAll("if\n");

        {
            self.pushIndent(false);
            defer self.popIndent();

            {
                try self.printIndentation(false);
                try self.writer.writeAll("condition\n");

                self.pushIndent(false);
                defer self.popIndent();

                try self.printNode(data.lhs, true);
            }

            // then
            {
                try self.printIndentation(elems == 2);
                try self.writer.writeAll("then\n");

                self.pushIndent(elems == 2);
                defer self.popIndent();

                try self.printNode(data.lhs + 1, true);
            }

            if (elems == 3) {
                try self.printIndentation(true);
                try self.writer.writeAll("else\n");

                self.pushIndent(true);
                defer self.popIndent();

                try self.printNode(data.lhs + 2, true);
            }
        }
    }

    fn printBinop(self: *PrettyPrinter, idx: usize, last: bool, name: []const u8) !void {
        try self.printIndentation(last);
        try self.writer.print("{s}\n", .{name});

        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printNode(self.data[idx].lhs, false);
            try self.printNode(self.data[idx].rhs, true);
        }
    }

    fn printCall(self: *PrettyPrinter, idx: usize, last: bool) !void {
        const data = self.data[idx];

        try self.printIndentation(last);
        try self.writer.writeAll("call\n");

        {
            self.pushIndent(last);
            defer self.popIndent();

            try self.printNode(data.lhs, false);

            {
                try self.printIndentation(true);
                try self.writer.writeAll("args\n");

                self.pushIndent(true);
                defer self.popIndent();

                for (data.lhs + 1..data.rhs) |index| {
                    try self.printNode(index, index == data.rhs - 1);
                }
            }
        }
    }

    fn printMember(self: *PrettyPrinter, idx: usize, last: bool) !void {
        try self.printBinop(idx, last, "member");
    }

    fn printType(self: *PrettyPrinter, idx: usize, last: bool) !void {
        return switch (self.tags[idx]) {
            .ident => {
                const data = self.data[idx];
                const name = self.ast.literals.items[data.lhs];
                try self.printIndentation(last);
                try self.writer.print("'{s}'\n", .{name});
            },
            .tuple => {
                const data = self.data[idx];
                try self.printIndentation(last);
                try self.writer.writeAll("tuple\n");

                self.pushIndent(true);
                defer self.popIndent();

                for (data.lhs..data.rhs) |node_ptr| {
                    try self.printType(node_ptr, node_ptr == data.rhs - 1);
                }
            },
            .slice => {
                const data = self.data[idx];
                try self.printIndentation(last);
                try self.writer.writeAll("slice\n");

                {
                    self.pushIndent(true);
                    defer self.popIndent();

                    try self.printType(data.lhs, true);
                }
            },
            .member => {
                const data = self.data[idx];
                try self.printIndentation(last);
                try self.writer.writeAll("member\n");

                {
                    self.pushIndent(true);
                    defer self.popIndent();

                    try self.printType(data.lhs, false);
                    try self.printType(data.rhs, true);
                }
            },
            else => {},
        };
    }

    fn printIndentation(self: *PrettyPrinter, last: bool) !void {
        for (0..self.current_indent) |idx| {
            const indent = self.last_indents.isSet(idx);
            if (indent) {
                try self.writer.writeAll("  ");
            } else {
                try self.writer.writeAll("│ ");
            }
        }

        if (last) {
            try self.writer.writeAll("└─");
        } else {
            try self.writer.writeAll("├─");
        }
    }

    fn pushIndent(self: *PrettyPrinter, last: bool) void {
        if (last) {
            self.last_indents.set(self.current_indent);
        }

        self.current_indent += 1;
    }

    fn popIndent(self: *PrettyPrinter) void {
        self.current_indent -= 1;
        self.last_indents.unset(self.current_indent);
    }
};
