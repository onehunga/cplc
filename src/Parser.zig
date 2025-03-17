const Self = @This();
const std = @import("std");
const mem = std.mem;
const Ast = @import("Ast.zig");
const Lexer = @import("Lexer.zig");

const Precedence = enum {
    none,
    sum,
    prod,
    equal,
    not_equal,
};

const ParserState = enum {
    root,
    struct_body,
};

const ParseError = error{
    UnknownStatement,
    UnexpectedToken,
    EndOfFile,
} || mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError;

allocator: mem.Allocator,
tokens: Lexer.TokenList,
source: []const u8,
tags: []const Lexer.Token.Tag,
locs: []const Lexer.Token.Location,
current: usize = 0,

nodes: Ast.Node.List = .{},
literals: std.ArrayListUnmanaged([]const u8) = .{},

scratch: std.ArrayListUnmanaged(Ast.Node) = .{},

state: ParserState = .root,

fn init(allocator: mem.Allocator, tokens: Lexer.TokenList, source: []const u8) Self {
    return .{
        .allocator = allocator,
        .tokens = tokens,
        .source = source,
        .tags = tokens.items(.tag),
        .locs = tokens.items(.location),
    };
}

fn deinit(self: *Self) void {
    self.scratch.deinit(self.allocator);
}

fn setupAst(self: *Self) ParseError!void {
    _ = try self.addNode(.{
        .tag = .root,
        .data = .{},
        .loc = self.locs[0],
    });
}

fn parseRoot(self: *Self) ParseError!void {
    while (self.current < self.tokens.len) {
        try self.parseStmt();
        self.advance();
    }

    self.nodes.items(.data)[0] = try self.addNodes(self.scratch.items);
}

fn parseStmt(self: *Self) ParseError!void {
    return switch (self.currentToken()) {
        .comment => self.parseComment(),
        .@"struct" => self.parseStruct(),
        .func => self.parseFunction(),
        .@"var" => self.parseVariable(),
        .@"return" => self.parseReturn(),
        else => switch (self.state) {
            .struct_body => self.parseFieldDecl(),
            else => self.parseExpressionStmt(),
        },
    };
}

fn parseComment(self: *Self) void {
    _ = self;
}

fn parseStruct(self: *Self) !void {
    const start = self.scratch.items.len;
    const start_loc = self.locs[self.current];

    self.advance();

    const struct_name = try self.parseStructName();
    try self.addScratch(struct_name);

    if (self.currentToken() != .left_brace) {
        return ParseError.InvalidCharacter;
    }
    self.advance();

    {
        const state = self.pushState(.struct_body);
        defer self.restoreState(state);
        while (self.currentToken() != .right_brace) {
            try self.parseStmt();
            self.advance();
        }
    }

    const node: Ast.Node = .{
        .tag = .struct_decl,
        .data = try self.addFromScratch(start),
        .loc = start_loc,
    };

    try self.addScratch(node);
}

fn parseStructName(self: *Self) !Ast.Node {
    const name = try self.takeToken(.ident, "expected a structure name name", .{});
    const lit = self.source[name.start..name.end];
    const ref = try self.addLiteral(lit);

    return .{
        .tag = .ident,
        .data = .{ .lhs = ref },
        .loc = name,
    };
}

fn parseFieldDecl(self: *Self) !void {
    std.debug.print("parsing field_decl\n", .{});

    if (self.currentToken() != .ident) {
        return ParseError.InvalidCharacter;
    }
    const name = try self.parseIdent();
    self.advance();

    const ty = try self.parseType();
    self.advance();

    try self.addScratch(.{
        .tag = .field_decl,
        .data = .{
            .lhs = try self.addNode(name),
            .rhs = try self.addNode(ty),
        },
        .loc = .{
            .start = name.loc.start,
            .end = ty.loc.end,
        },
    });
}

fn parseFunction(self: *Self) ParseError!void {
    const start = self.scratch.items.len;

    try self.scratch.append(self.allocator, undefined);
    try self.scratch.append(self.allocator, undefined);

    self.advance();
    const name = try self.parseFunctionName();
    self.scratch.items[start] = name;

    const proto = try self.parseFunctionProto();
    self.scratch.items[start + 1] = proto;

    if (!try self.consume(.left_brace)) {
        std.debug.print("expected '{{'\n", .{});
        return ParseError.UnexpectedToken;
    }

    while (self.currentToken() != .right_brace) {
        try self.parseStmt();
        self.advance();
    }

    var node: Ast.Node = .{
        .tag = .func_decl,
        .data = .{},
        .loc = name.loc,
    };
    node.data = try self.addNodes(self.scratch.items[start..]);

    self.scratch.resize(self.allocator, start) catch unreachable;
    try self.scratch.append(self.allocator, node);
}

fn parseFunctionProto(self: *Self) ParseError!Ast.Node {
    const start = self.scratch.items.len;

    try self.scratch.append(self.allocator, undefined);

    const start_loc = self.locs[self.current];
    if (!try self.consume(.left_paren)) {
        return ParseError.UnexpectedToken;
    }

    while (self.currentToken() != .right_paren) {
        const name = try self.takeToken(.ident, "expected a parameter name", .{});
        const ty = try self.parseType();

        const param = Ast.Node{
            .tag = .func_param,
            .data = .{
                .lhs = try self.addNode(.{
                    .tag = .ident,
                    .data = .{ .lhs = try self.addLiteral(self.source[name.start..name.end]) },
                    .loc = name,
                }),
                .rhs = try self.addNode(ty),
            },
            .loc = .{
                .start = name.start,
                .end = ty.loc.end,
            },
        };

        try self.scratch.append(self.allocator, param);

        self.advance();

        switch (self.currentToken()) {
            .comma => self.advance(),
            .right_paren => break,
            else => std.debug.print("expected ',' or ')'\n", .{}),
        }
    }
    self.advance();

    self.scratch.items[start] = try self.parseType();
    self.advance();

    const node = Ast.Node{
        .tag = .func_proto,
        .data = try self.addNodes(self.scratch.items[start..]),
        .loc = .{
            .start = start_loc.start,
            .end = self.locs[self.current].end,
        },
    };

    try self.scratch.resize(self.allocator, start);
    return node;
}

fn parseFunctionName(self: *Self) ParseError!Ast.Node {
    const name = try self.takeToken(.ident, "expected a function name", .{});
    const lit = self.source[name.start..name.end];
    const ref = try self.addLiteral(lit);

    return .{
        .tag = .ident,
        .data = .{ .lhs = ref },
        .loc = name,
    };
}

fn parseVariable(self: *Self) ParseError!void {
    const loc = self.locs[self.current];
    self.advance();

    var values: [3]Ast.Node = undefined;

    const name = try self.parseVariableName();
    values[0] = name;

    var tag: Ast.Node.Tag = .var_decl;
    if (self.currentToken() != .assign) {
        tag = .typed_var_decl;

        const ty = try self.parseType();
        self.advance();
        values[1] = ty;
    }

    if (try self.consume(.assign)) {
        const value = try self.parseExpression(.none);
        self.advance();

        if (tag == .typed_var_decl) {
            values[2] = value;
        } else {
            values[1] = value;
        }
    }

    if (self.currentToken() != .semicolon) {
        std.debug.print("expected a semicolon after a variable\n", .{});
    }

    const data = try if (tag == .var_decl) self.addNodes(values[0..2]) else self.addNodes(values[0..3]);

    const v: Ast.Node = .{
        .tag = tag,
        .data = data,
        .loc = loc,
    };

    try self.scratch.append(self.allocator, v);
}

fn parseVariableName(self: *Self) ParseError!Ast.Node {
    const name = try self.takeToken(.ident, "expected a variable name", .{});
    const lit = self.source[name.start..name.end];
    const ref = try self.addLiteral(lit);

    return .{
        .tag = .ident,
        .data = .{ .lhs = ref },
        .loc = name,
    };
}

fn parseReturn(self: *Self) !void {
    const loc = self.locs[self.current];

    self.advance();
    var value: u32 = 0;
    if (self.currentToken() != .semicolon) {
        const expr = try self.parseExpression(.none);
        value = try self.addNode(expr);
        self.advance();
    }

    if (self.currentToken() != .semicolon) {
        std.debug.print("expected an semicolon\n", .{});
    }

    try self.scratch.append(self.allocator, .{
        .tag = .@"return",
        .data = .{ .lhs = value },
        .loc = loc,
    });
}

fn parseExpressionStmt(self: *Self) !void {
    const expr = try self.parseExpression(.none);
    self.advance();

    if (self.currentToken() != .semicolon) {
        std.debug.print("expected semicolon {}\n", .{self.currentToken()});
        return ParseError.UnexpectedToken;
    }

    try self.scratch.append(self.allocator, expr);
}

fn parseExpression(self: *Self, prec: Precedence) ParseError!Ast.Node {
    var lhs = try self.parsePrefix();

    while (@intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
        self.advance();
        lhs = try self.parseInfix(lhs);
    }

    return lhs;
}

fn parsePrefix(self: *Self) !Ast.Node {
    return switch (self.currentToken()) {
        .int => self.parseInt(),
        .float => self.parseFloat(),
        .true, .false => self.parseBool(),
        .ident => self.parseIdent(),
        .left_brace => self.parseScope(),
        .left_bracket => self.parseArrayLiteral(),
        .@"if" => self.parseIf(),
        else => ParseError.UnexpectedToken,
    };
}

fn parseInt(self: *Self) !Ast.Node {
    const loc = self.locs[self.current];
    const lit = self.source[loc.start..loc.end];

    const num = try std.fmt.parseUnsigned(u64, lit, 10);

    return Ast.Node{
        .tag = .int,
        .data = .{
            .lhs = @intCast(num >> 32),
            .rhs = @truncate(num),
        },
        .loc = loc,
    };
}

fn parseFloat(self: *Self) !Ast.Node {
    const loc = self.locs[self.current];
    const lit = self.source[loc.start..loc.end];

    const num = try std.fmt.parseFloat(f64, lit);
    const bits: u64 = @bitCast(num);

    return Ast.Node{
        .tag = .float,
        .data = .{
            .lhs = @intCast(bits >> 32),
            .rhs = @truncate(bits),
        },
        .loc = loc,
    };
}

fn parseBool(self: *Self) Ast.Node {
    const value: u32 = switch (self.currentToken()) {
        .true => 1,
        .false => 0,
        else => unreachable,
    };

    return Ast.Node{
        .tag = .bool,
        .data = .{ .lhs = value },
        .loc = self.locs[self.current],
    };
}

fn parseIdent(self: *Self) !Ast.Node {
    const loc = self.locs[self.current];
    const ident = self.source[loc.start..loc.end];

    return .{
        .tag = .ident,
        .data = .{
            .lhs = try self.addLiteral(ident),
        },
        .loc = loc,
    };
}

fn parseScope(self: *Self) !Ast.Node {
    const offset = self.scratch.items.len;
    const start = self.locs[self.current];

    self.advance();

    while (self.currentToken() != .right_brace) {
        try self.parseStmt();
        self.advance();
    }

    const scope: Ast.Node = .{
        .tag = .scope,
        .data = try self.addNodes(self.scratch.items[offset..]),
        .loc = start,
    };

    try self.scratch.resize(self.allocator, offset);

    return scope;
}

fn parseArrayLiteral(self: *Self) !Ast.Node {
    const start = self.locs[self.current];
    self.advance();

    var nodes: std.ArrayListUnmanaged(Ast.Node) = .{};
    defer nodes.deinit(self.allocator);

    while (self.currentToken() != .right_bracket) {
        try nodes.append(self.allocator, try self.parseExpression(.none));
        self.advance();

        switch (self.currentToken()) {
            .comma => self.advance(),
            .right_bracket => break,
            else => return ParseError.UnexpectedToken,
        }
    }

    return Ast.Node{
        .tag = .array_literal,
        .data = try self.addNodes(nodes.items),
        .loc = start,
    };
}

fn parseIf(self: *Self) !Ast.Node {
    const start = self.locs[self.current];
    self.advance();

    const condition = try self.parseExpression(.none);
    self.advance();

    const then = try self.parseExpression(.none);

    var el: ?Ast.Node = null;
    if (self.peekToken() == .@"else") {
        self.advance();
        self.advance();

        el = try self.parseExpression(.none);
    }

    var data: Ast.Node.Data = .{
        .lhs = try self.addNode(condition),
        .rhs = try self.addNode(then),
    };
    if (el) |e| {
        data.rhs = try self.addNode(e);
    }

    return Ast.Node{
        .tag = .@"if",
        .data = data,
        .loc = start,
    };
}

fn parseInfix(self: *Self, lhs: Ast.Node) !Ast.Node {
    return switch (self.currentToken()) {
        .plus => self.parseBinary(lhs, .add, .sum),
        .minus => self.parseBinary(lhs, .sub, .sum),
        .star => self.parseBinary(lhs, .mul, .prod),
        .slash => self.parseBinary(lhs, .div, .prod),
        .equal => self.parseBinary(lhs, .equal, .equal),
        .not_equal => self.parseBinary(lhs, .not_equal, .not_equal),
        else => ParseError.UnexpectedToken,
    };
}

fn parseBinary(self: *Self, lhs: Ast.Node, tag: Ast.Node.Tag, prec: Precedence) !Ast.Node {
    const loc = self.locs[self.current];
    self.advance();

    const rhs = try self.parseExpression(prec);

    return Ast.Node{
        .tag = tag,
        .data = .{
            .lhs = try self.addNode(lhs),
            .rhs = try self.addNode(rhs),
        },
        .loc = loc,
    };
}

fn parseType(self: *Self) !Ast.Node {
    return switch (self.currentToken()) {
        .ident => self.parseNamedType(),
        .left_bracket => self.parseArrayType(),
        else => ParseError.UnexpectedToken,
    };
}

fn parseArrayType(self: *Self) ParseError!Ast.Node {
    const start = self.locs[self.current];
    self.advance();

    const ty = try self.parseType();
    self.advance();

    if (self.currentToken() != .right_bracket) {
        std.debug.print("expected ']'\n", .{});
        return ParseError.UnexpectedToken;
    }

    return Ast.Node{
        .tag = .slice,
        .data = .{
            .lhs = try self.addNode(ty),
        },
        .loc = start,
    };
}

fn parseNamedType(self: *Self) !Ast.Node {
    const span = self.locs[self.current];
    const lit = self.source[span.start..span.end];
    const name = try self.addLiteral(lit);

    return Ast.Node{
        .tag = .ident,
        .data = .{
            .lhs = name,
        },
        .loc = self.locs[self.current],
    };
}

fn peekPrecedence(self: *const Self) Precedence {
    if (self.peekToken()) |tok| {
        return matchPrecedence(tok);
    }

    return .none;
}

fn matchPrecedence(tag: Lexer.Token.Tag) Precedence {
    return switch (tag) {
        .plus, .minus => .sum,
        .star, .slash => .prod,
        .equal => .equal,
        .not_equal => .not_equal,
        else => .none,
    };
}

fn currentToken(self: *const Self) Lexer.Token.Tag {
    return self.tags[self.current];
}

fn peekToken(self: *const Self) ?Lexer.Token.Tag {
    if (1 + self.current > self.tokens.len) {
        return null;
    }

    return self.tags[self.current + 1];
}

fn consume(self: *Self, tag: Lexer.Token.Tag) !bool {
    if (self.current > self.tokens.len) {
        return ParseError.EndOfFile;
    }

    if (self.tags[self.current] != tag) {
        return false;
    }

    self.current += 1;
    return true;
}

fn takeToken(self: *Self, tag: Lexer.Token.Tag, comptime fmt: []const u8, args: anytype) ParseError!Lexer.Token.Location {
    if (self.currentToken() != tag) {
        std.debug.print(fmt, args);

        return ParseError.UnexpectedToken;
    }

    const loc = self.locs[self.current];
    self.advance();

    return loc;
}

fn advance(self: *Self) void {
    if (!self.isAtEnd()) {
        self.current += 1;
    }
}

fn isAtEnd(self: *const Self) bool {
    return self.current >= self.tokens.len;
}

fn addNode(self: *Self, node: Ast.Node) !u32 {
    try self.nodes.append(self.allocator, node);

    return @intCast(self.nodes.len - 1);
}

fn addNodes(self: *Self, nodes: []const Ast.Node) !Ast.Node.Data {
    var data: Ast.Node.Data = .{};

    for (nodes) |node| {
        data.rhs = try self.addNode(node);
    }
    data.rhs += 1;

    data.lhs = @intCast(data.rhs - nodes.len);
    return data;
}

fn addLiteral(self: *Self, literal: []const u8) !u32 {
    try self.literals.append(self.allocator, literal);

    return @intCast(self.literals.items.len - 1);
}

fn addScratch(self: *Self, node: Ast.Node) !void {
    return self.scratch.append(self.allocator, node);
}

fn addFromScratch(self: *Self, start: usize) !Ast.Node.Data {
    const slice = self.scratch.items[start..];
    const data = try self.addNodes(slice);

    try self.scratch.resize(self.allocator, start);

    return data;
}

fn pushState(self: *Self, state: ParserState) ParserState {
    const old = self.state;
    self.state = state;
    return old;
}

fn restoreState(self: *Self, state: ParserState) void {
    self.state = state;
}

pub fn parse(allocator: mem.Allocator, tokens: Lexer.TokenList, source: []const u8) ParseError!Ast {
    var parser = Self.init(allocator, tokens, source);
    defer parser.deinit();
    try parser.setupAst();
    try parser.parseRoot();

    return Ast.init(parser.nodes, parser.literals);
}
