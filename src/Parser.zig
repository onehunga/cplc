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
        .func => self.parseFunction(),
        .@"return" => self.parseReturn(),
        else => {
            return ParseError.UnknownStatement;
        },
    };
}

fn parseComment(self: *Self) void {
    _ = self;
}

fn parseFunction(self: *Self) ParseError!void {
    const start = self.scratch.items.len;

    self.advance();
    const name = try self.parseFunctionName();

    const proto = try self.parseFunctionProto();

    if (!try self.consume(.left_brace)) {
        std.debug.print("expected '{{'\n", .{});
        return ParseError.UnexpectedToken;
    }
    self.advance();

    while (self.currentToken() != .right_brace) {
        try self.parseStmt();
        self.advance();
    }

    var node: Ast.Node = .{
        .tag = .func_decl,
        .data = .{},
        .loc = name.loc,
    };
    const lhs = try self.addNode(name);
    _ = try self.addNode(proto);
    node.data = try self.addNodes(self.scratch.items[start..]);
    node.data.lhs = lhs;

    self.scratch.resize(self.allocator, start) catch unreachable;
    try self.scratch.append(self.allocator, node);
}

fn parseFunctionProto(self: *Self) ParseError!Ast.Node {
    var nodes: std.ArrayListUnmanaged(Ast.Node) = .{};
    defer nodes.deinit(self.allocator);

    const start = self.locs[self.current];
    if (!try self.consume(.left_paren)) {
        return ParseError.UnexpectedToken;
    }

    while (self.currentToken() != .right_paren) {
        self.advance();
    }
    self.advance();

    try nodes.append(self.allocator, try self.parseType());
    self.advance();

    return Ast.Node{
        .tag = .func_proto,
        .data = try self.addNodes(nodes.items),
        .loc = .{
            .start = start.start,
            .end = self.locs[self.current].end,
        },
    };
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
        else => ParseError.UnexpectedToken,
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

pub fn parse(allocator: mem.Allocator, tokens: Lexer.TokenList, source: []const u8) ParseError!Ast {
    var parser = Self.init(allocator, tokens, source);
    defer parser.deinit();
    try parser.setupAst();
    try parser.parseRoot();

    return Ast.init(parser.nodes, parser.literals);
}
