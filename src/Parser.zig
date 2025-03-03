const Self = @This();
const std = @import("std");
const mem = std.mem;
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");

const ParseError = error{
    UnknownStatement,
    UnexpectedToken,
    EndOfFile,
} || mem.Allocator.Error;

allocator: mem.Allocator,
tokens: lex.TokenList,
source: []const u8,
tags: []const lex.Token.Tag,
locs: []const lex.Token.Location,
current: usize = 0,

nodes: Ast.Node.List = .{},
literals: std.ArrayListUnmanaged([]const u8) = .{},

scratch: std.ArrayListUnmanaged(Ast.Node) = .{},

fn init(allocator: mem.Allocator, tokens: lex.TokenList, source: []const u8) Self {
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
        else => {
            return ParseError.UnknownStatement;
        },
    };
}

fn parseComment(self: *Self) void {
    _ = self;
}

fn parseFunction(self: *Self) ParseError!void {
    self.advance();
    const name = try self.parseFunctionName();

    const proto = try self.parseFunctionProto();

    if (!try self.consume(.left_brace)) {
        std.debug.print("expected '{{'\n", .{});
        return ParseError.UnexpectedToken;
    }

    while (self.currentToken() != .right_brace) {
        try self.parseStmt();
        self.advance();
    }

    var node: Ast.Node = .{ .tag = .func_decl, .data = .{}, .loc = name.loc };
    node.data.lhs = try self.addNode(name);
    node.data.rhs = try self.addNode(proto);

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

fn currentToken(self: *const Self) lex.Token.Tag {
    return self.tags[self.current];
}

fn consume(self: *Self, tag: lex.Token.Tag) !bool {
    if (self.current > self.tokens.len) {
        return ParseError.EndOfFile;
    }

    if (self.tags[self.current] != tag) {
        return false;
    }

    self.current += 1;
    return true;
}

fn takeToken(self: *Self, tag: lex.Token.Tag, comptime fmt: []const u8, args: anytype) ParseError!lex.Token.Location {
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

pub fn parse(allocator: mem.Allocator, tokens: lex.TokenList, source: []const u8) ParseError!Ast {
    var parser = Self.init(allocator, tokens, source);
    defer parser.deinit();
    try parser.setupAst();
    try parser.parseRoot();

    return Ast.init(parser.nodes, parser.literals);
}
