const Self = @This();
const std = @import("std");
const ascii = std.ascii;
const mem = std.mem;

pub const TokenList = std.MultiArrayList(Token);

const KEYWORDS = std.StaticStringMap(Token.Tag).initComptime(.{
    .{ "import", .import },
    .{ "struct", .@"struct" },
    .{ "func", .func },
    .{ "var", .@"var" },
    .{ "undefined", .undefined },
    .{ "return", .@"return" },
    .{ "for", .@"for" },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "switch", .@"switch" },
    .{ "true", .true },
    .{ "false", .false },
});

source: []const u8,
tokens: TokenList = .{},
allocator: mem.Allocator,

ch: u8 = 0,
pos: u32 = 0,

pub fn init(source: []const u8, allocator: mem.Allocator) Self {
    return Self{
        .source = source,
        .allocator = allocator,
    };
}

fn advance(self: *Self) void {
    if (self.pos < self.source.len) {
        self.ch = self.source[self.pos];
        self.pos += 1;
    } else {
        self.ch = 0;
    }
}

fn next(self: *Self) !void {
    self.advance();
    try self.skipWhitespace();

    if (ascii.isDigit(self.ch)) {
        try self.number();
        return;
    }
    if (ascii.isAlphabetic(self.ch)) {
        try self.identifier();
        return;
    }

    try switch (self.ch) {
        '(' => self.singleToken(.left_paren),
        ')' => self.singleToken(.right_paren),
        '{' => self.singleToken(.left_brace),
        '}' => self.singleToken(.right_brace),
        '[' => self.singleToken(.left_bracket),
        ']' => self.singleToken(.right_bracket),
        '+' => self.singleToken(.plus),
        '-' => self.singleToken(.minus),
        '*' => self.singleToken(.star),
        '/' => self.singleToken(.slash),
        '.' => self.singleToken(.dot),
        ',' => self.singleToken(.comma),
        ':' => self.singleToken(.colon),
        ';' => self.singleToken(.semicolon),
        '=' => self.doubleToken('=', .assign, .equal),
        '!' => self.doubleToken('=', .bang, .not_equal),
        '<' => self.doubleToken('=', .less_than, .less_equal),
        '>' => self.doubleToken('=', .greater_than, .greater_equal),
        else => {},
    };
}

/// handles all non code characters
fn skipWhitespace(self: *Self) !void {
    while (true) {
        switch (self.ch) {
            '/' => {
                if (self.peek() == '/') {
                    try self.comment();
                } else {
                    return;
                }
            },
            ' ', '\t', '\n', '\r' => self.advance(),
            else => return,
        }
    }
}

fn comment(self: *Self) !void {
    const start = self.pos - 1;

    while (self.ch != '\n') {
        self.advance();
    }

    try self.tokens.append(self.allocator, .{
        .tag = .comment,
        .location = .{ .start = start, .end = self.pos },
    });
}

fn number(self: *Self) !void {
    const start = self.pos - 1;
    var is_float = false;

    while (ascii.isDigit(self.peek())) {
        self.advance();
    }
    if (self.peek() == '.') {
        self.advance();
        is_float = true;
        while (ascii.isDigit(self.peek())) {
            self.advance();
        }
    }

    try self.tokens.append(self.allocator, .{
        .tag = if (is_float) .float else .int,
        .location = .{ .start = start, .end = self.pos },
    });
}

fn identifier(self: *Self) !void {
    const start = self.pos - 1;

    while (ascii.isAlphanumeric(self.peek())) {
        self.advance();
    }

    const span = self.source[start..self.pos];
    const tag = KEYWORDS.get(span) orelse .ident;

    try self.tokens.append(self.allocator, .{
        .tag = tag,
        .location = .{ .start = start, .end = self.pos },
    });
}

fn singleToken(self: *Self, tag: Token.Tag) !void {
    try self.tokens.append(self.allocator, .{
        .tag = tag,
        .location = self.location(1),
    });
}

fn doubleToken(self: *Self, expected: u8, single: Token.Tag, double: Token.Tag) !void {
    if (self.peek() == expected) {
        self.advance();
        try self.tokens.append(self.allocator, .{
            .tag = double,
            .location = self.location(2),
        });
    } else {
        try self.tokens.append(self.allocator, .{
            .tag = single,
            .location = self.location(1),
        });
    }
}

fn location(self: *const Self, length: u32) Token.Location {
    return Token.Location{
        .start = self.pos - length,
        .end = self.pos,
    };
}

fn peek(self: *const Self) u8 {
    if (self.pos < self.source.len) {
        return self.source[self.pos];
    } else {
        return 0;
    }
}

pub const Token = struct {
    tag: Tag,
    location: Location,

    pub const Location = struct {
        start: u32,
        end: u32,

        pub fn span(start: Location, end: Location) Location {
            return Location{
                .start = start.start,
                .end = end.end,
            };
        }
    };

    pub const Tag = enum {
        left_paren,
        right_paren,
        left_brace,
        right_brace,
        left_bracket,
        right_bracket,
        plus,
        minus,
        star,
        slash,
        dot,
        comma,
        colon,
        semicolon,
        assign,
        equal,
        bang,
        not_equal,
        less_than,
        less_equal,
        greater_than,
        greater_equal,
        comment,
        int,
        float,
        ident,
        import,
        @"struct",
        func,
        @"var",
        undefined,
        @"return",
        @"for",
        @"if",
        @"else",
        @"switch",
        true,
        false,
    };
};

pub fn lex(source: []const u8, allocator: mem.Allocator) !TokenList {
    var lexer = Self.init(source, allocator);

    while (lexer.pos < lexer.source.len) {
        try lexer.next();
    }

    return lexer.tokens;
}

test "single tokens" {
    const allocator = std.testing.allocator;
    const source = "(){}[]+-*/.,:;";
    const expected = [_]Token{
        .{ .tag = .left_paren, .location = .{ .start = 0, .end = 1 } },
        .{ .tag = .right_paren, .location = .{ .start = 1, .end = 2 } },
        .{ .tag = .left_brace, .location = .{ .start = 2, .end = 3 } },
        .{ .tag = .right_brace, .location = .{ .start = 3, .end = 4 } },
        .{ .tag = .left_bracket, .location = .{ .start = 4, .end = 5 } },
        .{ .tag = .right_bracket, .location = .{ .start = 5, .end = 6 } },
        .{ .tag = .plus, .location = .{ .start = 6, .end = 7 } },
        .{ .tag = .minus, .location = .{ .start = 7, .end = 8 } },
        .{ .tag = .star, .location = .{ .start = 8, .end = 9 } },
        .{ .tag = .slash, .location = .{ .start = 9, .end = 10 } },
        .{ .tag = .dot, .location = .{ .start = 10, .end = 11 } },
        .{ .tag = .comma, .location = .{ .start = 11, .end = 12 } },
        .{ .tag = .colon, .location = .{ .start = 12, .end = 13 } },
        .{ .tag = .semicolon, .location = .{ .start = 13, .end = 14 } },
    };

    var tokens = try lex(source, allocator);
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(expected.len, tokens.len);

    for (expected, 0..) |e, i| {
        const actual = tokens.get(i);
        try std.testing.expectEqual(e.tag, actual.tag);
        try std.testing.expectEqual(e.location, actual.location);
    }
}

test "double tokens" {
    const allocator = std.testing.allocator;
    const source = "!=<= = == >=";
    const expected = [_]Token{
        .{ .tag = .not_equal, .location = .{ .start = 0, .end = 2 } },
        .{ .tag = .less_equal, .location = .{ .start = 2, .end = 4 } },
        .{ .tag = .assign, .location = .{ .start = 5, .end = 6 } },
        .{ .tag = .equal, .location = .{ .start = 7, .end = 9 } },
        .{ .tag = .greater_equal, .location = .{ .start = 10, .end = 12 } },
    };

    var tokens = try lex(source, allocator);
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(expected.len, tokens.len);

    for (expected, 0..) |e, i| {
        const actual = tokens.get(i);
        try std.testing.expectEqual(e.tag, actual.tag);
        try std.testing.expectEqual(e.location, actual.location);
    }
}

test "identifiers" {
    const allocator = std.testing.allocator;
    const source = "main func return for if else switch";
    const expected = [_]Token{
        .{ .tag = .ident, .location = .{ .start = 0, .end = 4 } },
        .{ .tag = .func, .location = .{ .start = 5, .end = 9 } },
        .{ .tag = .@"return", .location = .{ .start = 10, .end = 16 } },
        .{ .tag = .@"for", .location = .{ .start = 17, .end = 20 } },
        .{ .tag = .@"if", .location = .{ .start = 21, .end = 23 } },
        .{ .tag = .@"else", .location = .{ .start = 24, .end = 28 } },
        .{ .tag = .@"switch", .location = .{ .start = 29, .end = 35 } },
    };

    var tokens = try lex(source, allocator);
    defer tokens.deinit(allocator);

    try std.testing.expectEqual(expected.len, tokens.len);

    for (expected, 0..) |e, i| {
        const actual = tokens.get(i);
        try std.testing.expectEqual(e.tag, actual.tag);
        try std.testing.expectEqual(e.location, actual.location);
    }
}
