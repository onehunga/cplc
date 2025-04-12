const Self = @This();
const std = @import("std");
const Lexer = @import("../Lexer.zig");

const SourceLocation = struct {
    start: u32,
    end: u32,
    line: u32,
    column: u32,
};

writer: std.fs.File.Writer,

pub fn init(writer: std.fs.File.Writer) Self {
    return .{
        .writer = writer,
    };
}

pub fn report(self: *Self, comptime fmt: []const u8, args: anytype, source: []const u8, location: Lexer.Token.Location) void {
    // TODO: implement a better interning for source files including names and paths
    self.reportError(fmt, args, "file", source, location) catch unreachable;
}

fn reportError(self: *Self, comptime fmt: []const u8, args: anytype, filename: []const u8, source: []const u8, location: Lexer.Token.Location) !void {
    const loc = locationToSourceLocation(source, location);

    try self.writer.print("error: ", .{});
    try self.writer.print(fmt, args);
    try self.writer.print("\n", .{});

    var start_of_line: usize = 0;
    if (loc.start > 0) {
        var i = loc.start - 1;
        while (i > 0) : (i -= 1) {
            if (source[i] == '\n') {
                start_of_line = i + 1;
                break;
            }
            if (i == 0 and source[i] != '\n') {
                start_of_line = 0;
                break;
            }
        }
        if (loc.start == 0) {
            start_of_line = 0;
        }
    }

    var end_of_line = loc.start;
    while (end_of_line < source.len and source[end_of_line] != '\n') {
        end_of_line += 1;
    }

    try self.writer.writeAll("\x1b[31m");
    try self.writer.print("    --> {s}:{d}:{d}", .{ filename, loc.line, loc.column });
    try self.writer.writeAll("\x1b[0m\n");
    try self.writer.writeAll("     |\n");

    try self.writer.print("{d: >4} | ", .{loc.line});

    var visual_col: usize = 0;
    for (source[start_of_line..end_of_line]) |char| {
        if (char == '\t') {
            const spaces_to_add = 4 - (visual_col % 4);
            for (0..spaces_to_add) |_| {
                try self.writer.writeByte(' ');
            }
            visual_col += spaces_to_add;
        } else {
            try self.writer.writeByte(char);
            visual_col += 1;
        }
    }
    try self.writer.writeByte('\n');

    try self.writer.writeAll("     | ");

    var current_visual_column: usize = 0;
    var i: usize = start_of_line;
    while (i < loc.start) : (i += 1) {
        const char = source[i];
        if (char == '\t') {
            const spaces_to_add = 4 - (current_visual_column % 4);
            for (0..spaces_to_add) |_| {
                try self.writer.print(" ", .{});
            }
            current_visual_column += spaces_to_add;
        } else {
            try self.writer.print(" ", .{});
            current_visual_column += 1;
        }
    }

    const error_len = if (loc.end > loc.start) loc.end - loc.start else 1;

    try self.writer.print("^", .{});
    for (1..error_len) |_| {
        try self.writer.print("~", .{});
    }

    try self.writer.print("\n", .{});
}

fn locationToSourceLocation(source: []const u8, location: Lexer.Token.Location) SourceLocation {
    var loc: SourceLocation = undefined;
    loc.start = location.start;
    loc.end = location.end;

    var line: u32 = 1;
    var column: u32 = 1;

    for (source[0..location.start]) |char| {
        if (char == '\n') {
            line += 1;
            column = 1;
        } else if (char == '\t') {
            column += 4 - ((column - 1) % 4);
        } else {
            column += 1;
        }
    }

    loc.line = line;
    loc.column = column;

    return loc;
}
