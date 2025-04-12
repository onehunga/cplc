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
    self.reportError(fmt, args, source, location) catch unreachable;
}

fn reportError(self: *Self, comptime fmt: []const u8, args: anytype, source: []const u8, location: Lexer.Token.Location) !void {
    const loc = locationToSourceLocation(source, location);

    // Print the error message with formatting
    try self.writer.print("error: ", .{});
    try self.writer.print(fmt, args);
    try self.writer.print("\n", .{});

    // Find the line containing the error
    var start_of_line: usize = 0;
    if (loc.start > 0) {
        var i = loc.start - 1;
        while (i > 0) : (i -= 1) {
            if (source[i] == '\n') {
                start_of_line = i + 1;
                break;
            }
        }
    }

    // Find the end of that line
    var end_of_line = loc.start;
    while (end_of_line < source.len and source[end_of_line] != '\n') {
        end_of_line += 1;
    }

    // Print the location information
    try self.writer.print(" --> line {d}:{d}\n", .{ loc.line, loc.column });
    try self.writer.print("  |\n", .{});

    // Print the line number and the line content
    try self.writer.print("{d: >4} | {s}\n", .{ loc.line, source[start_of_line..end_of_line] });

    // Print the caret pointer line
    try self.writer.print("     | ", .{});

    // Print spaces up to the error position
    for (0..loc.column - 1) |_| {
        try self.writer.print(" ", .{});
    }

    // Print carets for the length of the error
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
        } else {
            column += 1;
        }
    }

    loc.line = line;
    loc.column = column;

    return loc;
}
