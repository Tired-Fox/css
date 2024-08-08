const std = @import("std");
const Parser = @import("parser.zig").Parser;

pub const Span = struct {
    start: usize,
    end: usize,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("[{d}..{d}]", .{ self.start, self.end });
    }
};

pub const Number = struct {
    repr: []const u8,
    value: union(enum) {
        number: f64,
        integer: i64,
    },

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("Number {{ repr: '{s}', ", .{self.repr});
        switch (self.value) {
            .number => |n| try writer.print("type: number, value: {d} }}", .{n}),
            .integer => |i| try writer.print("type: integer, value: {d} }}", .{i}),
        }
    }
};

pub const Token = union(enum) {
    ident: []const u8,
    func: []const u8,
    at_keyword: []const u8,
    string: []const u8,
    url: []const u8,
    whitespace: []const u8,

    delim: u21,
    percentage: Number,

    hash: struct { value: []const u8, type: enum { Id, Unrestricted } },
    number: Number,
    dimension: struct { value: Number, unit: []const u8 },

    bad_string: void,
    bad_url: void,

    cdo: void,
    cdc: void,

    colon: void,
    semicolon: void,
    comma: void,
    @"[": void,
    @"]": void,
    @"(": void,
    @")": void,
    @"{": void,
    @"}": void,

    fn writeStringRepr(string: []const u8, writer: anytype) !void {
        try writer.writeAll("'");
        for (string) |c| {
            switch (c) {
                '\n' => try writer.writeAll("\\n"),
                '\t' => try writer.writeAll("\\t"),
                '\'' => try writer.writeAll("\\'"),
                else => try writer.writeAll(&[_]u8{c}),
            }
        }
        try writer.writeAll("'");
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .delim => |c| {
                try writer.writeAll("Delim { ");
                switch (c) {
                    '\n' => try writer.writeAll("\\n"),
                    '\t' => try writer.writeAll("\\t"),
                    else => {
                        var buffer: [4]u8 = undefined;
                        const read = std.unicode.utf8Encode(c, &buffer) catch unreachable;
                        try writer.print("{s}", .{buffer[0..read]});
                    },
                }
                try writer.writeAll(" }");
            },
            .whitespace => |s| {
                try writer.writeAll("Whitespace { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .ident => |s| {
                try writer.writeAll("Ident { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .func => |s| {
                try writer.writeAll("Func { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .at_keyword => |s| {
                try writer.writeAll("AtKeyword { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .string => |s| {
                try writer.writeAll("String { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .url => |s| {
                try writer.writeAll("Url { ");
                try writeStringRepr(s, writer);
                try writer.writeAll(" }");
            },
            .percentage => |p| {
                try writer.print("Percentage {{ {any} }}", .{p});
            },
            .hash => |hash| {
                try writer.writeAll("Hash { value: ");
                try writeStringRepr(hash.value, writer);
                try writer.print(", type: {s} }}", .{@tagName(hash.type)});
            },
            .number => |num| try writer.print("{any}", .{num}),
            .dimension => |dim| try writer.print("Dimension {{ value: {any}, unit: {s} }}", .{ dim.value, dim.unit }),
            .bad_string => try writer.writeAll("BadString"),
            .bad_url => try writer.writeAll("BadUrl"),
            .cdo => try writer.writeAll("CDO('<!--')"),
            .cdc => try writer.writeAll("CDC('-->')"),
            .colon => try writer.writeAll("Colon"),
            .semicolon => try writer.writeAll("Semicolon"),
            .comma => try writer.writeAll("Comma"),
            .@"[" => try writer.writeAll("OpenBracket"),
            .@"]" => try writer.writeAll("CloseBracket"),
            .@"(" => try writer.writeAll("OpenParen"),
            .@")" => try writer.writeAll("CloseParen"),
            .@"{" => try writer.writeAll("OpenBrace"),
            .@"}" => try writer.writeAll("CloseBrace"),
        }
    }
};

const ParseError = error{
    UnclosedComment,
    UnclosedString,
    UnclosedUrl,

    InvalidEscapeSequence,
};

/// Check if the code points are a valid escape sequence.
fn isValidEscape(code_points: *[2]?u21) bool {
    if (code_points[0] == null or code_points[1] == null) return false;
    if (code_points[0] != '\\' or code_points[1] == '\n') return false;
    return true;
}

fn isWhitespace(code_point: u21) bool {
    return code_point == ' ' or code_point == '\t' or code_point == '\n';
}

fn isHexDigit(code_point: u21) bool {
    switch (code_point) {
        '0'...'9', 'a'...'f', 'A'...'F' => return true,
        else => return false,
    }
}

fn isSurrogate(code_point: u32) bool {
    return (code_point >= 0xD800 and code_point <= 0xDBFF) or (code_point >= 0xDC00 and code_point <= 0xDFFF);
}

pub const Tokenizer = struct {
    alloc: std.mem.Allocator,
    stream: Parser,

    location: Span,

    idents: std.StringHashMap(void),
    hashes: std.StringHashMap(void),
    numbers: std.StringHashMap(void),
    strings: std.StringHashMap(void),
    whitespace: std.StringHashMap(void),

    parse_errors: std.ArrayList(ParseError),

    const Self = @This();

    /// Create a new tokenizer that will allocate memory for cached/shared data for idents, strings, etc.
    ///
    /// The memory is allocated since CSS Syntax Module Level 3 calls for the input to be parsed and transformed.
    pub fn init(alloc: std.mem.Allocator, stream: []const u8) !Self {
        return .{
            .stream = try Parser.init(stream),
            .location = .{ .start = 0, .end = 0 },
            .parse_errors = std.ArrayList(ParseError).init(alloc),

            .idents = std.StringHashMap(void).init(alloc),
            .hashes = std.StringHashMap(void).init(alloc),
            .numbers = std.StringHashMap(void).init(alloc),
            .strings = std.StringHashMap(void).init(alloc),
            .whitespace = std.StringHashMap(void).init(alloc),

            .alloc = alloc,
        };
    }

    /// Free all allocated memory for tokens.
    ///
    /// # Important
    /// Only call if you wish to free all allocated memory. Any references and uses of the token string slices
    /// will become invalid.
    pub fn deinit(self: *Self) void {
        // TODO PERF: Check if the stored string slices are also freed or if only the pointers are freed
        //      if not then iterate each key freeing the slices
        //      - Check this by using general purpose allocator instead of arena allocator
        self.idents.deinit();
        self.hashes.deinit();
        self.numbers.deinit();
        self.strings.deinit();
        self.whitespace.deinit();
    }

    /// Current span/location of the tokenizer in the source
    ///
    /// @return Span of the current location in the source where the last parsed token is located
    pub fn loc(self: *Self) Span {
        return self.location;
    }

    pub fn containsValidEscape(self: *Self) bool {
        const p2 = self.stream.peek2();
        return isValidEscape(p2);
    }

    /// Consume a comment if the next two code points are `/*`.
    ///
    /// @throws parse error if the comment is missing the closing `*/` code points
    pub fn consumeComment(self: *Self) !void {
        if (std.mem.eql(?u21, self.stream.peek2(), &[_]?u21{ '/', '*' })) {
            _ = self.stream.next2();
            while (true) {
                const peek = self.stream.peek2();
                if (std.mem.eql(?u21, peek, &[_]?u21{ '*', '/' })) {
                    _ = self.stream.next2();
                    break;
                } else if (peek[0] == null) {
                    try self.parse_errors.append(error.UnclosedComment);
                    return;
                } else if (peek[1] == null) {
                    _ = self.stream.next();
                    try self.parse_errors.append(error.UnclosedComment);
                    return;
                } else {
                    _ = self.stream.next();
                }
            }
        }
    }

    /// Consume all whitespace in the buffer until the next non-whitespace code point.
    ///
    /// @return the consumed whitespace
    /// @throws allocation errors
    pub fn consumeWhitespace(self: *Self) !Token {
        var whitespace = std.ArrayList(u8).init(self.alloc);
        defer whitespace.deinit();
        var peek = self.stream.peek();
        var buffer: [4]u8 = undefined;

        while (peek == ' ' or peek == '\t' or peek == '\n') {
            const read = try self.stream.nextSlice(&buffer);
            if (read) |r| {
                try whitespace.appendSlice(buffer[0..@intCast(r)]);
                peek = self.stream.peek();
            } else {
                break;
            }
        }

        self.location.end = self.stream.loc();
        const ws = (try self.whitespace.getOrPut(try whitespace.toOwnedSlice())).key_ptr.*;
        return .{ .whitespace = ws };
    }

    /// Consume an escape sequence.
    ///
    /// **WARN:** Caller is responsible for freeing the returned slice.
    ///
    /// @return the consumed escape sequence
    /// @throws allocation errors when the space for parsing the hex code point or byte representation
    ///     is not available
    pub fn consumeEscape(self: *Self) ![]const u8 {
        const next = self.stream.next();
        if (next) |n| {
            switch (n) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    var buffer = try std.ArrayList(u8).initCapacity(self.alloc, 6);
                    try buffer.append(@intCast(n));
                    for (0..5) |_| {
                        const peek = self.stream.peek();
                        if (peek) |p| {
                            if (isWhitespace(p)) {
                                _ = self.stream.next();
                                break;
                            } else if (isHexDigit(p)) {
                                try buffer.append(@intCast(self.stream.next().?));
                            }
                        } else {
                            break;
                        }
                    }

                    self.location.end = self.stream.loc();

                    var buff = try buffer.toOwnedSlice();
                    const hex = try std.fmt.parseInt(u21, buff, 16);
                    self.alloc.free(buff);
                    buffer.deinit();

                    if (hex == 0 or hex > 0x10FFFF or isSurrogate(hex)) {
                        return "�";
                    } else {
                        buff = try self.alloc.alloc(u8, try std.unicode.utf8CodepointSequenceLength(hex));
                        _ = try std.unicode.utf8Encode(hex, buff);
                        return buff;
                    }
                },
                else => {
                    const buffer = try self.alloc.alloc(u8, try std.unicode.utf8CodepointSequenceLength(n));
                    _ = try std.unicode.utf8Encode(n, buffer);
                    return buffer;
                },
            }
        } else {
            try self.parse_errors.append(error.InvalidEscapeSequence);
            return "�";
        }
        switch (self.stream.next()) {}
    }

    /// Consumes a string token until the end code point is reached.
    ///
    /// @return A `String` or `BadString` token
    /// @throws parse error when the string is missing the closing code point
    pub fn consumeString(self: *Self, end: u21) !Token {
        var string = std.ArrayList(u8).init(self.alloc);
        while (true) {
            const peek = self.stream.peek();
            if (peek) |p| {
                if (p == end) {
                    // End of string reached
                    _ = self.stream.next();
                    const s = (try self.strings.getOrPut(try string.toOwnedSlice())).key_ptr.*;
                    self.location.end = self.stream.loc();
                    return .{ .string = s };
                } else if (p == '\n') {
                    // Line break, string is invalid and not closed
                    try self.parse_errors.append(error.UnclosedString);
                    self.location.end = self.stream.loc();
                    return .{ .bad_string = {} };
                } else if (p == '\\') {
                    // Consume an escaped character regardless of what it is.
                    // Also translates unicode escape sequences to UTF-8 code points.
                    const p2 = self.stream.peek2();
                    if (p2[1] == null) {
                        continue;
                    } else if (p2[1] == '\n') {
                        _ = self.stream.next();
                        string.append('\n') catch unreachable;
                    } else if (isValidEscape(p2)) {
                        _ = self.stream.next();
                        const escape = try self.consumeEscape();
                        defer self.alloc.free(escape);
                        try string.appendSlice(escape);
                    }
                } else {
                    const next = self.stream.next().?;
                    const buff = try self.alloc.alloc(u8, try std.unicode.utf8CodepointSequenceLength(next));
                    _ = try std.unicode.utf8Encode(next, buff);
                    try string.appendSlice(buff);
                    self.alloc.free(buff);
                }
            } else {
                try self.parse_errors.append(error.UnclosedString);
                const s = (try self.strings.getOrPut(try string.toOwnedSlice())).key_ptr.*;
                self.location.end = self.stream.loc();
                return .{ .string = s };
            }
        }
    }

    /// Consume the next token.
    ///
    /// @return null if end of file (EOF) is reached
    /// @throws parse errors or allocation errors
    pub fn consume(self: *Self) !?Token {
        try self.consumeComment();

        self.location.start = self.stream.loc();
        if (self.stream.peek()) |peek| {
            switch (peek) {
                '\n', '\t', ' ' => {
                    return try self.consumeWhitespace();
                },
                '\'' => {
                    _ = self.stream.next();
                    return try self.consumeString('\'');
                },
                '"' => {
                    _ = self.stream.next();
                    return try self.consumeString('"');
                },
                '}' => {
                    _ = self.stream.next();
                    return .{ .@"}" = {} };
                },
                '{' => {
                    _ = self.stream.next();
                    return .{ .@"{" = {} };
                },
                ':' => {
                    _ = self.stream.next();
                    return .{ .colon = {} };
                },
                ';' => {
                    _ = self.stream.next();
                    return .{ .semicolon = {} };
                },
                ',' => {
                    _ = self.stream.next();
                    return .{ .comma = {} };
                },
                '[' => {
                    _ = self.stream.next();
                    return .{ .@"[" = {} };
                },
                ']' => {
                    _ = self.stream.next();
                    return .{ .@"]" = {} };
                },
                '(' => {
                    _ = self.stream.next();
                    return .{ .@"(" = {} };
                },
                ')' => {
                    _ = self.stream.next();
                    return .{ .@")" = {} };
                },
                else => {
                    self.location.end = self.stream.loc() + 1;
                    return .{ .delim = self.stream.next().? };
                },
            }
        }

        return null;
    }
};
