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

    InvalidUrl,
    InvalidEscapeSequence,
};

/// Convert a u21 (code point) to a string.
///
/// **WARN:** Caller is responsible for freeing the returned slice.
///
/// @param alloc The allocator to allocate the string to
/// @param code_point The code point to convert to a string
/// @return The utf-8 encoded string representation of the code point
fn codePointToString(alloc: std.mem.Allocator, code_point: u21) ![]const u8 {
    const buff = try alloc.alloc(u8, try std.unicode.utf8CodepointSequenceLength(code_point));
    errdefer alloc.free(buff);
    _ = try std.unicode.utf8Encode(code_point, buff);
    return buff;
}

/// Check if the code points are a valid escape sequence.
fn isValidEscape(code_points: *[2]?u21) bool {
    if (code_points[0] == null or code_points[1] == null) return false;
    if (code_points[0] != '\\' or code_points[1] == '\n') return false;
    return true;
}

fn isWhitespace(code_point: ?u21) bool {
    if (code_point == null) return false;
    return code_point == ' ' or code_point == '\t' or code_point == '\n';
}

fn isNonPrintable(code_point: u21) bool {
    return (code_point >= 0x0000 and code_point <= 0x0008) or (code_point >= 0x000E and code_point <= 0x001F) or code_point == 0x000B or code_point == 0x007F;
}

fn isHexDigit(code_point: u21) bool {
    switch (code_point) {
        '0'...'9', 'a'...'f', 'A'...'F' => return true,
        else => return false,
    }
}

fn isIdentStart(code_point: ?u21) bool {
    if (code_point == null) return false;
    const c = code_point.?;
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or (c >= 0x0080);
}

fn isIdent(code_point: ?u21) bool {
    if (code_point == null) return false;
    const c = code_point.?;
    return isIdentStart(c) or isDigit(c) or c == '-';
}

fn isDigit(code_point: ?u21) bool {
    if (code_point == null) return false;
    const c = code_point.?;
    return (c >= '0' and c <= '9');
}

fn isStartNumber(code_points: *[3]?u21) bool {
    if (code_points[0] == '-' or code_points[0] == '+') {
        return isDigit(code_points[1]) or (code_points[1] == '.' and isDigit(code_points[2]));
    }

    if (code_points[0] == '.') {
        return isDigit(code_points[1]);
    }

    return isDigit(code_points[0]);
}

fn isStartIdent(code_points: *[3]?u21) bool {
    if (code_points[0] == '-') {
        return (isIdentStart(code_points[1]) or code_points[1] == '-') or isValidEscape(code_points[1..]);
    }
    if (isIdentStart(code_points[0])) return true;
    if (isValidEscape(code_points[0..2])) return true;
    return false;
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

                    const buff = try buffer.toOwnedSlice();
                    const hex = try std.fmt.parseInt(u21, buff, 16);
                    self.alloc.free(buff);
                    buffer.deinit();

                    if (hex == 0 or hex > 0x10FFFF or isSurrogate(hex)) {
                        return "�";
                    } else {
                        return codePointToString(self.alloc, hex);
                    }
                },
                else => {
                    return try codePointToString(self.alloc, n);
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
        errdefer string.deinit();
        while (true) {
            const peek = self.stream.peek();
            if (peek) |p| {
                if (p == end) {
                    // End of string reached
                    _ = self.stream.next();
                    const s = (try self.strings.getOrPut(try string.toOwnedSlice())).key_ptr.*;
                    return .{ .string = s };
                } else if (p == '\n') {
                    // Line break, string is invalid and not closed
                    try self.parse_errors.append(error.UnclosedString);
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
                    const next = try codePointToString(self.alloc, self.stream.next().?);
                    defer self.alloc.free(next);
                    try string.appendSlice(next);
                }
            } else {
                try self.parse_errors.append(error.UnclosedString);
                const s = (try self.strings.getOrPut(try string.toOwnedSlice())).key_ptr.*;
                return .{ .string = s };
            }
        }
    }

    /// Consume the next ident token. The ident is cached for deduplication.
    ///
    /// @return Ident token
    /// @throws parse errors or allocation errors
    pub fn consumeIdent(self: *Self) ![]const u8 {
        var ident = std.ArrayList(u8).init(self.alloc);
        errdefer ident.deinit();
        while (self.stream.peek()) |p| {
            if (isIdent(p)) {
                const next = try codePointToString(self.alloc, self.stream.next().?);
                defer self.alloc.free(next);
                try ident.appendSlice(next);
            } else if (isValidEscape(self.stream.peek2())) {
                const escape = try self.consumeEscape();
                defer self.alloc.free(escape);
                try ident.appendSlice(escape);
            } else {
                break;
            }
        }

        try self.parse_errors.append(error.UnclosedString);
        const s = (try self.strings.getOrPut(try ident.toOwnedSlice())).key_ptr.*;
        return s;
    }

    /// Consume the next number token.
    ///
    /// @return Number, Percentage, or Dimension token
    /// @throws parse errors or allocation errors
    pub fn consumeNumber(self: *Self) !Number {
        var repr = std.ArrayList(u8).init(self.alloc);
        errdefer repr.deinit();
        const peek = self.stream.peek();
        var number = false;
        if (peek == '+' or peek == '-') {
            try repr.append(@intCast(self.stream.next().?));
        }

        while (self.stream.peek()) |p| {
            if (!isDigit(p)) break;
            try repr.append(@intCast(self.stream.next().?));
        }

        var n3 = self.stream.peek3();
        if (n3[0] == '.' and isDigit(n3[1])) {
            number = true;
            try repr.append('.');
            try repr.append(@intCast(n3[1].?));
            _ = self.stream.next2();
            while (self.stream.peek()) |p| {
                if (!isDigit(p)) break;
                try repr.append(@intCast(self.stream.next().?));
            }
        }

        n3 = self.stream.peek3();
        if (n3[0] == 'e' or n3[0] == 'E') {
            if ((n3[1] == '+' or n3[1] == '-') and isDigit(n3[2])) {
                const next3 = self.stream.next3();
                try repr.append(@intCast(next3[0].?));
                try repr.append(@intCast(next3[1].?));
                try repr.append(@intCast(next3[2].?));
                number = true;
                while (self.stream.peek()) |p| {
                    if (!isDigit(p)) break;
                    try repr.append(@intCast(self.stream.next().?));
                }
            } else if (isDigit(n3[1])) {
                const next2 = self.stream.next2();
                try repr.append(@intCast(next2[0].?));
                try repr.append(@intCast(next2[1].?));
                number = true;
                while (self.stream.peek()) |p| {
                    if (!isDigit(p)) break;
                    try repr.append(@intCast(self.stream.next().?));
                }
            }
        }

        if (number) {
            const num = try std.fmt.parseFloat(f64, repr.items);
            defer repr.deinit();
            const r = (try self.numbers.getOrPut(try repr.toOwnedSlice())).key_ptr.*;
            return .{ .repr = r, .value = .{ .number = num } };
        } else {
            const num = try std.fmt.parseInt(i64, repr.items, 10);
            defer repr.deinit();
            const r = (try self.numbers.getOrPut(try repr.toOwnedSlice())).key_ptr.*;
            return .{ .repr = r, .value = .{ .integer = num } };
        }
    }

    /// Consume the next numeric token.
    ///
    /// @return Number, Percentage, or Dimension token
    /// @throws parse errors or allocation errors
    pub fn consumeNumeric(self: *Self) !Token {
        const number = try self.consumeNumber();

        const n3 = self.stream.peek3();
        if (n3[0] == '%') {
            _ = self.stream.next();
            return .{ .percentage = number };
        }

        if (isStartIdent(n3)) {
            const ident = try self.consumeIdent();
            return .{ .dimension = .{ .value = number, .unit = ident } };
        }

        return .{ .number = number };
    }

    /// Consume the next bad url token.
    ///
    /// @return BadUrl token
    pub fn consumeBadUrl(self: *Self) Token {
        while (true) {
            if (self.stream.peek() == null or self.stream.peek() == ')') {
                _ = self.stream.next();
                return .{ .bad_url = {} };
            } else if (isValidEscape(self.stream.peek2())) {
                if (self.consumeEscape()) |escape| {
                    self.alloc.free(escape);
                } else |_| {}
            } else {
                _ = self.stream.next();
            }
        }
    }

    /// Consume the next url token.
    ///
    /// @return Url or BadUrl token
    /// @throws parse errors or allocation errors
    pub fn consumeUrl(self: *Self) !Token {
        var url = std.ArrayList(u8).init(self.alloc);
        errdefer url.deinit();

        while (isWhitespace(self.stream.peek())) {
            _ = self.stream.next();
        }

        while (self.stream.peek()) |p| {
            if (p == ')') {
                _ = self.stream.next();
                const val = try self.strings.getOrPut(try url.toOwnedSlice());
                defer url.deinit();
                return .{ .url = val.key_ptr.* };
            } else if (isWhitespace(p)) {
                _ = self.stream.next();
                while (isWhitespace(self.stream.peek())) {
                    _ = self.stream.next();
                }
                if (self.stream.peek() == null or self.stream.peek() == ')') {
                    const val = try self.strings.getOrPut(try url.toOwnedSlice());
                    defer url.deinit();
                    return .{ .url = val.key_ptr.* };
                }
                return self.consumeBadUrl();
            } else if (p == '"' or p == '\'' or p == '(' or isNonPrintable(p)) {
                try self.parse_errors.append(error.InvalidUrl);
                return self.consumeBadUrl();
            } else if (p == '\\') {
                if (isValidEscape(self.stream.peek2())) {
                    const escape = try self.consumeEscape();
                    defer self.alloc.free(escape);
                    try url.appendSlice(escape);
                } else {
                    try self.parse_errors.append(error.InvalidUrl);
                    return self.consumeBadUrl();
                }
            } else {
                const next = try codePointToString(self.alloc, self.stream.next().?);
                defer self.alloc.free(next);
                try url.appendSlice(next);
            }
        }

        try self.parse_errors.append(error.UnclosedUrl);
        const val = try self.strings.getOrPut(try url.toOwnedSlice());
        defer url.deinit();
        return .{ .url = val.key_ptr.* };
    }

    /// Consume the next ident-like token.
    ///
    /// @return Ident, Function, Url, or BadUrl token
    /// @throws parse errors or allocation errors
    pub fn consumeIdentLike(self: *Self) !Token {
        const ident = try self.consumeIdent();

        if (std.mem.eql(u8, ident, "url") and self.stream.peek() == '(') {
            _ = self.stream.next();
            var p2 = self.stream.peek2();
            while (isWhitespace(p2[0]) and isWhitespace(p2[1])) {
                _ = self.stream.next2();
                p2 = self.stream.peek2();
            }

            // TODO: Condition on ` '`, ` "`, `'`, or `"` consume function
            if ((isWhitespace(p2[0]) and (p2[1] == '"' or p2[1] == '\'')) or (p2[0] == '"' or p2[0] == '\'')) {
                return .{ .func = ident };
            }

            return try self.consumeUrl();
        }

        if (self.stream.peek() == '(') {
            _ = self.stream.next();
            return .{ .func = ident };
        }

        return .{ .ident = ident };
    }

    /// Consume the next token.
    ///
    /// @return null if end of file (EOF) is reached
    /// @throws parse errors or allocation errors
    pub fn consume(self: *Self) !?Token {
        try self.consumeComment();

        self.location.start = self.stream.loc();
        defer self.location.end = self.stream.loc();
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
                '#' => {
                    _ = self.stream.next();
                    if (isIdent(self.stream.peek()) or isValidEscape(self.stream.peek2())) {
                        if (isStartIdent(self.stream.peek3())) {
                            return .{ .hash = .{ .value = try self.consumeIdent(), .type = .Id } };
                        }
                        return .{ .hash = .{ .value = try self.consumeIdent(), .type = .Unrestricted } };
                    }
                    return .{ .delim = '#' };
                },
                '+' => {
                    if (isStartNumber(self.stream.peek3())) {
                        return try self.consumeNumeric();
                    }
                    return .{ .delim = self.stream.next().? };
                },
                '-' => {
                    if (isStartNumber(self.stream.peek3())) {
                        return try self.consumeNumeric();
                    }

                    if (std.mem.eql(?u21, self.stream.peek3(), &[_]?u21{ '-', '-', '>' })) {
                        _ = self.stream.next3();
                        return .{ .cdc = {} };
                    }

                    if (isStartIdent(self.stream.peek3())) {
                        return try self.consumeIdentLike();
                    }

                    return .{ .delim = self.stream.next().? };
                },
                '.' => {
                    if (isStartNumber(self.stream.peek3())) {
                        return try self.consumeNumeric();
                    }
                    return .{ .delim = self.stream.next().? };
                },
                '<' => {
                    _ = self.stream.next();
                    if (std.mem.eql(?u21, self.stream.peek3(), &[_]?u21{ '!', '-', '-' })) {
                        return .{ .cdo = {} };
                    }

                    return .{ .delim = '<' };
                },
                '@' => {
                    _ = self.stream.next();
                    if (isStartIdent(self.stream.peek3())) {
                        return .{ .at_keyword = try self.consumeIdent() };
                    }
                    return .{ .delim = '@' };
                },
                '\\' => {
                    if (isValidEscape(self.stream.peek2())) {
                        return try self.consumeIdentLike();
                    }
                    try self.parse_errors.append(error.InvalidEscapeSequence);
                    return .{ .delim = self.stream.next().? };
                },
                '0'...'9' => {
                    return try self.consumeNumeric();
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
                else => |p| {
                    if (isIdentStart(p)) {
                        return try self.consumeIdentLike();
                    }
                    return .{ .delim = self.stream.next().? };
                },
            }
        }

        return null;
    }
};
