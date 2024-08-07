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

pub const Tokenizer = struct {
    alloc: std.mem.Allocator,
    stream: Parser,

    location: Span,

    ident: std.ArrayList([]const u8),
    hash: std.ArrayList([]const u8),
    number: std.ArrayList([]const u8),
    string: std.ArrayList([]const u8),
    whitespace: std.ArrayList([]const u8),

    const Self = @This();

    /// Create a new tokenizer that will allocate memory for cached/shared data for idents, strings, etc.
    ///
    /// The memory is allocated since CSS Syntax Module Level 3 calls for the input to be parsed and transformed.
    pub fn init(alloc: std.mem.Allocator, stream: []const u8) !Self {
        return .{
            .stream = try Parser.init(stream),
            .location = .{ .start = 0, .end = 0 },

            .ident = std.ArrayList([]const u8).init(alloc),
            .hash = std.ArrayList([]const u8).init(alloc),
            .number = std.ArrayList([]const u8).init(alloc),
            .string = std.ArrayList([]const u8).init(alloc),
            .whitespace = std.ArrayList([]const u8).init(alloc),

            .alloc = alloc,
        };
    }

    /// Free all allocated memory for tokens.
    ///
    /// # Important
    /// Only call if you wish to free all allocated memory. Any references and uses of the token string slices
    /// will become invalid.
    pub fn deinit(self: *Self) void {
        self.ident.deinit();
        self.hash.deinit();
        self.number.deinit();
        self.string.deinit();
        self.whitespace.deinit();
    }

    /// Current span/location of the tokenizer in the source
    ///
    /// @return Span of the current location in the source where the last parsed token is located
    pub fn loc(self: *Self) Span {
        return self.location;
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
                    return error.CommentMissingEnd;
                } else if (peek[1] == null) {
                    _ = self.stream.next();
                    return error.CommentMissingEnd;
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
        try self.whitespace.append(try whitespace.toOwnedSlice());
        return .{ .whitespace = self.whitespace.items[self.whitespace.items.len - 1] };
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
                else => {
                    self.location.end = self.stream.loc() + 1;
                    return .{ .delim = self.stream.next().? };
                },
            }
        }

        return null;
    }
};
