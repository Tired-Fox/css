const std = @import("std");
const Parser = @import("parser.zig").Parser;

pub const Span = std.mem.Tuple(&.{ usize, usize });

pub const Token = union(enum) {
    ident: []const u8,
    func: []const u8,
    at_keyword: []const u8,
    string: []const u8,
    url: []const u8,
    whitespace: []const u8,

    delim: u21,
    percentage: f64,

    hash: struct { value: []const u8, type: enum {} },
    number: union(enum) {
        number: f64,
        integer: i64,
    },
    dimension: struct { value: f64, unit: []const u8 },

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
};

pub const Tokenizer = struct {
    alloc: std.mem.Allocator,
    stream: Parser,

    location: Span,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, stream: []const u8) !Self {
        return .{
            .alloc = alloc,
            .stream = try Parser.init(stream),
        };
    }

    /// Current span/location of the tokenizer in the source
    pub fn loc(self: *Self) Span {
        _ = self;
    }
};
