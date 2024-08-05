const std = @import("std");

/// A parser for UTF-8 encoded css text.
///
/// Supports UTF-8, UTF-16LE and UTF-16BE encodings.
/// All these encodings are interpreted and parsed as UTF-8.
///
/// Per the [CSS Syntax Module Level 3](https://www.w3.org/TR/css-syntax-3/#input-byte-stream)
pub const Parser = struct {
    stream: std.unicode.Utf8Iterator,
    buffer: [3]?u21,

    pub fn init(stream: []const u8) !@This() {
        return .{
            .stream = (try std.unicode.Utf8View.init(stream)).iterator(),
            .buffer = [_]?u21{null} ** 3,
        };
    }

    /// Map `\f`, `\r` and `\r\n` to `\n`.
    ///
    /// Assumes that the stream can be advanced depending on the result.
    /// e.g. If the code point is 0x0D (`\r`) and the next code point is 0x0A (`\n`),
    /// the stream will be advanced to the next code point and `\n` will be returned.
    fn mapCodepoint(self: *@This(), codepoint: ?u21) ?u21 {
        if (codepoint) |cp| {
            return switch (cp) {
                0x0C => 0x0A,
                0x0D => {
                    if (self.stream.peek(1)[0] == 0x0A) {
                        _ = self.stream.nextCodepoint();
                    }
                    return 0x0A;
                },
                // Null
                0x00 => 0xFFFD,
                // Leading surrogate
                0xD800...0xDBFF => 0xFFFD,
                // Trailing surrogate
                0xDC00...0xDFFF => 0xFFFD,
                else => codepoint,
            };
        }
        return null;
    }

    /// Get the next code point.
    ///
    /// @return null if the end of the stream has been reached.
    pub fn next(self: *@This()) ?u21 {
        if (self.buffer[0]) |c| {
            // Shift buffer down
            self.buffer[0] = self.buffer[1];
            self.buffer[1] = self.buffer[2];
            self.buffer[2] = null;
            return c;
        }

        return self.mapCodepoint(self.stream.nextCodepoint());
    }

    /// Get the next code point as a utf8 byte sequence.
    ///
    /// @return Number of bytes written to the buffer.
    pub fn nextSlice(self: *@This(), buffer: []u8) !?u3 {
        if (self.next()) |cp| {
            const size = try std.unicode.utf8CodepointSequenceLength(cp);
            if (size > buffer.len) {
                return error.BufferTooSmall;
            }
            return try std.unicode.utf8Encode(cp, buffer);
        }
        return null;
    }

    /// Get the next two code points.
    ///
    /// Code points are null if the end of the stream has been reached.
    pub fn next2(self: *@This()) [2]?u21 {
        var result = [_]?u21{null} ** 2;

        if (self.buffer[0]) |c| {
            result[0] = c;
        } else {
            result[0] = self.mapCodepoint(self.stream.nextCodepoint());
        }

        if (self.buffer[1]) |c| {
            result[1] = c;
        } else {
            result[1] = self.mapCodepoint(self.stream.nextCodepoint());
        }

        self.buffer[0] = self.buffer[2];
        self.buffer[1] = null;
        self.buffer[2] = null;

        return result;
    }

    /// Get the next three code points.
    ///
    /// Code points are null if the end of the stream has been reached.
    pub fn next3(self: *@This()) [3]?u21 {
        var result = [_]?u21{null} ** 3;

        if (self.buffer[0]) |c| {
            result[0] = c;
        } else {
            result[0] = self.mapCodepoint(self.stream.nextCodepoint());
        }

        if (self.buffer[1]) |c| {
            result[1] = c;
        } else {
            result[1] = self.mapCodepoint(self.stream.nextCodepoint());
        }

        if (self.buffer[2]) |c| {
            result[2] = c;
        } else {
            result[2] = self.mapCodepoint(self.stream.nextCodepoint());
        }

        self.buffer[0] = null;
        self.buffer[1] = null;
        self.buffer[2] = null;

        return result;
    }

    /// Peek at the next code point.
    ///
    /// @return null if the end of the stream has been reached.
    pub fn peek(self: *@This()) ?u21 {
        if (self.buffer[0] == null) {
            self.buffer[0] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        return self.buffer[0];
    }

    /// Peek the next code point as a utf8 byte sequence.
    ///
    /// @return Number of bytes written to the buffer.
    pub fn peekSlice(self: *@This(), buffer: []u8) !?u3 {
        if (self.peek()) |cp| {
            const size = try std.unicode.utf8CodepointSequenceLength(cp);
            if (size > buffer.len) {
                return error.BufferTooSmall;
            }
            return try std.unicode.utf8Encode(cp, buffer);
        }
        return null;
    }

    /// Peek at the next two code point.
    ///
    /// Code points are null if the end of the stream has been reached.
    pub fn peek2(self: *@This()) *[2]?u21 {
        if (self.buffer[0] == null) {
            self.buffer[0] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        if (self.buffer[1] == null) {
            self.buffer[1] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        return self.buffer[0..2];
    }

    /// Peek at the next three code point.
    ///
    /// Code points are null if the end of the stream has been reached.
    pub fn peek3(self: *@This()) *[3]?u21 {
        if (self.buffer[0] == null) {
            self.buffer[0] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        if (self.buffer[1] == null) {
            self.buffer[1] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        if (self.buffer[2] == null) {
            self.buffer[2] = self.mapCodepoint(self.stream.nextCodepoint());
        }
        return self.buffer[0..];
    }
};
