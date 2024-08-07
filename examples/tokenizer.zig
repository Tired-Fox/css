const std = @import("std");
const css = @import("css");

pub fn main() !void {
    const source = "\r\n/* Sample CSS */\r\np > a {\r\n  color: red;\r\n}\r\n";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var tokenizer = try css.Tokenizer.init(arena.allocator(), source);
    if (try tokenizer.consume()) |token| {
        std.debug.print("{any}\n", .{token});
        std.debug.print("Span {any}\n", .{tokenizer.loc()});
    }

    if (try tokenizer.consume()) |token| {
        std.debug.print("{any}\n", .{token});
        std.debug.print("Span {any}\n", .{tokenizer.loc()});
    }
}
