const std = @import("std");
const css = @import("css");

pub fn main() !void {
    const source = "\r\n/* Sample CSS */\r\n'hello \\263A'p > a {\r\n  color: red;\r\n}\r\n";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var tokenizer = try css.Tokenizer.init(arena.allocator(), source);
    while (try tokenizer.consume()) |token| {
        std.debug.print("{any} {any}\n", .{ token, tokenizer.loc() });
    }
}
