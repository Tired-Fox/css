const std = @import("std");
const css = @import("css");

pub fn main() !void {
    const source = "p > a {\r\n  color: red;\r\n}\r\n";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    _ = try css.Tokenizer.init(arena.allocator(), source);
}
