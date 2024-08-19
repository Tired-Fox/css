const std = @import("std");
const css = @import("css");

pub fn main() !void {
    // \n/* Sample CSS */\n'hello \\263A'p > a {\n  padding: 0;\n color: #F2CA4D;\n}\n
    // const source = "\r\n/* Sample CSS */\r\n'hello \\263A'p > a {\r\n  padding: 0;\r\n color: #F2CA4D;\r\n}\r\n";

    var file = try std.fs.cwd().openFile("examples/tokenizer/sample.css", .{});
    defer file.close();

    var src_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer src_arena.deinit();
    const buff = try file.readToEndAlloc(src_arena.allocator(), 99999);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var tokenizer = try css.Tokenizer.init(arena.allocator(), buff);
    while (try tokenizer.consume()) |token| {
        std.debug.print("{any} {any}\n", .{ token, tokenizer.loc() });
    }
}
