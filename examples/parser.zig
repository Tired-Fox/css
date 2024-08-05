const std = @import("std");
const css = @import("css");

pub fn main() !void {
    const s = "Hello, world! 😊\r\n";
    var parser = try css.Parser.init(s);

    var buffer: [4]u8 = undefined;
    while (try parser.nextSlice(&buffer)) |read| {
        std.debug.print("{s}", .{buffer[0..read]});
    }
}
