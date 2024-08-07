const std = @import("std");
const css = @import("css");

pub fn main() !void {
    const s = "Hello, world! 😊\r\n";
    var parser = try css.Parser.init(s);

    var buffer: [4]u8 = undefined;
    var location: usize = 0;
    while (try parser.nextSlice(&buffer)) |read| {
        const l = parser.loc();
        std.debug.print("{s} [{d}..{d}]\n", .{ buffer[0..read], location, l });
        location = l;
    }
}
