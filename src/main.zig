const std = @import("std");
const assert = std.debug.assert;
const Lexer = @import("lexer.zig").Lexer;
const ast = @import("ast.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.argsWithAllocator(allocator) catch @panic("Failed to get args");
    defer args.deinit();

    // Skip the program name.
    assert(args.skip() == true);

    const program = args.next() orelse {
        std.debug.print("No program specified\n", .{});
        std.process.exit(1);
    };

    for (program) |c| {
        std.debug.print("{c}\n", .{c});
    }

    var lexer = Lexer.init(allocator, program);
    defer lexer.deinit();

    const node = ast.Node{ .row = 0, .col = 0, .type = .{ .Number = .{ .val = 1 } } };
    _ = node;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
