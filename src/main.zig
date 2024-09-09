const std = @import("std");
const assert = std.debug.assert;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var args = std.process.argsWithAllocator(allocator) catch @panic("Failed to get args");
    defer args.deinit();

    // Skip the program name.
    assert(args.skip() == true);

    const program = args.next() orelse
        \\1 2 +
    ;
    // {
    //         // std.debug.print("No program specified\n", .{});
    //         // std.process.exit(1);
    //     };

    var lexerArena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer lexerArena.deinit();
    const lexerArenaAllocator = lexerArena.allocator();

    var lexer = Lexer.init(lexerArenaAllocator, program);
    defer lexer.deinit();

    var parserArena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer parserArena.deinit();
    const parserArenaAllocator = parserArena.allocator();

    const tokens = try lexer.getTokens();
    var parser = Parser.init(parserArenaAllocator, tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();

    for (parsed_ast.program) |node| {
        std.debug.print("node: {s}\n", .{node.toString(allocator)});
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
