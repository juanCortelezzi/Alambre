const std = @import("std");
const ast = @import("ast.zig");
const token = @import("tokens.zig");
const assert = std.debug.assert;

const ParserErrType = error{
    InvalidToken,
    UnknownIdentifier,
    InvalidConversion,
    NotEnoughArguments,
};

const ParseErr = struct {
    type: ParserErrType,
    row: usize,
    col: usize,
};

const ParseResult = union(enum) {
    ok: ast.Node,
    err: ParseErr,
};

pub const Parser = struct {
    tokens: []const token.Token,
    program: std.ArrayList(ast.Node),
    cur: token.Token,
    read_pos: usize,

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token) Parser {
        assert(tokens.len > 0);
        return Parser{
            .tokens = tokens,
            .program = std.ArrayList(ast.Node).init(allocator),
            .cur = tokens[0],
            .read_pos = 1,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.program.deinit();
    }

    fn advance(self: *Parser) void {
        if (self.read_pos >= self.tokens.len) {
            return;
        }

        self.cur = self.tokens[self.read_pos];
        self.read_pos += 1;
    }

    fn parseNumber(self: *Parser) ParseResult {
        const tok = self.cur;
        const value = std.fmt.parseFloat(f64, tok.literal) catch {
            return ParseResult{ .err = ParseErr{
                .type = ParserErrType.InvalidConversion,
                .row = tok.row,
                .col = tok.col,
            } };
        };

        self.advance();

        return ParseResult{
            .ok = ast.Node{
                .type = .{ .Number = .{ .val = value } },
                .row = tok.row,
                .col = tok.col,
            },
        };
    }

    fn parseString(self: *Parser) ParseResult {
        const tok = self.cur;
        self.advance();
        return ParseResult{
            .ok = ast.Node{
                .type = .{ .String = .{ .val = tok.literal } },
                .row = tok.row,
                .col = tok.col,
            },
        };
    }

    fn parseBinOp(self: *Parser) ParseResult {
        const tok = self.cur;
        assert(tok.type == .Plus);
        assert(self.program.items.len == 2);
        if (self.program.items.len < 2) {
            return ParseResult{
                .err = ParseErr{
                    .type = ParserErrType.NotEnoughArguments,
                    .row = tok.row,
                    .col = tok.col,
                },
            };
        }

        const rhs = self.program.pop();
        const lhs = self.program.pop();

        assert(lhs.type == .Number);
        assert(rhs.type == .Number);
        assert(self.program.items.len == 0);

        const binop_type = switch (tok.type) {
            .Plus => ast.BinOpType.Add,
            .Minus => ast.BinOpType.Sub,
            else => @panic("invalid binop"),
        };

        assert(binop_type == .Add);

        self.advance();
        assert(self.cur.type == .EOF);

        return ParseResult{
            .ok = ast.Node{
                .type = .{ .BinOp = .{ .op = binop_type, .lhs = &lhs, .rhs = &rhs } },
                .row = tok.row,
                .col = tok.col,
            },
        };
    }

    fn parseExpression(self: *Parser) ParseResult {
        assert(self.cur.type == .EOF or self.cur.type == .Plus or self.cur.type == .Number);
        switch (self.cur.type) {
            .Number => return self.parseNumber(),
            .String => return self.parseString(),
            .Plus,
            .Minus,
            .Asterisk,
            .Slash,
            .LessThan,
            .LessThanEqual,
            .GreaterThan,
            .GreaterThanEqual,
            .Equal,
            .NotEqual,
            .And,
            .Or,
            .Not,
            => return self.parseBinOp(),
            else => return ParseResult{
                .err = ParseErr{
                    .type = ParserErrType.InvalidToken,
                    .row = self.cur.row,
                    .col = self.cur.col,
                },
            },
        }

        // t := p.currToken
        //
        // switch t.Type {
        // case token.LSquiggly:
        // 	node, err := p.parseTable()
        // 	return node, err
        //
        // case token.LParen:
        // 	node, err := p.parseFn()
        // 	return node, err
        //
        // case token.Ident:
        // 	node, err := p.parseIdent()
        // 	return node, err
        //
        // default:
        // 	return nil, fmt.Errorf("%w: token '%s' at pos '%d:%d'", ErrInvalidToken, t.Literal, t.Row, t.Col)
        // }
    }

    pub fn parse(self: *Parser) []ast.Node {
        const upper_bound = 5;
        var index: usize = 0;
        while (self.cur.type != .EOF and index < upper_bound) : (index += 1) {
            const nodeMaybe = self.parseExpression();
            switch (nodeMaybe) {
                .err => |err| {
                    std.debug.print("error at: {}:{}\n", .{ err.row, err.col });
                    @panic("Parsing error");
                },
                .ok => |node| {
                    if (index == 0 or index == 1) {
                        assert(node.type == .Number);
                    }

                    if (index == 2) {
                        assert(node.type == .BinOp);
                        assert(node.type.BinOp.op == .Add);
                        assert(node.type.BinOp.lhs.type == .Number);
                        assert(node.type.BinOp.rhs.type == .Number);
                    }
                    self.program.append(node) catch @panic("failed to append node");
                },
            }
        }
        if (index == upper_bound) {
            @panic("reached parser's upper bound");
        }

        return self.program.items;
    }
};

// test "advance" {
//     const allocator = std.testing.allocator;
//     const tokens = [_]token.Token{
//         .{ .type = .Number, .row = 0, .col = 0, .literal = "0" },
//         .{ .type = .Number, .row = 0, .col = 2, .literal = "123" },
//         .{ .type = .EOF, .row = 0, .col = 4, .literal = "" },
//     };
//
//     var parser = Parser.init(allocator, &tokens);
//     defer parser.deinit();
//
//     try std.testing.expectEqualDeep(tokens[0], parser.cur);
//     parser.advance();
//     try std.testing.expectEqualDeep(tokens[1], parser.cur);
//     parser.advance();
//     try std.testing.expectEqualDeep(tokens[2], parser.cur);
//     parser.advance();
//     try std.testing.expectEqualDeep(tokens[2], parser.cur);
// }
//
// test "parse_basics" {
//     const allocator = std.testing.allocator;
//     const tokens = [_]token.Token{
//         .{ .type = .Number, .row = 0, .col = 0, .literal = "0" },
//         .{ .type = .Number, .row = 0, .col = 2, .literal = "123" },
//         .{ .type = .String, .row = 0, .col = 6, .literal = "hello there" },
//         .{ .type = .EOF, .row = 0, .col = 17, .literal = "" },
//     };
//
//     const expected_ast = [_]ast.Node{
//         ast.Node{
//             .type = .{ .Number = .{ .val = 0 } },
//             .row = 0,
//             .col = 0,
//         },
//         ast.Node{
//             .type = .{ .Number = .{ .val = 123 } },
//             .row = 0,
//             .col = 2,
//         },
//         ast.Node{
//             .type = .{ .String = .{ .val = "hello there" } },
//             .row = 0,
//             .col = 6,
//         },
//     };
//
//     var parser = Parser.init(allocator, &tokens);
//     defer parser.deinit();
//
//     const program = parser.parse();
//     try std.testing.expectEqual(expected_ast.len, program.len);
//
//     var i: usize = 0;
//     while (i < expected_ast.len) : (i += 1) {
//         const expected = expected_ast[i].toString(allocator);
//         const got = program[i].toString(allocator);
//         try std.testing.expectEqualStrings(expected, got);
//         allocator.free(expected);
//         allocator.free(got);
//     }
// }

test "parse_advanced" {
    const allocator = std.testing.allocator;
    const tokens = [_]token.Token{
        .{ .type = .Number, .row = 0, .col = 0, .literal = "1" },
        .{ .type = .Number, .row = 0, .col = 2, .literal = "2" },
        .{ .type = .Plus, .row = 0, .col = 4, .literal = "+" },
        .{ .type = .EOF, .row = 0, .col = 4, .literal = "" },
    };

    // const expected_ast = [_]ast.Node{
    //     ast.Node{
    //         .type = .{
    //             .BinOp = .{
    //                 .op = .Add,
    //                 .lhs = &ast.Node{
    //                     .type = .{ .Number = .{ .val = 1 } },
    //                     .row = 0,
    //                     .col = 0,
    //                 },
    //                 .rhs = &ast.Node{
    //                     .type = .{ .Number = .{ .val = 2 } },
    //                     .row = 0,
    //                     .col = 2,
    //                 },
    //             },
    //         },
    //         .row = 0,
    //         .col = 4,
    //     },
    // };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const program = parser.parse();
    try std.testing.expectEqual(1, program.len);

    const node = program[0];
    switch (node.type) {
        .BinOp => |b| {
            try std.testing.expectEqual(ast.BinOpType.Add, b.op);
            switch (b.lhs.type) {
                .Number => |n| try std.testing.expectEqual(1, n.val),
                else => @panic("invalid lhs type"),
            }
            switch (b.rhs.type) {
                .Number => |n| try std.testing.expectEqual(2, n.val),
                else => @panic("invalid rhs type"),
            }
        },
        else => @panic("invalid node type"),
    }

    const string = node.toString(allocator);
    std.log.err("Node: {s}", .{string});
    allocator.free(string);
}
