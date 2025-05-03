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

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []const token.Token,
    program: std.ArrayList(ast.Node),
    indexes: std.ArrayList(u32),
    cur: token.Token,
    read_pos: usize,

    pub fn init(allocator: std.mem.Allocator, tokens: []const token.Token) Parser {
        assert(tokens.len > 0);
        return Parser{
            .allocator = allocator,
            .tokens = tokens,
            .program = std.ArrayList(ast.Node).init(allocator),
            .indexes = std.ArrayList(u32).init(allocator),
            .cur = tokens[0],
            .read_pos = 1,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.program.deinit();
        self.indexes.deinit();
    }

    fn advance(self: *Parser) void {
        if (self.read_pos >= self.tokens.len) {
            return;
        }

        self.cur = self.tokens[self.read_pos];
        self.read_pos += 1;
    }

    fn parseNumber(self: *Parser) ?ParseErr {
        const tok = self.cur;
        const value = std.fmt.parseFloat(f64, tok.literal) catch {
            return ParseErr{
                .type = ParserErrType.InvalidConversion,
                .row = tok.row,
                .col = tok.col,
            };
        };

        self.advance();

        const number = ast.Node{ .number = .{ .val = value } };

        self.program.append(number) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseString(self: *Parser) ?ParseErr {
        const tok = self.cur;
        self.advance();
        const string = ast.Node{
            .string = .{ .val = tok.literal },
        };

        self.program.append(string) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseBoolean(self: *Parser) ?ParseErr {
        const tok = self.cur;
        self.advance();
        const boolean = ast.Node{
            .boolean = .{ .val = std.mem.eql(u8, tok.literal, "true") },
        };

        self.program.append(boolean) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseBinOp(self: *Parser) ?ParseErr {
        const tok = self.cur;
        if (self.program.items.len < 2) {
            return ParseErr{
                .type = ParserErrType.NotEnoughArguments,
                .row = tok.row,
                .col = tok.col,
            };
        }

        const rhs_index = self.indexes.pop();
        const lhs_index = self.indexes.pop();

        const binop_type = switch (tok.type) {
            .Plus => ast.BinOpType.Add,
            .Minus => ast.BinOpType.Sub,
            .Asterisk => ast.BinOpType.Multiply,
            else => @panic("invalid binop"),
        };

        self.advance();

        const binop = ast.Node{
            .binop = .{ .op = binop_type, .lhs = lhs_index, .rhs = rhs_index },
        };

        self.program.append(binop) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseTable(self: *Parser) ?ParseErr {
        assert(self.cur.type == .LSquiggly);
        self.advance();
        const new_indices = std.ArrayList(u32).init(self.allocator);
        var newParser = Parser{
            .allocator = self.allocator,
            .tokens = self.tokens,
            .program = self.program,
            .indexes = new_indices,
            .cur = self.cur,
            .read_pos = self.read_pos,
        };

        std.debug.print("parsing table {any}\n", .{newParser.tokens[self.read_pos..]});

        // const table = ast.Node{ .table = .{ .items = []u32{} } };

        var index: usize = 0;
        const upper_bound = self.tokens.len - self.read_pos;
        while (self.cur.type != .EOF) : (index += 1) {
            std.debug.print("iteration: {}\n", .{index});
            if (index >= upper_bound) {
                @panic("reached table item upper bound");
            }
            if (newParser.parseExpression()) |err| {
                if (err.type != ParserErrType.InvalidToken) {
                    return err;
                }
                const tok = newParser.cur;
                if (tok.type == .Comma) {
                    std.debug.print("found our item, {any}\n", .{newParser.indexes.items});
                    newParser.advance();
                }
                if (tok.type == .RSquiggly) {
                    std.debug.print("found our table\n", .{});
                    newParser.advance();
                }
                std.debug.print("error invalid token '{any}' at {}:{}\n", .{ tok, err.row, err.col });
            }
        }
        return null;
    }

    fn parseExpression(self: *Parser) ?ParseErr {
        switch (self.cur.type) {
            .Number => return self.parseNumber(),
            .String => return self.parseString(),
            .Boolean => return self.parseBoolean(),
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
            => return self.parseBinOp(),
            .LSquiggly => return self.parseTable(),
            else => return ParseErr{
                .type = ParserErrType.InvalidToken,
                .row = self.cur.row,
                .col = self.cur.col,
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

    pub fn parse(self: *Parser) struct { program: []ast.Node, indexes: []u32 } {
        var index: usize = 0;
        const upper_bound = self.tokens.len;
        while (self.cur.type != .EOF) : (index += 1) {
            if (index >= upper_bound) {
                @panic("reached parser's upper bound");
            }
            if (self.parseExpression()) |err| {
                std.debug.print("error at: {}:{}\n", .{ err.row, err.col });
                @panic("Parsing error");
            }
        }

        return .{ .program = self.program.items, .indexes = self.indexes.items };
    }
};

test "advance" {
    const allocator = std.testing.allocator;
    const tokens = [_]token.Token{
        .{ .type = .Number, .row = 0, .col = 0, .literal = "0" },
        .{ .type = .Number, .row = 0, .col = 2, .literal = "123" },
        .{ .type = .EOF, .row = 0, .col = 4, .literal = "" },
    };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    try std.testing.expectEqualDeep(tokens[0], parser.cur);
    parser.advance();
    try std.testing.expectEqualDeep(tokens[1], parser.cur);
    parser.advance();
    try std.testing.expectEqualDeep(tokens[2], parser.cur);
    parser.advance();
    try std.testing.expectEqualDeep(tokens[2], parser.cur);
}

test "parse_basics" {
    const allocator = std.testing.allocator;
    const tokens = [_]token.Token{
        .{ .type = .Number, .row = 0, .col = 0, .literal = "0" },
        .{ .type = .Number, .row = 0, .col = 2, .literal = "123" },
        .{ .type = .String, .row = 0, .col = 6, .literal = "hello there" },
        .{ .type = .Boolean, .row = 0, .col = 18, .literal = "true" },
        .{ .type = .Boolean, .row = 0, .col = 23, .literal = "false" },
        .{ .type = .EOF, .row = 0, .col = 27, .literal = "" },
    };

    const expected_ast = [_]ast.Node{
        ast.Node{ .number = .{ .val = 0 } },
        ast.Node{ .number = .{ .val = 123 } },
        ast.Node{ .string = .{ .val = "hello there" } },
        ast.Node{ .boolean = .{ .val = true } },
        ast.Node{ .boolean = .{ .val = false } },
    };

    const expected_ast_indexes = [_]u32{ 0, 1, 2, 3, 4 };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();
    try std.testing.expectEqual(expected_ast.len, parsed_ast.program.len);
    try std.testing.expectEqual(expected_ast_indexes.len, parsed_ast.indexes.len);

    try std.testing.expectEqualSlices(u32, &expected_ast_indexes, parsed_ast.indexes);

    var i: usize = 0;
    while (i < expected_ast.len) : (i += 1) {
        const expected = expected_ast[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(expected);

        const got = parsed_ast.program[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(got);

        try std.testing.expectEqualStrings(expected, got);
    }
}

test "parse_table" {
    const allocator = std.testing.allocator;
    const tokens = [_]token.Token{
        .{ .type = .LSquiggly, .row = 0, .col = 0, .literal = "{" },
        .{ .type = .Number, .row = 0, .col = 1, .literal = "1" },
        .{ .type = .Comma, .row = 0, .col = 2, .literal = "," },
        .{ .type = .Number, .row = 0, .col = 3, .literal = "2" },
        .{ .type = .RSquiggly, .row = 0, .col = 4, .literal = "}" },
        .{ .type = .EOF, .row = 0, .col = 5, .literal = "" },
    };

    const expected_ast = [_]ast.Node{
        ast.Node{
            .table = .{
                .items = &[_]ast.Node{
                    ast.Node{ .number = .{ .val = 1 } },
                    ast.Node{ .number = .{ .val = 2 } },
                },
            },
        },
    };

    const expected_ast_indexes = [_]u32{0};

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();
    try std.testing.expectEqual(expected_ast.len, parsed_ast.program.len);
    try std.testing.expectEqual(expected_ast_indexes.len, parsed_ast.indexes.len);

    try std.testing.expectEqualSlices(u32, &expected_ast_indexes, parsed_ast.indexes);

    var i: usize = 0;
    while (i < expected_ast.len) : (i += 1) {
        const expected = expected_ast[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(expected);

        const got = parsed_ast.program[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(got);

        try std.testing.expectEqualStrings(expected, got);
    }
}

test "parse_advanced" {
    const allocator = std.testing.allocator;
    const tokens = [_]token.Token{
        .{ .type = .Number, .row = 0, .col = 0, .literal = "1" },
        .{ .type = .Number, .row = 0, .col = 2, .literal = "2" },
        .{ .type = .Plus, .row = 0, .col = 4, .literal = "+" },
        .{ .type = .Number, .row = 0, .col = 6, .literal = "3" },
        .{ .type = .Asterisk, .row = 0, .col = 8, .literal = "*" },
        .{ .type = .EOF, .row = 0, .col = 4, .literal = "" },
    };

    const expected_ast = [_]ast.Node{
        ast.Node{ .number = .{ .val = 1 } },
        ast.Node{ .number = .{ .val = 2 } },
        ast.Node{ .binop = .{ .op = .Add, .lhs = 0, .rhs = 1 } },
        ast.Node{ .number = .{ .val = 3 } },
        ast.Node{ .binop = .{ .op = .Multiply, .lhs = 2, .rhs = 3 } },
    };

    const expected_ast_indexes = [_]u32{4};

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();
    try std.testing.expectEqual(expected_ast.len, parsed_ast.program.len);
    try std.testing.expectEqual(expected_ast_indexes.len, parsed_ast.indexes.len);

    try std.testing.expectEqualSlices(u32, &expected_ast_indexes, parsed_ast.indexes);

    var i: usize = 0;
    while (i < expected_ast.len) : (i += 1) {
        const expected = expected_ast[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(expected);

        const got = parsed_ast.program[i].toString(allocator) catch @panic("failed to stringify ast");
        defer allocator.free(got);

        try std.testing.expectEqualStrings(expected, got);
    }
}
