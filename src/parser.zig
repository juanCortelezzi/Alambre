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
            return  ParseErr{
                .type = ParserErrType.InvalidConversion,
                .row = tok.row,
                .col = tok.col,
            };
        };

        self.advance();

        const number = ast.Node{
            .type = .{ .Number = .{ .val = value } },
        };
        
        self.program.append(number) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
    return null;
        
    }

    fn parseString(self: *Parser) ?ParseErr {
        const tok = self.cur;
        self.advance();
            const string = ast.Node{
                .type = .{ .String = .{ .val = tok.literal } },
            };
        
        self.program.append(string) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseBinOp(self: *Parser) ?ParseErr {
        const tok = self.cur;
        if (self.program.items.len < 2) {
            return 
                ParseErr{
                    .type = ParserErrType.NotEnoughArguments,
                    .row = tok.row,
                    .col = tok.col,
            };
        }

        const rhs_index = self.indexes.pop();
        const lhs_index = self.indexes.pop();
        const rhs = self.program.items[rhs_index];
        const lhs = self.program.items[lhs_index];

        assert(lhs.type == .Number);
        assert(rhs.type == .Number);

        const binop_type = switch (tok.type) {
            .Plus => ast.BinOpType.Add,
            .Minus => ast.BinOpType.Sub,
            else => @panic("invalid binop"),
        };

        self.advance();

        const binop = ast.Node{
            .type = .{ .BinOp = .{ .op = binop_type, .lhs = lhs_index, .rhs = rhs_index } },
        };
        
        self.program.append(binop) catch @panic("failed to append node");
        self.indexes.append(@intCast(self.program.items.len - 1)) catch @panic("failed to append index");
        return null;
    }

    fn parseExpression(self: *Parser) ?ParseErr {
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
            else => return 
                ParseErr{
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

    pub fn parse(self: *Parser) struct {
        program :[]ast.Node,
        indexes :[]u32
    } {
        const upper_bound = 5;
        var index: usize = 0;
        while (self.cur.type != .EOF and index < upper_bound) : (index += 1) {
            const nodeMaybe = self.parseExpression();
            if (nodeMaybe) |err| {
                std.debug.print("error at: {}:{}\n", .{ err.row, err.col });
                @panic("Parsing error");
            }
        }
        if (index == upper_bound) {
            @panic("reached parser's upper bound");
        }

        return .{.program= self.program.items, .indexes= self.indexes.items};
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
        .{ .type = .EOF, .row = 0, .col = 17, .literal = "" },
    };

    const expected_ast = [_]ast.Node{
        ast.Node{
            .type = .{ .Number = .{ .val = 0 } },
        },
        ast.Node{
            .type = .{ .Number = .{ .val = 123 } },
        },
        ast.Node{
            .type = .{ .String = .{ .val = "hello there" } },
        },
    };

    const expected_ast_indexes = [_]u32{ 0, 1, 2 };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();
    try std.testing.expectEqual(expected_ast.len, parsed_ast.program.len);
    try std.testing.expectEqual(expected_ast_indexes.len, parsed_ast.indexes.len);

    try std.testing.expectEqualSlices(u32, &expected_ast_indexes, parsed_ast.indexes);

    var i: usize = 0;
    while (i < expected_ast.len) : (i += 1) {
        const expected = expected_ast[i].toString(allocator);
        defer allocator.free(expected);

        const got = parsed_ast.program[i].toString(allocator);
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
        .{ .type = .EOF, .row = 0, .col = 4, .literal = "" },
    };

    const expected_ast = [_]ast.Node{
        ast.Node{ .type = .{ .Number = .{ .val = 1 }, }, },
        ast.Node{ .type = .{ .Number = .{ .val = 2 }, }, },
        ast.Node{ .type = .{ .BinOp = .{ .op = .Add, .lhs = 0, .rhs = 1 }, }, },
    };

    const expected_ast_indexes = [_]u32{ 2 };

    var parser = Parser.init(allocator, &tokens);
    defer parser.deinit();

    const parsed_ast = parser.parse();
    try std.testing.expectEqual(expected_ast.len, parsed_ast.program.len);
    try std.testing.expectEqual(expected_ast_indexes.len, parsed_ast.indexes.len);

    try std.testing.expectEqualSlices(u32, &expected_ast_indexes, parsed_ast.indexes);

    var i: usize = 0;
    while (i < expected_ast.len) : (i += 1) {
        const expected = expected_ast[i].toString(allocator);
        defer allocator.free(expected);

        const got = parsed_ast.program[i].toString(allocator);
        defer allocator.free(got);

        try std.testing.expectEqualStrings(expected, got);
    }
}
