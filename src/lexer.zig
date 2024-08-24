const std = @import("std");
const Token = @import("tokens.zig").Token;
const assert = std.debug.assert;

const LexerErr = error{
    UnterminatedString,
};

pub const Lexer = struct {
    allocator: std.mem.Allocator,

    tokens: std.ArrayList(Token),
    src: []const u8,
    read_pos: usize,
    cur: u8,
    col: usize,
    row: usize,

    pub fn init(allocator: std.mem.Allocator, str: []const u8) Lexer {
        assert(str.len > 0);
        return Lexer{
            .allocator = allocator,
            .tokens = std.ArrayList(Token).init(allocator),
            .src = str,
            .read_pos = 1,
            .col = 0,
            .row = 0,
            .cur = str[0],
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
    }

    fn advance(self: *Lexer) void {
        if (self.read_pos >= self.src.len) {
            self.cur = 0;
            return;
        }

        if (self.cur == '\n') {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        self.cur = self.src[self.read_pos];
        self.read_pos += 1;
    }

    fn peek_char(self: *Lexer) u8 {
        if (self.read_pos >= self.src.len) {
            return 0;
        }

        return self.src[self.read_pos];
    }

    fn is_ascii_letter(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    fn is_ascii_digit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn parse_ident(self: *Lexer) []const u8 {
        const read_pos = self.read_pos;

        const upper_bound = 128;
        var i: usize = 0;
        while (is_ascii_letter(self.cur) and i < upper_bound) : (i += 1) {
            self.advance();
        } else {
            if (i == upper_bound) {
                @panic("too many characters in identifier");
            }
        }

        return self.src[read_pos - 1 .. self.read_pos - 1];
    }

    fn parse_digit(self: *Lexer) []const u8 {
        const read_pos = self.read_pos;

        const upper_bound = 64;
        var i: usize = 0;
        while (is_ascii_digit(self.cur) and i < upper_bound) : (i += 1) {
            self.advance();
        } else {
            if (i == upper_bound) {
                @panic("too many characters in digit");
            }
        }

        return self.src[read_pos - 1 .. self.read_pos - 1];
    }

    fn parse_string(self: *Lexer) LexerErr![]const u8 {
        assert(self.cur == '"');
        self.advance();

        const read_pos = self.read_pos;

        var i: usize = 0;
        while (i < self.src.len) : (i += 1) {
            if (self.cur == '"') {
                self.advance();
                return self.src[read_pos - 1 .. self.read_pos - 2];
            }

            if (self.cur == '\n') {
                return LexerErr.UnterminatedString;
            }

            self.advance();
        }

        return LexerErr.UnterminatedString;
    }

    fn skip_whitespace(self: *Lexer) void {
        const upper_bound = 2048;
        var i: usize = 0;
        while (i < upper_bound) : (i += 1) {
            if (!std.ascii.isWhitespace(self.cur)) {
                return;
            }

            self.advance();
        } else {
            @panic("too many characters in whitespace");
        }
    }

    pub fn next_token(self: *Lexer) LexerErr!Token {
        self.skip_whitespace();

        if (is_ascii_digit(self.cur)) {
            const row = self.row;
            const col = self.col;
            const digit = self.parse_digit();
            return Token{ .type = .Number, .row = row, .col = col, .literal = digit };
        }

        if (is_ascii_letter(self.cur)) {
            const row = self.row;
            const col = self.col;
            const ident = self.parse_ident();
            return Token{ .type = .Ident, .row = row, .col = col, .literal = ident };
        }

        const token = switch (self.cur) {
            ',' => Token{ .type = .Comma, .row = self.row, .col = self.col, .literal = "," },
            ':' => Token{ .type = .Colon, .row = self.row, .col = self.col, .literal = ":" },
            '(' => Token{ .type = .LParen, .row = self.row, .col = self.col, .literal = "(" },
            ')' => Token{ .type = .RParen, .row = self.row, .col = self.col, .literal = ")" },
            '{' => Token{ .type = .LSquiggly, .row = self.row, .col = self.col, .literal = "{" },
            '}' => Token{ .type = .RSquiggly, .row = self.row, .col = self.col, .literal = "}" },
            '+' => Token{ .type = .Plus, .row = self.row, .col = self.col, .literal = "+" },
            '-' => Token{ .type = .Minus, .row = self.row, .col = self.col, .literal = "-" },
            '*' => Token{ .type = .Asterisk, .row = self.row, .col = self.col, .literal = "*" },
            '/' => Token{ .type = .Slash, .row = self.row, .col = self.col, .literal = "/" },
            '!' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '=') {
                    self.advance();
                    break :out Token{ .type = .NotEqual, .row = row, .col = col, .literal = "!=" };
                }
                break :out Token{ .type = .Not, .row = row, .col = col, .literal = "!" };
            },
            '<' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '=') {
                    self.advance();
                    break :out Token{ .type = .LessThanEqual, .row = row, .col = col, .literal = "<=" };
                }
                break :out Token{ .type = .LessThan, .row = row, .col = col, .literal = "<" };
            },
            '>' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '=') {
                    self.advance();
                    break :out Token{ .type = .GreaterThanEqual, .row = row, .col = col, .literal = ">=" };
                }
                break :out Token{ .type = .GreaterThan, .row = row, .col = col, .literal = ">" };
            },
            '=' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '=') {
                    self.advance();
                    break :out Token{ .type = .Equal, .row = row, .col = col, .literal = "==" };
                }
                break :out Token{ .type = .Illegal, .row = row, .col = col, .literal = "=" };
            },
            '&' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '&') {
                    self.advance();
                    break :out Token{ .type = .And, .row = row, .col = col, .literal = "&&" };
                }
                break :out Token{ .type = .Illegal, .row = row, .col = col, .literal = "&" };
            },
            '|' => out: {
                const col = self.col;
                const row = self.row;
                const peek = self.peek_char();
                if (peek == '|') {
                    self.advance();
                    break :out Token{ .type = .Or, .row = row, .col = col, .literal = "||" };
                }
                break :out Token{ .type = .Illegal, .row = row, .col = col, .literal = "|" };
            },
            '"' => {
                const col = self.col;
                const row = self.row;
                const string = self.parse_string() catch |err| {
                    std.log.err("error parsing string: line {d} - position {d} - error '{any}'", .{ col, row, err });
                    std.process.exit(1);
                };
                return Token{ .type = .String, .row = row, .col = col, .literal = string };
            },
            0 => Token{ .type = .EOF, .row = self.row, .col = self.col, .literal = "" },
            else => Token{ .type = .Illegal, .row = self.row, .col = self.col, .literal = &[_]u8{self.cur} },
        };

        if (token.type != .EOF) {
            self.advance();
        }

        return token;
    }

    pub fn get_tokens(self: *Lexer) LexerErr![]Token {
        var index: usize = 0;
        while (index < self.src.len) : (index += 1) {
            const token = try self.next_token();
            self.tokens.append(token) catch unreachable;
            if (token.type == .EOF) {
                break;
            }
        } else {
            @panic("reached upper bound of token loop");
        }

        return self.tokens.items;
    }
};

test "read_char" {
    var lexer = Lexer.init(std.testing.allocator, "+-*/");
    defer lexer.deinit();

    try std.testing.expectEqual('+', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(0, lexer.col);
    lexer.advance();
    try std.testing.expectEqual('-', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(1, lexer.col);
    lexer.advance();
    try std.testing.expectEqual('*', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(2, lexer.col);
    lexer.advance();
    try std.testing.expectEqual('/', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(3, lexer.col);
    lexer.advance();
    try std.testing.expectEqual(0, lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(3, lexer.col);
    lexer.advance();
    try std.testing.expectEqual(0, lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(3, lexer.col);
}

test "peek_char" {
    var lexer = Lexer.init(std.testing.allocator, "+-");
    defer lexer.deinit();

    try std.testing.expectEqual('+', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(0, lexer.col);
    try std.testing.expectEqual('-', lexer.peek_char());
    lexer.advance();
    try std.testing.expectEqual('-', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(1, lexer.col);
    try std.testing.expectEqual(0, lexer.peek_char());
    lexer.advance();
    try std.testing.expectEqual(0, lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(1, lexer.col);
    try std.testing.expectEqual(0, lexer.peek_char());
}

test "skip_whitespace" {
    var lexer = Lexer.init(std.testing.allocator, "   \t1\na\r\n z  \n9");
    defer lexer.deinit();

    lexer.skip_whitespace();
    try std.testing.expectEqual('1', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(4, lexer.col);
    lexer.skip_whitespace();
    try std.testing.expectEqual('1', lexer.cur);
    try std.testing.expectEqual(0, lexer.row);
    try std.testing.expectEqual(4, lexer.col);
    lexer.advance();
    lexer.skip_whitespace();
    try std.testing.expectEqual('a', lexer.cur);
    try std.testing.expectEqual(1, lexer.row);
    try std.testing.expectEqual(0, lexer.col);
    lexer.advance();
    lexer.skip_whitespace();
    try std.testing.expectEqual('z', lexer.cur);
    try std.testing.expectEqual(2, lexer.row);
    try std.testing.expectEqual(1, lexer.col);
    lexer.advance();
    lexer.skip_whitespace();
    try std.testing.expectEqual('9', lexer.cur);
    try std.testing.expectEqual(3, lexer.row);
    try std.testing.expectEqual(0, lexer.col);
    lexer.advance();
    lexer.skip_whitespace();
    try std.testing.expectEqual(0, lexer.cur);
    try std.testing.expectEqual(3, lexer.row);
    try std.testing.expectEqual(0, lexer.col);
}

test "single_token" {
    const input = "+";
    var lexer = Lexer.init(std.testing.allocator, input);
    defer lexer.deinit();

    const tok = lexer.next_token();
    const expected = Token{ .type = .Plus, .row = 0, .col = 0, .literal = "+" };
    try std.testing.expectEqualDeep(expected, tok);
}

test "basic_tokens" {
    const input =
        \\, : ( ) { } + - * / ! < >
        \\!= <= >= == && ||
    ;

    const expected_tokens = [_]Token{
        .{ .type = .Comma, .row = 0, .col = 0, .literal = "," },
        .{ .type = .Colon, .row = 0, .col = 2, .literal = ":" },
        .{ .type = .LParen, .row = 0, .col = 4, .literal = "(" },
        .{ .type = .RParen, .row = 0, .col = 6, .literal = ")" },
        .{ .type = .LSquiggly, .row = 0, .col = 8, .literal = "{" },
        .{ .type = .RSquiggly, .row = 0, .col = 10, .literal = "}" },
        .{ .type = .Plus, .row = 0, .col = 12, .literal = "+" },
        .{ .type = .Minus, .row = 0, .col = 14, .literal = "-" },
        .{ .type = .Asterisk, .row = 0, .col = 16, .literal = "*" },
        .{ .type = .Slash, .row = 0, .col = 18, .literal = "/" },
        .{ .type = .Not, .row = 0, .col = 20, .literal = "!" },
        .{ .type = .LessThan, .row = 0, .col = 22, .literal = "<" },
        .{ .type = .GreaterThan, .row = 0, .col = 24, .literal = ">" },
        .{ .type = .NotEqual, .row = 1, .col = 0, .literal = "!=" },
        .{ .type = .LessThanEqual, .row = 1, .col = 3, .literal = "<=" },
        .{ .type = .GreaterThanEqual, .row = 1, .col = 6, .literal = ">=" },
        .{ .type = .Equal, .row = 1, .col = 9, .literal = "==" },
        .{ .type = .And, .row = 1, .col = 12, .literal = "&&" },
        .{ .type = .Or, .row = 1, .col = 15, .literal = "||" },
        .{ .type = .EOF, .row = 1, .col = 16, .literal = "" },
    };

    var lexer = Lexer.init(std.testing.allocator, input);
    defer lexer.deinit();

    for (expected_tokens) |expected| {
        const tok = try lexer.next_token();
        var bufa: [64]u8 = undefined;
        var bufb: [64]u8 = undefined;
        try std.testing.expectEqualStrings(expected.to_string(&bufa), tok.to_string(&bufb));
    }
}

test "next_token" {
    const input =
        \\{"1", "juan bautista", "3"} (to_int 0 or_else 1 add) map
        \\status
        \\+-<> >= <= == != && || !
        \\69 420 +
        \\"tumama"
        \\:
    ;

    const expected_tokens = [_]Token{
        .{ .type = .LSquiggly, .row = 0, .col = 0, .literal = "{" },
        .{ .type = .String, .row = 0, .col = 1, .literal = "1" },
        .{ .type = .Comma, .row = 0, .col = 4, .literal = "," },
        .{ .type = .String, .row = 0, .col = 6, .literal = "juan bautista" },
        .{ .type = .Comma, .row = 0, .col = 21, .literal = "," },
        .{ .type = .String, .row = 0, .col = 23, .literal = "3" },
        .{ .type = .RSquiggly, .row = 0, .col = 26, .literal = "}" },
        .{ .type = .LParen, .row = 0, .col = 28, .literal = "(" },
        .{ .type = .Ident, .row = 0, .col = 29, .literal = "to_int" },
        .{ .type = .Number, .row = 0, .col = 36, .literal = "0" },
        .{ .type = .Ident, .row = 0, .col = 38, .literal = "or_else" },
        .{ .type = .Number, .row = 0, .col = 46, .literal = "1" },
        .{ .type = .Ident, .row = 0, .col = 48, .literal = "add" },
        .{ .type = .RParen, .row = 0, .col = 51, .literal = ")" },
        .{ .type = .Ident, .row = 0, .col = 53, .literal = "map" },
        .{ .type = .Ident, .row = 1, .col = 0, .literal = "status" },
        .{ .type = .Plus, .row = 2, .col = 0, .literal = "+" },
        .{ .type = .Minus, .row = 2, .col = 1, .literal = "-" },
        .{ .type = .LessThan, .row = 2, .col = 2, .literal = "<" },
        .{ .type = .GreaterThan, .row = 2, .col = 3, .literal = ">" },
        .{ .type = .GreaterThanEqual, .row = 2, .col = 5, .literal = ">=" },
        .{ .type = .LessThanEqual, .row = 2, .col = 8, .literal = "<=" },
        .{ .type = .Equal, .row = 2, .col = 11, .literal = "==" },
        .{ .type = .NotEqual, .row = 2, .col = 14, .literal = "!=" },
        .{ .type = .And, .row = 2, .col = 17, .literal = "&&" },
        .{ .type = .Or, .row = 2, .col = 20, .literal = "||" },
        .{ .type = .Not, .row = 2, .col = 23, .literal = "!" },
        .{ .type = .Number, .row = 3, .col = 0, .literal = "69" },
        .{ .type = .Number, .row = 3, .col = 3, .literal = "420" },
        .{ .type = .Plus, .row = 3, .col = 7, .literal = "+" },
        .{ .type = .String, .row = 4, .col = 0, .literal = "tumama" },
        .{ .type = .Colon, .row = 5, .col = 0, .literal = ":" },
        .{ .type = .EOF, .row = 5, .col = 0, .literal = "" },
    };

    var lexer = Lexer.init(std.testing.allocator, input);
    defer lexer.deinit();

    for (expected_tokens) |expected| {
        const tok = try lexer.next_token();
        var bufa: [64]u8 = undefined;
        var bufb: [64]u8 = undefined;
        try std.testing.expectEqualStrings(expected.to_string(&bufa), tok.to_string(&bufb));
    }
}

test "get_tokens" {
    const input =
        \\{"1", "juan bautista", "3"} (to_int 0 or_else 1 add) map
        \\status
        \\+-<> >= <= == != && || !
        \\69 420 +
        \\"tumama"
        \\:
    ;

    const expected_tokens = [_]Token{
        .{ .type = .LSquiggly, .row = 0, .col = 0, .literal = "{" },
        .{ .type = .String, .row = 0, .col = 1, .literal = "1" },
        .{ .type = .Comma, .row = 0, .col = 4, .literal = "," },
        .{ .type = .String, .row = 0, .col = 6, .literal = "juan bautista" },
        .{ .type = .Comma, .row = 0, .col = 21, .literal = "," },
        .{ .type = .String, .row = 0, .col = 23, .literal = "3" },
        .{ .type = .RSquiggly, .row = 0, .col = 26, .literal = "}" },
        .{ .type = .LParen, .row = 0, .col = 28, .literal = "(" },
        .{ .type = .Ident, .row = 0, .col = 29, .literal = "to_int" },
        .{ .type = .Number, .row = 0, .col = 36, .literal = "0" },
        .{ .type = .Ident, .row = 0, .col = 38, .literal = "or_else" },
        .{ .type = .Number, .row = 0, .col = 46, .literal = "1" },
        .{ .type = .Ident, .row = 0, .col = 48, .literal = "add" },
        .{ .type = .RParen, .row = 0, .col = 51, .literal = ")" },
        .{ .type = .Ident, .row = 0, .col = 53, .literal = "map" },
        .{ .type = .Ident, .row = 1, .col = 0, .literal = "status" },
        .{ .type = .Plus, .row = 2, .col = 0, .literal = "+" },
        .{ .type = .Minus, .row = 2, .col = 1, .literal = "-" },
        .{ .type = .LessThan, .row = 2, .col = 2, .literal = "<" },
        .{ .type = .GreaterThan, .row = 2, .col = 3, .literal = ">" },
        .{ .type = .GreaterThanEqual, .row = 2, .col = 5, .literal = ">=" },
        .{ .type = .LessThanEqual, .row = 2, .col = 8, .literal = "<=" },
        .{ .type = .Equal, .row = 2, .col = 11, .literal = "==" },
        .{ .type = .NotEqual, .row = 2, .col = 14, .literal = "!=" },
        .{ .type = .And, .row = 2, .col = 17, .literal = "&&" },
        .{ .type = .Or, .row = 2, .col = 20, .literal = "||" },
        .{ .type = .Not, .row = 2, .col = 23, .literal = "!" },
        .{ .type = .Number, .row = 3, .col = 0, .literal = "69" },
        .{ .type = .Number, .row = 3, .col = 3, .literal = "420" },
        .{ .type = .Plus, .row = 3, .col = 7, .literal = "+" },
        .{ .type = .String, .row = 4, .col = 0, .literal = "tumama" },
        .{ .type = .Colon, .row = 5, .col = 0, .literal = ":" },
        .{ .type = .EOF, .row = 5, .col = 0, .literal = "" },
    };

    var lexer = Lexer.init(std.testing.allocator, input);
    defer lexer.deinit();

    const tokens = try lexer.get_tokens();
    try std.testing.expectEqual(expected_tokens.len, tokens.len);

    var i: usize = 0;
    while (i < expected_tokens.len) : (i += 1) {
        var bufa: [64]u8 = undefined;
        var bufb: [64]u8 = undefined;
        const expected = expected_tokens[i];
        const got = tokens[i];
        try std.testing.expectEqualStrings(expected.to_string(&bufa), got.to_string(&bufb));
    }
}
