const std = @import("std");

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    row: usize,
    col: usize,

    pub fn toString(self: Token, allocator: std.mem.Allocator) []u8 {
        return std.fmt.allocPrint(allocator, "Token<{s}:{d}:{d}>('{s}')", .{
            self.type.toString(),
            self.row,
            self.col,
            self.literal,
        }) catch @panic("failed to allocate memory");
    }
};

pub const TokenType = enum {
    EOF,
    Illegal,

    Ident,
    Number,
    String,

    Comma,
    Colon,
    LParen,
    RParen,
    LSquiggly,
    RSquiggly,

    Plus,
    Minus,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
    Not,
    pub fn toString(self: TokenType) []const u8 {
        return switch (self) {
            .EOF => "EOF",
            .Illegal => "ILLEGAL",
            .Ident => "IDENT",
            .Number => "NUMBER",
            .String => "STRING",
            .Comma => "COMMA",
            .Colon => "COLON",
            .LParen => "LPAREN",
            .RParen => "RPAREN",
            .LSquiggly => "LSQUIGGLY",
            .RSquiggly => "RSQUIGGLY",
            .Plus => "PLUS",
            .Minus => "MINUS",
            .Asterisk => "ASTERISK",
            .Slash => "SLASH",
            .LessThan => "LESS_THAN",
            .GreaterThan => "GREATER_THAN",
            .LessThanEqual => "LESS_THAN_EQUAL",
            .GreaterThanEqual => "GREATER_THAN_EQUAL",
            .Equal => "EQUAL",
            .NotEqual => "NOT_EQUAL",
            .And => "AND",
            .Or => "OR",
            .Not => "NOT",
        };
    }
};
