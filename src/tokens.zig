const std = @import("std");

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    row: usize,
    col: usize,

    pub fn to_string(self: Token, buf: []u8) []u8 {
        return std.fmt.bufPrint(buf, "Token<{s}:{d}:{d}>('{s}')", .{
            self.type.to_string(),
            self.row,
            self.col,
            self.literal,
        }) catch unreachable;
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
    pub fn to_string(self: TokenType) []const u8 {
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
