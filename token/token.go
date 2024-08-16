package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
	Col     int
	Row     int
}

const (
	EOF     TokenType = "EOF"
	Illegal TokenType = "ILLEGAL"

	Ident  TokenType = "IDENT"
	Number TokenType = "NUMBER"
	String TokenType = "STRING"

	Comma     TokenType = ","
	Colon     TokenType = ":"
	LParen    TokenType = "("
	RParen    TokenType = ")"
	LSquiggly TokenType = "{"
	RSquiggly TokenType = "}"

	// Operators
	Plus             TokenType = "+"
	Minus            TokenType = "-"
	Asterisk         TokenType = "*"
	Slash            TokenType = "/"
	LessThan         TokenType = "<"
	GreaterThan      TokenType = ">"
	LessThanEqual    TokenType = "<="
	GreaterThanEqual TokenType = ">="
	Equal            TokenType = "=="
	NotEqual         TokenType = "!="
	And              TokenType = "&&"
	Or               TokenType = "||"
	Not              TokenType = "!"
)
