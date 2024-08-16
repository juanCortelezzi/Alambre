package lexer_test

import (
	"testing"

	"github.com/juancortelezzi/alambre/lexer"
	"github.com/juancortelezzi/alambre/token"
)

func TestNextToken(t *testing.T) {
	input := `{"1", "juan bautista", "3"} (to_int 0 or_else 1 add) map
status 
+-<> >= <= == != && || !
69 420 +
:
`
	tests := []struct {
		expectedType     token.TokenType
		expectedLiteral  string
		expectedLine     int
		expectedPosition int
	}{
		{token.LSquiggly, "{", 1, 1},
		{token.String, "1", 1, 2},
		{token.Comma, ",", 1, 5},
		{token.String, "juan bautista", 1, 7},
		{token.Comma, ",", 1, 22},
		{token.String, "3", 1, 24},
		{token.RSquiggly, "}", 1, 27},
		{token.LParen, "(", 1, 29},
		{token.Ident, "to_int", 1, 30},
		{token.Number, "0", 1, 37},
		{token.Ident, "or_else", 1, 39},
		{token.Number, "1", 1, 47},
		{token.Ident, "add", 1, 49},
		{token.RParen, ")", 1, 52},
		{token.Ident, "map", 1, 54},
		{token.Ident, "status", 2, 1},
		{token.Plus, "+", 3, 1},
		{token.Minus, "-", 3, 2},
		{token.LessThan, "<", 3, 3},
		{token.GreaterThan, ">", 3, 4},
		{token.GreaterThanEqual, ">=", 3, 6},
		{token.LessThanEqual, "<=", 3, 9},
		{token.Equal, "==", 3, 12},
		{token.NotEqual, "!=", 3, 15},
		{token.And, "&&", 3, 18},
		{token.Or, "||", 3, 21},
		{token.Not, "!", 3, 24},
		{token.Number, "69", 4, 1},
		{token.Number, "420", 4, 4},
		{token.Plus, "+", 4, 8},
		{token.Colon, ":", 5, 1},
		{token.EOF, "", 6, 0},
	}

	l := lexer.NewLexerFromString(input)
	for i, test := range tests {
		tok := l.NextToken()

		if tok.Type != test.expectedType {
			t.Logf("lexer: %#v\n", l)
			t.Logf("token: %#v\n", tok)
			t.Fatalf("tests[%d] - tokenType wrong. expected=%q, got=%q", i, test.expectedType, tok.Type)
		}

		if tok.Literal != test.expectedLiteral {
			t.Logf("lexer: %#v\n", l)
			t.Logf("token: %#v\n", tok)
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q", i, test.expectedLiteral, tok.Literal)
		}

		if tok.Row != test.expectedLine {
			t.Logf("lexer: %#v\n", l)
			t.Logf("token: %#v\n", tok)
			t.Fatalf("tests[%d] - line wrong. expected=%d, got=%d", i, test.expectedLine, tok.Row)
		}

		if tok.Col != test.expectedPosition {
			t.Logf("lexer: %#v\n", l)
			t.Logf("token: %#v\n", tok)
			t.Fatalf("tests[%d] - position wrong. expected=%d, got=%d", i, test.expectedPosition, tok.Col)
		}
	}

}
