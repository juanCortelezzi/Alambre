package lexer

import (
	"bufio"
	"errors"
	"io"
	"log"
	"strings"

	"github.com/juancortelezzi/alambre/token"
)

type Lexer struct {
	col    int
	row    int
	reader *bufio.Reader
}

func NewLexerFromString(input string) *Lexer {
	reader := bufio.NewReader(strings.NewReader(input))
	return &Lexer{
		col:    0,
		row:    1,
		reader: reader,
	}
}

func NewLexer(reader *bufio.Reader) *Lexer {
	return &Lexer{
		col:    0,
		row:    0,
		reader: reader,
	}
}

func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

func (l *Lexer) readChar() byte {
	b, err := l.reader.ReadByte()
	if err != nil {
		if errors.Is(err, io.EOF) {
			return 0
		}

		// handle me baby
		panic(err)
	}

	// log.Printf("read character: '%c'", b)
	// log.Printf("updating character position: from=%d to=%d", l.position, l.position+1)
	l.col++
	return b
}

func (l *Lexer) peekChar() byte {
	arr, err := l.reader.Peek(1)
	if err != nil {
		if errors.Is(err, io.EOF) {
			return 0
		}

		panic(err)
	}
	// log.Printf("peek character: '%c'", arr[0])
	return arr[0]
}

func (l *Lexer) parseIdentifier() string {
	ident := ""
	for {
		peek := l.peekChar()
		if !isLetter(peek) {
			return ident
		}

		ident += string(l.readChar())
	}
}

func (l *Lexer) parseDigit() string {
	digit := ""
	for {
		peek := l.peekChar()
		if !isDigit(peek) {
			return digit
		}

		digit += string(l.readChar())
	}
}

func (l *Lexer) parseString() (string, error) {
	str := ""
	for {
		char := l.readChar()
		if char == '"' {
			return str, nil
		}

		if char == 0 || char == '\n' {
			return "", errors.New("unterminated string")
		}

		str += string(char)
	}
}

func (l *Lexer) skipWhitespace() {
	for {
		peek := l.peekChar()

		if peek != ' ' && peek != '\t' && peek != '\n' && peek != '\r' {
			break
		}

		char := l.readChar()

		if char == '\n' {
			l.row++
			// log.Printf("updating character position: from=%d to=%d", l.position, 0)
			l.col = 0
		}
	}
}

func (l *Lexer) NextToken() token.Token {
	l.skipWhitespace()

	char := l.readChar()

	if isLetter(char) {
		position := l.col
		line := l.row
		literal := string(char) + l.parseIdentifier()
		return token.Token{Type: token.Ident, Literal: literal, Col: position, Row: line}
	}

	if isDigit(char) {
		position := l.col
		line := l.row
		literal := string(char) + l.parseDigit()
		return token.Token{Type: token.Number, Literal: literal, Col: position, Row: line}
	}

	var tok token.Token

	switch char {
	case ',':
		tok = token.Token{Type: token.Comma, Literal: string(char), Col: l.col, Row: l.row}
	case ':':
		tok = token.Token{Type: token.Colon, Literal: string(char), Col: l.col, Row: l.row}
	case '(':
		tok = token.Token{Type: token.LParen, Literal: string(char), Col: l.col, Row: l.row}
	case ')':
		tok = token.Token{Type: token.RParen, Literal: string(char), Col: l.col, Row: l.row}
	case '{':
		tok = token.Token{Type: token.LSquiggly, Literal: string(char), Col: l.col, Row: l.row}
	case '}':
		tok = token.Token{Type: token.RSquiggly, Literal: string(char), Col: l.col, Row: l.row}
	case '+':
		tok = token.Token{Type: token.Plus, Literal: string(char), Col: l.col, Row: l.row}
	case '-':
		tok = token.Token{Type: token.Minus, Literal: string(char), Col: l.col, Row: l.row}
	case '*':
		tok = token.Token{Type: token.Asterisk, Literal: string(char), Col: l.col, Row: l.row}
	case '/':
		tok = token.Token{Type: token.Slash, Literal: string(char), Col: l.col, Row: l.row}

	case '!':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '=' {
			l.readChar()
			tok = token.Token{Type: token.NotEqual, Literal: "!=", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.Not, Literal: string(char), Col: position, Row: line}

	case '<':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '=' {
			l.readChar()
			tok = token.Token{Type: token.LessThanEqual, Literal: "<=", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.LessThan, Literal: string(char), Col: position, Row: line}

	case '>':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '=' {
			l.readChar()
			tok = token.Token{Type: token.GreaterThanEqual, Literal: ">=", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.GreaterThan, Literal: string(char), Col: position, Row: line}

	case '=':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '=' {
			l.readChar()
			tok = token.Token{Type: token.Equal, Literal: "==", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.Illegal, Literal: string(char), Col: position, Row: line}

	case '&':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '&' {
			l.readChar()
			tok = token.Token{Type: token.And, Literal: "&&", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.Illegal, Literal: string(char), Col: position, Row: line}

	case '|':
		peek := l.peekChar()
		position := l.col
		line := l.row
		if peek == '|' {
			l.readChar()
			tok = token.Token{Type: token.Or, Literal: "||", Col: position, Row: line}
			break
		}
		tok = token.Token{Type: token.Illegal, Literal: string(char), Col: position, Row: line}

	case '"':
		position := l.col
		line := l.row
		literal, err := l.parseString()
		if err != nil {
			log.Fatalf("error parsing string: line %d - position %d - error '%v'", line, position, err)
		}
		tok = token.Token{Type: token.String, Literal: literal, Col: position, Row: line}

	case 0:
		tok = token.Token{Type: token.EOF, Literal: "", Col: l.col, Row: l.row}

	default:
		tok = token.Token{Type: token.Illegal, Literal: string(char), Col: l.col, Row: l.row}
	}

	return tok
}
