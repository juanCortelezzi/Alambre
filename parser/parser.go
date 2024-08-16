package parser

import (
	"errors"
	"fmt"
	"strconv"

	"github.com/juancortelezzi/alambre/ast"
	"github.com/juancortelezzi/alambre/token"
)

var (
	ErrInvalidConversion = errors.New("Parser: invalid conversion")
	ErrInvalidToken      = errors.New("Parser: invalid token")
	ErrUnknownIdentifier = errors.New("Parser: unknown identifier")
	// input := `"\n" split (chars (to_int) map (is_some) filter first unwrap 10 * swap last unwrap swap drop +) map 0 (+) reduce`
	builtins = map[string]struct{}{
		"reduce":  {},
		"map":     {},
		"status":  {},
		"split":   {},
		"chars":   {},
		"to_int":  {},
		"filter":  {},
		"is_some": {},
		"first":   {},
		"unwrap":  {},
		"swap":    {},
		"last":    {},
		"drop":    {},
	}
)

type Parser struct {
	program   []ast.Node
	currToken token.Token
	peekToken token.Token
	nextToken func() token.Token
}

func NewParser(nextToken func() token.Token) *Parser {
	currToken := nextToken()
	peekToken := nextToken()
	return &Parser{
		program:   make([]ast.Node, 0, 10),
		currToken: currToken,
		peekToken: peekToken,
		nextToken: nextToken,
	}
}

func NewParserFromTokens(tokens []token.Token) *Parser {
	index := 0
	nextToken := func() token.Token {
		if index >= len(tokens) {
			return token.Token{Type: token.EOF, Literal: ""}
		}
		tok := tokens[index]
		index += 1
		return tok
	}

	return NewParser(nextToken)
}

func (p *Parser) readToken() {
	p.currToken = p.peekToken
	p.peekToken = p.nextToken()
}

func (p *Parser) parseNumber() (ast.Node, error) {
	tok := p.currToken
	value, err := strconv.ParseFloat(tok.Literal, 64)
	if err != nil {
		return nil, fmt.Errorf(
			"%w: converting '%s' as number at pos '%d:%d'",
			ErrInvalidConversion,
			tok.Literal,
			tok.Row,
			tok.Col,
		)
	}

	p.readToken()

	return &ast.Number{Value: value, Col: tok.Col, Row: tok.Row}, nil
}

func (p *Parser) parseString() ast.Node {
	tok := p.currToken
	p.readToken()
	return &ast.String{Value: tok.Literal, Col: tok.Col, Row: tok.Row}
}

func (p *Parser) parseBinop() ast.Node {
	tok := p.currToken
	var binopType ast.BinopType
	switch tok.Type {
	case token.Plus:
		binopType = ast.Addition
	case token.Minus:
		binopType = ast.Subtraction
	case token.Asterisk:
		binopType = ast.Multiplication
	case token.Slash:
		binopType = ast.Division
	case token.LessThan:
		binopType = ast.LessThan
	case token.LessThanEqual:
		binopType = ast.LessThanEqual
	case token.GreaterThan:
		binopType = ast.GreaterThan
	case token.GreaterThanEqual:
		binopType = ast.GreaterThanEqual
	case token.Equal:
		binopType = ast.Equal
	case token.NotEqual:
		binopType = ast.NotEqual
	case token.And:
		binopType = ast.And
	case token.Or:
		binopType = ast.Or
	case token.Not:
		binopType = ast.Not
	}

	p.readToken()
	return &ast.Binop{Type: binopType, Col: tok.Col, Row: tok.Row}
}

func (p *Parser) parseTable() (ast.Node, error) {
	if p.currToken.Type != token.LSquiggly {
		// NOTE: this is a bad error, this should not happen if I am a good
		// programmer. Too lazy to change it though.
		return nil, fmt.Errorf("%w: expected '{' at pos '%d:%d'", ErrInvalidToken, p.currToken.Row, p.currToken.Col)
	}

	col := p.currToken.Col
	row := p.currToken.Row

	p.readToken()

	table := &ast.Table{Items: make([]ast.TableItem, 0), Col: col, Row: row}
	tableItem := ast.TableItem{Value: make([]ast.Node, 0)}

	for {
		if len(tableItem.Value) == 0 {
			isStringOrIdent := p.currToken.Type == token.String || p.currToken.Type == token.Ident
			hasColon := p.peekToken.Type == token.Colon
			if isStringOrIdent && hasColon {
				tableItem.IsKvp = true
				tableItem.Key = p.currToken.Literal
				p.readToken()
				p.readToken()
				continue
			}
		}

		node, err := p.parseExpression()
		if err != nil && errors.Is(err, ErrInvalidToken) {
			if p.currToken.Type == token.Comma {
				table.Items = append(table.Items, tableItem)
				tableItem = ast.TableItem{Value: make([]ast.Node, 0)}
				p.readToken()
				continue
			}

			if p.currToken.Type == token.RSquiggly {
				table.Items = append(table.Items, tableItem)
				p.readToken()
				break
			}

			if p.currToken.Type == token.Colon {
				// p.readToken()
				panic("todo")
			}

			panic("this should not happen")
		}

		if err != nil {
			return nil, err
		}

		tableItem.Value = append(tableItem.Value, node)
	}

	return table, nil
}

func (p *Parser) parseIdent() (ast.Node, error) {
	tok := p.currToken
	p.readToken()

	if tok.Literal == "true" {
		return &ast.Boolean{Value: true, Col: tok.Col, Row: tok.Row}, nil
	}

	if tok.Literal == "false" {
		return &ast.Boolean{Value: false, Col: tok.Col, Row: tok.Row}, nil
	}

	if _, ok := builtins[tok.Literal]; ok {
		return &ast.FnCall{Name: tok.Literal, Col: tok.Col, Row: tok.Row}, nil
	}

	return &ast.Ident{Name: tok.Literal, Col: tok.Col, Row: tok.Row}, nil
}

func (p *Parser) parseFn() (ast.Node, error) {
	if p.currToken.Type != token.LParen {
		// NOTE: this is a bad error, this should not happen if I am a good
		// programmer. Too lazy to change it though. v2.
		return nil, fmt.Errorf("%w: expected '(' at pos '%d:%d'", ErrInvalidToken, p.currToken.Row, p.currToken.Col)
	}

	col := p.currToken.Col
	row := p.currToken.Row

	p.readToken()

	fn := &ast.Fn{Program: make([]ast.Node, 0), Col: col, Row: row}
	for {
		node, err := p.parseExpression()
		if err != nil && errors.Is(err, ErrInvalidToken) {
			if p.currToken.Type != token.RParen {
				panic("this should not happen")
			}
			p.readToken()
			return fn, nil
		}

		if err != nil {
			return nil, err
		}

		fn.Program = append(fn.Program, node)
	}
}

func (p *Parser) parseExpression() (ast.Node, error) {
	for {
		t := p.currToken

		switch t.Type {
		case token.Number:
			node, err := p.parseNumber()
			return node, err

		case token.String:
			node := p.parseString()
			return node, nil

		case token.LSquiggly:
			node, err := p.parseTable()
			return node, err

		case token.LParen:
			node, err := p.parseFn()
			return node, err

		case token.Ident:
			node, err := p.parseIdent()
			return node, err

		case token.Plus,
			token.Minus,
			token.Asterisk,
			token.Slash,
			token.LessThan,
			token.LessThanEqual,
			token.GreaterThan,
			token.GreaterThanEqual,
			token.Equal,
			token.NotEqual,
			token.And,
			token.Or,
			token.Not:
			node := p.parseBinop()
			return node, nil

		default:
			return nil, fmt.Errorf("%w: token '%s' at pos '%d:%d'", ErrInvalidToken, t.Literal, t.Row, t.Col)
		}
	}
}

func (p *Parser) Parse() ([]ast.Node, error) {
	for p.currToken.Type != token.EOF {
		node, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		p.program = append(p.program, node)
	}

	return p.program, nil
}
