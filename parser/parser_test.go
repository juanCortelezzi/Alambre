package parser_test

import (
	"fmt"
	"testing"

	"github.com/juancortelezzi/alambre/ast"
	"github.com/juancortelezzi/alambre/parser"
	"github.com/juancortelezzi/alambre/token"
)

func TestParserBalls(t *testing.T) {

	lexemes := []token.Token{
		{Type: token.String, Literal: "hello", Row: 0, Col: 0},
		{Type: token.LSquiggly, Literal: "{", Row: 0, Col: 6},
		{Type: token.Number, Literal: "1", Row: 0, Col: 7},
		{Type: token.Comma, Literal: ",", Row: 0, Col: 8},
		{Type: token.Number, Literal: "2", Row: 0, Col: 10},
		{Type: token.Comma, Literal: ",", Row: 0, Col: 11},
		{Type: token.Number, Literal: "3", Row: 0, Col: 13},
		{Type: token.Number, Literal: "3", Row: 0, Col: 15},
		{Type: token.Asterisk, Literal: "*", Row: 0, Col: 17},
		{Type: token.RSquiggly, Literal: "}", Row: 0, Col: 18},
		{Type: token.LParen, Literal: "(", Row: 0, Col: 20},
		{Type: token.Number, Literal: "1", Row: 0, Col: 21},
		{Type: token.Plus, Literal: "+", Row: 0, Col: 22},
		{Type: token.RParen, Literal: ")", Row: 0, Col: 23},
		{Type: token.Ident, Literal: "map", Row: 0, Col: 25},

		{Type: token.LParen, Literal: "(", Row: 1, Col: 0},
		{Type: token.Plus, Literal: "+", Row: 1, Col: 1},
		{Type: token.RParen, Literal: ")", Row: 1, Col: 2},
		{Type: token.Number, Literal: "0", Row: 1, Col: 4},
		{Type: token.Ident, Literal: "reduce", Row: 1, Col: 6},
		{Type: token.LSquiggly, Literal: "{", Row: 1, Col: 13},
		{Type: token.LSquiggly, Literal: "{", Row: 1, Col: 14},
		{Type: token.Number, Literal: "1", Row: 1, Col: 15},
		{Type: token.RSquiggly, Literal: "}", Row: 1, Col: 16},
		{Type: token.Comma, Literal: ",", Row: 1, Col: 17},
		{Type: token.LSquiggly, Literal: "{", Row: 1, Col: 19},
		{Type: token.Number, Literal: "2", Row: 1, Col: 20},
		{Type: token.RSquiggly, Literal: "}", Row: 1, Col: 21},
		{Type: token.RSquiggly, Literal: "}", Row: 1, Col: 22},
		{Type: token.EOF, Literal: "", Row: 1, Col: 23},
	}

	index := 0
	p := parser.NewParser(func() token.Token {
		if index == len(lexemes) {
			return lexemes[index-1]
		}

		token := lexemes[index]
		index += 1
		return token
	})

	program, err := p.Parse()

	if err != nil {
		t.Fatal(err)
	}

	nodes := []ast.Node{
		&ast.String{Value: "hello", Col: 0, Row: 0},
		&ast.Table{
			Items: []ast.TableItem{
				{Value: []ast.Node{&ast.Number{Value: 1, Col: 7, Row: 0}}},
				{Value: []ast.Node{&ast.Number{Value: 2, Col: 10, Row: 0}}},
				{Value: []ast.Node{
					&ast.Number{Value: 3, Col: 13, Row: 0},
					&ast.Number{Value: 3, Col: 15, Row: 0},
					&ast.Binop{Type: ast.Multiplication, Col: 17, Row: 0},
				}},
			},
			Col: 6,
			Row: 0,
		},
		&ast.Fn{
			Program: []ast.Node{
				&ast.Number{Value: 1, Col: 21, Row: 0},
				&ast.Binop{Type: ast.Addition, Col: 22, Row: 0},
			},
			Col: 20,
			Row: 0,
		},
		&ast.FnCall{Name: "map", Col: 25, Row: 0},
		&ast.Fn{
			Program: []ast.Node{
				&ast.Binop{Type: ast.Addition, Col: 1, Row: 1},
			},
			Col: 0,
			Row: 1,
		},
		&ast.Number{Value: 0, Col: 4, Row: 1},
		&ast.FnCall{Name: "reduce", Col: 6, Row: 1},
		&ast.Table{
			Items: []ast.TableItem{
				{Value: []ast.Node{
					&ast.Table{
						Items: []ast.TableItem{
							{Value: []ast.Node{&ast.Number{Value: 1, Col: 15, Row: 1}}},
						},
						Col: 14,
						Row: 1,
					},
				}},
				{Value: []ast.Node{
					&ast.Table{
						Items: []ast.TableItem{
							{Value: []ast.Node{&ast.Number{Value: 2, Col: 20, Row: 1}}},
						},
						Col: 19,
						Row: 1,
					},
				}},
			},
			Col: 13,
			Row: 1,
		},
	}

	for i, expected := range nodes {
		got := program[i]
		gotString := fmt.Sprintf("%s", got)
		expectedString := fmt.Sprintf("%s", expected)
		if gotString != expectedString {
			t.Fatalf("expected %s, but got %s", expected, got)
		}
		expectedCol, expectedRow := expected.Coords()
		gotCol, gotRow := got.Coords()
		if gotCol != expectedCol {
			t.Fatalf("expected %d, but got %d", expectedCol, gotCol)
		}
		if gotRow != expectedRow {
			t.Fatalf("expected %d, but got %d", expectedRow, gotRow)
		}
	}

}

func TestParserTable(t *testing.T) {
	lexemes := []token.Token{
		{Type: token.LSquiggly, Literal: "{", Row: 0, Col: 1},
		{Type: token.Number, Literal: "1", Row: 0, Col: 2},
		{Type: token.Comma, Literal: ",", Row: 0, Col: 3},
		{Type: token.Number, Literal: "2", Row: 0, Col: 5},
		{Type: token.Comma, Literal: ",", Row: 0, Col: 6},
		{Type: token.Ident, Literal: "key", Row: 0, Col: 8},
		{Type: token.Colon, Literal: ":", Row: 0, Col: 11},
		{Type: token.String, Literal: "value", Row: 0, Col: 13},
		{Type: token.RSquiggly, Literal: "}", Row: 0, Col: 18},
		{Type: token.EOF, Literal: "", Row: 0, Col: 19},
	}

	index := 0
	p := parser.NewParser(func() token.Token {
		if index == len(lexemes) {
			return lexemes[index-1]
		}

		token := lexemes[index]
		index += 1
		return token
	})

	program, err := p.Parse()

	if err != nil {
		t.Fatal(err)
	}

	nodes := []ast.Node{
		&ast.Table{
			Items: []ast.TableItem{
				{Value: []ast.Node{&ast.Number{Value: 1}}},
				{Value: []ast.Node{&ast.Number{Value: 2}}},
				{
					IsKvp: true,
					Key:   "key",
					Value: []ast.Node{&ast.String{Value: "value"}},
				},
			},
		},
	}

	for i, expected := range nodes {
		got := program[i]
		gotString := fmt.Sprintf("%s", got)
		expectedString := fmt.Sprintf("%s", expected)
		if gotString != expectedString {
			t.Fatalf("expected %s, but got %s", expected, got)
		}
	}

}
