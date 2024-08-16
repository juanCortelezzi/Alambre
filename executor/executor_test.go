package executor_test

import (
	"testing"

	"github.com/juancortelezzi/alambre/ast"
	"github.com/juancortelezzi/alambre/executor"
	"github.com/juancortelezzi/alambre/value"
)

func TestAdventOfCodeDay12024(t *testing.T) {
	// input := `"\n" split (chars (to_int) map (is_some) filter first unwrap 10 * swap last unwrap swap drop +) map 0 (+) reduce`
	input := []ast.Node{
		&ast.String{Value: "\n"},
		&ast.FnCall{Name: "split"},
		&ast.Fn{
			Program: []ast.Node{
				&ast.FnCall{Name: "chars"},
				&ast.Fn{
					Program: []ast.Node{
						&ast.FnCall{Name: "to_int"},
					},
				},
				&ast.FnCall{Name: "map"},
				&ast.Fn{
					Program: []ast.Node{
						&ast.FnCall{Name: "is_some"},
					},
				},
				&ast.FnCall{Name: "filter"},
				&ast.FnCall{Name: "first"},
				&ast.FnCall{Name: "unwrap"},
				&ast.Number{Value: 10},
				&ast.Binop{Type: ast.Multiplication},
				&ast.FnCall{Name: "swap"},
				&ast.FnCall{Name: "last"},
				&ast.FnCall{Name: "unwrap"},
				&ast.FnCall{Name: "swap"},
				&ast.FnCall{Name: "drop"},
				&ast.Binop{Type: ast.Addition},
			},
		},
		&ast.FnCall{Name: "map"},
		&ast.Number{Value: 0},
		&ast.Fn{
			Program: []ast.Node{
				&ast.Binop{Type: ast.Addition},
			},
		},
		&ast.FnCall{Name: "reduce"},
	}

	stack := []value.Node{
		&value.String{Value: "a2dej2l\na1tsj5l\na9l"},
	}
	e := executor.NewExecutorWithStack(stack)
	e.Execute(input)
	got := e.String()
	expected := "0: 136.000000\n"

	if got != expected {
		t.Fatalf("expected '%s', but got '%s'", expected, got)
	}
}
