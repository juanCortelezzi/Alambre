package main

import (
	"fmt"
	"os"

	"github.com/juancortelezzi/alambre/executor"
	"github.com/juancortelezzi/alambre/lexer"
	"github.com/juancortelezzi/alambre/parser"
	"github.com/juancortelezzi/alambre/value"
)

func main() {
	args := os.Args
	if len(args) < 2 {
		fmt.Println("Usage: alambre <program> <parameters>")
		os.Exit(1)
	}

	program := os.Args[1]
	parameters := os.Args[2:]

	l := lexer.NewLexerFromString(program)
	p := parser.NewParser(l.NextToken)
	programAst, err := p.Parse()
	if err != nil {
		panic(err)
	}

	stack := make([]value.Node, len(parameters))
	for index, parameter := range parameters {
		stack[index] = &value.String{Value: parameter}
	}

	e := executor.NewExecutorWithStack(stack)
	e.Execute(programAst)
}
