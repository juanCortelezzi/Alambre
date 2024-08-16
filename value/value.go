package value

import (
	"fmt"
	"github.com/juancortelezzi/alambre/ast"
	"strings"
)

type NodeType string

const (
	NumberType  NodeType = "Number"
	BooleanType NodeType = "Boolean"
	StringType  NodeType = "String"
	IdentType   NodeType = "Ident"
	TableType   NodeType = "Table"
	FnType      NodeType = "Fn"
	OptionType  NodeType = "Option"
)

type Node interface {
	Type() NodeType
	String() string
}

type Number struct {
	Value float64
}

var _ Node = &Number{}

func (n *Number) Type() NodeType {
	return NumberType
}

func (n *Number) String() string {
	return fmt.Sprintf("%f", n.Value)
}

type Boolean struct {
	Value bool
}

var _ Node = &Boolean{}

func (b *Boolean) Type() NodeType {
	return BooleanType
}

func (b *Boolean) String() string {
	if b.Value {
		return "true"
	}
	return "false"
}

type String struct {
	Value string
}

var _ Node = &String{}

func (s *String) Type() NodeType {
	return StringType
}

func (s *String) String() string {
	return fmt.Sprintf("\"%s\"", s.Value)
}

type Ident struct {
	Name string
}

var _ Node = &Ident{}

func (i *Ident) Type() NodeType {
	return IdentType
}

func (i *Ident) String() string {
	return fmt.Sprintf("*%s", i.Name)
}

type Table struct {
	Map map[Node]Node
	Arr []Node
}

var _ Node = &Table{}

func (i *Table) Type() NodeType {
	return TableType
}

func (t *Table) String() string {
	itemsString := make([]string, 0, len(t.Map)+len(t.Arr))
	for _, v := range t.Arr {
		itemsString = append(itemsString, v.String())
	}
	for k, v := range t.Map {
		itemsString = append(itemsString, fmt.Sprintf("%s: %s", k.String(), v.String()))
	}
	return fmt.Sprintf("{ %s }", strings.Join(itemsString, ", "))
}

type Fn struct {
	Program []ast.Node
}

var _ Node = &Fn{}

func (f *Fn) Type() NodeType {
	return FnType
}

func (f *Fn) String() string {
	stringifiedProgram := make([]string, 0, len(f.Program))
	for _, item := range f.Program {
		stringifiedProgram = append(stringifiedProgram, item.String())
	}

	return fmt.Sprintf("(%s)", strings.Join(stringifiedProgram, ", "))
}

type Option struct {
	IsSome bool
	Value  Node
}

var _ Node = &Option{}

func (o *Option) Type() NodeType {
	return OptionType
}

func (o *Option) String() string {
	if o.IsSome {
		return fmt.Sprintf("Some(%s)", o.Value.String())
	}
	return "None"
}
