// [compiler](https://c9x.me/compile/)
//
// [lua_reference](https://www.jucs.org/jucs_11_7/the_implementation_of_lua/jucs_11_7_1159_1176_defigueiredo.html)
package ast

import (
	"fmt"
	"strings"
)

type Node interface {
	String() string
	Coords() (int, int)
}

type Number struct {
	Value float64
	Col   int
	Row   int
}

var _ Node = &Number{}

func (n *Number) String() string {
	return fmt.Sprintf("Number(%f)", n.Value)
}

func (n *Number) Coords() (int, int) {
	return n.Col, n.Row
}

type Boolean struct {
	Value bool
	Col   int
	Row   int
}

var _ Node = &Boolean{}

func (b *Boolean) String() string {
	return fmt.Sprintf("Boolean(%t)", b.Value)
}

func (b *Boolean) Coords() (int, int) {
	return b.Col, b.Row
}

type String struct {
	Value string
	Col   int
	Row   int
}

var _ Node = &String{}

func (s *String) String() string {
	return fmt.Sprintf("String(%s)", s.Value)
}

func (s *String) Coords() (int, int) {
	return s.Col, s.Row
}

type Ident struct {
	Name string
	Col  int
	Row  int
}

var _ Node = &Ident{}

func (i *Ident) String() string {
	return fmt.Sprintf("Ident(%s)", i.Name)
}

func (i *Ident) Coords() (int, int) {
	return i.Col, i.Row
}

type Table struct {
	Items []TableItem
	Col   int
	Row   int
}

type TableItem struct {
	IsKvp bool
	Key   string
	Value []Node
}

var _ Node = &Table{}

func (t *Table) String() string {
	stringifiedItems := make([]string, 0, len(t.Items))
	for _, item := range t.Items {
		stringifiedItem := make([]string, 0, len(item.Value))
		for _, i := range item.Value {
			stringifiedItem = append(stringifiedItem, i.String())
		}

		var prefix string
		if item.IsKvp {
			prefix = fmt.Sprintf("%s: ", item.Key)
		}
		stringifiedItems = append(stringifiedItems, prefix+strings.Join(stringifiedItem, " "))
	}

	itemsString := strings.Join(stringifiedItems, ",\n")

	return fmt.Sprintf("Table([\n%s\n])", itemsString)
}

func (t *Table) Coords() (int, int) {
	return t.Col, t.Row
}

type Fn struct {
	Program []Node
	Col     int
	Row     int
}

var _ Node = &Fn{}

func (f *Fn) String() string {
	stringifiedProgram := make([]string, 0, len(f.Program))
	for _, item := range f.Program {
		stringifiedProgram = append(stringifiedProgram, item.String())
	}

	return fmt.Sprintf("Fn(%s)", strings.Join(stringifiedProgram, ", "))
}

func (f *Fn) Coords() (int, int) {
	return f.Col, f.Row
}

type FnCall struct {
	Name string
	Col  int
	Row  int
}

var _ Node = &FnCall{}

func (f *FnCall) String() string {
	return fmt.Sprintf("FnCall(%s)", f.Name)
}

func (f *FnCall) Coords() (int, int) {
	return f.Col, f.Row
}

type Binop struct {
	Type BinopType
	Col  int
	Row  int
}

var _ Node = &Binop{}

func (b *Binop) String() string {
	return fmt.Sprintf("Binop(%s)", b.Type)
}

func (b *Binop) Coords() (int, int) {
	return b.Col, b.Row
}
