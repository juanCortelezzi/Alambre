package executor

import (
	"errors"
	"fmt"
	"log"
	"strconv"
	"strings"

	"github.com/juancortelezzi/alambre/ast"
	"github.com/juancortelezzi/alambre/value"
)

var (
	ErrStackUnderflow = errors.New("stack underflow")
	ErrUnexpectedType = errors.New("unexpected type")
)

type Executor struct {
	stack []value.Node
}

func NewExecutor() *Executor {
	return &Executor{
		stack: make([]value.Node, 0),
	}
}

func NewExecutorWithStack(stack []value.Node) *Executor {
	return &Executor{
		stack: stack,
	}
}

func (e *Executor) String() string {
	s := ""
	for index, v := range e.stack {
		s += fmt.Sprintf("%d: %s\n", index, v.String())
	}
	return s
}

func executeBinop(stack []value.Node, binop *ast.Binop) ([]value.Node, error) {
	stackLen := len(stack)
	if stackLen < 2 {
		return nil, fmt.Errorf(
			"%w: BinOp(%s) expected 2 arguments but got %d at %d:%d",
			ErrStackUnderflow,
			binop.Type,
			stackLen,
			binop.Col,
			binop.Row,
		)

	}

	rhs, ok := stack[stackLen-1].(*value.Number)
	if !ok {
		return nil, fmt.Errorf(
			"%w: Binop(%s) expected a number but got '%s' at %d:%d",
			ErrUnexpectedType,
			binop.Type,
			stack[stackLen-1].String(),
			binop.Col,
			binop.Row,
		)
	}

	lhs, ok := stack[stackLen-2].(*value.Number)
	if !ok {
		return nil, fmt.Errorf(
			"%w: Binop(%s) expected a number but got '%s' at %d:%d",
			ErrUnexpectedType,
			binop.Type,
			stack[stackLen-2].String(),
			binop.Col,
			binop.Row,
		)
	}

	stack = stack[:stackLen-2]

	var result value.Node
	switch binop.Type {
	case ast.Addition:
		result = &value.Number{Value: lhs.Value + rhs.Value}
	case ast.Subtraction:
		result = &value.Number{Value: lhs.Value - rhs.Value}
	case ast.Multiplication:
		result = &value.Number{Value: lhs.Value * rhs.Value}
	case ast.Division:
		result = &value.Number{Value: lhs.Value / rhs.Value}
	case ast.LessThan:
		result = &value.Boolean{Value: lhs.Value < rhs.Value}
	case ast.LessThanEqual:
		result = &value.Boolean{Value: lhs.Value <= rhs.Value}
	case ast.GreaterThan:
		result = &value.Boolean{Value: lhs.Value > rhs.Value}
	case ast.GreaterThanEqual:
		result = &value.Boolean{Value: lhs.Value >= rhs.Value}
	case ast.Equal:
		result = &value.Boolean{Value: lhs.Value == rhs.Value}
	case ast.NotEqual:
		result = &value.Boolean{Value: lhs.Value != rhs.Value}
	default:
		log.Fatalf("binop '%s' not implemented", binop)
	}

	stack = append(stack, result)
	return stack, nil
}

func executeFnCall(stack []value.Node, fnCall *ast.FnCall) ([]value.Node, error) {
	if fnCall.Name == "status" {
		fmt.Println("Stack:")
		for index, v := range stack {
			fmt.Printf("%d: %s\n", index, v.String())
		}
		return stack, nil
	}

	if fnCall.Name == "split" {
		stackLen := len(stack)
		if stackLen < 2 {
			return nil, fmt.Errorf(
				"%w: split expected 2 arguments but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}
		delimeter, ok := stack[stackLen-1].(*value.String)
		if !ok {
			return nil, fmt.Errorf(
				"%w: split expected second argument to be the string delimeter but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		s, ok := stack[stackLen-2].(*value.String)
		if !ok {
			return nil, fmt.Errorf(
				"%w: split expected first argument to be a string but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-2].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-2]

		splits := strings.Split(s.Value, delimeter.Value)
		table := value.Table{
			Arr: make([]value.Node, 0, len(splits)),
		}
		for _, split := range splits {
			table.Arr = append(table.Arr, &value.String{Value: split})
		}

		stack = append(stack, &table)
		return stack, nil
	}

	if fnCall.Name == "map" {
		stackLen := len(stack)
		if stackLen < 2 {
			return nil, fmt.Errorf(
				"%w: map expected 2 arguments but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		fn, ok := stack[stackLen-1].(*value.Fn)
		if !ok {
			return nil, fmt.Errorf(
				"%w: map expected second argument to be a function at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		table, ok := stack[stackLen-2].(*value.Table)
		if !ok {
			return nil, fmt.Errorf(
				"%w: map expected first argument to be a table at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-2]

		newTable := value.Table{}

		if table.Arr != nil {
			newArr := make([]value.Node, 0, len(table.Arr))
			for _, item := range table.Arr {
				newStack := []value.Node{item}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: map expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				newArr = append(newArr, newStack[len(newStack)-1])
			}
			newTable.Arr = newArr
		}

		if table.Map != nil {
			newMap := make(map[value.Node]value.Node)
			for k, v := range table.Map {
				newStack := []value.Node{v}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: map expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				newMap[k] = newStack[len(newStack)-1]
			}
			newTable.Map = newMap
		}

		stack = append(stack, &newTable)
		return stack, nil
	}

	if fnCall.Name == "chars" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: chars expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		s, ok := stack[stackLen-1].(*value.String)
		if !ok {
			return nil, fmt.Errorf(
				"%w: chars expected first argument to be a string but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-1]

		newTable := value.Table{
			Arr: make([]value.Node, 0, len(s.Value)),
		}
		for _, c := range s.Value {
			newTable.Arr = append(newTable.Arr, &value.String{Value: string(c)})
		}

		stack = append(stack, &newTable)
		return stack, nil
	}

	if fnCall.Name == "to_int" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: to_int expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		s, ok := stack[stackLen-1].(*value.String)
		if !ok {
			return nil, fmt.Errorf(
				"%w: to_int expected first argument to be a string but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-1]

		num, err := strconv.ParseFloat(s.Value, 64)
		if err != nil {
			stack = append(stack, &value.Option{IsSome: false})
			return stack, nil
		}

		stack = append(stack, &value.Option{IsSome: true, Value: &value.Number{Value: num}})
		return stack, nil
	}

	if fnCall.Name == "filter" {
		stackLen := len(stack)
		if stackLen < 2 {
			return nil, fmt.Errorf(
				"%w: filter expected 2 arguments but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		fn, ok := stack[stackLen-1].(*value.Fn)
		if !ok {
			return nil, fmt.Errorf(
				"%w: filter expected second argument to be a function at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		table, ok := stack[stackLen-2].(*value.Table)
		if !ok {
			return nil, fmt.Errorf(
				"%w: filter expected first argument to be a table at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-2]

		newTable := value.Table{}

		if table.Arr != nil {
			newArr := make([]value.Node, 0, len(table.Arr))
			for _, item := range table.Arr {
				newStack := []value.Node{item}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: map expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				boolean, ok := newStack[len(newStack)-1].(*value.Boolean)
				if !ok {
					return nil, fmt.Errorf(
						"%w: map expected function to return boolean but got '%s' at %d:%d",
						ErrUnexpectedType,
						newStack[len(newStack)-1].String(),
						fnCall.Col,
						fnCall.Row,
					)
				}

				if boolean.Value {
					newArr = append(newArr, item)
				}
			}
			newTable.Arr = newArr
		}

		if table.Map != nil {
			newMap := make(map[value.Node]value.Node)
			for k, v := range table.Map {
				newStack := []value.Node{v}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: map expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				boolean, ok := newStack[len(newStack)-1].(*value.Boolean)
				if !ok {
					return nil, fmt.Errorf(
						"%w: map expected function to return boolean but got '%s' at %d:%d",
						ErrUnexpectedType,
						newStack[len(newStack)-1].String(),
						fnCall.Col,
						fnCall.Row,
					)
				}

				if boolean.Value {
					newMap[k] = v
				}
			}
			newTable.Map = newMap
		}

		stack = append(stack, &newTable)
		return stack, nil
	}

	if fnCall.Name == "is_some" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: is_some expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		option, ok := stack[stackLen-1].(*value.Option)
		if !ok {
			return nil, fmt.Errorf(
				"%w: is_some expected first argument to be an option but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-1]

		stack = append(stack, &value.Boolean{Value: option.IsSome})
		return stack, nil
	}

	if fnCall.Name == "unwrap" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: unwrap expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		option, ok := stack[stackLen-1].(*value.Option)
		if !ok {
			return nil, fmt.Errorf(
				"%w: unwrap expected first argument to be an option but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-1]

		if option.IsSome {
			stack = append(stack, option.Value)
			return stack, nil
		}

		return nil, fmt.Errorf(
			"%w: unwrap expected option to have a value but got none at %d:%d",
			ErrUnexpectedType,
			fnCall.Col,
			fnCall.Row,
		)
	}

	if fnCall.Name == "first" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: first expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		table, ok := stack[stackLen-1].(*value.Table)
		if !ok {
			return nil, fmt.Errorf(
				"%w: first expected first argument to be a table but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		if table.Arr != nil {
			if len(table.Arr) > 0 {
				stack = append(stack, table.Arr[0])
				return stack, nil
			}

			return nil, fmt.Errorf(
				"%w: first expected table to have at least 1 element but got 0 at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}
	}

	if fnCall.Name == "swap" {
		stackLen := len(stack)
		if stackLen < 2 {
			return nil, fmt.Errorf(
				"%w: swap expected 2 arguments but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack[stackLen-2], stack[stackLen-1] = stack[stackLen-1], stack[stackLen-2]

		return stack, nil
	}

	if fnCall.Name == "last" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: last expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		table, ok := stack[stackLen-1].(*value.Table)
		if !ok {
			return nil, fmt.Errorf(
				"%w: last expected first argument to be a table but got '%s' at %d:%d",
				ErrUnexpectedType,
				stack[stackLen-1].String(),
				fnCall.Col,
				fnCall.Row,
			)
		}

		if table.Arr != nil {
			if len(table.Arr) > 0 {
				stack = append(stack, table.Arr[len(table.Arr)-1])
				return stack, nil
			}

			return nil, fmt.Errorf(
				"%w: last expected table to have at least 1 element but got 0 at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}
	}

	if fnCall.Name == "drop" {
		stackLen := len(stack)
		if stackLen < 1 {
			return nil, fmt.Errorf(
				"%w: drop expected 1 argument but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-1]
		return stack, nil
	}

	if fnCall.Name == "reduce" {
		stackLen := len(stack)
		if stackLen < 3 {
			return nil, fmt.Errorf(
				"%w: reduce expected 3 arguments but got %d at %d:%d",
				ErrStackUnderflow,
				stackLen,
				fnCall.Col,
				fnCall.Row,
			)
		}

		fn, ok := stack[stackLen-1].(*value.Fn)
		if !ok {
			return nil, fmt.Errorf(
				"%w: reduce expected first argument to be a function at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		acc := stack[stackLen-2]

		table, ok := stack[stackLen-3].(*value.Table)
		if !ok {
			return nil, fmt.Errorf(
				"%w: reduce expected third argument to be a table at %d:%d",
				ErrUnexpectedType,
				fnCall.Col,
				fnCall.Row,
			)
		}

		stack = stack[:stackLen-3]

		if table.Arr != nil {
			for _, item := range table.Arr {
				newStack := []value.Node{acc, item}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: reduce expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				acc = newStack[len(newStack)-1]
			}
		}

		if table.Map != nil {
			for _, v := range table.Map {
				newStack := []value.Node{acc, v}
				for _, node := range fn.Program {
					newStack = Eval(newStack, node)
				}
				if len(newStack) < 1 {
					return nil, fmt.Errorf(
						"%w: reduce expected function to return at least 1 value but got %d at %d:%d",
						ErrUnexpectedType,
						len(newStack),
						fnCall.Col,
						fnCall.Row,
					)
				}
				acc = newStack[len(newStack)-1]
			}
		}

		stack = append(stack, acc)
		return stack, nil
	}

	log.Fatalf("fn '%s' not implemented", fnCall.Name)
	return nil, nil
}

func executeFn(stack []value.Node, fn *ast.Fn) ([]value.Node, error) {
	function := value.Fn{
		Program: fn.Program,
	}
	stack = append(stack, &function)
	return stack, nil
}

func Eval(stack []value.Node, node ast.Node) []value.Node {
	switch n := node.(type) {
	case *ast.Number:
		return append(stack, &value.Number{Value: n.Value})
	case *ast.Boolean:
		return append(stack, &value.Boolean{Value: n.Value})
	case *ast.String:
		return append(stack, &value.String{Value: n.Value})
	case *ast.Ident:
		panic("todo ident")
	case *ast.Table:
		panic("todo table")
	case *ast.Fn:
		val, err := executeFn(stack, n)
		if err != nil {
			panic(err)
		}
		return val
	case *ast.FnCall:
		val, err := executeFnCall(stack, n)
		if err != nil {
			panic(err)
		}
		return val
	case *ast.Binop:
		val, err := executeBinop(stack, n)
		if err != nil {
			panic(err)
		}
		return val
	default:
		log.Fatalf("not implemented '%s'", n)
	}
	panic("not implemented")
}

func (e *Executor) Execute(program []ast.Node) {
	for _, node := range program {
		e.stack = Eval(e.stack, node)
	}
}
