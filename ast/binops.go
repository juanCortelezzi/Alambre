package ast

type BinopType string

const (
	Addition         BinopType = "Addition"
	Subtraction      BinopType = "Subtraction"
	Multiplication   BinopType = "Multiplication"
	Division         BinopType = "Division"
	LessThan         BinopType = "LessThan"
	GreaterThan      BinopType = "GreaterThan"
	LessThanEqual    BinopType = "LessThanEqual"
	GreaterThanEqual BinopType = "GreaterThanEqual"
	Equal            BinopType = "Equal"
	NotEqual         BinopType = "NotEqual"
	And              BinopType = "And"
	Or               BinopType = "Or"
	Not              BinopType = "Not"
)
