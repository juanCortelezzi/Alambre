open Base

type t =
  | DataType
  | Builtin
  | Expression
[@@deriving sexp, compare]

and data_type =
  | Int of int
  | String of string
  | Function of t list
  | Array of data_type list

and builtin =
  | GT
  | LT
  | GTE
  | LTE
  | Equal
  | NotEqual
  | Not
  | And
  | Or
  | Add
  | Sub
  | Mul
  | Div
  | Status
  | Split
  | Map
  | Filter
  | Reduce
  | ToInt
  | Trim
  | RTrim
  | LTrim
  | OrElse
