open Base

type t =
  | DataType of data_type
  | Builtin of Token.builtin
  | Expression of expresion
[@@deriving sexp, compare]

and data_type =
  | Int of int
  | String of string
  | Function of t list
  | Array of data_type list

and expresion = If of if_expression

and if_expression =
  { consequence : t list
  ; alternative : t list
  }
