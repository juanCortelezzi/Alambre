open Base

type t =
  | Int of int
  | String of string
  | Builtin of builtin
  | Illegal of string
  | LParen
  | RParen
  | LCurly
  | RCurly
  | Comma
  | If
  | Else
  | End
  | EOF
[@@deriving sexp, compare, equal]

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
  | Print

let builtin_to_string b =
  match b with
  | GT -> ">"
  | LT -> "<"
  | GTE -> ">="
  | LTE -> "<="
  | Equal -> "="
  | NotEqual -> "!="
  | Not -> "!"
  | And -> "&&"
  | Or -> "||"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Status -> "status"
  | Split -> "split"
  | Map -> "map"
  | Filter -> "filter"
  | Reduce -> "reduce"
  | ToInt -> "to_int"
  | Trim -> "trim"
  | RTrim -> "trim_r"
  | LTrim -> "trim_l"
  | OrElse -> "or_else"
  | Print -> "print"
;;

let to_string t =
  match t with
  | Int i -> Int.to_string i
  | String s -> "'" ^ s ^ "'"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LCurly -> "LCurly"
  | RCurly -> "RCurly"
  | Comma -> "Comma"
  | If -> "If"
  | Else -> "Else"
  | End -> "End"
  | EOF -> "EOF"
  | Illegal c -> "Illegal `" ^ c ^ "`"
  | Builtin b -> builtin_to_string b
;;
