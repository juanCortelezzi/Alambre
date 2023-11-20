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
  | EOF
[@@deriving sexp, compare]

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

let to_string t =
  match t with
  | Int i -> Int.to_string i
  | String s -> "'" ^ s ^ "'"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LCurly -> "LCurly"
  | RCurly -> "RCurly"
  | Comma -> "Comma"
  | EOF -> "EOF"
  | Illegal c -> "Illegal `" ^ c ^ "`"
  | Builtin b ->
    (match b with
     | GT -> "GT"
     | LT -> "LT"
     | GTE -> "GTE"
     | LTE -> "LTE"
     | Equal -> "Equal"
     | NotEqual -> "NotEqual"
     | Not -> "Not"
     | And -> "And"
     | Or -> "Or"
     | Add -> "Add"
     | Sub -> "Sub"
     | Mul -> "Mul"
     | Div -> "Div"
     | Status -> "Status"
     | Split -> "Split"
     | Map -> "Map"
     | Filter -> "Filter"
     | Reduce -> "Reduce"
     | ToInt -> "To_int"
     | Trim -> "Trim"
     | RTrim -> "Rtrim"
     | LTrim -> "Ltrim"
     | OrElse -> "Or_else")
;;
