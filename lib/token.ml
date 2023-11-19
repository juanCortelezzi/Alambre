open Base

type t =
  | Int of int
  | String of string
  | Builtin of builtin
  | Illegal of char
  | LParen
  | RParen
  | LCurly
  | RCurly
  | GT
  | LT
  | GTE
  | LTE
  | Equal
  | NotEqual
  | Not
  | And
  | Or
  | Comma
  | EOF
[@@deriving sexp, compare]

and builtin =
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

let lookup_builtin s =
  match s with
  | "status" -> Some Status
  | "split" -> Some Split
  | "map" -> Some Map
  | "filter" -> Some Filter
  | "reduce" -> Some Reduce
  | "to_int" -> Some ToInt
  | "trim" -> Some Trim
  | "rtrim" -> Some RTrim
  | "ltrim" -> Some LTrim
  | "or_else" -> Some OrElse
  | _ -> None
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
  | GT -> "GT"
  | LT -> "LT"
  | GTE -> "GTE"
  | LTE -> "LTE"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | Not -> "Not"
  | And -> "And"
  | Or -> "Or"
  | EOF -> "EOF"
  | Illegal c -> "Illegal `" ^ String.of_char c ^ "`"
  | Builtin b ->
    (match b with
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
     | RTrim -> "rtrim"
     | LTrim -> "ltrim"
     | OrElse -> "or_else")
;;
