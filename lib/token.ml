open Base

(* type t = *)
(*   | Int of int *)
(*   | String of string *)
(*   | LParen *)
(*   | RParen *)
(*   | Add *)
(*   | Sub *)
(*   | Mul *)
(*   | Div *)
(*   | Status *)
(*   | Split *)
(*   | Map *)
(*   | ToInt *)
(*   | Trim *)
(*   | OrElse *)
(* [@@deriving sexp, compare] *)

type t =
  | DataType of data_type
  | Builtin of builtin
  | EOF
[@@deriving sexp, compare]

and data_type =
  | Int of int
  | String of string
  | Function of t list
  | Array of data_type list

and builtin =
  | Add
  | Sub
  | Mul
  | Div
  | Status
  | Split
  | Map
  | ToInt
  | Trim
  | OrElse

let lookup_builtin s =
  match s with
  | "add" -> Some Add
  | "sub" -> Some Sub
  | "mul" -> Some Mul
  | "div" -> Some Div
  | "status" -> Some Status
  | "split" -> Some Split
  | "map" -> Some Map
  | "to_int" -> Some ToInt
  | "trim" -> Some Trim
  | "or_else" -> Some OrElse
  | _ -> None
;;
