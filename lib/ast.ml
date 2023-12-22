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
  | Maybe of maybe
  | Void

and maybe =
  | Just of data_type
  | Nothing

and expresion = If of if_expression

and if_expression =
  { consequence : t list
  ; alternative : t list
  }

let rec to_string t =
  match t with
  | DataType d -> data_type_to_string d
  | Builtin b -> Token.builtin_to_string b
  | Expression e -> expresion_to_string e

and data_type_to_string d =
  match d with
  | Int i -> Printf.sprintf "Int(%d)" i
  | String s -> Printf.sprintf "String(%s)" s
  | Function _ -> "Function"
  | Array a ->
    Printf.sprintf
      "Array(%s)"
      (String.concat ~sep:", " (List.map ~f:data_type_to_string a))
  | Maybe m ->
    (match m with
     | Just d -> Printf.sprintf "Just(%s)" (data_type_to_string d)
     | Nothing -> "Nothing")
  | Void -> "Void"

and expresion_to_string e =
  match e with
  | If i ->
    Printf.sprintf
      "If(%s, %s)"
      (String.concat ~sep:", " (List.map ~f:to_string i.consequence))
      (String.concat ~sep:", " (List.map ~f:to_string i.alternative))
;;
