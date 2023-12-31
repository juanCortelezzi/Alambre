open Base

type t =
  | DataType of data_type
  | Builtin of Token.builtin
  | Expression of expression
[@@deriving sexp, compare]

and data_type =
  | Int of int
  | Bool of bool
  | String of string
  | Function of t list
  | Array of data_type list
  | Maybe of maybe
  | Void

and maybe =
  | Just of data_type
  | Nothing

and expression = If of if_expression

and if_expression =
  { consequence : t list
  ; alternative : t list
  }

let rec to_string t =
  match t with
  | DataType d -> data_type_to_string d
  | Builtin b -> Token.builtin_to_string b
  | Expression e -> expression_to_string e

and data_type_to_string d =
  match d with
  | Int i -> Printf.sprintf "%d" i
  | Bool b -> Printf.sprintf "%b" b
  | String s -> Printf.sprintf "%s" s
  | Function f -> Printf.sprintf "(%s)" (String.concat ~sep:" " (List.map ~f:to_string f))
  | Array a ->
    Printf.sprintf "{%s}" (String.concat ~sep:", " (List.map ~f:data_type_to_string a))
  | Maybe m ->
    (match m with
     | Just d -> Printf.sprintf "Just(%s)" (data_type_to_string d)
     | Nothing -> "Nothing")
  | Void -> "Void"

and expression_to_string e =
  match e with
  | If i ->
    let has_alternative = List.is_empty i.alternative |> not in
    if has_alternative
    then
      Printf.sprintf
        "if %s else %s"
        (String.concat ~sep:" " (List.map ~f:to_string i.consequence))
        (String.concat ~sep:" " (List.map ~f:to_string i.alternative))
    else
      Printf.sprintf
        "if %s"
        (String.concat ~sep:" " (List.map ~f:to_string i.consequence))
;;
