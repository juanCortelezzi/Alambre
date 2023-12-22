open Base

type t =
  { lexer : Lexer.t
  ; token : Token.t
  ; peek_token : Token.t
  }

let advance p =
  let lexer, peek_token = Lexer.next_token p.lexer in
  { lexer; token = p.peek_token; peek_token }
;;

let create lexer = { lexer; token = EOF; peek_token = EOF } |> advance |> advance

let rec parse_token p =
  match p.token with
  | Int i -> Ok (advance p, Ast.DataType (Int i))
  | String s -> Ok (advance p, DataType (String s))
  | Builtin b -> Ok (advance p, Builtin b)
  (* | If -> parse_if (advance p) *)
  | LParen -> parse_fn (advance p)
  | LCurly -> parse_arr (advance p)
  | EOF -> Error "end"
  | _ -> Error ("unexpected: " ^ Token.to_string p.token)

and parse_fn p =
  let rec loop p ast =
    match parse_token p with
    | Ok (p, token) -> loop p (token :: ast)
    | Error "end" -> Error "fn has no end"
    | Error _ when Token.equal p.token RParen ->
      Ok (advance p, Ast.DataType (Function (List.rev ast)))
    | Error e -> Error e
  in
  loop p []

and parse_arr p =
  let rec loop p ast =
    match parse_token p with
    | Ok (p, DataType token) -> loop p (token :: ast)
    | Ok _ -> Error "value in array must be a data type"
    | Error _ when Token.equal p.token Comma -> loop (advance p) ast
    | Error _ when Token.equal p.token RCurly ->
      Ok (advance p, Ast.DataType (Array (List.rev ast)))
    | Error "end" -> Error "arr has no end"
    | Error e -> Error e
  in
  loop p []
;;

let collect_ast p =
  let rec loop p ast =
    match parse_token p with
    | Ok (p, token) -> loop p (token :: ast)
    | Error "end" -> Ok (List.rev ast)
    | Error e -> Error e
  in
  loop p []
;;

let%expect_test "parse_ints" =
  let input = "1 2 3" in
  let parser = create (Lexer.create input) in
  let ast = collect_ast parser |> Result.ok_or_failwith in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Ast.t list] ast));
  [%expect {| ((DataType (Int 1)) (DataType (Int 2)) (DataType (Int 3))) |}]
;;

let%expect_test "parse_strings" =
  let input = {|"1" "2" "3" "100 200 300"|} in
  let parser = create (Lexer.create input) in
  let ast = collect_ast parser |> Result.ok_or_failwith in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Ast.t list] ast));
  [%expect
    {|
      ((DataType (String 1)) (DataType (String 2)) (DataType (String 3))
       (DataType (String "100 200 300"))) 
  |}]
;;

let%expect_test "parse_builtins" =
  let input = "+ - * / map or_else split" in
  let parser = create (Lexer.create input) in
  let ast = collect_ast parser |> Result.ok_or_failwith in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Ast.t list] ast));
  [%expect
    {|
    ((Builtin Add) (Builtin Sub) (Builtin Mul) (Builtin Div) (Builtin Map)
     (Builtin OrElse) (Builtin Split))
  |}]
;;

let%expect_test "parse_fn" =
  let input = "(+ 1 2)" in
  let parser = create (Lexer.create input) in
  let ast = collect_ast parser |> Result.ok_or_failwith in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Ast.t list] ast));
  [%expect
    {|
    ((DataType (Function ((Builtin Add) (DataType (Int 1)) (DataType (Int 2))))))
  |}]
;;
