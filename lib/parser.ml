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
    | Error _ when Token.equal p.token Token.RParen ->
      Ok (advance p, Ast.DataType (Function (List.rev ast)))
    | Error e -> Error e
  in
  loop p []

and parse_arr p =
  let rec loop p ast =
    match parse_token p with
    | Ok (p, Ast.DataType token) -> loop p (token :: ast)
    | Ok _ -> Error "value in array must be a data type"
    | Error _ when Token.equal p.token Token.Comma -> loop (advance p) ast
    | Error _ when Token.equal p.token Token.RCurly ->
      Ok (advance p, Ast.DataType (Array (List.rev ast)))
    | Error "end" -> Error "arr has no end"
    | Error e -> Error e
  in
  loop p []
;;

let get_ast p =
  let rec loop p ast =
    match parse_token p with
    | Ok (p, token) -> loop p (token :: ast)
    | Error "end" -> Ok (List.rev ast)
    | Error e -> Error e
  in
  loop p []
;;
