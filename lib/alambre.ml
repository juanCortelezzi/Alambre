open Base

let test_program = {|
  print a:"hello world!"
|}

(*
   Tokenizer output:
   Builtin Print, Ident "a", Colon, String "hello world!"
*)

type builtin =
  | Print
  | ParseInt
  | Split
  | Map
  | Trim
  | TrimLeft
  | TrimRight

type keyword = Input

type token =
  | Ident of string
  | Keyword of keyword
  | Builtin of builtin
  | String of string
  | Int of int
  | SquareBraceLeft
  | SquareBraceRight
  | ParenLeft
  | ParenRight
  | Colon

let lookup_keyword str =
  match str with
  | "input" -> Keyword Input
  | _ -> Ident str
;;

let lookup_builtin str =
  match str with
  | "print" -> Builtin Print
  | "parse_int" -> Builtin ParseInt
  | "split" -> Builtin Split
  | "map" -> Builtin Map
  | "trim" -> Builtin Trim
  | "trim_left" -> Builtin TrimLeft
  | "trim_right" -> Builtin TrimRight
  | _ -> Ident str
;;

type lexer =
  { input : string
  ; position : int
  ; ch : char option
  }
[@@deriving show]

let new_lexer input =
  if String.length input = 0
  then { input; position = 0; ch = None }
  else { input; position = 0; ch = Some (String.get input 0) }
;;

let advance lexer =
  let input_len = String.length lexer.input in
  let position = lexer.position + 1 in
  if position >= input_len - 1
  then { lexer with ch = None }
  else { lexer with position; ch = Some (String.get lexer.input position) }
;;

let rec seek lexer condition =
  (* skip until condition is not met *)
  if condition lexer.ch then seek (advance lexer) condition else lexer
;;

let skip_whitespace lexer =
  seek lexer (fun ch ->
    match ch with
    | Some ch -> Char.is_whitespace ch
    | None -> false)
;;

let print_current_char lexer =
  Stdlib.Printf.printf
    "`%s`\n"
    (match lexer.ch with
     | Some ch -> Char.to_string ch
     | None -> "None")
;;

let () =
  Stdlib.print_endline "------------------------------";
  Stdlib.print_endline test_program;
  Stdlib.print_endline "------------------------------";
  let lexer = new_lexer test_program in
  let lexer = skip_whitespace lexer in
  print_current_char lexer;
  let lexer = advance lexer in
  print_current_char lexer;
  let lexer = advance lexer in
  print_current_char lexer;
  Stdlib.print_endline "Hello, World!"
;;
