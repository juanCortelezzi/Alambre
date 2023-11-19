open Base

type t =
  { input : string
  ; pos : int
  ; read_pos : int
  ; ch : char option
  }
[@@deriving sexp_of, compare]

let string_get_opt s i =
  if i >= String.length s then None else Some (String.unsafe_get s i)
;;

let advance l =
  let ch = string_get_opt l.input l.read_pos in
  let new_lexer =
    if Option.is_none ch
    then (
      let input_len = String.length l.input in
      { l with pos = input_len; read_pos = input_len + 1; ch = None })
    else { l with pos = l.read_pos; read_pos = l.read_pos + 1; ch }
  in
  (* Stdlib.print_endline (sexp_of_t new_lexer |> Sexp.to_string_hum); *)
  new_lexer
;;

let create input = { input; pos = 0; read_pos = 0; ch = None } |> advance

let rec skip_whitespace l =
  match l.ch with
  | Some ch when Char.is_whitespace ch -> advance l |> skip_whitespace
  | _ -> l
;;

let peek l = string_get_opt l.input l.read_pos

let read_while ~f l =
  let rec loop f l =
    match l.ch with
    | Some ch when f ch -> advance l |> loop f
    | _ -> l
  in
  let new_lexer = loop f l in
  new_lexer, String.sub l.input ~pos:l.pos ~len:(new_lexer.pos - l.pos)
;;

let is_digit = Char.is_digit
let is_char c = Char.is_alpha c || Char.equal c '_'

let rec next_token l : t * Token.t =
  let l = skip_whitespace l in
  match l.ch with
  | Some '*' -> advance l, Builtin Mul
  | Some '+' -> advance l, Builtin Add
  | Some '-' -> advance l, Builtin Sub
  | Some '/' -> advance l, Builtin Div
  | Some '(' -> read_function l
  | Some '\'' -> read_string l
  | Some ch when is_digit ch -> read_digit l
  | Some ch when is_char ch -> read_ident l
  | _ -> l, EOF

and read_digit l =
  let new_lexer, digit_str = read_while ~f:is_digit l in
  new_lexer, Token.DataType (Int (Int.of_string digit_str))

and read_ident l =
  let new_lexer, ident = read_while ~f:is_char l in
  match Token.lookup_builtin ident with
  | Some builtin -> new_lexer, Builtin builtin
  | None -> failwith "Not implemented yet!"

and read_string l =
  let intermediate_lexer, inside_string =
    (* read_while reads until the ' that means that we need to advance the lexer
       to cover the whole string syntax from ' to ' *)
    read_while ~f:(fun c -> Char.equal '\'' c |> not) (advance l)
  in
  let new_lexer = advance intermediate_lexer in
  new_lexer, Token.DataType (String inside_string)

and read_function l =
  let rec read_between_parens lex ~counter =
    let lex, counter =
      match lex.ch with
      | Some '(' -> advance lex, counter + 1
      | Some ')' -> advance lex, counter - 1
      | None -> failwith "unbalanced parentheses"
      | _ -> advance lex, counter
    in
    if counter = 0 then lex else read_between_parens lex ~counter
  in
  let new_lexer = read_between_parens l ~counter:0 in
  let fn_program = String.sub l.input ~pos:(l.pos + 1) ~len:(new_lexer.pos - l.pos - 2) in
  let rec loop fn_lexer ~fn_tokens =
    let new_lex, token = next_token fn_lexer in
    match token with
    | EOF -> List.rev fn_tokens
    | _ -> loop new_lex ~fn_tokens:(token :: fn_tokens)
  in
  let fn_tokens = loop (create fn_program) ~fn_tokens:[] in
  new_lexer, Token.DataType (Function fn_tokens)
;;

let%test_unit "read_function" =
  let input = "(1 +) map" in
  let expected_l = { input; pos = 5; read_pos = 6; ch = Some ' ' } in
  let expected = Token.DataType (Function [ DataType (Int 1); Builtin Add ]) in
  let l = create input in
  let new_lexer, fn_tokens = read_function l in
  [%test_result: t] new_lexer ~expect:expected_l
;;

let%test_unit "read_while_str" =
  let input = "asdfadf" in
  let input_len = String.length input in
  let l = create input in
  let expected_l =
    { input = l.input; pos = input_len; read_pos = input_len + 1; ch = None }
  in
  let new_lexer, string = read_while ~f:Char.is_alpha l in
  [%test_result: t] new_lexer ~expect:expected_l;
  [%test_result: string] string ~expect:input
;;

let%test_unit "read_while_test" =
  let input = "'hola me llamo juan' outside" in
  let intermediate_lexer, string =
    read_while ~f:(fun c -> Char.equal '\'' c |> not) (create input |> advance)
  in
  let new_lexer = advance intermediate_lexer in
  [%test_result: string] string ~expect:"hola me llamo juan"
;;

let%test_unit "read_while_int" =
  let input = "1 2" in
  let input_len = String.length input in
  let l = create input in
  let expected_l = { input = l.input; pos = 1; read_pos = 2; ch = Some ' ' } in
  let new_lexer, string = read_while ~f:is_digit l in
  [%test_result: t] new_lexer ~expect:expected_l;
  [%test_result: string] string ~expect:"1"
;;

let%test_unit "math operators" =
  let input = "1 2 + 20 *" in
  let tokens =
    [ Token.DataType (Token.Int 1)
    ; Token.DataType (Token.Int 2)
    ; Token.Builtin Token.Add
    ; Token.DataType (Token.Int 20)
    ; Token.Builtin Token.Mul
    ; Token.EOF
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: Token.t] token ~expect:t;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;

let%test_unit "function" =
  let input = "(1 + 10 *) map" in
  let tokens =
    [ Token.DataType
        (Function [ DataType (Int 1); Builtin Add; DataType (Int 10); Builtin Mul ])
    ; Token.Builtin Map
    ; Token.EOF
    ]
  in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: Token.t] token ~expect:t;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;

let%test_unit "strings" =
  let input = "'hello there' map" in
  let tokens = [ Token.DataType (String "hello there"); Token.Builtin Map; Token.EOF ] in
  let rec loop index lexer =
    let new_lexer, token = next_token lexer in
    let expected_token = List.nth tokens index in
    match expected_token with
    | Some t ->
      [%test_result: Token.t] token ~expect:t;
      loop (index + 1) new_lexer
    | None -> ()
  in
  create input |> loop 0 |> ignore
;;
