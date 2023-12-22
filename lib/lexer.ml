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

let lookup_keyword s =
  let open Token in
  match s with
  | "if" -> Some If
  | "else" -> Some Else
  | "end" -> Some End
  | _ -> None
;;

let lookup_builtin s =
  let open Token in
  match s with
  | "status" -> Some (Builtin Status)
  | "split" -> Some (Builtin Split)
  | "map" -> Some (Builtin Map)
  | "filter" -> Some (Builtin Filter)
  | "reduce" -> Some (Builtin Reduce)
  | "to_int" -> Some (Builtin ToInt)
  | "trim" -> Some (Builtin Trim)
  | "rtrim" -> Some (Builtin RTrim)
  | "ltrim" -> Some (Builtin LTrim)
  | "or_else" -> Some (Builtin OrElse)
  | _ -> None
;;

let advance l =
  let ch = string_get_opt l.input l.read_pos in
  if Option.is_none ch
  then (
    let input_len = String.length l.input in
    { l with pos = input_len; read_pos = input_len + 1; ch = None })
  else { l with pos = l.read_pos; read_pos = l.read_pos + 1; ch }
;;

let create input = { input; pos = 0; read_pos = 0; ch = None } |> advance

let rec skip_whitespace l =
  match l.ch with
  | Some ch when Char.is_whitespace ch -> advance l |> skip_whitespace
  | _ -> l
;;

let peek_is l ~value =
  match string_get_opt l.input l.read_pos, value with
  | Some a, Some b -> Char.equal a b
  | None, None -> true
  | _ -> false
;;

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
  | Some '(' -> advance l, LParen
  | Some ')' -> advance l, RParen
  | Some '{' -> advance l, LCurly
  | Some '}' -> advance l, RCurly
  | Some ',' -> advance l, Comma
  | Some '=' -> advance l, Builtin Equal
  | Some '!' when peek_is l ~value:(Some '=') -> advance (advance l), Builtin NotEqual
  | Some '!' -> advance l, Builtin Not
  | Some '>' when peek_is l ~value:(Some '=') -> advance (advance l), Builtin GTE
  | Some '>' -> advance l, Builtin GT
  | Some '<' when peek_is l ~value:(Some '=') -> advance (advance l), Builtin LTE
  | Some '<' -> advance l, Builtin LT
  | Some '&' when peek_is l ~value:(Some '&') -> advance (advance l), Builtin And
  | Some '|' when peek_is l ~value:(Some '|') -> advance (advance l), Builtin Or
  | Some '"' -> read_string l
  | Some ch when is_digit ch -> read_digit l
  | Some ch when is_char ch -> read_ident l
  | Some ch -> advance l, Illegal (String.of_char ch)
  | _ -> l, EOF

and read_digit l =
  let new_lexer, digit_str = read_while ~f:is_digit l in
  new_lexer, Token.Int (Int.of_string digit_str)

and read_ident l =
  let new_lexer, ident = read_while ~f:is_char l in
  match lookup_builtin ident with
  | Some builtin -> new_lexer, builtin
  | None ->
    (match lookup_keyword ident with
     | Some keyword -> new_lexer, keyword
     | None -> new_lexer, Token.Illegal ident)

and read_string l =
  let intermediate_lexer, inside_string =
    (* read_while reads until the ' that means that we need to advance the lexer
       to cover the whole string syntax from ' to ' *)
    read_while ~f:(fun c -> Char.equal '"' c |> not) (advance l)
  in
  let new_lexer = advance intermediate_lexer in
  new_lexer, String inside_string
;;

let collect_tokens l =
  let rec loop l tokens =
    let new_lexer, token = next_token l in
    match token with
    | EOF as token -> List.rev (token :: tokens)
    | token -> loop new_lexer (token :: tokens)
  in
  loop l []
;;

let%expect_test "read_while_char_is_alpha" =
  let input = "asdfadf" in
  let input_len = String.length input in
  let new_lexer, string = read_while ~f:Char.is_alpha (create input) in
  Stdlib.print_endline (Sexp.to_string_hum (sexp_of_t new_lexer));
  Stdlib.print_endline (Sexp.to_string_hum (String.sexp_of_t string));
  [%expect {|
    ((input asdfadf) (pos 7) (read_pos 8) (ch ()))
    asdfadf
  |}]
;;

let%expect_test "read_until_string_termination" =
  let input = "hola me llamo juan' outside" in
  let lexer, string = read_while ~f:(fun c -> Char.equal '\'' c |> not) (create input) in
  Stdlib.print_endline (Sexp.to_string_hum (sexp_of_t lexer));
  Stdlib.print_endline (Sexp.to_string_hum (String.sexp_of_t string));
  [%expect
    {|
    ((input "hola me llamo juan' outside") (pos 18) (read_pos 19) (ch (')))
    "hola me llamo juan"
  |}]
;;

let%expect_test "read_while_int" =
  let input = "1 2" in
  let input_len = String.length input in
  let new_lexer, string = read_while ~f:is_digit (create input) in
  Stdlib.print_endline (Sexp.to_string_hum (sexp_of_t new_lexer));
  Stdlib.print_endline (Sexp.to_string_hum (String.sexp_of_t string));
  [%expect {|
    ((input "1 2") (pos 1) (read_pos 2) (ch (" ")))
    1
  |}]
;;

let%expect_test "math operators" =
  let input = "1 2 + 20 *" in
  let tokens = collect_tokens (create input) in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Token.t list] tokens));
  [%expect {| ((Int 1) (Int 2) (Builtin Add) (Int 20) (Builtin Mul) EOF) |}]
;;

let%expect_test "function" =
  let input = "(1 + 10 *) map" in
  let tokens = collect_tokens (create input) in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Token.t list] tokens));
  [%expect
    {|
    (LParen (Int 1) (Builtin Add) (Int 10) (Builtin Mul) RParen (Builtin Map)
     EOF)
  |}]
;;

let%expect_test "strings" =
  let input = {|"hello there" map|} in
  let tokens = collect_tokens (create input) in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Token.t list] tokens));
  [%expect {| ((String "hello there") (Builtin Map) EOF) |}]
;;

let%expect_test "test_all_tokens" =
  let input =
    {|
  + - * / !
  >= > <= < != = && ||
  ( ) { } ,
  5 10 20
  "hello there"
  map split filter reduce
  if end if else end
  |}
  in
  let tokens = collect_tokens (create input) in
  Stdlib.print_endline (Sexp.to_string_hum ([%sexp_of: Token.t list] tokens));
  [%expect
    {|
    ((Builtin Add) (Builtin Sub) (Builtin Mul) (Builtin Div) (Builtin Not)
     (Builtin GTE) (Builtin GT) (Builtin LTE) (Builtin LT) (Builtin NotEqual)
     (Builtin Equal) (Builtin And) (Builtin Or) LParen RParen LCurly RCurly Comma
     (Int 5) (Int 10) (Int 20) (String "hello there") (Builtin Map)
     (Builtin Split) (Builtin Filter) (Builtin Reduce) If End If Else End EOF) |}]
;;
