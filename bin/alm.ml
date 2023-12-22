open Base

let usage = {|almrepl <lexer|parser|repl> input|}
let get_args () = Sys.get_argv () |> Array.to_list |> List.tl |> Option.value_exn

let lexer_repl input =
  let open Alambre in
  let rec loop index lexer =
    let new_lexer, token = Lexer.next_token lexer in
    Stdlib.print_endline (Token.to_string token);
    match token with
    | EOF -> ()
    | _ -> loop (index + 1) new_lexer
  in
  Lexer.create input |> loop 0
;;

let parser_repl input =
  let open Alambre in
  let parser = Parser.create (Lexer.create input) in
  let ast = Parser.collect_ast parser |> Result.ok_or_failwith in
  sexp_of_list Ast.sexp_of_t ast |> Sexp.to_string_hum |> Stdlib.print_endline
;;

let repl_repl input =
  let open Alambre in
  let parser = Parser.create (Lexer.create input) in
  Executor.create (Parser.collect_ast parser |> Result.ok_or_failwith)
  |> Executor.run
  |> ignore
;;

let () =
  let args = get_args () in
  match args with
  | [ "lexer"; input ] -> lexer_repl input
  | [ "parser"; input ] -> parser_repl input
  | [ "repl"; input ] -> repl_repl input
  | _ -> Stdlib.print_endline usage
;;
