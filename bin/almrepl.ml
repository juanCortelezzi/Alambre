open Base

let usage = {|almrepl <program>|}
let get_args () = Sys.get_argv () |> Array.to_list |> List.tl |> Option.value_exn

let repl input =
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

let () =
  let args = get_args () in
  match args with
  | [ input ] -> repl input
  | _ -> Stdlib.print_endline usage
;;
