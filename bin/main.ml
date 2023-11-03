open Base

type data_type =
  | Void
  | Int of int
  | String of string
  | List of data_type list

let rec data_type_to_string d =
  match d with
  | Int i -> Int.to_string i
  | String s -> "\"" ^ s ^ "\""
  | List l -> "[" ^ (List.map l ~f:data_type_to_string |> String.concat ~sep:" ") ^ "]"
  | Void -> "void"
;;

type builtin =
  | Add
  | Sub
  | Status
  | Split
(* | Map *)

let builtin_to_string b =
  match b with
  | Add -> "add"
  | Sub -> "sub"
  | Status -> "status"
  | Split -> "split"
;;

(* | Map -> "map" *)

type token =
  | DataType of data_type
  | Builtin of builtin

type runner =
  { stack : data_type list
  ; program : token list
  ; index : int
  }

let runner_new p = { stack = []; program = p; index = 0 }

let alambre_status stack =
  List.map stack ~f:data_type_to_string
  |> List.fold ~init:"" ~f:(fun acc s -> acc ^ " " ^ s)
  |> String.strip
  |> Stdlib.print_endline;
  Ok stack
;;

let alambre_add a b = Int (a + b)
let alambre_sub a b = Int (b - a)
(* | _ -> Error "trying to sub something that should not be subbed" *)

let alambre_split s at = List (String.split ~on:at s |> List.map ~f:(fun s -> String s))
(* | _ -> Error "trying to split something that should not be splitted" *)

(* let alambre_map arr fn = *)

let execute_builtin stack b =
  let error_not_enough_elements b =
    Printf.sprintf "ERROR<%s> not enough elements in the stack" (builtin_to_string b)
  in
  match b with
  | Add ->
    (match stack with
     | Int a :: Int b :: rest -> Ok (alambre_add a b :: rest)
     | _ :: _ :: _ -> Error "trying to add something that should not be added"
     | _ -> Error (error_not_enough_elements b))
  | Sub ->
    (match stack with
     | Int a :: Int b :: rest -> Ok (alambre_sub a b :: rest)
     | _ :: _ :: _ -> Error "trying to sub something that should not be subbed"
     | _ -> Error (error_not_enough_elements b))
  | Split ->
    (match stack with
     | String s :: String on :: rest -> Ok (alambre_split s (Char.of_string on) :: rest)
     | _ :: _ :: _ -> Error "trying to split something that should not be splitted"
     | _ -> Error (error_not_enough_elements b))
  (* | Map -> *)
  (*   (match stack with *)
  (*    | List arr :: fn :: rest -> Ok (alambre_map arr fn :: rest) *)
  (*    | _ -> Error (error_not_enough_elements b)) *)
  | Status -> alambre_status stack
;;

let run_program r =
  let step r =
    match List.nth r.program r.index with
    | None -> Error "we have finished"
    | Some token ->
      (match token with
       | DataType d -> Ok { r with stack = d :: r.stack; index = r.index + 1 }
       | Builtin b ->
         execute_builtin r.stack b
         |> Result.map ~f:(fun stack -> { r with stack; index = r.index + 1 }))
  in
  let rec loop r =
    match step r with
    | Ok r -> loop r
    | Error e -> Stdlib.print_endline e
  in
  loop r
;;

let () =
  (* let falopa = {|"1 2 3 4" 8 3 status sub status|} in *)
  let program =
    [ DataType (String "1 2 3 4")
    ; DataType (Int 8)
    ; DataType (Int 3)
    ; Builtin Status
    ; Builtin Sub
    ; Builtin Status
    ]
  in
  runner_new program |> run_program
;;
