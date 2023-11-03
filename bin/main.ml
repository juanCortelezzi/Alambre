open Base

type token =
  | DataType of data_type
  | Builtin of builtin

and data_type =
  | Void
  | Int of int
  | String of string
  | List of data_type list
  | Function of token list

and builtin =
  | Add
  | Sub
  | Status
  | Split
  | Map

let rec token_to_string t =
  match t with
  | DataType d -> data_type_to_string d
  | Builtin b -> builtin_to_string b

and data_type_to_string d =
  match d with
  | Int i -> Int.to_string i
  | String s -> "\"" ^ s ^ "\""
  | List l -> "[" ^ (List.map l ~f:data_type_to_string |> String.concat ~sep:" ") ^ "]"
  | Function t -> "fn(" ^ (List.map ~f:token_to_string t |> String.concat ~sep:" ") ^ ")"
  | Void -> "void"

and builtin_to_string b =
  match b with
  | Add -> "add"
  | Sub -> "sub"
  | Status -> "status"
  | Split -> "split"
  | Map -> "map"
;;

type runner =
  { stack : data_type list
  ; program : token list
  ; index : int
  }

let runner_new p = { stack = []; program = p; index = 0 }

let rec run_program r =
  let step r =
    match List.nth r.program r.index with
    | None -> failwith "index out of bounds"
    | Some token ->
      (match token with
       | DataType d -> Ok { r with stack = d :: r.stack; index = r.index + 1 }
       | Builtin b ->
         execute_builtin r.stack b
         |> Result.map ~f:(fun stack -> { r with stack; index = r.index + 1 }))
  in
  let rec loop runner =
    match step runner with
    | Ok r -> if r.index < List.length r.program then loop r else r
    | Error e ->
      Stdlib.print_endline e;
      alambre_status runner.stack;
      Stdlib.exit 1
  in
  loop r

and execute_builtin stack b =
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
     | String on :: String s :: rest -> Ok (alambre_split s (Char.of_string on) :: rest)
     | _ :: _ :: _ -> Error "trying to split something that should not be splitted"
     | _ -> Error (error_not_enough_elements b))
  | Map ->
    (match stack with
     | Function program :: List arr :: rest -> Ok (alambre_map arr program :: rest)
     | _ :: _ :: _ -> Error "trying to fn something that should not be fned"
     | _ -> Error (error_not_enough_elements b))
  | Status -> Ok (alambre_status stack) |> Result.map ~f:(fun _ -> stack)

and alambre_status stack =
  List.map stack ~f:data_type_to_string
  |> List.fold ~init:"" ~f:(fun acc s -> acc ^ " " ^ s)
  |> String.strip
  |> Stdlib.print_endline

and alambre_add a b = Int (a + b)
and alambre_sub a b = Int (b - a)
and alambre_split s at = List (String.split ~on:at s |> List.map ~f:(fun s -> String s))

and alambre_map arr program =
  let f d =
    let runner = runner_new program in
    let runner = run_program { runner with stack = [ d ] } in
    Option.value (List.hd runner.stack) ~default:Void
  in
  List (List.map ~f arr)
;;

let () =
  (* let falopa = {|"1 2 3 4" 8 3 status sub status|} in *)
  let program =
    [ DataType (List [ Int 1; Int 2 ])
    ; DataType (Function [ DataType (Int 1); Builtin Add ])
    ; Builtin Status
    ; Builtin Map
    ; Builtin Status
    ]
  in
  runner_new program |> run_program |> ignore
;;
