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
  | Can of alambre_result

and alambre_result =
  | Data of data_type
  | Worm

and builtin =
  | Add
  | Sub
  | Status
  | Split
  | Map
  | ToInt
  | Trim
  | OrElse

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
  | Can d ->
    (match d with
     | Data d -> "Data(" ^ data_type_to_string d ^ ")"
     | Worm -> "Worm()")

and builtin_to_string b =
  match b with
  | Add -> "add"
  | Sub -> "sub"
  | Status -> "status"
  | Split -> "split"
  | Map -> "map"
  | ToInt -> "to_int"
  | Trim -> "trim"
  | OrElse -> "orelse"
;;

type runner =
  { stack : data_type list
  ; program : token list
  ; index : int
  }

let runner_new ?(stack = []) program = { stack; program; index = 0 }

let rec run_program (r : runner) : runner =
  let step (run : runner) =
    match List.nth run.program run.index with
    | None -> failwith "index out of bounds"
    | Some token ->
      (match token with
       | DataType d -> Ok { run with stack = d :: run.stack; index = run.index + 1 }
       | Builtin b ->
         execute_builtin run.stack b
         |> Result.map ~f:(fun stack -> { run with stack; index = run.index + 1 }))
  in
  let rec loop (r : runner) : runner =
    match step r with
    | Ok r -> if r.index < List.length r.program then loop r else r
    | Error e ->
      Stdlib.print_endline e;
      alambre_status r.stack;
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
     | Function program :: List arr :: rest -> Ok (alambre_arr_map arr program :: rest)
     | Function program :: Can res :: rest -> Ok (alambre_res_map res program :: rest)
     | _ :: _ :: _ -> Error "trying to fn something that should not be fned"
     | _ -> Error (error_not_enough_elements b))
  | ToInt ->
    (match stack with
     | String s :: rest -> Ok (alambre_to_int s :: rest)
     | _ :: _ -> Error "trying to parseInt something that should not be parsed"
     | _ -> Error (error_not_enough_elements b))
  | Trim ->
    (match stack with
     | String s :: rest -> Ok (alambre_trim s :: rest)
     | _ :: _ -> Error "trying to trim something that should not be trimmed"
     | _ -> Error (error_not_enough_elements b))
  | OrElse ->
    (match stack with
     | Can res :: def :: rest -> Ok (alambre_orelse res def :: rest)
     | _ :: _ -> Error "trying to orelse something that should not be orelsed"
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

and alambre_to_int s =
  Int.of_string_opt s
  |> Option.value_map ~default:(Can Worm) ~f:(fun i -> Can (Data (Int i)))

and alambre_trim s = String (String.strip s)

and alambre_orelse res def =
  Stdlib.print_endline (data_type_to_string def);
  match res with
  | Data d -> d
  | Worm -> def

and alambre_arr_map arr program =
  Stdlib.print_endline "arr_map";
  let f d =
    let r = runner_new program ?stack:(Some [ d ]) |> run_program in
    Option.value (List.hd r.stack) ~default:Void
  in
  List (List.map ~f arr)

and alambre_res_map res program =
  Stdlib.print_endline "res_map";
  match res with
  | Data d ->
    let r = runner_new program ?stack:(Some [ d ]) |> run_program in
    Can (Data (Option.value (List.hd r.stack) ~default:Void))
  | Worm -> Can res
;;

let () =
  (* "1 a 3 b" " " split (to_int (1 +) map (0 orelse) map) map # => [2, 0, 4, 0] *)
  let program =
    [ DataType (String "1 df 3")
    ; DataType (String " ")
    ; Builtin Split
    ; DataType
        (Function
           [ Builtin ToInt
           ; DataType (Function [ DataType (Int 1); Builtin Add ])
           ; Builtin Status
           ; Builtin Map
           ; DataType (Function [ DataType (Int 0); Builtin Status; Builtin OrElse ])
           ; Builtin Map
           ])
    ; Builtin Status
    ; Builtin Map
    ; Builtin Status
    ]
  in
  runner_new program |> run_program |> ignore
;;
