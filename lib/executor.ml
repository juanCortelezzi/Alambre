open Base

type t =
  { stack : Ast.data_type list
  ; program : Ast.t list
  ; index : int
  }

let create ?(stack = []) program = { stack; program; index = 0 }

let rec run (r : t) : t =
  let step (run : t) =
    match List.nth run.program run.index with
    | None -> failwith "index out of bounds"
    | Some token ->
      (match token with
       | Ast.DataType d -> Ok { run with stack = d :: run.stack; index = run.index + 1 }
       | Ast.Builtin b ->
         execute_builtin run.stack b
         |> Result.map ~f:(fun stack -> { run with stack; index = run.index + 1 })
       | _ -> failwith "Ast token not implemented")
  in
  let rec loop (r : t) : t =
    match step r with
    | Ok r -> if r.index < List.length r.program then loop r else r
    | Error e ->
      alambre_status r.stack;
      failwith e
  in
  loop r

and execute_builtin stack b =
  let error_not_enough_elements b =
    Printf.sprintf
      "ERROR<%s> not enough elements in the stack"
      (Token.builtin_to_string b)
  in
  match b with
  | Token.Add ->
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
     | Function program :: Ast.Array arr :: rest ->
       Ok (alambre_arr_map arr program :: rest)
     | Function program :: Ast.Maybe res :: rest ->
       Ok (alambre_res_map res program :: rest)
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
     | def :: Ast.Maybe res :: rest -> Ok (alambre_orelse ~res ~def :: rest)
     | x :: _ ->
       Error
         (Printf.sprintf
            "trying to orElse something that should not be orElsed `%s`"
            (Ast.data_type_to_string x))
     | _ -> Error (error_not_enough_elements b))
  | Status -> Ok (alambre_status stack) |> Result.map ~f:(fun _ -> stack)
  | b -> Error (Printf.sprintf "builtin %s not implemented" (Token.builtin_to_string b))

and alambre_status stack =
  List.map stack ~f:Ast.data_type_to_string
  |> List.fold ~init:"" ~f:(fun acc s -> acc ^ " " ^ s)
  |> String.strip
  |> Stdlib.print_endline

and alambre_add a b = Ast.Int (a + b)
and alambre_sub a b = Ast.Int (b - a)

and alambre_split s at =
  Ast.Array (String.split ~on:at s |> List.map ~f:(fun s -> Ast.String s))

and alambre_to_int s =
  Int.of_string_opt s
  |> Option.value_map ~default:(Ast.Maybe Nothing) ~f:(fun i -> Ast.Maybe (Just (Int i)))

and alambre_trim s = Ast.String (String.strip s)

and alambre_orelse ~res ~def =
  match res with
  | Ast.Just d -> d
  | Ast.Nothing -> def

and alambre_arr_map arr program =
  let f d =
    let r = create program ?stack:(Some [ d ]) |> run in
    Option.value (List.hd r.stack) ~default:Ast.Void
  in
  Ast.Array (List.map ~f arr)

and alambre_res_map res program =
  match res with
  | Ast.Just d ->
    let r = create program ?stack:(Some [ d ]) |> run in
    Maybe (Just (Option.value (List.hd r.stack) ~default:Void))
  | Nothing -> Maybe res
;;
