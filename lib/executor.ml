open Base

type t =
  { stack : Ast.data_type list
  ; ast : Ast.t list
  ; index : int
  }

let create ?(stack = []) ast = { stack; ast; index = 0 }

let rec run t =
  let step t =
    match List.nth t.ast t.index with
    | None -> failwith "index out of bounds"
    | Some token ->
      (match token with
       | DataType d -> Ok { t with stack = d :: t.stack; index = t.index + 1 }
       | Builtin b ->
         execute_builtin t.stack b
         |> Result.map ~f:(fun stack -> { t with stack; index = t.index + 1 })
       | Expression e ->
         execute_expression t.stack e
         |> Result.map ~f:(fun stack -> { t with stack; index = t.index + 1 }))
  in
  match step t with
  | Ok t -> if t.index < List.length t.ast then run t else t
  | Error e ->
    alambre_status t.stack;
    failwith e

and execute_expression stack expr =
  match expr with
  | If if_expression ->
    (match stack with
     | Bool b :: rest ->
       if b
       then Ok (run (create ~stack if_expression.consequence)).stack
       else Ok (run (create ~stack if_expression.alternative)).stack
     | _ :: _ -> Error "trying to use non boolean value for if expression"
     | _ -> Error (Printf.sprintf "not enough elements in the stack for if expression"))

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
  | Equal ->
    (match stack with
     | Int a :: Int b :: rest -> Ok (Bool (a = b) :: rest)
     | String a :: String b :: rest -> Ok (Bool (String.equal a b) :: rest)
     | Bool a :: Bool b :: rest -> Ok (Bool (Bool.equal a b) :: rest)
     | _ :: _ :: rest -> Error "trying to equal something that should not be equalled"
     | _ -> Error (error_not_enough_elements b))
  | Not ->
    (match stack with
     | Bool a :: rest -> Ok (Bool (not a) :: rest)
     | _ :: rest -> Error "trying to not something that should not be notted"
     | _ -> Error (error_not_enough_elements b))
  | Split ->
    (match stack with
     | String on :: String s :: rest -> Ok (alambre_split s (Char.of_string on) :: rest)
     | _ :: _ :: _ -> Error "trying to split something that should not be splitted"
     | _ -> Error (error_not_enough_elements b))
  | Map ->
    (match stack with
     | Function fn :: Array arr :: rest -> Ok (alambre_arr_map arr fn :: rest)
     | Function fn :: Maybe res :: rest -> Ok (alambre_res_map res fn :: rest)
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
     | default :: Maybe maybe :: rest -> Ok (alambre_orelse ~maybe ~default :: rest)
     | x :: _ ->
       Error
         (Printf.sprintf
            "trying to orElse something that should not be orElsed `%s`"
            (Ast.data_type_to_string x))
     | _ -> Error (error_not_enough_elements b))
  | Status ->
    alambre_status stack;
    Ok stack
  | Print ->
    alambre_print stack;
    Ok stack
  | b -> Error (Printf.sprintf "builtin %s not implemented" (Token.builtin_to_string b))

and alambre_print stack =
  match stack with
  | something :: rest -> Stdlib.print_endline (Ast.data_type_to_string something)
  | _ -> Stdlib.print_endline (Ast.data_type_to_string Ast.Void)

and alambre_status stack =
  List.map stack ~f:Ast.data_type_to_string
  |> List.fold ~init:"" ~f:(fun acc s -> acc ^ " " ^ s)
  |> String.strip
  |> Stdlib.print_endline

and alambre_add a b = Int (a + b)
and alambre_sub a b = Int (b - a)

and alambre_split s at =
  Array (String.split ~on:at s |> List.map ~f:(fun s -> Ast.String s))

and alambre_to_int s =
  Int.of_string_opt s
  |> Option.value_map ~default:(Ast.Maybe Nothing) ~f:(fun i -> Maybe (Just (Int i)))

and alambre_trim s = String (String.strip s)

and alambre_orelse ~maybe ~default =
  match maybe with
  | Just data -> data
  | Nothing -> default

and alambre_arr_map arr fn =
  let f d =
    let r = create fn ?stack:(Some [ d ]) |> run in
    Option.value (List.hd r.stack) ~default:Void
  in
  Array (List.map ~f arr)

and alambre_res_map maybe ast =
  match maybe with
  | Just d ->
    let r = create ast ?stack:(Some [ d ]) |> run in
    Maybe (Just (Option.value (List.hd r.stack) ~default:Void))
  | Nothing -> Maybe maybe
;;
