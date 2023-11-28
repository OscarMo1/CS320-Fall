#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type constant = Int of int | Bool of bool | Unit
type command = Push of constant | Pop | Trace | Add | Sub | Mul | Div | And | Or | Not | Lt | Gt
type state = constant list * string list

let constant_to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "True" else "False"
  | Unit -> "Unit"

let string_of_char_list char_list =
  string_make_fwork (fun work -> list_foreach char_list (fun c -> work (c)))

let parse_constant s =
  let rec parse_digits chars accum neg =
    match chars with
    | '-' :: cs when accum = 0 && not neg -> parse_digits cs accum true
    | [] -> if neg = true then Some (-1 * accum) else Some accum
    | c :: cs ->
        if char_isdigit c then
          let digit = digit_of_char c in
          parse_digits cs (10 * accum + digit) neg
        else
          None
  in
  match s with
  | "True" -> Some (Bool true)
  | "False" -> Some (Bool false)
  | "Unit" -> Some Unit
  | _ ->
      match parse_digits (string_listize s) 0 false with
      | Some n -> Some (Int n)
      | None -> None

let split_string (s : string) : string list =
  let rec aux (cs : char list) (current : string) (acc : string list) : string list =
    match cs with
    | [] -> 
        if current = "" then acc else current :: acc
    | ';' :: rest -> 
        aux rest "" (current :: acc)
    | c :: rest ->
        if char_iswhitespace c then
          if current = "" then
            aux rest current acc
          else
            aux rest "" (current :: acc)
        else
          aux rest (current ^ str c) acc
  in 
  list_reverse (aux (string_listize s) "" [])

let parse_program s =
  let tokens = split_string s in
  let rec parse tokens =
    match tokens with
    | [] -> Some []
    | "" :: rest -> parse rest
    | "Push" :: value :: rest -> 
        (match parse_constant value with
        | Some const -> (match parse rest with
                         | Some cmds -> Some (Push const :: cmds)
                         | None -> None)
        | None -> None)
    | "Push" :: [] -> None
    | "Pop" :: rest -> 
        (match parse rest with
        | Some cmds -> Some (Pop :: cmds)
        | None -> None)
    | "Trace" :: rest -> 
        (match parse rest with
        | Some cmds -> Some (Trace :: cmds)
        | None -> None)
    | "Add" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Add :: cmds)
         | None -> None)
    | "Sub" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Sub :: cmds)
         | None -> None)
    | "Mul" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Mul :: cmds)
         | None -> None)
    | "Div" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Div :: cmds)
         | None -> None)
    | "And" :: rest ->
         (match parse rest with
         | Some cmds -> Some (And :: cmds)
         | None -> None)
    | "Or" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Or :: cmds)
         | None -> None)
    | "Not" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Not :: cmds)
         | None -> None)
    | "Lt" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Lt :: cmds)
         | None -> None)
    | "Gt" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Gt :: cmds)
         | None -> None)
    | _ -> None
  in
  parse tokens