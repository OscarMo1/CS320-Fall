#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* 
# specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

#use "./../../../../Classlib/OCaml/MyOCaml.ml";;

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

(* Helper functions *)
let constant_to_string = function
  | Int i -> string_of_int i
  | Bool b -> if b then "True" else "False"
  | Unit -> "Unit"

let is_int = function
  | Int _ -> true
  | _ -> false

let is_bool = function
  | Bool _ -> true
  | _ -> false

(* Helper function to convert a list of characters back into a string *)
let rec string_of_char_list char_list =
  let rec aux chars acc =
    match chars with
    | [] -> acc
    | c :: cs -> aux cs (acc ^ str c)
  in aux char_list ""

(* Helper function to drop the first character of a string if it is a negative sign *)
let drop_negative_sign s =
  let char_list = string_listize s in
  match char_list with
  | '-' :: rest -> (true, string_of_char_list rest) (* Returns a tuple with a flag and the rest of the string *)
  | _ -> (false, s) 

let parse_constant s = 
  match s with
  | "True" -> Some (Bool true)
  | "False" -> Some (Bool false)
  | "Unit" -> Some Unit
  | _ ->
      let (is_negative, digits) = drop_negative_sign s in
      let rec parse_digits chars accum =
        match chars with
        | [] -> Some accum
        | c :: cs -> 
            if char_isdigit c then
              let digit = digit_of_char c in
              parse_digits cs (10 * accum + digit)
            else
              None
      in
      match parse_digits (string_listize digits) 0 with
      | Some n -> Some (Int (if is_negative then -n else n))
      | None -> None
   


(* Parses a string into a command - Placeholder, extend to handle all cases *)
let parse_command s =
  match s with
  | "Pop" -> Some Pop
  | "Trace" -> Some Trace
  | "Add" -> Some Add
  | "Sub" -> Some Sub
  | "Mul" -> Some Mul
  | "Div" -> Some Div
  | "And" -> Some And
  | "Or" -> Some Or
  | "Not" -> Some Not
  | "Lt" -> Some Lt
  | "Gt" -> Some Gt
  | "" -> None
  | _ -> None

let split_string (s : string) : string list =
  let rec aux (cs : char list) (current : string) (acc : string list) : string list =
    match cs with
    | [] -> 
        if current = "" then acc else current :: acc (* Handle the last word *)
    | ';' :: rest -> 
        aux rest "" (current :: acc) (* Split on semicolon *)
    | c :: rest ->
        if char_iswhitespace c then
          if current = "" then
            aux rest current acc (* Skip multiple spaces *)
          else
            aux rest "" (current :: acc) (* End of a word *)
        else
          aux rest (current ^ str c) acc (* Accumulate characters *)
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

(* Evaluates a single command *)
let eval_command cmd (stack, trace) =
  match cmd with
  | Push c -> Some (c :: stack, trace)
  | Pop -> 
      (match stack with
      | [] -> Some ([], "Panic" :: trace)  
      | _ :: rest -> Some (rest, trace))
  | Trace -> 
      (match stack with
      | [] -> Some ([Unit], "Panic" :: trace) 
      | c :: rest -> Some (Unit :: rest, (constant_to_string c) :: trace))
  | Add -> 
      (match stack with
      | Int i :: Int j :: rest -> Some ((Int (i + j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace))
  | Sub -> 
      (match stack with
      | Int j :: Int i :: rest -> Some ((Int (i - j)) :: rest, trace) 
      | _ -> Some ([], "Panic" :: trace)) (* SubError: Stack Underflow or Type Mismatch *)
  | Mul -> 
      (match stack with
      | Int i :: Int j :: rest -> Some ((Int (i * j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* MulError: Stack Underflow or Type Mismatch *)
  | Div -> 
      (match stack with
      | Int j :: Int i :: rest -> 
          if j = 0 then 
              Some ([], "Panic" :: trace)  (* DivisionError: Division by Zero *)
          else 
              Some ((Int (i / j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* DivError: Stack Underflow or Type Mismatch *)
  | And -> 
      (match stack with
      | Bool i :: Bool j :: rest -> Some ((Bool (i && j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* AndError: Stack Underflow or Type Mismatch *)
  | Or -> 
      (match stack with
      | Bool i :: Bool j :: rest -> Some ((Bool (i || j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* OrError: Stack Underflow or Type Mismatch *)
  | Not -> 
      (match stack with
      | Bool i :: rest -> Some ((Bool (not i)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* NotError: Stack Underflow or Type Mismatch *)
  | Lt -> 
      (match stack with
      | Int j :: Int i :: rest -> Some ((Bool (i < j)) :: rest, trace)  
      | _ -> Some ([], "Panic" :: trace)) (* LtError: Stack Underflow or Type Mismatch *)
  | Gt -> 
      (match stack with
      | Int j :: Int i :: rest -> Some ((Bool (i > j)) :: rest, trace)
      | _ -> Some ([], "Panic" :: trace)) (* GtError: Stack Underflow or Type Mismatch *)

let rec contains_panic trace = 
  match trace with
  | [] -> false
  | x :: xs -> if x = "Panic" then true else contains_panic xs

let rec eval_commands cmds state =
  match cmds with
  | [] -> Some state
  | cmd :: rest ->
      match eval_command cmd state with
      | Some (new_stack, new_trace) -> 
          if contains_panic new_trace then
            Some (new_stack, new_trace)  (* Stop execution if there is a "Panic" *)
          else
            eval_commands rest (new_stack, new_trace)
      | None -> None

let interp (s : string) : string list option =
  match parse_program s with
  | None -> None  (* Parsing error, return None *)
  | Some cmds -> 
      let initial_state = ([], []) in
      match eval_commands cmds initial_state with
      | Some (_, trace) -> Some (list_reverse trace) 
      | None -> Some ["Panic"]