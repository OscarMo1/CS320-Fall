#use "./../../../classlib/OCaml/MyOCaml.ml";;

type value =
  | Int of int
  | Bool of bool
  | Unit

type command =
  | Push of value
  | Pop
  | Trace
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Not
  | Lt
  | Gt

type stack = value list

type trace = string list

type config = { stack: stack; trace: trace; program: command list }

let rec is_blank cs =
  match cs with
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let rec trim cs =
  match string_get_at cs 0 with
  | c when is_blank c -> trim (string_tail cs)
  | _ -> cs

let rec list_map f xs =
  match xs with
  | [] -> []
  | x1 :: xs -> f x1 :: list_map f xs

let split_on_char sep str =
  let rec split_acc acc current = function
    | -1 -> current :: acc
    | i ->
      if string_get_at str i = sep then
        split_acc (current :: acc) "" (i - 1)
      else
        split_acc acc (string_cons (string_get_at str i) current) (i - 1)
  in
  split_acc [] "" (string_length str - 2)

let split_on_char_2 sep str =
  let rec split_acc acc current = function
    | -1 -> current :: acc
    | i ->
      if string_get_at str i = sep then
        split_acc (current :: acc) "" (i - 1)
      else
        split_acc acc (string_cons (string_get_at str i) current) (i - 1)
  in
  split_acc [] "" (string_length str - 1)

let string_of_bool b =
  if b then "True" else "False"

let rec string_of_int_custom n =
  let digit_to_char d = char_of_digit (digit_of_char '0' + d) in
  let rec aux acc n =
    if n = 0 then
      match acc with
      | [] -> "0"
      | _ -> string_concat_list (list_reverse acc)
    else
      let digit = n mod 10 in
      aux (str(digit_to_char digit) :: acc) (n / 10)
  in
  aux [] n
;;

let rec to_string : value -> string = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Unit -> "Unit"

let rec eval_command : config -> config option = function
  | { stack; trace; program = Push v :: rest } ->
      let new_stack = v :: stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack = v :: rest_stack; trace; program = Pop :: rest } ->
      eval_command { stack = rest_stack; trace; program = rest }

  | { stack = v :: rest_stack; trace; program = Trace :: rest } ->
      let new_trace = to_string v :: trace in
      eval_command { stack = rest_stack; trace = new_trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Add :: rest } ->
      let new_stack = Int (i1 + i2) :: rest_stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Sub :: rest } ->
      let new_stack = Int (i1 - i2) :: rest_stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Mul :: rest } ->
      let new_stack = Int (i1 * i2) :: rest_stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Div :: rest } ->
      if i2 = 0 then 
        let new_trace = "Panic" :: trace in
        Some { stack = []; trace = new_trace; program = [] }
      else
        let new_stack = Int (i1 / i2) :: rest_stack in
        eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Bool i1) :: (Bool i2) :: rest_stack; trace; program = And :: rest } ->
      let new_stack = Bool (i1 && i2) :: rest_stack in
        eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Bool i1) :: (Bool i2) :: rest_stack; trace; program = Or :: rest } ->
      let new_stack = Bool (i1 || i2) :: rest_stack in
        eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Bool v) :: rest_stack; trace; program = Not :: rest } ->
      if v then 
        eval_command { stack = Bool (false) :: rest_stack; trace; program = rest }
      else
        eval_command { stack = Bool (true) :: rest_stack; trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Lt :: rest } ->
      let new_stack = Bool (i1 < i2) :: rest_stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack = (Int i1) :: (Int i2) :: rest_stack; trace; program = Gt :: rest } ->
      let new_stack = Bool (i1 > i2) :: rest_stack in
      eval_command { stack = new_stack; trace; program = rest }

  | { stack; trace; program = [] } -> Some { stack; trace; program = [] }

  | { stack; trace; program = _ } ->
    (* Error: Unsupported operation or empty stack *)
    let new_trace = "Panic" :: trace in
    Some { stack = []; trace = new_trace; program = [] }

let parse_value (s : string) : value option =
  try Some (Int (int_of_string s))
  with Failure _ -> (
    match s with
    | "True" -> Some (Bool true)
    | "False" -> Some (Bool false)
    | "Unit" -> Some Unit
    | _ -> None
  )

let parse_command (s : string) : command option =
  match split_on_char_2 ' ' s with
  | ["Push"; value_str] -> (
      match parse_value value_str with
      | Some value -> Some (Push value)
      | None -> None
    )
  | "Pop" :: [] -> Some Pop
  | "Trace" :: [] -> Some Trace
  | "Add" :: [] -> Some Add
  | "Sub" :: [] -> Some Sub
  | "Mul" :: [] -> Some Mul
  | "Div" :: [] -> Some Div
  | "And" :: [] -> Some And
  | "Or" :: [] -> Some Or
  | "Not" :: [] -> Some Not
  | "Lt" :: [] -> Some Lt
  | "Gt" :: [] -> Some Gt
  | _ -> None

let rec parse_program : string list -> command list option = function
  | hd :: tl ->
      (match parse_command hd with
      | Some cmd ->
          (match parse_program tl with
          | Some rest -> Some (cmd :: rest)
          | None -> None)
      | None -> None)
  | [] -> Some []

let interp (program : string) : string list option =
  let program_tokens = list_map trim (split_on_char ';' program) in
  match parse_program program_tokens with
  | Some program ->
      let initial_config = { stack = []; trace = []; program } in
      (match eval_command initial_config with
      | Some final_config -> Some (list_reverse final_config.trace)
      | None -> None)
  | None -> None
