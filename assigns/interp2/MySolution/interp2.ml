#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type const =
  | Int of int
  | Bool of bool
  | Unit
  | Symbol of string

type closure = {
  body: coms;
  env: environment;
}
and environment = (string * const) list
and com =
  | Push of const
  | PushClosure of closure
  | Pop
  | Trace
  | Swap
  | Add
  | Sub
  | Mul 
  | Div
  | And 
  | Or 
  | Not
  | Lt 
  | Gt
  | IfElse of coms * coms  
  | Bind                   
  | Lookup                
  | Fun of string * coms    
  | Call                   
  | Return                
and coms = com list

let parse_nat = 
  let* n = natural << whitespaces in pure n

let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))

let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))

let parse_unit =
  keyword "Unit" >> pure Unit

let is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_underscore = function
  | '_' -> true
  | _ -> false

let parse_symbol_char =
  let is_first_char = ref true in
  satisfy (fun c ->
    let is_valid = is_letter c || is_digit c || (is_underscore c && not !is_first_char) in
    if !is_first_char then is_first_char := false;
    is_valid
  )

let parse_symbol : const parser =
  let* first_char = satisfy is_letter in  (* Symbols must start with a letter *)
  let* rest_chars = many parse_symbol_char in  (* Followed by any number of symbol characters *)

  let char_to_string c = string_make_fwork (fun work -> work c) in

  (* Convert the first character to a string *)
  let first_char_str = char_to_string first_char in

  (* Convert each character in rest_chars to a string *)
  let rest_chars_strs = list_make_fwork (fun work -> list_foreach rest_chars (fun c -> work (char_to_string c))) in

  (* Concatenate first_char_str with the list of single-character strings and wrap in Symbol constructor *)
  pure (Symbol (string_concat_list (first_char_str :: rest_chars_strs)))

let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_symbol

type trace = string list
type prog = coms

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = str (chr (d + ord '0')) in 
  if 0 < n0 then
    string_append (str_of_nat n0) s
  else s

let str_of_int (n : int) : string = 
  if n < 0 then
    string_append "-" (str_of_nat (-n))
  else str_of_nat n


let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Symbol s -> s

let debug_print str = 
  pure (Printf.printf "%s\n" str; flush stdout)


let rec parse_com () =
  let* _ = pure () in
  Printf.printf "Parsing command...\n";
  (keyword "Push" >> parse_const >>= fun c -> 
    Printf.printf "Parsed Push with const: %s\n" (toString c);
    pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (keyword "If" >> parse_coms () >>= fun c1 -> 
    keyword "Else" >> parse_coms () >>= fun c2 -> 
    keyword "End" >> pure (IfElse(c1, c2))) <|>  
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Fun" >>
   whitespaces >>  (* Ensure separation from 'Fun' keyword *)
   keyword "{" >>
   parse_coms () >>= fun body ->  (* Parse the function body *)
   keyword "}" >>
   debug_print "Parsed function body" >>
   pure (Fun ("anonymous", body))) <|>  
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return)
and parse_coms () =
  let* _ = pure () in 
  many (parse_com () << keyword ";")


type stack_item =
  | Const of const
  | Closure of closure
  | Marker of stack * environment
and stack = stack_item list

let assoc_opt key lst =
  let rec aux lst =
    match lst with
    | [] -> None
    | (k, v) :: tail -> if k = key then Some v else aux tail
  in
  aux lst

let rec eval s t e p =
  Printf.printf "Evaluating command with stack: [%s]\n" 
    (String.concat "; " (List.map (fun item ->
      match item with
      | Const c -> toString c
      | Closure _ -> "[Closure]"
      | Marker (_, _) -> "[Marker]") s));
  match p with
  | [] -> t
  | Push c :: p0 -> eval (Const c :: s) t e p0
  | PushClosure closure :: p0 -> eval (Closure closure :: s) t e p0
  | Pop :: p0 ->
    (match s with
     | _ :: s0 -> eval s0 t e p0
     | [] -> eval [] ("Panic" :: t) e p0)  (* Handle empty stack *)
  | Trace :: p0 ->
    (match s with
     | c :: s0 -> let str_c = 
                    (match c with
                     | Const const_val -> toString const_val
                     | Closure _ -> "[closure]"
                     | Marker (_, _) -> "[marker]")  (* Added Marker case *)
                  in eval (Const Unit :: s0) (str_c :: t) e p0
     | [] -> eval [] ("Panic" :: t) e p0)
  | Swap :: p0 ->
    (match s with
     | x1 :: x2 :: s0 -> eval (x2 :: x1 :: s0) t e p0
     | _ -> eval [] ("Panic" :: t) e p0)
  | Add :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 -> eval (Const (Int (i + j)) :: s0) t e p0
     | _ :: _ :: s0 (* AddError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* AddError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) e p0)
  | Sub :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* SubStack *)  -> eval (Const (Int (i - j)) :: s0) t e p0
     | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* SubError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) e p0)
  | Mul :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* MulStack *)  -> eval (Const (Int (i * j)) :: s0) t e p0
     | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* MulError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) e p0)
  | Div :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int 0) :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) e p0
     | Const (Int i) :: Const (Int j) :: s0 (* DivStack *)  -> eval ((Const (Int (i / j))) :: s0) t e p0
     | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* DivError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) e p0)
  | And :: p0 ->
    (match s with
     | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a && b)) :: s0) t e p0
     | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) e p0
     | []                     (* AndError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) e p0)
  | Or :: p0 ->
    (match s with
     | Const (Bool a) :: Const (Bool b) :: s0 -> eval (Const (Bool (a || b)) :: s0) t e p0
     | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) e p0
     | []                     (* OrError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) e p0)
  | Not :: p0 ->
    (match s with
     | Const (Bool a) :: s0 (* NotStack  *) -> eval (Const (Bool (not a)) :: s0) t e p0
     | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* NotError2 *) -> eval [] ("Panic" :: t) e p0)
  | Lt :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* GtStack *)  -> eval ((Const (Bool (i < j))) :: s0) t e p0
     | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* LtError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) e p0)
  | Gt :: p0 ->
    (match s with
     | Const (Int i) :: Const (Int j) :: s0 (* GtStack *)  -> eval ((Const (Bool (i > j))) :: s0) t e p0
     | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) e p0
     | []                   (* GtError2 *) -> eval [] ("Panic" :: t) e p0
     | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) e p0)
  | IfElse(c1, c2) :: p0 ->
    (match s with
     | Const (Bool b) :: s0 -> eval s0 t e (if b then list_append c1 p0 else list_append c2 p0)
     | _ :: s0      (* IfElseError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* IfElseError2 *) -> eval [] ("Panic" :: t) e p0)
  | Bind :: p0 ->
    (match s with
     | Const (Symbol sym) :: Const v :: s0 -> eval s0 t ((sym, v) :: e) p0
     | _ :: s0      (* BindError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* BindError2 *) -> eval [] ("Panic" :: t) e p0)
  | Lookup :: p0 ->
    (match s with
     | Const (Symbol sym) :: s0 -> 
       (match assoc_opt sym e with
        | Some v -> eval (Const v :: s0) t e p0
        | None      (* LookupError3 *) -> eval [] ("Panic" :: t) e p0)
     | _ :: s0      (* LookupError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* LookupError2 *) -> eval [] ("Panic" :: t) e p0)
  | Fun (name, body) :: p0 ->
     let closure = { body; env = e } in
     eval (Closure closure :: s) t e p0
  | Call :: p0 ->
    (match s with
     | Closure { body; env } :: s0 -> eval (Marker (s0, e) :: s0) t env body
     | _ :: s0      (* CallError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* CallError2 *) -> eval [] ("Panic" :: t) e p0)
  | Return :: p0 ->
    (match s with
     | Marker (s0, env0) :: _ -> eval s0 t env0 p0
     | _ :: s0      (* ReturnError1 *) -> eval [] ("Panic" :: t) e p0
     | []           (* ReturnError2 *) -> eval [] ("Panic" :: t) e p0)

let interp (s : string) : string list option =
  Printf.printf "Interpreting...\n";
  match string_parse (whitespaces >> parse_coms ()) s with
  | Some (p, []) ->
      let initial_stack = [] in  (* Initialize an empty stack *)
      let initial_trace = [] in  (* Initialize an empty trace *)
      let initial_environment = [] in  (* Initialize an empty environment *)
      Some (eval initial_stack initial_trace initial_environment p)  (* Start evaluation with the parsed program *)
  | _ -> None