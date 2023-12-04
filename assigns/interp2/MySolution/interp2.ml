#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(*
⟨digit⟩ ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
⟨nat⟩ ::= ⟨digit⟩ | ⟨digit⟩⟨nat⟩
⟨int⟩ ::= ⟨nat⟩ | -⟨nat⟩
⟨bool ⟩ ::= True | False
⟨char ⟩ ::= a | b | ... | z
⟨sym⟩ ::= ⟨char ⟩ | ⟨sym⟩⟨char ⟩ | ⟨sym⟩⟨digit⟩
⟨const⟩ ::= ⟨int⟩ | ⟨bool ⟩ | Unit | ⟨sym⟩

⟨prog⟩ ::= ⟨coms⟩
⟨com⟩ ::= Push ⟨const⟩ | Pop | Swap | Trace
| Add | Sub | Mul | Div
| And | Or | Not
| Lt | Gt
| If ⟨coms⟩ Else ⟨coms⟩ End
| Bind | Lookup
| Fun ⟨coms⟩ End | Call | Return
⟨coms⟩ ::= ϵ | ⟨com⟩; ⟨coms⟩

*)

type const =
  | Int of int 
  | Bool of bool 
  | Unit
  | Symbol of string
  | Closure of (string * ((string * const) list) * coms)


and com =
  | Push of const 
  | Pop 
  | Swap 
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
  | If of coms * coms
  | Bind 
  | Lookup
  | Fun of coms 
  | Call 
  | Return

and coms = com list
type stack = const list
type trace = string list
type prog = coms
type env = (string * const) list


let parse_nat = let* n = natural << whitespaces in pure n
 
let parse_int =
  (let* n = parse_nat in pure (Int n)) <|>
  (keyword "-" >> let* n = parse_nat in pure (Int (-n)))
 
let parse_bool =
  (keyword "True" >> pure (Bool true)) <|>
  (keyword "False" >> pure (Bool false))
 
let parse_unit =
  keyword "Unit" >> pure Unit

let char_escaped c =
    match c with
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | '\\' -> "\\\\"
    | '\'' -> "\\'"
    | _    -> str c
let character_grabber =
  let* chars = many (satisfy char_isletter) in
  pure (list_make_fwork (fun work -> list_foreach chars work))
let parse_string : string parser =
  fun ls ->
    match character_grabber ls with
    | Some (charList, rest) ->
      Some (list_foldleft charList "" (fun acc c -> acc ^ char_escaped c), rest)
    | None -> None
let parse_symbol =
  let* n = parse_string << whitespaces in pure (Symbol n)
 
let parse_const =
  parse_int <|>
  parse_bool <|>
  parse_unit <|>
  parse_symbol
 
let rec parse_com ()= 
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Trace" >> pure Trace) <|>
  (keyword "Add" >> pure Add) <|>
  (keyword "Sub" >> pure Sub) <|>
  (keyword "Mul" >> pure Mul) <|>
  (keyword "Div" >> pure Div) <|>
  (keyword "And" >> pure And) <|>
  (keyword "Or" >> pure Or) <|>
  (keyword "Not" >> pure Not) <|>
  (keyword "Lt" >> pure Lt) <|>
  (keyword "Gt" >> pure Gt) <|>
  (keyword "Swap" >> pure Swap) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (keyword "Return" >> pure Return) <|>
  (keyword "Call" >> pure Call) <|>
  (parse_if())  <|>
  (parse_fun())
and parse_if () =
  let* _ = keyword "If" in
  let* c1 = parse_coms () in
  let* _ = keyword "Else" in
  let* c2 = parse_coms () in
  let* _ = keyword "End" in
  pure(If (c1, c2))
and parse_fun () =
  let* _ = keyword "Fun" in
  let* c1 = parse_coms() in
  let* _ = keyword "End" in
  pure(Fun(c1))
and parse_coms() = many' (fun x -> parse_com x << keyword ";")
 

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
   | Closure (s, v, p) -> 
      let s1 = string_append ("Fun<") (s) in
      string_append (s1) (">")

let rec lookup_helper(s : string)(v : env) : const option =
  match v with
  | (str, value) :: rest when (str = s) -> Some value
  | (str, value) :: rest when (str != s) -> lookup_helper s rest
  | _ -> None 
let rec eval (s : stack) (t : trace) (v: env) (p : prog) : trace =
  match p with
  | [] -> t
  | Push c :: p0 -> eval (c :: s) t v p0
  | Pop :: p0 ->
    (match s with
    | _ :: s0 -> eval s0 t v p0
    | [] -> eval [] ("Panic" :: t) v [])
  | Swap :: p0 ->
    (match s with
    | c1 :: c2 :: s0 -> eval (c2 :: c1 :: s0) t v p0
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Trace :: p0 ->
    (match s with
    | c :: s0 -> eval (Unit :: s0) (toString c :: t) v p0
    | [] -> eval [] ("Panic" :: t) v [])
  | Add :: p0 ->
    (match s with
    | Int i :: Int j :: s0 -> eval (Int (i + j) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Sub :: p0 ->
    (match s with
    | Int i :: Int j :: s0 -> eval (Int (i - j) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: []  -> eval [] ("Panic" :: t) v [])
  | Mul :: p0 ->
    (match s with
    | Int i :: Int j :: s0 -> eval (Int (i * j) :: s0) t v p0 
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Div :: p0 ->
    (match s with
    | Int i :: Int 0 :: s0 -> eval [] ("Panic" :: t) v []
    | Int i :: Int j :: s0 -> eval (Int (i / j) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | And :: p0 ->
    (match s with
    | Bool a :: Bool b :: s0 -> eval (Bool (a && b) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Or :: p0 ->
    (match s with
    | Bool a :: Bool b :: s0 -> eval (Bool (a || b) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Not :: p0 ->
    (match s with
    | Bool a :: s0 -> eval (Bool (not a) :: s0) t v p0
    | _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])
  | Lt :: p0 ->
    (match s with
    | Int i :: Int j :: s0 -> eval (Bool (i < j) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Gt :: p0 ->
    (match s with
    | Int i :: Int j :: s0 -> eval (Bool (i > j) :: s0) t v p0
    | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v []
    | _ :: [] -> eval [] ("Panic" :: t) v [])
  | If(c1, c2) :: p0 ->
    (match s with
    | Bool b :: rest  (*If Else*)->
      if (b = true) then
        eval rest t v (list_append c1 p0)
      else 
        eval rest t v (list_append c2 p0)
    | _ :: rest -> eval [] ("Panic" :: t) v []
    | [] -> eval [] ("Panic" :: t) v [])
    | Bind :: p0 ->
      (match s with
      | Symbol x :: constant :: s0 -> eval s0 t ((x,constant) :: v) p0
      | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
      | [] -> eval [] ("Panic" :: t) v []
      | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Lookup :: p0 ->
    (match s with
      | Symbol x :: s0 -> 
        (match lookup_helper x v with
        | Some constant -> eval (constant :: s0) t v p0
        | _ -> eval [] ("Panic" :: t) v []
        )
      | _ :: s0 -> eval [] ("Panic" :: t) v []
      | [] -> eval [] ("Panic" :: t) v [])
      | Fun c :: p0 ->
         (match s with
         | Symbol x :: s0 -> eval (Closure(x, v, c) :: s0) t v p0
         | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
         | [] -> eval [] ("Panic" :: t) v []
         | _ :: [] -> eval [] ("Panic" :: t) v [])
| Call :: p0 ->
   (match s with
   | Closure (f, vf, c) :: a :: s0 ->
      let new_env = ((f, Closure (f, vf, c)) :: vf) in
      let cc_closure = Closure ("cc", v, p0) in
      eval (a :: cc_closure :: s0) (t) (new_env) (c)
   | _ :: _ :: s0 -> eval [] ("Panic" :: t) v []
   | [] -> eval [] ("Panic" :: t) v []
   | _ :: [] -> eval [] ("Panic" :: t) v [])
  | Return :: p0 -> 
    (match s with
    | Closure (f, vf, c) :: a :: s0 -> eval (a :: s0) (t) (vf) (c)
    | _ -> eval ([]) ("Panic" :: t) (v) ([]))
    

let interp (s : string) : string list option =
  match string_parse (whitespaces >> parse_coms()) s with
  | Some (p, []) -> Some (eval [] [] [] p)
  | _ -> None

(* My Tester *)
let test () =
   let test_program = 
   "" (* Placeholder for test input string *)
   in match interp test_program with
      | Some trace -> List.iter print_endline trace
      | None -> print_endline "Program failed to interpret"
 
 let _ = test()