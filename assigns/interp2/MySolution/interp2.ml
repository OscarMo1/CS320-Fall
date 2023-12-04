#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* defining types *)
type constant = 
   |Int of int 
   |Bool of bool 
   |Unit 
;;

(* type stackfun = {name: string; varenv: string; coms: string};; *)

(* type stackfun = string * (string list * constant list) * com list *)

type symbol = string;;

type value = 
   |Constant of constant 
   |Symbol of symbol
   |Closure of closure

and 
   closure = symbol * (string list * value list) * com list
   (* {
      name: symbol;
      varenv: string list * value list;
      coms: com list
   } *)
and
   com = 
      |Push of value
      |Pop
      |Trace
      |Add 
      |Sub 
      |Mul
      |Div
      |And 
      |Or 
      |Not 
      |Lt 
      |Gt
      |Swap
      |IfElse of com list list
      |Bind 
      |Lookup
      |Fun of com list
      |Call
      |Return 
;;

(* returns string representation of an integer *)
let rec intToString(x: int): string =
   if x < 0 then string_append "-" (intToString (x * -1))
   else if x < 10 then str (char_of_digit x)
   else string_append (intToString (x / 10)) (intToString (x mod 10))
;;

(* returns string presentations of a constant *)
let constantToString(x: constant): string = 
   match x with
   |Unit -> "Unit"
   |Bool b -> if b then "True" else "False"
   |Int i -> intToString i 
;;

let toString(x: value): string = 
   match x with 
   |Constant x -> constantToString x 
   |Symbol x -> x
   |Closure c -> 
      match c with 
      |(name, varenv, coms) -> string_append (string_append "Fun<" name) ">"
;;

(* defining boolean operators *)
let andd(a: bool)(b: bool): bool = 
   if a then 
      if b then true 
      else false 
   else false
;;

let orr(a: bool)(b: bool): bool = 
   if a then 
      true 
   else if b then 
      true 
   else false
;;

let nott(a: bool): bool = 
   if a then false else true 
;;

(* parse constants *)
let rec parse_constant () : value parser = 
   parse_pos () <|> parse_neg () <|> parse_true () <|> parse_false () <|> parse_unit ()

   and parse_pos () : value parser =
      let* n = natural in
      pure (Constant(Int n)) << whitespaces

   and parse_neg () : value parser = 
      let* _ = keyword "-" in 
      let* n = natural in 
      pure (Constant(Int (-1 * n))) << whitespaces

   and parse_true () : value parser =
      let* _ = keyword "True" in
      pure (Constant(Bool true)) << whitespaces
   
   and parse_false () : value parser = 
      let* _ = keyword "False" in 
      pure (Constant(Bool false)) << whitespaces

   and parse_unit () : value parser = 
      let* _ = keyword "Unit" in 
      pure (Constant(Unit)) << whitespaces
;;

let (&&) = andd;;

let (||) = orr;;

let (!!) = nott;;

let alphanum = 
   (satisfy char_isdigit) <|> (satisfy char_islower) <|> (satisfy char_isupper)

let str : string parser =
  fun ls ->
  let@ (xs, ls) = many1 alphanum ls in
  Some(list_foldleft(xs)("") (fun acc c -> string_snoc acc c), ls)
;;

let rec parse_symbol () : value parser = 
   parse_x () 

   and parse_x () : value parser =
      let* s = str in 
      pure (Symbol s) << whitespaces
;;

let parse_value () : value parser = 
   parse_constant () <|> parse_symbol ()
;;


(* parse coms *)
let rec parse_com () : com parser =
   parse_push () <|> parse_pop () <|> parse_trace () <|> 
   parse_add () <|> parse_sub () <|> parse_mul () <|> 
   parse_div () <|> parse_and () <|> parse_or () <|> 
   parse_not () <|> parse_lt () <|> parse_gt() <|>
   parse_swap () <|> parse_ifelse () <|> parse_bind () <|>
   parse_lookup () <|> parse_fun () <|> parse_call () <|>
   parse_return ()

   and parse_push () : com parser =
      let* _ = keyword "Push" in
      let* c = parse_value () in
      let* _ = keyword ";" in
      pure (Push c)

   and parse_pop () : com parser =
      let* _ = keyword "Pop" in
      let* _ = keyword ";" in
      pure (Pop)

   and parse_trace () : com parser =
      let* _ = keyword "Trace" in
      let* _ = keyword ";" in
      pure (Trace)

   and parse_add () : com parser =
      let* _ = keyword "Add" in
      let* _ = keyword ";" in
      pure (Add)

   and parse_sub () : com parser =
      let* _ = keyword "Sub" in
      let* _ = keyword ";" in
      pure (Sub)

   and parse_mul () : com parser =
      let* _ = keyword "Mul" in
      let* _ = keyword ";" in
      pure (Mul)

   and parse_div () : com parser =
      let* _ = keyword "Div" in
      let* _ = keyword ";" in
      pure (Div)

   and parse_and () : com parser =
      let* _ = keyword "And" in
      let* _ = keyword ";" in
      pure (And)

   and parse_or () : com parser =
      let* _ = keyword "Or" in
      let* _ = keyword ";" in
      pure (Or)

   and parse_not () : com parser =
      let* _ = keyword "Not" in
      let* _ = keyword ";" in
      pure (Not)

   and parse_lt () : com parser =
      let* _ = keyword "Lt" in
      let* _ = keyword ";" in
      pure (Lt)

   and parse_gt () : com parser =
      let* _ = keyword "Gt" in
      let* _ = keyword ";" in 
      pure (Gt)

   and parse_swap () : com parser = 
      let* _ = keyword "Swap" in 
      let* _ = keyword ";" in
      pure (Swap)

   and parse_ifelse () : com parser = 
      let* _ = keyword "If" in 
      let* c1 = many1' parse_com in
      let* _ = keyword "Else" in 
      let* c2 = many1' parse_com in
      let* _ = keyword "End" in 
      let* _ = keyword ";" in
      pure (IfElse [c1; c2])

   and parse_bind () : com parser = 
      let* _ = keyword "Bind" in 
      let* _ = keyword ";" in
      pure (Bind)

   and parse_lookup () : com parser = 
      let* _ = keyword "Lookup" in 
      let* _ = keyword ";" in
      pure (Lookup)

   and parse_fun () : com parser = 
      let* _ = keyword "Fun" in 
      let* cs = many1' parse_com in
      let* _ = keyword "End" in 
      let* _ = keyword ";" in
      pure (Fun cs)

   and parse_call () : com parser = 
      let* _ = keyword "Call" in 
      let* _ = keyword ";" in 
      pure (Call)

   and parse_return () : com parser = 
      let* _ = keyword "Return" in 
      let* _ = keyword ";" in 
      pure (Return)

(* remove blank chars at the front of a list *)
let rec trim_list(cs: char list): char list =
   match cs with
   | [] -> cs
   | '\n' :: cs -> trim_list cs
   | '\t' :: cs -> trim_list cs
   | '\r' :: cs -> trim_list cs
   | ' ' :: cs -> trim_list cs
   | _ -> cs
;;

let trim_string(cs: string): string = 
   list_foldleft (trim_list (string_listize cs)) "" (fun acc c -> string_snoc acc c)
;;


let rec parse_input(s: string): com list option = 
   (* remove leading whitespace *)
   let s = trim_string s in 
   if s = "" then Some([]) else
   match string_parse(parse_com ()) s with 
   |None -> None
   |Some(e, []) -> Some([e])
   |Some(e, rest) -> (* recurse to the next command *)
      let res = parse_input(list_foldleft(rest)("")(fun acc c -> string_snoc acc c)) in 
      match res with 
      |Some(r) -> Some(e::r)
      |None -> None
;;

let (++) = list_append;;

let rec valueOf(x:string)(varenv: string list * value list): value option = 
   match varenv with 
   |(vars, vals) -> 
      (match vars with 
      |var::vars -> 
         (match vals with 
         |v::vals ->          
         if var = x then Some(v) else (valueOf x (vars, vals))
         |_ -> None)
      |_ -> None)
;;

let rec compute(coms: com list)(stack: value list)(trace: string list)(varenv: string list * value list): string list = 
   match coms with 
   |[] -> trace (* base case *)
   |com::coms -> (* recursive case; more coms to process *)
      match com with 
      |Push c -> compute coms (c::stack) trace varenv
      |Pop -> 
         (match stack with 
         |c::stack -> compute coms stack trace varenv
         |_ -> "Panic"::trace)
      |Trace ->
         (match stack with
         |c::stack -> compute coms (Constant(Unit)::stack) (toString(c)::trace) varenv
         |_ -> "Panic"::trace)
      |Add ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> compute coms (Constant (Int(i+j))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Sub ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> compute coms (Constant(Int(i-j))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Mul ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> compute coms (Constant(Int(i*j))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Div ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> if j = 0 then ("Panic"::trace) else (compute coms (Constant(Int(i/j))::stack) trace varenv) 
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |And ->
         (match stack with 
         |a::b::stack -> 
            (match a with 
            |Constant Bool a -> 
               (match b with 
               |Constant Bool b -> compute coms (Constant(Bool(a && b))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Or ->
         (match stack with 
         |a::b::stack -> 
            (match a with 
            |Constant Bool a -> 
               (match b with 
               |Constant Bool b -> compute coms (Constant(Bool(a || b))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Not ->
         (match stack with 
         |a::stack -> 
            (match a with 
            |Constant Bool a -> compute coms (Constant(Bool(!! a))::stack) trace varenv
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Lt ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> compute coms (Constant(Bool(i<j))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Gt ->
         (match stack with 
         |i::j::stack -> 
            (match i with 
            |Constant Int i -> 
               (match j with 
               |Constant Int j -> compute coms (Constant(Bool(i>j))::stack) trace varenv
               |_ -> "Panic"::trace) 
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Swap -> 
         (match stack with 
         |c1::c2::stack -> compute coms (c2::c1::stack) trace varenv
         |_ -> "Panic"::trace)
      |IfElse cs -> 
         (match stack with 
         |b::stack -> 
            (match b with 
            |Constant Bool b -> 
               (match cs with 
               |c1::c2::[] -> if b then compute (c1++coms) stack trace varenv else compute (c2++coms) stack trace varenv
               |_ -> "Panic"::trace)
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Bind -> 
         (match stack with 
         |x::v::stack -> 
            (match x with 
            |Symbol x -> 
               (match varenv with 
               |(vars, vals) -> compute coms stack trace ((x::vars), (v::vals)))
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Lookup -> 
         (match stack with 
         |x::stack -> 
            (match x with 
            |Symbol x -> 
               (match valueOf x varenv with 
               |Some v -> compute coms (v::stack) trace varenv
               |_ -> "Panic"::trace)
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Fun cs -> 
         (match stack with 
         |x::stack -> 
            (match x with 
            |Symbol x -> 
               compute coms (Closure (x, varenv, cs)::stack) trace varenv
            |_ -> "Panic"::trace)
         |_ -> "Panic"::trace)
      |Call ->
         (match stack with
         |Closure (cname, cvarenv, ccoms) :: a :: stack -> 
            (match varenv with 
            |(vars, vals) -> compute ccoms (a::Closure ("cc", varenv, coms)::stack) trace (cname::vars, Closure (cname, cvarenv, ccoms)::vals))
         |_ -> "Panic"::trace)
      |Return -> 
         (match stack with 
         |Closure (ccname, cvarenv, ccoms) :: a :: stack -> compute ccoms (a::stack) trace cvarenv
         |_ -> "Panic"::trace)
;;

let interp (s : string) : string list option  = (* YOUR CODE *)
   (*
   parse input to create a list of commands; return None if parsing fails
   otherwise, perform the commands   
   *)
   match parse_input(s) with 
   |None -> None 
   |Some(coms) -> 
      Some(compute coms [] [] ([], []))
;;

(* ------------------------------------------------------------ *)

(* interp from file *)
(* copied from interp1 solution *)

let read_file (fname : string) : string =
   let fp = open_in fname in
   let s = string_make_fwork (fun work ->
       try
         while true do
           work (input_char fp)
         done
       with _ -> ())
   in
   close_in fp; s
 
 let interp_file (fname : string) : string list option =
   let src = read_file fname in
   interp src
 
