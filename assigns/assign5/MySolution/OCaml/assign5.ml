#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>

*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accpeted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)

let is_digit c = c >= '0' && c <= '9'

let rec parse_num cs =
  match cs with
  | [] -> None
  | ' ' :: rest -> parse_num (trim rest)
  | '(' :: _ -> None
  | ')' :: _ -> None
  | c :: rest when is_digit c ->
    let (digits, rest') = parse_digits (String.make 1 c) rest in
    Some (Int (int_of_string digits), rest')
  | _ -> None

and parse_digits acc cs =
  match cs with
  | [] -> (acc, [])
  | c :: rest when is_digit c -> parse_digits (acc ^ (String.make 1 c)) rest
  | _ -> (acc, cs)

and parse_exprs cs =
  match parse_expr cs with
  | Some (expr, rest) ->
    let (exprs, rest') = parse_exprs rest in
    (expr :: exprs, rest')
  | None -> ([], cs)

and parse_expr cs =
  match cs with
  | [] -> None
  | ' ' :: rest -> parse_expr (trim rest)
  | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: rest ->
    let (exprs, rest') = parse_exprs rest in
    begin
      match rest' with
      | ')' :: rest'' -> Some (Add exprs, rest'')
      | _ -> None
    end
  | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: rest ->
    let (exprs, rest') = parse_exprs rest in
    begin
      match rest' with
      | ')' :: rest'' -> Some (Mul exprs, rest'')
      | _ -> None
    end
  | _ -> parse_num cs

let parse (s : string) : expr option =
  let cs = string_listize s in
  match parse_expr (trim cs) with
  | Some (expr, []) -> Some expr
  | _ -> None