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


and parse_expr (cs : char list) : expr option * char list =
  match trim cs with
  | ('0'..'9' as d) :: rest ->
    let num, rest' = parse_num (d::rest) in
    (Some (Int num), rest')
  | '(' :: 'a' :: 'd' :: 'd' :: rest ->
    let (exprs, rest') = parse_exprs rest in
    (match exprs with
      | [] -> (None, rest')
      | _ -> (Some (Add exprs), rest'))
  | '(' :: 'm' :: 'u' :: 'l' :: rest ->
    let (exprs, rest') = parse_exprs rest in
    (match exprs with
      | [] -> (None, rest')
      | _ -> (Some (Mul exprs), rest'))
  | _ -> (None, cs)

and parse_exprs (cs : char list) : expr list * char list =
  match trim cs with
  | ')' :: rest -> ([], rest)
  | _ ->
    let (expr, rest) = parse_expr cs in
    (match expr with
      | None -> ([], rest)
      | Some e ->
        let (exprs, rest') = parse_exprs rest in
        (e :: exprs, rest'))

let parse (s : string) : expr option =
  let char_list = string_listize s in
  let (expr, rest) = parse_expr char_list in
  if List.length (trim rest) = 0 then
    expr
  else
    None
;;


