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

let is_digit char = char >= '0' && char <= '9'


let rec parse_exprs str =
  match parse_expr str with
  | Some (expr, t) ->
    let (exprs, t') = parse_exprs t in
    (expr :: exprs, t')
  | None -> ([], str)

and parse_expr str =
  match str with
  | [] -> None
  | ' ' :: t -> parse_expr (trim t)
  | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: t ->
    let (exprs, t') = parse_exprs t in
    begin
      match t' with
      | ')' :: t'' -> Some (Add exprs, t'')
      | _ -> None
    end
  | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: t ->
    let (exprs, t') = parse_exprs t in
    begin
      match t' with
      | ')' :: t'' -> Some (Mul exprs, t'')
      | _ -> None
    end
  | _ -> parse_num str
and parse_num str =
  match str with
  | [] -> None
  | ' ' :: t -> parse_num (trim t)
  | '(' :: _ -> None
  | ')' :: _ -> None
  | char :: t when is_digit char ->
    let (digits, t') = parse_digits (String.make 1 char) t in
    Some (Int (int_of_string digits), t')
  | _ -> None

and parse_digits acc str =
  match str with
  | [] -> (acc, [])
  | char :: t when is_digit char -> parse_digits (acc ^ (String.make 1 char)) t
  | _ -> (acc, str)



let parse (s : string) : expr option =
  let str = string_listize s in
  match parse_expr (trim str) with
  | Some (expr, []) -> Some expr
  | _ -> None
