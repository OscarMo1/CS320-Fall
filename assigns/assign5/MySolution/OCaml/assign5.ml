#use "./../../../../classlib/OCaml/MyOCaml.ml";;


type expr =
  | Int of int
  | Add of expr list
  | Mul of expr list

let is_digit c = c >= '0' && c <= '9'

let rec parse_num acc cs =
  match cs with
  | c :: rest when is_digit c ->
    let (digits, rest') = parse_num (acc ^ (String.make 1 c)) rest in
    (digits, rest')
  | _ -> (acc, cs)

let rec parse_expr cs =
  match cs with
  | [] -> (None, cs)
  | ' ' :: rest -> parse_expr (trim rest)
  | '(' :: rest ->
    let (op, rest') = parse_op (trim rest) in
    (op, rest')
  | _ -> parse_num "" cs

and parse_op cs =
  match cs with
  | [] -> (None, cs)
  | 'a' :: 'd' :: 'd' :: ' ' :: rest -> parse_add rest
  | 'm' :: 'u' :: 'l' :: ' ' :: rest -> parse_mul rest
  | _ -> (None, cs)

and parse_add cs =
  let (exprs, rest') = parse_exprs cs in
  match rest' with
  | ')' :: rest'' -> (Some (Add exprs), rest'')
  | _ -> (None, cs)

and parse_mul cs =
  let (exprs, rest') = parse_exprs cs in
  match rest' with
  | ')' :: rest'' -> (Some (Mul exprs), rest'')
  | _ -> (None, cs)

and parse_exprs cs =
  match parse_expr cs with
  | (Some expr, rest) ->
    let (exprs, rest') = parse_exprs rest in
    (expr :: exprs, rest')
  | _ -> ([], cs)

let parse (s : string) : expr option =
  let cs = string_listize s in
  match parse_expr (trim cs) with
  | (Some expr, []) -> Some expr
  | _ -> None
