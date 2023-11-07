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

let rec parse_expr (tokens : char list) : expr option * char list =
  match trim tokens with
  | '(' :: 'a' :: 'd' :: 'd' :: rest ->
      let exprs, rest' = parse_exprs rest in
      (match exprs with
      | Some exprs' -> Some (Add exprs'), rest'
      | None -> None, rest')
  | '(' :: 'm' :: 'u' :: 'l' :: rest ->
      let exprs, rest' = parse_exprs rest in
      (match exprs with
      | Some exprs' -> Some (Mul exprs'), rest'
      | None -> None, rest')
  | _ ->
      let num, rest = parse_num tokens in
      (match num with
      | Some num' -> Some (Int num'), rest
      | None -> None, rest)

and parse_exprs (tokens : char list) : expr list option * char list =
  match trim tokens with
  | ')' :: rest -> Some [], rest
  | _ ->
      let expr, rest = parse_expr tokens in
      (match expr with
      | Some expr' ->
          let exprs, rest' = parse_exprs rest in
          (match exprs with
          | Some exprs' -> Some (expr' :: exprs'), rest'
          | None -> None, rest')
      | None -> None, rest)

and parse_num (tokens : char list) : int option * char list =
  let rec parse_digits acc tokens =
    match tokens with
    | c :: rest when Char.is_digit c ->
        let acc' = acc * 10 + Char.to_int c - Char.to_int '0' in
        parse_digits acc' rest
    | _ -> acc, tokens
  in
  let num, rest = parse_digits 0 tokens in
  if num = 0 && tokens = rest then None, rest
  else Some num, rest

let parse (s : string) : expr option =
  let tokens = string_listize s in
  let expr, rest = parse_expr tokens in
  match expr with
  | Some expr' when rest = [] -> Some expr'
  | _ -> None
