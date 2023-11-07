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

let rec parse_num (cs : char list) : (int * char list) option =
  match cs with
  | [] -> None
  | '0' .. '9' as c :: rest ->
      let digit = digit_of_char c in
      let rec parse_digits acc rest =
        match rest with
        | [] -> (acc, [])
        | '0' .. '9' as c :: rest ->
            let digit = digit_of_char c in
            parse_digits (acc * 10 + digit) rest
        | _ -> (acc, rest)
      in
      let (num, rest) = parse_digits digit rest in
      Some (num, rest)
  | _ -> None

let rec parse_expr (cs : char list) : (expr * char list) option =
  match trim cs with
  | '(' :: rest ->
      let (op, rest') =
        match trim rest with
        | "add" :: rest' -> (Add, rest')
        | "mul" :: rest' -> (Mul, rest')
        | _ -> (fun _ -> Int 0, rest) (* Default to Int 0 if not add or mul *)
      in
      parse_exprs op rest'
  | '0' .. '9' :: _ ->
      match parse_num cs with
      | Some (num, rest) -> (Int num, rest)
      | None -> None
  | _ -> None

and parse_exprs (op : expr list -> expr) (cs : char list) : (expr * char list) option =
  match trim cs with
  | ')' :: rest -> Some (op [], rest)
  | _ ->
      match parse_expr cs with
      | Some (expr, rest) ->
          (match parse_exprs op rest with
          | Some (exprs, rest') -> Some (op (expr :: exprs), rest')
          | None -> None)
      | None -> None

let parse (s : string) : expr option =
  match parse_expr (string_listize s) with
  | Some (expr, []) -> Some expr
  | _ -> None
