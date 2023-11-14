#use "./../../../../classlib/OCaml/MyOCaml.ml"
(*
//
Assign6-1:
//
Please implement a print and parse function. Using parser combinators. When
given a valid string according to the grammar, your parse function returns an
sexpr value encoding the expression.

//
let sexpr_to_string (e : sexpr)  : string       = ...
let sexpr_parse     (s : string) : sexpr option = ...
//

Example (Accepted Strings):
sexpr_parse "(add 1 2 3)" = Some (SAdd [SInt 1; SInt 2; Int 3])
sexpr_parse "(mul (add 1 2) 3 (mul 1))" = Some (SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]])
//
Example (Rejected Strings):
sexpr_parse "()" = None
sexpr_parse "(add)" = None
sexpr_parse "(add 1 2))" = None
sexpr_parse "((mul 1 2)" = None
//
*)

(* ****** ****** *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)
type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)
;;


let rec sexpr_to_string (e : sexpr)  : string = 
  match e with 
  | SInt n -> string_of_int n
  | SAdd(t) -> "(add" ^ (list_foldleft t "" (fun acc e -> acc ^ " " ^ sexpr_to_string e)) ^ ")"
  | SMul(t) -> "(mul" ^ (list_foldleft t "" (fun acc e -> acc ^ " " ^ sexpr_to_string e)) ^ ")"
;;


let sexpr_parse(s : string) : sexpr option = 
  let rec parser_expr () =
    (let* _ = whitespaces in
    let* x = natural in
      pure(SInt x)
    )
    <|>
    (let* _ = whitespaces in
    let* _ = char '(' in
    let* _ = literal "add" in
    let* x = many (parser_expr()) in
    let* _ = char ')' in
    pure(SAdd(x))
    )
    <|>
    (let* _ = whitespaces in
    let* _ = char '(' in
    let* _ = literal "mul" in
    let* x = many (parser_expr()) in
    let* _ = char ')' in
    pure(SMul(x))
    )
  in 
  match string_parse (parser_expr ()) s with
  | Some (expr, []) -> Some expr
  | _ -> None
;;
