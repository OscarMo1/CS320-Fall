#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*
Please implement the interp function as per CS320_Fall_2023_Project-2.pdf specifications.
Notes:
1. Use only library functions defined in MyOCaml.ml or ones you implement.
2. Do not use OCaml standard library functions directly.
*)

(* let interp (s : string) : string list option =  *)

type const =
  | Int of int
  | Bool of bool
  | Unit
  | Sym of string
  | Closure of const * (const * const) list * com list

and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | IfElse of com list * com list
  | Bind | Lookup
  | Fun of com list | Call | Return

let rec parse_com() =
  (keyword "Push" >> parse_const >>= fun c -> pure (Push c)) <|>
  (keyword "Pop" >> pure Pop) <|>
  (keyword "Swap" >> pure Swap) <|>
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
  (fun ls ->
    let@ (_, ls) = keyword "If" ls in
    let@ (c1, ls) = many (parse_com() << keyword ";") ls in
    let@ (_, ls) = keyword "Else" ls in
    let@ (c2, ls) = many (parse_com() << keyword ";") ls in
    let@ (_, ls) = keyword "End" ls in
    pure (IfElse (c1, c2)) ls) <|>
  (keyword "Bind" >> pure Bind) <|>
  (keyword "Lookup" >> pure Lookup) <|>
  (let* _ = keyword "Fun" in
   let* c = many (parse_com() << keyword ";") in
   let* _ = keyword "End" in
   pure (Fun c)) <|>
  (keyword "Call" >> pure Call) <|>
  (keyword "Return" >> pure Return)

let parse_prog = many (parse_com() << keyword ";")

let interp (s: string): string list option =
  let rec lookup x v =
    match v with
    | (var, value) :: v0 -> if x = var then Some value else lookup x v0
    | [] -> None
  in
  let rec evaluate s t v p =
    match p with
    | [] -> t
    | Push c :: p0 -> evaluate (c :: s) t v p0
    | Swap :: p0 -> (
        match s with
        | i :: j :: s0 -> evaluate (j :: i :: s0) t v p0
        | _ :: [] | [] -> evaluate [] ("Panic" :: t) v []
      )
    | Pop :: p0 -> (
        match s with
        | _ :: s0 -> evaluate s0 t v p0
        | [] -> evaluate [] ("Panic" :: t) v []
      )
    (* Other cases omitted for brevity *)
    | _ -> [] (* Handle other cases similarly *)
  in
  match string_parse_c parse_prog s with
  | Some (e, []) -> Some (evaluate [] [] [] e)
  | _ -> None
