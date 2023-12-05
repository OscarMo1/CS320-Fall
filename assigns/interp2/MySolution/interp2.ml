
#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.
*)


type const = 
| Int of int
| Bool of bool
| Unit
| Sym of string
| Closure of const*(const*const)list*(com list)

and com =
  | Push of const | Pop | Swap | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt
  | IfElse of com list*com list
  | Bind | Lookup
  | Fun of com list | Call | Return

let symbol : string parser =
   fun ls ->
     match many1 (satisfy (fun c -> char_islower c || char_isdigit c)) ls with
     | Some (xs, ls') -> Some (list_foldleft xs "" (fun acc n -> string_append acc (str n)), ls')
     | None -> None
 
let parse_const = 
	(let* _ = char '-' in
	let* x = natural in pure  (Int (-x)))
	<|>
	(let* n = natural in
	pure ( Int n ))
	<|> 
	(let* _ = keyword "True" in 
	pure (Bool (true) ))
	<|> 
	(let* _ = keyword "False" in 
	pure (Bool( false )))
	<|> 
	(let* _ = keyword "Unit" in
	pure (Unit))
  <|>
  (let* sym = symbol in 
   pure (Sym ( sym )))

let rec array_len(xs: 'a list): int =
	list_foldleft(xs)(0)(fun acc x ->
		acc + 1)


      let num_length(x: int): int =
      let rec num_length_helper(x: int)(count: int): int =
         if x < 10 then count + 1
         else num_length_helper (x / 10) (count + 1)
      in
      num_length_helper x 0
   ;;
       
       
       
   let int2str(i0: int): string =
      (*get_digit returns the individual digit at specified index of number*)
      let getDigit(x: int): int =
      let rec getDigitHelper(x: int)(result: int): int =
         if x = 0 then result
         else getDigitHelper (x - 1) (result * 10)
      in
      getDigitHelper x 1 in
         
      let length = num_length (abs i0) in
      let isNeg = i0 < 0 in
      (*create string with length based on length and isNeg*)
      string_init (length + (if isNeg then 1 else 0))
      (* lambda function that calls index of the value of int and turns into string*)
      (fun i -> 
         if i = 0 && isNeg then '-'
         else
            let index = length - (if isNeg then (i - 1) else i) - 1 in
            let digit = abs (i0 / getDigit index) mod 10
         in
         chr(ord '0' + digit)
         );;
		
let const_to_string(const): string = 
match const with
| Int n -> int2str n
| Bool b -> if b then "True" else "False"
| Unit -> "Unit"
| Sym str -> str
| Closure (Sym str, _, _) -> string_append (string_append "Fun<" str) (">")

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
    let@ (_, ls) = keyword "If" (ls) in 
    let@ (c1, ls) = many (parse_com() << keyword ";" ) (ls) in
    let@ (_, ls) = keyword "Else" (ls) in
    let@ (c2, ls) = many (parse_com() << keyword ";") (ls) in
    let@ (_, ls) = keyword "End" (ls) in 
    pure (IfElse (c1, c2) )(ls))  <|>
  (keyword "Bind" >> pure Bind) <|> 
  (keyword "Lookup" >> pure Lookup) <|>
  (let* _ = keyword "Fun" in
   let* c = many (parse_com() << keyword ";")  in
   let* _ = keyword "End" in
   pure (Fun c)) <|>
  (keyword "Call" >> pure Call)  <|>
  (keyword "Return" >> pure Return) 
  

let parse_prog = many (parse_com() << keyword ";")

let string_parse_c(p: 'a parser)(s: string) =
  p(string_listize(s))

let rec eval(s : const list) (t : string list) (v : (const * const) list) (p : com list) : string list =
    match p with
    (* termination state returns the trace *)
    | [] -> t
    | Push c :: p0 (* PushStack *) -> eval (c :: s) t v p0
    | Swap :: p0 -> 
      (match s with
      | i :: j :: s0   (*swap stack*) -> eval ( j :: i :: s0) t v p0
      | _ :: []        (* Die *)      -> eval [] ("Panic" :: t) v []
      | []                            -> eval [] ("Panic" :: t) v []
      )
    | Pop :: p0 ->
      (match s with
       | _ :: s0 (* PopStack *) -> eval s0 t v p0
       | []      (* PopError *) -> eval [] ("Panic" :: t) v [])
    | Trace :: p0 ->
      (match s with
       | c :: s0 (* TraceStack *) -> eval (Unit  :: s0) (const_to_string c :: t) v p0
       | []      (* TraceError *) -> eval [] ("Panic" :: t) v [])
    | Add :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* AddStack *)  -> eval (Int (i + j) :: s0) t v p0
       | _ :: _ :: s0         (* AddError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* AddError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* AddError3 *) -> eval [] ("Panic" :: t) v [])
    | Sub :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* SubStack *)  -> eval (Int (i - j) :: s0) t v p0
       | _ :: _ :: s0         (* SubError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* SubError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* SubError3 *) -> eval [] ("Panic" :: t) v [])
    | Mul :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* MulStack *)  -> eval (Int (i * j) :: s0) t v p0
       | _ :: _ :: s0         (* MulError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* MulError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* MulError3 *) -> eval [] ("Panic" :: t) v [])
    | Div :: p0 ->
      (match s with
       | Int i :: Int 0 :: s0 (* DivError0 *) -> eval [] ("Panic" :: t) v []
       | Int i :: Int j :: s0 (* DivStack *)  -> eval (Int (i / j) :: s0) t v p0
       | _ :: _ :: s0         (* DivError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* DivError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* DivError3 *) -> eval [] ("Panic" :: t) v [])
    | And :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* AndStack *)  -> eval (Bool (a && b) :: s0) t v p0
       | _ :: _ :: s0           (* AndError1 *) -> eval [] ("Panic" :: t) v []
       | []                     (* AndError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []                (* AndError3 *) -> eval [] ("Panic" :: t) v [])
    | Or :: p0 ->
      (match s with
       | Bool a :: Bool b :: s0 (* OrStack *)  -> eval (Bool (a || b) :: s0) t v p0
       | _ :: _ :: s0           (* OrError1 *) -> eval [] ("Panic" :: t) v []
       | []                     (* OrError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []                (* OrError3 *) -> eval [] ("Panic" :: t) v [])
    | Not :: p0 ->
      (match s with
       | Bool a :: s0 (* NotStack  *) -> eval (Bool (not a) :: s0) t v p0
       | _ :: s0      (* NotError1 *) -> eval [] ("Panic" :: t) v []
       | []           (* NotError2 *) -> eval [] ("Panic" :: t) v [])
    | Lt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* LtStack *)  -> eval (Bool (i < j) :: s0) t v p0
       | _ :: _ :: s0         (* LtError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* LtError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* LtError3 *) -> eval [] ("Panic" :: t) v [])
    | Gt :: p0 ->
      (match s with
       | Int i :: Int j :: s0 (* GtStack *)  -> eval (Bool (i > j) :: s0) t v p0
       | _ :: _ :: s0         (* GtError1 *) -> eval [] ("Panic" :: t) v []
       | []                   (* GtError2 *) -> eval [] ("Panic" :: t) v []
       | _ :: []              (* GtError3 *) -> eval [] ("Panic" :: t) v [])
    | IfElse (c1, c2) :: p0 ->       
      (match s with 
        | Bool b :: s0 -> if b  then        eval s0 t v (list_append c1 p0) else eval s0 t v (list_append c2 p0)
        | _ :: s0      ->                   eval [] ("Panic" :: t) v []
        | []           ->                   eval [] ("Panic" :: t) v []
      )
    | Bind :: p0 -> 
      (match s with 
        | Sym x :: v0 :: s0 ->              eval s0 t (((Sym x), v0) :: v) p0
        | _ :: _ :: s0      ->              eval [] ("Panic" :: t) v []
        | _ :: s0      ->                   eval [] ("Panic" :: t) v []
        | []           ->                   eval [] ("Panic" :: t) v []
      )
    | Lookup :: p0 -> 
      (match s with 
        | Sym x :: s0 ->  (match (lookup (Sym(x)) v) with
          | Some (value) ->                eval (value :: s0) t v p0 
          | None         ->                eval [] ("Panic" :: t) v [])
        | _ :: s0        ->                eval [] ("Panic" :: t) v []
        | []             ->                eval [] ("Panic" :: t) v []     
      )
    | Fun c :: p0 -> 
      (match s with 
        | Sym x :: s0    ->                eval ((Closure (Sym x, v, c)) :: s0) t v p0
        | _ :: s0        ->                eval [] ("Panic" :: t) v []
        | []             ->                eval [] ("Panic" :: t) v []     
      )
    | Call :: p0 -> 
      (match s with 
        | Closure (f, vf, c) :: a :: s0 ->        eval (a :: (Closure (Sym "cc", v, p0) :: s0)) t ((f, Closure(f, vf, c)) :: vf) c
        | _ :: _ :: s0        ->                  eval [] ("Panic" :: t) v []
        | _ :: s0             ->                  eval [] ("Panic" :: t) v []
        | []                  ->                  eval [] ("Panic" :: t) v [] 
      )
    | Return :: p0 -> 
      (match s with 
        | Closure(f, vf, c) :: a :: s0 ->       eval (a :: s0) t vf c
        | _ :: _ :: s0        ->                eval [] ("Panic" :: t) v []
        | _ :: s0             ->                eval [] ("Panic" :: t) v []
        | []                  ->                eval [] ("Panic" :: t) v []    
      )
    in
	match string_parse_c (parse_prog) s with 
	| Some (e, []) -> Some(eval([])([])([])(e)) 
	| _ -> None 
let interp (s: string) : string list option = 
      let rec lookup (x : const) (v : (const * const) list) : const option = 
        match v with
        | (var, value) :: v0 -> if x = var then Some(value) else lookup x v0
        | [] -> None
      in
    