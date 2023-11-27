#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type const = 
| Int of int
| Bool of bool
| Unit of unit

type com =
  | Push of const | Pop | Trace
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt

(* type coms =
  | Empty
  | Coms of com * coms *)

let parse_const () : const parser = 
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
	pure (Unit( () )))

let rec array_len(xs: 'a list): int =
	list_foldleft(xs)(0)(fun acc x ->
		acc + 1)

let rec int2str(i0: int): string = 
	let rec digits x:int = if x < 10 then 1 else 1 + digits (x / 10) in
		let rec getChar(x: int) (i: int) : char = 
			let x = abs x in
			if i <= 0 then 
				let c = x mod 10 in
				chr(c + 48)
				else  
				getChar (x/10) (i - 1)
			in  
			let len = if i0 >= 0 then digits i0 else digits (-1 * i0) in 
			if i0 >= 0 then
			string_init len (fun i -> getChar i0 (len-i - 1)) (* for each digit, get char of index i from i0 *)
			else 
			string_init (len + 1) (fun i -> if i = 0 then '-' else getChar i0 (len-i))
		

let const_to_string(const): string = 
match const with
| Int n -> int2str n
| Bool true -> "True"
| Bool false -> "False"
| Unit _ -> "Unit"
(* | _ -> "what" *)

let rec parse_prog(prog: com list) =
	(
  let* _ = whitespaces in
  let* _ = keyword "Push" in
	let* _ = whitespaces in
	let* c = parse_const  () in
	(* let _ = print_string (const_to_string c) in  *)
	let* _ = char ';' in
	let* _ = whitespaces in
	parse_prog ((Push c) :: prog))
	<|> 
	(let* _ = keyword "Pop;" in
	parse_prog (Pop :: prog))
	<|>
	(let* _ = keyword "Trace;" in
	parse_prog (Trace :: prog))
	<|>
	(let* _ = keyword "Add;" in
	parse_prog (Add :: prog))
	<|>	
	(let* _ = keyword "Sub;" in
	parse_prog (Sub :: prog))
	<|>	
	(let* _ = keyword "Mul;" in
	parse_prog (Mul :: prog))
	<|>	
	(let* _ = keyword "Div;" in
	parse_prog (Div :: prog))
	<|>	
	(let* _ = keyword "And;" in
	parse_prog (And :: prog))
	<|>	
	(let* _ = keyword "Or;" in
	parse_prog (Or :: prog))
	<|>	
	(let* _ = keyword "Not;" in
	parse_prog (Not :: prog))
	<|>	
	(let* _ = keyword "Lt;" in
	parse_prog (Lt :: prog))
	<|>	
	(let* _ = keyword "Gt;" in
	parse_prog (Gt :: prog))
	<|>
	pure(list_reverse(prog))

let string_parse_c(p: 'a parser)(s: string) =
  p(string_listize(s))
;;

let interp (s: string) = 
	let rec evaluate(stack: const list)(trace: string list)(prog: com list) = 
		match prog with 
		| c :: rest -> 
			(match c with 
			| Push ct -> evaluate(ct :: stack)(trace)(rest)
			| Pop -> (if array_len stack = 0 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| _ :: st -> evaluate(st)(trace)(rest)
					| [] -> None
				))
			| Trace -> (if array_len stack = 0 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| top :: st -> evaluate((Unit ()) :: st)(const_to_string (top) :: trace)(rest)
					| [] -> None
				))
			| Add -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> evaluate( Int (i + j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Sub -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> evaluate( Int (i - j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Mul -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> evaluate( Int (i * j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Div -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int _, Int 0) -> (evaluate(stack)("Panic" :: trace)([]))
						| (Int i, Int j) -> evaluate( Int (i / j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| And -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Bool i, Bool j) -> evaluate( Bool (i && j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Or -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Bool i, Bool j) -> evaluate( Bool (i || j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Not -> (if array_len stack < 1 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: st -> 
						(match i with
						| Bool i -> evaluate( Bool (not i) :: st)(trace)(rest)
						| _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Lt -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> evaluate( Bool (i < j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Gt -> (if array_len stack < 2 then
				(evaluate(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> evaluate( Bool (i > j) :: st)(trace)(rest)
						| _, _ -> (evaluate(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			)
		| [] -> Some trace
		in
	match string_parse_c (parse_prog []) s with 
	| Some (e, []) -> evaluate([])([])(e) 
	| _ -> None  

(* 
let interp (s : string) : string list option = 
	match string_parse_c (parse_prog ([])) s with 
	| Some (e, []) -> Some e
	| _ -> None 
 *)


