#use "./../../../classlib/OCaml/MyOCaml.ml";;

(* 
# specifications described in CS320_Fall_2023_Project-1.pdf

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

let const_to_string const =
  match const with
  | Int n -> string_of_int n
  | Bool b -> if b then "True" else "False"
  | Unit _ -> "Unit"
       

let rec parse_prg(prg: com list) =
	(
  let* _ = whitespaces in
  let* _ = keyword "Push" in
	let* _ = whitespaces in
	let* c = parse_const  () in
	let* _ = char ';' in
	let* _ = whitespaces in
	parse_prg ((Push c) :: prg))
	<|> 
	(let* _ = keyword "Pop;" in
	parse_prg (Pop :: prg))
	<|>
	(let* _ = keyword "Trace;" in
	parse_prg (Trace :: prg))
	<|>
	(let* _ = keyword "Add;" in
	parse_prg (Add :: prg))
	<|>	
	(let* _ = keyword "Sub;" in
	parse_prg (Sub :: prg))
	<|>	
	(let* _ = keyword "Mul;" in
	parse_prg (Mul :: prg))
	<|>	
	(let* _ = keyword "Div;" in
	parse_prg (Div :: prg))
	<|>	
	(let* _ = keyword "And;" in
	parse_prg (And :: prg))
	<|>	
	(let* _ = keyword "Or;" in
	parse_prg (Or :: prg))
	<|>	
	(let* _ = keyword "Not;" in
	parse_prg (Not :: prg))
	<|>	
	(let* _ = keyword "Lt;" in
	parse_prg (Lt :: prg))
	<|>	
	(let* _ = keyword "Gt;" in
	parse_prg (Gt :: prg))
	<|>
	pure(list_reverse(prg))

let string_parse_c(p: 'a parser)(s: string) =
  p(string_listize(s))
;;

let interp (s: string) = 
	let rec op(stack: const list)(trace: string list)(prg: com list) = 
		match prg with 
		| c :: rest -> 
			(match c with 
			| Push ct -> op(ct :: stack)(trace)(rest)
			| Pop -> (if array_len stack = 0 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| _ :: st -> op(st)(trace)(rest)
					| [] -> None
				))
			| Trace -> (if array_len stack = 0 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| top :: st -> op((Unit ()) :: st)(const_to_string (top) :: trace)(rest)
					| [] -> None
				))
			| Add -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> op( Int (i + j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Sub -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> op( Int (i - j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Mul -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> op( Int (i * j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Div -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int _, Int 0) -> (op(stack)("Panic" :: trace)([]))
						| (Int i, Int j) -> op( Int (i / j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| And -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Bool i, Bool j) -> op( Bool (i && j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Or -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Bool i, Bool j) -> op( Bool (i || j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Not -> (if array_len stack < 1 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: st -> 
						(match i with
						| Bool i -> op( Bool (not i) :: st)(trace)(rest)
						| _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Lt -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> op( Bool (i < j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			| Gt -> (if array_len stack < 2 then
				(op(stack)("Panic" :: trace)([])) else (
					match stack with
					| i :: j :: st -> 
						(match i, j with
						| (Int i, Int j) -> op( Bool (i > j) :: st)(trace)(rest)
						| _, _ -> (op(stack)("Panic" :: trace)([])))
					| [] -> None
				))
			)
		| [] -> Some trace
		in
	match string_parse_c (parse_prg []) s with 
	| Some (e, []) -> op([])([])(e) 
	| _ -> None  
