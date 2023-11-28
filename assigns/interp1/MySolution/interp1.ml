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
       

let rec parse_prog(prog: com list) =
	(
  let* _ = whitespaces in
  let* _ = keyword "Push" in
	let* _ = whitespaces in
	let* c = parse_const  () in
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
   let rec evaluate stack trace prog =
     let array_len = List.length in
 
     let panic_trace = evaluate stack ("Panic" :: trace) [] in
 
     let pop_stack rest =
       match stack with
       | _ :: st -> evaluate st trace rest
       | [] -> None
     in
 
     let push_unit rest =
       match stack with
       | top :: st -> evaluate (Unit () :: st) (const_to_string top :: trace) rest
       | [] -> None
     in
 
     let binary_operation op_fn rest =
       match stack with
       | i :: j :: st ->
         (match i, j with
         | Int x, Int y -> evaluate (Int (op_fn x y) :: st) trace rest
         | _ -> panic_trace)
       | _ -> None
     in
 
     let division_operation rest =
       match stack with
       | i :: j :: st ->
         (match i, j with
         | Int _, Int 0 -> panic_trace
         | Int x, Int y -> evaluate (Int (x / y) :: st) trace rest
         | _ -> panic_trace)
       | _ -> None
     in
 
     let boolean_operation op_fn rest =
       match stack with
       | Bool x :: Bool y :: st ->
         evaluate (Bool (op_fn x y) :: st) trace rest
       | _ -> panic_trace
     in
 
     let not_operation rest =
       match stack with
       | Bool x :: st -> evaluate (Bool (not x) :: st) trace rest
       | _ -> panic_trace
     in
 
     let comparison_operation op_fn rest =
       match stack with
       | Int x :: Int y :: st ->
         evaluate (Bool (op_fn x y) :: st) trace rest
       | _ -> panic_trace
     in
 
     match prog with
     | Push ct :: rest -> evaluate (ct :: stack) trace rest
     | Pop :: rest -> (if array_len stack = 0 then panic_trace else pop_stack rest)
     | Trace :: rest -> (if array_len stack = 0 then panic_trace else push_unit rest)
     | Add :: rest -> (if array_len stack < 2 then panic_trace else binary_operation (+) rest)
     | Sub :: rest -> (if array_len stack < 2 then panic_trace else binary_operation (-) rest)
     | Mul :: rest -> (if array_len stack < 2 then panic_trace else binary_operation ( * ) rest)
     | Div :: rest -> (if array_len stack < 2 then panic_trace else division_operation rest)
     | And :: rest -> (if array_len stack < 2 then panic_trace else boolean_operation (&&) rest)
     | Or :: rest -> (if array_len stack < 2 then panic_trace else boolean_operation (||) rest)
     | Not :: rest -> (if array_len stack < 1 then panic_trace else not_operation rest)
     | Lt :: rest -> (if array_len stack < 2 then panic_trace else comparison_operation (<) rest)
     | Gt :: rest -> (if array_len stack < 2 then panic_trace else comparison_operation (>) rest)
     | _ :: _ -> panic_trace
     | [] -> Some trace
   in
 
   match string_parse_c (parse_prog []) s with 
   | Some (e, []) -> evaluate [] [] e
   | _ -> None
 