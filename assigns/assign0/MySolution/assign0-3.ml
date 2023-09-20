(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* recursive function that finds the number of digits in a number*)
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