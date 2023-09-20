(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* recursive function that finds the number of digits in a number*)
let rec num_length(x: int): int = 
  if x < 10 then 1 else 1 + num_length(x/10)


let int2str(i0: int): string =
  (*get_digit returns the individual digit at specified index of number*)
  let rec getDigit(x: int): int =
    if x = 0 then 1 else 10 * getDigit (x-1)
  in

  let length = num_length (abs i0) in
  let isNeg = i0 < 0 in
  string_init (length + (if isNeg then 1 else 0))
  (fun i -> 
    if i = 0 && isNeg then '-'
    else
      let index = length - (if isNeg then (i - 1) else i) - 1 in
      let digit = abs (i0 / getDigit index) mod 10
    in
    chr(ord '0' + digit)
    );;