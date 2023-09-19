(* ****** ****** *)
#use "./../MySolution/assign1-1.ml";;
(* ****** ****** *)

let intrev10(n: int): int =
let rec reverse_tail_recursive(n: int)(reversed: int): int =
  if n = 0 then
    reversed
  else
    let reversed = reversed * 10 + (n mod 10) in
    let remaining = n / 10 in
    reverse_tail_recursive remaining reversed
in
reverse_tail_recursive n 0
