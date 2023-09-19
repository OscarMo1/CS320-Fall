(* ****** ****** *)
#use "./../MySolution/assign1-1.ml";;
(* ****** ****** *)

let intrev10(n: int): int =
  let rec loop(n: int)(acc: int): int =
    if n = 0 then
      acc
    else
      let digit = n mod 10 in
      let new_acc = acc * 10 + digit in
      let remaining = n / 10 in
      loop remaining new_acc
  in
  loop n 0;;