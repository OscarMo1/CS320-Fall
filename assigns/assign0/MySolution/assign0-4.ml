(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* helper exponent function that keeps track of value in int*)
let rec exponent (power: int) (n: int): int = 
  if power = 0 then
    n
  else
    exponent (power-1) (n * 10);;

let str2int(cs: string): int =
    (* string_length value is stored to iterate in rec loop*)
    let l = string_length(cs) in
    let rec loop i x = 
      if i < 0 then
        x
      else
        (*converts each char in string to int*)
        let c = string_get(cs, i) in
        let n = (ord(c)-48) in
        loop (i-1) (x + n * (exponent (l-1-i) 1))
    in
    loop (l-1) 0;;