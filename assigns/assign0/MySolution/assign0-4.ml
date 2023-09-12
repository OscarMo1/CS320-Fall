(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let rec exponent (power: int) (n: int): int = 
  if power = 0 then
    n
  else
    exponent (power-1) (n * 10);;

let str2int(cs: string): int =
    let l = string_length(cs) in
    let rec loop i x = 
      if i < 0 then
        x
      else
        let c = string_get(cs, i) in
        let n = (ord(c)-48) in
        loop (i-1) (x + n * (exponent (l-1-i) 1))
    in
    loop (l-1) 0;;