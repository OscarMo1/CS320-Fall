(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let isPrime (x: int): bool =
  let rec isDivisible num =
    num > 1 && (x mod num = 0 || isDivisible (num - 1))
  in
  x > 1 && not (isDivisible (x - 1));;
