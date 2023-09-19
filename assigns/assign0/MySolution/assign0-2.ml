(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let isPrime (x: int): bool =
  (*recursive call that checks if any values less than num divides into num besides 1*)
  let rec isDivisible num =
    num > 1 && (x mod num = 0 || isDivisible (num - 1))
  in
  x > 1 && not (isDivisible (x - 1));;
