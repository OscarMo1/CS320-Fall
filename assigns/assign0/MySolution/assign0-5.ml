(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

let stringrev(cs: string): string = 
  let length = string_length cs in
  string_init length 
  (fun i -> string_get(cs, length-1 -i))
;;