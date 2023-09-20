(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)
(*lambda function used to iterate backwards of the string to create the reverse of string*)
let stringrev(cs: string): string = 
  let length = string_length cs in
  string_init length 
  (fun i -> string_get(cs, length-1 -i))
;;