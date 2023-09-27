
#use "./../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0  (* An empty list has a length of 0. *)
  | MyCons(_, xs')| MySnoc(xs, _) ->
    1 + mylist_length(xs)
  | MyReverse(xs) ->
    (* Recursively calculate the length of the tail of the list and add 1. *)
    mylist_length(xs)
  | MyAppend2(xs1, xs2) ->
    (* Handle the MyAppend2 constructor with two sublists. *)
    mylist_length(xs1) + mylist_length(xs2)
;;

