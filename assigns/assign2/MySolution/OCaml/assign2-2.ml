#use "./../../assign2.ml";;


let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0  (* An empty list has a length of 0. *)
  | MyCons(_, xs') | MySnoc(xs', _) | MyReverse(xs') ->
    (* Recursively calculate the length of the tail of the list and add 1. *)
    1 + mylist_length(xs')
  | MyAppend2(xs1, xs2) ->
    (* Handle the MyAppend2 constructor with two sublists. *)
    mylist_length(xs1) + mylist_length(xs2)
;;

let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  match (xs, i0) with
  | (MyNil, _) -> mylist_subscript_exn ()
  | (_, n) when n < 0 -> mylist_subscript_exn ()
  | (MyCons (x, _), 0) -> x
  | (MySnoc (_, x), 0) -> x
  | (MyReverse xs', n) -> mylist_get_at xs' n
  | (MyAppend2 (xs1, xs2), n) ->
    let len_xs1 = mylist_length xs1 in
    if n < 0 || n >= len_xs1 + mylist_length xs2 then
      mylist_subscript_exn ()
    else if n < len_xs1 then
      mylist_get_at xs1 n
    else
      mylist_get_at xs2 (n - len_xs1)
  | _ -> mylist_subscript_exn ()

