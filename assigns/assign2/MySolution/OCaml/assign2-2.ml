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

let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a =
  match (xs, i0) with
  | (MyNil, _) -> mylist_subscript_exn ()  (* Out of bounds for an empty list. *)
  | (_, 0) -> (
      match xs with
      | MyCons(x, _) -> x  (* Found the element at position 0. *)
      | _ -> mylist_subscript_exn ()  (* Unexpected case. Should not happen. *)
    )
  | (_, _) when i0 < 0 -> mylist_subscript_exn ()  (* Negative index is out of bounds. *)
  | (MyCons(_, xs'), _) | (MySnoc(xs', _), _) | (MyReverse(xs'), _) ->
    (* Recursively move through the list and decrement the position. *)
    mylist_get_at(xs', i0 - 1)
  | (MyAppend2(xs1, xs2), _) ->
    let len_xs1 = mylist_length(xs1) in
    if i0 < len_xs1 then
      mylist_get_at(xs1, i0)  (* The element is in the first sublist. *)
    else
      mylist_get_at(xs2, i0 - len_xs1)  (* The element is in the second sublist. *)
;;


