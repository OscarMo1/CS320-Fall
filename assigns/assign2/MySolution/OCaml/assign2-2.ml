#use "./../../assign2.ml";;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

exception MyListSubscriptExn

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons (_, t) | MySnoc (t, _) -> 1 + mylist_length t
  | MyReverse xs' -> mylist_length xs'
  | MyAppend2 (xs1, xs2) -> mylist_length xs1 + mylist_length xs2

let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  if i0 < 0 then raise MyListSubscriptExn
  else
    match xs with
    | MyNil -> raise MyListSubscriptExn
    | MyCons (h, t) ->
      if i0 = 0 then h else mylist_get_at t (i0 - 1)
    | MySnoc (t, h) ->
      let len = mylist_length t in
      if i0 = len then h
      else if i0 < len then mylist_get_at t i0
      else raise MyListSubscriptExn
    | MyReverse xs' -> mylist_get_at xs' (mylist_length xs' - 1 - i0)
    | MyAppend2 (xs1, xs2) ->
      let l1 = mylist_length xs1 in
      if i0 < l1 then mylist_get_at xs1 i0
      else mylist_get_at xs2 (i0 - l1)
