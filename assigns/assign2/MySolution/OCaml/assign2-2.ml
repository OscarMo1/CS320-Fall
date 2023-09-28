#use "./../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0  (* An empty list has a length of 0. *)
  | MyCons(_, xs )| MySnoc(xs, _) ->
    1 + mylist_length(xs)
  | MyReverse(xs) ->
    (* Recursively calculate the length of the tail of the list and add 1. *)
    mylist_length(xs)
  | MyAppend2(xs1, xs2) ->
    (* Handle the MyAppend2 constructor with two sublists. *)
    mylist_length(xs1) + mylist_length(xs2)
;;


let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  let rec get_at_helper i xs =
    match xs with
    | MyCons (x, _) when i = 0 -> x
    | MyCons (_, rest) when i > 0 -> get_at_helper (i - 1) rest
    | MySnoc (_, x) when i = 0 -> x
    | MySnoc (rest, _) when i > 0 -> get_at_helper (i - 1) rest
    | MyAppend2 (xs1, xs2) when i >= 0 ->
      let xs1_len = mylist_length xs1 in
      if i < xs1_len then
        get_at_helper i xs1
      else
        get_at_helper (i - xs1_len) xs2
    | MyReverse xs when i >= 0 ->
      let xs_len = mylist_length xs in
      if i < xs_len then
        get_at_helper(xs_len - 1 - i) xs
      else
        mylist_subscript_exn ()
    | _ -> mylist_subscript_exn ()
  in
  if i0 < 0 then mylist_subscript_exn ()
  else get_at_helper i0 xs

