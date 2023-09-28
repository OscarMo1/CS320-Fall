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


let mylist_get_at (xs: 'a mylist) (i0: int): 'a =
  let rec get_at_index index = function
    | MyNil -> mylist_subscript_exn ()  (* Position is out-of-bounds *)
    | MyCons(x, xs') | MySnoc(xs', x) ->
      if index = i0 then x
      else get_at_index (index + 1) xs'
    | MyReverse(xs') ->
      (* Recursively calculate the length of the tail of the list and add 1. *)
      get_at_index index xs'
    | MyAppend2(xs1, xs2) ->
      let len_xs1 = mylist_length xs1 in
      if index < len_xs1 then get_at_index index xs1
      else get_at_index (index - len_xs1) xs2
  in
  get_at_index i0 xs



