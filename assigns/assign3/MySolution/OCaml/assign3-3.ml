#use "./../../../../classlib/OCaml/MyOCaml.ml"let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  let rec combinations k xs =
    if k = 0 then [[]]
    else
      match xs with
      | [] -> []
      | x :: xs' ->
        let with_x = List.map (fun subset -> x :: subset) (combinations (k - 1) xs') in
        let without_x = combinations k xs' in
        with_x @ without_x
  in
  combinations n0 xs
;;



