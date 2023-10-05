#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec my_append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | x :: xs -> x :: my_append xs lst2;;

let rec my_map f lst =
  match lst with
  | [] -> []
  | x :: xs -> f x :: my_map f xs;;

let rec list_subsets (xs: 'a list): 'a list list =
  match xs with
  | [] -> [[]]
  | x :: xs ->
    let subsets = list_subsets xs in
    my_append subsets (my_map (fun subset -> x :: subset) subsets)
;;

let list_subsets_combinator (xs: 'a list): 'a list list =
  List.fold_left
    (fun acc x ->
      my_append acc (my_map (fun subset -> x :: subset) acc))
    [[]]
    xs
;;

