#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec list_subsets (xs: 'a list): 'a list list =
  match xs with
  | [] -> [[]]
  | x :: xs ->
    let subsets = list_subsets xs in
    subsets @ List.map (fun subset -> x :: subset) subsets
;;

(* Non-recursive implementation using list combinators *)
let list_subsets_combinator (xs: 'a list): 'a list list =
  List.fold_left
    (fun acc x ->
      acc @ List.map (fun subset -> x :: subset) acc)
    [[]]
    xs
;;
