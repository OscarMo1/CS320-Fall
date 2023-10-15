#use "./../../../../classlib/OCaml/MyOCaml.ml"

let rec
gtree_streamize_dfs (xs: 'a gtree): 'a list =
  let rec dfs_streamize_helper tree =
    match tree with
    | GTnil -> []
    | GTcons (value, children) ->
      let children_list = list_concat_map children dfs_streamize_helper in
      value :: children_list
  in dfs_streamize_helper xs

and
list_concat_map xs f =
  let rec concat_map_helper xs f acc =
    match xs with
    | [] -> acc
    | x :: xs' ->
      let x_list = f x in
      let acc' = acc @ x_list in
      concat_map_helper xs' f acc'
  in concat_map_helper xs f []

let rec
gtree_streamize_bfs (xs: 'a gtree): 'a list =
  let rec bfs_streamize_helper trees =
    match trees with
    | [] -> []
    | GTnil :: rest -> bfs_streamize_helper rest
    | GTcons (value, children) :: rest ->
      let children_list = list_concat children in
      value :: children_list @ bfs_streamize_helper (rest @ children)
  in bfs_streamize_helper [xs]
