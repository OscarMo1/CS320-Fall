#use "./../../../../classlib/OCaml/MyOCaml.ml"

type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)
  
let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  let rec helper tree_list () =
    match tree_list with
    | [] -> (fun () -> StrNil)
    | GTnil :: rest -> helper rest ()
    | GTcons (x, children) :: rest ->
      (fun () -> StrCons (x, helper (children @ rest) ()))
  in
  helper [xs] ()
;;
  
let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec helper tree_list () =
    match tree_list with
    | [] -> (fun () -> StrNil)
    | GTnil :: rest -> helper rest ()
    | GTcons (x, children) :: rest ->
      (fun () -> StrCons (x, helper (rest @ children) ()))
  in
  helper [xs] ()
;;


