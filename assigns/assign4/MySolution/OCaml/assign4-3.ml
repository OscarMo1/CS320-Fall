#use "./../../../../classlib/OCaml/MyOCaml.ml"

type 'a stream = Cons of 'a * (unit -> 'a stream)

type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)

(* Depth-First Search Enumeration *)
let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  match xs with
  | GTnil -> failwith "Cannot streamize an empty tree."
  | GTcons (value, children) ->
    let rec children_stream children =
      match children with
      | [] -> fun () -> gtree_streamize_dfs GTnil (* Empty stream for no children *)
      | child :: rest -> fun () -> Cons (gtree_streamize_dfs child, children_stream rest)
    in
    Cons (value, children_stream children)

(* Breadth-First Search Enumeration *)
let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec bfs_queue queue =
    match queue with
    | [] -> fun () -> gtree_streamize_bfs GTnil (* Empty stream for an empty queue *)
    | GTnil :: rest -> fun () -> bfs_queue rest ()
    | GTcons (value, children) :: rest ->
      fun () -> Cons (value, bfs_queue (rest @ children) ())
  in
  bfs_queue [xs] ()


