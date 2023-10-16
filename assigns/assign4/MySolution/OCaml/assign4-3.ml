#use "./../../../../classlib/OCaml/MyOCaml.ml"

type 'a gtree = Node of 'a * 'a gtree list
type 'a stream = Stream of 'a * (unit -> 'a stream)

let rec gtree_streamize_dfs (xs: 'a gtree): 'a stream =
  match xs with
  | Node (value, children) ->
    Stream (value, fun () -> gtree_streamize_dfs_list children)

and gtree_streamize_dfs_list children () =
  match children with
  | [] -> raise Stream.Failure
  | x :: xs -> try gtree_streamize_dfs x with Stream.Failure -> gtree_streamize_dfs_list xs ()

let rec gtree_streamize_bfs (xs: 'a gtree): 'a stream =
  let rec enqueue_children queue = function
    | [] -> queue
    | Node (value, children) :: rest ->
      Stream (value, fun () -> enqueue_children (queue @ children @ rest) [])
  in
  enqueue_children [] [xs]

